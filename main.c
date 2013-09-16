/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "enable_posix.h"

#include <errno.h>
#include <libfirm/be.h>
#include <libfirm/firm.h>
#include <libfirm/statev.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adt/array.h"
#include "adt/error.h"
#include "adt/strutil.h"
#include "adt/util.h"
#include "ast2firm.h"
#include "ast_t.h"
#include "constfold.h"
#include "constfoldbits.h"
#include "diagnostic.h"
#include "driver.h"
#include "driver/firm_machine.h"
#include "driver/firm_opt.h"
#include "driver/firm_timing.h"
#include "help.h"
#include "lang_features.h"
#include "mangle.h"
#include "parser.h"
#include "predefs.h"
#include "preprocessor.h"
#include "printer.h"
#include "symbol_table.h"
#include "tempfile.h"
#include "type_hash.h"
#include "type_t.h"
#include "types.h"
#include "version.h"
#include "wrappergen/write_compoundsizes.h"
#include "wrappergen/write_fluffy.h"
#include "wrappergen/write_jna.h"
#include <revision.h>

#ifndef PREPROCESSOR
#ifndef __WIN32__
#define PREPROCESSOR "gcc -E -U__STRICT_ANSI__ -U__BLOCKS__"
#else
#define PREPROCESSOR "cpp -U__STRICT_ANSI__"
#endif
#endif

#ifndef LINKER
#define LINKER    "gcc"
#endif

#ifndef ASSEMBLER
#define ASSEMBLER "gcc -c -xassembler"
#endif

#ifndef COMPILER_INCLUDE_DIR
#define COMPILER_INCLUDE_DIR NULL
#endif
#ifndef LOCAL_INCLUDE_DIR
#define LOCAL_INCLUDE_DIR NULL
#endif
#ifndef TARGET_INCLUDE_DIR
#define TARGET_INCLUDE_DIR NULL
#endif
#ifndef SYSTEM_INCLUDE_DIR
#define SYSTEM_INCLUDE_DIR NULL
#endif

static const char  *compiler_include_dir      = COMPILER_INCLUDE_DIR;
static const char  *local_include_dir         = LOCAL_INCLUDE_DIR;
static const char  *target_include_dir        = TARGET_INCLUDE_DIR;
static const char  *system_include_dir        = SYSTEM_INCLUDE_DIR;

c_dialect_t dialect = {
	.features       = _C89 | _C99 | _GNUC, /* TODO/FIXME should not be inited */
	.char_is_signed = true,
};
target_t target;

static struct obstack  file_obst;
static const char     *external_preprocessor;

typedef struct codegen_option_t codegen_option_t;

struct codegen_option_t {
	codegen_option_t *next;
	char              option[];
};

static int detect_color_terminal(void)
{
	/* we want to avoid bloated linking against termcap/ncurses, so we use a
	 * simple detection heuristic (similar to one git uses) */
	if (!isatty(1))
		return 0;

	char *term = getenv("TERM");
	if (term == NULL || streq(term, "dumb"))
		return 0;
	if (strstr(term, "256color") != 0)
		return 256;
	return 8;
}

static const char *type_to_string(const type_t *type)
{
	assert(type->kind == TYPE_ATOMIC);
	return get_atomic_kind_name(type->atomic.akind);
}

static void print_cparser_version(void)
{
	printf("cparser %s.%s.%s",
	       CPARSER_MAJOR, CPARSER_MINOR, CPARSER_PATCHLEVEL);
	if (cparser_REVISION[0] != '\0') {
		printf("(%s)", cparser_REVISION);
	}
	printf(" using libFirm %u.%u",
	       ir_get_version_major(), ir_get_version_minor());

	const char *revision = ir_get_version_revision();
	if (revision[0] != 0) {
		printf("(%s)", revision);
	}
	putchar('\n');
	fputs("This is free software; see the source for copying conditions.  There is NO\n"
	     "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n", stdout);
}

static void print_cparser_version_short(void)
{
	puts(cparser_REVISION);
}

static void set_be_option(const char *arg)
{
	int res = be_parse_arg(arg);
	(void) res;
	assert(res);
}

static bool init_os_support(void)
{
	dialect.wchar_atomic_kind        = ATOMIC_TYPE_INT;
	dialect.intmax_predefs           = false;
	target.enable_main_collect2_hack = false;
	driver_default_exe_output        = "a.out";

	if (firm_is_unixish_os(target.machine)) {
		set_create_ld_ident(create_name_linux_elf);
		target.user_label_prefix = "";
	} else if (firm_is_darwin_os(target.machine)) {
		set_create_ld_ident(create_name_macho);
		target.user_label_prefix = "_";
		dialect.intmax_predefs   = true;
	} else if (firm_is_windows_os(target.machine)) {
		set_create_ld_ident(create_name_win32);
		dialect.wchar_atomic_kind        = ATOMIC_TYPE_USHORT;
		target.enable_main_collect2_hack = true;
		target.user_label_prefix         = "_";
		driver_default_exe_output        = "a.exe";
	} else {
		return false;
	}

	return true;
}

static bool parse_target_triple(const char *arg)
{
	machine_triple_t *triple = firm_parse_machine_triple(arg);
	if (triple == NULL) {
		errorf(NULL, "target-triple '%s' is not in the form 'cpu_type-manufacturer-operating_system'", arg);
		return false;
	}
	target.machine = triple;
	return true;
}

static const char *setup_isa_from_triple(const machine_triple_t *machine)
{
	const char *cpu = machine->cpu_type;

	if (firm_is_ia32_cpu(cpu)) {
		return "ia32";
	} else if (streq(cpu, "x86_64")) {
		return "amd64";
	} else if (streq(cpu, "sparc")) {
		return "sparc";
	} else if (streq(cpu, "arm")) {
		return "arm";
	} else {
		errorf(NULL, "unknown cpu '%s' in target-triple", cpu);
		return NULL;
	}
}

static const char *setup_target_machine(void)
{
	if (!setup_firm_for_machine(target.machine))
		exit(1);

	const char *isa = setup_isa_from_triple(target.machine);

	if (isa == NULL)
		exit(1);

	init_os_support();

	return isa;
}

/**
 * initialize cparser type properties based on a firm type
 */
static void set_typeprops_type(atomic_type_properties_t* props, ir_type *type)
{
	props->size             = get_type_size_bytes(type);
	props->alignment        = get_type_alignment_bytes(type);
	props->struct_alignment = props->alignment;
}

/**
 * Copy atomic type properties except the integer conversion rank
 */
static void copy_typeprops(atomic_type_properties_t *dest,
                           const atomic_type_properties_t *src)
{
	dest->size             = src->size;
	dest->alignment        = src->alignment;
	dest->struct_alignment = src->struct_alignment;
	dest->flags            = src->flags;
}

static void init_types_and_adjust(void)
{
	const backend_params *be_params = be_get_backend_param();
	unsigned machine_size = be_params->machine_size;
	init_types(machine_size);

	atomic_type_properties_t *props = atomic_type_properties;

	/* adjust types as requested by target architecture */
	ir_type *const type_ld = be_params->type_long_double;
	if (type_ld) {
		set_typeprops_type(&props[ATOMIC_TYPE_LONG_DOUBLE], type_ld);
	}

	ir_type *const type_ll = be_params->type_long_long;
	if (type_ll)
		set_typeprops_type(&props[ATOMIC_TYPE_LONGLONG], type_ll);

	ir_type *const type_ull = be_params->type_unsigned_long_long;
	if (type_ull)
		set_typeprops_type(&props[ATOMIC_TYPE_ULONGLONG], type_ull);

	/* operating system ABI specifics */
	if (firm_is_darwin_os(target.machine)) {
		if (firm_is_ia32_cpu(target.machine->cpu_type)) {
			props[ATOMIC_TYPE_LONGLONG].struct_alignment    =  4;
			props[ATOMIC_TYPE_ULONGLONG].struct_alignment   =  4;
			props[ATOMIC_TYPE_DOUBLE].struct_alignment      =  4;
			props[ATOMIC_TYPE_LONG_DOUBLE].size             = 16;
			props[ATOMIC_TYPE_LONG_DOUBLE].alignment        = 16;
			props[ATOMIC_TYPE_LONG_DOUBLE].struct_alignment = 16;
		}
	} else if (firm_is_windows_os(target.machine)) {
		if (firm_is_ia32_cpu(target.machine->cpu_type)) {
			props[ATOMIC_TYPE_LONGLONG].struct_alignment    =  8;
			props[ATOMIC_TYPE_ULONGLONG].struct_alignment   =  8;
			props[ATOMIC_TYPE_DOUBLE].struct_alignment      =  8;
		} else if (machine_size == 64) {
			/* to ease porting of old c-code microsoft decided to use 32bits
			 * even for long */
			props[ATOMIC_TYPE_LONG]  = props[ATOMIC_TYPE_INT];
			props[ATOMIC_TYPE_ULONG] = props[ATOMIC_TYPE_UINT];
		}

		/* on windows long double is not supported */
		props[ATOMIC_TYPE_LONG_DOUBLE] = props[ATOMIC_TYPE_DOUBLE];
	} else if (firm_is_unixish_os(target.machine)) {
		if (firm_is_ia32_cpu(target.machine->cpu_type)) {
			props[ATOMIC_TYPE_DOUBLE].struct_alignment    = 4;
			props[ATOMIC_TYPE_LONGLONG].struct_alignment  = 4;
			props[ATOMIC_TYPE_ULONGLONG].struct_alignment = 4;
		}
	}

	/* stuff decided after processing operating system specifics and
	 * commandline flags */
	if (dialect.char_is_signed) {
		props[ATOMIC_TYPE_CHAR].flags |= ATOMIC_TYPE_FLAG_SIGNED;
	} else {
		props[ATOMIC_TYPE_CHAR].flags &= ~ATOMIC_TYPE_FLAG_SIGNED;
	}
	/* copy over wchar_t properties (including rank) */
	props[ATOMIC_TYPE_WCHAR_T] = props[dialect.wchar_atomic_kind];

	/* initialize defaults for unsupported types */
	if (!type_ld) {
		copy_typeprops(&props[ATOMIC_TYPE_LONG_DOUBLE],
		               &props[ATOMIC_TYPE_DOUBLE]);
	}

	target.byte_order_big_endian = be_params->byte_order_big_endian;
	if (be_params->modulo_shift_efficient) {
		target.modulo_shift = machine_size;
	} else {
		target.modulo_shift = 0;
	}

	/* initialize firm pointer modes */
	char     name[64];
	unsigned bit_size     = machine_size;
	unsigned modulo_shift = target.modulo_shift;

	snprintf(name, sizeof(name), "p%u", machine_size);
	ir_mode *ptr_mode = new_reference_mode(name, irma_twos_complement, bit_size, modulo_shift);

	if (machine_size == 16) {
		set_reference_mode_signed_eq(ptr_mode, mode_Hs);
		set_reference_mode_unsigned_eq(ptr_mode, mode_Hu);
	} else if (machine_size == 32) {
		set_reference_mode_signed_eq(ptr_mode, mode_Is);
		set_reference_mode_unsigned_eq(ptr_mode, mode_Iu);
	} else if (machine_size == 64) {
		set_reference_mode_signed_eq(ptr_mode, mode_Ls);
		set_reference_mode_unsigned_eq(ptr_mode, mode_Lu);
	} else {
		panic("strange machine_size when determining pointer modes");
	}

	/* Hmm, pointers should be machine size */
	set_modeP_data(ptr_mode);
	set_modeP_code(ptr_mode);
}

static void append_standard_include_paths(void)
{
	if (compiler_include_dir != NULL)
		append_include_path(&system_searchpath, compiler_include_dir);
	if (local_include_dir != NULL)
		append_include_path(&system_searchpath, local_include_dir);
	if (target_include_dir != NULL)
		append_include_path(&system_searchpath, target_include_dir);
	else if (target_include_dir == NULL && system_include_dir != NULL) {
		/* some guessing to find the "gcc-multilib" include dir */
		assert(obstack_object_size(&file_obst) == 0);
		if (firm_is_ia32_cpu(target.machine->cpu_type)
			&& firm_is_unixish_os(target.machine)) {
			obstack_printf(&file_obst, "%s/i386-linux-gnu", system_include_dir);
	path_from_obst:;
			obstack_1grow(&file_obst, '\0');
			char *path = obstack_finish(&file_obst);
			append_include_path(&system_searchpath, path);
		} else if (target.triple != NULL) {
			obstack_printf(&file_obst, "%s/%s", system_include_dir, target.triple);
			goto path_from_obst;
		}
	}
	if (system_include_dir != NULL)
		append_include_path(&system_searchpath, system_include_dir);
}

static void append_environment_include_paths(void)
{
	append_env_paths(&bracket_searchpath, "CPATH");
	append_env_paths(&system_searchpath,
	                 dialect.cpp ? "CPLUS_INCLUDE_PATH" : "C_INCLUDE_PATH");
}

static void init_driver_tools(void)
{
	assert(obstack_object_size(&file_obst) == 0);
	/* decide which linker, preprocessor, assembler to use */
	driver_preprocessor = external_preprocessor;
	if (driver_preprocessor == NULL)
		driver_preprocessor = getenv("CPARSER_PP");
	if (driver_preprocessor == NULL) {
		if (target.triple != NULL)
			obstack_printf(&file_obst, "%s-", target.triple);
		obstack_printf(&file_obst, "%s", PREPROCESSOR);
		driver_preprocessor = obstack_finish(&file_obst);
	}
	driver_assembler = getenv("CPARSER_AS");
	if (driver_assembler == NULL) {
		if (target.triple != NULL)
			obstack_printf(&file_obst, "%s-", target.triple);
		obstack_printf(&file_obst, "%s", ASSEMBLER);
		driver_assembler = obstack_finish(&file_obst);
	}
	driver_linker = getenv("CPARSER_LINK");
	if (driver_linker == NULL) {
		if (target.triple != NULL)
			obstack_printf(&file_obst, "%s-", target.triple);
		obstack_printf(&file_obst, "%s", LINKER);
		driver_linker = obstack_finish(&file_obst);
	}
}

static void init_external_preprocessor(void)
{
	struct obstack *o = &c_cpp_cppflags_obst;
	/* setup default defines */
	driver_add_flag(o, "-U__WCHAR_TYPE__");
	driver_add_flag(o, "-D__WCHAR_TYPE__=%s", type_to_string(type_wchar_t));
	driver_add_flag(o, "-U__SIZE_TYPE__");
	driver_add_flag(o, "-D__SIZE_TYPE__=%s", type_to_string(type_size_t));

	driver_add_flag(o, "-U__VERSION__");
	driver_add_flag(o, "-D__VERSION__=\"%s\"", CPARSER_VERSION);
	driver_add_flag(o, "-D__CPARSER_MAJOR__=\"%s\"", CPARSER_MAJOR);
	driver_add_flag(o, "-D__CPARSER_MINOR__=\"%s\"", CPARSER_MINOR);
	driver_add_flag(o, "-D__CPARSER_PATCHLEVEL__=\"%s\"",
					CPARSER_PATCHLEVEL);

	driver_add_flag(o, "-U_FORTIFY_SOURCE");
	driver_add_flag(o, "-D_FORTIFY_SOURCE=0");

	if (dialect.intmax_predefs) {
		driver_add_flag(o, "-U__INTMAX_TYPE__");
		driver_add_flag(o, "-D__INTMAX_TYPE__=%s",
						type_to_string(type_intmax_t));
		driver_add_flag(o, "-U__UINTMAX_TYPE__");
		driver_add_flag(o, "-D__UINTMAX_TYPE__=%s",
						type_to_string(type_uintmax_t));
	}
}

int main(int argc, char **argv)
{
	const char         *print_file_name_file   = NULL;
	int                 opt_level              = 1;
	char                firm_be[16]            = "ia32";
	bool                profile_generate       = false;
	bool                profile_use            = false;
	bool                do_timing              = false;
	bool                print_timing           = false;
	bool                stdinc                 = true;
	bool                had_inputs             = false;
	codegen_option_t   *codegen_options        = NULL;
	codegen_option_t  **codegen_options_anchor = &codegen_options;

	obstack_init(&file_obst);
	init_temp_files();
	init_symbol_table();
	init_tokens();
	init_driver();
	preprocessor_early_init();

#define GET_ARG_AFTER(def, args)                                             \
	do {                                                                     \
	def = &arg[sizeof(args)-1];                                              \
	if (def[0] == '\0') {                                                    \
		++i;                                                                 \
		if (i >= argc) {                                                     \
			errorf(NULL, "expected argument after '" args "'"); \
			argument_errors = true;                                          \
			break;                                                           \
		}                                                                    \
		def = argv[i];                                                       \
		if (def[0] == '-' && def[1] != '\0') {                               \
			errorf(NULL, "expected argument after '" args "'"); \
			argument_errors = true;                                          \
			continue;                                                        \
		}                                                                    \
	}                                                                        \
	} while (0)

#define SINGLE_OPTION(ch) (option[0] == (ch) && option[1] == '\0')

	/* initialize this early because it has to parse options */
	gen_firm_init();

	int colorterm = detect_color_terminal();
	diagnostic_enable_color(colorterm);

	/* early options parsing (find out optimization level and OS) */
	bool argument_errors = false;
	for (int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if (arg[0] != '-')
			continue;

		const char *option = &arg[1];
		if (option[0] == 'O') {
			if (option[2] != '\0')
				goto invalid_o_option;
			char opt = option[1];
			if (opt == 's') {
				opt_level = 2; /* for now until we have a real -Os */
			} else if (opt >= '0' && opt <= '9') {
				opt_level = opt - '0';
			} else {
invalid_o_option:
				errorf(NULL, "invalid optimization option '%s'", arg);
				argument_errors = true;
				continue;
			}
		}
	}

	if (target.machine == NULL) {
		target.machine = firm_get_host_machine();
	}
	choose_optimization_pack(opt_level);
	setup_target_machine();

	/* parse rest of options */
	compilation_unit_type_t forced_unittype = COMPILATION_UNIT_AUTODETECT;
	help_sections_t         help            = HELP_NONE;
	for (int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if (arg[0] == '-' && arg[1] != '\0') {
			/* an option */
			const char *option = &arg[1];
			if (option[0] == 'o') {
				GET_ARG_AFTER(outname, "-o");
			} else if (option[0] == 'g') {
				/* TODO: parse -gX with 0<=X<=3... */
				set_be_option("debug=frameinfo");
				set_be_option("ia32-optcc=false");
			} else if (SINGLE_OPTION('c')) {
				mode = MODE_COMPILE_ASSEMBLE;
			} else if (SINGLE_OPTION('E')) {
				mode = MODE_PREPROCESS_ONLY;
			} else if (SINGLE_OPTION('s')) {
				driver_add_flag(&ldflags_obst, "-s");
			} else if (SINGLE_OPTION('S')) {
				mode = MODE_COMPILE;
			} else if (option[0] == 'O') {
				continue;
			} else if (option[0] == 'I') {
				const char *opt;
				GET_ARG_AFTER(opt, "-I");
				driver_add_flag(&cppflags_obst, "-I%s", opt);
				append_include_path(&bracket_searchpath, opt);
			} else if (option[0] == 'D') {
				const char *opt;
				GET_ARG_AFTER(opt, "-D");
				driver_add_flag(&cppflags_obst, "-D%s", opt);
				parse_define(opt);
			} else if (option[0] == 'U') {
				const char *opt;
				GET_ARG_AFTER(opt, "-U");
				driver_add_flag(&cppflags_obst, "-U%s", opt);
				undefine(opt);
			} else if (option[0] == 'l') {
				const char *opt;
				GET_ARG_AFTER(opt, "-l");
				driver_add_flag(&ldflags_obst, "-l%s", opt);
			} else if (option[0] == 'L') {
				const char *opt;
				GET_ARG_AFTER(opt, "-L");
				driver_add_flag(&ldflags_obst, "-L%s", opt);
			} else if (SINGLE_OPTION('v')) {
				driver_verbose = true;
			} else if (SINGLE_OPTION('w')) {
				driver_add_flag(&cppflags_obst, "-w");
				disable_all_warnings();
			} else if (option[0] == 'x') {
				const char *opt;
				GET_ARG_AFTER(opt, "-x");
				forced_unittype = get_unit_type_from_string(opt);
				if (forced_unittype == COMPILATION_UNIT_UNKNOWN) {
					errorf(NULL, "unknown language '%s'", opt);
					argument_errors = true;
				}
			} else if (SINGLE_OPTION('M')
			        || streq(option, "MM")) {
				mode = MODE_GENERATE_DEPENDENCIES;
				driver_add_flag(&cppflags_obst, "-%s", option);
			} else if (streq(option, "MMD") ||
			           streq(option, "MD")) {
				construct_dep_target = true;
				driver_add_flag(&cppflags_obst, "-%s", option);
			} else if (streq(option, "MP")) {
				driver_add_flag(&cppflags_obst, "-%s", option);
			} else if (streq(option, "MT") ||
			           streq(option, "MQ") ||
			           streq(option, "MF")) {
				const char *opt;
				GET_ARG_AFTER(opt, "-MT");
				driver_add_flag(&cppflags_obst, "-%s", option);
				driver_add_flag(&cppflags_obst, "%s", opt);
			} else if (streq(option, "include")) {
				const char *opt;
				GET_ARG_AFTER(opt, "-include");
				driver_add_flag(&cppflags_obst, "-include");
				driver_add_flag(&cppflags_obst, "%s", opt);
			} else if (streq(option, "idirafter")) {
				const char *opt;
				GET_ARG_AFTER(opt, "-idirafter");
				driver_add_flag(&cppflags_obst, "-idirafter");
				driver_add_flag(&cppflags_obst, "%s", opt);
				append_include_path(&after_searchpath, opt);
			} else if (streq(option, "isystem")) {
				const char *opt;
				GET_ARG_AFTER(opt, "-isystem");
				driver_add_flag(&cppflags_obst, "-isystem");
				driver_add_flag(&cppflags_obst, "%s", opt);
				append_include_path(&system_searchpath, opt);
			} else if (streq(option, "iquote")) {
				const char *opt;
				GET_ARG_AFTER(opt, "-iquote");
				driver_add_flag(&cppflags_obst, "-iquote");
				driver_add_flag(&cppflags_obst, "%s", opt);
				append_include_path(&quote_searchpath, opt);
			} else if (streq(option, "nostdinc")) {
				stdinc = false;
				driver_add_flag(&cppflags_obst, "%s", arg);
			} else if (streq(option, "pthread")) {
				/* set flags for the preprocessor */
				driver_add_flag(&cppflags_obst, "-D_REENTRANT");
				/* set flags for the linker */
				driver_add_flag(&ldflags_obst, "-lpthread");
			} else if (streq(option, "trigraphs")) {
				/* pass these through to the preprocessor */
				driver_add_flag(&cppflags_obst, "%s", arg);
			} else if (streq(option, "pipe")) {
				/* here for gcc compatibility */
			} else if (streq(option, "static")) {
				driver_add_flag(&ldflags_obst, "-static");
			} else if (streq(option, "shared")) {
				driver_add_flag(&ldflags_obst, "-shared");
			} else if (option[0] == 'f') {
				char const *orig_opt;
				GET_ARG_AFTER(orig_opt, "-f");

				if (strstart(orig_opt, "input-charset=")) {
					char const* const encoding = strchr(orig_opt, '=') + 1;
					input_decoder = input_get_decoder(encoding);
					if (input_decoder == NULL) {
						errorf(NULL, "input encoding \"%s\" not supported",
						       encoding);
					}
				} else if (strstart(orig_opt, "align-loops=") ||
				           strstart(orig_opt, "align-jumps=") ||
				           strstart(orig_opt, "align-functions=")) {
					fprintf(stderr, "ignoring gcc option '-f%s'\n", orig_opt);
				} else if (strstart(orig_opt, "visibility=")) {
					const char *val = strchr(orig_opt, '=')+1;
					elf_visibility_tag_t visibility
						= get_elf_visibility_from_string(val);
					if (visibility == ELF_VISIBILITY_ERROR) {
						errorf(NULL, "invalid visibility '%s' specified", val);
						argument_errors = true;
					} else {
						set_default_visibility(visibility);
					}
				} else if (strstart(orig_opt, "message-length=")) {
					/* ignore: would only affect error message format */
				} else if (streq(orig_opt, "help")) {
					fprintf(stderr, "warning: -fhelp is deprecated\n");
					help |= HELP_OPTIMIZATION;
				} else {
					/* -f options which have an -fno- variant */
					char const *opt         = orig_opt;
					bool        truth_value = true;
					if (opt[0] == 'n' && opt[1] == 'o' && opt[2] == '-') {
						truth_value = false;
						opt += 3;
					}

					if (streq(opt, "diagnostics-show-option")) {
						diagnostics_show_option = truth_value;
					} else if (streq(opt, "dollars-in-identifiers")) {
						allow_dollar_in_symbol = truth_value;
					} else if (streq(opt, "fast-math")) {
						ir_allow_imprecise_float_transforms(truth_value);
					} else if (streq(opt, "omit-frame-pointer")) {
						set_be_option(truth_value ? "omitfp" : "omitfp=no");
					} else if (streq(opt, "short-wchar")) {
						dialect.wchar_atomic_kind
							= truth_value ? ATOMIC_TYPE_USHORT
							              : ATOMIC_TYPE_INT;
					} else if (streq(opt, "show-column")) {
						show_column = truth_value;
					} else if (streq(opt, "color-diagnostics")
					        || streq(opt, "diagnostics-color")) {
						diagnostic_enable_color(truth_value
						    ? (colorterm != 0 ? colorterm : 8)
						    : 0);
					} else if (streq(opt, "signed-char")) {
						dialect.char_is_signed = truth_value;
					} else if (streq(opt, "strength-reduce")) {
						/* does nothing, for gcc compatibility (even gcc does
						 * nothing for this switch anymore) */
					} else if (streq(opt, "syntax-only")) {
						mode = truth_value ? MODE_PARSE_ONLY
						                   : MODE_COMPILE_ASSEMBLE_LINK;
					} else if (streq(opt, "unsigned-char")) {
						dialect.char_is_signed = !truth_value;
					} else if (streq(opt, "freestanding")) {
						dialect.freestanding = truth_value;
					} else if (streq(opt, "hosted")) {
						dialect.freestanding = !truth_value;
					} else if (streq(opt, "profile-generate")) {
						profile_generate = truth_value;
					} else if (streq(opt, "profile-use")) {
						profile_use = truth_value;
					} else if (!truth_value &&
					           streq(opt, "asynchronous-unwind-tables")) {
					    /* nothing todo, a gcc feature which we do not support
					     * anyway was deactivated */
					} else if (streq(opt, "verbose-asm")) {
						/* ignore: we always print verbose assembler */
					} else if (streq(opt, "jump-tables")             ||
					           streq(opt, "expensive-optimizations") ||
					           streq(opt, "common")                  ||
					           streq(opt, "optimize-sibling-calls")  ||
					           streq(opt, "align-loops")             ||
					           streq(opt, "align-jumps")             ||
					           streq(opt, "align-functions")         ||
					           streq(opt, "PIC")                     ||
					           streq(opt, "stack-protector")         ||
					           streq(opt, "stack-protector-all")) {
						fprintf(stderr, "ignoring gcc option '-f%s'\n", orig_opt);
					} else {
						if (firm_option(orig_opt) == 0) {
							errorf(NULL, "unknown Firm option '-f%s'", orig_opt);
							argument_errors = true;
							continue;
						}
					}
				}
			} else if (option[0] == 'b') {
				const char *opt;
				GET_ARG_AFTER(opt, "-b");

				if (streq(opt, "help")) {
					fprintf(stderr, "warning: -bhelp is deprecated (use --help-firm)\n");
					help |= HELP_FIRM;
				} else {
					if (be_parse_arg(opt) == 0) {
						errorf(NULL, "unknown Firm backend option '-b%s'", opt);
						argument_errors = true;
					} else if (strstart(opt, "isa=")) {
						GET_ARG_AFTER(opt, "-bisa=");
						snprintf(firm_be, sizeof(firm_be), "%s", opt);
					}
				}
			} else if (option[0] == 'W') {
				if (strstart(option + 1, "a,")) {
					const char *opt;
					GET_ARG_AFTER(opt, "-Wa,");
					driver_add_flag(&asflags_obst, "-Wa,%s", opt);
				} else if (strstart(option + 1, "p,")) {
					// pass options directly to the preprocessor
					const char *opt;
					GET_ARG_AFTER(opt, "-Wp,");
					driver_add_flag(&cppflags_obst, "-Wp,%s", opt);
				} else if (strstart(option + 1, "l,")) {
					// pass options directly to the linker
					const char *opt;
					GET_ARG_AFTER(opt, "-Wl,");
					driver_add_flag(&ldflags_obst, "-Wl,%s", opt);
				} else if (streq(option + 1, "no-trigraphs")
							|| streq(option + 1, "undef")
							|| streq(option + 1, "missing-include-dirs")
							|| streq(option + 1, "endif-labels")) {
					driver_add_flag(&cppflags_obst, "%s", arg);
				} else if (streq(option+1, "init-self")) {
					/* ignored (same as gcc does) */
				} else if (streq(option+1, "format-y2k")
				           || streq(option+1, "format-security")
				           || streq(option+1, "old-style-declaration")
				           || streq(option+1, "type-limits")) {
					/* ignore (gcc compatibility) */
				} else {
					set_warning_opt(&option[1]);
				}
			} else if (option[0] == 'm') {
				/* -m options */
				const char *opt;

				GET_ARG_AFTER(opt, "-m");
				if (strstart(opt, "target=") || strstart(opt, "triple=")) {
					GET_ARG_AFTER(opt, "-mtarget=");
					if (!parse_target_triple(opt)) {
						argument_errors = true;
					} else {
						const char *isa = setup_target_machine();
						snprintf(firm_be, sizeof(firm_be), "%s", isa);
						target.triple = opt;
					}
				} else {
					/* remember option for backend */
					assert(obstack_object_size(&codegenflags_obst) == 0);
					obstack_blank(&codegenflags_obst, sizeof(codegen_option_t));
					size_t len = strlen(opt);
					obstack_grow0(&codegenflags_obst, opt, len);
					codegen_option_t *option
						= obstack_finish(&codegenflags_obst);
					option->next            = NULL;

					*codegen_options_anchor = option;
					codegen_options_anchor  = &option->next;
				}
			} else if (option[0] == 'X') {
				if (streq(option + 1, "assembler")) {
					const char *opt;
					GET_ARG_AFTER(opt, "-Xassembler");
					driver_add_flag(&asflags_obst, "-Xassembler");
					driver_add_flag(&asflags_obst, opt);
				} else if (streq(option + 1, "preprocessor")) {
					const char *opt;
					GET_ARG_AFTER(opt, "-Xpreprocessor");
					driver_add_flag(&cppflags_obst, "-Xpreprocessor");
					driver_add_flag(&cppflags_obst, opt);
				} else if (streq(option + 1, "linker")) {
					const char *opt;
					GET_ARG_AFTER(opt, "-Xlinker");
					driver_add_flag(&ldflags_obst, "-Xlinker");
					driver_add_flag(&ldflags_obst, opt);
				}
			} else if (streq(option, "pg")) {
				set_be_option("gprof");
				driver_add_flag(&ldflags_obst, "-pg");
			} else if (streq(option, "ansi")) {
				standard = STANDARD_ANSI;
			} else if (streq(option, "pedantic")) {
				dialect.strict = true;
				set_warning_opt("pedantic");
			} else if (strstart(option, "std=")) {
				const char *const o = &option[4];
				standard =
					streq(o, "c++")            ? STANDARD_CXX98   :
					streq(o, "c++98")          ? STANDARD_CXX98   :
					streq(o, "c11")            ? STANDARD_C11     :
					streq(o, "c1x")            ? STANDARD_C11     : // deprecated
					streq(o, "c89")            ? STANDARD_C89     :
					streq(o, "c90")            ? STANDARD_C89     :
					streq(o, "c99")            ? STANDARD_C99     :
					streq(o, "c9x")            ? STANDARD_C99     : // deprecated
					streq(o, "gnu++98")        ? STANDARD_GNUXX98 :
					streq(o, "gnu11")          ? STANDARD_GNU11   :
					streq(o, "gnu1x")          ? STANDARD_GNU11   : // deprecated
					streq(o, "gnu89")          ? STANDARD_GNU89   :
					streq(o, "gnu99")          ? STANDARD_GNU99   :
					streq(o, "gnu9x")          ? STANDARD_GNU99   : // deprecated
					streq(o, "iso9899:1990")   ? STANDARD_C89     :
					streq(o, "iso9899:199409") ? STANDARD_C89AMD1 :
					streq(o, "iso9899:1999")   ? STANDARD_C99     :
					streq(o, "iso9899:199x")   ? STANDARD_C99     : // deprecated
					streq(o, "iso9899:2011")   ? STANDARD_C11     :
					(fprintf(stderr, "warning: ignoring gcc option '%s'\n", arg), standard);
			} else if (streq(option, "version")) {
				print_cparser_version();
				return EXIT_SUCCESS;
			} else if (streq(option, "dumpversion")) {
				/* gcc compatibility option */
				print_cparser_version_short();
				return EXIT_SUCCESS;
			} else if (strstart(option, "print-file-name=")) {
				GET_ARG_AFTER(print_file_name_file, "-print-file-name=");
			} else if (option[0] == 'd') {
				/* scan debug flags */
				for (const char *flag = &option[1]; *flag != '\0'; ++flag) {
					if (*flag == 'M')
						dump_defines = true;
				}
			} else if (option[0] == '-') {
				/* double dash option */
				++option;
				if (streq(option, "gcc")) {
					features_on  |=  _GNUC;
					features_off &= ~_GNUC;
				} else if (streq(option, "no-gcc")) {
					features_on  &= ~_GNUC;
					features_off |=  _GNUC;
				} else if (streq(option, "ms")) {
					features_on  |=  _MS;
					features_off &= ~_MS;
				} else if (streq(option, "no-ms")) {
					features_on  &= ~_MS;
					features_off |=  _MS;
				} else if (streq(option, "benchmark")) {
					mode = MODE_BENCHMARK_PARSER;
				} else if (streq(option, "print-ast")) {
					mode = MODE_PRINT_AST;
				} else if (streq(option, "print-implicit-cast")) {
					print_implicit_casts = true;
				} else if (streq(option, "print-parenthesis")) {
					print_parenthesis = true;
				} else if (streq(option, "print-fluffy")) {
					mode = MODE_PRINT_FLUFFY;
				} else if (streq(option, "print-compound-sizes")) {
					mode = MODE_PRINT_COMPOUND_SIZE;
				} else if (streq(option, "print-jna")) {
					mode = MODE_PRINT_JNA;
				} else if (streq(option, "jna-limit")) {
					++i;
					if (i >= argc) {
						errorf(NULL, "expected argument after '--jna-limit'");
						argument_errors = true;
						break;
					}
					jna_limit_output(argv[i]);
				} else if (streq(option, "jna-libname")) {
					++i;
					if (i >= argc) {
						errorf(NULL, "expected argument after '--jna-libname'");
						argument_errors = true;
						break;
					}
					jna_set_libname(argv[i]);
				} else if (streq(option, "external-pp")) {
					driver_use_external_preprocessor = true;
					if (i+1 < argc && argv[i+1][0] != '-') {
						++i;
						external_preprocessor = argv[i+1];
					} else {
						external_preprocessor = NULL;
					}
				} else if (streq(option, "no-external-pp")) {
					driver_use_external_preprocessor = false;
				} else if (streq(option, "time")) {
					do_timing    = true;
					print_timing = true;
				} else if (streq(option, "statev")) {
					do_timing      = true;
					produce_statev = true;
				} else if (strstart(option, "filtev=")) {
					GET_ARG_AFTER(filtev, "--filtev=");
				} else if (streq(option, "version")) {
					print_cparser_version();
					return EXIT_SUCCESS;
				} else if (streq(option, "help")) {
					help |= HELP_BASIC;
				} else if (streq(option, "help-preprocessor")) {
					help |= HELP_PREPROCESSOR;
				} else if (streq(option, "help-parser")) {
					help |= HELP_PARSER;
				} else if (streq(option, "help-warnings")) {
					help |= HELP_WARNINGS;
				} else if (streq(option, "help-codegen")) {
					help |= HELP_CODEGEN;
				} else if (streq(option, "help-linker")) {
					help |= HELP_LINKER;
				} else if (streq(option, "help-optimization")) {
					help |= HELP_OPTIMIZATION;
				} else if (streq(option, "help-language-tools")) {
					help |= HELP_LANGUAGETOOLS;
				} else if (streq(option, "help-debug")) {
					help |= HELP_DEBUG;
				} else if (streq(option, "help-firm")) {
					help |= HELP_FIRM;
				} else if (streq(option, "help-all")) {
					help |= HELP_ALL;
				} else if (streq(option, "dump-function")) {
					++i;
					if (i >= argc) {
						errorf(NULL, "expected argument after '--dump-function'");
						argument_errors = true;
						break;
					}
					dumpfunction = argv[i];
					mode         = MODE_COMPILE_DUMP;
				} else if (streq(option, "export-ir")) {
					mode = MODE_COMPILE_EXPORTIR;
				} else if (streq(option, "unroll-loops")) {
					/* ignore (gcc compatibility) */
				} else {
					errorf(NULL, "unknown argument '%s'", arg);
					argument_errors = true;
				}
			} else {
				errorf(NULL, "unknown argument '%s'", arg);
				argument_errors = true;
			}
		} else {
			/* argument is not an option but an input filename */
			compilation_unit_type_t type = forced_unittype;
			if (type == COMPILATION_UNIT_AUTODETECT && streq(arg, "-")) {
				/* - implicitly means C source file */
				type = COMPILATION_UNIT_C;
			}
			driver_add_input(arg, type);
			had_inputs = true;
		}
	}

	if (help != HELP_NONE) {
		help_print(argv[0], help);
		return argument_errors ? EXIT_FAILURE : EXIT_SUCCESS;
	}

	if (print_file_name_file != NULL) {
		driver_print_file_name(print_file_name_file);
		return EXIT_SUCCESS;
	}

	/* pass options to firm backend (this happens delayed because we first
	 * had to decide which backend is actually used) */
	for (codegen_option_t *option = codegen_options; option != NULL;
	     option = option->next) {
		char        buf[256];
	    const char *opt = option->option;
	    /* pass option along to firm backend (except the -m32, -m64 stuff) */
	    if (opt[0] < '0' || opt[0] > '9') {
			snprintf(buf, sizeof(buf), "%s-%s", firm_be, opt);
			if (be_parse_arg(buf) == 0) {
				errorf(NULL, "Unknown codegen option '-m%s'", opt);
				argument_errors = true;
				continue;
			}
		}

		/* hack to emulate the behaviour of some gcc spec files which filter
		 * flags to pass to cpp/ld/as */
		static char const *const pass_to_cpp_and_ld[] = {
			"soft-float", "32", "64", "16"
		};
		for (size_t i = 0; i < ARRAY_SIZE(pass_to_cpp_and_ld); ++i) {
			if (streq(pass_to_cpp_and_ld[i], option->option)) {
				snprintf(buf, sizeof(buf), "-m%s", option->option);
				driver_add_flag(&cppflags_obst, buf);
				driver_add_flag(&asflags_obst, buf);
				driver_add_flag(&ldflags_obst, buf);
				break;
			}
		}
	}

	if (!had_inputs) {
		errorf(NULL, "no input files specified");
		argument_errors = true;
	}

	if (argument_errors) {
		help_usage(argv[0]);
		return EXIT_FAILURE;
	}

	init_driver_tools();

	/* apply some effects from switches */
	if (profile_generate) {
		driver_add_flag(&ldflags_obst, "-lfirmprof");
		set_be_option("profilegenerate");
	}
	if (profile_use) {
		set_be_option("profileuse");
	}

	/* TODO/FIXME we should have nothing depending on c dialect before we
	 * are processing the first source file... */
	init_c_dialect(false, standard != STANDARD_DEFAULT ? standard
	                                                   : STANDARD_GNU99);
	init_types_and_adjust();
	init_typehash();
	init_basic_types();
	if (dialect.cpp) {
		init_wchar_types(ATOMIC_TYPE_WCHAR_T);
	} else {
		init_wchar_types(dialect.wchar_atomic_kind);
	}
	if (stdinc)
		append_standard_include_paths();
	append_environment_include_paths();
	init_preprocessor();
	if (driver_use_external_preprocessor)
		init_external_preprocessor();
	init_ast();
	init_constfold();
	init_parser();
	init_ast2firm();
	init_mangle();

	if (do_timing)
		timer_init();

	int result = driver_go();

	if (do_timing)
		timer_term(print_timing ? stderr : NULL);

	free_temp_files();
	obstack_free(&file_obst, NULL);

	gen_firm_finish();
	exit_mangle();
	exit_ast2firm();
	exit_parser();
	exit_ast();
	exit_preprocessor();
	exit_typehash();
	exit_types();
	exit_tokens();
	exit_symbol_table();
	exit_driver();
	return result;
}
