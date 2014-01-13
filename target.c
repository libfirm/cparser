/*
 * This file is part of cparser.
 * Copyright (C) 2014 Matthias Braun <matze@braunis.de>
 */
#include <libfirm/be.h>
#include <libfirm/firm.h>

#include "adt/error.h"
#include "adt/strutil.h"
#include "ast2firm.h"
#include "diagnostic.h"
#include "driver.h"
#include "lang_features.h"
#include "mangle.h"
#include "target.h"
#include "types.h"
#include "type_t.h"
#include "warning.h"

target_t    target;
const char *multilib_directory_target_triple;

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

static void setup_types(void)
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
	const char *operating_system = target.machine->operating_system;
	if (firm_is_darwin_os(operating_system)) {
		if (streq(firm_isa, "ia32")) {
			props[ATOMIC_TYPE_LONGLONG].struct_alignment    =  4;
			props[ATOMIC_TYPE_ULONGLONG].struct_alignment   =  4;
			props[ATOMIC_TYPE_DOUBLE].struct_alignment      =  4;
			props[ATOMIC_TYPE_LONG_DOUBLE].size             = 16;
			props[ATOMIC_TYPE_LONG_DOUBLE].alignment        = 16;
			props[ATOMIC_TYPE_LONG_DOUBLE].struct_alignment = 16;
		}
	} else if (firm_is_windows_os(operating_system)) {
		if (streq(firm_isa, "ia32")) {
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
	} else if (streq(firm_isa, "ia32")) {
		props[ATOMIC_TYPE_DOUBLE].struct_alignment    = 4;
		props[ATOMIC_TYPE_LONGLONG].struct_alignment  = 4;
		props[ATOMIC_TYPE_ULONGLONG].struct_alignment = 4;
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
	target.modulo_shift          = be_params->modulo_shift;
	target.float_int_overflow    = be_params->float_int_overflow;

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

static ident *compilerlib_name_mangle_default(ident *id, ir_type *mt)
{
	(void)mt;
	return id;
}

static ident *compilerlib_name_mangle_underscore(ident *id, ir_type *mt)
{
	(void)mt;
	return id_mangle3("_", id, "");
}

static void set_be_option(const char *arg)
{
	int res = be_parse_arg(arg);
	if (!res)
		panic("setting firm backend option failed");
}

static void init_os_support(void)
{
	const char *os = target.machine->operating_system;
	dialect.wchar_atomic_kind        = ATOMIC_TYPE_INT;
	dialect.intmax_predefs           = false;
	target.enable_main_collect2_hack = false;
	driver_default_exe_output        = "a.out";

	if (firm_is_elf_os(os)) {
		set_create_ld_ident(create_name_linux_elf);
		target.user_label_prefix = "";

		set_be_option("ia32-gasmode=elf");
		set_compilerlib_name_mangle(compilerlib_name_mangle_default);
	} else if (firm_is_darwin_os(os)) {
		set_create_ld_ident(create_name_macho);
		target.user_label_prefix = "_";
		dialect.intmax_predefs   = true;

		set_be_option("ia32-gasmode=macho");
		set_be_option("ia32-stackalign=4");
		set_be_option("pic=true");
		set_compilerlib_name_mangle(compilerlib_name_mangle_underscore);
	} else if (firm_is_windows_os(os)) {
		set_create_ld_ident(create_name_win32);
		dialect.wchar_atomic_kind        = ATOMIC_TYPE_USHORT;
		target.enable_main_collect2_hack = true;
		target.user_label_prefix         = "_";
		driver_default_exe_output        = "a.exe";

		set_be_option("ia32-gasmode=mingw");
		set_compilerlib_name_mangle(compilerlib_name_mangle_underscore);
	} else {
		errorf(NULL, "unknown operating system '%s' in target-triple", os);
		exit(EXIT_FAILURE);
	}
}

static unsigned get_bitsize_codegen_opt(void)
{
	unsigned size = 0;
	for (codegen_option_t *option = codegen_options; option != NULL;
	     option = option->next) {
		const char *opt = option->option;
		if (opt[0] < '0' || opt[0] > '9')
			continue;
		size = atoi(opt);
	}
	return size;
}

static void setup_isa(const char *isa)
{
	char buf[64];
	snprintf(buf, sizeof(buf), "isa=%s", isa);
	set_be_option(buf);

	strcpy(firm_isa, isa);
}

static void setup_x86_64(void)
{
	warningf(WARN_EXPERIMENTAL, NULL,
			 "the x86_64 backend is highly experimental and unfinished (consider the -m32 switch)");
	setup_isa("amd64");
}

static void setup_arm(void)
{
	warningf(WARN_EXPERIMENTAL, NULL,
			 "the arm backend is highly experimental and unfinished");
	setup_isa("arm");
}

static void setup_ia32(const char *firm_arch)
{
	set_be_option("isa=ia32");
	char buf[64];
	snprintf(buf, sizeof(buf), "ia32-arch=%s", firm_arch);
	set_be_option(buf);

	strcpy(firm_isa, "ia32");
}

static void setup_firm_isa(void)
{
	if (firm_isa[0] != '\0') {
		setup_isa(firm_isa);
		return;
	}

	const char *cpu = target.machine->cpu_type;
	if (streq(cpu, "i386") || streq(cpu, "i486")
	    || streq(cpu, "i586") || streq(cpu, "i686")) {
		if (get_bitsize_codegen_opt() == 64) {
			free(target.machine->cpu_type);
			target.machine->cpu_type = xstrdup("x86_64");
			setup_x86_64();
		} else {
			setup_ia32(cpu);
		}
	} else if (streq(cpu, "x86_64")) {
		if (get_bitsize_codegen_opt() == 32) {
			free(target.machine->cpu_type);
			target.machine->cpu_type = xstrdup("i686");
			setup_ia32("i686");
		} else {
			setup_x86_64();
		}
	} else if (streq(cpu, "sparc")) {
		setup_isa("sparc");
		const char *manufacturer = target.machine->manufacturer;
		if (streq(manufacturer, "leon") || streq(manufacturer, "invasic"))
			set_be_option("sparc-cpu=leon");
	} else if (streq(cpu, "arm")) {
		setup_arm();
	} else {
		errorf(NULL, "unknown cpu '%s' in target-triple", cpu);
		exit(EXIT_FAILURE);
	}
}

static void pass_options_to_firm_be(options_state_t *s)
{
	/* pass options to firm backend (this happens delayed because we first
	 * had to decide which backend is actually used) */
	for (codegen_option_t *option = codegen_options; option != NULL;
	     option = option->next) {
		char        buf[256];
	    const char *opt = option->option;
	    /* pass option along to firm backend (except the -m32, -m64 stuff) */
	    if (opt[0] < '0' || opt[0] > '9') {
			snprintf(buf, sizeof(buf), "%s-%s", firm_isa, opt);
			if (be_parse_arg(buf) == 0) {
				errorf(NULL, "Unknown codegen option '-m%s'", opt);
				s->argument_errors = true;
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

	if (profile_generate) {
		driver_add_flag(&ldflags_obst, "-lfirmprof");
		set_be_option("profilegenerate");
	}
	if (profile_use) {
		set_be_option("profileuse");
	}
}

void target_setup(options_state_t *options_state)
{
	if (target.machine == NULL)
		target.machine = firm_get_host_machine();

	setup_firm_isa();
	init_os_support();
	pass_options_to_firm_be(options_state);
	setup_types();

	multilib_directory_target_triple = NULL;
	if (target.triple == NULL) {
		unsigned size = be_get_backend_param()->machine_size;
#ifdef MULTILIB_M32_TRIPLE
		if (size == 32)
			multilib_directory_target_triple = MULTILIB_M32_TRIPLE;
#endif
#ifdef MULTILIB_M64_TRIPLE
		if (size == 64)
			multilib_directory_target_triple = MULTILIB_M64_TRIPLE;
#endif
	}
}
