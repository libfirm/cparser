/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#include "enable_posix.h"
#include "driver.h"

#include <assert.h>
#include <errno.h>
#include <libfirm/statev.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "adt/error.h"
#include "adt/strutil.h"
#include "adt/util.h"
#include "ast2firm.h"
#include "diagnostic.h"
#include "driver/firm_opt.h"
#include "driver/firm_timing.h"
#include "lang_features.h"
#include "parser.h"
#include "predefs.h"
#include "preprocessor.h"
#include "printer.h"
#include "target.h"
#include "type_t.h"
#include "types.h"
#include "version.h"
#include "wrappergen/write_compoundsizes.h"
#include "wrappergen/write_fluffy.h"
#include "wrappergen/write_jna.h"

#ifndef PREPROCESSOR
#define PREPROCESSOR "gcc -E"
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
#ifndef SYSTEM_INCLUDE_DIR
#define SYSTEM_INCLUDE_DIR NULL
#endif

static const char *compiler_include_dir = COMPILER_INCLUDE_DIR;
static const char *local_include_dir    = LOCAL_INCLUDE_DIR;
static const char *system_include_dir   = SYSTEM_INCLUDE_DIR;

compile_mode_t  mode = MODE_COMPILE_ASSEMBLE_LINK;
const char     *outname;
struct obstack  cppflags_obst;
struct obstack  c_cpp_cppflags_obst;
struct obstack  ldflags_obst;
struct obstack  asflags_obst;
struct obstack  codegenflags_obst;
unsigned        features_on;
unsigned        features_off;
bool            construct_dep_target;
bool            dump_defines;
bool            produce_statev;
bool            print_timing;
bool            do_timing;
const char     *filtev;
const char     *dumpfunction;
lang_standard_t standard;
int             colorterm;
int             driver_use_integrated_preprocessor = -1;
bool            driver_verbose;
bool            driver_no_stdinc;
const char     *driver_linker;
const char     *driver_preprocessor;
const char     *driver_assembler;
const char     *driver_default_exe_output = "a.out";

static const char         *asflags;
static compilation_unit_t *units;
static compilation_unit_t *last_unit;
static struct obstack      file_obst;

void driver_add_flag(struct obstack *obst, const char *format, ...)
{
	char buf[65536];
	va_list ap;

	va_start(ap, format);
#ifdef _WIN32
	int len =
#endif
		vsnprintf(buf, sizeof(buf), format, ap);
	va_end(ap);

	obstack_1grow(obst, ' ');
#ifdef _WIN32
	obstack_1grow(obst, '"');
	obstack_grow(obst, buf, len);
	obstack_1grow(obst, '"');
#else
	/* escape stuff... */
	for (char *c = buf; *c != '\0'; ++c) {
		switch (*c) {
		case ' ':
		case '"':
		case '$':
		case '&':
		case '(':
		case ')':
		case ';':
		case '<':
		case '>':
		case '\'':
		case '\\':
		case '\n':
		case '\r':
		case '\t':
		case '`':
		case '|':
			obstack_1grow(obst, '\\');
			/* FALLTHROUGH */
		default:
			obstack_1grow(obst, *c);
			break;
		}
	}
#endif
}

static void get_output_name(char *buf, size_t buflen, const char *inputname,
                            const char *newext)
{
	if (inputname == NULL)
		inputname = "a";

	char const *const last_slash = strrchr(inputname, '/');
	char const *const filename   =
		last_slash != NULL ? last_slash + 1 : inputname;
	char const *const last_dot   = strrchr(filename, '.');
	char const *const name_end   =
		last_dot != NULL ? last_dot : strchr(filename, '\0');

	int const len = snprintf(buf, buflen, "%.*s%s",
			(int)(name_end - filename), filename, newext);
#ifdef _WIN32
	if (len < 0 || buflen <= (size_t)len)
#else
	if (buflen <= (size_t)len)
#endif
		panic("filename too long");
}

static FILE *open_temp_file(const char *basename, const char *extension,
                            const char **final_name)
{
	char tmpname[512];
	char uniquenum_extension[32];
	static unsigned nextnum = 0;
	snprintf(uniquenum_extension, sizeof(uniquenum_extension), "-%u%s",
	         nextnum++, extension);
	get_output_name(tmpname, sizeof(tmpname), basename, uniquenum_extension);
	return make_temp_file(tmpname, final_name);
}

static bool close_input(compilation_unit_t *unit)
{
	assert(unit->input);
	bool res;
	if (unit->input == stdin) {
		res = true;
	} else if (unit->input_is_pipe) {
		res = pclose(unit->input) == EXIT_SUCCESS;
	} else {
		fclose(unit->input);
		res = true;
	}
	unit->input = NULL;
	unit->name  = NULL;
	return res;
}

static char const* str_lang_standard(lang_standard_t const standard)
{
	switch (standard) {
	case STANDARD_C89:     return "c89";
	case STANDARD_C89AMD1: return "iso9899:199409";
	case STANDARD_C99:     return "c99";
	case STANDARD_C11:     return "c11";
	case STANDARD_GNU89:   return "gnu89";
	case STANDARD_GNU99:   return "gnu99";
	case STANDARD_GNU11:   return "gnu11";
	case STANDARD_CXX98:   return "c++98";
	case STANDARD_GNUXX98: return "gnu++98";
	case STANDARD_ANSI:    break;
	case STANDARD_DEFAULT: break;
	}
	panic("invalid standard");
}

static void copy_file(FILE *dest, FILE *input)
{
	char buf[16384];

	for (;;) {
		size_t bytes_read = fread(buf, 1, sizeof(buf), input);
		if (bytes_read == 0)
			break;
		if (fwrite(buf, 1, bytes_read, dest) != bytes_read) {
			perror("could not write output");
		}
	}
}

static const char *type_to_string(const type_t *type)
{
	assert(type->kind == TYPE_ATOMIC);
	return get_atomic_kind_name(type->atomic.akind);
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

	if (!dialect.gnu && !dialect.ms && !dialect.cpp)
		driver_add_flag(o, "-U__STRICT_ANSI__");
	driver_add_flag(o, "-U__BLOCKS__");

	if (dialect.intmax_predefs) {
		driver_add_flag(o, "-U__INTMAX_TYPE__");
		driver_add_flag(o, "-D__INTMAX_TYPE__=%s",
						type_to_string(type_intmax_t));
		driver_add_flag(o, "-U__UINTMAX_TYPE__");
		driver_add_flag(o, "-D__UINTMAX_TYPE__=%s",
						type_to_string(type_uintmax_t));
	}
}

static bool run_external_preprocessor(compilation_unit_t *unit, FILE *out,
                                      compile_mode_t mode)
{
	init_external_preprocessor();

	obstack_1grow(&cppflags_obst, '\0');
	const char *flags = obstack_finish(&cppflags_obst);

	obstack_printf(&cppflags_obst, "%s", driver_preprocessor);

	char const *lang;
	switch (unit->type) {
	case COMPILATION_UNIT_C:         lang = "c";                  break;
	case COMPILATION_UNIT_CXX:       lang = "c++";                break;
	case COMPILATION_UNIT_ASSEMBLER: lang = "assembler-with-cpp"; break;
	default:                         lang = NULL;                 break;
	}
	if (lang)
		driver_add_flag(&cppflags_obst, "-x%s", lang);

	if (unit->type == COMPILATION_UNIT_C
	    || unit->type == COMPILATION_UNIT_CXX) {
	    obstack_1grow(&c_cpp_cppflags_obst, '\0');
	    size_t      len        = obstack_object_size(&c_cpp_cppflags_obst)-1;
	    const char *extraflags = obstack_finish(&c_cpp_cppflags_obst);
	    obstack_1grow(&cppflags_obst, ' ');
	    obstack_grow(&cppflags_obst, extraflags, len);
	    driver_add_flag(&cppflags_obst, "-std=%s",
		                str_lang_standard(unit->standard));
	}
	if (flags[0] != '\0') {
		size_t len = strlen(flags);
		obstack_1grow(&cppflags_obst, ' ');
		obstack_grow(&cppflags_obst, flags, len);
	}
	if (!is_warn_on(WARN_TRIGRAPHS))
		driver_add_flag(&cppflags_obst, "-Wno-trigraphs");
	if (is_warn_on(WARN_UNDEF))
		driver_add_flag(&cppflags_obst, "-Wundef");
	if (is_warn_on(WARN_MISSING_INCLUDE_DIRS))
		driver_add_flag(&cppflags_obst, "-Wmissing-include-dirs");
	if (is_warn_on(WARN_ENDIF_LABELS))
		driver_add_flag(&cppflags_obst, "-Wendif-labels");
	if (is_warn_on(WARN_COMMENT))
		driver_add_flag(&cppflags_obst, "-Wcomment");

	/* handle dependency generation */
	if (construct_dep_target) {
		static char dep_target[4096];
		if (outname != 0) {
			size_t len = strlen(outname);
			len = MIN(len, sizeof(dep_target) - 4); /* leave room for .d extension */
			memcpy(dep_target, outname, len);
			/* replace extension with .d if found */
			char *dot = &dep_target[len-1];
			for ( ; dot >= dep_target && *dot != '/'; --dot) {
				if (*dot == '.') {
					dot[1] = 'd';
					len = (dot-dep_target)+2;
					break;
				}
			}
			dep_target[len] = '\0';
		} else {
			get_output_name(dep_target, sizeof(dep_target), unit->name, ".d");
		}

		driver_add_flag(&cppflags_obst, "-MF");
		driver_add_flag(&cppflags_obst, dep_target);
	}
	assert(unit->input == NULL);
	driver_add_flag(&cppflags_obst, unit->name);
	obstack_1grow(&cppflags_obst, '\0');

	char *commandline = obstack_finish(&cppflags_obst);
	if (driver_verbose) {
		puts(commandline);
	}
	FILE *f = popen(commandline, "r");
	if (f == NULL) {
		fprintf(stderr, "%s: error: invoking preprocessor failed\n",
		        unit->name);
		return false;
	}
	/* we do not really need that anymore */
	obstack_free(&cppflags_obst, commandline);

	unit->input         = f;
	unit->input_is_pipe = true;
	bool res            = true;
	if (mode == MODE_GENERATE_DEPENDENCIES) {
		unit->type = COMPILATION_UNIT_DEPENDENCIES;
		copy_file(out, unit->input);
		res &= close_input(unit);
	} else {
		if (mode == MODE_PREPROCESS_ONLY) {
			copy_file(out, unit->input);
			res &= close_input(unit);
		}
		switch (unit->type) {
		case COMPILATION_UNIT_ASSEMBLER:
			unit->type = COMPILATION_UNIT_PREPROCESSED_ASSEMBLER;
			break;
		case COMPILATION_UNIT_C:
			unit->type = COMPILATION_UNIT_PREPROCESSED_C;
			break;
		case COMPILATION_UNIT_CXX:
			unit->type = COMPILATION_UNIT_PREPROCESSED_CXX;
			break;
		default:
			unit->type = COMPILATION_UNIT_UNKNOWN;
			break;
		}
	}

	return res;
}

static bool assemble(compilation_unit_t *unit, const char *out, const char *in)
{
	if (asflags == NULL) {
		obstack_1grow(&asflags_obst, '\0');
		asflags = obstack_finish(&asflags_obst);
	}

	obstack_printf(&asflags_obst, "%s", driver_assembler);
	if (asflags[0] != '\0')
		obstack_printf(&asflags_obst, " %s", asflags);

	obstack_printf(&asflags_obst, " %s -o %s", in, out);
	obstack_1grow(&asflags_obst, '\0');

	char *commandline = obstack_finish(&asflags_obst);
	if (driver_verbose) {
		puts(commandline);
	}
	int err = system(commandline);
	if (err != EXIT_SUCCESS) {
		fprintf(stderr, "%s: error: assembler reported an error\n", in);
		return false;
	}
	obstack_free(&asflags_obst, commandline);
	unit->type = COMPILATION_UNIT_OBJECT;
	return true;
}

compilation_unit_type_t get_unit_type_from_string(const char *string)
{
	if (streq(string, "c") || streq(string, "c-header"))
		return COMPILATION_UNIT_C;
	if (streq(string, "c++") || streq(string, "c++-header"))
		return COMPILATION_UNIT_CXX;
	if (streq(string, "assembler"))
		return COMPILATION_UNIT_PREPROCESSED_ASSEMBLER;
	if (streq(string, "assembler-with-cpp"))
		return COMPILATION_UNIT_ASSEMBLER;
	if (streq(string, "none"))
		return COMPILATION_UNIT_AUTODETECT;

	return COMPILATION_UNIT_UNKNOWN;
}

static void init_c_dialect(bool is_cpp, lang_standard_t standard)
{
	lang_features_t features = 0;
	if (!is_cpp) {
		switch (standard) {
		case STANDARD_C89:     features = _C89;                       break;
							   /* TODO determine difference between these two */
		case STANDARD_C89AMD1: features = _C89;                       break;
		case STANDARD_C99:     features = _C89 | _C99;                break;
		case STANDARD_C11:     features = _C89 | _C99 | _C11;         break;
		case STANDARD_GNU89:   features = _C89 |               _GNUC; break;
		case STANDARD_GNU11:   features = _C89 | _C99 | _C11 | _GNUC; break;

		case STANDARD_ANSI:
		case STANDARD_CXX98:
		case STANDARD_GNUXX98:
		case STANDARD_DEFAULT:
			fprintf(stderr, "warning: command line option \"-std=%s\" is not valid for C\n", str_lang_standard(standard));
			/* FALLTHROUGH */
		case STANDARD_GNU99:   features = _C89 | _C99 | _GNUC; break;
		default:
			panic("invalid standard");
		}
	} else {
		switch (standard) {
		case STANDARD_CXX98: features = _CXX; break;

		case STANDARD_ANSI:
		case STANDARD_C89:
		case STANDARD_C89AMD1:
		case STANDARD_C99:
		case STANDARD_C11:
		case STANDARD_GNU89:
		case STANDARD_GNU99:
		case STANDARD_GNU11:
		case STANDARD_DEFAULT:
			fprintf(stderr, "warning: command line option \"-std=%s\" is not valid for C++\n", str_lang_standard(standard));
			/* FALLTHROUGH */
		case STANDARD_GNUXX98: features = _CXX | _GNUC; break;
		default:
			panic("invalid standard");
		}
	}

	features |= features_on;
	features &= ~features_off;
	dialect.features = features;

	dialect.c89 = dialect.c99 = dialect.c11
	    = dialect.gnu = dialect.ms = dialect.cpp = false;
	if (features & _C11)
	    dialect.c89 = dialect.c99 = dialect.c11 = true;
	if (features & _C99)
	    dialect.c89 = dialect.c99 = true;
	if (features & _C89)
	    dialect.c89 = true;
	if (features & _GNUC)
	    dialect.gnu = true;
	if (features & _MS)
	    dialect.ms = true;
	if (features & _CXX)
	    dialect.c89 = dialect.cpp = true;

	init_types_dialect();
}

static void init_c_dialect_for_unit(const compilation_unit_t *unit)
{
	compilation_unit_type_t type = unit->type;
	bool                    is_cpp;
	if (type == COMPILATION_UNIT_PREPROCESSED_C || type == COMPILATION_UNIT_C
	 || type == COMPILATION_UNIT_LEXER_TOKENS_C) {
		is_cpp = false;
	} else if (type == COMPILATION_UNIT_PREPROCESSED_CXX
	        || type == COMPILATION_UNIT_CXX
	        || type == COMPILATION_UNIT_LEXER_TOKENS_CXX) {
		is_cpp = true;
	} else {
		panic("can't determine c mode from unit type");
	}
	init_c_dialect(is_cpp, unit->standard);
}

static void determine_unit_standard(compilation_unit_t *unit,
                                    lang_standard_t standard)
{
	unit->standard = standard;
	switch (standard) {
	case STANDARD_ANSI:
		switch (unit->type) {
		case COMPILATION_UNIT_C:
		case COMPILATION_UNIT_PREPROCESSED_C:
			unit->standard = STANDARD_C89;
			break;
		case COMPILATION_UNIT_CXX:
		case COMPILATION_UNIT_PREPROCESSED_CXX:
			unit->standard = STANDARD_CXX98;
			break;
		default:
			break;
		}
		break;

	case STANDARD_DEFAULT:
		switch (unit->type) {
		case COMPILATION_UNIT_C:
		case COMPILATION_UNIT_PREPROCESSED_C:
			unit->standard = STANDARD_GNU99;
			break;
		case COMPILATION_UNIT_CXX:
		case COMPILATION_UNIT_PREPROCESSED_CXX:
			unit->standard = STANDARD_GNUXX98;
			break;
		default:
			break;
		}
		break;

	default:
		break;
	}
}

static bool open_input(compilation_unit_t *unit)
{
	/* input already available as FILE? */
	if (unit->input != NULL)
		return true;

	const char *const inputname = unit->name;
	unit->input_is_pipe = false;
	if (streq(inputname, "-")) {
		unit->input   = stdin;
	} else {
		unit->input = fopen(inputname, "r");
		if (unit->input == NULL) {
			fprintf(stderr, "%s: error: could not open: %s\n", inputname,
			        strerror(errno));
			return false;
		}
	}
	return true;
}

static void append_standard_include_paths(void)
{
	if (compiler_include_dir != NULL)
		append_include_path(&system_searchpath, compiler_include_dir);
#ifdef APPEND_MULTILIB_DIRS
	assert(obstack_object_size(&file_obst) == 0);
	const char *triple = multilib_directory_target_triple != NULL
					   ? multilib_directory_target_triple : target.triple;
	if (triple != NULL) {
		obstack_printf(&file_obst, "%s/%s", local_include_dir, triple);
		obstack_1grow(&file_obst, '\0');
		char *path = obstack_finish(&file_obst);
		append_include_path(&system_searchpath, path);
	}
#endif
	if (local_include_dir != NULL)
		append_include_path(&system_searchpath, local_include_dir);
#ifdef APPEND_MULTILIB_DIRS
	if (triple != NULL) {
		obstack_printf(&file_obst, "%s/%s", system_include_dir, triple);
		obstack_1grow(&file_obst, '\0');
		char *path = obstack_finish(&file_obst);
		append_include_path(&system_searchpath, path);
	}
#endif
	if (system_include_dir != NULL)
		append_include_path(&system_searchpath, system_include_dir);
}

static void append_environment_include_paths(void)
{
	append_env_paths(&bracket_searchpath, "CPATH");
	append_env_paths(&system_searchpath,
	                 dialect.cpp ? "CPLUS_INCLUDE_PATH" : "C_INCLUDE_PATH");
}

static bool start_preprocessing(compilation_unit_t *unit, FILE *out,
                                compile_mode_t mode)
{
	if (!open_input(unit))
		return false;

	if (!driver_no_stdinc)
		append_standard_include_paths();
	append_environment_include_paths();
	init_preprocessor();
	if (driver_verbose)
		print_include_paths();

	add_predefined_macros();
	if (mode == MODE_PREPROCESS_ONLY) {
		set_preprocessor_output(out);
		/* just here for gcc compatibility */
		fprintf(out, "# 1 \"%s\"\n", unit->name);
		fprintf(out, "# 1 \"<built-in>\"\n");
		fprintf(out, "# 1 \"<command-line>\"\n");
	}
	input_t *decoder = input_from_stream(unit->input, input_decoder);
	switch_pp_input(decoder, unit->name, NULL, false);
	unit->input_decoder = decoder;

	switch (unit->type) {
	case COMPILATION_UNIT_C:
	case COMPILATION_UNIT_PREPROCESSED_C:
		unit->type = COMPILATION_UNIT_LEXER_TOKENS_C;
		break;
	case COMPILATION_UNIT_CXX:
	case COMPILATION_UNIT_PREPROCESSED_CXX:
		unit->type = COMPILATION_UNIT_LEXER_TOKENS_CXX;
		break;
	case COMPILATION_UNIT_ASSEMBLER:
		unit->type = COMPILATION_UNIT_LEXER_TOKENS_ASSEMBLER;
		break;
	default:
		panic("invalid input for preprocessing");
	}
	return true;
}

static bool finish_preprocessing(compilation_unit_t *unit)
{
	close_pp_input();
	input_free(unit->input_decoder);
	set_preprocessor_output(NULL);
	bool res = close_input(unit);
	if (!res || error_count > 0)
		return false;

	if (dump_defines)
		print_defines();
	return true;
}

static void print_error_summary(void)
{
	if (error_count > 0) {
		/* parsing failed because of errors */
		fprintf(stderr, "%u error(s), %u warning(s)\n", error_count,
				warning_count);
	} else if (warning_count > 0) {
		fprintf(stderr, "%u warning(s)\n", warning_count);
	}
}

static bool print_preprocessing_tokens(compilation_unit_t *unit, FILE *out)
{
	for (;;) {
		next_preprocessing_token();
		if (pp_token.kind == T_EOF)
			break;
		emit_pp_token();
	}

	fputc('\n', out);
	check_unclosed_conditionals();
	bool res = finish_preprocessing(unit);
	print_error_summary();

	if (unit->type == COMPILATION_UNIT_LEXER_TOKENS_C) {
		unit->type = COMPILATION_UNIT_PREPROCESSED_C;
	} else if (unit->type == COMPILATION_UNIT_LEXER_TOKENS_CXX) {
		unit->type = COMPILATION_UNIT_PREPROCESSED_CXX;
	} else {
		assert(unit->type == COMPILATION_UNIT_LEXER_TOKENS_ASSEMBLER);
		unit->type = COMPILATION_UNIT_PREPROCESSED_ASSEMBLER;
	}
	return res;
}

static bool do_parsing(compilation_unit_t *unit, compile_mode_t mode)
{
	ir_timer_t *t_parsing = ir_timer_new();
	timer_register(t_parsing, "Frontend: Parsing");
	timer_start(t_parsing);

	start_parsing();

	parse();
	unit->ast = finish_parsing();
	check_unclosed_conditionals();
	bool res = finish_preprocessing(unit);
	print_error_summary();

	unit->type         = COMPILATION_UNIT_AST;
	timer_stop(t_parsing);
	if (stat_ev_enabled) {
		stat_ev_dbl("time_parsing", ir_timer_elapsed_sec(t_parsing));
	}

	return (res && error_count == 0) || mode == MODE_PRINT_AST;
}

static void node_counter(ir_node *node, void *env)
{
	(void)node;
	unsigned long long *count = (unsigned long long*)env;
	++(*count);
}

static unsigned long long count_firm_nodes(void)
{
	unsigned long long count = 0;

	int n_irgs = get_irp_n_irgs();
	for (int i = 0; i < n_irgs; ++i) {
		ir_graph *irg = get_irp_irg(i);
		irg_walk_graph(irg, node_counter, NULL, &count);
	}
	return count;
}

static bool already_constructed_firm = false;

static bool build_firm_ir(compilation_unit_t *unit)
{
	ir_timer_t *t_construct = ir_timer_new();
	timer_register(t_construct, "Frontend: Graph construction");
	timer_start(t_construct);
	if (already_constructed_firm) {
		panic("compiling multiple files/translation units not possible");
	}
	already_constructed_firm = true;
	init_implicit_optimizations();
	translation_unit_to_firm(unit->ast);
	timer_stop(t_construct);
	if (stat_ev_enabled) {
		stat_ev_dbl("time_graph_construction",
					ir_timer_elapsed_sec(t_construct));
		stat_ev_int("size_graph_construction", count_firm_nodes());
	}
	unit->type = COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION;
	return true;
}

static bool read_ir_file(compilation_unit_t *unit)
{
	if (!open_input(unit))
		return false;
	if (ir_import_file(unit->input, unit->name)) {
		position_t const pos = { unit->name, 0, 0, 0 };
		errorf(&pos, "import of firm graph failed");
		return false;
	}
	already_constructed_firm = true;
	unit->type = COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION;
	return true;
}

static int compilation_loop(compile_mode_t mode, compilation_unit_t *units,
                            lang_standard_t standard, FILE *out)
{
	int  result = EXIT_SUCCESS;

	for (compilation_unit_t *unit = units; unit != NULL; unit = unit->next) {
		const char *const inputname = unit->name ?
			unit->name : "stdin.c";

		determine_unit_standard(unit, standard);

		stat_ev_ctx_push_str("compilation_unit", inputname);

again:
		switch (unit->type) {
		case COMPILATION_UNIT_IR:
			if (!read_ir_file(unit)) {
				result = EXIT_FAILURE;
				break;
			}
			goto again;

		case COMPILATION_UNIT_C:
		case COMPILATION_UNIT_CXX:
			init_c_dialect_for_unit(unit);
			/* FALLTHROUGH */
		case COMPILATION_UNIT_ASSEMBLER:
			if (driver_use_integrated_preprocessor == -1) {
				/* don't use the integrated preprocessor when crosscompiling
				 * since we probably don't have the correct location of the
				 * system headers compiled in. */
				driver_use_integrated_preprocessor = target.triple == NULL;
			}
			bool (*preproc)(compilation_unit_t*, FILE*, compile_mode_t)
				= driver_use_integrated_preprocessor
				? start_preprocessing : run_external_preprocessor;
			if (!preproc(unit, out, mode)) {
				result = EXIT_FAILURE;
				break;
			}
			goto again;

		case COMPILATION_UNIT_LEXER_TOKENS_C:
		case COMPILATION_UNIT_LEXER_TOKENS_CXX:
			init_c_dialect_for_unit(unit);
			/* FALLTHROUGH */
		case COMPILATION_UNIT_LEXER_TOKENS_ASSEMBLER:
			if (mode == MODE_PREPROCESS_ONLY) {
				if (!print_preprocessing_tokens(unit, out)) {
					result = EXIT_FAILURE;
					break;
				}
			} else if (mode == MODE_GENERATE_DEPENDENCIES) {
				position_t const pos = { inputname, 0, 0, 0 };
				errorf(&pos, "builtin preprocessor does not support dependency generation");
				result = EXIT_FAILURE;
				break;
			} else if (unit->type == COMPILATION_UNIT_LEXER_TOKENS_C
			        || unit->type == COMPILATION_UNIT_LEXER_TOKENS_CXX) {
				if (!do_parsing(unit, mode)) {
					result = EXIT_FAILURE;
					break;
				}
			}
			goto again;

		case COMPILATION_UNIT_PREPROCESSED_C:
		case COMPILATION_UNIT_PREPROCESSED_CXX:
			init_c_dialect_for_unit(unit);
			if (mode == MODE_PREPROCESS_ONLY)
				break;
			if (!start_preprocessing(unit, out, mode)) {
				result = EXIT_FAILURE;
				break;
			}
			goto again;

		case COMPILATION_UNIT_AST:
			if (mode == MODE_PRINT_AST) {
				print_to_file(out);
				print_ast(unit->ast);
				if (error_count > 0)
					result = EXIT_FAILURE;
				break;
			} else if (mode == MODE_BENCHMARK_PARSER) {
				break;
			} else if (mode == MODE_PRINT_FLUFFY) {
				write_fluffy_decls(out, unit->ast);
				break;
			} else if (mode == MODE_PRINT_JNA) {
				write_jna_decls(out, unit->ast);
				break;
			} else if (mode == MODE_PRINT_COMPOUND_SIZE) {
				write_compoundsizes(out, unit->ast);
				break;
			}

			if (!build_firm_ir(unit)) {
				result = EXIT_FAILURE;
				break;
			}
			goto again;

		case COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION:
			if (mode == MODE_PARSE_ONLY || mode == MODE_COMPILE_DUMP
			 || mode == MODE_COMPILE_EXPORTIR)
				break;

			FILE *asm_out;
			if (mode == MODE_COMPILE) {
				asm_out = out;
			} else {
				asm_out = open_temp_file(inputname, ".s", &unit->name);
			}
			ir_timer_t *t_opt_codegen = ir_timer_new();
			timer_register(t_opt_codegen, "Optimization and Codegeneration");
			timer_start(t_opt_codegen);
			generate_code(asm_out, inputname);
			timer_stop(t_opt_codegen);
			if (stat_ev_enabled) {
				stat_ev_dbl("time_opt_codegen", ir_timer_elapsed_sec(t_opt_codegen));
			}
			if (asm_out != out) {
				fclose(asm_out);
			}
			unit->type = COMPILATION_UNIT_PREPROCESSED_ASSEMBLER;
			goto again;

		case COMPILATION_UNIT_PREPROCESSED_ASSEMBLER:
			if (mode != MODE_COMPILE_ASSEMBLE
			 && mode != MODE_COMPILE_ASSEMBLE_LINK)
				break;

			/* assemble */
			const char *input = unit->name;
			if (mode == MODE_COMPILE_ASSEMBLE) {
				fclose(out);
				unit->name = outname;
			} else {
				FILE *tempf = open_temp_file(inputname, ".o", &unit->name);
				/* racy/hackish... */
				fclose(tempf);
			}

			if (!assemble(unit, unit->name, input)) {
				result = EXIT_FAILURE;
				break;
			}
			goto again;

		case COMPILATION_UNIT_DEPENDENCIES:
		case COMPILATION_UNIT_UNKNOWN:
		case COMPILATION_UNIT_AUTODETECT:
		case COMPILATION_UNIT_OBJECT:
			break;
		}

		stat_ev_ctx_pop("compilation_unit");
	}
	return result;
}

static int link_program(compilation_unit_t *units)
{
	obstack_1grow(&ldflags_obst, '\0');
	const char *flags = obstack_finish(&ldflags_obst);

	/* construct commandline */
	obstack_printf(&file_obst, "%s ", driver_linker);

	for (compilation_unit_t *unit = units; unit != NULL; unit = unit->next) {
		if (unit->type != COMPILATION_UNIT_OBJECT)
			continue;

		driver_add_flag(&file_obst, "%s", unit->name);
	}

	driver_add_flag(&file_obst, "-o");
	driver_add_flag(&file_obst, outname);
	obstack_printf(&file_obst, "%s", flags);
	obstack_1grow(&file_obst, '\0');

	char *commandline = obstack_finish(&file_obst);

	if (driver_verbose) {
		puts(commandline);
	}
	int err = system(commandline);
	if (err != EXIT_SUCCESS) {
		errorf(NULL, "linker reported an error");
		return EXIT_FAILURE;
	}
	return EXIT_SUCCESS;
}

static compilation_unit_type_t autodetect_input(const char *filename)
{
	const char *suffix = strrchr(filename, '.');
	/* Ensure there is at least one char before the suffix */
	if (suffix == NULL || suffix == filename)
		return COMPILATION_UNIT_OBJECT;
	++suffix;
	return
		streq(suffix, "S")   ? COMPILATION_UNIT_ASSEMBLER              :
		streq(suffix, "a")   ? COMPILATION_UNIT_OBJECT                 :
		streq(suffix, "c")   ? COMPILATION_UNIT_C                      :
		streq(suffix, "i")   ? COMPILATION_UNIT_PREPROCESSED_C         :
		streq(suffix, "C")   ? COMPILATION_UNIT_CXX                    :
		streq(suffix, "cc")  ? COMPILATION_UNIT_CXX                    :
		streq(suffix, "cp")  ? COMPILATION_UNIT_CXX                    :
		streq(suffix, "cpp") ? COMPILATION_UNIT_CXX                    :
		streq(suffix, "CPP") ? COMPILATION_UNIT_CXX                    :
		streq(suffix, "cxx") ? COMPILATION_UNIT_CXX                    :
		streq(suffix, "c++") ? COMPILATION_UNIT_CXX                    :
		streq(suffix, "ii")  ? COMPILATION_UNIT_PREPROCESSED_CXX       :
		streq(suffix, "h")   ? COMPILATION_UNIT_C                      :
		streq(suffix, "ir")  ? COMPILATION_UNIT_IR                     :
		streq(suffix, "o")   ? COMPILATION_UNIT_OBJECT                 :
		streq(suffix, "s")   ? COMPILATION_UNIT_PREPROCESSED_ASSEMBLER :
		streq(suffix, "so")  ? COMPILATION_UNIT_OBJECT                 :
		COMPILATION_UNIT_OBJECT; /* gcc behavior: unknown file extension means
		                            object file */
}

void driver_add_input(const char *filename, compilation_unit_type_t type)
{
	if (type == COMPILATION_UNIT_AUTODETECT)
		type = autodetect_input(filename);

	compilation_unit_t *entry = OALLOCZ(&file_obst, compilation_unit_t);
	entry->name = filename;
	entry->type = type;

	if (last_unit != NULL) {
		last_unit->next = entry;
	} else {
		units = entry;
	}
	last_unit = entry;
}

static void init_driver_tools(void)
{
	assert(obstack_object_size(&file_obst) == 0);
	/* decide which linker, preprocessor, assembler to use */
	driver_preprocessor = getenv("CPARSER_PP");
	if (driver_preprocessor == NULL) {
		if (target.triple != NULL)
			obstack_printf(&file_obst, "%s-", target.triple);
		obstack_printf(&file_obst, "%s", PREPROCESSOR);
		obstack_1grow(&file_obst, '\0');
		driver_preprocessor = obstack_finish(&file_obst);
	}
	driver_assembler = getenv("CPARSER_AS");
	if (driver_assembler == NULL) {
		if (target.triple != NULL)
			obstack_printf(&file_obst, "%s-", target.triple);
		obstack_printf(&file_obst, "%s", ASSEMBLER);
		obstack_1grow(&file_obst, '\0');
		driver_assembler = obstack_finish(&file_obst);
	}
	driver_linker = getenv("CPARSER_LINK");
	if (driver_linker == NULL) {
		if (target.triple != NULL)
			obstack_printf(&file_obst, "%s-", target.triple);
		obstack_printf(&file_obst, "%s", LINKER);
		obstack_1grow(&file_obst, '\0');
		driver_linker = obstack_finish(&file_obst);
	}
}

int action_compile(const char *argv0)
{
	(void)argv0;
	if (units == NULL) {
		errorf(NULL, "no input files specified");
		return EXIT_FAILURE;
	}

	init_driver_tools();

	if (do_timing)
		timer_init();

	char outnamebuf[4096];
	if (outname == NULL) {
		const char *filename = units->name;

		switch (mode) {
		case MODE_BENCHMARK_PARSER:
		case MODE_PRINT_AST:
		case MODE_PRINT_FLUFFY:
		case MODE_PRINT_JNA:
		case MODE_PRINT_COMPOUND_SIZE:
		case MODE_PREPROCESS_ONLY:
		case MODE_GENERATE_DEPENDENCIES:
		case MODE_PARSE_ONLY:
			outname = "-";
			break;
		case MODE_COMPILE:
			get_output_name(outnamebuf, sizeof(outnamebuf), filename, ".s");
			outname = outnamebuf;
			break;
		case MODE_COMPILE_ASSEMBLE:
			get_output_name(outnamebuf, sizeof(outnamebuf), filename, ".o");
			outname = outnamebuf;
			break;
		case MODE_COMPILE_DUMP:
			get_output_name(outnamebuf, sizeof(outnamebuf), dumpfunction,
			                ".vcg");
			outname = outnamebuf;
			break;
		case MODE_COMPILE_EXPORTIR:
			get_output_name(outnamebuf, sizeof(outnamebuf), filename, ".ir");
			outname = outnamebuf;
			break;
		case MODE_COMPILE_ASSEMBLE_LINK:
			outname = driver_default_exe_output;
			break;
		}
	}

	assert(outname != NULL);

	FILE *out;
	if (streq(outname, "-")) {
		out = stdout;
	} else {
		out = fopen(outname, "w");
		if (out == NULL) {
			position_t const pos = { outname, 0, 0, 0 };
			errorf(&pos, "could not open for writing: %s", strerror(errno));
			return EXIT_FAILURE;
		}
	}

	if (produce_statev && units != NULL) {
		/* attempt to guess a good name for the file */
		const char *first_cup = units->name;
		if (first_cup != NULL) {
			const char *dot = strrchr(first_cup, '.');
			const char *pos = dot ? dot : first_cup + strlen(first_cup);
			char        buf[pos-first_cup+1];
			strncpy(buf, first_cup, pos-first_cup);
			buf[pos-first_cup] = '\0';

			stat_ev_begin(buf, filtev);
		}
	}

	int result = compilation_loop(mode, units, standard, out);
	if (stat_ev_enabled) {
		stat_ev_end();
	}

	if (result != EXIT_SUCCESS) {
		if (out != stdout)
			unlink(outname);
		return result;
	}

	/* link program file */
	if (mode == MODE_COMPILE_DUMP) {
		/* find irg */
		ident    *id     = new_id_from_str(dumpfunction);
		ir_graph *irg    = NULL;
		int       n_irgs = get_irp_n_irgs();
		for (int i = 0; i < n_irgs; ++i) {
			ir_graph *tirg   = get_irp_irg(i);
			ident    *irg_id = get_entity_ident(get_irg_entity(tirg));
			if (irg_id == id) {
				irg = tirg;
				break;
			}
		}

		if (irg == NULL) {
			errorf(NULL, "no graph for function '%s' found", dumpfunction);
			return EXIT_FAILURE;
		}

		dump_ir_graph_file(out, irg);
		fclose(out);
	} else if (mode == MODE_COMPILE_EXPORTIR) {
		ir_export_file(out);
		if (ferror(out) != 0) {
			errorf(NULL, "writing to output failed");
			return EXIT_FAILURE;
		}
	} else if (mode == MODE_COMPILE_ASSEMBLE_LINK) {
		int const link_result = link_program(units);
		if (link_result != EXIT_SUCCESS) {
			if (out != stdout)
				unlink(outname);
			return link_result;
		}
	}

	if (do_timing)
		timer_term(print_timing ? stderr : NULL);
	return EXIT_SUCCESS;
}

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

void init_driver(void)
{
	obstack_init(&codegenflags_obst);
	obstack_init(&cppflags_obst);
	obstack_init(&c_cpp_cppflags_obst);
	obstack_init(&ldflags_obst);
	obstack_init(&asflags_obst);
	obstack_init(&file_obst);

	colorterm = detect_color_terminal();
	diagnostic_enable_color(colorterm);
}

void exit_driver(void)
{
	obstack_free(&codegenflags_obst, NULL);
	obstack_free(&cppflags_obst, NULL);
	obstack_free(&c_cpp_cppflags_obst, NULL);
	obstack_free(&ldflags_obst, NULL);
	obstack_free(&asflags_obst, NULL);
	obstack_free(&file_obst, NULL);
}
