/*
 * This file is part of cparser.
 * Copyright (C) 2014 Matthias Braun <matze@braunis.de>
 */
#include "enable_posix.h"
#include "c_driver.h"

#include <assert.h>
#include <libfirm/statev.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "adt/error.h"
#include "adt/strutil.h"
#include "adt/util.h"
#include "ast/printer.h"
#include "ast/types.h"
#include "ast/type_t.h"
#include "diagnostic.h"
#include "driver_t.h"
#include "firm/ast2firm.h"
#include "firm/firm_opt.h"
#include "lang_features.h"
#include "parser/parser.h"
#include "parser/preprocessor.h"
#include "predefs.h"
#include "target.h"
#include "timing.h"
#include "version.h"

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

c_dialect_t dialect = {
	.features       = _C89 | _C99 | _GNUC, /* TODO/FIXME should not be inited */
	.char_is_signed = true,
};

static const char *compiler_include_dir = COMPILER_INCLUDE_DIR;
static const char *local_include_dir    = LOCAL_INCLUDE_DIR;
static const char *system_include_dir   = SYSTEM_INCLUDE_DIR;

static const char *driver_linker;
static const char *driver_preprocessor;
static const char *driver_assembler;
static const char *asflags;

struct obstack  cppflags_obst;
struct obstack  c_cpp_cppflags_obst;
struct obstack  ldflags_obst;
struct obstack  asflags_obst;
struct obstack  codegenflags_obst;
bool            construct_dep_target;
int             driver_use_integrated_preprocessor = -1;
bool            driver_no_stdinc;
bool            driver_verbose;
bool            dump_defines;
bool            dump_dependencies_instead_of_preprocessing;
unsigned        features_on;
unsigned        features_off;
lang_standard_t standard;
const char     *dumpfunction;
const char     *print_file_name_file;

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

static const char *type_to_string(const type_t *type)
{
	assert(type->kind == TYPE_ATOMIC);
	return get_atomic_kind_name(type->atomic.akind);
}

static void decide_external_preprocessor(void)
{
	if (driver_preprocessor != NULL)
		return;
	driver_preprocessor = getenv("CPARSER_PP");
	if (driver_preprocessor != NULL)
		return;
	assert(obstack_object_size(&file_obst) == 0);
	if (target.triple != NULL)
		obstack_printf(&file_obst, "%s-", target.triple);
	obstack_printf(&file_obst, "%s", PREPROCESSOR);
	obstack_1grow(&file_obst, '\0');
	driver_preprocessor = obstack_finish(&file_obst);
}

static void init_external_preprocessor(void)
{
	decide_external_preprocessor();

	/* setup default defines */
	struct obstack *o = &c_cpp_cppflags_obst;
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

static void determine_unit_standard(compilation_unit_t *unit)
{
	if (unit->standard != STANDARD_DEFAULT)
		return;

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
		case COMPILATION_UNIT_ASSEMBLER:
		default:
			unit->standard = STANDARD_C89;
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
		case COMPILATION_UNIT_ASSEMBLER:
		default:
			unit->standard = STANDARD_GNU99;
			break;
		}
		break;

	default:
		break;
	}
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

static void init_c_dialect_for_unit(compilation_unit_t *unit)
{
	determine_unit_standard(unit);

	compilation_unit_type_t type = unit->type;
	bool                    is_cpp;
	if (type == COMPILATION_UNIT_PREPROCESSED_C || type == COMPILATION_UNIT_C
	 || type == COMPILATION_UNIT_LEXER_TOKENS_C) {
		is_cpp = false;
	} else if (type == COMPILATION_UNIT_PREPROCESSED_CXX
	        || type == COMPILATION_UNIT_CXX
	        || type == COMPILATION_UNIT_LEXER_TOKENS_CXX) {
		is_cpp = true;
	} else if (type == COMPILATION_UNIT_ASSEMBLER
	        || type == COMPILATION_UNIT_PREPROCESSED_ASSEMBLER
	        || type == COMPILATION_UNIT_LEXER_TOKENS_ASSEMBLER) {
	    is_cpp = false;
	} else {
		panic("can't determine c mode from unit type");
	}
	init_c_dialect(is_cpp, unit->standard);
}

static bool run_external_preprocessor(compilation_env_t *env,
                                      compilation_unit_t *unit)
{
	(void)env;
	init_c_dialect_for_unit(unit);
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
		const char *dep_target_name;
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
			dep_target_name = dep_target;
		} else {
			dep_target_name = get_output_name(unit->original_name, ".d");
		}

		driver_add_flag(&cppflags_obst, "-MF");
		driver_add_flag(&cppflags_obst, dep_target_name);
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
	if (dump_dependencies_instead_of_preprocessing) {
		unit->type = COMPILATION_UNIT_DEPENDENCIES;
	} else {
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
	return true;
}

static void decide_assembler(void)
{
	if (driver_assembler != NULL)
		return;
	driver_assembler = getenv("CPARSER_AS");
	if (driver_assembler != NULL)
		return;
	assert(obstack_object_size(&file_obst) == 0);
	if (target.triple != NULL)
		obstack_printf(&file_obst, "%s-", target.triple);
	obstack_printf(&file_obst, "%s", ASSEMBLER);
	obstack_1grow(&file_obst, '\0');
	driver_assembler = obstack_finish(&file_obst);
}

static bool assemble(compilation_unit_t *unit, const char *o_name)
{
	if (asflags == NULL) {
		obstack_1grow(&asflags_obst, '\0');
		asflags = obstack_finish(&asflags_obst);
	}

	decide_assembler();
	obstack_printf(&asflags_obst, "%s", driver_assembler);
	if (asflags[0] != '\0')
		obstack_printf(&asflags_obst, " %s", asflags);

	obstack_printf(&asflags_obst, " %s -o %s", unit->name, o_name);
	obstack_1grow(&asflags_obst, '\0');

	char *commandline = obstack_finish(&asflags_obst);
	if (driver_verbose) {
		puts(commandline);
	}
	int err = system(commandline);
	if (err != EXIT_SUCCESS) {
		fprintf(stderr, "%s: error: assembler reported an error\n", unit->name);
		unlink(o_name);
		return false;
	}
	obstack_free(&asflags_obst, commandline);
	unit->type = COMPILATION_UNIT_OBJECT;
	unit->name = o_name;
	return true;
}

bool assemble_final(compilation_env_t *env, compilation_unit_t *unit)
{
	const char *outname = env->outname;
	if (outname == NULL) {
		outname = get_output_name(unit->original_name, ".o");
	}
	return assemble(unit, outname);
}

bool assemble_intermediate(compilation_env_t *env, compilation_unit_t *unit)
{
	(void)env;
	const char *o_name;
	FILE *tempf = open_temp_file(unit->name, ".o", &o_name);
	if (tempf == NULL)
		return NULL;
	/* hackish... */
	fclose(tempf);
	return assemble(unit, o_name);
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

static bool start_preprocessing(compilation_env_t *env,
                                compilation_unit_t *unit)
{
	(void)env;
	if (!open_input(unit))
		return false;

	init_c_dialect_for_unit(unit);
	if (!driver_no_stdinc)
		append_standard_include_paths();
	append_environment_include_paths();
	setup_preprocessor();
	if (driver_verbose)
		print_include_paths();

	add_predefined_macros();
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

static bool preprocess(compilation_env_t *env, compilation_unit_t *unit)
{
	if (driver_use_integrated_preprocessor == -1) {
		/* don't use the integrated preprocessor when crosscompiling
		 * since we probably don't have the correct location of the
		 * system headers compiled in. */
		driver_use_integrated_preprocessor = target.triple == NULL;
	}
	compilation_unit_handler preprocessor
		= driver_use_integrated_preprocessor
		? start_preprocessing : run_external_preprocessor;

	return preprocessor(env, unit);
}

bool generate_dependencies(compilation_env_t *env, compilation_unit_t *unit)
{
	(void)env;
	position_t const pos = { unit->name, 0, 0, 0 };
	errorf(&pos,
		   "builtin preprocessor does not support dependency generation yet");
	return false;
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

static bool do_print_preprocessing_tokens(FILE *out, compilation_unit_t *unit)
{
	set_preprocessor_output(out);
	print_pp_header();

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
	return res;
}

bool print_preprocessing_tokens(compilation_env_t *env,
                                compilation_unit_t *unit)
{
	if (!open_output(env))
		return false;
	bool res = do_print_preprocessing_tokens(env->out, unit);
	close_output(env);
	return res;
}

static bool print_preprocessed_intermediate(compilation_env_t *env,
                                            compilation_unit_t *unit)
{
	(void)env;
	const char *s_name;
	FILE *out = open_temp_file(unit->name, ".s", &s_name);
	if (out == NULL)
		return false;
	bool res = do_print_preprocessing_tokens(out, unit);
	fclose(out);
	unit->name = s_name;
	unit->type = COMPILATION_UNIT_PREPROCESSED_ASSEMBLER;
	return res;
}

bool do_parsing(compilation_env_t *env, compilation_unit_t *unit)
{
	(void)env;
	ir_timer_t *t_parsing = ir_timer_new();
	timer_register(t_parsing, "Frontend: Parsing");
	timer_start(t_parsing);

	init_c_dialect_for_unit(unit);

	start_parsing();

	parse();
	unit->ast = finish_parsing();
	check_unclosed_conditionals();
	bool res = finish_preprocessing(unit);
	print_error_summary();

	unit->type = COMPILATION_UNIT_AST;
	timer_stop(t_parsing);
	if (stat_ev_enabled) {
		stat_ev_dbl("time_parsing", ir_timer_elapsed_sec(t_parsing));
	}

	return res && error_count == 0;
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

bool build_firm_ir(compilation_env_t *env, compilation_unit_t *unit)
{
	(void)env;
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
	if (error_count > 0)
		return false;

	return true;
}

static bool read_ir_file(compilation_env_t *env, compilation_unit_t *unit)
{
	(void)env;
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

bool do_print_ast(compilation_env_t *env, compilation_unit_t *unit)
{
	if (!open_output(env))
		return false;
	print_to_file(env->out);
	print_ast(unit->ast);
	close_output(env);
	return error_count == 0;
}

bool do_nothing(compilation_env_t *env, compilation_unit_t *unit)
{
	(void)env;
	(void)unit;
	return true;
}

static bool do_generate_code(FILE *asm_out, compilation_unit_t *unit)
{
	warn_experimental_target();

	ir_timer_t *t_opt_codegen = ir_timer_new();
	timer_register(t_opt_codegen, "Optimization and Codegeneration");
	timer_start(t_opt_codegen);
	generate_code(asm_out, unit->original_name);
	timer_stop(t_opt_codegen);
	if (stat_ev_enabled) {
		stat_ev_dbl("time_opt_codegen", ir_timer_elapsed_sec(t_opt_codegen));
	}
	unit->type = COMPILATION_UNIT_PREPROCESSED_ASSEMBLER;
	return true;
}

bool generate_code_final(compilation_env_t *env, compilation_unit_t *unit)
{
	if (!open_output_for_unit(env, unit, ".s"))
		return false;
	bool res = do_generate_code(env->out, unit);
	close_output(env);
	if (!res)
		unlink(env->outname);
	return res;
}

bool generate_code_intermediate(compilation_env_t *env,
                                compilation_unit_t *unit)
{
	(void)env;
	const char *s_name;
	FILE *asm_out = open_temp_file(unit->name, ".s", &s_name);
	if (asm_out == NULL)
		return false;
	bool result = do_generate_code(asm_out, unit);
	unit->name = s_name;
	fclose(asm_out);
	return result;
}

static void decide_linker(void)
{
	if (driver_linker != NULL)
		return;
	driver_linker = getenv("CPARSER_LINK");
	if (driver_linker != NULL)
		return;
	assert(obstack_object_size(&file_obst) == 0);
	if (target.triple != NULL)
		obstack_printf(&file_obst, "%s-", target.triple);
	obstack_printf(&file_obst, "%s", LINKER);
	obstack_1grow(&file_obst, '\0');
	driver_linker = obstack_finish(&file_obst);
}

bool link_program(compilation_env_t *env, compilation_unit_t *units)
{
	const char *outname = env->outname;
	if (outname == NULL) {
		outname = driver_default_exe_output;
	}

	obstack_1grow(&ldflags_obst, '\0');
	const char *flags = obstack_finish(&ldflags_obst);

	/* construct commandline */
	decide_linker();
	assert(obstack_object_size(&file_obst) == 0);
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
		unlink(outname);
		return false;
	}
	return true;
}

bool write_ir_file(compilation_env_t *env, compilation_unit_t *unit)
{
	if (!open_output_for_unit(env, unit, ".ir"))
		return false;
	ir_export_file(env->out);
	int errors = ferror(env->out);
	close_output(env);

	if (errors != 0) {
		errorf(NULL, "writing to output failed");
		unlink(env->outname);
		return false;
	}
	return true;
}

bool dump_irg(compilation_env_t *env, compilation_unit_t *units)
{
	(void)units;
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
		return false;
	}

	if (env->outname == NULL)
		env->outname = "a.vcg";
	if (!open_output(env))
		return false;

	dump_ir_graph_file(env->out, irg);
	close_output(env);
	return true;
}

int action_print_file_name(const char *argv0)
{
	(void)argv0;
	driver_add_flag(&ldflags_obst, "-print-file-name=%s", print_file_name_file);

	obstack_1grow(&ldflags_obst, '\0');
	const char *flags = obstack_finish(&ldflags_obst);

	/* construct commandline */
	obstack_printf(&ldflags_obst, "%s ", driver_linker);
	obstack_printf(&ldflags_obst, "%s", flags);
	obstack_1grow(&ldflags_obst, '\0');

	char *commandline = obstack_finish(&ldflags_obst);
	if (driver_verbose) {
		puts(commandline);
	}
	int err = system(commandline);
	if (err != EXIT_SUCCESS) {
		fprintf(stderr, "%s: error: linker reported an error\n",
		        print_file_name_file);
	}
	obstack_free(&ldflags_obst, commandline);
	return err;
}

void set_default_handlers(void)
{
	set_unit_handler(COMPILATION_UNIT_OBJECT,  do_nothing, true);
	set_unit_handler(COMPILATION_UNIT_UNKNOWN, do_nothing, true);

	set_unit_handler(COMPILATION_UNIT_IR,        read_ir_file, false);
	set_unit_handler(COMPILATION_UNIT_C,         preprocess,   false);
	set_unit_handler(COMPILATION_UNIT_CXX,       preprocess,   false);
	set_unit_handler(COMPILATION_UNIT_ASSEMBLER, preprocess,   false);

	set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_C,   do_parsing, false);
	set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_CXX, do_parsing, false);
	set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_ASSEMBLER,
	                 print_preprocessed_intermediate, false);

	set_unit_handler(COMPILATION_UNIT_PREPROCESSED_C,
	                 start_preprocessing, false);
	set_unit_handler(COMPILATION_UNIT_PREPROCESSED_CXX,
	                 start_preprocessing, false);
	set_unit_handler(COMPILATION_UNIT_AST, build_firm_ir, false);
	set_unit_handler(COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION,
	                 generate_code_intermediate, false);
	set_unit_handler(COMPILATION_UNIT_PREPROCESSED_ASSEMBLER,
	                 assemble_intermediate, false);
	set_unit_handler(COMPILATION_UNIT_DEPENDENCIES, do_copy_file, true);
}

void init_default_driver(void)
{
	obstack_init(&codegenflags_obst);
	obstack_init(&cppflags_obst);
	obstack_init(&c_cpp_cppflags_obst);
	obstack_init(&ldflags_obst);
	obstack_init(&asflags_obst);
}

void exit_default_driver(void)
{
	obstack_free(&codegenflags_obst, NULL);
	obstack_free(&cppflags_obst, NULL);
	obstack_free(&c_cpp_cppflags_obst, NULL);
	obstack_free(&ldflags_obst, NULL);
	obstack_free(&asflags_obst, NULL);
}
