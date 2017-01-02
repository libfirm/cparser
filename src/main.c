/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adt/panic.h"
#include "adt/strutil.h"
#include "ast/ast.h"
#include "driver/c_driver.h"
#include "driver/diagnostic.h"
#include "driver/driver.h"
#include "driver/driver_t.h"
#include "driver/help.h"
#include "driver/options.h"
#include "driver/target.h"
#include "driver/tempfile.h"
#include "driver/timing.h"
#include "firm/ast2firm.h"
#include "firm/firm_opt.h"
#include "firm/jittest.h"
#include "parser/parser.h"
#include "parser/preprocessor.h"
#include "wrappergen/write_compoundsizes.h"
#include "wrappergen/write_fluffy.h"
#include "wrappergen/write_jna.h"

typedef enum compile_mode_t {
	/* note the following is ordered according to gcc option precedence */
	MODE_GENERATE_DEPENDENCIES,
	MODE_PREPROCESS_ONLY,
	MODE_PARSE_ONLY,
	MODE_COMPILE,
	MODE_COMPILE_ASSEMBLE,

	MODE_COMPILE_ASSEMBLE_LINK,
	MODE_COMPILE_DUMP,
	MODE_COMPILE_EXPORTIR,
	MODE_BENCHMARK_PARSER,
	MODE_PRINT_AST,
	MODE_PRINT_FLUFFY,
	MODE_PRINT_JNA,
	MODE_PRINT_COMPOUND_SIZE,
	MODE_JITTEST,
} compile_mode_t;
static compile_mode_t mode = MODE_COMPILE_ASSEMBLE_LINK;

static bool print_fluffy(compilation_env_t *env, compilation_unit_t *unit)
{
	if (!open_output(env))
		return false;
	write_fluffy_decls(env->out, unit->ast);
	return true;
}

static bool print_jna(compilation_env_t *env, compilation_unit_t *unit)
{
	if (!open_output(env))
		return false;
	write_jna_decls(env->out, unit->ast);
	return true;
}

static bool print_compound_size(compilation_env_t *env,
                                compilation_unit_t *unit)
{
	if (!open_output(env))
		return false;
	write_compoundsizes(env->out, unit->ast);
	return true;
}

static bool parse_ignore_errors(compilation_env_t *env,
                                compilation_unit_t *unit)
{
	do_parsing(env, unit); /* ignore return value */
	return true;
}

static void set_mode_gcc_prec(compile_mode_t new_mode, const char *arg)
{
	/* in gcc the compilation modes appear to have a precedence */
	if (mode >= new_mode) {
		mode = new_mode;
	} else {
		warningf(WARN_UNUSED_OPTION, NULL,
		         "ignoring option '%s' because of gcc precedence", arg);
	}
}

static bool warn_unused_input(compilation_env_t *env, compilation_unit_t *unit)
{
	(void)env;
	warningf(WARN_UNUSED_OPTION, NULL,
	         "ignoring input '%s' in current compilation mode", unit->name);
	unit->name = NULL; /* avoid subsequence warnings */
	return true;
}

static bool warn_no_linking(compilation_env_t *env, compilation_unit_t *units)
{
	(void)env;
	for (compilation_unit_t *unit = units; unit != NULL; unit = unit->next) {
		/* if file was processed somehow don't warn */
		if (unit->original_name != unit->name)
			continue;
		warningf(WARN_UNUSED_OPTION, NULL,
		         "ignoring input '%s' because no linking is performed",
		         unit->name);
	}
	return true;
}

static void set_unused_after(compile_mode_t mode)
{
	switch (mode) {
	case MODE_PREPROCESS_ONLY:
		set_unit_handler(COMPILATION_UNIT_PREPROCESSED_C,   warn_unused_input,
		                 true);
		set_unit_handler(COMPILATION_UNIT_PREPROCESSED_CXX, warn_unused_input,
		                 true);
		/* FALLTHROUGH */
	case MODE_PARSE_ONLY:
		set_unit_handler(COMPILATION_UNIT_IR, warn_unused_input, true);
		/* FALLTHROUGH */
	case MODE_COMPILE:
		set_unit_handler(COMPILATION_UNIT_PREPROCESSED_ASSEMBLER,
		                 warn_unused_input, true);
		set_unit_handler(COMPILATION_UNIT_OBJECT, warn_unused_input, true);
		break;
	default:
		panic("invalid argument to set_unused_after");
	}
}

static bool jittest(compilation_env_t *env, compilation_unit_t *unit)
{
	(void)env;
	(void)unit;
	jit_compile_execute_main();
	return true;
}

/** modify compilation sequence based on choosen compilation mode */
static void set_handlers(compile_mode_t mode)
{
	set_default_handlers();
	switch (mode) {
	case MODE_COMPILE_ASSEMBLE_LINK:
		break;
	case MODE_PREPROCESS_ONLY:
		set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_C,
		                 print_preprocessing_tokens, true);
		set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_CXX,
		                 print_preprocessing_tokens, true);
		set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_ASSEMBLER,
		                 print_preprocessing_tokens, true);
		set_unused_after(MODE_PREPROCESS_ONLY);
		return;
	case MODE_PRINT_AST:
		set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_C,
		                 parse_ignore_errors, false);
		set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_CXX,
		                 parse_ignore_errors, false);
		set_unit_handler(COMPILATION_UNIT_AST, do_print_ast, true);
		set_unused_after(MODE_PARSE_ONLY);
		return;
	case MODE_BENCHMARK_PARSER:
		set_unit_handler(COMPILATION_UNIT_AST, do_nothing, true);
		set_unused_after(MODE_PARSE_ONLY);
		return;
	case MODE_PRINT_FLUFFY:
		set_unit_handler(COMPILATION_UNIT_AST, print_fluffy, true);
		set_unused_after(MODE_PARSE_ONLY);
		return;
	case MODE_PRINT_JNA:
		set_unit_handler(COMPILATION_UNIT_AST, print_jna, true);
		set_unused_after(MODE_PARSE_ONLY);
		return;
	case MODE_PRINT_COMPOUND_SIZE:
		set_unit_handler(COMPILATION_UNIT_AST, print_compound_size, true);
		set_unused_after(MODE_PARSE_ONLY);
		return;
	case MODE_PARSE_ONLY:
	case MODE_COMPILE_DUMP:
		set_unit_handler(COMPILATION_UNIT_AST, build_firm_ir, true);
		set_unused_after(MODE_COMPILE);
		return;
	case MODE_COMPILE_EXPORTIR:
		set_unit_handler(COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION,
		                 write_ir_file, true);
		set_unused_after(MODE_COMPILE);
		return;
	case MODE_COMPILE:
		set_unit_handler(COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION,
		                 generate_code_final, true);
		set_unused_after(MODE_COMPILE);
		return;
	case MODE_JITTEST:
		set_unit_handler(COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION,
		                 jittest, true);
		set_unused_after(MODE_COMPILE);
		return;
	case MODE_COMPILE_ASSEMBLE:
		set_unit_handler(COMPILATION_UNIT_PREPROCESSED_ASSEMBLER,
		                 assemble_final, true);
		return;
	case MODE_GENERATE_DEPENDENCIES:
		set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_C,
		                 generate_dependencies, true);
		set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_CXX,
		                 generate_dependencies, true);
		set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_ASSEMBLER,
		                 generate_dependencies, true);
		set_unused_after(MODE_PREPROCESS_ONLY);
		return;
	}
}

/** parse options that influence the compilation mode */
static bool parse_compile_mode_options(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;
	const char *option = &full_option[1];

	const char *arg;
	bool is_M = false;
	if ((is_M = simple_arg("M", s)) || simple_arg("MM", s)) {
		set_mode_gcc_prec(MODE_GENERATE_DEPENDENCIES, full_option);
		driver_add_flag(&cppflags_obst, "-%s", option);
		print_dependencies_instead_of_preprocessing = true;
		include_system_headers_in_dependencies = is_M;
	} else if (simple_arg("c", s)) {
		set_mode_gcc_prec(MODE_COMPILE_ASSEMBLE, full_option);
	} else if (simple_arg("E", s)) {
		set_mode_gcc_prec(MODE_PREPROCESS_ONLY, full_option);
	} else if (simple_arg("S", s)) {
		set_mode_gcc_prec(MODE_COMPILE, full_option);
	} else if (simple_arg("-benchmark", s)) {
		mode = MODE_BENCHMARK_PARSER;
	} else if (simple_arg("-print-ast", s)) {
		mode = MODE_PRINT_AST;
	} else if (simple_arg("-print-fluffy", s)) {
		mode = MODE_PRINT_FLUFFY;
	} else if (simple_arg("-print-compound-sizes", s)) {
		mode = MODE_PRINT_COMPOUND_SIZE;
	} else if (simple_arg("-print-jna", s)) {
		mode = MODE_PRINT_JNA;
	} else if ((arg = spaced_arg("-dump-function", s)) != NULL) {
		dumpfunction = arg;
		mode         = MODE_COMPILE_DUMP;
	} else if (simple_arg("-export-ir", s)) {
		mode = MODE_COMPILE_EXPORTIR;
	} else if (simple_arg("-jittest", s)) {
		mode = MODE_JITTEST;
	} else if (simple_arg("fsyntax-only", s)) {
		set_mode_gcc_prec(MODE_PARSE_ONLY, full_option);
	} else if (simple_arg("fno-syntax-only", s)) {
	} else {
		return false;
	}
	return true;
}

int action_compile(const char *argv0)
{
	(void)argv0;
	set_handlers(mode);

	begin_statistics();
	if (do_timing)
		timer_init();

	compilation_env_t env;
	memset(&env, 0, sizeof(env));
	env.outname = outname;

	/* process single compilation units (parsing, assembling, ...) */
	int result = EXIT_SUCCESS;
	if (!process_all_units(&env))
		result = EXIT_FAILURE;

	/* link results */
	if (result == EXIT_SUCCESS) {
		bool (*final_step)(compilation_env_t *env, compilation_unit_t *units)
			= mode == MODE_COMPILE_ASSEMBLE_LINK ? link_program
			: mode == MODE_COMPILE_DUMP          ? dump_irg
			: warn_no_linking;
		bool res = final_step(&env, units);
		if (!res) {
			result = EXIT_FAILURE;
		}
	}

	if (do_timing)
		timer_term(print_timing ? stderr : NULL);
	end_statistics();
	return result;
}

int main(int argc, char **argv)
{
	init_temp_files();
	init_driver();
	init_default_driver();
	init_preprocessor();
	init_ast();
	init_parser();

	options_state_t state;
	memset(&state, 0, sizeof(state));
	state.argc   = argc;
	state.argv   = argv;
	state.action = action_compile;

	/* do early option parsing */
	for (state.i = 1; state.i < argc; ++state.i) {
		if (options_parse_early_target(&state)
		 || options_parse_early_sysroot(&state)
		 || options_parse_early_codegen(&state))
			state.argv[state.i] = NULL;
	}

	if (!isysroot)
		isysroot = lsysroot;

	/* Initialize firm now that we know the target machine */
	init_firm_target();
	init_firm_opt();
	set_optimization_level(opt_level);

	/* parse rest of options */
	for (state.i = 1; state.i < argc; ++state.i) {
		if (state.argv[state.i] == NULL)
			continue;
		if (options_parse_assembler(&state)
		 || options_parse_c_dialect(&state)
		 || options_parse_codegen(&state)
		 || options_parse_diagnostics(&state)
		 || parse_compile_mode_options(&state)
		 || options_parse_driver(&state)
		 || options_parse_help(&state)
		 || options_parse_linker(&state)
		 || options_parse_preprocessor(&state)) {
			continue;
		}
		errorf(NULL, "unknown argument '%s'", argv[state.i]);
		state.argument_errors = true;
	}
	if (state.argument_errors) {
		help_usage(argv[0]);
		return EXIT_FAILURE;
	}

	if (!target_setup())
		return EXIT_FAILURE;

	assert(state.action != NULL);
	int ret = state.action(argv[0]);

	exit_firm_opt();
	exit_ast2firm();
	exit_parser();
	exit_ast();
	exit_preprocessor();
	exit_driver();
	exit_default_driver();
	exit_temp_files();
	return ret;
}
