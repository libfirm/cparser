/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "driver/enable_posix.h"

#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adt/array.h"
#include "adt/error.h"
#include "adt/strutil.h"
#include "adt/util.h"
#include "ast/ast_t.h"
#include "ast/constfold.h"
#include "ast/constfoldbits.h"
#include "ast/printer.h"
#include "ast/symbol_table.h"
#include "ast/type_hash.h"
#include "ast/type_t.h"
#include "ast/types.h"
#include "driver/c_driver.h"
#include "driver/diagnostic.h"
#include "driver/driver.h"
#include "driver/driver_t.h"
#include "driver/help.h"
#include "driver/lang_features.h"
#include "driver/machine_triple.h"
#include "driver/options.h"
#include "driver/predefs.h"
#include "driver/target.h"
#include "driver/tempfile.h"
#include "driver/version.h"
#include "driver/warning.h"
#include "firm/ast2firm.h"
#include "firm/firm_opt.h"
#include "firm/firm_timing.h"
#include "firm/mangle.h"
#include "parser/parser.h"
#include "parser/preprocessor.h"
#include "wrappergen/write_compoundsizes.h"
#include "wrappergen/write_fluffy.h"
#include "wrappergen/write_jna.h"

typedef enum compile_mode_t {
	MODE_BENCHMARK_PARSER,
	MODE_PREPROCESS_ONLY,
	MODE_GENERATE_DEPENDENCIES,
	MODE_PARSE_ONLY,
	MODE_COMPILE,
	MODE_COMPILE_DUMP,
	MODE_COMPILE_EXPORTIR,
	MODE_COMPILE_ASSEMBLE,
	MODE_COMPILE_ASSEMBLE_LINK,
	MODE_PRINT_AST,
	MODE_PRINT_FLUFFY,
	MODE_PRINT_JNA,
	MODE_PRINT_COMPOUND_SIZE,
} compile_mode_t;
static compile_mode_t mode = MODE_COMPILE_ASSEMBLE_LINK;

static bool print_fluffy(compilation_env_t *env, compilation_unit_t *unit)
{
	write_fluffy_decls(env->out, unit->ast);
	return true;
}

static bool print_jna(compilation_env_t *env, compilation_unit_t *unit)
{
	write_jna_decls(env->out, unit->ast);
	return true;
}

static bool print_compound_size(compilation_env_t *env,
                                compilation_unit_t *unit)
{
	write_compoundsizes(env->out, unit->ast);
	return true;
}

static bool parse_ignore_errors(compilation_env_t *env,
                                compilation_unit_t *unit)
{
	do_parsing(env, unit); /* ignore return value */
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
		set_unit_handler(COMPILATION_UNIT_PREPROCESSED_C,   do_copy_file, true);
		set_unit_handler(COMPILATION_UNIT_PREPROCESSED_CXX, do_copy_file, true);
		set_unit_handler(COMPILATION_UNIT_PREPROCESSED_ASSEMBLER, do_copy_file,
		                 true);
		return;
	case MODE_PRINT_AST:
		set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_C,
		                 parse_ignore_errors, false);
		set_unit_handler(COMPILATION_UNIT_LEXER_TOKENS_CXX,
		                 parse_ignore_errors, false);
		set_unit_handler(COMPILATION_UNIT_AST, do_print_ast, true);
		return;
	case MODE_BENCHMARK_PARSER:
		set_unit_handler(COMPILATION_UNIT_AST, do_nothing, true);
		return;
	case MODE_PRINT_FLUFFY:
		set_unit_handler(COMPILATION_UNIT_AST, print_fluffy, true);
		return;
	case MODE_PRINT_JNA:
		set_unit_handler(COMPILATION_UNIT_AST, print_jna, true);
		return;
	case MODE_PRINT_COMPOUND_SIZE:
		set_unit_handler(COMPILATION_UNIT_AST, print_compound_size, true);
		return;
	case MODE_PARSE_ONLY:
	case MODE_COMPILE_DUMP:
	case MODE_COMPILE_EXPORTIR:
		set_unit_handler(COMPILATION_UNIT_AST, build_firm_ir, true);
	    return;
	case MODE_COMPILE:
		set_unit_handler(COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION,
		                 generate_code_final, true);
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
		set_unit_handler(COMPILATION_UNIT_PREPROCESSED_C,   do_copy_file, true);
		set_unit_handler(COMPILATION_UNIT_PREPROCESSED_CXX, do_copy_file, true);
		set_unit_handler(COMPILATION_UNIT_PREPROCESSED_ASSEMBLER, do_copy_file,
		                 true);
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
	if (streq(option, "M") || streq(option, "MM")) {
		mode = MODE_GENERATE_DEPENDENCIES;
		driver_add_flag(&cppflags_obst, "-%s", option);
	} else if (streq(option, "c")) {
		mode = MODE_COMPILE_ASSEMBLE;
	} else if (streq(option, "E")) {
		mode = MODE_PREPROCESS_ONLY;
	} else if (streq(option, "S")) {
		mode = MODE_COMPILE;
	} else if (streq(option, "-benchmark")) {
		mode = MODE_BENCHMARK_PARSER;
	} else if (streq(option, "-print-ast")) {
		mode = MODE_PRINT_AST;
	} else if (streq(option, "-print-fluffy")) {
		mode = MODE_PRINT_FLUFFY;
	} else if (streq(option, "-print-compound-sizes")) {
		mode = MODE_PRINT_COMPOUND_SIZE;
	} else if (streq(option, "-print-jna")) {
		mode = MODE_PRINT_JNA;
	} else if ((arg = spaced_arg("-dump-function", s, false)) != NULL) {
		dumpfunction = arg;
		mode         = MODE_COMPILE_DUMP;
	} else if (streq(option, "-export-ir")) {
		mode = MODE_COMPILE_EXPORTIR;
	} else if (streq(option, "fsyntax-only")) {
		mode = MODE_PARSE_ONLY;
	} else if (streq(option, "fno-syntax-only")) {
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
			: mode == MODE_COMPILE_EXPORTIR      ? write_ir_file
			: NULL;
		if (final_step != NULL) {
			bool res = final_step(&env, units);
			if (!res) {
				result = EXIT_FAILURE;
			}
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
	init_symbol_table();
	init_tokens();
	init_driver();
	init_default_driver();
	preprocessor_early_init();

	/* initialize this early because it has to parse options */
	gen_firm_init();

	options_state_t state;
	memset(&state, 0, sizeof(state));
	state.argc   = argc;
	state.argv   = argv;
	state.i      = 1;
	state.action = action_compile;

	options_parse_early(&state);

	/* parse rest of options */
	for (state.i = 1; state.i < argc; ++state.i) {
		if (options_parse_assembler(&state)
		 || options_parse_c_dialect(&state)
		 || options_parse_codegen(&state)
		 || options_parse_diagnostics(&state)
		 || parse_compile_mode_options(&state)
		 || options_parse_driver(&state)
		 || options_parse_help(&state)
		 || options_parse_linker(&state)
		 || options_parse_meta(&state)
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
	init_typehash();
	init_basic_types();
	init_ast();
	init_constfold();
	init_parser();
	init_ast2firm();
	init_mangle();

	assert(state.action != NULL);
	int ret = state.action(argv[0]);

	free_temp_files();

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
	exit_default_driver();
	return ret;
}
