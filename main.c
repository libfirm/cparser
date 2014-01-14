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
#include "options.h"
#include "parser.h"
#include "predefs.h"
#include "preprocessor.h"
#include "printer.h"
#include "symbol_table.h"
#include "target.h"
#include "tempfile.h"
#include "type_hash.h"
#include "type_t.h"
#include "types.h"
#include "version.h"
#include "warning.h"

c_dialect_t dialect = {
	.features       = _C89 | _C99 | _GNUC, /* TODO/FIXME should not be inited */
	.char_is_signed = true,
};

int main(int argc, char **argv)
{
	int opt_level = 1;

	init_temp_files();
	init_symbol_table();
	init_tokens();
	init_driver();
	preprocessor_early_init();

	/* initialize this early because it has to parse options */
	gen_firm_init();

	options_state_t state;
	memset(&state, 0, sizeof(state));
	state.argc   = argc;
	state.argv   = argv;
	state.i      = 1;
	state.action = action_compile;

	/* early options parsing (find out optimization level and OS) */
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
				state.argument_errors = true;
				continue;
			}
		}
	}

	choose_optimization_pack(opt_level);

	/* parse rest of options */
	for (state.i = 1; state.i < argc; ++state.i) {
		if (options_parse_assembler(&state)
		 || options_parse_c_dialect(&state)
		 || options_parse_codegen(&state)
		 || options_parse_diagnostics(&state)
		 || options_parse_driver(&state)
		 || options_parse_help(&state)
		 || options_parse_linker(&state)
		 || options_parse_meta(&state)
		 || options_parse_preprocessor(&state)) {
			continue;
		}

		const char *arg = argv[state.i];
		if (arg[0] == '-' && arg[1] != '\0') {
			/* an option */
			const char *option = &arg[1];
			if (option[0] == 'O') {
				continue;
			} else if (option[0] == 'd') {
				/* scan debug flags */
				for (const char *flag = &option[1]; *flag != '\0'; ++flag) {
					if (*flag == 'M')
						dump_defines = true;
				}
			} else {
				goto unknown_arg;
			}
		} else {
unknown_arg:
			errorf(NULL, "unknown argument '%s'", arg);
			state.argument_errors = true;
		}
	}

	target_setup(&state);

	if (state.argument_errors) {
		help_usage(argv[0]);
		return EXIT_FAILURE;
	}

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
	return ret;
}
