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
target_t target;

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

	if (target.machine == NULL) {
		target.machine = firm_get_host_machine();
	}
	choose_optimization_pack(opt_level);
	setup_target_machine();

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

	pass_options_to_firm_be(&state);

	if (state.argument_errors) {
		help_usage(argv[0]);
		return EXIT_FAILURE;
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
