/*
 * This file is part of cparser.
 * Copyright (C) 2007-2008 Matthias Braun <matze@braunis.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
#include <stdio.h>
#include <string.h>
#include "warning.h"

warning_t warning = {
	.other                               = true,

	.address                             = true,
	.aggregate_return                    = false,
	.attribute                           = true,
	.cast_qual                           = false,
	.char_subscripts                     = true,
	.comment                             = false,
	.conversion                          = false,
	.declaration_after_statement         = false,
	.deprecated_declarations             = true,
	.div_by_zero                         = true,
	.empty_statement                     = false,
	.error_implicit_function_declaration = false,
	.fatal_errors                        = false,
	.float_equal                         = false,
	.format                              = true,
	.implicit_function_declaration       = true,
	.implicit_int                        = true,
	.init_self                           = true,
	.long_long                           = false,
	.main                                = true,
	.missing_declarations                = false,
	.missing_noreturn                    = false,
	.missing_prototypes                  = false,
	.multichar                           = true,
	.nested_externs                      = false,
	.nonnull                             = true,
	.old_style_definition                = false,
	.packed                              = false,
	.padded                              = false,
	.pointer_arith                       = true,
	.redundant_decls                     = true,
	.return_type                         = true,
	.s_are_errors                        = false,
	.shadow                              = false,
	.sign_compare                        = false,
	.strict_prototypes                   = true,
	.switch_default                      = false,
	.switch_enum                         = false,
	.traditional                         = false,
	.unknown_pragmas                     = true,
	.unreachable_code                    = false,
	.unused_function                     = false,
	.unused_label                        = false,
	.unused_parameter                    = false,
	.unused_value                        = true,
	.unused_variable                     = false,
	.write_strings                       = false
};

void set_warning_opt(const char *const opt)
{
	const char* s = opt;

	bool state = true;

	/* "no-" prefix */
	if (s[0] == 'n' && s[1] == 'o' && s[2] == '-') {
		s += 3;
		state = false;
	}

	if (0) {}
#define OPTX(x)   else if (strcmp(s, x) == 0)
#define SET(y)    (void)(warning.y = state)
#define OPT(x, y) OPTX(x) SET(y)
	OPTX("all") {
		/* Note: this switched on a lot more warnings than gcc's -Wall */
		SET(other);

		SET(address);
		SET(attribute);
		SET(char_subscripts);
		SET(comment);
		SET(empty_statement);
		SET(format);
		SET(implicit_function_declaration);
		SET(implicit_int);
		SET(init_self);
		SET(main);
		SET(nonnull);
		SET(pointer_arith);
		SET(redundant_decls);
		SET(return_type);
		SET(shadow);
		SET(sign_compare);
		SET(strict_prototypes);
		SET(switch_enum);
		SET(unknown_pragmas);
		SET(unreachable_code);
		SET(unused_function);
		SET(unused_label);
		SET(unused_parameter);
		SET(unused_value);
		SET(unused_variable);
	}
	OPT("address",                             address);
	OPT("aggregate-return",                    aggregate_return);
	OPT("attribute",                           attribute);
	OPT("cast-qual",                           cast_qual);
	OPT("char-subscripts",                     char_subscripts);
	OPT("comment",                             comment);
	OPT("conversion",                          conversion);
	OPT("declaration-after-statement",         declaration_after_statement);
	OPT("deprecated-declarations",             deprecated_declarations);
	OPT("div-by-zero",                         div_by_zero);
	OPT("empty-statement",                     empty_statement);
	OPT("error",                               s_are_errors);
	OPT("error-implicit-function-declaration", error_implicit_function_declaration);
	OPTX("extra") {
		/* TODO */
		// TODO SET(function_end_without_return);
		SET(empty_statement);
		// TODO SET(incomplete_aggregate_init);
		// TODO SET(missing_field_initializers);
		// TODO SET(pointless_comparison);
		SET(unused_parameter);
		SET(unused_value);
	}
	OPT("fatal-errors",                        fatal_errors);
	OPT("float-equal",                         float_equal);
	OPTX("format") {
		SET(format);
		SET(nonnull);
	}
	OPTX("implicit") {
		SET(implicit_function_declaration);
		SET(implicit_int);
	}
	OPT("implicit-function-declaration",       implicit_function_declaration);
	OPT("implicit-int",                        implicit_int);
	OPT("init-self",                           init_self);
	OPT("long-long",                           long_long);
	OPT("main",                                main);
	OPT("missing-declarations",                missing_declarations);
	OPT("missing-noreturn",                    missing_noreturn);
	OPT("missing-prototypes",                  missing_prototypes);
	OPT("multichar",                           multichar);
	OPT("nested-externs",                      nested_externs);
	OPT("nonnull",                             nonnull);
	OPT("old-style-definition",                old_style_definition);
	OPT("packed",                              packed);
	OPT("padded",                              padded);
	OPT("pointer-arith",                       pointer_arith);
	OPT("redundant-decls",                     redundant_decls);
	OPT("return-type",                         return_type);
	OPT("shadow",                              shadow);
	OPT("sign-compare",                        sign_compare);
	OPT("strict-prototypes",                   strict_prototypes);
	OPT("switch-default",                      switch_default);
	OPT("switch-enum",                         switch_enum);
	OPT("traditional",                         traditional);
	OPT("unknown-pragmas",                     unknown_pragmas);
	OPT("unreachable-code",                    unreachable_code);
	OPTX("unused") {
		SET(unused_function);
		SET(unused_label);
		SET(unused_parameter);
		SET(unused_value);
		SET(unused_variable);
	}
	OPT("unused-function",                     unused_function);
	OPT("unused-label",                        unused_label);
	OPT("unused-parameter",                    unused_parameter);
	OPT("unused-value",                        unused_value);
	OPT("unused-variable",                     unused_variable);
	OPT("write-strings",                       write_strings);
#undef OPT
#undef SET
#undef OPT_X
	else {
		fprintf(stderr, "warning: ignoring unknown option -W%s\n", opt);
	}
}
