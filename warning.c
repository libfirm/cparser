/*
 * This file is part of cparser.
 * Copyright (C) 2007-2009 Matthias Braun <matze@braunis.de>
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
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "adt/util.h"
#include "warning.h"
#include "help.h"

static warning_switch_t warning[] = {
	[WARN_ADDRESS]                       = { WARN_LEVEL_ON,  "address"                       },
	[WARN_AGGREGATE_RETURN]              = { WARN_LEVEL_OFF, "aggregate-return"              },
	[WARN_ATTRIBUTE]                     = { WARN_LEVEL_ON,  "attribute"                     },
	[WARN_CAST_QUAL]                     = { WARN_LEVEL_OFF, "cast-qual",                    },
	[WARN_CHAR_SUBSCRIPTS]               = { WARN_LEVEL_ON,  "char-subscripts",              },
	[WARN_COMMENT]                       = { WARN_LEVEL_OFF, "comment",                      },
	[WARN_CONVERSION]                    = { WARN_LEVEL_OFF, "conversion",                   },
	[WARN_DECLARATION_AFTER_STATEMENT]   = { WARN_LEVEL_OFF, "declaration-after-statement",  },
	[WARN_DEPRECATED_DECLARATIONS]       = { WARN_LEVEL_ON,  "deprecated-declarations",      },
	[WARN_DIV_BY_ZERO]                   = { WARN_LEVEL_ON,  "div-by-zero",                  },
	[WARN_EMPTY_STATEMENT]               = { WARN_LEVEL_OFF, "empty-statement",              },
	[WARN_ERROR]                         = { WARN_LEVEL_OFF, "error"                         },
	[WARN_FATAL_ERRORS]                  = { WARN_LEVEL_OFF, "fatal-errors"                  },
	[WARN_FLOAT_EQUAL]                   = { WARN_LEVEL_OFF, "float-equal",                  },
	[WARN_FORMAT]                        = { WARN_LEVEL_ON,  "format"                        },
	[WARN_IMPLICIT_FUNCTION_DECLARATION] = { WARN_LEVEL_ON,  "implicit-function-declaration" },
	[WARN_IMPLICIT_INT]                  = { WARN_LEVEL_ON,  "implicit-int"                  },
	[WARN_INIT_SELF]                     = { WARN_LEVEL_ON,  "init-self",                    },
	[WARN_LONG_LONG]                     = { WARN_LEVEL_OFF, "long-long"                     },
	[WARN_MAIN]                          = { WARN_LEVEL_ON,  "main",                         },
	[WARN_MISSING_DECLARATIONS]          = { WARN_LEVEL_OFF, "missing-declarations",         },
	[WARN_MISSING_NORETURN]              = { WARN_LEVEL_OFF, "missing-noreturn",             },
	[WARN_MISSING_PROTOTYPES]            = { WARN_LEVEL_OFF, "missing-prototypes",           },
	[WARN_MULTICHAR]                     = { WARN_LEVEL_ON,  "multichar",                    },
	[WARN_NESTED_EXTERNS]                = { WARN_LEVEL_OFF, "nested-externs"                },
	[WARN_NONNULL]                       = { WARN_LEVEL_ON,  "nonnull",                      },
	[WARN_OLD_STYLE_DEFINITION]          = { WARN_LEVEL_OFF, "old-style-definition",         },
	[WARN_OTHER]                         = { WARN_LEVEL_ON,  "other"                         },
	[WARN_PACKED]                        = { WARN_LEVEL_OFF, "packed",                       },
	[WARN_PADDED]                        = { WARN_LEVEL_OFF, "padded",                       },
	[WARN_PARENTHESES]                   = { WARN_LEVEL_OFF, "parentheses",                  },
	[WARN_POINTER_ARITH]                 = { WARN_LEVEL_ON,  "pointer-arith",                },
	[WARN_REDUNDANT_DECLS]               = { WARN_LEVEL_ON,  "redundant-decls",              },
	[WARN_RETURN_TYPE]                   = { WARN_LEVEL_ON,  "return-type",                  },
	[WARN_SHADOW]                        = { WARN_LEVEL_OFF, "shadow",                       },
	[WARN_SHADOW_LOCAL]                  = { WARN_LEVEL_OFF, "shadow-local",                 },
	[WARN_SIGN_COMPARE]                  = { WARN_LEVEL_OFF, "sign-compare",                 },
	[WARN_STRICT_PROTOTYPES]             = { WARN_LEVEL_ON,  "strict-prototypes"             },
	[WARN_SWITCH_DEFAULT]                = { WARN_LEVEL_OFF, "switch-default",               },
	[WARN_SWITCH_ENUM]                   = { WARN_LEVEL_OFF, "switch-enum",                  },
	[WARN_TRADITIONAL]                   = { WARN_LEVEL_OFF, "traditional"                   },
	[WARN_UNINITIALIZED]                 = { WARN_LEVEL_ON,  "uninitialized",                },
	[WARN_UNKNOWN_PRAGMAS]               = { WARN_LEVEL_ON,  "unknown-pragmas",              },
	[WARN_UNREACHABLE_CODE]              = { WARN_LEVEL_OFF, "unreachable-code"              },
	[WARN_UNUSED_FUNCTION]               = { WARN_LEVEL_OFF, "unused-function",              },
	[WARN_UNUSED_LABEL]                  = { WARN_LEVEL_OFF, "unused-label",                 },
	[WARN_UNUSED_PARAMETER]              = { WARN_LEVEL_OFF, "unused-parameter",             },
	[WARN_UNUSED_VALUE]                  = { WARN_LEVEL_ON,  "unused-value",                 },
	[WARN_UNUSED_VARIABLE]               = { WARN_LEVEL_OFF, "unused-variable",              },
	[WARN_WRITE_STRINGS]                 = { WARN_LEVEL_OFF, "write-strings",                },
};

warning_switch_t const *get_warn_switch(warning_t const w)
{
	assert((size_t)w < lengthof(warning));
	assert(warning[w].name);
	return &warning[w];
}

void print_warning_opt_help(void)
{
	/* TODO: write explanations */
	for (warning_switch_t* i = warning; i != endof(warning); ++i) {
		put_help(i->name, "");
	}
}

void set_warning_opt(const char *const opt)
{
	const char* s = opt;

	warn_level_t const level =
		strncmp(s, "no-",    3) == 0 ? s += 3, WARN_LEVEL_OFF   : /* "no-" prefix */
		strncmp(s, "error-", 6) == 0 ? s += 6, WARN_LEVEL_ERROR : /* "error-" prefix */
		WARN_LEVEL_ON;

	for (warning_switch_t* i = warning; i != endof(warning); ++i) {
		if (strcmp(i->name, s) == 0) {
			i->level = level;
			return;
		}
	}

	if (s[0] == '\0') { // -W is an alias for -Wextra
		goto extra;
	}
#define OPTX(x)   else if (strcmp(s, x) == 0)
#define SET(y)    (void)(warning[y].level = level)
	OPTX("all") {
		/* Note: this switched on a lot more warnings than gcc's -Wall */
		SET(WARN_ADDRESS);
		SET(WARN_ATTRIBUTE);
		SET(WARN_CHAR_SUBSCRIPTS);
		SET(WARN_COMMENT);
		SET(WARN_EMPTY_STATEMENT);
		SET(WARN_FORMAT);
		SET(WARN_IMPLICIT_FUNCTION_DECLARATION);
		SET(WARN_IMPLICIT_INT);
		SET(WARN_INIT_SELF);
		SET(WARN_MAIN);
		SET(WARN_MISSING_DECLARATIONS);
		SET(WARN_NONNULL);
		SET(WARN_OTHER);
		SET(WARN_PARENTHESES);
		SET(WARN_POINTER_ARITH);
		SET(WARN_REDUNDANT_DECLS);
		SET(WARN_RETURN_TYPE);
		SET(WARN_SHADOW_LOCAL);
		SET(WARN_SIGN_COMPARE);
		SET(WARN_STRICT_PROTOTYPES);
		SET(WARN_SWITCH_ENUM);
		SET(WARN_UNINITIALIZED);
		SET(WARN_UNKNOWN_PRAGMAS);
		SET(WARN_UNREACHABLE_CODE);
		SET(WARN_UNUSED_FUNCTION);
		SET(WARN_UNUSED_LABEL);
		SET(WARN_UNUSED_PARAMETER);
		SET(WARN_UNUSED_VALUE);
		SET(WARN_UNUSED_VARIABLE);
	}
	OPTX("extra") {
extra:
		/* TODO */
		// TODO SET(function_end_without_return);
		SET(WARN_EMPTY_STATEMENT);
		// TODO SET(incomplete_aggregate_init);
		// TODO SET(missing_field_initializers);
		// TODO SET(pointless_comparison);
		SET(WARN_SHADOW);
		SET(WARN_UNUSED_PARAMETER);
		SET(WARN_UNUSED_VALUE);
	}
	OPTX("implicit") {
		SET(WARN_IMPLICIT_FUNCTION_DECLARATION);
		SET(WARN_IMPLICIT_INT);
	}
	OPTX("unused") {
		SET(WARN_UNUSED_FUNCTION);
		SET(WARN_UNUSED_LABEL);
		SET(WARN_UNUSED_PARAMETER);
		SET(WARN_UNUSED_VALUE);
		SET(WARN_UNUSED_VARIABLE);
	}
#undef SET
#undef OPT_X
	else {
		fprintf(stderr, "warning: ignoring unknown option -W%s\n", opt);
	}
}

void disable_all_warnings(void)
{
	for (warning_switch_t* i = warning; i != endof(warning); ++i) {
		if (i != &warning[WARN_ERROR] &&
		    i != &warning[WARN_FATAL_ERRORS]) {
			i->level = WARN_LEVEL_OFF;
		}
	}
}
