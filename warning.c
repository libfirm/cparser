/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "adt/strutil.h"
#include "adt/util.h"
#include "warning.h"
#include "help.h"

static warning_switch_t warning[] = {
	[WARN_ADDRESS]                       = { WARN_STATE_ON,   "address"                       },
	[WARN_AGGREGATE_RETURN]              = { WARN_STATE_NONE, "aggregate-return"              },
	[WARN_ATTRIBUTE]                     = { WARN_STATE_ON,   "attribute"                     },
	[WARN_CAST_QUAL]                     = { WARN_STATE_NONE, "cast-qual",                    },
	[WARN_CHAR_SUBSCRIPTS]               = { WARN_STATE_ON,   "char-subscripts",              },
	[WARN_COMMENT]                       = { WARN_STATE_NONE, "comment",                      },
	[WARN_CONVERSION]                    = { WARN_STATE_NONE, "conversion",                   },
	[WARN_DECLARATION_AFTER_STATEMENT]   = { WARN_STATE_NONE, "declaration-after-statement",  },
	[WARN_DEPRECATED_DECLARATIONS]       = { WARN_STATE_ON,   "deprecated-declarations",      },
	[WARN_DIV_BY_ZERO]                   = { WARN_STATE_ON,   "div-by-zero",                  },
	[WARN_EMPTY_BODY]                    = { WARN_STATE_NONE, "empty-body",                   },
	[WARN_EMPTY_STATEMENT]               = { WARN_STATE_NONE, "empty-statement",              },
	[WARN_ENDIF_LABELS]                  = { WARN_STATE_ON,   "endif-labels"                  },
	[WARN_ERROR]                         = { WARN_STATE_NONE, "error"                         },
	[WARN_FATAL_ERRORS]                  = { WARN_STATE_NONE, "fatal-errors"                  },
	[WARN_FLOAT_EQUAL]                   = { WARN_STATE_NONE, "float-equal",                  },
	[WARN_FORMAT]                        = { WARN_STATE_ON,   "format"                        },
	[WARN_IGNORED_QUALIFIERS]            = { WARN_STATE_ON,   "ignored-qualifiers"            },
	[WARN_IMPLICIT_FUNCTION_DECLARATION] = { WARN_STATE_ON,   "implicit-function-declaration" },
	[WARN_IMPLICIT_INT]                  = { WARN_STATE_ON,   "implicit-int"                  },
	[WARN_LONG_LONG]                     = { WARN_STATE_NONE, "long-long"                     },
	[WARN_MAIN]                          = { WARN_STATE_ON,   "main",                         },
	[WARN_MISSING_DECLARATIONS]          = { WARN_STATE_NONE, "missing-declarations",         },
	[WARN_MISSING_NORETURN]              = { WARN_STATE_NONE, "missing-noreturn",             },
	[WARN_MISSING_PROTOTYPES]            = { WARN_STATE_NONE, "missing-prototypes",           },
	[WARN_MULTICHAR]                     = { WARN_STATE_ON,   "multichar",                    },
	[WARN_NESTED_EXTERNS]                = { WARN_STATE_NONE, "nested-externs"                },
	[WARN_NONNULL]                       = { WARN_STATE_ON,   "nonnull",                      },
	[WARN_OLD_STYLE_DEFINITION]          = { WARN_STATE_NONE, "old-style-definition",         },
	[WARN_OTHER]                         = { WARN_STATE_ON,   "other"                         },
	[WARN_PACKED]                        = { WARN_STATE_NONE, "packed",                       },
	[WARN_PADDED]                        = { WARN_STATE_NONE, "padded",                       },
	[WARN_PARENTHESES]                   = { WARN_STATE_NONE, "parentheses",                  },
	[WARN_POINTER_ARITH]                 = { WARN_STATE_ON,   "pointer-arith",                },
	[WARN_REDUNDANT_DECLS]               = { WARN_STATE_ON,   "redundant-decls",              },
	[WARN_RETURN_TYPE]                   = { WARN_STATE_ON,   "return-type",                  },
	[WARN_SHADOW]                        = { WARN_STATE_NONE, "shadow",                       },
	[WARN_SHADOW_LOCAL]                  = { WARN_STATE_NONE, "shadow-local",                 },
	[WARN_SIGN_COMPARE]                  = { WARN_STATE_NONE, "sign-compare",                 },
	[WARN_STRAY_SEMICOLON]               = { WARN_STATE_ON,   "stray-semicolon",              },
	[WARN_STRICT_PROTOTYPES]             = { WARN_STATE_ON,   "strict-prototypes"             },
	[WARN_SWITCH_DEFAULT]                = { WARN_STATE_NONE, "switch-default",               },
	[WARN_SWITCH_ENUM]                   = { WARN_STATE_NONE, "switch-enum",                  },
	[WARN_SYSTEM]                        = { WARN_STATE_NONE, "system",                       },
	[WARN_TRADITIONAL]                   = { WARN_STATE_NONE, "traditional"                   },
	[WARN_UNINITIALIZED]                 = { WARN_STATE_ON,   "uninitialized",                },
	[WARN_UNKNOWN_PRAGMAS]               = { WARN_STATE_ON,   "unknown-pragmas",              },
	[WARN_UNREACHABLE_CODE]              = { WARN_STATE_NONE, "unreachable-code"              },
	[WARN_UNUSED_FUNCTION]               = { WARN_STATE_NONE, "unused-function",              },
	[WARN_UNUSED_LABEL]                  = { WARN_STATE_NONE, "unused-label",                 },
	[WARN_UNUSED_PARAMETER]              = { WARN_STATE_NONE, "unused-parameter",             },
	[WARN_UNUSED_VALUE]                  = { WARN_STATE_ON,   "unused-value",                 },
	[WARN_UNUSED_VARIABLE]               = { WARN_STATE_NONE, "unused-variable",              },
	[WARN_WRITE_STRINGS]                 = { WARN_STATE_NONE, "write-strings",                },
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
		char buf[256];
		snprintf(buf, sizeof(buf), "-W%s", i->name);
		put_help(buf, "");
	}
}

void set_warning_opt(const char *const opt)
{
	/* Process prefixes: -W[no-][error=] */
	char const *s     = opt;
	char const *rest;
	bool const  no    = (rest = strstart(s, "no-"))    ? s = rest, true : false;
	bool const  error = (rest = strstart(s, "error=")) ? s = rest, true : false;

	warn_state_t on  = WARN_STATE_NONE;
	warn_state_t off = WARN_STATE_NONE;
	if (!no || !error)
		on |= WARN_STATE_ON;
	if (error) {
		on  |= WARN_STATE_ERROR;
		off |= WARN_STATE_NO_ERROR;
	}
	if (no) {
		warn_state_t const tmp = on;
		on  = off;
		off = tmp;
	}

	for (warning_switch_t* i = warning; i != endof(warning); ++i) {
		if (streq(i->name, s)) {
			i->state = (i->state & ~off) | on;
			return;
		}
	}

	if (s[0] == '\0') { // -W is an alias for -Wextra
		goto extra;
	}
#define OPTX(x)   else if (streq(s, x))
#define SET(y)    (void)(warning[y].state = (warning[y].state & ~off) | on)
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
	else if (streq(opt /* sic */, "error-implicit-function-declaration")) {
		/* GCC legacy: This way it only can be activated. */
		warning[WARN_IMPLICIT_FUNCTION_DECLARATION].state = WARN_STATE_ON | WARN_STATE_ERROR;
		return;
	}
	else {
		fprintf(stderr, "warning: ignoring unknown option -W%s\n", opt);
	}
}

void disable_all_warnings(void)
{
	for (warning_switch_t* i = warning; i != endof(warning); ++i) {
		if (i != &warning[WARN_ERROR] &&
		    i != &warning[WARN_FATAL_ERRORS]) {
			i->state &= ~WARN_STATE_ON;
		}
	}
}
