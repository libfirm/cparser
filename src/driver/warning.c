/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "warning.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "adt/strutil.h"
#include "adt/util.h"
#include "diagnostic.h"
#include "help.h"

static warning_switch_t warning[] = {
#define ERR WARN_STATE_ON | WARN_STATE_ERROR
#define ON  WARN_STATE_ON
#define OFF WARN_STATE_NONE
#define M(warning, state, option, explanation) [warning] = { state, option, explanation },
	WARNINGS(M)
#undef M
#undef OFF
#undef ON
};

warning_switch_t const *get_warn_switch(warning_t const w)
{
	assert((size_t)w < ARRAY_SIZE(warning));
	assert(warning[w].name);
	return &warning[w];
}

void print_warning_opt_help(void)
{
	/* TODO: write explanations */
	for (warning_switch_t* i = warning; i != endof(warning); ++i) {
		char buf[256];
		snprintf(buf, sizeof(buf), "-W%s", i->name);
		help_simple(buf, i->explanation);
	}
}

#define SET(y) (void)(warning[y].state = (warning[y].state & ~off) | on)

static void warn_parentheses(warn_state_t const on, warn_state_t const off)
{
	SET(WARN_PARENTHESES_ASSIGNMENT);
	SET(WARN_PARENTHESES_COMPARISON);
	SET(WARN_PARENTHESES_ELSE);
	SET(WARN_PARENTHESES_LOGICAL);
	SET(WARN_PARENTHESES_SHIFT);
}

static void warn_unused(warn_state_t const on, warn_state_t const off)
{
	SET(WARN_UNUSED_FUNCTION);
	SET(WARN_UNUSED_LABEL);
	SET(WARN_UNUSED_PARAMETER);
	SET(WARN_UNUSED_VALUE);
	SET(WARN_UNUSED_VARIABLE);
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
	OPTX("all") {
		/* Note: this switched on a lot more warnings than gcc's -Wall */
		SET(WARN_ADDRESS);
		SET(WARN_ATTRIBUTE);
		SET(WARN_CHAR_SUBSCRIPTS);
		SET(WARN_CHAR_CTYPE);
		SET(WARN_COMMENT);
		SET(WARN_EMPTY_STATEMENT);
		SET(WARN_FORMAT);
		SET(WARN_IMPLICIT_FUNCTION_DECLARATION);
		SET(WARN_IMPLICIT_INT);
		SET(WARN_MAIN);
		SET(WARN_MISSING_PROTOTYPES);
		SET(WARN_MISSING_VARIABLE_DECLARATIONS);
		SET(WARN_NONNULL);
		SET(WARN_OTHER);
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
		warn_parentheses(on, off);
		warn_unused(on, off);
	}
	OPTX("comments") {
		SET(WARN_COMMENT);
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
	OPTX("parentheses") {
		warn_parentheses(on, off);
	}
	OPTX("unused") {
		warn_unused(on, off);
	}
#undef SET
#undef OPT_X
	else if (streq(opt /* sic */, "error-implicit-function-declaration")) {
		/* GCC legacy: This way it only can be activated. */
		warning[WARN_IMPLICIT_FUNCTION_DECLARATION].state = WARN_STATE_ON | WARN_STATE_ERROR;
	} else {
		warningf(WARN_UNKNOWN_WARNING_OPTION, NULL, "ignoring unknown option '%hs%hs'", "-W", opt);
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
