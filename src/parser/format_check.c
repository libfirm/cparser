/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "format_check.h"

#include <ctype.h>

#include "adt/strutil.h"
#include "adt/util.h"
#include "ast/ast_t.h"
#include "ast/attribute_t.h"
#include "ast/constfold.h"
#include "ast/dialect.h"
#include "ast/entity_t.h"
#include "ast/symbol_t.h"
#include "ast/type_t.h"
#include "ast/types.h"
#include "driver/diagnostic.h"
#include "driver/warning.h"
#include "parser.h"

typedef struct format_env_t {
	unsigned               num_fmt;
	unsigned               num_arg;
	call_argument_t const *arg;
	position_t      const *pos;
	void            const *ctx;
} format_env_t;

static expression_t const *get_next_arg(format_env_t *const env)
{
	call_argument_t const *const arg = env->arg;
	if (!arg)
		return NULL;
	env->arg      = arg->next;
	env->num_arg += 1;
	return arg->expression;
}

static inline bool accept(char const **const c_inout, char const expected)
{
	if (**c_inout == expected) {
		++*c_inout;
		return true;
	} else {
		return false;
	}
}

typedef enum format_flag_t {
	FMT_FLAG_NONE  = 0,
	FMT_FLAG_HASH  = 1U << 0,
	FMT_FLAG_ZERO  = 1U << 1,
	FMT_FLAG_MINUS = 1U << 2,
	FMT_FLAG_SPACE = 1U << 3,
	FMT_FLAG_PLUS  = 1U << 4,
	FMT_FLAG_TICK  = 1U << 5
} format_flag_t;

typedef unsigned format_flags_t;

typedef enum format_length_modifier_t {
	FMT_MOD_NONE,
	FMT_MOD_L,
	FMT_MOD_hh,
	FMT_MOD_h,
	FMT_MOD_l,
	FMT_MOD_ll,
	FMT_MOD_j,
	FMT_MOD_t,
	FMT_MOD_z,
	FMT_MOD_q,
	/* only in microsoft mode */
	FMT_MOD_w,
	FMT_MOD_I,
	FMT_MOD_I32,
	FMT_MOD_I64
} format_length_modifier_t;

typedef struct format_spec_t {
	const char    *name;     /**< name of the function */
	format_kind_t  fmt_kind; /**< kind */
	unsigned       fmt_idx;  /**< index of the format string */
	unsigned       arg_idx;  /**< index of the first argument */
} format_spec_t;

static char const *const format_length_modifier_names[] = {
	[FMT_MOD_NONE] = "",
	[FMT_MOD_L]    = "L",
	[FMT_MOD_hh]   = "hh",
	[FMT_MOD_h]    = "h",
	[FMT_MOD_l]    = "l",
	[FMT_MOD_ll]   = "ll",
	[FMT_MOD_j]    = "j",
	[FMT_MOD_t]    = "t",
	[FMT_MOD_z]    = "z",
	[FMT_MOD_q]    = "q",
	/* only in microsoft mode */
	[FMT_MOD_w]    = "w",
	[FMT_MOD_I]    = "I",
	[FMT_MOD_I32]  = "I32",
	[FMT_MOD_I64]  = "I64"
};

static const char* get_length_modifier_name(const format_length_modifier_t mod)
{
	assert((size_t)mod < ARRAY_SIZE(format_length_modifier_names));
	return format_length_modifier_names[mod];
}

static format_length_modifier_t parse_length_modifier(char const **const c_inout)
{
	format_length_modifier_t mod      = FMT_MOD_NONE;
	char        const *const c        = *c_inout;
	char        const *      last_end = c;
	char const *const *      i        = format_length_modifier_names + 1;
	char const *const *const e        = dialect.ms ? endof(format_length_modifier_names) : format_length_modifier_names + FMT_MOD_w;
	for (; i != e; ++i) {
		char const *const end = strstart(c, *i);
		if (end && last_end < end) {
			last_end = end;
			mod      = (format_length_modifier_t)(i - format_length_modifier_names);
		}
	}
	*c_inout = last_end;
	return mod;
}

static void warn_invalid_length_modifier(const position_t *pos,
                                         const format_length_modifier_t mod,
                                         const char conversion)
{
	char const *const lmod = get_length_modifier_name(mod);
	warningf(WARN_FORMAT, pos, "invalid length modifier '%s' for conversion specifier '%%%c'", lmod, conversion);
}

static void check_argument_type(format_env_t *const env, type_t *const spec_type, char const *const spec_begin, char const *const spec_end)
{
	if (!spec_type) {
		warningf(WARN_FORMAT, env->pos, "dangling '%%' in format string");
		return;
	}

	type_t *const spec_skip = skip_typeref(spec_type);
	if (is_type_void(spec_skip))
		return;

	expression_t const *const arg = get_next_arg(env);
	if (!arg) {
		warningf(WARN_FORMAT, env->pos, "too few arguments for format string");
		return;
	}

	type_t *const arg_type = arg->base.type;
	type_t *const arg_skip = skip_typeref(arg_type);
	if (is_type_pointer(spec_skip)) {
		if (is_type_pointer(arg_skip)) {
			type_t *const spec_to = skip_typeref(spec_skip->pointer.points_to);
			type_t *const arg_to  = skip_typeref(arg_skip->pointer.points_to);
			/* Allow any pointer type, if void* is expected. */
			if ((arg_to->base.qualifiers & ~spec_to->base.qualifiers) == 0 &&
					(types_compatible_ignore_qualifiers(arg_to, spec_to) || is_type_void(spec_to)))
				return;
		}
	} else if (types_compatible_ignore_qualifiers(arg_skip, spec_skip)) {
		return;
	} else if (arg->kind == EXPR_UNARY_CAST && arg->base.implicit) {
		expression_t const *const expr        = arg->unary.value;
		type_t             *const unprom_type = skip_typeref(expr->base.type);
		if (types_compatible_ignore_qualifiers(unprom_type, spec_skip))
			return;
		if (spec_skip == type_unsigned_int && !is_type_signed(unprom_type))
			return;
	} else if (arg->kind == EXPR_LITERAL_CHARACTER) {
		if (spec_skip == type_char && arg->string_literal.value->size == 1)
			return;
	} else if (arg->kind == EXPR_ENUM_CONSTANT) {
		if (spec_skip->kind == TYPE_ENUM && spec_skip->enumt.enume == arg->reference.entity->enum_value.enume)
			return;
	}
	if (!is_type_valid(arg_skip) || !is_type_valid(spec_skip))
		return;
	position_t const *const apos = &arg->base.pos;
	int               const slen = spec_end - spec_begin;
	warningf(WARN_FORMAT, apos, "conversion '%%%.*s' at position %u specifies type '%T' but the argument has type '%T'", slen, spec_begin, env->num_fmt, spec_type, arg_type);
}

static bool check_digits_or_star(format_env_t *const env, char const **const pc, char const *const ctx)
{
	if (accept(pc, '*')) {
		expression_t const *const arg = get_next_arg(env);
		if (!arg) {
			warningf(WARN_FORMAT, env->pos, "missing argument for '*' %s in conversion specification %u", ctx, env->num_fmt);
			return false;
		}
		type_t *const arg_type = arg->base.type;
		if (!types_compatible_ignore_qualifiers(skip_typeref(arg_type), type_int))
			warningf(WARN_FORMAT, env->pos, "argument for '*' %s in conversion specification %u is not an 'int', but an '%T'", ctx, env->num_fmt, arg_type);
	} else {
		while (is_digit(**pc)) {
			++*pc;
		}
	}
	return true;
}

/**
 * Check printf-style format. Returns number of expected arguments.
 */
static char const *check_printf_format(format_env_t *const env, char const *c)
{
	if (accept(&c, '%'))
		return c;

	format_flags_t fmt_flags = FMT_FLAG_NONE;
	if (accept(&c, '0'))
		fmt_flags |= FMT_FLAG_ZERO;

	/* argument selector or minimum field width */
	if (is_digit(*c)) {
		do {
			++c;
		} while (is_digit(*c));

		/* digit string was ... */
		if (accept(&c, '$')) {
			/* ... argument selector */
			fmt_flags = FMT_FLAG_NONE; /* reset possibly set 0-flag */
			/* TODO implement */
			return NULL;
		}
		/* ... minimum field width */
	} else {
		/* flags */
		for (;;) {
			format_flags_t flag;
			char const fmt = *c;
			switch (fmt) {
			case '#':  flag = FMT_FLAG_HASH;  break;
			case '0':  flag = FMT_FLAG_ZERO;  break;
			case '-':  flag = FMT_FLAG_MINUS; break;
			case '\'': flag = FMT_FLAG_TICK;  break;

			case ' ':
				if (fmt_flags & FMT_FLAG_PLUS)
					warningf(WARN_FORMAT, env->pos, "' ' is overridden by prior '+' in conversion specification %u", env->num_fmt);
				flag = FMT_FLAG_SPACE;
				break;

			case '+':
				if (fmt_flags & FMT_FLAG_SPACE)
					warningf(WARN_FORMAT, env->pos, "'+' overrides prior ' ' in conversion specification %u", env->num_fmt);
				flag = FMT_FLAG_PLUS;
				break;

			default: goto break_fmt_flags;
			}
			if (fmt_flags & flag)
				warningf(WARN_FORMAT, env->pos, "repeated flag '%c' in conversion specification %u", fmt, env->num_fmt);
			fmt_flags |= flag;
			++c;
		}
break_fmt_flags:

		/* minimum field width */
		if (!check_digits_or_star(env, &c, "field width"))
			return NULL;
	}

	/* precision */
	if (accept(&c, '.')) {
		if (fmt_flags & FMT_FLAG_ZERO)
			warningf(WARN_FORMAT, env->pos, "'0' flag ignored with precision in conversion specification %u", env->num_fmt);
		if (!check_digits_or_star(env, &c, "precision"))
			return NULL;
	}

	char              const *const spec_begin = c;
	format_length_modifier_t const fmt_mod    = parse_length_modifier(&c);

	type_t        *expected_type;
	format_flags_t allowed_flags;
	char     const fmt = *c++;
	switch (fmt) {
	case '\0':
		--c;
		expected_type = NULL;
		allowed_flags = ~FMT_FLAG_NONE;
		break;

	case 'd':
	case 'i':
		switch (fmt_mod) {
		case FMT_MOD_NONE: expected_type = type_int;         break;
		case FMT_MOD_hh:   expected_type = type_signed_char; break;
		case FMT_MOD_h:    expected_type = type_short;       break;
		case FMT_MOD_l:    expected_type = type_long;        break;
		case FMT_MOD_ll:   expected_type = type_long_long;   break;
		case FMT_MOD_j:    expected_type = type_intmax_t;    break;
		case FMT_MOD_z:    expected_type = type_ssize_t;     break;
		case FMT_MOD_t:    expected_type = type_ptrdiff_t;   break;
		case FMT_MOD_I:    expected_type = type_ptrdiff_t;   break;
		case FMT_MOD_I32:  expected_type = type_int32;       break;
		case FMT_MOD_I64:  expected_type = type_int64;       break;
		default:           goto bad_len_mod;
		}
		allowed_flags = FMT_FLAG_MINUS | FMT_FLAG_SPACE | FMT_FLAG_PLUS | FMT_FLAG_ZERO;
		break;

	case 'o':
	case 'X':
	case 'x':
		allowed_flags = FMT_FLAG_MINUS | FMT_FLAG_HASH | FMT_FLAG_ZERO;
		goto eval_fmt_mod_unsigned;

	case 'u':
		allowed_flags = FMT_FLAG_MINUS | FMT_FLAG_ZERO;
eval_fmt_mod_unsigned:
		switch (fmt_mod) {
		case FMT_MOD_NONE: expected_type = type_unsigned_int;       break;
		case FMT_MOD_hh:   expected_type = type_unsigned_char;      break;
		case FMT_MOD_h:    expected_type = type_unsigned_short;     break;
		case FMT_MOD_l:    expected_type = type_unsigned_long;      break;
		case FMT_MOD_ll:   expected_type = type_unsigned_long_long; break;
		case FMT_MOD_j:    expected_type = type_uintmax_t;          break;
		case FMT_MOD_z:    expected_type = type_size_t;             break;
		case FMT_MOD_t:    expected_type = type_uptrdiff_t;         break;
		case FMT_MOD_I:    expected_type = type_size_t;             break;
		case FMT_MOD_I32:  expected_type = type_unsigned_int32;     break;
		case FMT_MOD_I64:  expected_type = type_unsigned_int64;     break;
		default:           goto bad_len_mod;
		}
		break;

	case 'A':
	case 'a':
	case 'E':
	case 'e':
	case 'F':
	case 'f':
	case 'G':
	case 'g':
		switch (fmt_mod) {
		case FMT_MOD_l:    /* l modifier is ignored */
		case FMT_MOD_NONE: expected_type = type_double;      break;
		case FMT_MOD_L:    expected_type = type_long_double; break;
		default:           goto bad_len_mod;
		}
		allowed_flags = FMT_FLAG_MINUS | FMT_FLAG_SPACE | FMT_FLAG_PLUS | FMT_FLAG_HASH | FMT_FLAG_ZERO;
		break;

	case 'C':
		if (fmt_mod != FMT_MOD_NONE)
			goto bad_len_mod;
		expected_type = type_wchar_t;
		allowed_flags = FMT_FLAG_MINUS;
		break;

	case 'c':
		expected_type = type_int;
		switch (fmt_mod) {
		case FMT_MOD_NONE: expected_type = type_int;     break; /* TODO promoted char */
		case FMT_MOD_l:    expected_type = type_wint_t;  break;
		case FMT_MOD_w:    expected_type = type_wchar_t; break;
		default:           goto bad_len_mod;
		}
		allowed_flags = FMT_FLAG_MINUS;
		break;

	case 'S':
		if (fmt_mod != FMT_MOD_NONE)
			goto bad_len_mod;
		expected_type = type_const_wchar_t_ptr;
		allowed_flags = FMT_FLAG_MINUS;
		break;

	case 's':
		switch (fmt_mod) {
		case FMT_MOD_NONE: expected_type = type_const_char_ptr;    break;
		case FMT_MOD_l:    expected_type = type_const_wchar_t_ptr; break;
		case FMT_MOD_w:    expected_type = type_const_wchar_t_ptr; break;
		default:           goto bad_len_mod;
		}
		allowed_flags = FMT_FLAG_MINUS;
		break;

	case 'p':
		if (fmt_mod != FMT_MOD_NONE)
			goto bad_len_mod;
		expected_type = type_void_ptr;
		allowed_flags = FMT_FLAG_MINUS;
		break;

	case 'n':
		switch (fmt_mod) {
		case FMT_MOD_NONE: expected_type = type_int_ptr;         break;
		case FMT_MOD_hh:   expected_type = type_signed_char_ptr; break;
		case FMT_MOD_h:    expected_type = type_short_ptr;       break;
		case FMT_MOD_l:    expected_type = type_long_ptr;        break;
		case FMT_MOD_ll:   expected_type = type_long_long_ptr;   break;
		case FMT_MOD_j:    expected_type = type_intmax_t_ptr;    break;
		case FMT_MOD_z:    expected_type = type_ssize_t_ptr;     break;
		case FMT_MOD_t:    expected_type = type_ptrdiff_t_ptr;   break;

		default:
bad_len_mod:
			warn_invalid_length_modifier(env->pos, fmt_mod, fmt);
			goto bad_spec;
		}
		allowed_flags = FMT_FLAG_NONE;
		break;

	default:
		warningf(WARN_FORMAT, env->pos, "encountered unknown conversion specifier '%%%c' at position %u", fmt, env->num_fmt);
bad_spec:
		expected_type = type_error_type;
		allowed_flags = ~FMT_FLAG_NONE;
		break;
	}

	format_flags_t const wrong_flags = fmt_flags & ~allowed_flags;
	if (wrong_flags != FMT_FLAG_NONE) {
		char  wrong[8];
		char *p = wrong;
		if (wrong_flags & FMT_FLAG_HASH)  *p++ = '#';
		if (wrong_flags & FMT_FLAG_ZERO)  *p++ = '0';
		if (wrong_flags & FMT_FLAG_MINUS) *p++ = '-';
		if (wrong_flags & FMT_FLAG_SPACE) *p++ = ' ';
		if (wrong_flags & FMT_FLAG_PLUS)  *p++ = '+';
		if (wrong_flags & FMT_FLAG_TICK)  *p++ = '\'';
		*p = '\0';

		warningf(WARN_FORMAT, env->pos, "invalid format flags \"%s\" in conversion specification %%%c at position %u", wrong, fmt, env->num_fmt);
	}

	check_argument_type(env, expected_type, spec_begin, c);
	return c;
}

/**
 * Check scanf-style format.
 */
static char const *check_scanf_format(format_env_t *const env, char const *c)
{
	if (accept(&c, '%'))
		return c;

	bool const suppress_assignment = accept(&c, '*');

	size_t width = 0;
	if (is_digit(*c)) {
		do {
			width = width * 10 + (*c++ - '0');
		} while (is_digit(*c));
		if (width == 0)
			warningf(WARN_FORMAT, env->pos, "field width is zero at format %u", env->num_fmt);
	}

	char              const *const spec_begin = c;
	format_length_modifier_t const fmt_mod    = parse_length_modifier(&c);

	type_t    *expected_type;
	char const fmt = *c++;
	switch (fmt) {
	case '\0':
		--c;
		expected_type = NULL;
		break;

	case 'd':
	case 'i':
		switch (fmt_mod) {
		case FMT_MOD_NONE: expected_type = type_int_ptr;         break;
		case FMT_MOD_hh:   expected_type = type_signed_char_ptr; break;
		case FMT_MOD_h:    expected_type = type_short_ptr;       break;
		case FMT_MOD_l:    expected_type = type_long_ptr;        break;
		case FMT_MOD_ll:   expected_type = type_long_long_ptr;   break;
		case FMT_MOD_j:    expected_type = type_intmax_t_ptr;    break;
		case FMT_MOD_z:    expected_type = type_ssize_t_ptr;     break;
		case FMT_MOD_t:    expected_type = type_ptrdiff_t_ptr;   break;
		case FMT_MOD_I:    expected_type = type_ptrdiff_t_ptr;   break;
		case FMT_MOD_I32:  expected_type = type_int32_ptr;       break;
		case FMT_MOD_I64:  expected_type = type_int64_ptr;       break;
		default:           goto bad_len_mod;
		}
		break;

	case 'o':
	case 'X':
	case 'x':
	case 'u':
		switch (fmt_mod) {
		case FMT_MOD_NONE: expected_type = type_unsigned_int_ptr;       break;
		case FMT_MOD_hh:   expected_type = type_unsigned_char_ptr;      break;
		case FMT_MOD_h:    expected_type = type_unsigned_short_ptr;     break;
		case FMT_MOD_l:    expected_type = type_unsigned_long_ptr;      break;
		case FMT_MOD_ll:   expected_type = type_unsigned_long_long_ptr; break;
		case FMT_MOD_j:    expected_type = type_uintmax_t_ptr;          break;
		case FMT_MOD_z:    expected_type = type_size_t_ptr;             break;
		case FMT_MOD_t:    expected_type = type_uptrdiff_t_ptr;         break;
		case FMT_MOD_I:    expected_type = type_size_t_ptr;             break;
		case FMT_MOD_I32:  expected_type = type_unsigned_int32_ptr;     break;
		case FMT_MOD_I64:  expected_type = type_unsigned_int64_ptr;     break;
		default:           goto bad_len_mod;
		}
		break;

	case 'A':
	case 'a':
	case 'E':
	case 'e':
	case 'F':
	case 'f':
	case 'G':
	case 'g':
		switch (fmt_mod) {
		case FMT_MOD_l:    expected_type = type_double_ptr;      break;
		case FMT_MOD_NONE: expected_type = type_float_ptr;       break;
		case FMT_MOD_L:    expected_type = type_long_double_ptr; break;
		default:           goto bad_len_mod;
		}
		break;

	case 'C':
		if (fmt_mod != FMT_MOD_NONE)
			goto bad_len_mod;
		expected_type = type_wchar_t_ptr;
		goto check_c_width;

	case 'c': {
		switch (fmt_mod) {
		case FMT_MOD_NONE: expected_type = type_char_ptr;    break;
		case FMT_MOD_l:    expected_type = type_wchar_t_ptr; break;
		case FMT_MOD_w:    expected_type = type_wchar_t_ptr; break;
		default:           goto bad_len_mod;
		}

check_c_width:
		if (width == 0)
			width = 1;
		if (!suppress_assignment && env->arg) {
			type_t *const type = skip_typeref(revert_automatic_type_conversion(env->arg->expression));
			if (is_type_array(type)       &&
					type->array.size_constant &&
					width > type->array.size) {
				warningf(WARN_FORMAT, env->pos, "target buffer '%T' is too small for %u characters at format %u", type, width, env->num_fmt);
			}
		}
		break;
	}

	case 'S':
		if (fmt_mod != FMT_MOD_NONE)
			goto bad_len_mod;
		expected_type = type_wchar_t_ptr;
		break;

	case 's':
	case '[': {
		switch (fmt_mod) {
		case FMT_MOD_NONE: expected_type = type_char_ptr;    break;
		case FMT_MOD_l:    expected_type = type_wchar_t_ptr; break;
		case FMT_MOD_w:    expected_type = type_wchar_t_ptr; break;
		default:           goto bad_len_mod;
		}

		if (!suppress_assignment && width != 0 && env->arg) {
			type_t *const type = skip_typeref(revert_automatic_type_conversion(env->arg->expression));
			if (is_type_array(type)       &&
					type->array.size_constant &&
					width >= type->array.size) {
				warningf(WARN_FORMAT, env->pos, "target buffer '%T' is too small for %u characters and \\0 at format %u", type, width, env->num_fmt);
			}
		}
		break;
	}

	case 'p':
		if (fmt_mod != FMT_MOD_NONE)
			goto bad_len_mod;
		expected_type = type_void_ptr;
		break;

	case 'n': {
		if (suppress_assignment)
			warningf(WARN_FORMAT, env->pos, "conversion '%n' cannot be suppressed with '*' at format %u", env->num_fmt);

		switch (fmt_mod) {
		case FMT_MOD_NONE: expected_type = type_int_ptr;         break;
		case FMT_MOD_hh:   expected_type = type_signed_char_ptr; break;
		case FMT_MOD_h:    expected_type = type_short_ptr;       break;
		case FMT_MOD_l:    expected_type = type_long_ptr;        break;
		case FMT_MOD_ll:   expected_type = type_long_long_ptr;   break;
		case FMT_MOD_j:    expected_type = type_intmax_t_ptr;    break;
		case FMT_MOD_z:    expected_type = type_ssize_t_ptr;     break;
		case FMT_MOD_t:    expected_type = type_ptrdiff_t_ptr;   break;

		default:
bad_len_mod:
			warn_invalid_length_modifier(env->pos, fmt_mod, fmt);
			goto bad_spec;
		}
		break;
	}

	default:
		warningf(WARN_FORMAT, env->pos, "encountered unknown conversion specifier '%%%c' at format %u", fmt, env->num_fmt);
bad_spec:
		expected_type = type_error_type;
		break;
	}

	if (!suppress_assignment)
		check_argument_type(env, expected_type, spec_begin, c);
	return c;
}

static char const *check_custom_format(format_env_t *const env, char const *c)
{
	attribute_format_argument_t const *const fmt = (attribute_format_argument_t const*)env->ctx;

	/* Skip format flags. */
	c += strspn(c, fmt->flags->begin);

	char const *const spec_begin = c;
	type_t           *expected_type;
	if (*c == '\0') {
		expected_type = NULL;
	} else {
		/* Determine format specifier. */
		attribute_format_specifier_t const *best_spec = NULL;
		char                         const *best_end  = c;
		for (attribute_format_specifier_t const *s = fmt->specifiers; s; s = s->next) {
			char const *const end = strstart(c, s->specifier->begin);
			if (end && best_end < end) {
				best_spec = s;
				best_end  = end;
			}
		}

		if (best_spec) {
			expected_type = best_spec->type;
		} else {
			warningf(WARN_FORMAT, env->pos, "encountered unknown conversion specifier '%%%c' at format %u", *c, env->num_fmt);
			expected_type = type_error_type;
		}
		c = best_end;
	}

	check_argument_type(env, expected_type, spec_begin, c);
	return c;
}

typedef char const *check_func_t(format_env_t *env, char const *c);

static int do_check_format(string_literal_expression_t const *const fmt_string, call_argument_t const *const arg, check_func_t *const check_func, void const *const ctx)
{
	format_env_t env = {
		.num_fmt = 0,
		.num_arg = 0,
		.arg     = arg,
		.pos     = &fmt_string->base.pos,
		.ctx     = ctx,
	};

	char const *const string = fmt_string->value->begin;
	for (char const *c = string;;) {
		if (*c == '\0') {
			if (c + 1 < string + fmt_string->value->size)
				warningf(WARN_FORMAT, env.pos, "format string contains '\\0'");
			break;
		} else if (*c++ == '%') {
			++env.num_fmt;
			c = check_func(&env, c);
			if (!c)
				break;
		}
	}
	return env.num_arg;
}

static int internal_check_format_recursive(expression_t const *const fmt_expr, call_argument_t const *const arg, check_func_t *const check_func, void const *const ctx)
{
	switch (fmt_expr->kind) {
	case EXPR_CONDITIONAL: {
		/* gettext results in expressions like (X ? "format_string" : Y).
		 * Recursively check both alternatives. */
		conditional_expression_t const *const c = &fmt_expr->conditional;
		expression_t             const *      t = c->true_expression;
		if (!t)
			t = c->condition;
		int const nt = internal_check_format_recursive(t,                   arg, check_func, ctx);
		int const nf = internal_check_format_recursive(c->false_expression, arg, check_func, ctx);
		return MAX(nt, nf);
	}

	case EXPR_STRING_LITERAL:
		return do_check_format(&fmt_expr->string_literal, arg, check_func, ctx);

	case EXPR_UNARY_CAST:
		return internal_check_format_recursive(fmt_expr->unary.value, arg, check_func, ctx);

	default:
		return -1;
	}
}

static void internal_check_format(unsigned const fmt_idx, unsigned const arg_idx, call_argument_t const *arg, check_func_t *const check_func, void const *const ctx)
{
	unsigned idx = 0;

	/* Find format argument. */
	for (;; ++idx, arg = arg->next) {
		if (!arg)
			return;
		if (idx == fmt_idx)
			break;
	}
	expression_t const *const fmt_expr = arg->expression;

	/* Find start of variadic arguments. */
	for (; arg; ++idx, arg = arg->next) {
		if (idx == arg_idx)
			break;
	}

	int const num_fmt = internal_check_format_recursive(fmt_expr, arg, check_func, ctx);
	if (num_fmt < 0)
		return;

	unsigned num_args = 0;
	for (; arg; arg = arg->next) {
		++num_args;
	}
	if (num_args > (unsigned)num_fmt) {
		position_t const *const pos = &fmt_expr->base.pos;
		warningf(WARN_FORMAT, pos, "%u argument%s given but only %u argument%s used", num_args, num_args != 1 ? "s" : "", num_fmt,  num_fmt  != 1 ? "s" : "");
	}
}

static const format_spec_t builtin_table[] = {
	{ "printf",        FORMAT_PRINTF,   0, 1 },
	{ "wprintf",       FORMAT_PRINTF,   0, 1 },
	{ "sprintf",       FORMAT_PRINTF,   1, 2 },
	{ "swprintf",      FORMAT_PRINTF,   1, 2 },
	{ "snprintf",      FORMAT_PRINTF,   2, 3 },
	{ "snwprintf",     FORMAT_PRINTF,   2, 3 },
	{ "fprintf",       FORMAT_PRINTF,   1, 2 },
	{ "fwprintf",      FORMAT_PRINTF,   1, 2 },
	{ "snwprintf",     FORMAT_PRINTF,   2, 3 },
	{ "snwprintf",     FORMAT_PRINTF,   2, 3 },

	{ "scanf",         FORMAT_SCANF,    0, 1 },
	{ "wscanf",        FORMAT_SCANF,    0, 1 },
	{ "sscanf",        FORMAT_SCANF,    1, 2 },
	{ "swscanf",       FORMAT_SCANF,    1, 2 },
	{ "fscanf",        FORMAT_SCANF,    1, 2 },
	{ "fwscanf",       FORMAT_SCANF,    1, 2 },

	{ "strftime",      FORMAT_STRFTIME, 3, 4 },
	{ "wcstrftime",    FORMAT_STRFTIME, 3, 4 },

	{ "strfmon",       FORMAT_STRFMON,  3, 4 },

	/* MS extensions */
	{ "_snprintf",     FORMAT_PRINTF,   2, 3 },
	{ "_snwprintf",    FORMAT_PRINTF,   2, 3 },
	{ "_scrintf",      FORMAT_PRINTF,   0, 1 },
	{ "_scwprintf",    FORMAT_PRINTF,   0, 1 },
	{ "printf_s",      FORMAT_PRINTF,   0, 1 },
	{ "wprintf_s",     FORMAT_PRINTF,   0, 1 },
	{ "sprintf_s",     FORMAT_PRINTF,   3, 4 },
	{ "swprintf_s",    FORMAT_PRINTF,   3, 4 },
	{ "fprintf_s",     FORMAT_PRINTF,   1, 2 },
	{ "fwprintf_s",    FORMAT_PRINTF,   1, 2 },
	{ "_sprintf_l",    FORMAT_PRINTF,   1, 3 },
	{ "_swprintf_l",   FORMAT_PRINTF,   1, 3 },
	{ "_printf_l",     FORMAT_PRINTF,   0, 2 },
	{ "_wprintf_l",    FORMAT_PRINTF,   0, 2 },
	{ "_fprintf_l",    FORMAT_PRINTF,   1, 3 },
	{ "_fwprintf_l",   FORMAT_PRINTF,   1, 3 },
	{ "_printf_s_l",   FORMAT_PRINTF,   0, 2 },
	{ "_wprintf_s_l",  FORMAT_PRINTF,   0, 2 },
	{ "_sprintf_s_l",  FORMAT_PRINTF,   3, 5 },
	{ "_swprintf_s_l", FORMAT_PRINTF,   3, 5 },
	{ "_fprintf_s_l",  FORMAT_PRINTF,   1, 3 },
	{ "_fwprintf_s_l", FORMAT_PRINTF,   1, 3 },
};

void check_format(const call_expression_t *const call)
{
	if (!is_warn_on(WARN_FORMAT))
		return;

	const expression_t *const func_expr = call->function;
	if (func_expr->kind != EXPR_REFERENCE)
		return;

	const entity_t        *const entity = func_expr->reference.entity;
	const call_argument_t *      arg    = call->arguments;

	/*
	 * For some functions we always check the format, even if it was not
	 * specified. This allows to check format even in MS mode or without
	 * header included.
	 */
	const char *const name = entity->base.symbol->string;
	for (format_spec_t const *i = builtin_table; i != endof(builtin_table); ++i) {
		if (streq(name, i->name)) {
			switch (i->fmt_kind) {
			case FORMAT_PRINTF: internal_check_format(i->fmt_idx, i->arg_idx, arg, &check_printf_format, NULL); break;
			case FORMAT_SCANF:  internal_check_format(i->fmt_idx, i->arg_idx, arg, &check_scanf_format,  NULL); break;
			case FORMAT_STRFTIME:
			case FORMAT_STRFMON:
				/* TODO: implement other cases */
				break;
			}
			break;
		}
	}

	attribute_t const *const fmt_attr = get_attribute(entity->declaration.attributes, ATTRIBUTE_CPARSER_CUSTOM_FORMAT);
	if (fmt_attr) {
		attribute_format_argument_t const *const fmt = fmt_attr->a.format;
		if (is_constant_expression(fmt->fmt_idx) == EXPR_CLASS_INTEGER_CONSTANT) {
			unsigned const idx = fold_expression_to_int(fmt->fmt_idx) - 1;
			internal_check_format(idx, idx + 1 /* TODO make more flexible */, arg, &check_custom_format, fmt);
		}
	}
}
