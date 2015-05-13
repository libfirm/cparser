/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "format_check.h"

#include <ctype.h>

#include "adt/strutil.h"
#include "adt/util.h"
#include "ast/ast_t.h"
#include "ast/entity_t.h"
#include "ast/symbol_t.h"
#include "ast/type_t.h"
#include "ast/types.h"
#include "driver/diagnostic.h"
#include "driver/lang_features.h"
#include "driver/warning.h"
#include "parser.h"

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

/**
 * Check printf-style format. Returns number of expected arguments.
 */
static int check_printf_format(expression_t const *fmt_expr, call_argument_t const *arg)
{
	while (fmt_expr->kind == EXPR_UNARY_CAST) {
		fmt_expr = fmt_expr->unary.value;
	}

	/*
	 * gettext results in expressions like (X ? "format_string" : Y)
	 * we assume the left part is the format string
	 */
	if (fmt_expr->kind == EXPR_CONDITIONAL) {
		conditional_expression_t const *const c = &fmt_expr->conditional;
		expression_t             const *      t = c->true_expression;
		if (t == NULL)
			t = c->condition;
		int const nt = check_printf_format(t,                   arg);
		int const nf = check_printf_format(c->false_expression, arg);
		return MAX(nt, nf);
	}

	if (fmt_expr->kind != EXPR_STRING_LITERAL)
		return -1;

	const char *string = fmt_expr->string_literal.value->begin;
	size_t      size   = fmt_expr->string_literal.value->size;
	const char *c      = string;

	const position_t *pos = &fmt_expr->base.pos;
	unsigned num_fmt  = 0;
	unsigned num_args = 0;
	char     fmt;
	for (fmt = *c; fmt != '\0'; fmt = *(++c)) {
		if (fmt != '%')
			continue;
		fmt = *(++c);

		if (fmt == '%')
			continue;

		++num_fmt;
		++num_args;

		format_flags_t fmt_flags = FMT_FLAG_NONE;
		if (fmt == '0') {
			fmt = *(++c);
			fmt_flags |= FMT_FLAG_ZERO;
		}

		/* argument selector or minimum field width */
		if (isdigit((unsigned char)fmt)) {
			do {
				fmt = *(++c);
			} while (isdigit((unsigned char)fmt));

			/* digit string was ... */
			if (fmt == '$') {
				/* ... argument selector */
				fmt_flags = FMT_FLAG_NONE; /* reset possibly set 0-flag */
				/* TODO implement */
				return -1;
			}
			/* ... minimum field width */
		} else {
			/* flags */
			for (;;) {
				format_flags_t flag;
				switch (fmt) {
					case '#':  flag = FMT_FLAG_HASH;  break;
					case '0':  flag = FMT_FLAG_ZERO;  break;
					case '-':  flag = FMT_FLAG_MINUS; break;
					case '\'': flag = FMT_FLAG_TICK;  break;

					case ' ':
						if (fmt_flags & FMT_FLAG_PLUS) {
							warningf(WARN_FORMAT, pos, "' ' is overridden by prior '+' in conversion specification %u", num_fmt);
						}
						flag = FMT_FLAG_SPACE;
						break;

					case '+':
						if (fmt_flags & FMT_FLAG_SPACE) {
							warningf(WARN_FORMAT, pos, "'+' overrides prior ' ' in conversion specification %u", num_fmt);
						}
						flag = FMT_FLAG_PLUS;
						break;

					default: goto break_fmt_flags;
				}
				if (fmt_flags & flag) {
					warningf(WARN_FORMAT, pos, "repeated flag '%c' in conversion specification %u", (char)fmt, num_fmt);
				}
				fmt_flags |= flag;
				fmt = *(++c);
			}
break_fmt_flags:

			/* minimum field width */
			if (fmt == '*') {
				++num_args;
				fmt = *(++c);
				if (arg == NULL) {
					warningf(WARN_FORMAT, pos, "missing argument for '*' field width in conversion specification %u", num_fmt);
					return -1;
				}
				const type_t *const arg_type = arg->expression->base.type;
				if (arg_type != type_int) {
					warningf(WARN_FORMAT, pos, "argument for '*' field width in conversion specification %u is not an 'int', but an '%T'", num_fmt, arg_type);
				}
				arg = arg->next;
			} else {
				while (isdigit((unsigned char)fmt)) {
					fmt = *(++c);
				}
			}
		}

		/* precision */
		if (fmt == '.') {
			if (fmt_flags & FMT_FLAG_ZERO) {
				warningf(WARN_FORMAT, pos, "'0' flag ignored with precision in conversion specification %u", num_fmt);
			}

			++num_args;
			fmt = *(++c);
			if (fmt == '*') {
				fmt = *(++c);
				if (arg == NULL) {
					warningf(WARN_FORMAT, pos, "missing argument for '*' precision in conversion specification %u", num_fmt);
					return -1;
				}
				const type_t *const arg_type = arg->expression->base.type;
				if (arg_type != type_int) {
					warningf(WARN_FORMAT, pos, "argument for '*' precision in conversion specification %u is not an 'int', but an '%T'", num_fmt, arg_type);
				}
				arg = arg->next;
			} else {
				/* digit string may be omitted */
				while (isdigit((unsigned char)fmt)) {
					fmt = *(++c);
				}
			}
		}

		format_length_modifier_t const fmt_mod = parse_length_modifier(&c);
		fmt = *c;

		if (fmt == '\0') {
			warningf(WARN_FORMAT, pos, "dangling %% in format string");
			break;
		}

		type_t        *expected_type;
		format_flags_t allowed_flags;
		switch (fmt) {
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

					default:
						warn_invalid_length_modifier(pos, fmt_mod, fmt);
						goto next_arg;
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

					default:
						warn_invalid_length_modifier(pos, fmt_mod, fmt);
						goto next_arg;
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

					default:
						warn_invalid_length_modifier(pos, fmt_mod, fmt);
						goto next_arg;
				}
				allowed_flags = FMT_FLAG_MINUS | FMT_FLAG_SPACE | FMT_FLAG_PLUS | FMT_FLAG_HASH | FMT_FLAG_ZERO;
				break;

			case 'C':
				if (fmt_mod != FMT_MOD_NONE) {
					warn_invalid_length_modifier(pos, fmt_mod, fmt);
					goto next_arg;
				}
				expected_type = type_wchar_t;
				allowed_flags = FMT_FLAG_NONE;
				break;

			case 'c':
				expected_type = type_int;
				switch (fmt_mod) {
					case FMT_MOD_NONE: expected_type = type_int;     break; /* TODO promoted char */
					case FMT_MOD_l:    expected_type = type_wint_t;  break;
					case FMT_MOD_w:    expected_type = type_wchar_t; break;

					default:
						warn_invalid_length_modifier(pos, fmt_mod, fmt);
						goto next_arg;
				}
				allowed_flags = FMT_FLAG_NONE;
				break;

			case 'S':
				if (fmt_mod != FMT_MOD_NONE) {
					warn_invalid_length_modifier(pos, fmt_mod, fmt);
					goto next_arg;
				}
				expected_type = type_const_wchar_t_ptr;
				allowed_flags = FMT_FLAG_MINUS;
				break;

			case 's':
				switch (fmt_mod) {
					case FMT_MOD_NONE: expected_type = type_const_char_ptr;    break;
					case FMT_MOD_l:    expected_type = type_const_wchar_t_ptr; break;
					case FMT_MOD_w:    expected_type = type_const_wchar_t_ptr; break;

					default:
						warn_invalid_length_modifier(pos, fmt_mod, fmt);
						goto next_arg;
				}
				allowed_flags = FMT_FLAG_MINUS;
				break;

			case 'p':
				if (fmt_mod != FMT_MOD_NONE) {
					warn_invalid_length_modifier(pos, fmt_mod, fmt);
					goto next_arg;
				}
				expected_type = type_void_ptr;
				allowed_flags = FMT_FLAG_NONE;
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
						warn_invalid_length_modifier(pos, fmt_mod, fmt);
						goto next_arg;
				}
				allowed_flags = FMT_FLAG_NONE;
				break;

			default:
				warningf(WARN_FORMAT, pos, "encountered unknown conversion specifier '%%%c' at position %u", fmt, num_fmt);
				if (arg == NULL) {
					goto too_few_args;
				}
				goto next_arg;
		}

		format_flags_t wrong_flags = fmt_flags & ~allowed_flags;
		if (wrong_flags != 0) {
			char  wrong[8];
			char *p = wrong;
			if (wrong_flags & FMT_FLAG_HASH)  *p++ = '#';
			if (wrong_flags & FMT_FLAG_ZERO)  *p++ = '0';
			if (wrong_flags & FMT_FLAG_MINUS) *p++ = '-';
			if (wrong_flags & FMT_FLAG_SPACE) *p++ = ' ';
			if (wrong_flags & FMT_FLAG_PLUS)  *p++ = '+';
			if (wrong_flags & FMT_FLAG_TICK)  *p++ = '\'';
			*p = '\0';

			warningf(WARN_FORMAT, pos, "invalid format flags \"%s\" in conversion specification %%%c at position %u", wrong, fmt, num_fmt);
		}

		if (arg == NULL) {
too_few_args:
			warningf(WARN_FORMAT, pos, "too few arguments for format string");
			return -1;
		}

		{ /* create a scope here to prevent warning about the jump to next_arg */
			type_t *const arg_type           = arg->expression->base.type;
			type_t *const arg_skip           = skip_typeref(arg_type);
			type_t *const expected_type_skip = skip_typeref(expected_type);

			if (fmt == 'p') {
				/* allow any pointer type for %p, not just void */
				if (is_type_pointer(arg_skip))
					goto next_arg;
			}

			if (is_type_pointer(expected_type_skip)) {
				if (is_type_pointer(arg_skip)) {
					type_t *const exp_to = skip_typeref(expected_type_skip->pointer.points_to);
					type_t *const arg_to = skip_typeref(arg_skip->pointer.points_to);
					if ((arg_to->base.qualifiers & ~exp_to->base.qualifiers) == 0 &&
						types_compatible_ignore_qualifiers(arg_to, exp_to))
						goto next_arg;
				}
			} else if (types_compatible_ignore_qualifiers(arg_skip,
			                                              expected_type_skip)) {
				goto next_arg;
			} else if (arg->expression->kind == EXPR_UNARY_CAST) {
				expression_t const *const expr        = arg->expression->unary.value;
				type_t             *const unprom_type = skip_typeref(expr->base.type);
				if (types_compatible_ignore_qualifiers(unprom_type,
				                                       expected_type_skip)) {
					goto next_arg;
				}
				if (expected_type_skip == type_unsigned_int
				    && !is_type_signed(unprom_type)) {
					goto next_arg;
				}
			}
			if (is_type_valid(arg_skip)) {
				position_t const *const apos = &arg->expression->base.pos;
				char       const *const mod  = get_length_modifier_name(fmt_mod);
				warningf(WARN_FORMAT, apos, "conversion '%%%s%c' at position %u specifies type '%T' but the argument has type '%T'", mod, (char)fmt, num_fmt, expected_type, arg_type);
			}
		}
next_arg:
		arg = arg->next;
	}
	assert(fmt == '\0');
	if (c+1 < string + size) {
		warningf(WARN_FORMAT, pos, "format string contains '\\0'");
	}
	return num_args;
}

/**
 * Check scanf-style format.
 */
static int check_scanf_format(expression_t const *fmt_expr, call_argument_t const *arg)
{
	if (fmt_expr->kind == EXPR_UNARY_CAST) {
		fmt_expr = fmt_expr->unary.value;
	}

	if (fmt_expr->kind != EXPR_STRING_LITERAL)
		return -1;

	const char *string = fmt_expr->string_literal.value->begin;
	size_t      size   = fmt_expr->string_literal.value->size;
	const char *c      = string;

	const position_t *pos = &fmt_expr->base.pos;
	unsigned num_fmt = 0;
	char     fmt;
	for (fmt = *c; fmt != '\0'; fmt = *(++c)) {
		if (fmt != '%')
			continue;
		fmt = *(++c);
		if (fmt == '%')
			continue;

		++num_fmt;

		bool suppress_assignment = false;
		if (fmt == '*') {
			fmt = *++c;
			suppress_assignment = true;
		}

		size_t width = 0;
		if ('0' <= fmt && fmt <= '9') {
			do {
				width = width * 10 + (fmt - '0');
				fmt   = *++c;
			} while ('0' <= fmt && fmt <= '9');
			if (width == 0) {
				warningf(WARN_FORMAT, pos, "field width is zero at format %u", num_fmt);
			}
		}

		format_length_modifier_t const fmt_mod = parse_length_modifier(&c);
		fmt = *c;

		if (fmt == '\0') {
			warningf(WARN_FORMAT, pos, "dangling %% with conversion specififer in format string");
			break;
		}

		type_t *expected_type;
		switch (fmt) {
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

			default:
				warn_invalid_length_modifier(pos, fmt_mod, fmt);
				goto next_arg;
			}
			break;

		case 'o':
		case 'X':
		case 'x':
		case 'u':
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

			default:
				warn_invalid_length_modifier(pos, fmt_mod, fmt);
				goto next_arg;
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
			case FMT_MOD_l:    expected_type = type_double;      break;
			case FMT_MOD_NONE: expected_type = type_float;       break;
			case FMT_MOD_L:    expected_type = type_long_double; break;

			default:
				warn_invalid_length_modifier(pos, fmt_mod, fmt);
				goto next_arg;
			}
			break;

		case 'C':
			if (fmt_mod != FMT_MOD_NONE) {
				warn_invalid_length_modifier(pos, fmt_mod, fmt);
				goto next_arg;
			}
			expected_type = type_wchar_t;
			goto check_c_width;

		case 'c': {
			switch (fmt_mod) {
			case FMT_MOD_NONE: expected_type = type_char;    break;
			case FMT_MOD_l:    expected_type = type_wchar_t; break;
			case FMT_MOD_w:    expected_type = type_wchar_t; break;

			default:
				warn_invalid_length_modifier(pos, fmt_mod, fmt);
				goto next_arg;
			}

check_c_width:
			if (width == 0)
				width = 1;
			if (!suppress_assignment && arg != NULL) {
				type_t *const type = skip_typeref(revert_automatic_type_conversion(arg->expression));
				if (is_type_array(type)       &&
				    type->array.size_constant &&
				    width > type->array.size) {
					warningf(WARN_FORMAT, pos, "target buffer '%T' is too small for %u characters at format %u", type, width, num_fmt);
				}
			}
			break;
		}

		case 'S':
			if (fmt_mod != FMT_MOD_NONE) {
				warn_invalid_length_modifier(pos, fmt_mod, fmt);
				goto next_arg;
			}
			expected_type = type_wchar_t;
			break;

		case 's':
		case '[': {
			switch (fmt_mod) {
				case FMT_MOD_NONE: expected_type = type_char;    break;
				case FMT_MOD_l:    expected_type = type_wchar_t; break;
				case FMT_MOD_w:    expected_type = type_wchar_t; break;

				default:
					warn_invalid_length_modifier(pos, fmt_mod, fmt);
					goto next_arg;
			}

			if (!suppress_assignment &&
			    width != 0           &&
			    arg   != NULL) {
				type_t *const type = skip_typeref(revert_automatic_type_conversion(arg->expression));
				if (is_type_array(type)       &&
				    type->array.size_constant &&
				    width >= type->array.size) {
					warningf(WARN_FORMAT, pos, "target buffer '%T' is too small for %u characters and \\0 at format %u", type, width, num_fmt);
				}
			}
			break;
		}

		case 'p':
			if (fmt_mod != FMT_MOD_NONE) {
				warn_invalid_length_modifier(pos, fmt_mod, fmt);
				goto next_arg;
			}
			expected_type = type_void;
			break;

		case 'n': {
			if (suppress_assignment) {
				warningf(WARN_FORMAT, pos, "conversion '%n' cannot be suppressed with '*' at format %u", num_fmt);
			}

			switch (fmt_mod) {
			case FMT_MOD_NONE: expected_type = type_int;         break;
			case FMT_MOD_hh:   expected_type = type_signed_char; break;
			case FMT_MOD_h:    expected_type = type_short;       break;
			case FMT_MOD_l:    expected_type = type_long;        break;
			case FMT_MOD_ll:   expected_type = type_long_long;   break;
			case FMT_MOD_j:    expected_type = type_intmax_t;    break;
			case FMT_MOD_z:    expected_type = type_ssize_t;     break;
			case FMT_MOD_t:    expected_type = type_ptrdiff_t;   break;

			default:
				warn_invalid_length_modifier(pos, fmt_mod, fmt);
				goto next_arg;
			}
			break;
		}

		default:
			warningf(WARN_FORMAT, pos, "encountered unknown conversion specifier '%%%c' at format %u", fmt, num_fmt);
			if (suppress_assignment)
				continue;
			if (arg == NULL)
				goto too_few_args;
			goto next_arg;
		}

		if (suppress_assignment)
			continue;

		if (arg == NULL) {
too_few_args:
			warningf(WARN_FORMAT, pos, "too few arguments for format string");
			return -1;
		}

		{ /* create a scope here to prevent warning about the jump to next_arg */
			type_t *const arg_type           = arg->expression->base.type;
			type_t *const arg_skip           = skip_typeref(arg_type);
			type_t *const expected_type_skip = skip_typeref(expected_type);

			if (!is_type_pointer(arg_skip))
				goto error_arg_type;
			type_t *const ptr_skip = skip_typeref(arg_skip->pointer.points_to);

			if (fmt == 'p') {
				/* allow any pointer type for %p, not just void */
				if (is_type_pointer(ptr_skip))
					goto next_arg;
			}

			/* do NOT allow const or restrict, all other should be ok */
			if (ptr_skip->base.qualifiers & (TYPE_QUALIFIER_CONST | TYPE_QUALIFIER_VOLATILE))
				goto error_arg_type;
			if (types_compatible_ignore_qualifiers(ptr_skip,
			                                       expected_type_skip)) {
				goto next_arg;
			} else if (expected_type_skip == type_char) {
				/* char matches with unsigned char AND signed char */
				if (types_compatible_ignore_qualifiers(ptr_skip, type_signed_char)
				 || types_compatible_ignore_qualifiers(ptr_skip, type_unsigned_char))
					goto next_arg;
			}
error_arg_type:
			if (is_type_valid(arg_skip)) {
				position_t const *const apos = &arg->expression->base.pos;
				char       const *const mod  = get_length_modifier_name(fmt_mod);
				warningf(WARN_FORMAT, apos, "conversion '%%%s%c' at position %u specifies type '%T*' but the argument has type '%T'", mod, (char)fmt, num_fmt, expected_type, arg_type);
			}
		}
next_arg:
		arg = arg->next;
	}
	assert(fmt == '\0');
	if (c+1 < string + size) {
		warningf(WARN_FORMAT, pos, "format string contains '\\0'");
	}
	return num_fmt;
}

typedef int check_func_t(expression_t const *fmt_expr, call_argument_t const *arg);

static void internal_check_format(format_spec_t const *const spec, call_argument_t const *arg, check_func_t *const check_func)
{
	unsigned idx = 0;

	/* Find format argument. */
	for (;; ++idx, arg = arg->next) {
		if (!arg)
			return;
		if (idx == spec->fmt_idx)
			break;
	}
	expression_t const *const fmt_expr = arg->expression;

	/* Find start of variadic arguments. */
	for (; arg; ++idx, arg = arg->next) {
		if (idx == spec->arg_idx)
			break;
	}

	int const num_fmt = check_func(fmt_expr, arg);
	if (num_fmt < 0)
		return;

	unsigned num_args = 0;
	for (; arg; arg = arg->next) {
		++num_args;
	}
	if (num_args > (unsigned)num_fmt) {
		position_t const *const pos = &fmt_expr->base.pos;
		warningf(WARN_FORMAT, pos, "%u argument%s but only %u format specifier%s", num_args, num_args != 1 ? "s" : "", num_fmt,  num_fmt  != 1 ? "s" : "");
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
			case FORMAT_PRINTF: internal_check_format(i, arg, &check_printf_format); break;
			case FORMAT_SCANF:  internal_check_format(i, arg, &check_scanf_format);  break;
			case FORMAT_STRFTIME:
			case FORMAT_STRFMON:
				/* TODO: implement other cases */
				break;
			}
			break;
		}
	}
}
