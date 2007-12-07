#include <wctype.h>

#include "ast_t.h"
#include "diagnostic.h"
#include "format_check.h"
#include "types.h"


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
	FMT_MOD_q
} format_length_modifier_t;

static void warn_invalid_length_modifier(const source_position_t pos,
                                         const format_length_modifier_t mod,
                                         const char conversion)
{
	static const char* const names[] = {
		[FMT_MOD_NONE] = "",
		[FMT_MOD_L]    = "L",
		[FMT_MOD_hh]   = "hh",
		[FMT_MOD_h]    = "h",
		[FMT_MOD_l]    = "l",
		[FMT_MOD_ll]   = "ll",
		[FMT_MOD_j]    = "j",
		[FMT_MOD_t]    = "t",
		[FMT_MOD_z]    = "z",
		[FMT_MOD_q]    = "q"
	};
	assert(mod < sizeof(names) / sizeof(*names));

	warningf(pos,
		"invalid length modifier '%s' for conversion specifier '%%%c'",
		names[mod], conversion
	);
}

static void check_format_arguments(const call_argument_t *const fmt_arg, const call_argument_t* arg)
{
	const expression_t *fmt_expr = fmt_arg->expression;
	if (fmt_expr->kind == EXPR_UNARY_CAST_IMPLICIT) {
		fmt_expr = fmt_expr->unary.value;
	}

	if (fmt_expr->kind != EXPR_WIDE_STRING_LITERAL)
		return;

	const source_position_t    pos     = fmt_expr->base.source_position;
	const wide_string_t *const wstring = &fmt_expr->wide_string.value;
	const wchar_rep_t *fmt = wstring->begin;
	for (; *fmt != '\0'; ++fmt) {
		if (*fmt != '%')
			continue;
		++fmt;

		if (*fmt == '%')
			continue;

		format_flags_t fmt_flags = FMT_FLAG_NONE;
		if (*fmt == '0') {
			++fmt;
			fmt_flags |= FMT_FLAG_ZERO;
		}

		/* argument selector or minimum field width */
		if (iswdigit(*fmt)) {
			do {
				++fmt;
			} while (iswdigit(*fmt));

			/* digit string was ... */
			if (*fmt == '$') {
				/* ... argument selector */
				fmt_flags = FMT_FLAG_NONE; /* reset possibly set 0-flag */
				/* TODO implement */
				return;
			}
			/* ... minimum field width */
		} else {
			/* flags */
			for (;;) {
				format_flags_t flag;
				switch (*fmt) {
					case '#':  flag = FMT_FLAG_HASH;  break;
					case '0':  flag = FMT_FLAG_ZERO;  break;
					case '-':  flag = FMT_FLAG_MINUS; break;
					case '\'': flag = FMT_FLAG_TICK;  break;

					case ' ':
						if (fmt_flags & FMT_FLAG_PLUS) {
							warningf(pos, "' ' is overridden by prior '+' in conversion specification");
						}
						flag = FMT_FLAG_SPACE;
						break;

					case '+':
						if (fmt_flags & FMT_FLAG_SPACE) {
							warningf(pos, "'+' overrides prior ' ' in conversion specification");
						}
						flag = FMT_FLAG_PLUS;
						break;

					default: goto break_fmt_flags;
				}
				if (fmt_flags & flag) {
					warningf(pos, "repeated flag '%c' in conversion specification", (char)*fmt);
				}
				fmt_flags |= flag;
				++fmt;
			}
break_fmt_flags:

			/* minimum field width */
			if (*fmt == '*') {
				if (arg == NULL) {
					warningf(pos, "missing argument for '*' field width in conversion specification");
					return;
				}
				const type_t *const arg_type = arg->expression->base.datatype;
				if (arg_type != type_int) {
					warningf(pos, "argument for '*' field width in conversion specification is not an 'int', but an '%T'", arg_type);
				}
				arg = arg->next;
			} else {
				while (iswdigit(*fmt)) {
					++fmt;
				}
			}
		}

		/* precision */
		if (*fmt == '.') {
			++fmt;
			if (*fmt == '*') {
				if (arg == NULL) {
					warningf(pos, "missing argument for '*' precision in conversion specification");
					return;
				}
				const type_t *const arg_type = arg->expression->base.datatype;
				if (arg_type != type_int) {
					warningf(pos, "argument for '*' precision in conversion specification is not an 'int', but an '%T'", arg_type);
				}
				arg = arg->next;
			} else {
				/* digit string may be omitted */
				while (iswdigit(*fmt)) {
					++fmt;
				}
			}
		}

		/* length modifier */
		format_length_modifier_t fmt_mod;
		switch (*fmt) {
			case 'h':
				++fmt;
				if (*fmt == 'h') {
					++fmt;
					fmt_mod = FMT_MOD_hh;
				} else {
					fmt_mod = FMT_MOD_h;
				}
				break;

			case 'l':
				++fmt;
				if (*fmt == 'l') {
					++fmt;
					fmt_mod = FMT_MOD_ll;
				} else {
					fmt_mod = FMT_MOD_l;
				}
				break;

			case 'L': ++fmt; fmt_mod = FMT_MOD_L;    break;
			case 'j': ++fmt; fmt_mod = FMT_MOD_j;    break;
			case 't': ++fmt; fmt_mod = FMT_MOD_t;    break;
			case 'z': ++fmt; fmt_mod = FMT_MOD_z;    break;
			case 'q': ++fmt; fmt_mod = FMT_MOD_q;    break;
			default:         fmt_mod = FMT_MOD_NONE; break;
		}

		if (*fmt == '\0') {
			warningf(pos, "dangling %% in format string");
			break;
		}

		const type_t   *expected_type;
		format_flags_t  allowed_flags;
		switch (*fmt) {
			case 'd':
			case 'i':
				switch (fmt_mod) {
					case FMT_MOD_NONE: expected_type = type_int;       break;
					case FMT_MOD_hh:   expected_type = type_int;       break; /* TODO promoted signed char */
					case FMT_MOD_h:    expected_type = type_int;       break; /* TODO promoted short */
					case FMT_MOD_l:    expected_type = type_long;      break;
					case FMT_MOD_ll:   expected_type = type_long_long; break;
					case FMT_MOD_j:    expected_type = type_intmax_t;  break;
					case FMT_MOD_z:    expected_type = type_ssize_t;   break;
					case FMT_MOD_t:    expected_type = type_ptrdiff_t; break;

					default:
						warn_invalid_length_modifier(pos, fmt_mod, *fmt);
						break;
				}
				allowed_flags = FMT_FLAG_MINUS | FMT_FLAG_PLUS | FMT_FLAG_ZERO;
				break;

			case 'o':
			case 'X':
			case 'x':
				allowed_flags = FMT_FLAG_MINUS | FMT_FLAG_HASH | FMT_FLAG_ZERO;
				goto eval_fmt_mod_unsigned;
				break;

			case 'u':
				allowed_flags = FMT_FLAG_MINUS | FMT_FLAG_ZERO;
eval_fmt_mod_unsigned:
				switch (fmt_mod) {
					case FMT_MOD_NONE: expected_type = type_unsigned_int;       break;
					case FMT_MOD_hh:   expected_type = type_int;                break; /* TODO promoted unsigned char */
					case FMT_MOD_h:    expected_type = type_int;                break; /* TODO promoted unsigned short */
					case FMT_MOD_l:    expected_type = type_unsigned_long;      break;
					case FMT_MOD_ll:   expected_type = type_unsigned_long_long; break;
					case FMT_MOD_j:    expected_type = type_uintmax_t;          break;
					case FMT_MOD_z:    expected_type = type_size_t;             break;
					case FMT_MOD_t:    expected_type = type_uptrdiff_t;         break;

					default:
						warn_invalid_length_modifier(pos, fmt_mod, *fmt);
						break;
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
						warn_invalid_length_modifier(pos, fmt_mod, *fmt);
						break;
				}
				allowed_flags = FMT_FLAG_MINUS | FMT_FLAG_PLUS | FMT_FLAG_HASH | FMT_FLAG_ZERO;
				break;

			case 'C':
				if (fmt_mod != FMT_MOD_NONE) {
					warn_invalid_length_modifier(pos, fmt_mod, *fmt);
				}
				expected_type = type_wchar_t;
				allowed_flags = FMT_FLAG_NONE;
				break;

			case 'c':
				expected_type = type_int;
				switch (fmt_mod) {
					case FMT_MOD_NONE: expected_type = type_int;    break; /* TODO promoted char */
					case FMT_MOD_l:    expected_type = type_wint_t; break;

					default:
						warn_invalid_length_modifier(pos, fmt_mod, *fmt);
						break;
				}
				allowed_flags = FMT_FLAG_NONE;
				break;

			case 'S':
				if (fmt_mod != FMT_MOD_NONE) {
					warn_invalid_length_modifier(pos, fmt_mod, *fmt);
				}
				expected_type = type_wchar_t_ptr;
				allowed_flags = FMT_FLAG_NONE;
				break;

			case 's':
				switch (fmt_mod) {
					case FMT_MOD_NONE: expected_type = type_string;      break;
					case FMT_MOD_l:    expected_type = type_wchar_t_ptr; break;

					default:
						warn_invalid_length_modifier(pos, fmt_mod, *fmt);
						break;
				}
				allowed_flags = FMT_FLAG_NONE;
				break;

			case 'p':
				if (fmt_mod != FMT_MOD_NONE) {
					warn_invalid_length_modifier(pos, fmt_mod, *fmt);
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
						warn_invalid_length_modifier(pos, fmt_mod, *fmt);
						break;
				}
				allowed_flags = FMT_FLAG_NONE;
				break;

			default:
				warningf(pos, "encountered unknown conversion specifier '%%%C'", (wint_t)*fmt);
				arg = arg->next;
				continue;
		}

		if ((fmt_flags & ~allowed_flags) != 0) {
			/* TODO better warning message text */
			warningf(pos, "invalid format flags in conversion specification");
		}

		if (arg == NULL) {
			warningf(pos, "too few arguments for format string");
			return;
		}

		const type_t *const arg_type = arg->expression->base.datatype;
		if (arg_type != expected_type) {
			warningf(pos, "argument type '%T' does not match conversion specifier '%%%c'\n", arg_type, (char)*fmt);
		}

		arg = arg->next;
	}
	if (fmt + 1 != wstring->begin + wstring->size) {
		warningf(pos, "format string contains NUL");
	}
	if (arg != NULL) {
		warningf(pos, "too many arguments for format string");
	}
}

void check_format(const call_expression_t *const call)
{
	const expression_t *const func_expr = call->function;
	if (func_expr->kind != EXPR_REFERENCE)
		return;

	const char            *const name = func_expr->reference.symbol->string;
	const call_argument_t *      arg  = call->arguments;
	if (strcmp(name, "wprintf") == 0) { /* TODO gammlig */
		check_format_arguments(arg, arg->next);
	} else if (strcmp(name, "swprintf") == 0) {
		arg = arg->next->next; /* skip destination buffer and size */
		check_format_arguments(arg, arg->next);
	}
}
