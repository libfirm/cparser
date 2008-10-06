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
#include <ctype.h>
#include <wctype.h>

#include "format_check.h"
#include "symbol_t.h"
#include "ast_t.h"
#include "diagnostic.h"
#include "types.h"
#include "type_t.h"
#include "warning.h"
#include "lang_features.h"

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

static const char* get_length_modifier_name(const format_length_modifier_t mod)
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
		[FMT_MOD_q]    = "q",
		/* only in microsoft mode */
		[FMT_MOD_w]    = "w",
		[FMT_MOD_I]    = "I",
		[FMT_MOD_I32]  = "I32",
		[FMT_MOD_I64]  = "I64"
	};
	assert(mod < sizeof(names) / sizeof(*names));
	return names[mod];
}

static void warn_invalid_length_modifier(const source_position_t *pos,
                                         const format_length_modifier_t mod,
                                         const wchar_rep_t conversion)
{
	warningf(pos,
		"invalid length modifier '%s' for conversion specifier '%%%c'",
		get_length_modifier_name(mod), conversion
	);
}

typedef struct vchar_t vchar_t;
struct vchar_t {
	const void *string;   /**< the string */
	size_t     position;  /**< current position */
	size_t     size;      /**< size of the string */

	/** return the first character of the string and setthe position to 0. */
	unsigned (*first)(vchar_t *self);
	/** return the next character of the string */
	unsigned (*next)(vchar_t *self);
	/** return non_zero if the given character is a digit */
	int (*is_digit)(unsigned vchar);
};

static unsigned string_first(vchar_t *self) {
	self->position = 0;
	const string_t *string = self->string;
	return string->begin[0];
}

static unsigned string_next(vchar_t *self) {
	++self->position;
	const string_t *string = self->string;
	return string->begin[self->position];
}

static int string_isdigit(unsigned vchar) {
	return isdigit(vchar);
}

static unsigned wstring_first(vchar_t *self) {
	self->position = 0;
	const wide_string_t *wstring = self->string;
	return wstring->begin[0];
}

static unsigned wstring_next(vchar_t *self) {
	++self->position;
	const wide_string_t *wstring = self->string;
	return wstring->begin[self->position];
}

static int wstring_isdigit(unsigned vchar) {
	return iswdigit(vchar);
}

static bool atend(vchar_t *self) {
	return self->position + 1 == self->size;
}

/**
 * Check printf-style format.
 */
static void check_format_arguments(const call_argument_t *arg, unsigned idx_fmt,
		unsigned idx_param)
{
	const call_argument_t *fmt_arg;
	unsigned idx = 0;
	unsigned num_fmt = 0;

	/* find format arg */
	for (idx = 0; idx < idx_fmt; ++idx)
		arg = arg->next;
	fmt_arg = arg;

	const expression_t *fmt_expr = fmt_arg->expression;
	if (fmt_expr->kind == EXPR_UNARY_CAST_IMPLICIT) {
		fmt_expr = fmt_expr->unary.value;
	}

	vchar_t vchar;
	if (fmt_expr->kind == EXPR_WIDE_STRING_LITERAL) {
		vchar.string   = &fmt_expr->wide_string.value;
		vchar.size     = fmt_expr->wide_string.value.size;
		vchar.first    = wstring_first;
		vchar.next     = wstring_next;
		vchar.is_digit = wstring_isdigit;
	} else if (fmt_expr->kind == EXPR_STRING_LITERAL) {
		vchar.string   = &fmt_expr->string.value;
		vchar.size     = fmt_expr->string.value.size;
		vchar.first    = string_first;
		vchar.next     = string_next;
		vchar.is_digit = string_isdigit;
	} else {
		return;
	}
	/* find the real args */
	for(; idx < idx_param; ++idx)
		arg = arg->next;

	const source_position_t *pos = &fmt_expr->base.source_position;
	unsigned fmt = vchar.first(&vchar);
	for (; fmt != '\0'; fmt = vchar.next(&vchar)) {
		if (fmt != '%')
			continue;
		fmt = vchar.next(&vchar);

		if (fmt == '%')
			continue;

		++num_fmt;

		format_flags_t fmt_flags = FMT_FLAG_NONE;
		if (fmt == '0') {
			fmt = vchar.next(&vchar);
			fmt_flags |= FMT_FLAG_ZERO;
		}

		/* argument selector or minimum field width */
		if (vchar.is_digit(fmt)) {
			do {
				fmt = vchar.next(&vchar);
			} while (vchar.is_digit(fmt));

			/* digit string was ... */
			if (fmt == '$') {
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
				switch (fmt) {
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
					warningf(pos, "repeated flag '%c' in conversion specification", (char)fmt);
				}
				fmt_flags |= flag;
				fmt = vchar.next(&vchar);
			}
break_fmt_flags:

			/* minimum field width */
			if (fmt == '*') {
				fmt = vchar.next(&vchar);
				if (arg == NULL) {
					warningf(pos, "missing argument for '*' field width in conversion specification");
					return;
				}
				const type_t *const arg_type = arg->expression->base.type;
				if (arg_type != type_int) {
					warningf(pos, "argument for '*' field width in conversion specification is not an 'int', but an '%T'", arg_type);
				}
				arg = arg->next;
			} else {
				while (vchar.is_digit(fmt)) {
					fmt = vchar.next(&vchar);
				}
			}
		}

		/* precision */
		if (fmt == '.') {
			fmt = vchar.next(&vchar);
			if (fmt == '*') {
				fmt = vchar.next(&vchar);
				if (arg == NULL) {
					warningf(pos, "missing argument for '*' precision in conversion specification");
					return;
				}
				const type_t *const arg_type = arg->expression->base.type;
				if (arg_type != type_int) {
					warningf(pos, "argument for '*' precision in conversion specification is not an 'int', but an '%T'", arg_type);
				}
				arg = arg->next;
			} else {
				/* digit string may be omitted */
				while (vchar.is_digit(fmt)) {
					fmt = vchar.next(&vchar);
				}
			}
		}

		/* length modifier */
		format_length_modifier_t fmt_mod;
		switch (fmt) {
			case 'h':
				fmt = vchar.next(&vchar);
				if (fmt == 'h') {
					fmt = vchar.next(&vchar);
					fmt_mod = FMT_MOD_hh;
				} else {
					fmt_mod = FMT_MOD_h;
				}
				break;

			case 'l':
				fmt = vchar.next(&vchar);
				if (fmt == 'l') {
					fmt = vchar.next(&vchar);
					fmt_mod = FMT_MOD_ll;
				} else {
					fmt_mod = FMT_MOD_l;
				}
				break;

			case 'L': fmt = vchar.next(&vchar); fmt_mod = FMT_MOD_L;    break;
			case 'j': fmt = vchar.next(&vchar); fmt_mod = FMT_MOD_j;    break;
			case 't': fmt = vchar.next(&vchar); fmt_mod = FMT_MOD_t;    break;
			case 'z': fmt = vchar.next(&vchar); fmt_mod = FMT_MOD_z;    break;
			case 'q': fmt = vchar.next(&vchar); fmt_mod = FMT_MOD_q;    break;
			/* microsoft mode */
			case 'w':
				if (c_mode & _MS) {
					fmt = vchar.next(&vchar); fmt_mod = FMT_MOD_w;
				} else {
					fmt_mod = FMT_MOD_NONE;
				}
				break;
			case 'I':
				if (c_mode & _MS) {
					fmt = vchar.next(&vchar); fmt_mod = FMT_MOD_I;
					if (fmt == '3') {
						fmt = vchar.next(&vchar);
						if (fmt == '2') {
							fmt = vchar.next(&vchar);
							fmt_mod = FMT_MOD_I32;
						} else {
							/* rewind */
							--vchar.position;
						}
					} else if (fmt == '6') {
						fmt = vchar.next(&vchar);
						if (fmt == '4') {
							fmt = vchar.next(&vchar);
							fmt_mod = FMT_MOD_I64;
						} else {
							/* rewind */
							--vchar.position;
						}
					}
				} else {
					fmt_mod = FMT_MOD_NONE;
				}
				break;
			default:
				fmt_mod = FMT_MOD_NONE;
				break;
		}

		if (fmt == '\0') {
			warningf(pos, "dangling %% in format string");
			break;
		}

		type_t            *expected_type;
		type_qualifiers_t  expected_qual = TYPE_QUALIFIER_NONE;
		format_flags_t     allowed_flags;
		switch (fmt) {
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
					case FMT_MOD_I:    expected_type = type_ptrdiff_t; break;
					case FMT_MOD_I32:  expected_type = type_int32;     break;
					case FMT_MOD_I64:  expected_type = type_int64;     break;

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
				expected_type = type_wchar_t_ptr;
				expected_qual = TYPE_QUALIFIER_CONST;
				allowed_flags = FMT_FLAG_MINUS;
				break;

			case 's':
				switch (fmt_mod) {
					case FMT_MOD_NONE: expected_type = type_char_ptr;    break;
					case FMT_MOD_l:    expected_type = type_wchar_t_ptr; break;
					case FMT_MOD_w:    expected_type = type_wchar_t_ptr; break;

					default:
						warn_invalid_length_modifier(pos, fmt_mod, fmt);
						goto next_arg;
				}
				expected_qual = TYPE_QUALIFIER_CONST;
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
				warningf(pos, "encountered unknown conversion specifier '%%%C'", (wint_t)fmt);
				goto next_arg;
		}

		format_flags_t wrong_flags = fmt_flags & ~allowed_flags;
		if (wrong_flags != 0) {
			char wrong[8];
			int idx = 0;
			if (wrong_flags & FMT_FLAG_HASH)  wrong[idx++] = '#';
			if (wrong_flags & FMT_FLAG_ZERO)  wrong[idx++] = '0';
			if (wrong_flags & FMT_FLAG_MINUS) wrong[idx++] = '-';
			if (wrong_flags & FMT_FLAG_SPACE) wrong[idx++] = ' ';
			if (wrong_flags & FMT_FLAG_PLUS)  wrong[idx++] = '+';
			if (wrong_flags & FMT_FLAG_TICK)  wrong[idx++] = '\'';
			wrong[idx] = '\0';

			warningf(pos, "invalid format flags \"%s\" in conversion specification %%%c", wrong, fmt);
		}

		if (arg == NULL) {
			warningf(pos, "too few arguments for format string");
			return;
		}

		{	/* create a scope here to prevent warning about the jump to next_arg */
			type_t *const arg_type           = arg->expression->base.type;
			type_t *const arg_skip           = skip_typeref(arg_type);
			type_t *const expected_type_skip = skip_typeref(expected_type);
			if (is_type_pointer(expected_type_skip)) {
				if (is_type_pointer(arg_skip)) {
					type_t *const exp_to = skip_typeref(expected_type_skip->pointer.points_to);
					type_t *const arg_to = skip_typeref(arg_skip->pointer.points_to);
					if ((arg_to->base.qualifiers & ~expected_qual) == 0 &&
						get_unqualified_type(arg_to) == exp_to) {
						goto next_arg;
					}
				}
			} else {
				if (get_unqualified_type(arg_skip) == expected_type_skip) {
					goto next_arg;
				}
			}
			if (is_type_valid(arg_skip)) {
				warningf(pos,
					"argument type '%T' does not match conversion specifier '%%%s%c'",
					arg_type, get_length_modifier_name(fmt_mod), (char)fmt);
			}
		}
next_arg:
		arg = arg->next;
	}
	if (!atend(&vchar)) {
		warningf(pos, "format string contains NUL");
	}
	if (arg != NULL) {
		unsigned num_args = num_fmt;
		while (arg != NULL) {
			++num_args;
			arg = arg->next;
		}
		warningf(pos, "%u arguments but only %u format string(s)", num_args, num_fmt);
	}
}

static const struct {
	const char    *name;
	format_kind_t  fmt_kind;
	unsigned       fmt_idx;
	unsigned       arg_idx;
} builtin_table[] = {
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
	if (!warning.format)
		return;

	const expression_t *const func_expr = call->function;
	if (func_expr->kind != EXPR_REFERENCE)
		return;

	const declaration_t   *const decl = func_expr->reference.declaration;
	const call_argument_t *      arg  = call->arguments;

	if(false) {
		/* the declaration has a GNU format attribute, check it */
	} else {
		/*
		 * For some functions we always check the format, even if it was not specified.
		 * This allows to check format even in MS mode or without header included.
		 */
		const char            *const name = decl->symbol->string;
		for(size_t i = 0; i < sizeof(builtin_table) / sizeof(builtin_table[0]); ++i) {
			if(strcmp(name, builtin_table[i].name) == 0) {
				if(builtin_table[i].fmt_kind == FORMAT_PRINTF) {
					check_format_arguments(arg,
					                       builtin_table[i].fmt_idx,
					                       builtin_table[i].arg_idx);
				}
				break;
			}
		}
	}
}
