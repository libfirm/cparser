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
#include <config.h>

#include "adt/strutil.h"
#include "input.h"
#include "diagnostic.h"
#include "lexer.h"
#include "symbol_t.h"
#include "token_t.h"
#include "symbol_table_t.h"
#include "adt/error.h"
#include "adt/strset.h"
#include "adt/util.h"
#include "types.h"
#include "type_t.h"
#include "parser.h"
#include "warning.h"
#include "lang_features.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#ifndef _WIN32
#include <strings.h>
#endif

#define MAX_PUTBACK 16    // 3 would be enough, but 16 gives a nicer alignment
#define BUF_SIZE    1024

static input_t           *input;
static utf32              input_buf[BUF_SIZE + MAX_PUTBACK];
static const utf32       *bufpos;
static const utf32       *bufend;
static utf32              c;
static source_position_t  lexer_pos;
token_t                   lexer_token;
static symbol_t          *symbol_L;
static strset_t           stringset;
bool                      allow_dollar_in_symbol = true;

/**
 * Prints a parse error message at the current token.
 *
 * @param msg   the error message
 */
static void parse_error(const char *msg)
{
	errorf(&lexer_pos, "%s", msg);
}

/**
 * Prints an internal error message at the current token.
 *
 * @param msg   the error message
 */
static NORETURN internal_error(const char *msg)
{
	internal_errorf(&lexer_pos, "%s", msg);
}

static inline void next_real_char(void)
{
	assert(bufpos <= bufend);
	if (bufpos >= bufend) {
		size_t n = decode(input, input_buf+MAX_PUTBACK, BUF_SIZE);
		if (n == 0) {
			c = EOF;
			return;
		}
		bufpos = input_buf + MAX_PUTBACK;
		bufend = bufpos + n;
	}
	c = *bufpos++;
	++lexer_pos.colno;
}

/**
 * Put a character back into the buffer.
 *
 * @param pc  the character to put back
 */
static inline void put_back(utf32 const pc)
{
	*(--bufpos - input_buf + input_buf) = pc;
	--lexer_pos.colno;
}

static inline void next_char(void);

#define NEWLINE  \
	'\r': \
		next_char(); \
		if (c == '\n') { \
	case '\n': \
			next_char(); \
		} \
		lexer_pos.lineno++; \
		lexer_pos.colno = 1; \
		goto newline; \
		newline // Let it look like an ordinary case label.

#define eat(c_type) (assert(c == c_type), next_char())

static void maybe_concat_lines(void)
{
	eat('\\');

	switch (c) {
	case NEWLINE:
		return;

	default:
		break;
	}

	put_back(c);
	c = '\\';
}

/**
 * Set c to the next input character, ie.
 * after expanding trigraphs.
 */
static inline void next_char(void)
{
	next_real_char();

	/* filter trigraphs */
	if (UNLIKELY(c == '\\')) {
		maybe_concat_lines();
		return;
	}

	if (LIKELY(c != '?'))
		return;

	next_real_char();
	if (LIKELY(c != '?')) {
		put_back(c);
		c = '?';
		return;
	}

	next_real_char();
	switch (c) {
	case '=': c = '#'; break;
	case '(': c = '['; break;
	case '/': c = '\\'; maybe_concat_lines(); break;
	case ')': c = ']'; break;
	case '\'': c = '^'; break;
	case '<': c = '{'; break;
	case '!': c = '|'; break;
	case '>': c = '}'; break;
	case '-': c = '~'; break;
	default:
		put_back(c);
		put_back('?');
		c = '?';
		break;
	}
}

#define SYMBOL_CHARS  \
	case '$': if (!allow_dollar_in_symbol) goto dollar_sign; \
	case 'a':         \
	case 'b':         \
	case 'c':         \
	case 'd':         \
	case 'e':         \
	case 'f':         \
	case 'g':         \
	case 'h':         \
	case 'i':         \
	case 'j':         \
	case 'k':         \
	case 'l':         \
	case 'm':         \
	case 'n':         \
	case 'o':         \
	case 'p':         \
	case 'q':         \
	case 'r':         \
	case 's':         \
	case 't':         \
	case 'u':         \
	case 'v':         \
	case 'w':         \
	case 'x':         \
	case 'y':         \
	case 'z':         \
	case 'A':         \
	case 'B':         \
	case 'C':         \
	case 'D':         \
	case 'E':         \
	case 'F':         \
	case 'G':         \
	case 'H':         \
	case 'I':         \
	case 'J':         \
	case 'K':         \
	case 'L':         \
	case 'M':         \
	case 'N':         \
	case 'O':         \
	case 'P':         \
	case 'Q':         \
	case 'R':         \
	case 'S':         \
	case 'T':         \
	case 'U':         \
	case 'V':         \
	case 'W':         \
	case 'X':         \
	case 'Y':         \
	case 'Z':         \
	case '_':

#define DIGITS        \
	case '0':         \
	case '1':         \
	case '2':         \
	case '3':         \
	case '4':         \
	case '5':         \
	case '6':         \
	case '7':         \
	case '8':         \
	case '9':

static bool is_universal_char_valid(utf32 const v)
{
	/* C11 §6.4.3:2 */
	if (v < 0xA0U && v != 0x24 && v != 0x40 && v != 0x60)
		return false;
	if (0xD800 <= v && v <= 0xDFFF)
		return false;
	return true;
}

static int digit_value(utf32 digit);

static utf32 parse_universal_char(unsigned const n_digits)
{
	utf32 v = 0;
	for (unsigned k = n_digits; k != 0; --k) {
		if (isxdigit(c)) {
			v = 16 * v + digit_value(c);
			next_char();
		} else {
			errorf(&lexer_pos, "short universal character name, expected %u more digits", k);
			break;
		}
	}
	if (!is_universal_char_valid(v)) {
		errorf(&lexer_pos, "\\%c%0*X is not a valid universal character name", n_digits == 4 ? 'u' : 'U', (int)n_digits, v);
	}
	return v;
}

static bool is_universal_char_valid_identifier(utf32 const v)
{
	/* C11 Annex D.1 */
	if (                v == 0x000A8) return true;
	if (                v == 0x000AA) return true;
	if (                v == 0x000AD) return true;
	if (                v == 0x000AF) return true;
	if (0x000B2 <= v && v <= 0x000B5) return true;
	if (0x000B7 <= v && v <= 0x000BA) return true;
	if (0x000BC <= v && v <= 0x000BE) return true;
	if (0x000C0 <= v && v <= 0x000D6) return true;
	if (0x000D8 <= v && v <= 0x000F6) return true;
	if (0x000F8 <= v && v <= 0x000FF) return true;
	if (0x00100 <= v && v <= 0x0167F) return true;
	if (0x01681 <= v && v <= 0x0180D) return true;
	if (0x0180F <= v && v <= 0x01FFF) return true;
	if (0x0200B <= v && v <= 0x0200D) return true;
	if (0x0202A <= v && v <= 0x0202E) return true;
	if (0x0203F <= v && v <= 0x02040) return true;
	if (                v == 0x02054) return true;
	if (0x02060 <= v && v <= 0x0206F) return true;
	if (0x02070 <= v && v <= 0x0218F) return true;
	if (0x02460 <= v && v <= 0x024FF) return true;
	if (0x02776 <= v && v <= 0x02793) return true;
	if (0x02C00 <= v && v <= 0x02DFF) return true;
	if (0x02E80 <= v && v <= 0x02FFF) return true;
	if (0x03004 <= v && v <= 0x03007) return true;
	if (0x03021 <= v && v <= 0x0302F) return true;
	if (0x03031 <= v && v <= 0x0303F) return true;
	if (0x03040 <= v && v <= 0x0D7FF) return true;
	if (0x0F900 <= v && v <= 0x0FD3D) return true;
	if (0x0FD40 <= v && v <= 0x0FDCF) return true;
	if (0x0FDF0 <= v && v <= 0x0FE44) return true;
	if (0x0FE47 <= v && v <= 0x0FFFD) return true;
	if (0x10000 <= v && v <= 0x1FFFD) return true;
	if (0x20000 <= v && v <= 0x2FFFD) return true;
	if (0x30000 <= v && v <= 0x3FFFD) return true;
	if (0x40000 <= v && v <= 0x4FFFD) return true;
	if (0x50000 <= v && v <= 0x5FFFD) return true;
	if (0x60000 <= v && v <= 0x6FFFD) return true;
	if (0x70000 <= v && v <= 0x7FFFD) return true;
	if (0x80000 <= v && v <= 0x8FFFD) return true;
	if (0x90000 <= v && v <= 0x9FFFD) return true;
	if (0xA0000 <= v && v <= 0xAFFFD) return true;
	if (0xB0000 <= v && v <= 0xBFFFD) return true;
	if (0xC0000 <= v && v <= 0xCFFFD) return true;
	if (0xD0000 <= v && v <= 0xDFFFD) return true;
	if (0xE0000 <= v && v <= 0xEFFFD) return true;
	return false;
}

static bool is_universal_char_valid_identifier_start(utf32 const v)
{
	/* C11 Annex D.2 */
	if (0x0300 <= v && v <= 0x036F) return false;
	if (0x1DC0 <= v && v <= 0x1DFF) return false;
	if (0x20D0 <= v && v <= 0x20FF) return false;
	if (0xFE20 <= v && v <= 0xFE2F) return false;
	return true;
}

/**
 * Read a symbol from the input and build
 * the lexer_token.
 */
static void parse_symbol(void)
{
	while (true) {
		switch (c) {
		DIGITS
		SYMBOL_CHARS
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
			break;

		case '\\':
			next_char();
			switch (c) {
			{
				unsigned n;
			case 'U': n = 8; goto universal;
			case 'u': n = 4; goto universal;
universal:
				next_char();
				utf32 const v = parse_universal_char(n);
				if (!is_universal_char_valid_identifier(v)) {
					if (is_universal_char_valid(v)) {
						errorf(&lexer_pos, "universal character \\%c%0*X is not valid in an identifier", n == 4 ? 'u' : 'U', (int)n, v);
					}
				} else if (obstack_object_size(&symbol_obstack) == 0 && !is_universal_char_valid_identifier_start(v)) {
					errorf(&lexer_pos, "universal character \\%c%0*X is not valid as start of an identifier", n == 4 ? 'u' : 'U', (int)n, v);
				} else {
					obstack_grow_symbol(&symbol_obstack, v);
				}
				break;
			}

			default:
				put_back(c);
				c = '\\';
				goto end_symbol;
			}

		default:
dollar_sign:
			goto end_symbol;
		}
	}

end_symbol:
	obstack_1grow(&symbol_obstack, '\0');

	char     *string = obstack_finish(&symbol_obstack);
	symbol_t *symbol = symbol_table_insert(string);

	lexer_token.kind        = symbol->ID;
	lexer_token.base.symbol = symbol;

	if (symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}
}

static string_t sym_make_string(void)
{
	obstack_1grow(&symbol_obstack, '\0');
	size_t const len    = obstack_object_size(&symbol_obstack) - 1;
	char  *const string = obstack_finish(&symbol_obstack);

	/* TODO hash */
#if 0
	const char *result = strset_insert(&stringset, concat);
	if (result != concat) {
		obstack_free(&symbol_obstack, concat);
	}
#else
	const char *result = string;
#endif
	return (string_t) {result, len};
}

/**
 * parse suffixes like 'LU' or 'f' after numbers
 */
static void parse_number_suffix(void)
{
	assert(obstack_object_size(&symbol_obstack) == 0);
	while (true) {
		switch (c) {
		SYMBOL_CHARS
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
			break;
		default:
		dollar_sign:
			goto finish_suffix;
		}
	}
finish_suffix:
	if (obstack_object_size(&symbol_obstack) == 0) {
		lexer_token.number.suffix.begin = NULL;
		lexer_token.number.suffix.size  = 0;
		return;
	}

	lexer_token.number.suffix = sym_make_string();
}

static void parse_exponent(void)
{
	if (c == '-' || c == '+') {
		obstack_1grow(&symbol_obstack, (char)c);
		next_char();
	}

	if (isdigit(c)) {
		do {
			obstack_1grow(&symbol_obstack, (char)c);
			next_char();
		} while (isdigit(c));
	} else {
		errorf(&lexer_token.base.source_position, "exponent has no digits");
	}
}

/**
 * Parses a hex number including hex floats and set the
 * lexer_token.
 */
static void parse_number_hex(void)
{
	bool is_float   = false;
	bool has_digits = false;

	while (isxdigit(c)) {
		has_digits = true;
		obstack_1grow(&symbol_obstack, (char) c);
		next_char();
	}

	if (c == '.') {
		is_float = true;
		obstack_1grow(&symbol_obstack, (char) c);
		next_char();

		while (isxdigit(c)) {
			has_digits = true;
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}
	}
	if (c == 'p' || c == 'P') {
		is_float = true;
		obstack_1grow(&symbol_obstack, (char) c);
		next_char();
		parse_exponent();
	} else if (is_float) {
		errorf(&lexer_token.base.source_position,
		       "hexadecimal floatingpoint constant requires an exponent");
	}

	lexer_token.number.number = sym_make_string();

	lexer_token.kind = is_float ? T_FLOATINGPOINT : T_INTEGER;

	if (!has_digits) {
		errorf(&lexer_token.base.source_position, "invalid number literal '%S'", &lexer_token.number.number);
		lexer_token.number.number.begin = "0";
		lexer_token.number.number.size  = 1;
	}

	parse_number_suffix();
}

static void parse_number_bin(void)
{
	bool has_digits = false;

	while (c == '0' || c == '1') {
		has_digits = true;
		obstack_1grow(&symbol_obstack, (char)c);
		next_char();
	}

	lexer_token.number.number = sym_make_string();
	lexer_token.kind          = T_INTEGER;

	if (!has_digits) {
		errorf(&lexer_token.base.source_position, "invalid number literal '%S'", &lexer_token.number.number);
		lexer_token.number.number.begin = "0";
		lexer_token.number.number.size  = 1;
	}

	parse_number_suffix();
}

/**
 * Returns true if the given char is a octal digit.
 *
 * @param char  the character to check
 */
static bool is_octal_digit(utf32 chr)
{
	return '0' <= chr && chr <= '7';
}

/**
 * Parses a number and sets the lexer_token.
 */
static void parse_number(void)
{
	bool is_float   = false;
	bool has_digits = false;

	assert(obstack_object_size(&symbol_obstack) == 0);
	if (c == '0') {
		obstack_1grow(&symbol_obstack, (char)c);
		next_char();
		if (c == 'x' || c == 'X') {
			obstack_1grow(&symbol_obstack, (char)c);
			next_char();
			parse_number_hex();
			return;
		} else if (c == 'b' || c == 'B') {
			/* GCC extension: binary constant 0x[bB][01]+.  */
			obstack_1grow(&symbol_obstack, (char)c);
			next_char();
			parse_number_bin();
			return;
		}
		has_digits = true;
	}

	while (isdigit(c)) {
		has_digits = true;
		obstack_1grow(&symbol_obstack, (char) c);
		next_char();
	}

	if (c == '.') {
		is_float = true;
		obstack_1grow(&symbol_obstack, '.');
		next_char();

		while (isdigit(c)) {
			has_digits = true;
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}
	}
	if (c == 'e' || c == 'E') {
		is_float = true;
		obstack_1grow(&symbol_obstack, 'e');
		next_char();
		parse_exponent();
	}

	lexer_token.number.number = sym_make_string();

	if (is_float) {
		lexer_token.kind = T_FLOATINGPOINT;
	} else {
		lexer_token.kind = T_INTEGER;

		if (lexer_token.number.number.begin[0] == '0') {
			/* check for invalid octal digits */
			for (size_t i= 0; i < lexer_token.number.number.size; ++i) {
				char t = lexer_token.number.number.begin[i];
				if (t >= '8')
					errorf(&lexer_token.base.source_position, "invalid digit '%c' in octal number", t);
			}
		}
	}

	if (!has_digits) {
		errorf(&lexer_token.base.source_position, "invalid number literal '%S'",
		       &lexer_token.number.number);
	}

	parse_number_suffix();
}

/**
 * Returns the value of a digit.
 * The only portable way to do it ...
 */
static int digit_value(utf32 const digit)
{
	switch (digit) {
	case '0': return 0;
	case '1': return 1;
	case '2': return 2;
	case '3': return 3;
	case '4': return 4;
	case '5': return 5;
	case '6': return 6;
	case '7': return 7;
	case '8': return 8;
	case '9': return 9;
	case 'a':
	case 'A': return 10;
	case 'b':
	case 'B': return 11;
	case 'c':
	case 'C': return 12;
	case 'd':
	case 'D': return 13;
	case 'e':
	case 'E': return 14;
	case 'f':
	case 'F': return 15;
	default:
		internal_error("wrong character given");
	}
}

/**
 * Parses an octal character sequence.
 *
 * @param first_digit  the already read first digit
 */
static utf32 parse_octal_sequence(utf32 const first_digit)
{
	assert(is_octal_digit(first_digit));
	utf32 value = digit_value(first_digit);
	if (!is_octal_digit(c)) return value;
	value = 8 * value + digit_value(c);
	next_char();
	if (!is_octal_digit(c)) return value;
	value = 8 * value + digit_value(c);
	next_char();
	return value;
}

/**
 * Parses a hex character sequence.
 */
static utf32 parse_hex_sequence(void)
{
	utf32 value = 0;
	while (isxdigit(c)) {
		value = 16 * value + digit_value(c);
		next_char();
	}
	return value;
}

/**
 * Parse an escape sequence.
 */
static utf32 parse_escape_sequence(void)
{
	eat('\\');

	utf32 const ec = c;
	next_char();

	switch (ec) {
	case '"':  return '"';
	case '\'': return '\'';
	case '\\': return '\\';
	case '?': return '\?';
	case 'a': return '\a';
	case 'b': return '\b';
	case 'f': return '\f';
	case 'n': return '\n';
	case 'r': return '\r';
	case 't': return '\t';
	case 'v': return '\v';
	case 'x':
		return parse_hex_sequence();
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
		return parse_octal_sequence(ec);
	case EOF:
		parse_error("reached end of file while parsing escape sequence");
		return EOF;
	/* \E is not documented, but handled, by GCC.  It is acceptable according
	 * to §6.11.4, whereas \e is not. */
	case 'E':
	case 'e':
		if (c_mode & _GNUC)
			return 27;   /* hopefully 27 is ALWAYS the code for ESCAPE */
		break;

	case 'U': return parse_universal_char(8);
	case 'u': return parse_universal_char(4);

	default:
		break;
	}
	/* §6.4.4.4:8 footnote 64 */
	parse_error("unknown escape sequence");
	return EOF;
}

string_t make_string(const char *string)
{
	obstack_grow(&symbol_obstack, string, strlen(string));
	return sym_make_string();
}

static void parse_string(utf32 const delim, token_kind_t const kind, string_encoding_t const enc, char const *const context)
{
	eat(delim);

	while (true) {
		switch (c) {
		case '\\': {
			utf32 const tc = parse_escape_sequence();
			if (enc == STRING_ENCODING_CHAR) {
				if (tc >= 0x100) {
					warningf(WARN_OTHER, &lexer_pos, "escape sequence out of range");
				}
				obstack_1grow(&symbol_obstack, tc);
			} else {
				obstack_grow_symbol(&symbol_obstack, tc);
			}
			break;
		}

		case NEWLINE:
			errorf(&lexer_pos, "newline while parsing %s", context);
			break;

		case EOF:
			errorf(&lexer_token.base.source_position, "EOF while parsing %s", context);
			goto end_of_string;

		default:
			if (c == delim) {
				next_char();
				goto end_of_string;
			} else {
				obstack_grow_symbol(&symbol_obstack, c);
				next_char();
				break;
			}
		}
	}

end_of_string:
	lexer_token.kind            = kind;
	lexer_token.string.encoding = enc;
	lexer_token.string.string   = sym_make_string();
}

/**
 * Parse a string literal and set lexer_token.
 */
static void parse_string_literal(string_encoding_t const enc)
{
	parse_string('"', T_STRING_LITERAL, enc, "string literal");
}

/**
 * Parse a character constant and set lexer_token.
 */
static void parse_character_constant(string_encoding_t const enc)
{
	parse_string('\'', T_CHARACTER_CONSTANT, enc, "character constant");
	if (lexer_token.string.string.size == 0) {
		errorf(&lexer_token.base.source_position, "empty character constant");
	}
}

/**
 * Skip a multiline comment.
 */
static void skip_multiline_comment(void)
{
	while (true) {
		switch (c) {
		case '/':
			next_char();
			if (c == '*') {
				/* nested comment, warn here */
				warningf(WARN_COMMENT, &lexer_pos, "'/*' within comment");
			}
			break;
		case '*':
			next_char();
			if (c == '/') {
				next_char();
				return;
			}
			break;

		case NEWLINE:
			break;

		case EOF: {
			errorf(&lexer_token.base.source_position,
			       "at end of file while looking for comment end");
			return;
		}

		default:
			next_char();
			break;
		}
	}
}

/**
 * Skip a single line comment.
 */
static void skip_line_comment(void)
{
	while (true) {
		switch (c) {
		case EOF:
			return;

		case '\n':
		case '\r':
			return;

		case '\\':
			next_char();
			if (c == '\n' || c == '\r') {
				warningf(WARN_COMMENT, &lexer_pos, "multi-line comment");
				return;
			}
			break;

		default:
			next_char();
			break;
		}
	}
}

/** The current preprocessor token. */
static token_t pp_token;

/**
 * Read the next preprocessor token.
 */
static inline void next_pp_token(void)
{
	lexer_next_preprocessing_token();
	pp_token = lexer_token;
}

/**
 * Eat all preprocessor tokens until newline.
 */
static void eat_until_newline(void)
{
	while (pp_token.kind != '\n' && pp_token.kind != T_EOF) {
		next_pp_token();
	}
}

/**
 * Parse the line directive.
 */
static void parse_line_directive(void)
{
	if (pp_token.kind != T_INTEGER) {
		parse_error("expected integer");
	} else {
		/* use offset -1 as this is about the next line */
		lexer_pos.lineno = atoi(pp_token.number.number.begin) - 1;
		next_pp_token();
	}
	if (pp_token.kind == T_STRING_LITERAL && pp_token.string.encoding == STRING_ENCODING_CHAR) {
		lexer_pos.input_name = pp_token.string.string.begin;
		lexer_pos.is_system_header = false;
		next_pp_token();

		/* attempt to parse numeric flags as outputted by gcc preprocessor */
		while (pp_token.kind == T_INTEGER) {
			/* flags:
			 * 1 - indicates start of a new file
			 * 2 - indicates return from a file
			 * 3 - indicates system header
			 * 4 - indicates implicit extern "C" in C++ mode
			 *
			 * currently we're only interested in "3"
			 */
			if (streq(pp_token.number.number.begin, "3")) {
				lexer_pos.is_system_header = true;
			}
			next_pp_token();
		}
	}

	eat_until_newline();
}

/**
 * STDC pragmas.
 */
typedef enum stdc_pragma_kind_t {
	STDC_UNKNOWN,
	STDC_FP_CONTRACT,
	STDC_FENV_ACCESS,
	STDC_CX_LIMITED_RANGE
} stdc_pragma_kind_t;

/**
 * STDC pragma values.
 */
typedef enum stdc_pragma_value_kind_t {
	STDC_VALUE_UNKNOWN,
	STDC_VALUE_ON,
	STDC_VALUE_OFF,
	STDC_VALUE_DEFAULT
} stdc_pragma_value_kind_t;

/**
 * Parse a pragma directive.
 */
static void parse_pragma(void)
{
	next_pp_token();
	if (pp_token.kind != T_IDENTIFIER) {
		warningf(WARN_UNKNOWN_PRAGMAS, &pp_token.base.source_position,
		         "expected identifier after #pragma");
		eat_until_newline();
		return;
	}

	stdc_pragma_kind_t kind = STDC_UNKNOWN;
	if (pp_token.base.symbol->pp_ID == TP_STDC && c_mode & _C99) {
		/* a STDC pragma */
		next_pp_token();

		switch (pp_token.base.symbol->pp_ID) {
		case TP_FP_CONTRACT:      kind = STDC_FP_CONTRACT;      break;
		case TP_FENV_ACCESS:      kind = STDC_FENV_ACCESS;      break;
		case TP_CX_LIMITED_RANGE: kind = STDC_CX_LIMITED_RANGE; break;
		default:                  break;
		}
		if (kind != STDC_UNKNOWN) {
			next_pp_token();
			stdc_pragma_value_kind_t value;
			switch (pp_token.base.symbol->pp_ID) {
			case TP_ON:      value = STDC_VALUE_ON;      break;
			case TP_OFF:     value = STDC_VALUE_OFF;     break;
			case TP_DEFAULT: value = STDC_VALUE_DEFAULT; break;
			default:         value = STDC_VALUE_UNKNOWN; break;
			}
			if (value == STDC_VALUE_UNKNOWN) {
				kind = STDC_UNKNOWN;
				errorf(&pp_token.base.source_position, "bad STDC pragma argument");
			}
		}
	}
	eat_until_newline();
	if (kind == STDC_UNKNOWN) {
		warningf(WARN_UNKNOWN_PRAGMAS, &pp_token.base.source_position,
		         "encountered unknown #pragma");
	}
}

/**
 * Parse a preprocessor non-null directive.
 */
static void parse_preprocessor_identifier(void)
{
	assert(pp_token.kind == T_IDENTIFIER);
	switch (pp_token.base.symbol->pp_ID) {
	case TP_line:
		next_pp_token();
		parse_line_directive();
		break;
	case TP_pragma:
		parse_pragma();
		break;
	case TP_error:
		/* TODO; output the rest of the line */
		parse_error("#error directive");
		break;
	}
}

/**
 * Parse a preprocessor directive.
 */
static void parse_preprocessor_directive(void)
{
	next_pp_token();

	switch (pp_token.kind) {
	case T_IDENTIFIER:
		parse_preprocessor_identifier();
		break;
	case T_INTEGER:
		parse_line_directive();
		break;
	case '\n':
		/* NULL directive, see §6.10.7 */
		break;
	default:
		parse_error("invalid preprocessor directive");
		eat_until_newline();
		break;
	}
}

#define MAYBE_PROLOG                                       \
			next_char();                                   \
			while (true) {                                 \
				switch (c) {

#define MAYBE(ch, set_type)                                \
				case ch:                                   \
					next_char();                           \
					lexer_token.kind = set_type;           \
					return;

/* must use this as last thing */
#define MAYBE_MODE(ch, set_type, mode)                     \
				case ch:                                   \
					if (c_mode & mode) {                   \
						next_char();                       \
						lexer_token.kind = set_type;       \
						return;                            \
					}                                      \
					/* fallthrough */

#define ELSE_CODE(code)                                    \
				default:                                   \
					code                                   \
					return;                                \
				}                                          \
			} /* end of while (true) */                    \

#define ELSE(set_type)                                     \
		ELSE_CODE(                                         \
			lexer_token.kind = set_type;                   \
		)

void lexer_next_preprocessing_token(void)
{
	while (true) {
		lexer_token.base.source_position = lexer_pos;
		lexer_token.base.symbol          = NULL;

		switch (c) {
		case ' ':
		case '\t':
			next_char();
			break;

		case NEWLINE:
			lexer_token.kind = '\n';
			return;

		SYMBOL_CHARS {
			parse_symbol();
			/* might be a wide string ( L"string" ) */
			string_encoding_t const enc = STRING_ENCODING_WIDE;
			if (lexer_token.base.symbol == symbol_L) {
				switch (c) {
				case '"':  parse_string_literal(enc);     break;
				case '\'': parse_character_constant(enc); break;
				}
			}
			return;
		}

		DIGITS
			parse_number();
			return;

		case '"':
			parse_string_literal(STRING_ENCODING_CHAR);
			return;

		case '\'':
			parse_character_constant(STRING_ENCODING_CHAR);
			return;

		case '.':
			MAYBE_PROLOG
				DIGITS
					put_back(c);
					c = '.';
					parse_number();
					return;

				case '.':
					MAYBE_PROLOG
					MAYBE('.', T_DOTDOTDOT)
					ELSE_CODE(
						put_back(c);
						c = '.';
						lexer_token.kind = '.';
					)
			ELSE('.')
		case '&':
			MAYBE_PROLOG
			MAYBE('&', T_ANDAND)
			MAYBE('=', T_ANDEQUAL)
			ELSE('&')
		case '*':
			MAYBE_PROLOG
			MAYBE('=', T_ASTERISKEQUAL)
			ELSE('*')
		case '+':
			MAYBE_PROLOG
			MAYBE('+', T_PLUSPLUS)
			MAYBE('=', T_PLUSEQUAL)
			ELSE('+')
		case '-':
			MAYBE_PROLOG
			MAYBE('>', T_MINUSGREATER)
			MAYBE('-', T_MINUSMINUS)
			MAYBE('=', T_MINUSEQUAL)
			ELSE('-')
		case '!':
			MAYBE_PROLOG
			MAYBE('=', T_EXCLAMATIONMARKEQUAL)
			ELSE('!')
		case '/':
			MAYBE_PROLOG
			MAYBE('=', T_SLASHEQUAL)
				case '*':
					next_char();
					skip_multiline_comment();
					lexer_next_preprocessing_token();
					return;
				case '/':
					next_char();
					skip_line_comment();
					lexer_next_preprocessing_token();
					return;
			ELSE('/')
		case '%':
			MAYBE_PROLOG
			MAYBE('>', '}')
			MAYBE('=', T_PERCENTEQUAL)
				case ':':
					MAYBE_PROLOG
						case '%':
							MAYBE_PROLOG
							MAYBE(':', T_HASHHASH)
							ELSE_CODE(
								put_back(c);
								c = '%';
								lexer_token.kind = '#';
							)
					ELSE('#')
			ELSE('%')
		case '<':
			MAYBE_PROLOG
			MAYBE(':', '[')
			MAYBE('%', '{')
			MAYBE('=', T_LESSEQUAL)
				case '<':
					MAYBE_PROLOG
					MAYBE('=', T_LESSLESSEQUAL)
					ELSE(T_LESSLESS)
			ELSE('<')
		case '>':
			MAYBE_PROLOG
			MAYBE('=', T_GREATEREQUAL)
				case '>':
					MAYBE_PROLOG
					MAYBE('=', T_GREATERGREATEREQUAL)
					ELSE(T_GREATERGREATER)
			ELSE('>')
		case '^':
			MAYBE_PROLOG
			MAYBE('=', T_CARETEQUAL)
			ELSE('^')
		case '|':
			MAYBE_PROLOG
			MAYBE('=', T_PIPEEQUAL)
			MAYBE('|', T_PIPEPIPE)
			ELSE('|')
		case ':':
			MAYBE_PROLOG
			MAYBE('>', ']')
			MAYBE_MODE(':', T_COLONCOLON, _CXX)
			ELSE(':')
		case '=':
			MAYBE_PROLOG
			MAYBE('=', T_EQUALEQUAL)
			ELSE('=')
		case '#':
			MAYBE_PROLOG
			MAYBE('#', T_HASHHASH)
			ELSE('#')

		case '\\':
			next_char();
			if (c == 'U' || c == 'u') {
				put_back(c);
				c = '\\';
				parse_symbol();
			} else {
				lexer_token.kind = '\\';
			}
			return;

		case '?':
		case '[':
		case ']':
		case '(':
		case ')':
		case '{':
		case '}':
		case '~':
		case ';':
		case ',':
			lexer_token.kind = c;
			next_char();
			return;

		case EOF:
			lexer_token.kind = T_EOF;
			return;

		default:
dollar_sign:
			errorf(&lexer_pos, "unknown character '%c' found", c);
			next_char();
			break;
		}
	}
}

void lexer_next_token(void)
{
	lexer_next_preprocessing_token();

	while (lexer_token.kind == '\n') {
newline_found:
		lexer_next_preprocessing_token();
	}

	if (lexer_token.kind == '#') {
		parse_preprocessor_directive();
		goto newline_found;
	}
}

void init_lexer(void)
{
	strset_init(&stringset);
	symbol_L = symbol_table_insert("L");
}

static void input_error(unsigned delta_lines, unsigned delta_cols,
                        const char *message)
{
	lexer_pos.lineno += delta_lines;
	lexer_pos.colno  += delta_cols;
	errorf(&lexer_pos, "%s", message);
}

void lexer_switch_input(input_t *new_input, const char *input_name)
{
	lexer_pos.lineno     = 0;
	lexer_pos.colno      = 0;
	lexer_pos.input_name = input_name;

	set_input_error_callback(input_error);
	input  = new_input;
	bufpos = NULL;
	bufend = NULL;

	/* place a virtual \n at the beginning so the lexer knows that we're
	 * at the beginning of a line */
	c = '\n';
}

void exit_lexer(void)
{
	strset_destroy(&stringset);
}

static __attribute__((unused))
void dbg_pos(const source_position_t source_position)
{
	fprintf(stdout, "%s:%u:%u\n", source_position.input_name,
	        source_position.lineno, (unsigned)source_position.colno);
	fflush(stdout);
}
