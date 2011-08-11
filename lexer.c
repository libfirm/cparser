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
#include "target_architecture.h"
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

#define MATCH_NEWLINE(code)  \
	case '\r':               \
		next_char();         \
		if (c == '\n') {     \
	case '\n':               \
			next_char();     \
		}                    \
		lexer_pos.lineno++;  \
		lexer_pos.colno = 1; \
		code

#define eat(c_type) (assert(c == c_type), next_char())

static void maybe_concat_lines(void)
{
	eat('\\');

	switch (c) {
	MATCH_NEWLINE(return;)

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

/**
 * Read a symbol from the input and build
 * the lexer_token.
 */
static void parse_symbol(void)
{
	obstack_1grow(&symbol_obstack, (char) c);
	next_char();

	while (true) {
		switch (c) {
		DIGITS
		SYMBOL_CHARS
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
			break;

		default:
dollar_sign:
			goto end_symbol;
		}
	}

end_symbol:
	obstack_1grow(&symbol_obstack, '\0');

	char     *string = obstack_finish(&symbol_obstack);
	symbol_t *symbol = symbol_table_insert(string);

	lexer_token.kind              = symbol->ID;
	lexer_token.identifier.symbol = symbol;

	if (symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}
}

static string_t identify_string(char *string, size_t len)
{
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

	obstack_1grow(&symbol_obstack, '\0');
	size_t    size   = obstack_object_size(&symbol_obstack);
	char     *string = obstack_finish(&symbol_obstack);

	lexer_token.number.suffix = identify_string(string, size);
}

/**
 * Parses a hex number including hex floats and set the
 * lexer_token.
 */
static void parse_number_hex(void)
{
	bool is_float   = false;
	bool has_digits = false;

	assert(obstack_object_size(&symbol_obstack) == 0);
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

		if (c == '-' || c == '+') {
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}

		while (isxdigit(c)) {
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}
	} else if (is_float) {
		errorf(&lexer_token.base.source_position,
		       "hexadecimal floatingpoint constant requires an exponent");
	}
	obstack_1grow(&symbol_obstack, '\0');

	size_t  size   = obstack_object_size(&symbol_obstack) - 1;
	char   *string = obstack_finish(&symbol_obstack);
	lexer_token.number.number = identify_string(string, size);

	lexer_token.kind    =
		is_float ? T_FLOATINGPOINT_HEXADECIMAL : T_INTEGER_HEXADECIMAL;

	if (!has_digits) {
		errorf(&lexer_token.base.source_position,
		       "invalid number literal '0x%S'", &lexer_token.number.number);
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
		next_char();
		if (c == 'x' || c == 'X') {
			next_char();
			parse_number_hex();
			return;
		} else {
			has_digits = true;
		}
		obstack_1grow(&symbol_obstack, '0');
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

		if (c == '-' || c == '+') {
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}

		while (isdigit(c)) {
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}
	}

	obstack_1grow(&symbol_obstack, '\0');
	size_t  size   = obstack_object_size(&symbol_obstack) - 1;
	char   *string = obstack_finish(&symbol_obstack);
	lexer_token.number.number = identify_string(string, size);

	/* is it an octal number? */
	if (is_float) {
		lexer_token.kind = T_FLOATINGPOINT;
	} else if (string[0] == '0') {
		lexer_token.kind = T_INTEGER_OCTAL;

		/* check for invalid octal digits */
		for (size_t i= 0; i < size; ++i) {
			char t = string[i];
			if (t >= '8')
				errorf(&lexer_token.base.source_position,
				       "invalid digit '%c' in octal number", t);
		}
	} else {
		lexer_token.kind = T_INTEGER;
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
	case 'u':
	case 'U':
		parse_error("universal character parsing not implemented yet");
		return EOF;
	default:
		break;
	}
	/* §6.4.4.4:8 footnote 64 */
	parse_error("unknown escape sequence");
	return EOF;
}

/**
 * Concatenate two strings.
 */
string_t concat_strings(const string_t *const s1, const string_t *const s2)
{
	const size_t len1 = s1->size - 1;
	const size_t len2 = s2->size - 1;

	char *const concat = obstack_alloc(&symbol_obstack, len1 + len2 + 1);
	memcpy(concat, s1->begin, len1);
	memcpy(concat + len1, s2->begin, len2 + 1);

	return identify_string(concat, len1 + len2 + 1);
}

string_t make_string(const char *string)
{
	size_t      len   = strlen(string) + 1;
	char *const space = obstack_alloc(&symbol_obstack, len);
	memcpy(space, string, len);

	return identify_string(space, len);
}

/**
 * Parse a string literal and set lexer_token.
 */
static void parse_string_literal(void)
{
	eat('"');

	while (true) {
		switch (c) {
		case '\\': {
			utf32 const tc = parse_escape_sequence();
			if (tc >= 0x100) {
				warningf(WARN_OTHER, &lexer_pos, "escape sequence out of range");
			}
			obstack_1grow(&symbol_obstack, tc);
			break;
		}

		case EOF: {
			errorf(&lexer_token.base.source_position, "string has no end");
			lexer_token.kind = T_ERROR;
			return;
		}

		case '"':
			next_char();
			goto end_of_string;

		default:
			obstack_grow_symbol(&symbol_obstack, c);
			next_char();
			break;
		}
	}

end_of_string:

	/* TODO: concatenate multiple strings separated by whitespace... */

	/* add finishing 0 to the string */
	obstack_1grow(&symbol_obstack, '\0');
	const size_t  size   = (size_t)obstack_object_size(&symbol_obstack);
	char         *string = obstack_finish(&symbol_obstack);

	lexer_token.kind          = T_STRING_LITERAL;
	lexer_token.string.string = identify_string(string, size);
}

/**
 * Parse a wide character constant and set lexer_token.
 */
static void parse_wide_character_constant(void)
{
	eat('\'');

	while (true) {
		switch (c) {
		case '\\': {
			const utf32 tc = parse_escape_sequence();
			obstack_grow_symbol(&symbol_obstack, tc);
			break;
		}

		MATCH_NEWLINE(
			parse_error("newline while parsing character constant");
			break;
		)

		case '\'':
			next_char();
			goto end_of_wide_char_constant;

		case EOF: {
			errorf(&lexer_token.base.source_position,
			       "EOF while parsing character constant");
			lexer_token.kind = T_ERROR;
			return;
		}

		default:
			obstack_grow_symbol(&symbol_obstack, c);
			next_char();
			break;
		}
	}

end_of_wide_char_constant:;
	obstack_1grow(&symbol_obstack, '\0');
	size_t  size   = (size_t) obstack_object_size(&symbol_obstack) - 1;
	char   *string = obstack_finish(&symbol_obstack);

	lexer_token.kind          = T_WIDE_CHARACTER_CONSTANT;
	lexer_token.string.string = identify_string(string, size);

	if (size == 0) {
		errorf(&lexer_token.base.source_position, "empty character constant");
	}
}

/**
 * Parse a wide string literal and set lexer_token.
 */
static void parse_wide_string_literal(void)
{
	parse_string_literal();
	if (lexer_token.kind == T_STRING_LITERAL)
		lexer_token.kind = T_WIDE_STRING_LITERAL;
}

/**
 * Parse a character constant and set lexer_token.
 */
static void parse_character_constant(void)
{
	eat('\'');

	while (true) {
		switch (c) {
		case '\\': {
			utf32 const tc = parse_escape_sequence();
			if (tc >= 0x100) {
				warningf(WARN_OTHER, &lexer_pos, "escape sequence out of range");
			}
			obstack_1grow(&symbol_obstack, tc);
			break;
		}

		MATCH_NEWLINE(
			parse_error("newline while parsing character constant");
			break;
		)

		case '\'':
			next_char();
			goto end_of_char_constant;

		case EOF: {
			errorf(&lexer_token.base.source_position,
			       "EOF while parsing character constant");
			lexer_token.kind = T_ERROR;
			return;
		}

		default:
			obstack_grow_symbol(&symbol_obstack, c);
			next_char();
			break;

		}
	}

end_of_char_constant:;
	obstack_1grow(&symbol_obstack, '\0');
	const size_t        size   = (size_t)obstack_object_size(&symbol_obstack)-1;
	char         *const string = obstack_finish(&symbol_obstack);

	lexer_token.kind          = T_CHARACTER_CONSTANT;
	lexer_token.string.string = identify_string(string, size);

	if (size == 0) {
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

		MATCH_NEWLINE(break;)

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
 * Handle the define directive.
 */
static void define_directive(void)
{
	lexer_next_preprocessing_token();
	if (lexer_token.kind != T_IDENTIFIER) {
		parse_error("expected identifier after #define\n");
		eat_until_newline();
	}
}

/**
 * Handle the ifdef directive.
 */
static void ifdef_directive(int is_ifndef)
{
	(void) is_ifndef;
	lexer_next_preprocessing_token();
	//expect_identifier();
	//extect_newline();
}

/**
 * Handle the endif directive.
 */
static void endif_directive(void)
{
	//expect_newline();
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
	if (pp_token.kind == T_STRING_LITERAL) {
		lexer_pos.input_name = pp_token.string.string.begin;
		next_pp_token();
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
	bool unknown_pragma = true;

	next_pp_token();
	if (pp_token.kind != T_IDENTIFIER) {
		warningf(WARN_UNKNOWN_PRAGMAS, &pp_token.base.source_position,
		         "expected identifier after #pragma");
		eat_until_newline();
		return;
	}

	symbol_t *symbol = pp_token.identifier.symbol;
	if (symbol->pp_ID == TP_STDC) {
		stdc_pragma_kind_t kind = STDC_UNKNOWN;
		/* a STDC pragma */
		if (c_mode & _C99) {
			next_pp_token();

			switch (pp_token.identifier.symbol->pp_ID) {
			case TP_FP_CONTRACT:
				kind = STDC_FP_CONTRACT;
				break;
			case TP_FENV_ACCESS:
				kind = STDC_FENV_ACCESS;
				break;
			case TP_CX_LIMITED_RANGE:
				kind = STDC_CX_LIMITED_RANGE;
				break;
			default:
				break;
			}
			if (kind != STDC_UNKNOWN) {
				stdc_pragma_value_kind_t value = STDC_VALUE_UNKNOWN;
				next_pp_token();
				switch (pp_token.identifier.symbol->pp_ID) {
				case TP_ON:
					value = STDC_VALUE_ON;
					break;
				case TP_OFF:
					value = STDC_VALUE_OFF;
					break;
				case TP_DEFAULT:
					value = STDC_VALUE_DEFAULT;
					break;
				default:
					break;
				}
				if (value != STDC_VALUE_UNKNOWN) {
					unknown_pragma = false;
				} else {
					errorf(&pp_token.base.source_position,
					       "bad STDC pragma argument");
				}
			}
		}
	} else {
		unknown_pragma = true;
	}
	eat_until_newline();
	if (unknown_pragma) {
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
	symbol_t *symbol = pp_token.identifier.symbol;

	switch (symbol->pp_ID) {
	case TP_include:
		printf("include - enable header name parsing!\n");
		break;
	case TP_define:
		define_directive();
		break;
	case TP_ifdef:
		ifdef_directive(0);
		break;
	case TP_ifndef:
		ifdef_directive(1);
		break;
	case TP_endif:
		endif_directive();
		break;
	case TP_line:
		next_pp_token();
		parse_line_directive();
		break;
	case TP_if:
	case TP_else:
	case TP_elif:
	case TP_undef:
	case TP_error:
		/* TODO; output the rest of the line */
		parse_error("#error directive: ");
		break;
	case TP_pragma:
		parse_pragma();
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

		switch (c) {
		case ' ':
		case '\t':
			next_char();
			break;

		MATCH_NEWLINE(
			lexer_token.kind = '\n';
			return;
		)

		SYMBOL_CHARS
			parse_symbol();
			/* might be a wide string ( L"string" ) */
			if (lexer_token.identifier.symbol == symbol_L) {
				switch (c) {
					case '"':  parse_wide_string_literal();     break;
					case '\'': parse_wide_character_constant(); break;
				}
			}
			return;

		DIGITS
			parse_number();
			return;

		case '"':
			parse_string_literal();
			return;

		case '\'':
			parse_character_constant();
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

		case '@':
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
		case '\\':
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
			lexer_token.kind = T_ERROR;
			return;
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
	        source_position.lineno, source_position.colno);
	fflush(stdout);
}
