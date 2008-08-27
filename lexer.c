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
#include <config.h>

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

//#define DEBUG_CHARS
#define MAX_PUTBACK 3

#ifdef _WIN32
/* No strtold on windows and no replacement yet */
#define strtold(s, e) strtod(s, e)
#endif

static int         c;
token_t            lexer_token;
symbol_t          *symbol_L;
static FILE       *input;
static char        buf[1024 + MAX_PUTBACK];
static const char *bufend;
static const char *bufpos;
static strset_t    stringset;

/**
 * Prints a parse error message at the current token.
 *
 * @param msg   the error message
 */
static void parse_error(const char *msg)
{
	errorf(&lexer_token.source_position,  "%s", msg);
}

/**
 * Prints an internal error message at the current token.
 *
 * @param msg   the error message
 */
static NORETURN internal_error(const char *msg)
{
	internal_errorf(&lexer_token.source_position,  "%s", msg);
}

static inline void next_real_char(void)
{
	assert(bufpos <= bufend);
	if (bufpos >= bufend) {
		if (input == NULL) {
			c = EOF;
			return;
		}

		size_t s = fread(buf + MAX_PUTBACK, 1, sizeof(buf) - MAX_PUTBACK,
		                 input);
		if(s == 0) {
			c = EOF;
			return;
		}
		bufpos = buf + MAX_PUTBACK;
		bufend = buf + MAX_PUTBACK + s;
	}
	c = *bufpos++;
}

/**
 * Put a character back into the buffer.
 *
 * @param pc  the character to put back
 */
static inline void put_back(int pc)
{
	assert(bufpos > buf);
	*(--bufpos - buf + buf) = (char) pc;

#ifdef DEBUG_CHARS
	printf("putback '%c'\n", pc);
#endif
}

static inline void next_char(void);

#define MATCH_NEWLINE(code)                   \
	case '\r':                                \
		next_char();                          \
		if(c == '\n') {                       \
			next_char();                      \
		}                                     \
		lexer_token.source_position.linenr++; \
		code                                  \
	case '\n':                                \
		next_char();                          \
		lexer_token.source_position.linenr++; \
		code

#define eat(c_type)  do { assert(c == c_type); next_char(); } while(0)

static void maybe_concat_lines(void)
{
	eat('\\');

	switch(c) {
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
	if(UNLIKELY(c == '\\')) {
		maybe_concat_lines();
		goto end_of_next_char;
	}

	if(LIKELY(c != '?'))
		goto end_of_next_char;

	next_real_char();
	if(LIKELY(c != '?')) {
		put_back(c);
		c = '?';
		goto end_of_next_char;
	}

	next_real_char();
	switch(c) {
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

end_of_next_char:;
#ifdef DEBUG_CHARS
	printf("nchar '%c'\n", c);
#endif
}

#define SYMBOL_CHARS  \
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
	case '_':         \
	case '$': // TODO add option to deactivate $ in identifers

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
	symbol_t *symbol;
	char     *string;

	obstack_1grow(&symbol_obstack, (char) c);
	next_char();

	while(1) {
		switch(c) {
		DIGITS
		SYMBOL_CHARS
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
			break;

		default:
			goto end_symbol;
		}
	}

end_symbol:
	obstack_1grow(&symbol_obstack, '\0');

	string = obstack_finish(&symbol_obstack);
	symbol = symbol_table_insert(string);

	lexer_token.type     = symbol->ID;
	lexer_token.v.symbol = symbol;

	if(symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}
}

static void parse_integer_suffix(bool is_oct_hex)
{
	bool is_unsigned  = false;
	bool min_long     = false;
	bool min_longlong = false;

	if(c == 'U' || c == 'u') {
		is_unsigned = true;
		next_char();
		if(c == 'L' || c == 'l') {
			min_long = true;
			next_char();
			if(c == 'L' || c == 'l') {
				min_longlong = true;
				next_char();
			}
		}
	} else if(c == 'l' || c == 'L') {
		min_long = true;
		next_char();
		if(c == 'l' || c == 'L') {
			min_longlong = true;
			next_char();
			if(c == 'u' || c == 'U') {
				is_unsigned = true;
				next_char();
			}
		} else if(c == 'u' || c == 'U') {
			is_unsigned = true;
			next_char();
			lexer_token.datatype = type_unsigned_long;
		}
	}

	if(!is_unsigned) {
		long long v = lexer_token.v.intvalue;
		if(!min_long) {
			if(v >= TARGET_INT_MIN && v <= TARGET_INT_MAX) {
				lexer_token.datatype = type_int;
				return;
			} else if(is_oct_hex && v >= 0 && v <= TARGET_UINT_MAX) {
				lexer_token.datatype = type_unsigned_int;
				return;
			}
		}
		if(!min_longlong) {
			if(v >= TARGET_LONG_MIN && v <= TARGET_LONG_MAX) {
				lexer_token.datatype = type_long;
				return;
			} else if(is_oct_hex && v >= 0 && (unsigned long long)v <= (unsigned long long)TARGET_ULONG_MAX) {
				lexer_token.datatype = type_unsigned_long;
				return;
			}
		}
		unsigned long long uv = (unsigned long long) v;
		if(is_oct_hex && uv > (unsigned long long) TARGET_LONGLONG_MAX) {
			lexer_token.datatype = type_unsigned_long_long;
			return;
		}

		lexer_token.datatype = type_long_long;
	} else {
		unsigned long long v = (unsigned long long) lexer_token.v.intvalue;
		if(!min_long && v <= TARGET_UINT_MAX) {
			lexer_token.datatype = type_unsigned_int;
			return;
		}
		if(!min_longlong && v <= TARGET_ULONG_MAX) {
			lexer_token.datatype = type_unsigned_long;
			return;
		}
		lexer_token.datatype = type_unsigned_long_long;
	}
}

static void parse_floating_suffix(void)
{
	switch(c) {
	/* TODO: do something useful with the suffixes... */
	case 'f':
	case 'F':
		next_char();
		lexer_token.datatype = type_float;
		break;
	case 'l':
	case 'L':
		next_char();
		lexer_token.datatype = type_long_double;
		break;
	default:
		lexer_token.datatype = type_double;
		break;
	}
}

/**
 * A replacement for strtoull. Only those parts needed for
 * our parser are implemented.
 */
static unsigned long long parse_int_string(const char *s, const char **endptr, int base) {
	unsigned long long v = 0;

	switch (base) {
	case 16:
		for (;; ++s) {
			/* check for overrun */
			if (v >= 0x1000000000000000ULL)
				break;
			switch (tolower(*s)) {
			case '0': v <<= 4; break;
			case '1': v <<= 4; v |= 0x1; break;
			case '2': v <<= 4; v |= 0x2; break;
			case '3': v <<= 4; v |= 0x3; break;
			case '4': v <<= 4; v |= 0x4; break;
			case '5': v <<= 4; v |= 0x5; break;
			case '6': v <<= 4; v |= 0x6; break;
			case '7': v <<= 4; v |= 0x7; break;
			case '8': v <<= 4; v |= 0x8; break;
			case '9': v <<= 4; v |= 0x9; break;
			case 'a': v <<= 4; v |= 0xa; break;
			case 'b': v <<= 4; v |= 0xb; break;
			case 'c': v <<= 4; v |= 0xc; break;
			case 'd': v <<= 4; v |= 0xd; break;
			case 'e': v <<= 4; v |= 0xe; break;
			case 'f': v <<= 4; v |= 0xf; break;
			default:
				goto end;
			}
		}
		break;
	case 8:
		for (;; ++s) {
			/* check for overrun */
			if (v >= 0x2000000000000000ULL)
				break;
			switch (tolower(*s)) {
			case '0': v <<= 3; break;
			case '1': v <<= 3; v |= 1; break;
			case '2': v <<= 3; v |= 2; break;
			case '3': v <<= 3; v |= 3; break;
			case '4': v <<= 3; v |= 4; break;
			case '5': v <<= 3; v |= 5; break;
			case '6': v <<= 3; v |= 6; break;
			case '7': v <<= 3; v |= 7; break;
			default:
				goto end;
			}
		}
		break;
	case 10:
		for (;; ++s) {
			/* check for overrun */
			if (v > 0x1999999999999999ULL)
				break;
			switch (tolower(*s)) {
			case '0': v *= 10; break;
			case '1': v *= 10; v += 1; break;
			case '2': v *= 10; v += 2; break;
			case '3': v *= 10; v += 3; break;
			case '4': v *= 10; v += 4; break;
			case '5': v *= 10; v += 5; break;
			case '6': v *= 10; v += 6; break;
			case '7': v *= 10; v += 7; break;
			case '8': v *= 10; v += 8; break;
			case '9': v *= 10; v += 9; break;
			default:
				goto end;
			}
		}
		break;
	default:
		assert(0);
		break;
	}
end:
	*endptr = s;
	return v;
}

/**
 * Parses a hex number including hex floats and set the
 * lexer_token.
 */
static void parse_number_hex(void)
{
	assert(c == 'x' || c == 'X');
	next_char();

	while(isxdigit(c)) {
		obstack_1grow(&symbol_obstack, (char) c);
		next_char();
	}
	obstack_1grow(&symbol_obstack, '\0');
	char *string = obstack_finish(&symbol_obstack);

	if(c == '.' || c == 'p' || c == 'P') {
		next_char();
		internal_error("Hex floating point numbers not implemented yet");
	}
	if(*string == '\0') {
		parse_error("invalid hex number");
		lexer_token.type = T_ERROR;
	}

	const char *endptr;
	lexer_token.type       = T_INTEGER;
	lexer_token.v.intvalue = parse_int_string(string, &endptr, 16);
	if(*endptr != '\0') {
		parse_error("hex number literal too long");
	}

	obstack_free(&symbol_obstack, string);
	parse_integer_suffix(true);
}

/**
 * Returns true if the given char is a octal digit.
 *
 * @param char  the character to check
 */
static inline bool is_octal_digit(int chr)
{
	switch(chr) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
		return true;
	default:
		return false;
	}
}

/**
 * Parses a octal number and set the lexer_token.
 */
static void parse_number_oct(void)
{
	while(is_octal_digit(c)) {
		obstack_1grow(&symbol_obstack, (char) c);
		next_char();
	}
	obstack_1grow(&symbol_obstack, '\0');
	char *string = obstack_finish(&symbol_obstack);

	const char *endptr;
	lexer_token.type       = T_INTEGER;
	lexer_token.v.intvalue = parse_int_string(string, &endptr, 8);
	if(*endptr != '\0') {
		parse_error("octal number literal too long");
	}

	obstack_free(&symbol_obstack, string);
	parse_integer_suffix(true);
}

/**
 * Parses a decimal including float number and set the
 * lexer_token.
 */
static void parse_number_dec(void)
{
	bool is_float = false;
	while(isdigit(c)) {
		obstack_1grow(&symbol_obstack, (char) c);
		next_char();
	}

	if(c == '.') {
		obstack_1grow(&symbol_obstack, '.');
		next_char();

		while(isdigit(c)) {
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}
		is_float = true;
	}
	if(c == 'e' || c == 'E') {
		obstack_1grow(&symbol_obstack, 'e');
		next_char();

		if(c == '-' || c == '+') {
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}

		while(isdigit(c)) {
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}
		is_float = true;
	}

	obstack_1grow(&symbol_obstack, '\0');
	char *string = obstack_finish(&symbol_obstack);

	if(is_float) {
		char *endptr;
		lexer_token.type         = T_FLOATINGPOINT;
		lexer_token.v.floatvalue = strtold(string, &endptr);

		if(*endptr != '\0') {
			parse_error("invalid number literal");
		}

		parse_floating_suffix();
	} else {
		const char *endptr;
		lexer_token.type       = T_INTEGER;
		lexer_token.v.intvalue = parse_int_string(string, &endptr, 10);

		if(*endptr != '\0') {
			parse_error("invalid number literal");
		}

		parse_integer_suffix(false);
	}
	obstack_free(&symbol_obstack, string);
}

/**
 * Parses a number and sets the lexer_token.
 */
static void parse_number(void)
{
	if (c == '0') {
		next_char();
		switch (c) {
			case 'X':
			case 'x':
				parse_number_hex();
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
				parse_number_oct();
				break;
			case '8':
			case '9':
				next_char();
				parse_error("invalid octal number");
				lexer_token.type = T_ERROR;
				return;
			case '.':
			case 'e':
			case 'E':
			default:
				obstack_1grow(&symbol_obstack, '0');
				parse_number_dec();
				return;
		}
	} else {
		parse_number_dec();
	}
}

/**
 * Returns the value of a digit.
 * The only portable way to do it ...
 */
static int digit_value(int digit) {
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
static int parse_octal_sequence(const int first_digit)
{
	assert(is_octal_digit(first_digit));
	int value = digit_value(first_digit);
	if (!is_octal_digit(c)) return value;
	value = 8 * value + digit_value(c);
	next_char();
	if (!is_octal_digit(c)) return value;
	value = 8 * value + digit_value(c);
	next_char();

	if(char_is_signed) {
		return (signed char) value;
	} else {
		return (unsigned char) value;
	}
}

/**
 * Parses a hex character sequence.
 */
static int parse_hex_sequence(void)
{
	int value = 0;
	while(isxdigit(c)) {
		value = 16 * value + digit_value(c);
		next_char();
	}

	if(char_is_signed) {
		return (signed char) value;
	} else {
		return (unsigned char) value;
	}
}

/**
 * Parse an escape sequence.
 */
static int parse_escape_sequence(void)
{
	eat('\\');

	int ec = c;
	next_char();

	switch(ec) {
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
	default:
		parse_error("unknown escape sequence");
		return EOF;
	}
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

#if 0 /* TODO hash */
	const char *result = strset_insert(&stringset, concat);
	if(result != concat) {
		obstack_free(&symbol_obstack, concat);
	}

	return result;
#else
	return (string_t){ concat, len1 + len2 + 1 };
#endif
}

/**
 * Concatenate a string and a wide string.
 */
wide_string_t concat_string_wide_string(const string_t *const s1, const wide_string_t *const s2)
{
	const size_t len1 = s1->size - 1;
	const size_t len2 = s2->size - 1;

	wchar_rep_t *const concat = obstack_alloc(&symbol_obstack, (len1 + len2 + 1) * sizeof(*concat));
	const char *const src = s1->begin;
	for (size_t i = 0; i != len1; ++i) {
		concat[i] = src[i];
	}
	memcpy(concat + len1, s2->begin, (len2 + 1) * sizeof(*concat));

	return (wide_string_t){ concat, len1 + len2 + 1 };
}

/**
 * Concatenate two wide strings.
 */
wide_string_t concat_wide_strings(const wide_string_t *const s1, const wide_string_t *const s2)
{
	const size_t len1 = s1->size - 1;
	const size_t len2 = s2->size - 1;

	wchar_rep_t *const concat = obstack_alloc(&symbol_obstack, (len1 + len2 + 1) * sizeof(*concat));
	memcpy(concat,        s1->begin, len1       * sizeof(*concat));
	memcpy(concat + len1, s2->begin, (len2 + 1) * sizeof(*concat));

	return (wide_string_t){ concat, len1 + len2 + 1 };
}

/**
 * Concatenate a wide string and a string.
 */
wide_string_t concat_wide_string_string(const wide_string_t *const s1, const string_t *const s2)
{
	const size_t len1 = s1->size - 1;
	const size_t len2 = s2->size - 1;

	wchar_rep_t *const concat = obstack_alloc(&symbol_obstack, (len1 + len2 + 1) * sizeof(*concat));
	memcpy(concat, s1->begin, len1 * sizeof(*concat));
	const char *const src = s2->begin;
	for (size_t i = 0; i != len2 + 1; ++i) {
		concat[i] = src[i];
	}

	return (wide_string_t){ concat, len1 + len2 + 1 };
}

/**
 * Parse a string literal and set lexer_token.
 */
static void parse_string_literal(void)
{
	const unsigned start_linenr = lexer_token.source_position.linenr;

	eat('"');

	int tc;
	while(1) {
		switch(c) {
		case '\\':
			tc = parse_escape_sequence();
			obstack_1grow(&symbol_obstack, (char) tc);
			break;

		case EOF: {
			source_position_t source_position;
			source_position.input_name = lexer_token.source_position.input_name;
			source_position.linenr     = start_linenr;
			errorf(&source_position, "string has no end");
			lexer_token.type = T_ERROR;
			return;
		}

		case '"':
			next_char();
			goto end_of_string;

		default:
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
			break;
		}
	}

end_of_string:

	/* TODO: concatenate multiple strings separated by whitespace... */

	/* add finishing 0 to the string */
	obstack_1grow(&symbol_obstack, '\0');
	const size_t      size   = (size_t)obstack_object_size(&symbol_obstack);
	const char *const string = obstack_finish(&symbol_obstack);

#if 0 /* TODO hash */
	/* check if there is already a copy of the string */
	result = strset_insert(&stringset, string);
	if(result != string) {
		obstack_free(&symbol_obstack, string);
	}
#else
	const char *const result = string;
#endif

	lexer_token.type           = T_STRING_LITERAL;
	lexer_token.v.string.begin = result;
	lexer_token.v.string.size  = size;
}

/**
 * Parse a wide character constant and set lexer_token.
 */
static void parse_wide_character_constant(void)
{
	const unsigned start_linenr = lexer_token.source_position.linenr;

	eat('\'');

	while(1) {
		switch(c) {
		case '\\': {
			wchar_rep_t tc = parse_escape_sequence();
			obstack_grow(&symbol_obstack, &tc, sizeof(tc));
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
			source_position_t source_position = lexer_token.source_position;
			source_position.linenr = start_linenr;
			errorf(&source_position, "EOF while parsing character constant");
			lexer_token.type = T_ERROR;
			return;
		}

		default: {
			wchar_rep_t tc = (wchar_rep_t) c;
			obstack_grow(&symbol_obstack, &tc, sizeof(tc));
			next_char();
			break;
		}
		}
	}

end_of_wide_char_constant:;
	size_t             size   = (size_t) obstack_object_size(&symbol_obstack);
	assert(size % sizeof(wchar_rep_t) == 0);
	size /= sizeof(wchar_rep_t);

	const wchar_rep_t *string = obstack_finish(&symbol_obstack);

	lexer_token.type                = T_WIDE_CHARACTER_CONSTANT;
	lexer_token.v.wide_string.begin = string;
	lexer_token.v.wide_string.size  = size;
	lexer_token.datatype            = type_wchar_t;
}

/**
 * Parse a wide string literal and set lexer_token.
 */
static void parse_wide_string_literal(void)
{
	const unsigned start_linenr = lexer_token.source_position.linenr;

	assert(c == '"');
	next_char();

	while(1) {
		switch(c) {
		case '\\': {
			wchar_rep_t tc = parse_escape_sequence();
			obstack_grow(&symbol_obstack, &tc, sizeof(tc));
			break;
		}

		case EOF: {
			source_position_t source_position;
			source_position.input_name = lexer_token.source_position.input_name;
			source_position.linenr     = start_linenr;
			errorf(&source_position, "string has no end");
			lexer_token.type = T_ERROR;
			return;
		}

		case '"':
			next_char();
			goto end_of_string;

		default: {
			wchar_rep_t tc = c;
			obstack_grow(&symbol_obstack, &tc, sizeof(tc));
			next_char();
			break;
		}
		}
	}

end_of_string:;

	/* TODO: concatenate multiple strings separated by whitespace... */

	/* add finishing 0 to the string */
	wchar_rep_t nul = L'\0';
	obstack_grow(&symbol_obstack, &nul, sizeof(nul));
	const size_t             size   = (size_t)obstack_object_size(&symbol_obstack) / sizeof(wchar_rep_t);
	const wchar_rep_t *const string = obstack_finish(&symbol_obstack);

#if 0 /* TODO hash */
	/* check if there is already a copy of the string */
	const wchar_rep_t *const result = strset_insert(&stringset, string);
	if(result != string) {
		obstack_free(&symbol_obstack, string);
	}
#else
	const wchar_rep_t *const result = string;
#endif

	lexer_token.type                = T_WIDE_STRING_LITERAL;
	lexer_token.v.wide_string.begin = result;
	lexer_token.v.wide_string.size  = size;
}

/**
 * Parse a character constant and set lexer_token.
 */
static void parse_character_constant(void)
{
	const unsigned start_linenr = lexer_token.source_position.linenr;

	eat('\'');

	while(1) {
		switch(c) {
		case '\\': {
			int tc = parse_escape_sequence();
			obstack_1grow(&symbol_obstack, (char) tc);
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
			source_position_t source_position;
			source_position.input_name = lexer_token.source_position.input_name;
			source_position.linenr     = start_linenr;
			errorf(&source_position, "EOF while parsing character constant");
			lexer_token.type = T_ERROR;
			return;
		}

		default:
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
			break;

		}
	}

end_of_char_constant:;
	const size_t      size   = (size_t)obstack_object_size(&symbol_obstack);
	const char *const string = obstack_finish(&symbol_obstack);

	lexer_token.type           = T_CHARACTER_CONSTANT;
	lexer_token.v.string.begin = string;
	lexer_token.v.string.size  = size;
	lexer_token.datatype       = type_int;
}

/**
 * Skip a multiline comment.
 */
static void skip_multiline_comment(void)
{
	unsigned start_linenr = lexer_token.source_position.linenr;

	while(1) {
		switch(c) {
		case '/':
			next_char();
			if (c == '*') {
				/* TODO: nested comment, warn here */
			}
			break;
		case '*':
			next_char();
			if(c == '/') {
				next_char();
				return;
			}
			break;

		MATCH_NEWLINE(break;)

		case EOF: {
			source_position_t source_position;
			source_position.input_name = lexer_token.source_position.input_name;
			source_position.linenr     = start_linenr;
			errorf(&source_position, "at end of file while looking for comment end");
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
	while(1) {
		switch(c) {
		case EOF:
			return;

		case '\n':
		case '\r':
			return;

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
	while(pp_token.type != '\n' && pp_token.type != T_EOF) {
		next_pp_token();
	}
}

/**
 * Handle the define directive.
 */
static void define_directive(void)
{
	lexer_next_preprocessing_token();
	if(lexer_token.type != T_IDENTIFIER) {
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
	if(pp_token.type != T_INTEGER) {
		parse_error("expected integer");
	} else {
		lexer_token.source_position.linenr = (unsigned int)(pp_token.v.intvalue - 1);
		next_pp_token();
	}
	if(pp_token.type == T_STRING_LITERAL) {
		lexer_token.source_position.input_name = pp_token.v.string.begin;
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
static void parse_pragma(void) {
	bool unknown_pragma = true;

	next_pp_token();
	if (pp_token.v.symbol->pp_ID == TP_STDC) {
		stdc_pragma_kind_t kind = STDC_UNKNOWN;
		/* a STDC pragma */
		if (c_mode & _C99) {
			next_pp_token();

			switch (pp_token.v.symbol->pp_ID) {
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
				switch (pp_token.v.symbol->pp_ID) {
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
					errorf(&pp_token.source_position, "bad STDC pragma argument");
				}
			}
		}
	} else {
		unknown_pragma = true;
	}
	eat_until_newline();
	if (unknown_pragma && warning.unknown_pragmas) {
		warningf(&pp_token.source_position, "encountered unknown #pragma");
	}
}

/**
 * Parse a preprocessor non-null directive.
 */
static void parse_preprocessor_identifier(void)
{
	assert(pp_token.type == T_IDENTIFIER);
	symbol_t *symbol = pp_token.v.symbol;

	switch(symbol->pp_ID) {
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

	switch(pp_token.type) {
	case T_IDENTIFIER:
		parse_preprocessor_identifier();
		break;
	case T_INTEGER:
		parse_line_directive();
		break;
	case '\n':
		/* NULL directive, see § 6.10.7 */
		break;
	default:
		parse_error("invalid preprocessor directive");
		eat_until_newline();
		break;
	}
}

#define MAYBE_PROLOG                                       \
			next_char();                                   \
			while(1) {                                     \
				switch(c) {

#define MAYBE(ch, set_type)                                \
				case ch:                                   \
					next_char();                           \
					lexer_token.type = set_type;           \
					return;

#define ELSE_CODE(code)                                    \
				default:                                   \
					code                                   \
				}                                          \
			} /* end of while(1) */                        \
			break;

#define ELSE(set_type)                                     \
		ELSE_CODE(                                         \
			lexer_token.type = set_type;                   \
			return;                                        \
		)

void lexer_next_preprocessing_token(void)
{
	while(1) {
		switch(c) {
		case ' ':
		case '\t':
			next_char();
			break;

		MATCH_NEWLINE(
			lexer_token.type = '\n';
			return;
		)

		SYMBOL_CHARS
			parse_symbol();
			/* might be a wide string ( L"string" ) */
			if(lexer_token.type == T_IDENTIFIER &&
			    lexer_token.v.symbol == symbol_L) {
			    if(c == '"') {
					parse_wide_string_literal();
				} else if(c == '\'') {
					parse_wide_character_constant();
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
					parse_number_dec();
					return;

				case '.':
					MAYBE_PROLOG
					MAYBE('.', T_DOTDOTDOT)
					ELSE_CODE(
						put_back(c);
						c = '.';
						lexer_token.type = '.';
						return;
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
								lexer_token.type = '#';
								return;
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
			ELSE(':')
		case '=':
			MAYBE_PROLOG
			MAYBE('=', T_EQUALEQUAL)
			ELSE('=')
		case '#':
			MAYBE_PROLOG
			MAYBE('#', T_HASHHASH)
			ELSE('#')

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
			lexer_token.type = c;
			next_char();
			return;

		case EOF:
			lexer_token.type = T_EOF;
			return;

		default:
			errorf(&lexer_token.source_position, "unknown character '%c' found", c);
			next_char();
			lexer_token.type = T_ERROR;
			return;
		}
	}
}

void lexer_next_token(void)
{
	lexer_next_preprocessing_token();

	while (lexer_token.type == '\n') {
newline_found:
		lexer_next_preprocessing_token();
	}

	if (lexer_token.type == '#') {
		parse_preprocessor_directive();
		goto newline_found;
	}
}

void init_lexer(void)
{
	strset_init(&stringset);
	symbol_L = symbol_table_insert("L");
}

void lexer_open_stream(FILE *stream, const char *input_name)
{
	input                                  = stream;
	lexer_token.source_position.linenr     = 0;
	lexer_token.source_position.input_name = input_name;

	bufpos = NULL;
	bufend = NULL;

	/* place a virtual \n at the beginning so the lexer knows that we're
	 * at the beginning of a line */
	c = '\n';
}

void lexer_open_buffer(const char *buffer, size_t len, const char *input_name)
{
	input                                  = NULL;
	lexer_token.source_position.linenr     = 0;
	lexer_token.source_position.input_name = input_name;

	bufpos = buffer;
	bufend = buffer + len;

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
	fprintf(stdout, "%s:%u\n", source_position.input_name,
	        source_position.linenr);
	fflush(stdout);
}
