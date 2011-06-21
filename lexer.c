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

//#define DEBUG_CHARS
#define MAX_PUTBACK 3
#define BUF_SIZE    1024

#if defined(_WIN32) || defined(__CYGWIN__)
/* No strtold on windows and no replacement yet */
#define strtold(s, e) strtod(s, e)
#endif

static utf32             c;
static source_position_t lexer_pos;
token_t                  lexer_token;
static symbol_t         *symbol_L;
static FILE             *input;
static utf32             buf[BUF_SIZE + MAX_PUTBACK];
static const utf32      *bufend;
static const utf32      *bufpos;
static strset_t          stringset;
bool                     allow_dollar_in_symbol = true;

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

static size_t read_block(unsigned char *const read_buf, size_t const n)
{
	size_t const s = fread(read_buf, 1, n, input);
	if (s == 0) {
		/* on OS/X ferror appears to return true on eof as well when running
		 * the application in gdb... */
		if (!feof(input) && ferror(input))
			parse_error("read from input failed");
		buf[MAX_PUTBACK] = EOF;
		bufpos           = buf + MAX_PUTBACK;
		bufend           = buf + MAX_PUTBACK + 1;
	}
	return s;
}

static void decode_iso_8859_1(void)
{
	unsigned char read_buf[BUF_SIZE];
	size_t const s = read_block(read_buf, sizeof(read_buf));
	if (s == 0)
		return;

	unsigned char const *src = read_buf;
	unsigned char const *end = read_buf + s;
	utf32               *dst = buf + MAX_PUTBACK;
	while (src != end)
		*dst++ = *src++;

	bufpos = buf + MAX_PUTBACK;
	bufend = dst;
}

static void decode_iso_8859_15(void)
{
	unsigned char read_buf[BUF_SIZE];
	size_t const s = read_block(read_buf, sizeof(read_buf));
	if (s == 0)
		return;

	unsigned char const *src = read_buf;
	unsigned char const *end = read_buf + s;
	utf32               *dst = buf + MAX_PUTBACK;
	while (src != end) {
		utf32 tc = *src++;
		switch (tc) {
			case 0xA4: tc = 0x20AC; break; // €
			case 0xA6: tc = 0x0160; break; // Š
			case 0xA8: tc = 0x0161; break; // š
			case 0xB4: tc = 0x017D; break; // Ž
			case 0xB8: tc = 0x017E; break; // ž
			case 0xBC: tc = 0x0152; break; // Œ
			case 0xBD: tc = 0x0153; break; // œ
			case 0xBE: tc = 0x0178; break; // Ÿ
		}
		*dst++ = tc;
	}

	bufpos = buf + MAX_PUTBACK;
	bufend = dst;
}

static void decode_utf8(void)
{
	static utf32  part_decoded_min_code;
	static utf32  part_decoded_char;
	static size_t part_decoded_rest_len;

	do {
		unsigned char read_buf[BUF_SIZE];
		size_t const s = read_block(read_buf, sizeof(read_buf));
		if (s == 0) {
			if (part_decoded_rest_len > 0)
				parse_error("incomplete input char at end of input");
			return;
		}

		unsigned char const *src = read_buf;
		unsigned char const *end = read_buf + s;
		utf32               *dst = buf + MAX_PUTBACK;
		utf32                decoded;
		utf32                min_code;

		if (part_decoded_rest_len != 0) {
			min_code              = part_decoded_min_code;
			decoded               = part_decoded_char;
			size_t const rest_len = part_decoded_rest_len;
			part_decoded_rest_len = 0;
			switch (rest_len) {
				case 4:  goto realign;
				case 3:  goto three_more;
				case 2:  goto two_more;
				default: goto one_more;
			}
		}

		while (src != end) {
			if ((*src & 0x80) == 0) {
				decoded = *src++;
			} else if ((*src & 0xE0) == 0xC0) {
				min_code = 0x80;
				decoded  = *src++ & 0x1F;
one_more:
				if (src == end) {
					part_decoded_min_code = min_code;
					part_decoded_char     = decoded;
					part_decoded_rest_len = 1;
					break;
				}
				if ((*src & 0xC0) == 0x80) {
					decoded = (decoded << 6) | (*src++ & 0x3F);
				} else {
					goto invalid_char;
				}
				if (decoded < min_code                      ||
						decoded > 0x10FFFF                      ||
						(0xD800 <= decoded && decoded < 0xE000) || // high/low surrogates
						(0xFDD0 <= decoded && decoded < 0xFDF0) || // noncharacters
						(decoded & 0xFFFE) == 0xFFFE) {            // noncharacters
					parse_error("invalid byte sequence in input");
				}
			} else if ((*src & 0xF0) == 0xE0) {
				min_code = 0x800;
				decoded  = *src++ & 0x0F;
two_more:
				if (src == end) {
					part_decoded_min_code = min_code;
					part_decoded_char     = decoded;
					part_decoded_rest_len = 2;
					break;
				}
				if ((*src & 0xC0) == 0x80) {
					decoded = (decoded << 6) | (*src++ & 0x3F);
				} else {
					goto invalid_char;
				}
				goto one_more;
			} else if ((*src & 0xF8) == 0xF0) {
				min_code = 0x10000;
				decoded  = *src++ & 0x07;
three_more:
				if (src == end) {
					part_decoded_min_code = min_code;
					part_decoded_char     = decoded;
					part_decoded_rest_len = 3;
					break;
				}
				if ((*src & 0xC0) == 0x80) {
					decoded = (decoded << 6) | (*src++ & 0x3F);
				} else {
					goto invalid_char;
				}
				goto two_more;
			} else {
invalid_char:
				parse_error("invalid byte sequence in input");
realign:
				do {
					++src;
					if (src == end) {
						part_decoded_rest_len = 4;
						break;
					}
				} while ((*src & 0xC0) == 0x80 || (*src & 0xF8) == 0xF8);
				continue;
			}
			*dst++ = decoded;
		}

		bufpos = buf + MAX_PUTBACK;
		bufend = dst;
	} while (bufpos == bufend);
}

static void decode_windows_1252(void)
{
	unsigned char read_buf[BUF_SIZE];
	size_t const s = read_block(read_buf, sizeof(read_buf));
	if (s == 0)
		return;

	unsigned char const *src = read_buf;
	unsigned char const *end = read_buf + s;
	utf32               *dst = buf + MAX_PUTBACK;
	while (src != end) {
		utf32 tc = *src++;
		switch (tc) {
			case 0x80: tc = 0x20AC; break; // €
			case 0x82: tc = 0x201A; break; // ‚
			case 0x83: tc = 0x0192; break; // ƒ
			case 0x84: tc = 0x201E; break; // „
			case 0x85: tc = 0x2026; break; // …
			case 0x86: tc = 0x2020; break; // †
			case 0x87: tc = 0x2021; break; // ‡
			case 0x88: tc = 0x02C6; break; // ˆ
			case 0x89: tc = 0x2030; break; // ‰
			case 0x8A: tc = 0x0160; break; // Š
			case 0x8B: tc = 0x2039; break; // ‹
			case 0x8C: tc = 0x0152; break; // Œ
			case 0x8E: tc = 0x017D; break; // Ž
			case 0x91: tc = 0x2018; break; // ‘
			case 0x92: tc = 0x2019; break; // ’
			case 0x93: tc = 0x201C; break; // “
			case 0x94: tc = 0x201D; break; // ”
			case 0x95: tc = 0x2022; break; // •
			case 0x96: tc = 0x2013; break; // –
			case 0x97: tc = 0x2014; break; // —
			case 0x98: tc = 0x02DC; break; // ˜
			case 0x99: tc = 0x2122; break; // ™
			case 0x9A: tc = 0x0161; break; // š
			case 0x9B: tc = 0x203A; break; // ›
			case 0x9C: tc = 0x0153; break; // œ
			case 0x9E: tc = 0x017E; break; // ž
			case 0x9F: tc = 0x0178; break; // Ÿ
		}
		*dst++ = tc;
	}

	bufpos = buf + MAX_PUTBACK;
	bufend = dst;
}

typedef void (*decoder_t)(void);

static decoder_t decoder = decode_utf8;

typedef struct named_decoder_t {
	char const *name;
	decoder_t   decoder;
} named_decoder_t;

static named_decoder_t const decoders[] = {
	{ "CP819",           decode_iso_8859_1   }, // offical alias
	{ "IBM819",          decode_iso_8859_1   }, // offical alias
	{ "ISO-8859-1",      decode_iso_8859_1   }, // offical alias
	{ "ISO-8859-15",     decode_iso_8859_15  }, // offical name
	{ "ISO8859-1",       decode_iso_8859_1   },
	{ "ISO8859-15",      decode_iso_8859_15  },
	{ "ISO_8859-1",      decode_iso_8859_1   }, // offical alias
	{ "ISO_8859-15",     decode_iso_8859_15  }, // offical alias
	{ "ISO_8859-1:1987", decode_iso_8859_1   }, // offical name
	{ "Latin-9",         decode_iso_8859_15  }, // offical alias
	{ "UTF-8",           decode_utf8         }, // offical name
	{ "csISOLatin1",     decode_iso_8859_1   }, // offical alias
	{ "cp1252",          decode_windows_1252 },
	{ "iso-ir-100",      decode_iso_8859_1   }, // offical alias
	{ "l1",              decode_iso_8859_1   }, // offical alias
	{ "latin1",          decode_iso_8859_1   }, // offical alias
	{ "windows-1252",    decode_windows_1252 }, // official name

	{ NULL,              NULL                }
};

/** strcasecmp is not part of C99 so we need our own implementation here */
static int my_strcasecmp(const char *s1, const char *s2)
{
	for ( ; *s1 != 0; ++s1, ++s2) {
		if (tolower(*s1) != tolower(*s2))
			break;
	}
	return (unsigned char)*s1 - (unsigned char)*s2;
}

void select_input_encoding(char const* const encoding)
{
	for (named_decoder_t const *i = decoders; i->name != NULL; ++i) {
		if (my_strcasecmp(encoding, i->name) != 0)
			continue;
		decoder = i->decoder;
		return;
	}
	fprintf(stderr, "error: input encoding \"%s\" not supported\n", encoding);
}

static inline void next_real_char(void)
{
	assert(bufpos <= bufend);
	if (bufpos >= bufend) {
		if (input == NULL) {
			c = EOF;
			return;
		}
		decoder();
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
	assert(bufpos > buf);
	*(--bufpos - buf + buf) = pc;
	--lexer_pos.colno;

#ifdef DEBUG_CHARS
	printf("putback '%lc'\n", pc);
#endif
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

#define eat(c_type)  do { assert(c == c_type); next_char(); } while (0)

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
		goto end_of_next_char;
	}

	if (LIKELY(c != '?'))
		goto end_of_next_char;

	next_real_char();
	if (LIKELY(c != '?')) {
		put_back(c);
		c = '?';
		goto end_of_next_char;
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

end_of_next_char:;
#ifdef DEBUG_CHARS
	printf("nchar '%c'\n", c);
#endif
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

	lexer_token.type   = symbol->ID;
	lexer_token.symbol = symbol;

	if (symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}
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
		lexer_token.symbol = NULL;
		return;
	}

	obstack_1grow(&symbol_obstack, '\0');
	char     *string = obstack_finish(&symbol_obstack);
	symbol_t *symbol = symbol_table_insert(string);

	if (symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}
	lexer_token.symbol = symbol;
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
		errorf(&lexer_token.source_position,
		       "hexadecimal floatingpoint constant requires an exponent");
	}
	obstack_1grow(&symbol_obstack, '\0');

	size_t  size   = obstack_object_size(&symbol_obstack) - 1;
	char   *string = obstack_finish(&symbol_obstack);
	lexer_token.literal = identify_string(string, size);

	lexer_token.type    =
		is_float ? T_FLOATINGPOINT_HEXADECIMAL : T_INTEGER_HEXADECIMAL;

	if (!has_digits) {
		errorf(&lexer_token.source_position, "invalid number literal '0x%S'",
		       &lexer_token.literal);
		lexer_token.literal.begin = "0";
		lexer_token.literal.size  = 1;
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
	switch (chr) {
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
	lexer_token.literal = identify_string(string, size);

	/* is it an octal number? */
	if (is_float) {
		lexer_token.type = T_FLOATINGPOINT;
	} else if (string[0] == '0') {
		lexer_token.type = T_INTEGER_OCTAL;

		/* check for invalid octal digits */
		for (size_t i= 0; i < size; ++i) {
			char t = string[i];
			if (t >= '8')
				errorf(&lexer_token.source_position,
				       "invalid digit '%c' in octal number", t);
		}
	} else {
		lexer_token.type = T_INTEGER;
	}

	if (!has_digits) {
		errorf(&lexer_token.source_position, "invalid number literal '%S'",
		       &lexer_token.literal);
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

static void grow_symbol(utf32 const tc)
{
	struct obstack *const o  = &symbol_obstack;
	if (tc < 0x80U) {
		obstack_1grow(o, tc);
	} else if (tc < 0x800) {
		obstack_1grow(o, 0xC0 | (tc >> 6));
		obstack_1grow(o, 0x80 | (tc & 0x3F));
	} else if (tc < 0x10000) {
		obstack_1grow(o, 0xE0 | ( tc >> 12));
		obstack_1grow(o, 0x80 | ((tc >>  6) & 0x3F));
		obstack_1grow(o, 0x80 | ( tc        & 0x3F));
	} else {
		obstack_1grow(o, 0xF0 | ( tc >> 18));
		obstack_1grow(o, 0x80 | ((tc >> 12) & 0x3F));
		obstack_1grow(o, 0x80 | ((tc >>  6) & 0x3F));
		obstack_1grow(o, 0x80 | ( tc        & 0x3F));
	}
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
			errorf(&lexer_token.source_position, "string has no end");
			lexer_token.type = T_ERROR;
			return;
		}

		case '"':
			next_char();
			goto end_of_string;

		default:
			grow_symbol(c);
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

	lexer_token.type    = T_STRING_LITERAL;
	lexer_token.literal = identify_string(string, size);
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
			grow_symbol(tc);
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
			errorf(&lexer_token.source_position, "EOF while parsing character constant");
			lexer_token.type = T_ERROR;
			return;
		}

		default:
			grow_symbol(c);
			next_char();
			break;
		}
	}

end_of_wide_char_constant:;
	obstack_1grow(&symbol_obstack, '\0');
	size_t  size   = (size_t) obstack_object_size(&symbol_obstack) - 1;
	char   *string = obstack_finish(&symbol_obstack);

	lexer_token.type     = T_WIDE_CHARACTER_CONSTANT;
	lexer_token.literal  = identify_string(string, size);

	if (size == 0) {
		errorf(&lexer_token.source_position, "empty character constant");
	}
}

/**
 * Parse a wide string literal and set lexer_token.
 */
static void parse_wide_string_literal(void)
{
	parse_string_literal();
	if (lexer_token.type == T_STRING_LITERAL)
		lexer_token.type = T_WIDE_STRING_LITERAL;
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
			errorf(&lexer_token.source_position, "EOF while parsing character constant");
			lexer_token.type = T_ERROR;
			return;
		}

		default:
			grow_symbol(c);
			next_char();
			break;

		}
	}

end_of_char_constant:;
	obstack_1grow(&symbol_obstack, '\0');
	const size_t        size   = (size_t)obstack_object_size(&symbol_obstack)-1;
	char         *const string = obstack_finish(&symbol_obstack);

	lexer_token.type    = T_CHARACTER_CONSTANT;
	lexer_token.literal = identify_string(string, size);

	if (size == 0) {
		errorf(&lexer_token.source_position, "empty character constant");
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
			errorf(&lexer_token.source_position, "at end of file while looking for comment end");
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
	while (pp_token.type != '\n' && pp_token.type != T_EOF) {
		next_pp_token();
	}
}

/**
 * Handle the define directive.
 */
static void define_directive(void)
{
	lexer_next_preprocessing_token();
	if (lexer_token.type != T_IDENTIFIER) {
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
	if (pp_token.type != T_INTEGER) {
		parse_error("expected integer");
	} else {
		/* use offset -1 as this is about the next line */
		lexer_pos.lineno = atoi(pp_token.literal.begin) - 1;
		next_pp_token();
	}
	if (pp_token.type == T_STRING_LITERAL) {
		lexer_pos.input_name = pp_token.literal.begin;
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
	if (pp_token.symbol->pp_ID == TP_STDC) {
		stdc_pragma_kind_t kind = STDC_UNKNOWN;
		/* a STDC pragma */
		if (c_mode & _C99) {
			next_pp_token();

			switch (pp_token.symbol->pp_ID) {
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
				switch (pp_token.symbol->pp_ID) {
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
	if (unknown_pragma) {
		warningf(WARN_UNKNOWN_PRAGMAS, &pp_token.source_position, "encountered unknown #pragma");
	}
}

/**
 * Parse a preprocessor non-null directive.
 */
static void parse_preprocessor_identifier(void)
{
	assert(pp_token.type == T_IDENTIFIER);
	symbol_t *symbol = pp_token.symbol;

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

	switch (pp_token.type) {
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
					lexer_token.type = set_type;           \
					return;

/* must use this as last thing */
#define MAYBE_MODE(ch, set_type, mode)                     \
				case ch:                                   \
					if (c_mode & mode) {                   \
						next_char();                       \
						lexer_token.type = set_type;       \
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
			lexer_token.type = set_type;                   \
		)

void lexer_next_preprocessing_token(void)
{
	while (true) {
		lexer_token.source_position = lexer_pos;

		switch (c) {
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
			if (lexer_token.symbol == symbol_L) {
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
						lexer_token.type = '.';
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
dollar_sign:
			errorf(&lexer_pos, "unknown character '%c' found", c);
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
	input                = stream;
	lexer_pos.lineno     = 0;
	lexer_pos.colno      = 0;
	lexer_pos.input_name = input_name;

	bufpos = NULL;
	bufend = NULL;

	/* place a virtual \n at the beginning so the lexer knows that we're
	 * at the beginning of a line */
	c = '\n';
}

void lexer_open_buffer(const char *buffer, size_t len, const char *input_name)
{
	input                = NULL;
	lexer_pos.lineno     = 0;
	lexer_pos.colno      = 0;
	lexer_pos.input_name = input_name;

#if 0 // TODO
	bufpos = buffer;
	bufend = buffer + len;

	/* place a virtual \n at the beginning so the lexer knows that we're
	 * at the beginning of a line */
	c = '\n';
#else
	(void)buffer;
	(void)len;
	panic("builtin lexing not done yet");
#endif
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
