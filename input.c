/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "input.h"

#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

#include "adt/util.h"
#include "adt/xmalloc.h"

typedef size_t (*decode_func)(input_t *input, utf32 *buffer, size_t buffer_size);

typedef enum {
	INPUT_FILE,
	INPUT_STRING
} input_kind_t;

struct input_t {
	input_kind_t kind;
	union {
		FILE *file;
		const char *string;
	} in;
	input_decoder_t *decoder;

	/* state for utf-8 decoder */
	utf32  utf8_part_decoded_min_code;
	utf32  utf8_part_decoded_char;
	size_t utf8_part_decoded_rest_len;
};

static input_error_callback_func input_error;

void set_input_error_callback(input_error_callback_func new_func)
{
	input_error = new_func;
}

static size_t read_block(input_t *input, unsigned char *const read_buf,
                         size_t const n)
{
	if (input->kind == INPUT_FILE) {
		FILE *file = input->in.file;
		size_t const s = fread(read_buf, 1, n, file);
		if (s == 0) {
			/* on OS/X ferror appears to return true on eof as well when running
			 * the application in gdb... */
			if (!feof(file) && ferror(file))
				input_error(0, 0, "read from input failed");
			return 0;
		}
		return s;
	} else {
		assert(input->kind == INPUT_STRING);
		size_t len = strlen(input->in.string);
		len = MIN(len, n);
		memcpy(read_buf, input->in.string, len);
		input->in.string += len;
		return len;
	}
}

static size_t decode_iso_8859_1(input_t *input, utf32 *buffer,
                                size_t buffer_size)
{
	unsigned char read_buf[buffer_size];
	size_t const s = read_block(input, read_buf, sizeof(read_buf));

	unsigned char const *src = read_buf;
	unsigned char const *end = read_buf + s;
	utf32               *dst = buffer;
	while (src != end)
		*dst++ = *src++;

	return s;
}

static size_t decode_iso_8859_15(input_t *input, utf32 *buffer,
                                 size_t buffer_size)
{
	unsigned char read_buf[buffer_size];
	size_t const s = read_block(input, read_buf, sizeof(read_buf));

	unsigned char const *src = read_buf;
	unsigned char const *end = read_buf + s;
	utf32               *dst = buffer;
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

	return s;
}

static void report_invalid_sequence(utf32 const *const bufstart,
                                    utf32 const *const pos)
{
	unsigned col = 0;

	/* estimate number of linefeeds */
	unsigned linefeeds = 0;
	for (utf32 const *c = bufstart; c < pos; ++c) {
		if (*c == (utf32)'\r') {
			if (c+1 < pos && *(c+1) == '\n')
				++c;
			goto linefeed;
		} else if (*c == (utf32)'\n') {
linefeed:
			++linefeeds;
			col = 1;
		} else {
			++col;
		}
	}

	/* report error */
	input_error(linefeeds, col, "invalid byte sequence in input");
}

#define UNICODE_REPLACEMENT_CHARACTER  0xFFFD

size_t input_decode_utf8(input_t *input, utf32 *buffer, size_t buffer_size)
{
	unsigned char read_buf[buffer_size];

	for (;;) {
		size_t const s = read_block(input, read_buf, sizeof(read_buf));
		if (s == 0) {
			if (input->utf8_part_decoded_rest_len > 0)
				input_error(0, 0, "incomplete input char at end of input");
			return 0;
		}

		unsigned char const *src = read_buf;
		unsigned char const *end = read_buf + s;
		utf32               *dst = buffer;
		utf32                decoded;
		utf32                min_code;

		if (input->utf8_part_decoded_rest_len != 0) {
			min_code              = input->utf8_part_decoded_min_code;
			decoded               = input->utf8_part_decoded_char;
			size_t const rest_len = input->utf8_part_decoded_rest_len;
			input->utf8_part_decoded_rest_len = 0;
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
					input->utf8_part_decoded_min_code = min_code;
					input->utf8_part_decoded_char     = decoded;
					input->utf8_part_decoded_rest_len = 1;
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
					report_invalid_sequence(buffer, dst);
					decoded = UNICODE_REPLACEMENT_CHARACTER;
				}
			} else if ((*src & 0xF0) == 0xE0) {
				min_code = 0x800;
				decoded  = *src++ & 0x0F;
two_more:
				if (src == end) {
					input->utf8_part_decoded_min_code = min_code;
					input->utf8_part_decoded_char     = decoded;
					input->utf8_part_decoded_rest_len = 2;
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
					input->utf8_part_decoded_min_code = min_code;
					input->utf8_part_decoded_char     = decoded;
					input->utf8_part_decoded_rest_len = 3;
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
				report_invalid_sequence(buffer, dst);
				decoded = UNICODE_REPLACEMENT_CHARACTER;
realign:
				do {
					++src;
					if (src == end) {
						input->utf8_part_decoded_rest_len = 4;
						break;
					}
				} while ((*src & 0xC0) == 0x80 || (*src & 0xF8) == 0xF8);
			}
			*dst++ = decoded;
		}

		/* we're done when we could read more than 1 char */
		if (buffer != dst)
			return dst - buffer;
	}
}

static size_t decode_windows_1252(input_t *input, utf32 *buffer,
                                  size_t buffer_size)
{
	unsigned char read_buf[buffer_size];
	size_t const s = read_block(input, read_buf, sizeof(read_buf));

	unsigned char const *src = read_buf;
	unsigned char const *end = read_buf + s;
	utf32               *dst = buffer;
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

	return s;
}

typedef struct named_decoder_t {
	char const       *name;
	input_decoder_t  *decoder;
} named_decoder_t;

static named_decoder_t const decoders[] = {
	{ "CP819",           decode_iso_8859_1   }, // official alias
	{ "IBM819",          decode_iso_8859_1   }, // official alias
	{ "ISO-8859-1",      decode_iso_8859_1   }, // official alias
	{ "ISO-8859-15",     decode_iso_8859_15  }, // official name
	{ "ISO8859-1",       decode_iso_8859_1   },
	{ "ISO8859-15",      decode_iso_8859_15  },
	{ "ISO_8859-1",      decode_iso_8859_1   }, // official alias
	{ "ISO_8859-15",     decode_iso_8859_15  }, // official alias
	{ "ISO_8859-1:1987", decode_iso_8859_1   }, // official name
	{ "Latin-9",         decode_iso_8859_15  }, // official alias
	{ "UTF-8",           input_decode_utf8   }, // official name
	{ "csISOLatin1",     decode_iso_8859_1   }, // official alias
	{ "cp1252",          decode_windows_1252 },
	{ "iso-ir-100",      decode_iso_8859_1   }, // official alias
	{ "l1",              decode_iso_8859_1   }, // official alias
	{ "latin1",          decode_iso_8859_1   }, // official alias
	{ "windows-1252",    decode_windows_1252 }, // official name

	{ NULL,              NULL                }
};

/** strcasecmp is not part of C99 so we need our own implementation here */
static bool strcaseeq(char const *s1, char const *s2)
{
	for (; tolower((unsigned char)*s1) == tolower((unsigned char)*s2); ++s1, ++s2) {
		if (*s1 == '\0')
			return true;
	}
	return false;
}

input_decoder_t *input_get_decoder(const char *encoding)
{
	for (named_decoder_t const *i = decoders; i->name != NULL; ++i) {
		if (strcaseeq(encoding, i->name))
			return i->decoder;
	}
	return NULL;
}

input_t *input_from_stream(FILE *file, input_decoder_t *decoder)
{
	input_t *result = XMALLOCZ(input_t);
	result->kind    = INPUT_FILE;
	result->in.file = file;
	result->decoder = decoder;
	return result;
}

FILE *input_get_file(const input_t *const input)
{
	if (input->kind != INPUT_FILE)
		return NULL;
	return input->in.file;
}

input_t *input_from_string(const char *string, input_decoder_t *decoder)
{
	input_t *result   = XMALLOCZ(input_t);
	result->kind      = INPUT_STRING;
	result->in.string = string;
	result->decoder   = decoder;
	return result;
}

size_t decode(input_t *input, utf32 *buffer, size_t buffer_size)
{
	return input->decoder(input, buffer, buffer_size);
}

void input_free(input_t *input)
{
	free(input);
}
