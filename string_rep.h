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
#ifndef STRING_REP_H
#define STRING_REP_H

#include <assert.h>
#include <stdlib.h>

typedef struct string_t {
	const char *begin; /**< UTF-8 encoded string, the last character is
						* guaranteed to be 0 */
	size_t      size;  /**< size of string in bytes (not characters) */
} string_t;

typedef unsigned int utf32;
#define UTF32_PRINTF_FORMAT "%u"

/**
 * "parse" an utf8 character from a string.
 * Warning: This function only works for valid utf-8 inputs. The behaviour
 * is undefined for invalid utf-8 input.
 *
 * @param p    A pointer to a pointer into the string. The pointer
 *             is incremented for each consumed char
 */
static inline utf32 read_utf8_char(const char **p)
{
	const unsigned char *c      = (const unsigned char *) *p;
	utf32                result;

	if ((*c & 0x80) == 0) {
		/* 1 character encoding: 0b0??????? */
 		result = *c++;
	} else if ((*c & 0xE0) == 0xC0) {
		/* 2 character encoding: 0b110?????, 0b10?????? */
		result = *c++ & 0x1F;
		result = (result << 6) | (*c++ & 0x3F);
	} else if ((*c & 0xF0) == 0xE0) {
		/* 3 character encoding: 0b1110????, 0b10??????, 0b10?????? */
		result = *c++ & 0x0F;
		result = (result << 6) | (*c++ & 0x3F);
		result = (result << 6) | (*c++ & 0x3F);
	} else {
		/* 4 character enc.: 0b11110???, 0b10??????, 0b10??????, 0b10?????? */
		assert((*c & 0xF8) == 0xF0);
		result = *c++ & 0x07;
		result = (result << 6) | (*c++ & 0x3F);
		result = (result << 6) | (*c++ & 0x3F);
		result = (result << 6) | (*c++ & 0x3F);
	}

	*p = (const char*) c;
	return result;
}

static inline size_t wstrlen(const string_t *string)
{
	size_t      result = 0;
	const char *p      = string->begin;
	const char *end    = p + string->size;
	while (p < end) {
		read_utf8_char(&p);
		++result;
	}
	return result;
}

#endif
