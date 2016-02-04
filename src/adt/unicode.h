/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef UNICODE_H
#define UNICODE_H

#include <assert.h>

#include "adt/obst.h"

typedef unsigned int utf32;
#define UTF32_PRINTF_FORMAT "%u"
#define UTF32_EOF           ((utf32)-1)

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

static inline void obstack_grow_utf8(struct obstack *const obst, utf32 const c)
{
	if (c < 0x80U) {
		obstack_1grow(obst, c);
	} else if (c < 0x800) {
		obstack_1grow(obst, 0xC0 |  (c >>  6));
		goto one_more;
	} else if (c < 0x10000) {
		obstack_1grow(obst, 0xE0 |  (c >> 12));
		goto two_more;
	} else {
		assert(c < 0x200000);
		obstack_1grow(obst, 0xF0 |  (c >> 18));
		obstack_1grow(obst, 0x80 | ((c >> 12) & 0x3F));
two_more:
		obstack_1grow(obst, 0x80 | ((c >>  6) & 0x3F));
one_more:
		obstack_1grow(obst, 0x80 | ( c        & 0x3F));
	}
}

#endif
