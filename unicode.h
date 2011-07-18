#ifndef UNICODE_H
#define UNICODE_H

#include <assert.h>

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

#endif
