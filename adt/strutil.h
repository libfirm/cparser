#ifndef STRUTIL_H
#define STRUTIL_H

#include <stdbool.h>
#include <string.h>

static inline bool streq(char const* a, char const* b)
{
	return strcmp(a, b) == 0;
}

static inline char const* strstart(char const* str, char const* start)
{
	do {
		if (*start == '\0')
			return str;
	} while (*str++ == *start++);
	return NULL;
}

#endif
