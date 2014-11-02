/*
 * This file is part of cparser.
 * Copyright (C) 2012 Christoph Mallon <christoph.mallon@gmx.de>
 */
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

/**
 * Test two strings for equality, ignoring double underscores on the second.
 */
bool streq_underscore(const char *s1, const char *s2);

/**
 * Search for start of extension and optionally start of filename.
 */
char const *find_extension(char const *path, char const **name_out);

#endif
