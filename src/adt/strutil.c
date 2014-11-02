/*
 * This file is part of cparser.
 * Copyright (C) 2012 Christoph Mallon <christoph.mallon@gmx.de>
 */
#include "strutil.h"

bool streq_underscore(const char *const s1, const char *const s2)
{
	char const* const middle = strstart(s2, "__");
	if (middle) {
		char const* const rest = strstart(middle, s1);
		if (rest && streq(rest, "__"))
			return true;
	}

	return streq(s1, s2);
}

char const *find_extension(char const *path, char const **const name_out)
{
	char const *name = path;
	char const *ext  = NULL;
	for (char const *i = path;; ++i) {
		switch (*i) {
		case '\0':
			if (!ext)
				ext = i;
			if (name_out)
				*name_out = name;
			return ext;

		case '.':
			ext = i;
			break;

		case '/':
			name = i + 1;
			ext  = NULL;
			break;
		}
	}
}
