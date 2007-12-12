#ifndef STRING_REP_H
#define STRING_REP_H

#include <wchar.h>

typedef wchar_t wchar_rep_t;

typedef struct string_t {
	const char *begin;
	size_t      size;
} string_t;

typedef struct wide_string_t {
	const wchar_rep_t *begin;
	size_t             size;
} wide_string_t;

#endif
