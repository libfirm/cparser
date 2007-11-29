#ifndef STRING_REP_H
#define STRING_REP_H

#include <wchar.h>

typedef wchar_t wchar_rep_t;

#if 0 /* TODO */
typedef struct string_t {
	const char *begin;
	const char *end;
} string_t;
#endif

typedef struct wide_string_t {
	const wchar_rep_t *begin;
	size_t             size;
} wide_string_t;

#endif
