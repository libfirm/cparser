/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef STRING_REP_H
#define STRING_REP_H

#include <stdlib.h>
#include "unicode.h"

enum string_encoding_t {
	STRING_ENCODING_CHAR,
	STRING_ENCODING_CHAR16,
	STRING_ENCODING_CHAR32,
	STRING_ENCODING_UTF8,
	STRING_ENCODING_WIDE
};
typedef enum string_encoding_t string_encoding_t;

typedef struct string_t {
	char const       *begin; /**< UTF-8 encoded string, the last character is guaranteed to be \0. */
	size_t            size;  /**< size of string in bytes (not characters), without terminating \0. */
	string_encoding_t encoding;
} string_t;

size_t get_string_len(string_t const *str);

#endif
