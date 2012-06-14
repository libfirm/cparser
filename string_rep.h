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

#include <stdlib.h>
#include "unicode.h"

enum string_encoding_t {
	STRING_ENCODING_CHAR,
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
