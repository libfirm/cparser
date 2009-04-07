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
#ifndef _FIRM_HASH_STRING_H_
#define _FIRM_HASH_STRING_H_

#define _FIRM_FNV_OFFSET_BASIS 2166136261U
#define _FIRM_FNV_FNV_PRIME 16777619U

static inline __attribute__((pure))
unsigned hash_string(const char* str)
{
	const unsigned char *p;
	unsigned hash = _FIRM_FNV_OFFSET_BASIS;

	for(p = (const unsigned char*) str; *p != 0; ++p) {
		hash *= _FIRM_FNV_FNV_PRIME;
		hash ^= *p;
	}

	return hash;
}

static inline __attribute__((pure))
unsigned hash_string_size(const char* str, size_t size)
{
	size_t i;
	unsigned hash = _FIRM_FNV_OFFSET_BASIS;

	for(i = 0; i < size; ++i) {
		hash *= _FIRM_FNV_FNV_PRIME;
		hash ^= str[i];
	}

	return hash;
}

#endif
