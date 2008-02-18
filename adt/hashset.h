/*
 * This file is part of cparser.
 * Copyright (C) 2007-2008 Matthias Braun <matze@braunis.de>
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

/**
 * @file
 * @date    16.03.2007
 * @brief   Generic hashset functions
 * @author  Matthias Braun
 * @version $Id$
 */

/* You have to specialize this header by defining HashSet, HashSetIterator and
 * ValueType */
#ifdef HashSet

#include <stdlib.h>

#ifdef DO_REHASH
#define HashSetEntry ValueType
#else
typedef struct HashSetEntry {
	ValueType data;
	unsigned hash;
} HashSetEntry;
#endif

#ifndef NO_TYPEDEFS
typedef struct HashSet         HashSet;
typedef struct HashSetIterator HashSetIterator;
#endif

struct HashSet {
	HashSetEntry *entries;
	size_t num_buckets;
	size_t enlarge_threshold;
	size_t shrink_threshold;
	size_t num_elements;
	size_t num_deleted;
	int consider_shrink;
#ifndef NDEBUG
	unsigned entries_version;
#endif
#ifdef ADDITIONAL_DATA
	ADDITIONAL_DATA;
#endif
};

struct HashSetIterator {
	HashSetEntry *current_bucket;
	HashSetEntry *end;
#ifndef NDEBUG
	const HashSet *set;
	unsigned entries_version;
#endif
};

#ifdef DO_REHASH
#undef HashSetEntry
#endif

#endif
