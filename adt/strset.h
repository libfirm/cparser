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
#ifndef _FIRM_STRSET_H_
#define _FIRM_STRSET_H_

#define HashSet          strset_t
#define HashSetIterator  strset_iterator_t
#define HashSetEntry     strset_entry_t
#define ValueType        const char*
#include "hashset.h"
#undef ValueType
#undef HashSetEntry
#undef HashSetIterator
#undef HashSet

/**
 * Initializes a strset
 *
 * @param strset   Pointer to allocated space for the strset
 */
void strset_init(strset_t *strset);

/**
 * Initializes a strset
 *
 * @param strset                Pointer to allocated space for the strset
 * @param expected_elements   Number of elements expected in the strset (rougly)
 */
void strset_init_size(strset_t *strset, size_t expected_elements);

/**
 * Destroys a strset and frees the memory allocated for hashtable. The memory of
 * the strset itself is not freed.
 *
 * @param strset   Pointer to the strset
 */
void strset_destroy(strset_t *strset);

/**
 * Inserts a string into a strset.
 *
 * @param strset   Pointer to the strset
 * @param ptr      Pointer to insert into the strset
 * @returns        @p ptr if the string was inserted into the set,
 *                 otherwise a pointer to the string already in the set
 */
const char *strset_insert(strset_t *strset, const char *ptr);

/**
 * Removes an element from a strset. Does nothing if the strset doesn't contain the
 * element.
 *
 * @param strset   Pointer to the strset
 * @param ptr    Pointer to remove from the strset
 */
void strset_remove(strset_t *strset, const char *ptr);

/**
 * Tests whether a strset contains a pointer
 *
 * @param strset   Pointer to the strset
 * @param ptr    The pointer to test
 * @returns      1 @p strset contains the @p ptr, 0 otherwise
 */
const char* strset_find(const strset_t *strset, const char *ptr);

/**
 * Returns the number of pointers contained in the strset
 *
 * @param strset   Pointer to the strset
 * @returns      Number of pointers contained in the strset
 */
size_t strset_size(const strset_t *strset);

/**
 * Initializes a strset iterator. Sets the iterator before the first element in
 * the strset.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param strset       Pointer to the strset
 */
void strset_iterator_init(strset_iterator_t *iterator, const strset_t *strset);

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the strset have been processed.
 * @attention It is not allowed to use strset_insert or strset_remove while
 *            iterating over a strset.
 *
 * @param iterator  Pointer to the strset iterator.
 * @returns         Next element in the strset or NULL
 */
const char *strset_iterator_next(strset_iterator_t *iterator);

/**
 * Removes the string from the set that the iterator currently points to
 *
 * @param strset    Pointer to the strset
 * @param iter      Pointer to the iterator
 */
void strset_remove_iterator(strset_t *strset,
                            const strset_iterator_t *iterator);

#endif
