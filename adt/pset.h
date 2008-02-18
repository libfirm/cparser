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
 * @date    17.03.2007
 * @brief   A hashset that contains pointers
 * @author  Matthias Braun
 * @version $Id$
 */
#ifndef _FIRM_PSET_H_
#define _FIRM_PSET_H_

/* collides with libfirm... */
#if 0

#define HashSet          pset_t
#define HashSetIterator  pset_iterator_t
#define ValueType        void*
#define DO_REHASH
#include "hashset.h"
#undef DO_REHASH
#undef HashSet
#undef HashSetIterator
#undef ValueType

/**
 * Initializes a pset
 *
 * @param pset   Pointer to allocated space for the pset
 */
void pset_init(pset_t *pset);

/**
 * Initializes a pset
 *
 * @param pset                Pointer to allocated space for the pset
 * @param expected_elements   Number of elements expected in the pset (rougly)
 */
void pset_init_size(pset_t *pset, size_t expected_elements);

/**
 * Destroys a pset and frees the memory allocated for hashtable. The memory of
 * the pset itself is not freed.
 *
 * @param pset   Pointer to the pset
 */
void pset_destroy(pset_t *pset);

/**
 * Inserts an element into a pset.
 *
 * @param pset   Pointer to the pset
 * @param ptr    Pointer to insert into the pset
 * @returns      1 if the pointer was inserted, 0 if it was already there
 */
int pset_insert(pset_t *pset, void *ptr);

/**
 * Removes an element from a pset. Does nothing if the pset doesn't contain the
 * element.
 *
 * @param pset   Pointer to the pset
 * @param ptr    Pointer to remove from the pset
 */
void pset_remove(pset_t *pset, const void *ptr);

/**
 * Tests whether a pset contains a pointer
 *
 * @param pset   Pointer to the pset
 * @param ptr    The pointer to test
 * @returns      1 @p pset contains the @p ptr, 0 otherwise
 */
int pset_contains(const pset_t *pset, const void *ptr);

/**
 * Returns the number of pointers contained in the pset
 *
 * @param pset   Pointer to the pset
 * @returns      Number of pointers contained in the pset
 */
size_t pset_size(const pset_t *pset);

/**
 * Initializes a pset iterator. Sets the iterator before the first element in
 * the pset.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param pset       Pointer to the pset
 */
void pset_iterator_init(pset_iterator_t *iterator, const pset_t *pset);

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the pset have been processed.
 * @attention It is not allowed to use pset_insert or pset_remove while
 *            iterating over a pset; pset_remove_iter is allowed.
 *
 * @param iterator  Pointer to the pset iterator.
 * @returns         Next element in the pset or NULL
 */
void* pset_iterator_next(pset_iterator_t *iterator);

/**
 * Removes the element that the iterator currently points to from the hashset.
 *
 * @param pset      Pointer to the pset
 * @param iterator  Pointer to the iterator
 */
void pset_remove_iterator(pset_t *pset, const pset_iterator_t *iterator);
#endif

#endif
