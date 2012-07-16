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
#include <config.h>

#include "entitymap_t.h"

static entitymap_entry_t null_entitymap_entry = { NULL, NULL };

static unsigned hash_ptr(const void *ptr)
{
	unsigned ptr_int = ((char*) ptr - (char*) NULL);
	return ptr_int >> 3;
}

#define DO_REHASH
#define HashSet                   entitymap_t
#define ValueType                 entitymap_entry_t
#define NullValue                 null_entitymap_entry
#define KeyType                   symbol_t*
#define ConstKeyType              const symbol_t*
#define GetKey(value)             (value).symbol
#define InitData(self,value,key)  (value).symbol = (key)
#define Hash(self,key)            hash_ptr(key)
#define KeysEqual(self,key1,key2) (key1) == (key2)
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof((ptr)[0]))
#define EntrySetEmpty(value)      (value).symbol = NULL
#define EntrySetDeleted(value)    (value).symbol = (symbol_t*) -1
#define EntryIsEmpty(value)       ((value).symbol == NULL)
#define EntryIsDeleted(value)     ((value).symbol == (symbol_t*)-1)

#define hashset_init            entitymap_init
#define hashset_destroy         entitymap_destroy
entitymap_entry_t *_entitymap_insert(entitymap_t *map, symbol_t *symbol);
#define hashset_insert          _entitymap_insert
entitymap_entry_t *_entitymap_find(const entitymap_t *map, const symbol_t *symbol);
#define hashset_find            _entitymap_find

#include "adt/hashset.c.inl"

ir_entity *entitymap_get(const entitymap_t *map, symbol_t *symbol)
{
	entitymap_entry_t *entry = _entitymap_find(map, symbol);
	return entry->entity;
}

void entitymap_insert(entitymap_t *map, symbol_t *symbol, ir_entity *entity)
{
	entitymap_entry_t *entry = _entitymap_insert(map, symbol);
	entry->entity = entity;
}
