/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
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

#include "adt/hashset.c.h"

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
