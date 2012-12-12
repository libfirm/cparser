/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef ENTITYMAP_T_H
#define ENTITYMAP_T_H

#include <libfirm/firm_types.h>
#include "symbol.h"

typedef struct entitymap_entry_t {
	symbol_t  *symbol;
	ir_entity *entity;
} entitymap_entry_t;

#define HashSet          entitymap_t
#define ValueType        entitymap_entry_t
#define DO_REHASH
#include "adt/hashset.h"
#undef DO_REHASH
#undef HashSetEntry
#undef HashSet

typedef struct entitymap_iterator_t  entitymap_iterator_t;
typedef struct entitymap_t           entitymap_t;

void entitymap_init(entitymap_t *map);

void entitymap_destroy(entitymap_t *map);

void entitymap_insert(entitymap_t *map, symbol_t *symbol, ir_entity *entity);

ir_entity *entitymap_get(const entitymap_t *map, symbol_t *symbol);

#endif
