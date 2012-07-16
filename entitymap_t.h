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
