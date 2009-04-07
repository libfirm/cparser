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
#ifndef SYMBOL_TABLE_T_H
#define SYMBOL_TABLE_T_H

#include "symbol_table.h"
#include "symbol.h"

#define HashSet          symbol_table_t
#define HashSetIterator  symbol_table_iterator_t
#define HashSetEntry     symbol_table_hash_entry_t
#define ValueType        symbol_t*
#include "adt/hashset.h"
#undef ValueType
#undef HashSetEntry
#undef HashSetIterator
#undef HashSet

typedef struct symbol_table_iterator_t  symbol_table_iterator_t;
typedef struct symbol_table_t           symbol_table_t;

#endif
