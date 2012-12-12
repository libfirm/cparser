/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef SYMBOL_TABLE_T_H
#define SYMBOL_TABLE_T_H

#include "symbol_table.h"
#include "symbol.h"

#define HashSet          symbol_table_t
#define HashSetEntry     symbol_table_hash_entry_t
#define ValueType        symbol_t*
#include "adt/hashset.h"
#undef ValueType
#undef HashSetEntry
#undef HashSet

typedef struct symbol_table_iterator_t  symbol_table_iterator_t;
typedef struct symbol_table_t           symbol_table_t;

#endif
