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

#endif
