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

#include "adt/strutil.h"
#include "symbol_table_t.h"
#include "symbol_t.h"
#include "token_t.h"
#include "adt/hash_string.h"
#include "adt/obst.h"

struct obstack symbol_obstack;

static inline
void init_symbol_table_entry(symbol_t *entry, const char *string)
{
	entry->string        = string;
	entry->ID            = T_IDENTIFIER;
	entry->pp_ID         = TP_IDENTIFIER;
	entry->entity        = NULL;
	entry->pp_definition = NULL;
}

#define HashSet                    symbol_table_t
#define HashSetIterator            symbol_table_iterator_t
#define HashSetEntry               symbol_table_hash_entry_t
#define ValueType                  symbol_t*
#define NullValue                  NULL
#define DeletedValue               ((symbol_t*)-1)
#define KeyType                    const char *
#define ConstKeyType               const char *
#define GetKey(value)              (value)->string
#define InitData(this,value,key)   ((void)((value) = (ValueType)obstack_alloc(&symbol_obstack, sizeof(symbol_t)), init_symbol_table_entry((value), key)))
#define Hash(this, key)            hash_string(key)
#define KeysEqual(this,key1,key2)  (streq(key1, key2))
#define SetRangeEmpty(ptr,size)    memset(ptr, 0, (size) * sizeof(symbol_table_hash_entry_t))
#define SCALAR_RETURN

#define hashset_init            _symbol_table_init
#define hashset_init_size       _symbol_table_init_size
#define hashset_destroy         _symbol_table_destroy
#define hashset_insert          _symbol_table_insert
#define hashset_remove          _symbol_table_remove
#define hashset_find            _symbol_table_find
#define hashset_size            _symbol_table_size
#define hashset_iterator_init   _symbol_table_iterator_init
#define hashset_iterator_next   _symbol_table_iterator_next
#define hashset_remove_iterator _symbol_table_remove_iterator

#include "adt/hashset.c"

static symbol_table_t  symbol_table;

symbol_t *symbol_table_insert(const char *string)
{
	return _symbol_table_insert(&symbol_table, string);
}

void init_symbol_table(void)
{
	obstack_init(&symbol_obstack);
	_symbol_table_init(&symbol_table);
}

void exit_symbol_table(void)
{
	_symbol_table_destroy(&symbol_table);
	obstack_free(&symbol_obstack, NULL);
}
