#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "symbol.h"
#include "adt/obst.h"

symbol_t *symbol_table_insert(const char *symbol);
symbol_t *preprocessor_symbol_table_insert(const char *symbol);
symbol_t *preprocessor_symbol_table_find(const char *symbol);

void init_symbol_table(void);
void exit_symbol_table(void);

extern struct obstack symbol_obstack;

#endif
