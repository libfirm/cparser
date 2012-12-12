/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "symbol.h"
#include "adt/obst.h"

symbol_t *symbol_table_insert(const char *string);

void init_symbol_table(void);
void exit_symbol_table(void);

extern struct obstack symbol_obstack;

#endif
