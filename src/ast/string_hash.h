/*
 * This file is part of cparser.
 * Copyright (C) 2014 Matthias Braun <matze@braunis.de>
 */
#ifndef STRING_HASH_H
#define STRING_HASH_H

#include "adt/obst.h"
#include "string_rep.h"

extern struct obstack string_obst;

void init_string_hash(void);
void exit_string_hash(void);

void begin_string_construction(void);
void abort_string_construction(void);
string_t *finish_string_construction(string_encoding_t encoding);

void begin_string_construction_on(struct obstack *obst);
string_t *finish_string_construction_on(struct obstack *on,
                                        string_encoding_t encoding);

#endif
