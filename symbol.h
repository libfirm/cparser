/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef SYMBOL_H
#define SYMBOL_H

typedef struct symbol_t         symbol_t;
typedef struct pp_definition_t  pp_definition_t;

/** special symbol used for anonymous/error entities. */
extern symbol_t *sym_anonymous;

#endif
