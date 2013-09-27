/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef SYMBOL_T_H
#define SYMBOL_T_H

#include "entity.h"
#include "symbol.h"
#include "token_t.h"

struct symbol_t {
	char const      *string;
	token_kind_t     ID;
	pp_token_kind_t  pp_ID;
	entity_t        *entity;
	pp_definition_t *pp_definition;
};

#endif
