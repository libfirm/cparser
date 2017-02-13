/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef MANGLE_H
#define MANGLE_H

#include <libfirm/firm_types.h>

#include "ast/entity.h"

ident *create_ld_ident(entity_t const *entity);

void init_mangle(void);
void exit_mangle(void);

#endif
