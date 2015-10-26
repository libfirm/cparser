/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef MANGLE_H
#define MANGLE_H

#include <libfirm/firm_types.h>

#include "ast/entity.h"

ident *create_name_elf(entity_t *entity);
ident *create_name_macho(entity_t *entity);
ident *create_name_win32(entity_t *entity);
ident *create_name_win64(entity_t *entity);

void init_mangle(void);
void exit_mangle(void);

#endif
