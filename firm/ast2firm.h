/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef AST2FIRM_H
#define AST2FIRM_H

#include <libfirm/firm.h>

#include "ast/ast.h"
#include "ast/type.h"

void translation_unit_to_firm(translation_unit_t *unit);

void init_ast2firm(void);
void exit_ast2firm(void);

typedef ident* (*create_ld_ident_func)(entity_t *entity);

void set_create_ld_ident(create_ld_ident_func func);

#endif
