/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef AST2FIRM_H
#define AST2FIRM_H

#include <libfirm/firm.h>
#include "ast.h"
#include "type.h"

void translation_unit_to_firm(translation_unit_t *unit);

void init_ast2firm(void);
void exit_ast2firm(void);

typedef ident* (*create_ld_ident_func)(entity_t *entity);

void set_create_ld_ident(create_ld_ident_func func);

ir_tarval *fold_constant_to_tarval(const expression_t *expression);
void determine_enum_values(enum_type_t *type);

extern fp_model_t firm_fp_model;
extern ir_mode *atomic_modes[ATOMIC_TYPE_LAST+1];

#endif
