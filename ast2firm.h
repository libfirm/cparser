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
#ifndef AST2FIRM_H
#define AST2FIRM_H

#include <libfirm/firm_types.h>
#include "ast.h"
#include "type.h"

void translation_unit_to_firm(translation_unit_t *unit);

void init_ast2firm(void);
void exit_ast2firm(void);

ir_mode *get_atomic_mode(atomic_type_kind_t kind);
ir_type *get_ir_type(type_t *type);

typedef ident* (*create_ld_ident_func)(entity_t *entity);

void set_create_ld_ident(create_ld_ident_func func);

#endif
