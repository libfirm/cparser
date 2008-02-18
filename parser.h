/*
 * This file is part of cparser.
 * Copyright (C) 2007-2008 Matthias Braun <matze@braunis.de>
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
#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "type.h"

typedef struct environment_entry_t environment_entry_t;

void init_parser(void);
void exit_parser(void);

translation_unit_t *parse(void);

type_t *revert_automatic_type_conversion(const expression_t *expression);
declaration_t *expr_is_variable(const expression_t *expression);

#endif
