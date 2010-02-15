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
#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "type.h"

typedef struct environment_entry_t environment_entry_t;

/**
 * Initialize parser. Should be called once when the program starts
 */
void init_parser(void);
/**
 * Frees resources occupied by parser. Should be called once before the program
 * exits.
 */
void exit_parser(void);

/**
 * Start parsing a new compilation unit
 */
void start_parsing(void);

/**
 * Parse input. The source of the input is determined by the lexer module
 */
void parse(void);

/**
 * Finish parsing a complete compilation unit and return the AST.
 */
translation_unit_t *finish_parsing(void);

type_t   *revert_automatic_type_conversion(const expression_t *expression);
entity_t *expression_is_variable(const expression_t *expression);

void prepare_main_collect2(entity_t *entity);

entity_t *record_entity(entity_t *entity, bool is_definition);

#endif
