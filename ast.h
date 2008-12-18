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
#ifndef AST_H
#define AST_H

#include <stdio.h>
#include <stdbool.h>
#include "entity.h"

typedef struct expression_base_t                expression_base_t;
typedef struct const_expression_t               const_expression_t;
typedef struct string_literal_expression_t      string_literal_expression_t;
typedef struct funcname_expression_t            funcname_expression_t;
typedef struct wide_string_literal_expression_t wide_string_literal_expression_t;
typedef struct compound_literal_expression_t    compound_literal_expression_t;
typedef struct reference_expression_t           reference_expression_t;
typedef struct cast_expression_t                cast_expression_t;
typedef struct call_argument_t                  call_argument_t;
typedef struct type_argument_t                  type_argument_t;
typedef struct call_expression_t                call_expression_t;
typedef struct binary_expression_t              binary_expression_t;
typedef struct unary_expression_t               unary_expression_t;
typedef struct select_expression_t              select_expression_t;
typedef struct array_access_expression_t        array_access_expression_t;
typedef struct typeprop_expression_t            typeprop_expression_t;
typedef struct conditional_expression_t         conditional_expression_t;
typedef struct expression_list_element_t        expression_list_element_t;
typedef struct comma_expression_t               comma_expression_t;
typedef struct statement_expression_t           statement_expression_t;
typedef struct designator_t                     designator_t;
typedef struct offsetof_expression_t            offsetof_expression_t;
typedef struct va_start_expression_t            va_start_expression_t;
typedef struct va_arg_expression_t              va_arg_expression_t;
typedef struct builtin_symbol_expression_t      builtin_symbol_expression_t;
typedef struct builtin_constant_expression_t    builtin_constant_expression_t;
typedef struct builtin_prefetch_expression_t    builtin_prefetch_expression_t;
typedef struct classify_type_expression_t       classify_type_expression_t;
typedef struct bitfield_extract_expression_t    bitfield_extract_expression_t;
typedef struct label_address_expression_t       label_address_expression_t;
typedef union  expression_t                     expression_t;

typedef struct initializer_base_t           initializer_base_t;
typedef struct initializer_list_t           initializer_list_t;
typedef struct initializer_value_t          initializer_value_t;
typedef struct initializer_string_t         initializer_string_t;
typedef struct initializer_wide_string_t    initializer_wide_string_t;
typedef struct initializer_designator_t     initializer_designator_t;
typedef union  initializer_t                initializer_t;

typedef struct statement_base_t             statement_base_t;
typedef struct invalid_statement_t          invalid_statement_t;
typedef struct empty_statement_t            empty_statement_t;
typedef struct compound_statement_t         compound_statement_t;
typedef struct return_statement_t           return_statement_t;
typedef struct if_statement_t               if_statement_t;
typedef struct switch_statement_t           switch_statement_t;
typedef struct declaration_statement_t      declaration_statement_t;
typedef struct expression_statement_t       expression_statement_t;
typedef struct goto_statement_t             goto_statement_t;
typedef struct label_statement_t            label_statement_t;
typedef struct case_label_statement_t       case_label_statement_t;
typedef struct while_statement_t            while_statement_t;
typedef struct do_while_statement_t         do_while_statement_t;
typedef struct for_statement_t              for_statement_t;
typedef struct asm_argument_t               asm_argument_t;
typedef struct asm_clobber_t                asm_clobber_t;
typedef struct asm_statement_t              asm_statement_t;
typedef struct ms_try_statement_t           ms_try_statement_t;
typedef struct leave_statement_t            leave_statement_t;
typedef union  statement_t                  statement_t;

typedef struct translation_unit_t           translation_unit_t;

void  init_ast(void);
void  exit_ast(void);

void  ast_set_output(FILE *out);
void  print_expression(const expression_t *expression);
void  print_initializer(const initializer_t *initializer);
void  print_ast(const translation_unit_t *unit);
void  print_indent(void);
void  print_declaration(const entity_t *entity);
void  print_entity(const entity_t *entity);
void  change_indent(int delta);
void *allocate_ast(size_t size);

/**
 * Returns true if a given expression is a compile time
 * constant.
 *
 * @param expression  the expression to check
 */
bool is_constant_initializer(const initializer_t *initializer);

/**
 * Returns true if a given expression is a compile time
 * constant.
 *
 * @param expression  the expression to check
 */
bool is_constant_expression(const expression_t *expression);

bool is_address_constant(const expression_t *expression);

long fold_constant(const expression_t *expression);

#endif
