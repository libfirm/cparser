/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef AST_H
#define AST_H

#include <stdio.h>
#include <stdbool.h>
#include "entity.h"

typedef struct expression_base_t                     expression_base_t;
typedef struct literal_expression_t                  literal_expression_t;
typedef struct string_literal_expression_t           string_literal_expression_t;
typedef struct funcname_expression_t                 funcname_expression_t;
typedef struct compound_literal_expression_t         compound_literal_expression_t;
typedef struct reference_expression_t                reference_expression_t;
typedef struct call_argument_t                       call_argument_t;
typedef struct call_expression_t                     call_expression_t;
typedef struct binary_expression_t                   binary_expression_t;
typedef struct unary_expression_t                    unary_expression_t;
typedef struct select_expression_t                   select_expression_t;
typedef struct array_access_expression_t             array_access_expression_t;
typedef struct typeprop_expression_t                 typeprop_expression_t;
typedef struct conditional_expression_t              conditional_expression_t;
typedef struct statement_expression_t                statement_expression_t;
typedef struct designator_t                          designator_t;
typedef struct offsetof_expression_t                 offsetof_expression_t;
typedef struct va_start_expression_t                 va_start_expression_t;
typedef struct va_arg_expression_t                   va_arg_expression_t;
typedef struct va_copy_expression_t                  va_copy_expression_t;
typedef struct builtin_constant_expression_t         builtin_constant_expression_t;
typedef struct builtin_types_compatible_expression_t builtin_types_compatible_expression_t;
typedef struct classify_type_expression_t            classify_type_expression_t;
typedef struct label_address_expression_t            label_address_expression_t;
typedef union  expression_t                          expression_t;

typedef struct initializer_base_t                    initializer_base_t;
typedef struct initializer_list_t                    initializer_list_t;
typedef struct initializer_value_t                   initializer_value_t;
typedef struct initializer_designator_t              initializer_designator_t;
typedef union  initializer_t                         initializer_t;

typedef struct statement_base_t                      statement_base_t;
typedef struct compound_statement_t                  compound_statement_t;
typedef struct return_statement_t                    return_statement_t;
typedef struct if_statement_t                        if_statement_t;
typedef struct switch_statement_t                    switch_statement_t;
typedef struct declaration_statement_t               declaration_statement_t;
typedef struct expression_statement_t                expression_statement_t;
typedef struct computed_goto_statement_t             computed_goto_statement_t;
typedef struct goto_statement_t                      goto_statement_t;
typedef struct label_statement_t                     label_statement_t;
typedef struct case_label_statement_t                case_label_statement_t;
typedef struct do_while_statement_t                  do_while_statement_t;
typedef struct for_statement_t                       for_statement_t;
typedef struct asm_argument_t                        asm_argument_t;
typedef struct asm_clobber_t                         asm_clobber_t;
typedef struct asm_label_t                           asm_label_t;
typedef struct asm_statement_t                       asm_statement_t;
typedef struct ms_try_statement_t                    ms_try_statement_t;
typedef struct leave_statement_t                     leave_statement_t;
typedef union  statement_t                           statement_t;

typedef struct translation_unit_t                    translation_unit_t;

/**
 * Initialize the AST construction.
 */
void init_ast(void);

/**
 * Free the AST.
 */
void exit_ast(void);

void print_expression(const expression_t *expression);
void print_initializer(const initializer_t *initializer);
void print_ast(const translation_unit_t *unit);
void print_indent(void);
void print_declaration(const entity_t *entity);
void print_entity(const entity_t *entity);
void change_indent(int delta);

typedef enum expression_classification_t {
	EXPR_CLASS_VARIABLE,
	EXPR_CLASS_ERROR,
	EXPR_CLASS_CONSTANT
} expression_classification_t;

/**
 * Returns true when an initializer contains only constants/linker_constant
 * values.
 */
expression_classification_t is_constant_initializer(const initializer_t *initializer);

/**
 * Returns true if a given expression is a compile time
 * constant.
 *
 * @param expression  the expression to check
 */
expression_classification_t is_constant_expression(const expression_t *expression);

/**
 * Checks if an expression is a constant/known value to the linker. Examples:
 *  - all constant/linker constant expression casted to a pointer type
 *  - "&x", with x being a global variable.
 *  - "array" or "a.array" in case array is an array and array and a,
 *  respectively is an object with link time constant address
 */
expression_classification_t is_linker_constant(const expression_t *expression);

long fold_constant_to_int(const expression_t *expression);
bool fold_constant_to_bool(const expression_t *expression);
bool constant_is_negative(const expression_t *constant);

/**
 * the type of a literal is usually the biggest type that can hold the value.
 * Since this is backend dependent the parses needs this call exposed.
 * Works for EXPR_LITERAL_* expressions.
 */
void determine_literal_type(literal_expression_t *literal);

#endif
