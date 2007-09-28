#ifndef AST_H
#define AST_H

#include <stdio.h>

typedef struct context_t                    context_t;

typedef struct expression_t                 expression_t;
typedef struct const_t                      const_t;
typedef struct string_literal_t             string_literal_t;
typedef struct reference_expression_t       reference_expression_t;
typedef struct cast_expression_t            cast_expression_t;
typedef struct call_argument_t              call_argument_t;
typedef struct type_argument_t              type_argument_t;
typedef struct call_expression_t            call_expression_t;
typedef struct binary_expression_t          binary_expression_t;
typedef struct unary_expression_t           unary_expression_t;
typedef struct select_expression_t          select_expression_t;
typedef struct array_access_expression_t    array_access_expression_t;
typedef struct sizeof_expression_t          sizeof_expression_t;
typedef struct conditional_expression_t     conditional_expression_t;
typedef struct expression_list_element_t    expression_list_element_t;
typedef struct comma_expression_t           comma_expression_t;
typedef struct statement_expression_t       statement_expression_t;
typedef struct member_designator_t          member_designator_t;
typedef struct offsetof_expression_t        offsetof_expression_t;
typedef struct va_arg_expression_t          va_arg_expression_t;
typedef struct builtin_symbol_expression_t  builtin_symbol_expression_t;

typedef struct declaration_t                declaration_t;

typedef struct statement_t                  statement_t;
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

typedef struct translation_unit_t           translation_unit_t;
typedef struct method_t                     method_t;
typedef struct global_variable_t            global_variable_t;

void  init_ast(void);
void  exit_ast(void);

void  ast_set_output(FILE *out);
void  print_expression(const expression_t *expression);
void  print_ast(const translation_unit_t *unit);
void *allocate_ast(size_t size);

#endif
