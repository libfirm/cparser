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

/* some builtin types */
extern type_t *type_wchar_t;
extern type_t *type_size_t;
extern type_t *type_ptrdiff_t;
extern type_t *type_wchar_ptr_t;

#endif
