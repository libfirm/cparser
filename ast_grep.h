#ifndef AST_GREP_H
#define AST_GREP_H

#include "ast.h"

expression_t *parse_grep_expression(translation_unit_t *context, const char *str);

void ast_grep(translation_unit_t *unit, expression_t *pattern);

#endif
