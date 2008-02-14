#ifndef AST2FIRM_H
#define AST2FIRM_H

#include "ast.h"

void translation_unit_to_firm(translation_unit_t *unit);

void init_ast2firm(void);
void exit_ast2firm(void);

#endif
