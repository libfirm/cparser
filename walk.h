#ifndef WALK_STATEMENTS_H
#define WALK_STATEMENTS_H

#include "ast.h"

typedef void (*statement_callback)(statement_t*, void *env);
typedef void (*expression_callback)(expression_t*, void *env);
typedef void (*declaration_callback)(entity_t*, void *env);

void walk_translation_unit(translation_unit_t *unit,
                           declaration_callback,
						   statement_callback,
						   expression_callback,
						   void *env);

void walk_statements(statement_t*, statement_callback, void *env);

#endif
