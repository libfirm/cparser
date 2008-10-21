#ifndef WALK_STATEMENTS_H
#define WALK_STATEMENTS_H

#include "ast.h"

typedef void (*statement_callback)(statement_t*, void *env);

void walk_statements(statement_t*, statement_callback, void *env);

#endif
