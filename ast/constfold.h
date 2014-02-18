#ifndef CONSTFOLD_H
#define CONSTFOLD_H

#include <libfirm/firm_types.h>
#include "ast/ast_t.h"
#include "ast/type.h"

bool fold_expression_to_bool(const expression_t *expression);
long fold_expression_to_int(const expression_t *expression);
bool folded_expression_is_negative(const expression_t *expression);

ir_tarval *fold_expression(const expression_t *expression);
ir_tarval *get_enum_value(const enum_value_t *enum_value);

void init_constfold(void);

#endif
