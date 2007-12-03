#ifndef AST2FIRM_H
#define AST2FIRM_H

#include "ast.h"

void translation_unit_to_firm(translation_unit_t *unit);

void init_ast2firm(void);
void exit_ast2firm(void);

const char *dbg_retrieve(const dbg_info *dbg, unsigned *line);
ir_node *uninitialized_local_var(ir_graph *irg, ir_mode *mode, int pos);
unsigned dbg_snprint(char *buf, unsigned len, const dbg_info *dbg);

#endif
