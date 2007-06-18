#ifndef PARSER_H
#define PARSER_H

#include "ast.h"

typedef struct environment_entry_t environment_entry_t;

void init_parser(void);
void exit_parser(void);

translation_unit_t *parse(void);

#endif
