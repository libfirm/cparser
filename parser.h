#ifndef PARSER_H
#define PARSER_H

#include "ast.h"

void init_parser(void);
void exit_parser(void);

translation_unit_t *parse(void);

#endif
