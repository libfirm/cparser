#ifndef PARSER_T_H
#define PARSER_T_H

#include "parser.h"

extern token_t token;

typedef expression_t* (*parse_expression_function)(void);
void register_expression_parser(parse_expression_function parser,
                                int token_kind);

void parser_next_token();

expression_t *parse_expression(void);
type_t *parse_typename(void);

#endif
