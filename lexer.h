#ifndef LEXER_H
#define LEXER_H

#include "symbol_table_t.h"
#include "token_t.h"

typedef struct lexer_t lexer_t;

void lexer_next_token(lexer_t *lexer, token_t *token);

#endif
