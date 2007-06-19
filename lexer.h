#ifndef LEXER_H
#define LEXER_H

#include "symbol_table_t.h"
#include "token_t.h"

void lexer_next_token(token_t *token);

/* for debugging */
void lexer_next_preprocessing_token(token_t *token);

#endif
