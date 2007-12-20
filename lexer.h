#ifndef LEXER_H
#define LEXER_H

#include "symbol_table_t.h"
#include "token_t.h"

extern token_t lexer_token;

void lexer_next_token(void);

/* for debugging */
void lexer_next_preprocessing_token(void);

void init_lexer(void);
void exit_lexer(void);

void lexer_open_stream(FILE *stream, const char *input_name);

string_t      concat_strings(           const string_t      *s1, const string_t      *s2);
wide_string_t concat_string_wide_string(const string_t      *s1, const wide_string_t *s2);
wide_string_t concat_wide_strings(      const wide_string_t *s1, const wide_string_t *s2);
wide_string_t concat_wide_string_string(const wide_string_t *s1, const string_t      *s2);

#endif
