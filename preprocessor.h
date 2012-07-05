#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "token_t.h"

void init_preprocessor(void);

void exit_preprocessor(void);

/** Switch input to another file. The current token is not changed. */
void switch_input(FILE *file, char const *filename);

FILE* close_input(void);

void next_preprocessing_token(void);

string_t make_string(char const *string);

extern bool    allow_dollar_in_symbol;
extern token_t pp_token;

void set_preprocessor_output(FILE *output);
void emit_pp_token(void);
void check_unclosed_conditionals(void);

#endif
