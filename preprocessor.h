#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "token_t.h"

void init_preprocessor(void);

void exit_preprocessor(void);

typedef struct searchpath_entry_t searchpath_entry_t;

/** Switch input to another file. The current token is not changed. */
void switch_pp_input(FILE *file, char const *filename, searchpath_entry_t *entry);

FILE *close_pp_input(void);

void next_preprocessing_token(void);

string_t make_string(char const *string);

extern bool    allow_dollar_in_symbol;
extern token_t pp_token;

void set_preprocessor_output(FILE *output);
void emit_pp_token(void);
void check_unclosed_conditionals(void);

#endif
