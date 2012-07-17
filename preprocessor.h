#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "token_t.h"

void init_preprocessor(void);

void exit_preprocessor(void);

typedef struct searchpath_entry_t searchpath_entry_t;

/** Switch input to another file. The current token is not changed. */
void switch_pp_input(FILE *file, char const *filename, searchpath_entry_t *entry, bool is_system_header);

FILE *close_pp_input(void);

void next_preprocessing_token(void);

string_t make_string(char const *string);

extern bool    allow_dollar_in_symbol;
extern token_t pp_token;

void set_preprocessor_output(FILE *output);
void emit_pp_token(void);
void check_unclosed_conditionals(void);

typedef struct searchpath_t searchpath_t;
extern searchpath_t bracket_searchpath; /**< paths for < > includes */
extern searchpath_t quote_searchpath;   /**< paths for " " includes */
extern searchpath_t system_searchpath;  /**< system searchpath (appended to
                                             quote searchpath) */
extern searchpath_t after_searchpath;   /**< searchpath for -idirafter (also system paths) */

void init_include_paths(void);
void append_include_path(searchpath_t *searchpath, const char *path);

#endif
