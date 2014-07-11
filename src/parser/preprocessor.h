/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "input.h"
#include "token_t.h"
#include "ast/position.h"

/** initialize just enough so we can use add_define(),add_define_string(),
 * undefine(), append_include_path(), append_env_paths() for commandline
 * parsing */
void init_preprocessor(void);
void setup_preprocessor(void);

void exit_preprocessor(void);

typedef struct searchpath_entry_t searchpath_entry_t;

/** Switch input to another file. The current token is not changed. */
void switch_pp_input(input_t *decoder, char const *input_name,
                     searchpath_entry_t *entry, bool is_system_header);
void close_pp_input(void);

/** print the header displayed by gcc when writing preprocessing tokens */
void print_pp_header(void);

void next_preprocessing_token(void);

/**
 * @param standard_define    The definition is mentioned as predefined macro
 *                           in the C standard (so we issue warnings/errors
 *                           on #undef and redefinition)
 */
void add_define(char const *name, char const *val, bool standard_define);
void add_define_string(char const *name, char const *val, bool standard_define);
void add_define_macro(char const *name, char const *macro_arg,
                      char const *val, bool standard_define);
void parse_define(char const *string);
void undefine(char const *name);

string_t *make_string(char const *string);

extern bool             no_dollar_in_symbol;
extern token_t          pp_token;
extern input_decoder_t *input_decoder;

void set_preprocessor_output(FILE *output);
void emit_pp_token(void);
void check_unclosed_conditionals(void);

typedef struct searchpath_t searchpath_t;
extern searchpath_t bracket_searchpath; /**< paths for < > includes */
extern searchpath_t quote_searchpath;   /**< paths for " " includes */
extern searchpath_t system_searchpath;  /**< system searchpath (appended to
                                             quote searchpath) */
extern searchpath_t after_searchpath;   /**< searchpath for -idirafter (also system paths) */

void append_include_path(searchpath_t *searchpath, const char *path);
void append_env_paths(searchpath_t *paths, const char *envvar);
void print_include_paths(void);
void print_defines(void);

#endif
