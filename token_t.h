/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef TOKEN_T_H
#define TOKEN_T_H

#include <stdbool.h>
#include <stdio.h>

#include "string_rep.h"
#include "symbol.h"

typedef enum token_kind_tag_t {
	T_NULL  =  0,
#define T(mode,x,str,val) x val,
#include "tokens.inc"
#undef T
	T_LAST_TOKEN
} token_kind_tag_t;
typedef unsigned short token_kind_t;

typedef enum pp_token_kind_tag_t {
	TP_NULL = 0,
#define T(token) TP_##token,
#include "tokens_preprocessor.inc"
#undef T
	TP_LAST_TOKEN
} pp_token_kind_tag_t;
typedef unsigned short pp_token_kind_t;

typedef struct position_t position_t;
struct position_t {
	const char *input_name;
	unsigned    lineno;
	unsigned    colno            : 31;
	unsigned    is_system_header : 1;
};

extern symbol_t *token_symbols[];

/* position used for "builtin" declarations/types */
extern const position_t builtin_position;

typedef struct token_base_t      token_base_t;
typedef struct literal_t         literal_t;
typedef struct macro_parameter_t macro_parameter_t;
typedef union  token_t           token_t;

struct token_base_t {
	token_kind_t kind;
	/** there was whitespace before the token */
	bool         space_before        : 1;
	/** token mustn't be expanded further */
	bool         expansion_forbidden : 1;
	position_t   pos;
	symbol_t    *symbol;
};

struct literal_t {
	token_base_t base;
	string_t     string;
};

struct macro_parameter_t {
	token_base_t     base;
	pp_definition_t *def;
};

union token_t {
	token_kind_t      kind;
	token_base_t      base;
	literal_t         literal;
	macro_parameter_t macro_parameter;
};

char const *get_string_encoding_prefix(string_encoding_t);

void init_tokens(void);
void exit_tokens(void);
void print_token_kind(FILE *out, token_kind_t token_kind);
void print_token(FILE *out, const token_t *token);

/**
 * returns true if pasting 2 preprocessing tokens next to each other
 * without a space in between would generate (an)other preprocessing token(s)
 */
bool tokens_would_paste(token_kind_t token1, token_kind_t token2);

#endif
