/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef TOKEN_T_H
#define TOKEN_T_H

#include <stdbool.h>
#include <stdio.h>

#include "adt/util.h"
#include "ast/position.h"
#include "ast/string_rep.h"
#include "ast/symbol.h"

typedef enum token_kind_t {
	T_NULL  =  0,
#define T(mode, x, str, val, is_keyword) x val,
#include "tokens.h"
#undef T
	T_LAST_TOKEN
} token_kind_t;

typedef enum pp_token_kind_t {
	TP_NULL = 0,
#define T(token) TP_##token,
#include "tokens_preprocessor.h"
#undef T
	TP_LAST_TOKEN
} pp_token_kind_t;

extern symbol_t *token_symbols[];

typedef struct token_base_t      token_base_t;
typedef struct literal_t         literal_t;
typedef struct macro_parameter_t macro_parameter_t;
typedef union  token_t           token_t;

struct token_base_t {
	ENUMBF(token_kind_t) kind : 16;
	/** there was whitespace (no newline) before the token. Used for
	 * saving macro expansion lists */
	bool         space_before        : 1;
	/** token mustn't be expanded further */
	bool         expansion_forbidden : 1;
	position_t   pos;
	symbol_t    *symbol;
};

struct literal_t {
	token_base_t base;
	string_t    *string;
};

struct macro_parameter_t {
	token_base_t     base;
	pp_definition_t *def;
};

union token_t {
	ENUMBF(token_kind_t) kind : 16;
	token_base_t      base;
	literal_t         literal;
	macro_parameter_t macro_parameter;
};

char const *get_string_encoding_prefix(string_encoding_t);

void init_tokens(void);
void exit_tokens(void);
void print_token_kind(FILE *out, token_kind_t token_kind);
void print_token(FILE *out, const token_t *token, bool simple);

/**
 * returns true if pasting 2 preprocessing tokens next to each other
 * without a space in between would generate (an)other preprocessing token(s)
 */
bool tokens_would_paste(token_kind_t token1, token_kind_t token2);

#endif
