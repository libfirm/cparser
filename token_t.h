#ifndef TOKEN_T_H
#define TOKEN_T_H

#include <stdio.h>
#include "symbol.h"
#include "symbol_table.h"

typedef enum {
#define T(x,str,val) T_##x val,
#define TS(x,str,val) T_##x val,
#include "tokens.inc"
#undef TS
#undef T

	T_EOF   = -1,
	T_ERROR = -2
} token_type_t;

typedef struct {
	int type;
	union {
		symbol_t   *symbol;
		int         intvalue;
		const char *string;
	} v;
} token_t;

void init_tokens(void);
void exit_tokens(void);
void print_token_type(FILE *out, token_type_t token_type);
void print_token(FILE *out, const token_t *token);

#endif
