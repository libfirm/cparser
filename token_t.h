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

typedef enum {
#define T(x,str,val) TP_##x val,
#define TS(x,str,val) TP_##x val,
#include "tokens_preprocessor.inc"
#undef TS
#undef T
} preprocessor_token_type_t;

typedef struct source_position_t source_position_t;
struct source_position_t {
	const char *input_name;
	unsigned    linenr;
};

typedef struct {
	int type;
	union {
		symbol_t   *symbol;
		int         intvalue;
		double      floatvalue;
		const char *string;
	} v;
	source_position_t  source_position;
} token_t;

void init_tokens(void);
void exit_tokens(void);
void print_token_type(FILE *out, token_type_t token_type);
void print_token(FILE *out, const token_t *token);

#endif
