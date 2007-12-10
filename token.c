#include <config.h>

#include "token_t.h"

#include <assert.h>
#include <stdio.h>

#include "symbol.h"
#include "lang_features.h"
#include "adt/array.h"

static symbol_t *token_symbols[T_LAST_TOKEN];

source_position_t builtin_source_position = { "<built-in>", 0 };

void init_tokens(void)
{
	symbol_t *symbol;
	int       last_id = -2;

	memset(token_symbols, 0, T_LAST_TOKEN * sizeof(token_symbols[0]));

#define T(mode,x,str,val)                                          \
	if (T_##x > 255) {                                             \
		assert(T_##x >= last_id);                                  \
		last_id = T_##x;                                           \
	}                                                              \
	if (c_mode & (mode)) {                                         \
		assert(T_##x >= 0 && T_##x < T_LAST_TOKEN);                \
		symbol               = symbol_table_insert(str);           \
		symbol->ID           = T_##x;                              \
		token_symbols[T_##x] = symbol;                             \
	}

#define TS(x,str,val)                                              \
	assert(T_##x >= 0 && T_##x < T_LAST_TOKEN);                    \
	symbol               = symbol_table_insert(str);               \
	token_symbols[T_##x] = symbol;

#include "tokens.inc"

#undef TS
#undef T

#define T(mode,x,str,val)                                          \
	assert(TP_##x >= 0 && TP_##x < TP_LAST_TOKEN);                 \
	symbol               = symbol_table_insert(str);               \
	symbol->pp_ID        = TP_##x;

#include "tokens_preprocessor.inc"

#undef T
}

void exit_tokens(void)
{
}

void print_token_type(FILE *f, token_type_t token_type)
{
	if(token_type == T_EOF) {
		fputs("end of file", f);
		return;
	}
	if(token_type == T_ERROR) {
		fputs("error", f);
		return;
	}

	int token_symbols_len = T_LAST_TOKEN;
	if(token_type < 0 || token_type >= token_symbols_len) {
		fputs("invalid token", f);
		return;
	}

	const symbol_t *symbol = token_symbols[token_type];
	if(symbol != NULL) {
		fprintf(f, "'%s'", symbol->string);
	} else {
		if(token_type >= 0 && token_type < 256) {
			fprintf(f, "'%c'", token_type);
			return;
		}
		fputs("unknown token", f);
	}
}

void print_token(FILE *f, const token_t *token)
{
	switch(token->type) {
	case T_IDENTIFIER:
		fprintf(f, "symbol '%s'", token->v.symbol->string);
		break;
	case T_INTEGER:
		fprintf(f, "integer number %lld", token->v.intvalue);
		break;
	case T_FLOATINGPOINT:
		fprintf(f, "floatingpointer number %LF", token->v.floatvalue);
		break;
	case T_STRING_LITERAL:
		fprintf(f, "string '%s'", token->v.string);
		break;
	default:
		print_token_type(f, (token_type_t)token->type);
		break;
	}
}
