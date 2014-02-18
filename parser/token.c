/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include <assert.h>

#include "adt/array.h"
#include "adt/error.h"
#include "adt/util.h"
#include "ast/symbol_t.h"
#include "ast/symbol_table.h"
#include "driver/lang_features.h"
#include "token_t.h"

symbol_t *token_symbols[T_LAST_TOKEN];

const position_t builtin_position = { "<built-in>", 0, 0, true };

static token_kind_t last_id;

static symbol_t *intern_register_token(token_kind_t id, const char *string)
{
	assert(id < T_LAST_TOKEN);
	symbol_t *symbol = symbol_table_insert(string);
	if (token_symbols[id] == NULL)
		token_symbols[id] = symbol;
	return symbol;
}

static void register_token(lang_features_t mode, token_kind_t id,
                           const char *string, bool is_keyword)
{
	if (id > 255) {
		assert(id >= last_id);
		last_id = id;
	}
	if (dialect.features & mode) {
		symbol_t *symbol = intern_register_token(id, string);
		if (is_keyword)
			symbol->ID = id;
	}
}

static void register_pp_token(pp_token_kind_t const id, char const *const string)
{
	assert(id < TP_LAST_TOKEN);
	symbol_t *const symbol = symbol_table_insert(string);
	symbol->pp_ID = id;
}

void init_tokens(void)
{
	static bool tokens_initialized = false;
	if (tokens_initialized)
		return;
	tokens_initialized = true;

	memset(token_symbols, 0, T_LAST_TOKEN * sizeof(token_symbols[0]));

#define T(mode, x, str, val, is_keyword) register_token(mode, x, str, is_keyword);
#include "tokens.h"
#undef T

#define T(token) register_pp_token(TP_##token, #token);
#include "tokens_preprocessor.h"
#undef T
}

void exit_tokens(void)
{
}

void print_token_kind(FILE *f, token_kind_t token_kind)
{
	if (token_kind >= ARRAY_SIZE(token_symbols)) {
		fputs("invalid token", f);
		return;
	}

	fputs(token_symbols[token_kind]->string, f);
}

char const *get_string_encoding_prefix(string_encoding_t const enc)
{
	switch (enc) {
	case STRING_ENCODING_CHAR:   return "";
	case STRING_ENCODING_CHAR16: return "u";
	case STRING_ENCODING_CHAR32: return "U";
	case STRING_ENCODING_UTF8:   return "u8";
	case STRING_ENCODING_WIDE:   return "L";
	}
	panic("invalid string encoding");
}

void print_token(FILE *f, const token_t *token)
{
	char        delim = '\'';
	char const *enc   = "";
	char const *val;
	switch (token->kind) {
	case T_IDENTIFIER:
	case T_MACRO_PARAMETER:
	case T_UNKNOWN_CHAR:
		val = token->base.symbol->string;
		break;

	case T_STRING_LITERAL:
		delim = '"';
		/* FALLTHROUGH */
	case T_CHARACTER_CONSTANT:
		enc = get_string_encoding_prefix(token->literal.string.encoding);
		/* FALLTHROUGH */
	case T_NUMBER:
		val = token->literal.string.begin;
		break;

	default: {
		char const *kind  = (token->base.symbol ? token->base.symbol : token_symbols[token->kind])->string;
		fprintf(f, "'%s'", kind);
		return;
	}
	}
	fprintf(f, "%s %s%c%s%c", token_symbols[token->kind]->string, enc, delim, val, delim);
}

bool tokens_would_paste(token_kind_t token1, token_kind_t token2)
{
	char const c = token_symbols[token2]->string[0];

	switch (token1) {
	case '>': return c == '>' || c == '=';
	case '<': return c == '<' || c == '=' || c == '%' || c == ':';
	case '+': return c == '+' || c == '=';
	case '-': return c == '-' || c == '>';
	case '/': return c == '/' || c == '=' || c == '*';
	case '%': return c == ':' || c == '=' || c == '>';
	case '&': return c == '&' || c == '=';
	case '|': return c == '|' || c == '=';
	case ':': return c == ':' || c == '>';
	case '*': return c == '*' || c == '=';
	case '.': return c == '.' || c == '%' || token2 == T_NUMBER;
	case '#': return c == '#' || c == '%';
	case T_GREATERGREATER: return c == '=';
	case T_LESSLESS:       return c == '=';
	case '^':              return c == '=';
	case '!':              return c == '=';

	case T_IDENTIFIER:
		return token2 == T_CHARACTER_CONSTANT ||
		       token2 == T_IDENTIFIER         ||
		       token2 == T_NUMBER             ||
		       token2 == T_STRING_LITERAL; /* L */

	case T_NUMBER:
		return token2 == T_IDENTIFIER || token2 == T_NUMBER ||
		       token2 == '.' || token2 == '+' || token2 == '-';

	default:
		return false;
	}
}
