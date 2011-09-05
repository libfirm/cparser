/*
 * This file is part of cparser.
 * Copyright (C) 2007-2009 Matthias Braun <matze@braunis.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
#include <config.h>

#include "token_t.h"
#include "symbol_t.h"

#include <assert.h>
#include <stdio.h>

#include "symbol.h"
#include "lang_features.h"
#include "adt/array.h"

static symbol_t *token_symbols[T_LAST_TOKEN];
static symbol_t *pp_token_symbols[TP_LAST_TOKEN];

const source_position_t builtin_source_position = { "<built-in>", 0, 0, true };

static int last_id;

static symbol_t *intern_register_token(token_kind_t id, const char *string)
{
	assert(0 <= id && id < T_LAST_TOKEN);
	symbol_t *symbol = symbol_table_insert(string);
	if (token_symbols[id] == NULL)
		token_symbols[id] = symbol;
	return symbol;
}

static symbol_t *intern_register_pp_token(preprocessor_token_kind_t id, const char *string)
{
	assert(0 <= id && id < TP_LAST_TOKEN);
	symbol_t *symbol = symbol_table_insert(string);
	if (pp_token_symbols[id] == NULL)
		pp_token_symbols[id] = symbol;
	return symbol;
}

static void register_token(unsigned mode, token_kind_t id, const char *string)
{
	if (id > 255) {
		assert(id >= last_id);
		last_id = id;
	}
	if (c_mode & mode) {
		symbol_t *symbol = intern_register_token(id, string);
		symbol->ID = id;
	}
}

static void register_pp_token(unsigned mode, token_kind_t id,
                              const char *string)
{
	if (! (c_mode & mode))
		return;

	symbol_t *symbol = intern_register_pp_token(id, string);
	symbol->pp_ID = id;
}

void init_tokens(void)
{
	memset(token_symbols, 0, T_LAST_TOKEN * sizeof(token_symbols[0]));
	memset(pp_token_symbols, 0, TP_LAST_TOKEN * sizeof(pp_token_symbols[0]));

	last_id = -2;

#define T(mode,x,str,val)  register_token(mode, T_##x, str);
#define TS(x,str,val)      intern_register_token(T_##x, str);
#include "tokens.inc"
#undef TS
#undef T

#define T(mode,x,str,val)  register_pp_token(mode, TP_##x, str);
#define TS(x,str,val)      intern_register_pp_token(TP_##x, str);
#include "tokens_preprocessor.inc"
#undef TS
#undef T
}

void exit_tokens(void)
{
}

void print_token_kind(FILE *f, token_kind_t token_kind)
{
	if(token_kind == T_EOF) {
		fputs("end of file", f);
		return;
	}
	if(token_kind == T_ERROR) {
		fputs("error", f);
		return;
	}

	int token_symbols_len = T_LAST_TOKEN;
	if(token_kind < 0 || token_kind >= token_symbols_len) {
		fputs("invalid token", f);
		return;
	}

	const symbol_t *symbol = token_symbols[token_kind];
	if(symbol != NULL) {
		fputs(symbol->string, f);
	} else {
		if(token_kind >= 0 && token_kind < 256) {
			fputc(token_kind, f);
			return;
		}
		fputs("unknown token", f);
	}
}

symbol_t *get_token_kind_symbol(int kind)
{
	return token_symbols[kind];
}

static void print_stringrep(const string_t *string, FILE *f)
{
	for (size_t i = 0; i < string->size; ++i) {
		fputc(string->begin[i], f);
	}
}

void print_token(FILE *f, const token_t *token)
{
	switch(token->kind) {
	case T_IDENTIFIER:
		fprintf(f, "identifier '%s'", token->identifier.symbol->string);
		break;
	case T_INTEGER:
	case T_INTEGER_OCTAL:
	case T_INTEGER_HEXADECIMAL:
	case T_FLOATINGPOINT:
	case T_FLOATINGPOINT_HEXADECIMAL:
		print_token_kind(f, (token_kind_t)token->kind);
		fputs(" '", f);
		print_stringrep(&token->number.number, f);
		if (token->number.suffix.size > 0)
			print_stringrep(&token->number.suffix, f);
		fputc('\'', f);
		break;
	case T_WIDE_STRING_LITERAL:
	case T_STRING_LITERAL:
		print_token_kind(f, (token_kind_t)token->kind);
		fprintf(f, " \"%s\"", token->string.string.begin);
		break;
	case T_CHARACTER_CONSTANT:
	case T_WIDE_CHARACTER_CONSTANT:
		print_token_kind(f, (token_kind_t)token->kind);
		fputs(" \'", f);
		print_stringrep(&token->string.string, f);
		fputs("'", f);
		break;
	default:
		fputc('\'', f);
		print_token_kind(f, (token_kind_t)token->kind);
		fputc('\'', f);
		break;
	}
}

void print_pp_token_kind(FILE *f, int token_kind)
{
	if (token_kind == TP_EOF) {
		fputs("end of file", f);
		return;
	}
	if (token_kind == TP_ERROR) {
		fputs("error", f);
		return;
	}

	int token_symbols_len = TP_LAST_TOKEN;
	if (token_kind < 0 || token_kind >= token_symbols_len) {
		fputs("invalid token", f);
		return;
	}

	const symbol_t *symbol = pp_token_symbols[token_kind];
	if (symbol != NULL) {
		fputs(symbol->string, f);
	} else {
		if(token_kind >= 0 && token_kind < 256) {
			fputc(token_kind, f);
			return;
		}
		fputs("unknown token", f);
	}
}

void print_pp_token(FILE *f, const token_t *token)
{
	switch((preprocessor_token_kind_t) token->kind) {
	case TP_IDENTIFIER:
		fprintf(f, "identifier '%s'", token->identifier.symbol->string);
		break;
	case TP_NUMBER:
		fprintf(f, "number '%s'", token->number.number.begin);
		break;
	case TP_STRING_LITERAL:
		fprintf(f, "string \"%s\"", token->string.string.begin);
		break;
	default:
		print_pp_token_kind(f, (preprocessor_token_kind_t) token->kind);
		break;
	}
}

bool tokens_would_paste(preprocessor_token_kind_t token1,
                        preprocessor_token_kind_t token2)
{
	char c = token2 < 256 ? (char) token2 : pp_token_symbols[token2]->string[0];

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
	case '.': return c == '.' || c == '%' || token2 == TP_NUMBER;
	case '#': return c == '#' || c == '%';
	case TP_GREATERGREATER: return c == '=';
	case TP_LESSLESS:       return c == '=';
	case '^':               return c == '=';
	case '!':               return c == '=';
	case TP_IDENTIFIER:
		return token2 == TP_IDENTIFIER || token2 == TP_NUMBER ||
		       token2 == TP_CHARACTER_CONSTANT ||
		       token2 == TP_WIDE_CHARACTER_CONSTANT ||
		       token2 == TP_WIDE_STRING_LITERAL ||
		       token2 == TP_STRING_LITERAL; /* L */
	case TP_NUMBER:
		return token2 == TP_NUMBER || token2 == TP_IDENTIFIER ||
		       token2 == '.' || token2 == '+' || token2 == '-';
	default:
		return false;
	}
}
