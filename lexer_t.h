#ifndef LEXER_T_H
#define LEXER_T_H

#include "lexer.h"

#include <stdio.h>
#include "symbol_table_t.h"
#include "adt/obst.h"
#include "adt/strset.h"

#define MAX_INDENT               256

typedef struct source_position_t source_position_t;
struct source_position_t {
	const char *input_name;
	unsigned    linenr;
};

struct lexer_t {
	int               c;
	source_position_t source_position;
	FILE             *input;
	char              buf[1024];
	const char       *bufend;
	const char       *bufpos;
	strset_t          stringset;
};

void lexer_init(lexer_t *lexer, FILE *stream, const char *input_name);

void lexer_destroy(lexer_t *lexer);

#endif
