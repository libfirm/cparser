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

extern source_position_t source_position;

void init_lexer(void);
void exit_lexer(void);

void lexer_open_stream(FILE *stream, const char *input_name);

#endif
