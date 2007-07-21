#ifndef SYMBOL_H
#define SYMBOL_H

#include "ast.h"

typedef struct symbol_t symbol_t;

struct symbol_t {
	const char     *string;
	unsigned short  ID;
	unsigned short  pp_ID;
	declaration_t  *declaration;
	const void     *context;
};

#endif
