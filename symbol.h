#ifndef SYMBOL_H
#define SYMBOL_H

#include "parser.h"

typedef struct symbol_t symbol_t;

struct symbol_t {
	const char          *string;
	unsigned short       ID;
	unsigned short       pp_ID;
	environment_entry_t *thing;
};

#endif
