/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef POSITION_H
#define POSITION_H

#include <stdbool.h>

typedef struct position_t position_t;
struct position_t {
	const char *input_name;
	unsigned    lineno;
	unsigned    colno            : 31;
	bool        is_system_header : 1;
};

/* position used for "builtin" declarations/types */
extern const position_t builtin_position;

#endif
