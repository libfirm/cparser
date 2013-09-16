/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#ifndef OPTIONS_H
#define OPTIONS_H

#include <stdbool.h>

typedef struct options_state_t {
	int    argc;
	char **argv;
	int    i;
	bool   argument_errors;
	bool   had_inputs;
} options_state_t;

bool options_parse_preprocessor(options_state_t *state);
bool options_parse_assembler(options_state_t *state);
bool options_parse_linker(options_state_t *state);
bool options_parse_driver(options_state_t *state);

#endif
