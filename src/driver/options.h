/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#ifndef OPTIONS_H
#define OPTIONS_H

#include <stdbool.h>

typedef int (*action_func)(const char *argv0);

typedef struct options_state_t {
	int         argc;
	char      **argv;
	int         i;
	bool        argument_errors;
	bool        had_inputs;
	action_func action;
} options_state_t;

bool options_parse_early_target(options_state_t *state);
bool options_parse_early_codegen(options_state_t *state);

bool options_parse_assembler(options_state_t *state);
bool options_parse_c_dialect(options_state_t *state);
bool options_parse_codegen(options_state_t *state);
bool options_parse_diagnostics(options_state_t *state);
bool options_parse_driver(options_state_t *state);
bool options_parse_help(options_state_t *state);
bool options_parse_linker(options_state_t *state);
bool options_parse_preprocessor(options_state_t *state);

bool action_print_help(const char *argv0);
void setup_target_machine(void);

const char *spaced_arg(const char *arg, options_state_t *s,
                       bool arg_may_be_option);
bool simple_arg(const char *arg, options_state_t *s);

#endif
