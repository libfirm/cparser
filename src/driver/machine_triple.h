/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef FIRM_MACHINE_H
#define FIRM_MACHINE_H

#include <stdbool.h>

typedef struct machine_triple_t {
	char *cpu_type;
	char *manufacturer;
	char *operating_system;
} machine_triple_t;

machine_triple_t *get_host_machine_triple(void);

machine_triple_t *parse_machine_triple(const char *triple_string);

void free_machine_triple(machine_triple_t *triple);

#endif
