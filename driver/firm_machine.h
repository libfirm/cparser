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

machine_triple_t *firm_get_host_machine(void);

machine_triple_t *firm_parse_machine_triple(const char *triple_string);

void firm_free_machine_triple(machine_triple_t *triple);

bool setup_firm_for_machine(const machine_triple_t *machine);

bool firm_is_unixish_os(const machine_triple_t *machine);

bool firm_is_darwin_os(const machine_triple_t *machine);

bool firm_is_windows_os(const machine_triple_t *machine);

bool firm_is_ia32_cpu(const char *architecture);

#endif
