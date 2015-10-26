/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include <assert.h>
#include <stdbool.h>
#include "machine_triple.h"
#include "adt/strutil.h"
#include "adt/xmalloc.h"

void free_machine_triple(machine_triple_t *machine)
{
	free(machine->cpu_type);
	free(machine->manufacturer);
	free(machine->operating_system);
	free(machine);
}

machine_triple_t *parse_machine_triple(const char *triple_string)
{
	const char *manufacturer = strchr(triple_string, '-');
	if (manufacturer == NULL) {
		return NULL;
	}
	manufacturer += 1;

	const char *os = strchr(manufacturer, '-');
	if (os != NULL) {
		os += 1;
	}

	/* Note: Triples are more or less defined by what the config.guess and
	 * config.sub scripts from GNU autoconf emit. We have to lookup there what
	 * triples are possible */

	const char *cpu = triple_string;

	machine_triple_t *triple = XMALLOCZ(machine_triple_t);

	size_t cpu_type_len = manufacturer-cpu;
	triple->cpu_type = XMALLOCN(char, cpu_type_len);
	memcpy(triple->cpu_type, cpu, cpu_type_len-1);
	triple->cpu_type[cpu_type_len-1] = '\0';

	/* process manufacturer, alot of people incorrectly leave out the
	 * manufacturer instead of using unknown- */
	if (strstart(manufacturer, "linux") || streq(manufacturer, "elf")
	    || os == NULL) {
		triple->manufacturer = xstrdup("unknown");
		os                   = manufacturer;
	} else {
		size_t manufacturer_len = os-manufacturer;
		triple->manufacturer = XMALLOCN(char, manufacturer_len);
		memcpy(triple->manufacturer, manufacturer, manufacturer_len-1);
		triple->manufacturer[manufacturer_len-1] = '\0';
	}

	triple->operating_system = xstrdup(os);
	return triple;
}
