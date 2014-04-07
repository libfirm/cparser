/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include <assert.h>
#include <stdbool.h>
#include "machine_triple.h"
#include "adt/strutil.h"
#include "adt/xmalloc.h"

bool is_unixish_os(const char *os)
{
	return strstr(os, "linux") != NULL || strstr(os, "bsd") != NULL
	    || strstart(os, "solaris");
}

bool is_elf_os(const char *os)
{
	return is_unixish_os(os) || streq(os, "elf") || streq(os, "octopos")
	    || streq(os, "irtss");
}

bool is_darwin_os(const char *os)
{
	return strstart(os, "darwin");
}

bool is_windows_os(const char *os)
{
	return strstart(os, "mingw") || streq(os, "win32");
}

bool is_ia32_cpu(const char *architecture)
{
	return streq(architecture, "i386")
	    || streq(architecture, "i486")
	    || streq(architecture, "i586")
	    || streq(architecture, "i686")
	    || streq(architecture, "i786");
}

machine_triple_t *get_host_machine_triple(void)
{
#ifdef HOST_TRIPLE
	/* a triple for the host machine was defined in the Makefile
	 * or config.mak */
	return parse_machine_triple(HOST_TRIPLE);
#else
	/* no host triple specified, we do some guessing based on preprocessor
	 * defines (look into predefs.c for inspiration) */
	machine_triple_t *machine = XMALLOC(machine_triple_t);

#if defined(__x86_64__)
	machine->cpu_type = xstrdup("x86_64");
#elif defined(__i686__)
	machine->cpu_type = xstrdup("i686");
#elif defined(__i386__)
	machine->cpu_type = xstrdup("i386");
#elif defined(__sparc__)
	machine->cpu_type = xstrdup("sparc");
#elif defined(__arm__)
	machine->cpu_type = xstrdup("arm");
#endif

#if defined(__leon__)
	machine->manufacturer = xstrdup("leon");
#else
	machine->manufacturer = xstrdup("unknown");
#endif

#if defined(_WIN32) || defined(__CYGWIN__)
	machine->operating_system = xstrdup("win32");
#elif defined(__APPLE__)
	machine->operating_system = xstrdup("darwin");
#elif defined(__gnu_linux__)
	machine->operating_system = xstrdup("linux-gnu");
#elif defined(__linux__)
	machine->operating_system = xstrdup("linux");
#elif defined(__ELF__)
	machine->operating_system = xstrdup("elf");
#else
	machine->operating_system = xstrdup("unknown");
#endif
	return machine;
#endif
}

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
