/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include <config.h>

#include <assert.h>
#include <stdbool.h>
#include "firm_machine.h"
#include "adt/strutil.h"
#include "adt/xmalloc.h"
#include <libfirm/firm.h>

static void set_be_option(const char *arg)
{
	int res = be_parse_arg(arg);
	(void) res;
	assert(res);
}

static ident *compilerlib_name_mangle_default(ident *id, ir_type *mt)
{
	(void)mt;
	return id;
}

static ident *compilerlib_name_mangle_underscore(ident *id, ir_type *mt)
{
	(void)mt;
	return id_mangle3("_", id, "");
}

bool firm_is_unixish_os(const machine_triple_t *machine)
{
	const char *os = machine->operating_system;
	return strstr(os, "linux") != NULL || strstr(os, "bsd") != NULL
		|| strstart(os, "solaris");
}

bool firm_is_darwin_os(const machine_triple_t *machine)
{
	const char *os = machine->operating_system;
	return strstart(os, "darwin");
}

bool firm_is_windows_os(const machine_triple_t *machine)
{
	const char *os = machine->operating_system;
	return strstart(os, "mingw") || streq(os, "win32");
}

/**
 * Initialize firm codegeneration for a specific operating system.
 * The argument is the operating system part of a target-triple
 */
static bool setup_os_support(const machine_triple_t *machine)
{
	if (firm_is_unixish_os(machine)
	    || streq(machine->operating_system, "elf")) {
		set_be_option("ia32-gasmode=elf");
		set_compilerlib_name_mangle(compilerlib_name_mangle_default);
	} else if (firm_is_darwin_os(machine)) {
		set_be_option("ia32-gasmode=macho");
		set_be_option("ia32-stackalign=4");
		set_be_option("pic=true");
		set_compilerlib_name_mangle(compilerlib_name_mangle_underscore);
	} else if (firm_is_windows_os(machine)) {
		set_be_option("ia32-gasmode=mingw");
		set_compilerlib_name_mangle(compilerlib_name_mangle_underscore);
	} else {
		return false;
	}

	return true;
}

bool setup_firm_for_machine(const machine_triple_t *machine)
{
	const char *cpu = machine->cpu_type;

	if (streq(cpu, "i386")) {
		set_be_option("isa=ia32");
		set_be_option("ia32-arch=i386");
	} else if (streq(cpu, "i486")) {
		set_be_option("isa=ia32");
		set_be_option("ia32-arch=i486");
	} else if (streq(cpu, "i586")) {
		set_be_option("isa=ia32");
		set_be_option("ia32-arch=i586");
	} else if (streq(cpu, "i686")) {
		set_be_option("isa=ia32");
		set_be_option("ia32-arch=i686");
	} else if (streq(cpu, "i786")) {
		set_be_option("isa=ia32");
		set_be_option("ia32-arch=pentium4");
	} else if (streq(cpu, "x86_64")) {
		set_be_option("isa=amd64");
	} else if (streq(cpu, "sparc")) {
		set_be_option("isa=sparc");
	} else if (streq(cpu, "arm")) {
		set_be_option("isa=arm");
	} else {
		fprintf(stderr, "Unknown cpu '%s' in target-triple\n", cpu);
		return false;
	}

	/* process operating system */
	if (!setup_os_support(machine)) {
		fprintf(stderr, "Unknown operating system '%s' in target-triple\n", machine->operating_system);
		return false;
	}
	return true;
}

machine_triple_t *firm_get_host_machine(void)
{
	machine_triple_t *machine = XMALLOC(machine_triple_t);
	machine->cpu_type = xstrdup("i386");
	machine->manufacturer = xstrdup("unknown");
#if defined(_WIN32) || defined(__CYGWIN__)
	machine->operating_system = xstrdup("win32");
#elif defined(__APPLE__)
	machine->operating_system = xstrdup("darwin");
#else
	machine->operating_system = xstrdup("linux");
#endif
	return machine;
}

void firm_free_machine_triple(machine_triple_t *machine)
{
	free(machine->cpu_type);
	free(machine->manufacturer);
	free(machine->operating_system);
	free(machine);
}

machine_triple_t *firm_parse_machine_triple(const char *triple_string)
{
	const char *manufacturer = strchr(triple_string, '-');
	if (manufacturer == NULL) {
		return NULL;
	}
	manufacturer += 1;

	const char *os = strchr(manufacturer, '-');
	if (os == NULL) {
		return false;
	}
	os += 1;

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
	if (strstart(manufacturer, "linux")) {
		triple->manufacturer = xstrdup("unknown");
		os = manufacturer;
	} else {
		size_t manufacturer_len = os-manufacturer;
		triple->manufacturer = XMALLOCN(char, manufacturer_len);
		memcpy(triple->manufacturer, manufacturer, manufacturer_len-1);
		triple->manufacturer[manufacturer_len-1] = '\0';
	}

	triple->operating_system = xstrdup(os);
	return triple;
}
