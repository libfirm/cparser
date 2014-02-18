/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#include "actions.h"

#include <libfirm/firm.h>
#include <stdio.h>
#include <stdlib.h>

#include "driver.h"
#include "version.h"
#include <revision.h>

const char *print_file_name_file;

int action_version(const char *argv0)
{
	(void)argv0;
	printf("cparser %s.%s.%s",
	       CPARSER_MAJOR, CPARSER_MINOR, CPARSER_PATCHLEVEL);
	if (cparser_REVISION[0] != '\0') {
		printf("(%s)", cparser_REVISION);
	}
	printf(" using libFirm %u.%u",
	       ir_get_version_major(), ir_get_version_minor());

	const char *revision = ir_get_version_revision();
	if (revision[0] != 0) {
		printf("(%s)", revision);
	}
	putchar('\n');
	return EXIT_SUCCESS;
}

int action_version_short(const char *argv0)
{
	(void)argv0;
	printf("%s.%s.%s\n",
	       CPARSER_MAJOR, CPARSER_MINOR, CPARSER_PATCHLEVEL);
	return EXIT_SUCCESS;
}

int action_print_file_name(const char *argv0)
{
	(void)argv0;
	driver_add_flag(&ldflags_obst, "-print-file-name=%s", print_file_name_file);

	obstack_1grow(&ldflags_obst, '\0');
	const char *flags = obstack_finish(&ldflags_obst);

	/* construct commandline */
	obstack_printf(&ldflags_obst, "%s ", driver_linker);
	obstack_printf(&ldflags_obst, "%s", flags);
	obstack_1grow(&ldflags_obst, '\0');

	char *commandline = obstack_finish(&ldflags_obst);
	if (driver_verbose) {
		puts(commandline);
	}
	int err = system(commandline);
	if (err != EXIT_SUCCESS) {
		fprintf(stderr, "%s: error: linker reported an error\n",
		        print_file_name_file);
	}
	obstack_free(&ldflags_obst, commandline);
	return err;
}
