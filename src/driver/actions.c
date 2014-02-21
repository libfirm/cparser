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
