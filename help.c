/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include <stdio.h>

#include "help.h"

void put_help(const char *option, const char *explanation)
{
	printf("\t%-15s  %s\n", option, explanation);
}

void put_choice(const char *choice, const char *explanation)
{
	printf("\t    %-11s  %s\n", choice, explanation);
}
