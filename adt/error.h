/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef ADT_ERROR_H
#define ADT_ERROR_H

#include <stdio.h>
#include <stdlib.h>
#include "config.h"

static inline __attribute__((noreturn)) void panic(char const *const file, int const line, char const *const func, char const *const msg)
{
	fprintf(stderr, "%s:%d: panic in %s: %s\n", file, line, func, msg);
	abort();
}

#define panic(msg) panic(__FILE__, __LINE__, __func__, (msg))

#endif
