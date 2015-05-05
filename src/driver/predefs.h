/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#ifndef PREDEFS_H
#define PREDEFS_H

#include <stdbool.h>

extern bool predef_optimize;
extern bool predef_optimize_size;

void add_predefined_macros(void);

#endif
