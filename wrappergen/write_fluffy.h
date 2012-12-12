/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef WRITE_FLUFFY_H
#define WRITE_FLUFFY_H

#include "ast.h"

void write_fluffy_decls(FILE *out, const translation_unit_t *unit);

#endif
