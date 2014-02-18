/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef WRITE_JNA_H
#define WRITE_JNA_H

#include "ast/ast.h"

void jna_limit_output(const char *filename);

void jna_set_libname(const char *libname);

void write_jna_decls(FILE *out, const translation_unit_t *unit);

#endif
