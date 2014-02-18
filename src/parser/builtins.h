/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef BUILTINS_H
#define BUILTINS_H

#include "ast/entity.h"

/**
 * Create predefined gnu builtins.
 */
void create_gnu_builtins(void);

/**
 * Create predefined MS intrinsics.
 */
void create_microsoft_intrinsics(void);

/**
 * Some functions like setjmp,longjmp are known from libc and need special
 * attributes like noreturn or returns_twice.
 * (Adding __attribute__(())s in the libc headers would be enough but apparently
 *  this is not done in most cases since people rely on a list of hardcoded
 *  names in gcc, so we have to duplicate this here)
 */
void adapt_special_functions(function_t *function);

#endif
