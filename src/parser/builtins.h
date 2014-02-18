/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef BUILTINS_H
#define BUILTINS_H

#include "ast/entity.h"

typedef enum {
	BUILTIN_NONE,
	BUILTIN_ALLOCA,
	BUILTIN_INF,
	BUILTIN_NAN,
	BUILTIN_EXPECT,
	BUILTIN_VA_END,
	BUILTIN_OBJECT_SIZE,
	BUILTIN_ROTL,
	BUILTIN_ROTR,
	BUILTIN_LIBC,
	BUILTIN_LIBC_CHECK,
	BUILTIN_FIRM,
} builtin_kind_t;

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
