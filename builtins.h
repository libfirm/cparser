/*
 * This file is part of cparser.
 * Copyright (C) 2007-2010 Matthias Braun <matze@braunis.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
#ifndef BUILTINS_H
#define BUILTINS_H

#include <stdbool.h>

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

#endif
