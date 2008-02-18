/*
 * This file is part of cparser.
 * Copyright (C) 2007-2008 Matthias Braun <matze@braunis.de>
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

/**
 * @file
 * @brief     More comfortable allocations.
 * @author    Markus Armbruster
 * @data      1999 by getting from fiasco
 * @version   $Id$
 * Copyright: (c) 1995, 1996 Markus Armbruster
 * Licence:   This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _XMALLOC_H_
#define _XMALLOC_H_

#include <stdlib.h>
#include "config.h"

__attribute__((malloc))
void *xmalloc(size_t size);

__attribute__((malloc))
void *xcalloc(size_t num, size_t size);

void *xrealloc(void *ptr, size_t size);

__attribute__((malloc))
char *xstrdup(const char *str);

#define xfree(ptr)      free(ptr)

#endif
