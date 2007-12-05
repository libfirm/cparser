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
