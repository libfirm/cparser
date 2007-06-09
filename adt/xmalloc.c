/*
 * Project:     libFIRM
 * File name:   ir/adt/xmalloc.c
 * Purpose:     Xmalloc --- never failing wrappers for malloc() & friends.
 * Author:      Markus Armbruster
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Markus Armbruster
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/* @@@ ToDo: replace this file with the one from liberty.
   [reimplement xstrdup, ... ] */
#include <config.h>

#include <stdlib.h>
#include <string.h>

#include "xmalloc.h"
#include "error.h"
#include "util.h"

static inline __attribute__((noreturn))
void out_of_memory(void) {
	panic("out of memory");
}

void *xmalloc(size_t size) {
	void *res = malloc(size);

	if (UNLIKELY(res == NULL))
		out_of_memory();

	return res;
}

void *xcalloc(size_t num, size_t size) {
	void *res = calloc(num, size);

	if (UNLIKELY(res == NULL))
		out_of_memory();

	return res;
}

void *xrealloc(void *ptr, size_t size) {
	void *res = realloc (ptr, size);

	if (UNLIKELY(res == NULL))
		out_of_memory();

	return res;
}

char *xstrdup(const char *str) {
  	size_t len = strlen(str) + 1;
	char *res = xmalloc(len);
	memcpy(res, str, len);

	return res;
}
