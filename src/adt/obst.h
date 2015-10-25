/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef ADT_OBST_H
#define ADT_OBST_H

#include "adt/xmalloc.h"
#include <libfirm/adt/obstack.h>

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free  free

static inline void *obstack_nul_finish(struct obstack *const obst)
{
	obstack_1grow(obst, '\0');
	return obstack_finish(obst);
}

#endif
