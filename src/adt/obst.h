/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "adt/xmalloc.h"
#include <libfirm/adt/obstack.h>

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free  free
