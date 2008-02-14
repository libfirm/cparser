/**
 * @file
 * @date    17.03.2007
 * @brief   Makes using obstack.h easier by including obstack_chunk_alloc and
 *          obstack_chunk_free
 * @author  Martin Trapp, Christian Schaefer
 * @version $Id$
 */
#include "obstack.h"
#include "xmalloc.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free  free
