/* collides with libfirm */
#if 0

#include <config.h>

#include "pset.h"

/** probing method: quadratic probing */
#define DO_REHASH
#define HashSet                    pset_t
#define HashSetIterator            pset_iterator_t
#define ValueType                  void*
#define NullValue                  NULL
#define DeletedValue               ((void*)-1)
#define KeysEqual(this,key1,key2)  1
#define SetRangeEmpty(ptr,size)    memset(ptr, 0, (size) * sizeof(HashSetEntry))

#define hashset_init            pset_init
#define hashset_init_size       pset_init_size
#define hashset_destroy         pset_destroy
#define hashset_insert          pset_insert
#define hashset_remove          pset_remove
#define hashset_find            pset_find
#define hashset_size            pset_size
#define hashset_iterator_init   pset_iterator_init
#define hashset_iterator_next   pset_iterator_next
#define hashset_remove_iterator pset_remove_iterator

#include "hashset.c"

int pset_contains(const pset_t *pset, const ValueType val)
{
	return pset_find(pset, val) != NullValue;
}

#endif
