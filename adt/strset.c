#include <config.h>

#include "strset.h"
#include "hash_string.h"

#define HashSet                    strset_t
#define HashSetIterator            strset_iterator_t
#define HashSetEntry               strset_entry_t
#define ValueType                  const char*
#define ConstKeyType               const char*
#define NullValue                  NULL
#define DeletedValue               ((void*)-1)
#define Hash(this, value)          hash_string(value)
#define KeysEqual(this,key1,key2)  (strcmp(key1, key2) == 0)
#define SetRangeEmpty(ptr,size)    memset(ptr, 0, (size) * sizeof(strset_entry_t))

#define hashset_init            strset_init
#define hashset_init_size       strset_init_size
#define hashset_destroy         strset_destroy
#define hashset_insert          strset_insert
#define hashset_remove          strset_remove
#define hashset_find            strset_find
#define hashset_size            strset_size
#define hashset_iterator_init   strset_iterator_init
#define hashset_iterator_next   strset_iterator_next
#define hashset_remove_iterator strset_remove_iterator

#include "hashset.c"
