/*
 * This file is part of cparser.
 * Copyright (C) 2014 Matthias Braun <matze@braunis.de>
 */
#include "string_hash.h"

#include <stdbool.h>
#include "adt/hash_string.h"
#include "adt/obst.h"

struct obstack string_obst;

typedef struct string_hash_t string_hash_t;
#define HashSet                   string_hash_t
#define HashSetEntry              string_hash_entry_t
#define ValueType                 string_t*
#include "adt/hashset.h"

static inline bool strings_equal(const string_t *str1, const string_t *str2)
{
	size_t size = str1->size;
	if (str2->size != size || str1->encoding != str2->encoding)
		return false;
	return memcmp(str1->begin, str2->begin, size) == 0;
}

#define NullValue                 NULL
#define DeletedValue              ((ValueType)-1)
//#define KeyType                   string_t*
#define ConstKeyType              const string_t*
//#define GetKey(value)             (value)
//#define InitData(this,value,key)  (value) = (key)
#define Hash(this,key)            hash_string_size(key->begin, key->size)
#define KeysEqual(this,key1,key2) strings_equal(key1, key2)
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof(HashSetEntry))
#define SCALAR_RETURN

void string_hash_init_(string_hash_t *hash);
#define hashset_init       string_hash_init_
void string_hash_destroy_(string_hash_t *hash);
#define hashset_destroy    string_hash_destroy_
string_t *string_hash_insert_(string_hash_t *hash, string_t *key);
#define hashset_insert     string_hash_insert_

#include "adt/hashset.c.h"

static string_hash_t string_table;

void init_string_hash(void)
{
	obstack_init(&string_obst);
	string_hash_init_(&string_table);
}

void exit_string_hash(void)
{
	string_hash_destroy_(&string_table);
	obstack_free(&string_obst, NULL);
}

void begin_string_construction_on(struct obstack *obst)
{
	/* push string header on the obstack */
	assert(obstack_object_size(obst) == 0);
	obstack_blank(obst, sizeof(string_t));
}

void begin_string_construction(void)
{
	begin_string_construction_on(&string_obst);
}

void abort_string_construction(void)
{
	char *dummy = obstack_finish(&string_obst);
	obstack_free(&string_obst, dummy);
}

string_t *finish_string_construction_on(struct obstack *obst,
                                        string_encoding_t encoding)
{
	size_t    const size = obstack_object_size(obst) - sizeof(string_t);
	string_t *const news = obstack_nul_finish(obst);
	news->size     = size;
	news->encoding = encoding;
	news->entity   = NULL;
	string_t *result = string_hash_insert_(&string_table, news);
	if (news != result)
		obstack_free(obst, news);
	return result;
}

string_t *finish_string_construction(string_encoding_t encoding)
{
	return finish_string_construction_on(&string_obst, encoding);
}
