#include <config.h>

#include "type_hash.h"

#include "adt/error.h"
#include "type_t.h"

#include <assert.h>

#define HashSet         type_hash_t
#define HashSetIterator type_hash_iterator_t
#define ValueType       type_t*
#include "adt/hashset.h"
#undef ValueType
#undef HashSetIterator
#undef HashSet

/* TODO: ^= is a bad way of combining hashes since most addresses are very
 * similar */

static unsigned hash_ptr(const void *ptr)
{
	unsigned ptr_int = ((char*) ptr - (char*) NULL);
	return ptr_int >> 3;
}

static unsigned hash_atomic_type(const atomic_type_t *type)
{
	unsigned some_prime = 27644437;
	unsigned result     = type->atype * some_prime;

	return result;
}

static unsigned hash_pointer_type(const pointer_type_t *type)
{
	return hash_ptr(type->points_to);
}

static unsigned hash_compound_type(const compound_type_t *type)
{
	unsigned result = hash_ptr(type->symbol);

	return result;
}

static unsigned hash_type(const type_t *type);

static unsigned hash_method_type(const method_type_t *type)
{
	unsigned result = hash_ptr(type->result_type);

	method_parameter_t *parameter = type->parameters;
	while(parameter != NULL) {
		result   ^= hash_ptr(parameter->type);
		parameter = parameter->next;
	}

	return result;
}

static unsigned hash_enum_type(const enum_type_t *type)
{
	unsigned result = hash_ptr(type->symbol);

	return result;
}

static unsigned hash_type(const type_t *type)
{
	unsigned hash;

	switch(type->type) {
	case TYPE_INVALID:
		panic("internalizing void or invalid types not possible");
		return 0;
	case TYPE_ATOMIC:
		hash = hash_atomic_type((const atomic_type_t*) type);
		break;
	case TYPE_ENUM:
		hash = hash_enum_type((const enum_type_t*) type);
		break;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		hash = hash_compound_type((const compound_type_t*) type);
		break;
	case TYPE_METHOD:
		hash = hash_method_type((const method_type_t*) type);
		break;
	case TYPE_POINTER:
		hash = hash_pointer_type((const pointer_type_t*) type);
		break;
	case TYPE_BUILTIN:
		hash = hash_ptr(((const builtin_type_t*) type)->symbol);
		break;
	}

	unsigned some_prime = 99991;
	hash ^= some_prime * type->qualifiers;

	return hash;
}

static int atomic_types_equal(const atomic_type_t *type1,
                              const atomic_type_t *type2)
{
	return type1->atype == type2->atype;
}

static int compound_types_equal(const compound_type_t *type1,
                                const compound_type_t *type2)
{
	if(type1->type.type != type2->type.type)
		return 0;
	if(type1->symbol != type2->symbol)
		return 0;

	/* anonymous types? */
	if(type1->symbol == NULL) {
		/* previous tests should already have checked for this */
		assert(type1 != type2);
		/* anonymous types are only equal if they are the very same type */
		return 0;
	}

	/* non-anonymous types with same symbol are equal */
	return 1;
}

static int enum_types_equal(const enum_type_t *type1, const enum_type_t *type2)
{
	if(type1->symbol != type2->symbol)
		return 0;

	/* anonymous types? */
	if(type1->symbol == NULL) {
		/* previous tests should already have checked for this */
		assert(type1 != type2);
		/* 2 anonymous enums are never equal */
		return 0;
	}

	/* non-anonymous types with same symbol are equal */
	return 1;
}

static int method_types_equal(const method_type_t *type1,
                              const method_type_t *type2)
{
	if(type1->result_type != type2->result_type)
		return 0;
	if(type1->variadic != type2->variadic)
		return 0;
	if(type1->unspecified_parameters != type2->unspecified_parameters)
		return 0;

	method_parameter_t *param1 = type1->parameters;
	method_parameter_t *param2 = type2->parameters;
	while(param1 != NULL && param2 != NULL) {
		if(param1->type != param2->type)
			return 0;
		param1 = param1->next;
		param2 = param2->next;
	}
	if(param1 != NULL || param2 != NULL)
		return 0;

	return 1;
}

static int pointer_types_equal(const pointer_type_t *type1,
                               const pointer_type_t *type2)
{
	return type1->points_to == type2->points_to;
}

static int builtin_types_equal(const builtin_type_t *type1,
                               const builtin_type_t *type2)
{
	return type1->symbol == type2->symbol;
}

static int types_equal(const type_t *type1, const type_t *type2)
{
	if(type1 == type2)
		return 1;
	if(type1->type != type2->type)
		return 0;
	if(type1->qualifiers != type2->qualifiers)
		return 0;

	switch(type1->type) {
	case TYPE_INVALID:
		return 0;
	case TYPE_ATOMIC:
		return atomic_types_equal((const atomic_type_t*) type1,
		                          (const atomic_type_t*) type2);
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return compound_types_equal((const compound_type_t*) type1,
		                            (const compound_type_t*) type2);
	case TYPE_ENUM:
		return enum_types_equal((const enum_type_t*) type1,
		                        (const enum_type_t*) type2);
	case TYPE_METHOD:
		return method_types_equal((const method_type_t*) type1,
		                          (const method_type_t*) type2);
	case TYPE_POINTER:
		return pointer_types_equal((const pointer_type_t*) type1,
		                           (const pointer_type_t*) type2);
	case TYPE_BUILTIN:
		return builtin_types_equal((const builtin_type_t*) type1,
		                           (const builtin_type_t*) type2);
	}

	abort();
}

#define HashSet                    type_hash_t
#define HashSetIterator            type_hash_iterator_t
#define ValueType                  type_t*
#define NullValue                  NULL
#define DeletedValue               ((type_t*)-1)
#define Hash(this, key)            hash_type(key)
#define KeysEqual(this,key1,key2)  types_equal(key1, key2)
#define SetRangeEmpty(ptr,size)    memset(ptr, 0, (size) * sizeof(*(ptr)))

#define hashset_init             _typehash_init
#define hashset_init_size        _typehash_init_size
#define hashset_destroy          _typehash_destroy
#define hashset_insert           _typehash_insert
#define hashset_remove           typehash_remove
#define hashset_find             typehash_find
#define hashset_size             typehash_size
#define hashset_iterator_init    typehash_iterator_init
#define hashset_iterator_next    typehash_iterator_next
#define hashset_remove_iterator  typehash_remove_iterator

#include "adt/hashset.c"

static type_hash_t typehash;

void init_typehash(void)
{
	_typehash_init(&typehash);
}

void exit_typehash(void)
{
	_typehash_destroy(&typehash);
}

type_t *typehash_insert(type_t *type)
{
	return _typehash_insert(&typehash, type);
}
