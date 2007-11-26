#include <config.h>

#include <stdbool.h>

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

static unsigned hash_atomic_type(const type_t *type)
{
	unsigned some_prime = 27644437;
	unsigned result     = type->v.atomic_type.atype * some_prime;

	return result;
}

static unsigned hash_pointer_type(const type_t *type)
{
	return hash_ptr(type->v.pointer_type.points_to);
}

static unsigned hash_array_type(const type_t *type)
{
	return hash_ptr(type->v.array_type.element_type);
}

static unsigned hash_compound_type(const type_t *type)
{
	return hash_ptr(type->v.compound_type.declaration);
}

static unsigned hash_type(const type_t *type);

static unsigned hash_function_type(const type_t *type)
{
	unsigned result = hash_ptr(type->v.function_type.result_type);

	function_parameter_t *parameter = type->v.function_type.parameters;
	while(parameter != NULL) {
		result   ^= hash_ptr(parameter->type);
		parameter = parameter->next;
	}

	return result;
}

static unsigned hash_enum_type(const type_t *type)
{
	return hash_ptr(type->v.enum_type.declaration);
}

static unsigned hash_typeof_type(const type_t *type)
{
	unsigned result = hash_ptr(type->v.typeof_type.expression);
	result         ^= hash_ptr(type->v.typeof_type.typeof_type);

	return result;
}

static unsigned hash_type(const type_t *type)
{
	unsigned hash = 0;

	switch(type->type) {
	case TYPE_INVALID:
		panic("internalizing void or invalid types not possible");
		return 0;
	case TYPE_ATOMIC:
		hash = hash_atomic_type(type);
		break;
	case TYPE_ENUM:
		hash = hash_enum_type(type);
		break;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		hash = hash_compound_type(type);
		break;
	case TYPE_FUNCTION:
		hash = hash_function_type(type);
		break;
	case TYPE_POINTER:
		hash = hash_pointer_type(type);
		break;
	case TYPE_ARRAY:
		hash = hash_array_type(type);
		break;
	case TYPE_BUILTIN:
		hash = hash_ptr(type->v.builtin_type.symbol);
		break;
	case TYPE_TYPEDEF:
		hash = hash_ptr(type->v.typedef_type.declaration);
		break;
	case TYPE_TYPEOF:
		hash = hash_typeof_type(type);
		break;
	}

	unsigned some_prime = 99991;
	hash ^= some_prime * type->qualifiers;

	return hash;
}

static bool atomic_types_equal(const type_t *type1,
                               const type_t *type2)
{
	return type1->v.atomic_type.atype == type2->v.atomic_type.atype;
}

static bool function_types_equal(const type_t *type1,
                                 const type_t *type2)
{
	if(type1->v.function_type.result_type != type2->v.function_type.result_type)
		return false;
	if(type1->v.function_type.variadic != type2->v.function_type.variadic)
		return false;
	if(type1->v.function_type.unspecified_parameters !=
	   type2->v.function_type.unspecified_parameters)
		return false;

	function_parameter_t *param1 = type1->v.function_type.parameters;
	function_parameter_t *param2 = type2->v.function_type.parameters;
	while(param1 != NULL && param2 != NULL) {
		if(param1->type != param2->type)
			return false;
		param1 = param1->next;
		param2 = param2->next;
	}
	if(param1 != NULL || param2 != NULL)
		return false;

	return true;
}

static bool pointer_types_equal(const type_t *type1,
                                const type_t *type2)
{
	return type1->v.pointer_type.points_to == type2->v.pointer_type.points_to;
}

static bool array_types_equal(const type_t *type1,
                              const type_t *type2)
{
	if(type1->v.array_type.element_type != type2->v.array_type.element_type)
		return false;
	if(type1->v.array_type.is_variable != type2->v.array_type.is_variable)
		return false;
	if(type1->v.array_type.is_static != type2->v.array_type.is_static)
		return false;
	/* TODO: compare expressions for equality... */
	if(type1->v.array_type.size != type2->v.array_type.size)
		return false;

	return true;
}

static bool builtin_types_equal(const type_t *type1,
                                const type_t *type2)
{
	return type1->v.builtin_type.symbol == type2->v.builtin_type.symbol;
}

static bool compound_types_equal(const type_t *type1,
                                 const type_t *type2)
{
	return type1->v.compound_type.declaration == type2->v.compound_type.declaration;
}

static bool enum_types_equal(const type_t *type1,
                             const type_t *type2)
{
	return type1->v.enum_type.declaration == type2->v.enum_type.declaration;
}

static bool typedef_types_equal(const type_t *type1,
                                const type_t *type2)
{
	return type1->v.typedef_type.declaration == type2->v.typedef_type.declaration;
}

static bool typeof_types_equal(const type_t *type1,
                               const type_t *type2)
{
	if(type1->v.typeof_type.expression != type2->v.typeof_type.expression)
		return false;
	if(type1->v.typeof_type.typeof_type != type2->v.typeof_type.typeof_type)
		return false;

	return true;
}

static bool types_equal(const type_t *type1, const type_t *type2)
{
	if(type1 == type2)
		return true;
	if(type1->type != type2->type)
		return false;
	if(type1->qualifiers != type2->qualifiers)
		return false;

	switch(type1->type) {
	case TYPE_INVALID:
		return false;
	case TYPE_ATOMIC:
		return atomic_types_equal(type1, type2);
	case TYPE_ENUM:
		return enum_types_equal(type1, type2);
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return compound_types_equal(type1, type2);
	case TYPE_FUNCTION:
		return function_types_equal(type1, type2);
	case TYPE_POINTER:
		return pointer_types_equal(type1, type2);
	case TYPE_ARRAY:
		return array_types_equal(type1, type2);
	case TYPE_BUILTIN:
		return builtin_types_equal(type1, type2);
	case TYPE_TYPEOF:
		return typeof_types_equal(type1, type2);
	case TYPE_TYPEDEF:
		return typedef_types_equal(type1, type2);
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
