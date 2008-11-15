/*
 * This file is part of cparser.
 * Copyright (C) 2007-2008 Matthias Braun <matze@braunis.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
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

typedef struct type_hash_iterator_t  type_hash_iterator_t;
typedef struct type_hash_t           type_hash_t;

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
	unsigned result     = type->akind * some_prime;

	return result;
}

static unsigned hash_complex_type(const complex_type_t *type)
{
	unsigned some_prime = 27644437;
	unsigned result     = type->akind * some_prime;

	return result;
}

static unsigned hash_imaginary_type(const imaginary_type_t *type)
{
	unsigned some_prime = 27644437;
	unsigned result     = type->akind * some_prime;

	return result;
}

static unsigned hash_pointer_type(const pointer_type_t *type)
{
	return hash_ptr(type->points_to);
}

static unsigned hash_array_type(const array_type_t *type)
{
	return hash_ptr(type->element_type);
}

static unsigned hash_compound_type(const compound_type_t *type)
{
	return hash_ptr(type->compound);
}

static unsigned hash_type(const type_t *type);

static unsigned hash_function_type(const function_type_t *type)
{
	unsigned result = hash_ptr(type->return_type);

	function_parameter_t *parameter = type->parameters;
	while (parameter != NULL) {
		result   ^= hash_ptr(parameter->type);
		parameter = parameter->next;
	}
	result += type->linkage;
	result += type->calling_convention;

	return result;
}

static unsigned hash_enum_type(const enum_type_t *type)
{
	return hash_ptr(type->enume);
}

static unsigned hash_typeof_type(const typeof_type_t *type)
{
	unsigned result = hash_ptr(type->expression);
	result         ^= hash_ptr(type->typeof_type);

	return result;
}

static unsigned hash_bitfield_type(const bitfield_type_t *type)
{
	unsigned result  = hash_ptr(type->base_type);
	result          ^= 27172145;

	return result;
}

static unsigned hash_type(const type_t *type)
{
	unsigned hash = 0;

	switch (type->kind) {
	case TYPE_INVALID:
		panic("internalizing void or invalid types not possible");
		return 0;
	case TYPE_ERROR:
		return 0;
	case TYPE_ATOMIC:
		hash = hash_atomic_type(&type->atomic);
		break;
	case TYPE_COMPLEX:
		hash = hash_complex_type(&type->complex);
		break;
	case TYPE_IMAGINARY:
		hash = hash_imaginary_type(&type->imaginary);
		break;
	case TYPE_ENUM:
		hash = hash_enum_type(&type->enumt);
		break;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		hash = hash_compound_type(&type->compound);
		break;
	case TYPE_FUNCTION:
		hash = hash_function_type(&type->function);
		break;
	case TYPE_POINTER:
		hash = hash_pointer_type(&type->pointer);
		break;
	case TYPE_ARRAY:
		hash = hash_array_type(&type->array);
		break;
	case TYPE_BUILTIN:
		hash = hash_ptr(type->builtin.symbol);
		break;
	case TYPE_TYPEDEF:
		hash = hash_ptr(type->typedeft.typedefe);
		break;
	case TYPE_TYPEOF:
		hash = hash_typeof_type(&type->typeoft);
		break;
	case TYPE_BITFIELD:
		hash = hash_bitfield_type(&type->bitfield);
		break;
	}

	unsigned some_prime = 99991;
	hash ^= some_prime * type->base.qualifiers;

	return hash;
}

static bool atomic_types_equal(const atomic_type_t *type1,
							   const atomic_type_t *type2)
{
	return type1->akind == type2->akind;
}

static bool complex_types_equal(const complex_type_t *type1,
							    const complex_type_t *type2)
{
	return type1->akind == type2->akind;
}

static bool imaginary_types_equal(const imaginary_type_t *type1,
							      const imaginary_type_t *type2)
{
	return type1->akind == type2->akind;
}

static bool function_types_equal(const function_type_t *type1,
                                 const function_type_t *type2)
{
	if (type1->return_type != type2->return_type)
		return false;
	if (type1->variadic != type2->variadic)
		return false;
	if (type1->unspecified_parameters != type2->unspecified_parameters)
		return false;
	if (type1->kr_style_parameters != type2->kr_style_parameters)
		return false;
	if (type1->linkage != type2->linkage)
		return false;
	if (type1->calling_convention != type2->calling_convention)
		return false;

	function_parameter_t *param1 = type1->parameters;
	function_parameter_t *param2 = type2->parameters;
	while (param1 != NULL && param2 != NULL) {
		if (param1->type != param2->type)
			return false;
		param1 = param1->next;
		param2 = param2->next;
	}
	if (param1 != NULL || param2 != NULL)
		return false;

	return true;
}

static bool pointer_types_equal(const pointer_type_t *type1,
                                const pointer_type_t *type2)
{
	return type1->points_to == type2->points_to;
}

static bool array_types_equal(const array_type_t *type1,
                              const array_type_t *type2)
{
	if (type1->element_type != type2->element_type)
		return false;
	if (type1->is_variable != type2->is_variable)
		return false;
	if (type1->is_static != type2->is_static)
		return false;
	if (type1->size_constant != type2->size_constant)
		return false;

	/* never identify vla types, because we need them for caching calculated
	 * sizes later in ast2firm */
	if (type1->is_vla || type2->is_vla)
		return false;

	/* TODO: compare size expressions for equality... */

	return false;
}

static bool builtin_types_equal(const builtin_type_t *type1,
                                const builtin_type_t *type2)
{
	return type1->symbol == type2->symbol;
}

static bool compound_types_equal(const compound_type_t *type1,
                                 const compound_type_t *type2)
{
	return type1->compound == type2->compound;
}

static bool enum_types_equal(const enum_type_t *type1,
                             const enum_type_t *type2)
{
	return type1->enume == type2->enume;
}

static bool typedef_types_equal(const typedef_type_t *type1,
                                const typedef_type_t *type2)
{
	return type1->typedefe == type2->typedefe;
}

static bool typeof_types_equal(const typeof_type_t *type1,
                               const typeof_type_t *type2)
{
	if (type1->expression != type2->expression)
		return false;
	if (type1->typeof_type != type2->typeof_type)
		return false;

	return true;
}

static bool bitfield_types_equal(const bitfield_type_t *type1,
                                 const bitfield_type_t *type2)
{
	if (type1->base_type != type2->base_type)
		return false;
	/* TODO: compare size expression */
	return false;
}

static bool types_equal(const type_t *type1, const type_t *type2)
{
	if (type1 == type2)
		return true;
	if (type1->kind != type2->kind)
		return false;
	if (type1->base.qualifiers != type2->base.qualifiers)
		return false;
	if (type1->base.modifiers != type2->base.modifiers)
		return false;

	switch (type1->kind) {
	case TYPE_ERROR:
		/* Hmm, the error type is never equal */
		return false;
	case TYPE_INVALID:
		return false;
	case TYPE_ATOMIC:
		return atomic_types_equal(&type1->atomic, &type2->atomic);
	case TYPE_COMPLEX:
		return complex_types_equal(&type1->complex, &type2->complex);
	case TYPE_IMAGINARY:
		return imaginary_types_equal(&type1->imaginary, &type2->imaginary);
	case TYPE_ENUM:
		return enum_types_equal(&type1->enumt, &type2->enumt);
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return compound_types_equal(&type1->compound, &type2->compound);
	case TYPE_FUNCTION:
		return function_types_equal(&type1->function, &type2->function);
	case TYPE_POINTER:
		return pointer_types_equal(&type1->pointer, &type2->pointer);
	case TYPE_ARRAY:
		return array_types_equal(&type1->array, &type2->array);
	case TYPE_BUILTIN:
		return builtin_types_equal(&type1->builtin, &type2->builtin);
	case TYPE_TYPEOF:
		return typeof_types_equal(&type1->typeoft, &type2->typeoft);
	case TYPE_TYPEDEF:
		return typedef_types_equal(&type1->typedeft, &type2->typedeft);
	case TYPE_BITFIELD:
		return bitfield_types_equal(&type1->bitfield, &type2->bitfield);
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
#define SCALAR_RETURN

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
