/*
 * This file is part of cparser.
 * Copyright (C) 2007-2009 Matthias Braun <matze@braunis.de>
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

#include <stdio.h>
#include <assert.h>

#include "type_t.h"
#include "types.h"
#include "entity_t.h"
#include "symbol_t.h"
#include "type_hash.h"
#include "adt/error.h"
#include "adt/util.h"
#include "lang_features.h"
#include "warning.h"
#include "diagnostic.h"
#include "printer.h"
#include "separator_t.h"

/** The default calling convention. */
cc_kind_t default_calling_convention = CC_CDECL;

static struct obstack type_obst;
static bool           print_implicit_array_size = false;

static void intern_print_type_pre(const type_t *type);
static void intern_print_type_post(const type_t *type);

/**
 * Returns the size of a type node.
 *
 * @param kind  the type kind
 */
static size_t get_type_struct_size(type_kind_t kind)
{
	static const size_t sizes[] = {
		[TYPE_ATOMIC]          = sizeof(atomic_type_t),
		[TYPE_IMAGINARY]       = sizeof(atomic_type_t),
		[TYPE_COMPLEX]         = sizeof(atomic_type_t),
		[TYPE_COMPOUND_STRUCT] = sizeof(compound_type_t),
		[TYPE_COMPOUND_UNION]  = sizeof(compound_type_t),
		[TYPE_ENUM]            = sizeof(enum_type_t),
		[TYPE_FUNCTION]        = sizeof(function_type_t),
		[TYPE_POINTER]         = sizeof(pointer_type_t),
		[TYPE_REFERENCE]       = sizeof(reference_type_t),
		[TYPE_ARRAY]           = sizeof(array_type_t),
		[TYPE_TYPEDEF]         = sizeof(typedef_type_t),
		[TYPE_TYPEOF]          = sizeof(typeof_type_t),
	};
	assert(lengthof(sizes) == (int)TYPE_TYPEOF + 1);
	assert(kind <= TYPE_TYPEOF);
	assert(sizes[kind] != 0);
	return sizes[kind];
}

type_t *allocate_type_zero(type_kind_t kind)
{
	size_t  const size = get_type_struct_size(kind);
	type_t *const res  = obstack_alloc(&type_obst, size);
	memset(res, 0, size);
	res->base.kind = kind;

	return res;
}

/**
 * Properties of atomic types.
 */
atomic_type_properties_t atomic_type_properties[ATOMIC_TYPE_LAST+1] = {
	[ATOMIC_TYPE_VOID] = {
		.size      = 1,
		.alignment = 1,
		.flags     = ATOMIC_TYPE_FLAG_NONE,
		.rank      = 0,
	},
	[ATOMIC_TYPE_BOOL] = {
		.size       = 1,
		.alignment  = 1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
		.rank       = 1,
	},
	[ATOMIC_TYPE_CHAR] = {
		.size      = 1,
		.alignment = 1,
		.flags     = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
		.rank      = 2,
	},
	[ATOMIC_TYPE_SCHAR] = {
		.size      = 1,
		.alignment = 1,
		.flags     = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC
		           | ATOMIC_TYPE_FLAG_SIGNED,
		.rank      = 2,
	},
	[ATOMIC_TYPE_UCHAR] = {
		.size      = 1,
		.alignment = 1,
		.flags     = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
		.rank      = 2,
	},
	[ATOMIC_TYPE_SHORT] = {
		.size       = 2,
		.alignment  = 2,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 3,
	},
	[ATOMIC_TYPE_USHORT] = {
		.size       = 2,
		.alignment  = 2,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
		.rank       = 3,
	},
	[ATOMIC_TYPE_INT] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 4,
	},
	[ATOMIC_TYPE_UINT] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
		.rank       = 4,
	},
	[ATOMIC_TYPE_LONG] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 5,
	},
	[ATOMIC_TYPE_ULONG] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
		.rank       = 5,
	},
	[ATOMIC_TYPE_LONGLONG] = {
		.size       = 8,
		.alignment  = 8,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 6,
	},
	[ATOMIC_TYPE_ULONGLONG] = {
		.size       = 8,
		.alignment  = 8,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
		.rank       = 6,
	},
	[ATOMIC_TYPE_FLOAT] = {
		.size       = 4,
		.alignment  = 4,
		.flags      = ATOMIC_TYPE_FLAG_FLOAT | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 0,
	},
	[ATOMIC_TYPE_DOUBLE] = {
		.size       = 8,
		.alignment  = 8,
		.flags      = ATOMIC_TYPE_FLAG_FLOAT | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 0,
	},
	[ATOMIC_TYPE_WCHAR_T] = {
		.size      = (unsigned)-1,
		.alignment = (unsigned)-1,
		.flags     = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
		.rank      = (unsigned)-1,
	},
};
atomic_type_properties_t pointer_properties = {
	.size      = 4,
	.alignment = 4,
	.flags     = ATOMIC_TYPE_FLAG_NONE,
};

static inline bool is_po2(unsigned x)
{
	return (x & (x-1)) == 0;
}

void init_types(unsigned machine_size)
{
	obstack_init(&type_obst);

	atomic_type_properties_t *props = atomic_type_properties;

	/* atempt to set some sane defaults based on machine size */

	unsigned int_size   = machine_size < 32 ? 2 : 4;
	unsigned long_size  = machine_size < 64 ? 4 : 8;

	props[ATOMIC_TYPE_INT].size        = int_size;
	props[ATOMIC_TYPE_INT].alignment   = int_size;
	props[ATOMIC_TYPE_UINT].size       = int_size;
	props[ATOMIC_TYPE_UINT].alignment  = int_size;
	props[ATOMIC_TYPE_LONG].size       = long_size;
	props[ATOMIC_TYPE_LONG].alignment  = long_size;
	props[ATOMIC_TYPE_ULONG].size      = long_size;
	props[ATOMIC_TYPE_ULONG].alignment = long_size;

	pointer_properties.size             = long_size;
	pointer_properties.alignment        = long_size;
	pointer_properties.struct_alignment = long_size;

	props[ATOMIC_TYPE_LONG_DOUBLE] = props[ATOMIC_TYPE_DOUBLE];
	props[ATOMIC_TYPE_WCHAR_T]     = props[ATOMIC_TYPE_INT];

	/* set struct alignments to the same value as alignment */
	for (size_t i = 0; i != lengthof(atomic_type_properties); ++i) {
		props[i].struct_alignment = props[i].alignment;
	}
}

void exit_types(void)
{
	obstack_free(&type_obst, NULL);
}

void print_type_qualifiers(type_qualifiers_t const qualifiers, QualifierSeparators const q)
{
	size_t sep = q & QUAL_SEP_START ? 0 : 1;
	if (qualifiers & TYPE_QUALIFIER_CONST) {
		print_string(&" const"[sep]);
		sep = 0;
	}
	if (qualifiers & TYPE_QUALIFIER_VOLATILE) {
		print_string(&" volatile"[sep]);
		sep = 0;
	}
	if (qualifiers & TYPE_QUALIFIER_RESTRICT) {
		print_string(&" restrict"[sep]);
		sep = 0;
	}
	if (sep == 0 && q & QUAL_SEP_END)
		print_char(' ');
}

const char *get_atomic_kind_name(atomic_type_kind_t kind)
{
	switch (kind) {
	case ATOMIC_TYPE_VOID:        return "void";
	case ATOMIC_TYPE_WCHAR_T:     return "wchar_t";
	case ATOMIC_TYPE_BOOL:        return c_mode & _CXX ? "bool" : "_Bool";
	case ATOMIC_TYPE_CHAR:        return "char";
	case ATOMIC_TYPE_SCHAR:       return "signed char";
	case ATOMIC_TYPE_UCHAR:       return "unsigned char";
	case ATOMIC_TYPE_INT:         return "int";
	case ATOMIC_TYPE_UINT:        return "unsigned int";
	case ATOMIC_TYPE_SHORT:       return "short";
	case ATOMIC_TYPE_USHORT:      return "unsigned short";
	case ATOMIC_TYPE_LONG:        return "long";
	case ATOMIC_TYPE_ULONG:       return "unsigned long";
	case ATOMIC_TYPE_LONGLONG:    return "long long";
	case ATOMIC_TYPE_ULONGLONG:   return "unsigned long long";
	case ATOMIC_TYPE_LONG_DOUBLE: return "long double";
	case ATOMIC_TYPE_FLOAT:       return "float";
	case ATOMIC_TYPE_DOUBLE:      return "double";
	}
	return "INVALIDATOMIC";
}

/**
 * Prints the name of an atomic type kinds.
 *
 * @param kind  The type kind.
 */
static void print_atomic_kinds(atomic_type_kind_t kind)
{
	const char *s = get_atomic_kind_name(kind);
	print_string(s);
}

/**
 * Prints the name of an atomic type.
 *
 * @param type  The type.
 */
static void print_atomic_type(const atomic_type_t *type)
{
	print_type_qualifiers(type->base.qualifiers, QUAL_SEP_END);
	print_atomic_kinds(type->akind);
}

/**
 * Prints the name of a complex type.
 *
 * @param type  The type.
 */
static void print_complex_type(const atomic_type_t *type)
{
	print_type_qualifiers(type->base.qualifiers, QUAL_SEP_END);
	print_string("_Complex ");
	print_atomic_kinds(type->akind);
}

/**
 * Prints the name of an imaginary type.
 *
 * @param type  The type.
 */
static void print_imaginary_type(const atomic_type_t *type)
{
	print_type_qualifiers(type->base.qualifiers, QUAL_SEP_END);
	print_string("_Imaginary ");
	print_atomic_kinds(type->akind);
}

/**
 * Print the first part (the prefix) of a type.
 *
 * @param type   The type to print.
 */
static void print_function_type_pre(const function_type_t *type)
{
	switch (type->linkage) {
		case LINKAGE_C:
			if (c_mode & _CXX)
				print_string("extern \"C\" ");
			break;

		case LINKAGE_CXX:
			if (!(c_mode & _CXX))
				print_string("extern \"C++\" ");
			break;
	}

	print_type_qualifiers(type->base.qualifiers, QUAL_SEP_END);

	intern_print_type_pre(type->return_type);

	cc_kind_t cc = type->calling_convention;
restart:
	switch (cc) {
	case CC_CDECL:    print_string(" __cdecl");    break;
	case CC_STDCALL:  print_string(" __stdcall");  break;
	case CC_FASTCALL: print_string(" __fastcall"); break;
	case CC_THISCALL: print_string(" __thiscall"); break;
	case CC_DEFAULT:
		if (default_calling_convention != CC_CDECL) {
			/* show the default calling convention if its not cdecl */
			cc = default_calling_convention;
			goto restart;
		}
		break;
	}
}

/**
 * Print the second part (the postfix) of a type.
 *
 * @param type   The type to print.
 */
static void print_function_type_post(const function_type_t *type,
                                     const scope_t *parameters)
{
	print_char('(');
	separator_t sep = { "", ", " };
	if (parameters == NULL) {
		function_parameter_t *parameter = type->parameters;
		for ( ; parameter != NULL; parameter = parameter->next) {
			print_string(sep_next(&sep));
			print_type(parameter->type);
		}
	} else {
		entity_t *parameter = parameters->entities;
		for (; parameter != NULL; parameter = parameter->base.next) {
			if (parameter->kind != ENTITY_PARAMETER)
				continue;

			print_string(sep_next(&sep));
			const type_t *const param_type = parameter->declaration.type;
			if (param_type == NULL) {
				print_string(parameter->base.symbol->string);
			} else {
				print_type_ext(param_type, parameter->base.symbol, NULL);
			}
		}
	}
	if (type->variadic) {
		print_string(sep_next(&sep));
		print_string("...");
	}
	if (sep_at_first(&sep) && !type->unspecified_parameters) {
		print_string("void");
	}
	print_char(')');

	intern_print_type_post(type->return_type);
}

/**
 * Prints the prefix part of a pointer type.
 *
 * @param type   The pointer type.
 */
static void print_pointer_type_pre(const pointer_type_t *type)
{
	type_t const *const points_to = type->points_to;
	intern_print_type_pre(points_to);
	if (points_to->kind == TYPE_ARRAY || points_to->kind == TYPE_FUNCTION)
		print_string(" (");
	variable_t *const variable = type->base_variable;
	if (variable != NULL) {
		print_string(" __based(");
		print_string(variable->base.base.symbol->string);
		print_string(") ");
	}
	print_char('*');
	print_type_qualifiers(type->base.qualifiers, QUAL_SEP_START);
}

/**
 * Prints the postfix part of a pointer type.
 *
 * @param type   The pointer type.
 */
static void print_pointer_type_post(const pointer_type_t *type)
{
	type_t const *const points_to = type->points_to;
	if (points_to->kind == TYPE_ARRAY || points_to->kind == TYPE_FUNCTION)
		print_char(')');
	intern_print_type_post(points_to);
}

/**
 * Prints the prefix part of a reference type.
 *
 * @param type   The reference type.
 */
static void print_reference_type_pre(const reference_type_t *type)
{
	type_t const *const refers_to = type->refers_to;
	intern_print_type_pre(refers_to);
	if (refers_to->kind == TYPE_ARRAY || refers_to->kind == TYPE_FUNCTION)
		print_string(" (");
	print_char('&');
}

/**
 * Prints the postfix part of a reference type.
 *
 * @param type   The reference type.
 */
static void print_reference_type_post(const reference_type_t *type)
{
	type_t const *const refers_to = type->refers_to;
	if (refers_to->kind == TYPE_ARRAY || refers_to->kind == TYPE_FUNCTION)
		print_char(')');
	intern_print_type_post(refers_to);
}

/**
 * Prints the prefix part of an array type.
 *
 * @param type   The array type.
 */
static void print_array_type_pre(const array_type_t *type)
{
	intern_print_type_pre(type->element_type);
}

/**
 * Prints the postfix part of an array type.
 *
 * @param type   The array type.
 */
static void print_array_type_post(const array_type_t *type)
{
	print_char('[');
	if (type->is_static) {
		print_string("static ");
	}
	print_type_qualifiers(type->base.qualifiers, QUAL_SEP_END);
	if (type->size_expression != NULL
			&& (print_implicit_array_size || !type->has_implicit_size)) {
		print_expression(type->size_expression);
	}
	print_char(']');
	intern_print_type_post(type->element_type);
}

void print_enum_definition(const enum_t *enume)
{
	print_string("{\n");

	change_indent(1);

	entity_t *entry = enume->base.next;
	for ( ; entry != NULL && entry->kind == ENTITY_ENUM_VALUE;
	       entry = entry->base.next) {

		print_indent();
		print_string(entry->base.symbol->string);
		if (entry->enum_value.value != NULL) {
			print_string(" = ");
			print_expression(entry->enum_value.value);
		}
		print_string(",\n");
	}

	change_indent(-1);
	print_indent();
	print_char('}');
}

/**
 * Prints an enum type.
 *
 * @param type  The enum type.
 */
static void print_type_enum(const enum_type_t *type)
{
	print_type_qualifiers(type->base.base.qualifiers, QUAL_SEP_END);
	print_string("enum ");

	enum_t   *enume  = type->enume;
	symbol_t *symbol = enume->base.symbol;
	if (symbol != NULL) {
		print_string(symbol->string);
	} else {
		print_enum_definition(enume);
	}
}

void print_compound_definition(const compound_t *compound)
{
	print_string("{\n");
	change_indent(1);

	entity_t *entity = compound->members.entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		if (entity->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		print_indent();
		print_entity(entity);
		print_char('\n');
	}

	change_indent(-1);
	print_indent();
	print_char('}');
	if (compound->modifiers & DM_TRANSPARENT_UNION) {
		print_string("__attribute__((__transparent_union__))");
	}
}

/**
 * Prints a compound type.
 *
 * @param kind  The name of the compound kind.
 * @param type  The compound type.
 */
static void print_compound_type(char const *const kind, compound_type_t const *const type)
{
	print_type_qualifiers(type->base.qualifiers, QUAL_SEP_END);
	print_string(kind);

	compound_t *compound = type->compound;
	symbol_t   *symbol   = compound->base.symbol;
	if (symbol != NULL) {
		print_string(symbol->string);
	} else {
		print_compound_definition(compound);
	}
}

/**
 * Prints the prefix part of a typedef type.
 *
 * @param type   The typedef type.
 */
static void print_typedef_type_pre(const typedef_type_t *const type)
{
	print_type_qualifiers(type->base.qualifiers, QUAL_SEP_END);
	print_string(type->typedefe->base.symbol->string);
}

/**
 * Prints the prefix part of a typeof type.
 *
 * @param type   The typeof type.
 */
static void print_typeof_type_pre(const typeof_type_t *const type)
{
	print_string("typeof(");
	if (type->expression != NULL) {
		print_expression(type->expression);
	} else {
		print_type(type->typeof_type);
	}
	print_char(')');
}

/**
 * Prints the prefix part of a type.
 *
 * @param type   The type.
 */
static void intern_print_type_pre(const type_t *const type)
{
	switch (type->kind) {
	case TYPE_ARRAY:           print_array_type_pre(          &type->array);     return;
	case TYPE_ATOMIC:          print_atomic_type(             &type->atomic);    return;
	case TYPE_COMPLEX:         print_complex_type(            &type->atomic);    return;
	case TYPE_COMPOUND_STRUCT: print_compound_type("struct ", &type->compound);  return;
	case TYPE_COMPOUND_UNION:  print_compound_type("union ",  &type->compound);  return;
	case TYPE_ENUM:            print_type_enum(               &type->enumt);     return;
	case TYPE_ERROR:           print_string("<error>");                          return;
	case TYPE_FUNCTION:        print_function_type_pre(       &type->function);  return;
	case TYPE_IMAGINARY:       print_imaginary_type(          &type->atomic);    return;
	case TYPE_POINTER:         print_pointer_type_pre(        &type->pointer);   return;
	case TYPE_REFERENCE:       print_reference_type_pre(      &type->reference); return;
	case TYPE_TYPEDEF:         print_typedef_type_pre(        &type->typedeft);  return;
	case TYPE_TYPEOF:          print_typeof_type_pre(         &type->typeoft);   return;
	}
	print_string("unknown");
}

/**
 * Prints the postfix part of a type.
 *
 * @param type   The type.
 */
static void intern_print_type_post(const type_t *const type)
{
	switch (type->kind) {
	case TYPE_FUNCTION:
		print_function_type_post(&type->function, NULL);
		return;
	case TYPE_POINTER:
		print_pointer_type_post(&type->pointer);
		return;
	case TYPE_REFERENCE:
		print_reference_type_post(&type->reference);
		return;
	case TYPE_ARRAY:
		print_array_type_post(&type->array);
		return;
	case TYPE_ERROR:
	case TYPE_ATOMIC:
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
	case TYPE_ENUM:
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
		break;
	}
}

void print_type(const type_t *const type)
{
	print_type_ext(type, NULL, NULL);
}

void print_type_ext(const type_t *const type, const symbol_t *symbol,
                    const scope_t *parameters)
{
	intern_print_type_pre(type);
	if (symbol != NULL) {
		print_char(' ');
		print_string(symbol->string);
	}
	if (type->kind == TYPE_FUNCTION) {
		print_function_type_post(&type->function, parameters);
	} else {
		intern_print_type_post(type);
	}
}

type_t *duplicate_type(const type_t *type)
{
	size_t size = get_type_struct_size(type->kind);

	type_t *const copy = obstack_copy(&type_obst, type, size);
	copy->base.firm_type = NULL;

	return copy;
}

type_t *get_unqualified_type(type_t *type)
{
	assert(!is_typeref(type));

	if (type->base.qualifiers == TYPE_QUALIFIER_NONE)
		return type;

	type_t *unqualified_type          = duplicate_type(type);
	unqualified_type->base.qualifiers = TYPE_QUALIFIER_NONE;

	return identify_new_type(unqualified_type);
}

type_t *get_qualified_type(type_t *orig_type, type_qualifiers_t const qual)
{
	type_t *type = skip_typeref(orig_type);

	type_t *copy;
	if (is_type_array(type)) {
		/* For array types the element type has to be adjusted */
		type_t *element_type      = type->array.element_type;
		type_t *qual_element_type = get_qualified_type(element_type, qual);

		if (qual_element_type == element_type)
			return orig_type;

		copy                     = duplicate_type(type);
		copy->array.element_type = qual_element_type;
	} else if (is_type_valid(type)) {
		if ((type->base.qualifiers & qual) == (int)qual)
			return orig_type;

		copy                   = duplicate_type(type);
		copy->base.qualifiers |= qual;
	} else {
		return type;
	}

	return identify_new_type(copy);
}

static bool test_atomic_type_flag(atomic_type_kind_t kind,
                                  atomic_type_flag_t flag)
{
	assert(kind <= ATOMIC_TYPE_LAST);
	return (atomic_type_properties[kind].flags & flag) != 0;
}

bool is_type_integer(const type_t *type)
{
	assert(!is_typeref(type));

	if (type->kind == TYPE_ENUM)
		return true;
	if (type->kind != TYPE_ATOMIC)
		return false;

	return test_atomic_type_flag(type->atomic.akind, ATOMIC_TYPE_FLAG_INTEGER);
}

bool is_type_enum(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_ENUM;
}

bool is_type_float(const type_t *type)
{
	assert(!is_typeref(type));

	if (type->kind != TYPE_ATOMIC)
		return false;

	return test_atomic_type_flag(type->atomic.akind, ATOMIC_TYPE_FLAG_FLOAT);
}

bool is_type_signed(const type_t *type)
{
	assert(!is_typeref(type));

	/* enum types are int for now */
	if (type->kind == TYPE_ENUM)
		return true;
	if (type->kind != TYPE_ATOMIC)
		return false;

	return test_atomic_type_flag(type->atomic.akind, ATOMIC_TYPE_FLAG_SIGNED);
}

bool is_type_arithmetic(const type_t *type)
{
	assert(!is_typeref(type));

	switch (type->kind) {
	case TYPE_ENUM:
		return true;
	case TYPE_ATOMIC:
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
		return test_atomic_type_flag(type->atomic.akind, ATOMIC_TYPE_FLAG_ARITHMETIC);
	default:
		return false;
	}
}

bool is_type_real(const type_t *type)
{
	/* 6.2.5 (17) */
	return is_type_integer(type) || is_type_float(type);
}

bool is_type_scalar(const type_t *type)
{
	assert(!is_typeref(type));

	switch (type->kind) {
	case TYPE_POINTER:
	case TYPE_ENUM:
		return true;
	case TYPE_ATOMIC:
	case TYPE_IMAGINARY:
		return test_atomic_type_flag(type->atomic.akind, ATOMIC_TYPE_FLAG_ARITHMETIC);
	default:
		return false;
	}
}

bool is_type_incomplete(const type_t *type)
{
	assert(!is_typeref(type));

	switch (type->kind) {
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION: {
		const compound_type_t *compound_type = &type->compound;
		return !compound_type->compound->complete;
	}
	case TYPE_ENUM:
		return false;

	case TYPE_ARRAY:
		return type->array.size_expression == NULL
			&& !type->array.size_constant;

	case TYPE_ATOMIC:
	case TYPE_IMAGINARY:
	case TYPE_COMPLEX:
		return type->atomic.akind == ATOMIC_TYPE_VOID;

	case TYPE_FUNCTION:
	case TYPE_POINTER:
	case TYPE_REFERENCE:
	case TYPE_ERROR:
		return false;

	case TYPE_TYPEDEF:
	case TYPE_TYPEOF:
		panic("typedef not skipped");
	}

	panic("invalid type");
}

bool is_type_object(const type_t *type)
{
	return !is_type_function(type) && !is_type_incomplete(type);
}

/**
 * Check if two function types are compatible.
 */
static bool function_types_compatible(const function_type_t *func1,
                                      const function_type_t *func2)
{
	const type_t* const ret1 = skip_typeref(func1->return_type);
	const type_t* const ret2 = skip_typeref(func2->return_type);
	if (!types_compatible(ret1, ret2))
		return false;

	if (func1->linkage != func2->linkage)
		return false;

	cc_kind_t cc1 = func1->calling_convention;
	if (cc1 == CC_DEFAULT)
		cc1 = default_calling_convention;
	cc_kind_t cc2 = func2->calling_convention;
	if (cc2 == CC_DEFAULT)
		cc2 = default_calling_convention;

	if (cc1 != cc2)
		return false;

	if (func1->variadic != func2->variadic)
		return false;

	/* can parameters be compared? */
	if ((func1->unspecified_parameters && !func1->kr_style_parameters)
			|| (func2->unspecified_parameters && !func2->kr_style_parameters))
		return true;

	/* TODO: handling of unspecified parameters not correct yet */

	/* all argument types must be compatible */
	function_parameter_t *parameter1 = func1->parameters;
	function_parameter_t *parameter2 = func2->parameters;
	for ( ; parameter1 != NULL && parameter2 != NULL;
			parameter1 = parameter1->next, parameter2 = parameter2->next) {
		type_t *parameter1_type = skip_typeref(parameter1->type);
		type_t *parameter2_type = skip_typeref(parameter2->type);

		parameter1_type = get_unqualified_type(parameter1_type);
		parameter2_type = get_unqualified_type(parameter2_type);

		if (!types_compatible(parameter1_type, parameter2_type))
			return false;
	}
	/* same number of arguments? */
	if (parameter1 != NULL || parameter2 != NULL)
		return false;

	return true;
}

/**
 * Check if two array types are compatible.
 */
static bool array_types_compatible(const array_type_t *array1,
                                   const array_type_t *array2)
{
	type_t *element_type1 = skip_typeref(array1->element_type);
	type_t *element_type2 = skip_typeref(array2->element_type);
	if (!types_compatible(element_type1, element_type2))
		return false;

	if (!array1->size_constant || !array2->size_constant)
		return true;

	return array1->size == array2->size;
}

bool types_compatible(const type_t *type1, const type_t *type2)
{
	assert(!is_typeref(type1));
	assert(!is_typeref(type2));

	/* shortcut: the same type is always compatible */
	if (type1 == type2)
		return true;

	if (type1->base.qualifiers == type2->base.qualifiers &&
	    type1->kind            == type2->kind) {
		switch (type1->kind) {
		case TYPE_FUNCTION:
			return function_types_compatible(&type1->function, &type2->function);
		case TYPE_ATOMIC:
		case TYPE_IMAGINARY:
		case TYPE_COMPLEX:
			return type1->atomic.akind == type2->atomic.akind;
		case TYPE_ARRAY:
			return array_types_compatible(&type1->array, &type2->array);

		case TYPE_POINTER: {
			const type_t *const to1 = skip_typeref(type1->pointer.points_to);
			const type_t *const to2 = skip_typeref(type2->pointer.points_to);
			return types_compatible(to1, to2);
		}

		case TYPE_REFERENCE: {
			const type_t *const to1 = skip_typeref(type1->reference.refers_to);
			const type_t *const to2 = skip_typeref(type2->reference.refers_to);
			return types_compatible(to1, to2);
		}

		case TYPE_COMPOUND_STRUCT:
		case TYPE_COMPOUND_UNION:
			break;

		case TYPE_ENUM:
			/* TODO: not implemented */
			break;

		case TYPE_ERROR:
			/* Hmm, the error type should be compatible to all other types */
			return true;
		case TYPE_TYPEDEF:
		case TYPE_TYPEOF:
			panic("typeref not skipped");
		}
	}

	return !is_type_valid(type1) || !is_type_valid(type2);
}

/**
 * Skip all typerefs and return the underlying type.
 */
type_t *skip_typeref(type_t *type)
{
	type_qualifiers_t qualifiers = TYPE_QUALIFIER_NONE;

	while (true) {
		switch (type->kind) {
		case TYPE_ERROR:
			return type;
		case TYPE_TYPEDEF: {
			qualifiers |= type->base.qualifiers;

			const typedef_type_t *typedef_type = &type->typedeft;
			if (typedef_type->resolved_type != NULL) {
				type = typedef_type->resolved_type;
				break;
			}
			type = typedef_type->typedefe->type;
			continue;
		}
		case TYPE_TYPEOF:
			qualifiers |= type->base.qualifiers;
			type        = type->typeoft.typeof_type;
			continue;
		default:
			break;
		}
		break;
	}

	if (qualifiers != TYPE_QUALIFIER_NONE) {
		type_t *const copy = duplicate_type(type);

		/* for const with typedefed array type the element type has to be
		 * adjusted */
		if (is_type_array(copy)) {
			type_t *element_type           = copy->array.element_type;
			element_type                   = duplicate_type(element_type);
			element_type->base.qualifiers |= qualifiers;
			copy->array.element_type       = element_type;
		} else {
			copy->base.qualifiers |= qualifiers;
		}

		type = identify_new_type(copy);
	}

	return type;
}

unsigned get_type_size(type_t *type)
{
	switch (type->kind) {
	case TYPE_ERROR:
		return 0;
	case TYPE_ATOMIC:
	case TYPE_IMAGINARY:
	case TYPE_ENUM:
		return get_atomic_type_size(type->atomic.akind);
	case TYPE_COMPLEX:
		return get_atomic_type_size(type->atomic.akind) * 2;
	case TYPE_COMPOUND_UNION:
		layout_union_type(&type->compound);
		return type->compound.compound->size;
	case TYPE_COMPOUND_STRUCT:
		layout_struct_type(&type->compound);
		return type->compound.compound->size;
	case TYPE_FUNCTION:
		return 1; /* strange GNU extensions: sizeof(function) == 1 */
	case TYPE_REFERENCE:
	case TYPE_POINTER:
		return pointer_properties.size;
	case TYPE_ARRAY: {
		/* TODO: correct if element_type is aligned? */
		il_size_t element_size = get_type_size(type->array.element_type);
		return type->array.size * element_size;
	}
	case TYPE_TYPEDEF:
		return get_type_size(type->typedeft.typedefe->type);
	case TYPE_TYPEOF:
		return get_type_size(type->typeoft.typeof_type);
	}
	panic("invalid type");
}

unsigned get_type_alignment(type_t *type)
{
	switch (type->kind) {
	case TYPE_ERROR:
		return 0;
	case TYPE_ATOMIC:
	case TYPE_IMAGINARY:
	case TYPE_COMPLEX:
	case TYPE_ENUM:
		return get_atomic_type_alignment(type->atomic.akind);
	case TYPE_COMPOUND_UNION:
		layout_union_type(&type->compound);
		return type->compound.compound->alignment;
	case TYPE_COMPOUND_STRUCT:
		layout_struct_type(&type->compound);
		return type->compound.compound->alignment;
	case TYPE_FUNCTION:
		/* gcc says 1 here... */
		return 1;
	case TYPE_REFERENCE:
	case TYPE_POINTER:
		return pointer_properties.alignment;
	case TYPE_ARRAY:
		return get_type_alignment(type->array.element_type);
	case TYPE_TYPEDEF: {
		il_alignment_t alignment
			= get_type_alignment(type->typedeft.typedefe->type);
		if (type->typedeft.typedefe->alignment > alignment)
			alignment = type->typedeft.typedefe->alignment;

		return alignment;
	}
	case TYPE_TYPEOF:
		return get_type_alignment(type->typeoft.typeof_type);
	}
	panic("invalid type");
}

/**
 * get alignment of a type when used inside a compound.
 * Some ABIs are broken and alignment inside a compound is different from
 * recommended alignment of a type
 */
static unsigned get_type_alignment_compound(type_t *const type)
{
	assert(!is_typeref(type));
	if (type->kind == TYPE_ATOMIC)
		return atomic_type_properties[type->atomic.akind].struct_alignment;
	return get_type_alignment(type);
}

decl_modifiers_t get_type_modifiers(const type_t *type)
{
	switch (type->kind) {
	case TYPE_ERROR:
		break;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return type->compound.compound->modifiers;
	case TYPE_FUNCTION:
		return type->function.modifiers;
	case TYPE_ENUM:
	case TYPE_ATOMIC:
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
	case TYPE_REFERENCE:
	case TYPE_POINTER:
	case TYPE_ARRAY:
		return 0;
	case TYPE_TYPEDEF: {
		decl_modifiers_t modifiers = type->typedeft.typedefe->modifiers;
		modifiers |= get_type_modifiers(type->typedeft.typedefe->type);
		return modifiers;
	}
	case TYPE_TYPEOF:
		return get_type_modifiers(type->typeoft.typeof_type);
	}
	panic("invalid type");
}

type_qualifiers_t get_type_qualifier(const type_t *type, bool skip_array_type)
{
	type_qualifiers_t qualifiers = TYPE_QUALIFIER_NONE;

	while (true) {
		switch (type->base.kind) {
		case TYPE_ERROR:
			return TYPE_QUALIFIER_NONE;
		case TYPE_TYPEDEF:
			qualifiers |= type->base.qualifiers;
			const typedef_type_t *typedef_type = &type->typedeft;
			if (typedef_type->resolved_type != NULL)
				type = typedef_type->resolved_type;
			else
				type = typedef_type->typedefe->type;
			continue;
		case TYPE_TYPEOF:
			type = type->typeoft.typeof_type;
			continue;
		case TYPE_ARRAY:
			if (skip_array_type) {
				type = type->array.element_type;
				continue;
			}
			break;
		default:
			break;
		}
		break;
	}
	return type->base.qualifiers | qualifiers;
}

unsigned get_atomic_type_size(atomic_type_kind_t kind)
{
	assert(kind <= ATOMIC_TYPE_LAST);
	return atomic_type_properties[kind].size;
}

unsigned get_atomic_type_alignment(atomic_type_kind_t kind)
{
	assert(kind <= ATOMIC_TYPE_LAST);
	return atomic_type_properties[kind].alignment;
}

unsigned get_atomic_type_flags(atomic_type_kind_t kind)
{
	assert(kind <= ATOMIC_TYPE_LAST);
	return atomic_type_properties[kind].flags;
}

/**
 * Find the atomic type kind representing a given size (signed).
 */
atomic_type_kind_t find_signed_int_atomic_type_kind_for_size(unsigned size)
{
	static atomic_type_kind_t kinds[32];

	assert(size < 32);
	atomic_type_kind_t kind = kinds[size];
	if (kind == (atomic_type_kind_t)0) {
		static const atomic_type_kind_t possible_kinds[] = {
			ATOMIC_TYPE_SCHAR,
			ATOMIC_TYPE_SHORT,
			ATOMIC_TYPE_INT,
			ATOMIC_TYPE_LONG,
			ATOMIC_TYPE_LONGLONG
		};
		for (size_t i = 0; i < lengthof(possible_kinds); ++i) {
			if (get_atomic_type_size(possible_kinds[i]) == size) {
				kind = possible_kinds[i];
				break;
			}
		}
		kinds[size] = kind;
	}
	return kind;
}

/**
 * Find the atomic type kind representing a given size (signed).
 */
atomic_type_kind_t find_unsigned_int_atomic_type_kind_for_size(unsigned size)
{
	static atomic_type_kind_t kinds[32];

	assert(size < 32);
	atomic_type_kind_t kind = kinds[size];
	if (kind == (atomic_type_kind_t)0) {
		static const atomic_type_kind_t possible_kinds[] = {
			ATOMIC_TYPE_UCHAR,
			ATOMIC_TYPE_USHORT,
			ATOMIC_TYPE_UINT,
			ATOMIC_TYPE_ULONG,
			ATOMIC_TYPE_ULONGLONG
		};
		for (size_t i = 0; i < lengthof(possible_kinds); ++i) {
			if (get_atomic_type_size(possible_kinds[i]) == size) {
				kind = possible_kinds[i];
				break;
			}
		}
		kinds[size] = kind;
	}
	return kind;
}

/**
 * Hash the given type and return the "singleton" version
 * of it.
 */
type_t *identify_new_type(type_t *type)
{
	type_t *result = typehash_insert(type);
	if (result != type) {
		obstack_free(&type_obst, type);
	}
	return result;
}

/**
 * Creates a new atomic type.
 *
 * @param akind       The kind of the atomic type.
 * @param qualifiers  Type qualifiers for the new type.
 */
type_t *make_atomic_type(atomic_type_kind_t akind, type_qualifiers_t qualifiers)
{
	type_t *const type = allocate_type_zero(TYPE_ATOMIC);
	type->base.qualifiers = qualifiers;
	type->atomic.akind    = akind;

	return identify_new_type(type);
}

/**
 * Creates a new complex type.
 *
 * @param akind       The kind of the atomic type.
 * @param qualifiers  Type qualifiers for the new type.
 */
type_t *make_complex_type(atomic_type_kind_t akind,
                          type_qualifiers_t qualifiers)
{
	type_t *const type = allocate_type_zero(TYPE_COMPLEX);
	type->base.qualifiers = qualifiers;
	type->atomic.akind   = akind;

	return identify_new_type(type);
}

/**
 * Creates a new imaginary type.
 *
 * @param akind       The kind of the atomic type.
 * @param qualifiers  Type qualifiers for the new type.
 */
type_t *make_imaginary_type(atomic_type_kind_t akind,
                            type_qualifiers_t qualifiers)
{
	type_t *const type = allocate_type_zero(TYPE_IMAGINARY);
	type->base.qualifiers = qualifiers;
	type->atomic.akind = akind;

	return identify_new_type(type);
}

/**
 * Creates a new pointer type.
 *
 * @param points_to   The points-to type for the new type.
 * @param qualifiers  Type qualifiers for the new type.
 */
type_t *make_pointer_type(type_t *points_to, type_qualifiers_t qualifiers)
{
	type_t *const type = allocate_type_zero(TYPE_POINTER);
	type->base.qualifiers       = qualifiers;
	type->pointer.points_to     = points_to;
	type->pointer.base_variable = NULL;

	return identify_new_type(type);
}

/**
 * Creates a new reference type.
 *
 * @param refers_to   The referred-to type for the new type.
 */
type_t *make_reference_type(type_t *refers_to)
{
	type_t *const type = allocate_type_zero(TYPE_REFERENCE);
	type->base.qualifiers     = TYPE_QUALIFIER_NONE;
	type->reference.refers_to = refers_to;

	return identify_new_type(type);
}

/**
 * Creates a new based pointer type.
 *
 * @param points_to   The points-to type for the new type.
 * @param qualifiers  Type qualifiers for the new type.
 * @param variable    The based variable
 */
type_t *make_based_pointer_type(type_t *points_to,
								type_qualifiers_t qualifiers, variable_t *variable)
{
	type_t *const type = allocate_type_zero(TYPE_POINTER);
	type->base.qualifiers       = qualifiers;
	type->pointer.points_to     = points_to;
	type->pointer.base_variable = variable;

	return identify_new_type(type);
}


type_t *make_array_type(type_t *element_type, size_t size,
                        type_qualifiers_t qualifiers)
{
	type_t *const type = allocate_type_zero(TYPE_ARRAY);
	type->base.qualifiers     = qualifiers;
	type->array.element_type  = element_type;
	type->array.size          = size;
	type->array.size_constant = true;

	return identify_new_type(type);
}

static entity_t *pack_bitfield_members(il_size_t *struct_offset,
                                       il_alignment_t *struct_alignment,
									   bool packed, entity_t *first)
{
	il_size_t      offset     = *struct_offset;
	il_alignment_t alignment  = *struct_alignment;
	size_t         bit_offset = 0;

	entity_t *member;
	for (member = first; member != NULL; member = member->base.next) {
		if (member->kind != ENTITY_COMPOUND_MEMBER)
			continue;
		if (!member->compound_member.bitfield)
			break;

		type_t *const base_type = skip_typeref(member->declaration.type);
		il_alignment_t base_alignment = get_type_alignment_compound(base_type);
		il_alignment_t alignment_mask = base_alignment-1;
		if (base_alignment > alignment)
			alignment = base_alignment;

		size_t bit_size = member->compound_member.bit_size;
		if (!packed) {
			bit_offset += (offset & alignment_mask) * BITS_PER_BYTE;
			offset     &= ~alignment_mask;
			size_t base_size = get_type_size(base_type) * BITS_PER_BYTE;

			if (bit_offset + bit_size > base_size || bit_size == 0) {
				offset    += (bit_offset+BITS_PER_BYTE-1) / BITS_PER_BYTE;
				offset     = (offset + base_alignment-1) & ~alignment_mask;
				bit_offset = 0;
			}
		}

		if (byte_order_big_endian) {
			size_t base_size = get_type_size(base_type) * BITS_PER_BYTE;
			member->compound_member.offset     = offset & ~alignment_mask;
			member->compound_member.bit_offset = base_size - bit_offset - bit_size;
		} else {
			member->compound_member.offset     = offset;
			member->compound_member.bit_offset = bit_offset;
		}

		bit_offset += bit_size;
		offset     += bit_offset / BITS_PER_BYTE;
		bit_offset %= BITS_PER_BYTE;
	}

	if (bit_offset > 0)
		offset += 1;

	*struct_offset    = offset;
	*struct_alignment = alignment;
	return member;
}

void layout_struct_type(compound_type_t *type)
{
	assert(type->compound != NULL);

	compound_t *compound = type->compound;
	if (!compound->complete)
		return;
	if (type->compound->layouted)
		return;
	compound->layouted = true;

	il_size_t      offset    = 0;
	il_alignment_t alignment = compound->alignment;
	bool           need_pad  = false;

	entity_t *entry = compound->members.entities;
	while (entry != NULL) {
		if (entry->kind != ENTITY_COMPOUND_MEMBER)
			goto next;

		type_t *const m_type = skip_typeref(entry->declaration.type);
		if (!is_type_valid(m_type))
			goto next;

		if (entry->compound_member.bitfield) {
			entry = pack_bitfield_members(&offset, &alignment,
			                              compound->packed, entry);
			continue;
		}

		il_alignment_t m_alignment = get_type_alignment_compound(m_type);
		if (m_alignment > alignment)
			alignment = m_alignment;

		if (!compound->packed) {
			il_size_t new_offset = (offset + m_alignment-1) & -m_alignment;

			if (new_offset > offset) {
				need_pad = true;
				offset   = new_offset;
			}
		}

		entry->compound_member.offset = offset;
		offset += get_type_size(m_type);

next:
		entry = entry->base.next;
	}

	if (!compound->packed) {
		il_size_t new_offset = (offset + alignment-1) & -alignment;
		if (new_offset > offset) {
			need_pad = true;
			offset   = new_offset;
		}
	}

	position_t const *const pos = &compound->base.pos;
	if (need_pad) {
		warningf(WARN_PADDED, pos, "'%T' needs padding", type);
	} else if (compound->packed) {
		warningf(WARN_PACKED, pos, "superfluous packed attribute on '%T'", type);
	}

	compound->size      = offset;
	compound->alignment = alignment;
}

void layout_union_type(compound_type_t *type)
{
	assert(type->compound != NULL);

	compound_t *compound = type->compound;
	if (! compound->complete)
		return;
	if (compound->layouted)
		return;
	compound->layouted = true;

	il_size_t      size      = 0;
	il_alignment_t alignment = compound->alignment;

	entity_t *entry = compound->members.entities;
	for (; entry != NULL; entry = entry->base.next) {
		if (entry->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		type_t *m_type = skip_typeref(entry->declaration.type);
		if (! is_type_valid(skip_typeref(m_type)))
			continue;

		entry->compound_member.offset = 0;
		il_size_t m_size = get_type_size(m_type);
		if (m_size > size)
			size = m_size;
		il_alignment_t m_alignment = get_type_alignment_compound(m_type);
		if (m_alignment > alignment)
			alignment = m_alignment;
	}
	size = (size + alignment - 1) & -alignment;

	compound->size      = size;
	compound->alignment = alignment;
}

function_parameter_t *allocate_parameter(type_t *const type)
{
	function_parameter_t *const param = obstack_alloc(&type_obst, sizeof(*param));
	memset(param, 0, sizeof(*param));
	param->type = type;
	return param;
}

type_t *make_function_2_type(type_t *return_type, type_t *argument_type1,
                             type_t *argument_type2, decl_modifiers_t modifiers)
{
	function_parameter_t *const parameter2 = allocate_parameter(argument_type2);
	function_parameter_t *const parameter1 = allocate_parameter(argument_type1);
	parameter1->next = parameter2;

	type_t *type               = allocate_type_zero(TYPE_FUNCTION);
	type->function.return_type = return_type;
	type->function.parameters  = parameter1;
	type->function.modifiers  |= modifiers;
	type->function.linkage     = LINKAGE_C;

	return identify_new_type(type);
}

type_t *make_function_1_type(type_t *return_type, type_t *argument_type,
                             decl_modifiers_t modifiers)
{
	function_parameter_t *const parameter = allocate_parameter(argument_type);

	type_t *type               = allocate_type_zero(TYPE_FUNCTION);
	type->function.return_type = return_type;
	type->function.parameters  = parameter;
	type->function.modifiers  |= modifiers;
	type->function.linkage     = LINKAGE_C;

	return identify_new_type(type);
}

type_t *make_function_1_type_variadic(type_t *return_type,
                                      type_t *argument_type,
                                      decl_modifiers_t modifiers)
{
	function_parameter_t *const parameter = allocate_parameter(argument_type);

	type_t *type               = allocate_type_zero(TYPE_FUNCTION);
	type->function.return_type = return_type;
	type->function.parameters  = parameter;
	type->function.variadic    = true;
	type->function.modifiers  |= modifiers;
	type->function.linkage     = LINKAGE_C;

	return identify_new_type(type);
}

type_t *make_function_0_type(type_t *return_type, decl_modifiers_t modifiers)
{
	type_t *type               = allocate_type_zero(TYPE_FUNCTION);
	type->function.return_type = return_type;
	type->function.parameters  = NULL;
	type->function.modifiers  |= modifiers;
	type->function.linkage     = LINKAGE_C;

	return identify_new_type(type);
}

type_t *make_function_type(type_t *return_type, int n_types,
                           type_t *const *argument_types,
						   decl_modifiers_t modifiers)
{
	type_t *type               = allocate_type_zero(TYPE_FUNCTION);
	type->function.return_type = return_type;
	type->function.modifiers  |= modifiers;
	type->function.linkage     = LINKAGE_C;

	function_parameter_t **anchor = &type->function.parameters;
	for (int i = 0; i < n_types; ++i) {
		function_parameter_t *parameter = allocate_parameter(argument_types[i]);
		*anchor = parameter;
		anchor  = &parameter->next;
	}

	return identify_new_type(type);
}

/**
 * Debug helper. Prints the given type to stdout.
 */
static __attribute__((unused))
void dbg_type(const type_t *type)
{
	print_to_file(stderr);
	print_type(type);
	print_char('\n');
	fflush(stderr);
}
