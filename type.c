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

#include <stdio.h>
#include <assert.h>

#include "type_t.h"
#include "symbol_t.h"
#include "type_hash.h"
#include "adt/error.h"
#include "lang_features.h"

static struct obstack   _type_obst;
static FILE            *out;
struct obstack         *type_obst                 = &_type_obst;
static int              type_visited              = 0;
static bool             print_implicit_array_size = false;

static void intern_print_type_pre(const type_t *type, bool top);
static void intern_print_type_post(const type_t *type, bool top);

typedef struct atomic_type_properties_t atomic_type_properties_t;
struct atomic_type_properties_t {
	unsigned   size;              /**< type size in bytes */
	unsigned   alignment;         /**< type alignment in bytes */
	unsigned   flags;             /**< type flags from atomic_type_flag_t */
};

static atomic_type_properties_t atomic_type_properties[ATOMIC_TYPE_LAST+1] = {
	//ATOMIC_TYPE_INVALID = 0,
	[ATOMIC_TYPE_VOID] = {
		.size       = 0,
		.alignment  = 0,
		.flags      = ATOMIC_TYPE_FLAG_NONE
	},
	[ATOMIC_TYPE_CHAR] = {
		.size       = 1,
		.alignment  = 1,
		/* signed flag will be set when known */
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
	},
	[ATOMIC_TYPE_SCHAR] = {
		.size       = 1,
		.alignment  = 1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
	},
	[ATOMIC_TYPE_UCHAR] = {
		.size       = 1,
		.alignment  = 1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
	},
	[ATOMIC_TYPE_SHORT] = {
		.size       = 2,
		.alignment  = 2,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED
	},
	[ATOMIC_TYPE_USHORT] = {
		.size       = 2,
		.alignment  = 2,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
	},
	[ATOMIC_TYPE_INT] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
	},
	[ATOMIC_TYPE_UINT] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
	},
	[ATOMIC_TYPE_LONG] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
	},
	[ATOMIC_TYPE_ULONG] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
	},
	[ATOMIC_TYPE_LONGLONG] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
	},
	[ATOMIC_TYPE_ULONGLONG] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
	},
	[ATOMIC_TYPE_BOOL] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC,
	},
	[ATOMIC_TYPE_FLOAT] = {
		.size       = 4,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_FLOAT | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
	},
	[ATOMIC_TYPE_DOUBLE] = {
		.size       = 8,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_FLOAT | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
	},
	[ATOMIC_TYPE_LONG_DOUBLE] = {
		.size       = 12,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_FLOAT | ATOMIC_TYPE_FLAG_ARITHMETIC
		              | ATOMIC_TYPE_FLAG_SIGNED,
	},
	/* complex and imaginary types are set in init_types */
};

void init_types(void)
{
	obstack_init(type_obst);

	atomic_type_properties_t *props = atomic_type_properties;

	if (char_is_signed) {
		props[ATOMIC_TYPE_CHAR].flags |= ATOMIC_TYPE_FLAG_SIGNED;
	}

	unsigned int_size   = machine_size < 32 ? 2 : 4;
	unsigned long_size  = machine_size < 64 ? 4 : 8;
	unsigned llong_size = machine_size < 32 ? 4 : 8;

	props[ATOMIC_TYPE_INT].size            = int_size;
	props[ATOMIC_TYPE_INT].alignment       = int_size;
	props[ATOMIC_TYPE_UINT].size           = int_size;
	props[ATOMIC_TYPE_UINT].alignment      = int_size;
	props[ATOMIC_TYPE_LONG].size           = long_size;
	props[ATOMIC_TYPE_LONG].alignment      = long_size;
	props[ATOMIC_TYPE_ULONG].size          = long_size;
	props[ATOMIC_TYPE_ULONG].alignment     = long_size;
	props[ATOMIC_TYPE_LONGLONG].size       = llong_size;
	props[ATOMIC_TYPE_LONGLONG].alignment  = llong_size;
	props[ATOMIC_TYPE_ULONGLONG].size      = llong_size;
	props[ATOMIC_TYPE_ULONGLONG].alignment = llong_size;

	/* TODO: backend specific, need a way to query the backend for this.
	 * The following are good settings for x86 */
	props[ATOMIC_TYPE_FLOAT].alignment       = 4;
	props[ATOMIC_TYPE_DOUBLE].alignment      = 4;
	props[ATOMIC_TYPE_LONG_DOUBLE].alignment = 4;
	props[ATOMIC_TYPE_LONGLONG].alignment    = 4;
	props[ATOMIC_TYPE_ULONGLONG].alignment   = 4;

	props[ATOMIC_TYPE_BOOL] = props[ATOMIC_TYPE_UINT];
}

void exit_types(void)
{
	obstack_free(type_obst, NULL);
}

void type_set_output(FILE *stream)
{
	out = stream;
}

void inc_type_visited(void)
{
	type_visited++;
}

void print_type_qualifiers(type_qualifiers_t qualifiers)
{
	int first = 1;
	if (qualifiers & TYPE_QUALIFIER_CONST) {
		fputs(" const" + first,    out);
		first = 0;
	}
	if (qualifiers & TYPE_QUALIFIER_VOLATILE) {
		fputs(" volatile" + first, out);
		first = 0;
	}
	if (qualifiers & TYPE_QUALIFIER_RESTRICT) {
		fputs(" restrict" + first, out);
		first = 0;
	}
}

/**
 * Prints the name of an atomic type kinds.
 *
 * @param kind  The type kind.
 */
static
void print_atomic_kinds(atomic_type_kind_t kind)
{
	const char *s = "INVALIDATOMIC";
	switch(kind) {
	case ATOMIC_TYPE_INVALID:                               break;
	case ATOMIC_TYPE_VOID:        s = "void";               break;
	case ATOMIC_TYPE_BOOL:        s = "_Bool";              break;
	case ATOMIC_TYPE_CHAR:        s = "char";               break;
	case ATOMIC_TYPE_SCHAR:       s = "signed char";        break;
	case ATOMIC_TYPE_UCHAR:       s = "unsigned char";      break;
	case ATOMIC_TYPE_INT:         s = "int";                break;
	case ATOMIC_TYPE_UINT:        s = "unsigned int";       break;
	case ATOMIC_TYPE_SHORT:       s = "short";              break;
	case ATOMIC_TYPE_USHORT:      s = "unsigned short";     break;
	case ATOMIC_TYPE_LONG:        s = "long";               break;
	case ATOMIC_TYPE_ULONG:       s = "unsigned long";      break;
	case ATOMIC_TYPE_LONGLONG:    s = "long long";          break;
	case ATOMIC_TYPE_ULONGLONG:   s = "unsigned long long"; break;
	case ATOMIC_TYPE_LONG_DOUBLE: s = "long double";        break;
	case ATOMIC_TYPE_FLOAT:       s = "float";              break;
	case ATOMIC_TYPE_DOUBLE:      s = "double";             break;
	}
	fputs(s, out);
}

/**
 * Prints the name of an atomic type.
 *
 * @param type  The type.
 */
static
void print_atomic_type(const atomic_type_t *type)
{
	print_type_qualifiers(type->base.qualifiers);
	if (type->base.qualifiers != 0)
		fputc(' ', out);
	print_atomic_kinds(type->akind);
}

/**
 * Prints the name of a complex type.
 *
 * @param type  The type.
 */
static
void print_complex_type(const complex_type_t *type)
{
	int empty = type->base.qualifiers == 0;
	print_type_qualifiers(type->base.qualifiers);
	fputs(" _Complex " + empty, out);
	print_atomic_kinds(type->akind);
}

/**
 * Prints the name of an imaginary type.
 *
 * @param type  The type.
 */
static
void print_imaginary_type(const imaginary_type_t *type)
{
	int empty = type->base.qualifiers == 0;
	print_type_qualifiers(type->base.qualifiers);
	fputs(" _Imaginary " + empty, out);
	print_atomic_kinds(type->akind);
}

/**
 * Print the first part (the prefix) of a type.
 *
 * @param type   The type to print.
 * @param top    true, if this is the top type, false if it's an embedded type.
 */
static void print_function_type_pre(const function_type_t *type, bool top)
{
	print_type_qualifiers(type->base.qualifiers);
	if (type->base.qualifiers != 0)
		fputc(' ', out);


	intern_print_type_pre(type->return_type, false);

	switch (type->calling_convention) {
	case CC_CDECL:
		fputs("__cdecl ", out);
		break;
	case CC_STDCALL:
		fputs("__stdcall ", out);
		break;
	case CC_FASTCALL:
		fputs("__fastcall ", out);
		break;
	case CC_THISCALL:
		fputs("__thiscall ", out);
		break;
	case CC_DEFAULT:
		break;
	}

	/* don't emit braces if we're the toplevel type... */
	if (!top)
		fputc('(', out);
}

/**
 * Print the second part (the postfix) of a type.
 *
 * @param type   The type to print.
 * @param top    true, if this is the top type, false if it's an embedded type.
 */
static void print_function_type_post(const function_type_t *type,
                                     const scope_t *scope, bool top)
{
	intern_print_type_post(type->return_type, false);

	fputc('(', out);
	bool first = true;
	if (scope == NULL) {
		function_parameter_t *parameter = type->parameters;
		for( ; parameter != NULL; parameter = parameter->next) {
			if (first) {
				first = false;
			} else {
				fputs(", ", out);
			}
			print_type(parameter->type);
		}
	} else {
		declaration_t *parameter = scope->declarations;
		for( ; parameter != NULL; parameter = parameter->next) {
			if (first) {
				first = false;
			} else {
				fputs(", ", out);
			}
			print_type_ext(parameter->type, parameter->symbol,
			               &parameter->scope);
		}
	}
	if (type->variadic) {
		if (first) {
			first = false;
		} else {
			fputs(", ", out);
		}
		fputs("...", out);
	}
	if (first && !type->unspecified_parameters) {
		fputs("void", out);
	}
	fputc(')', out);

	/* don't emit braces if we're the toplevel type... */
	if (!top)
		fputc(')', out);
}

/**
 * Prints the prefix part of a pointer type.
 *
 * @param type   The pointer type.
 */
static void print_pointer_type_pre(const pointer_type_t *type)
{
	intern_print_type_pre(type->points_to, false);
	fputs("*", out);
	print_type_qualifiers(type->base.qualifiers);
	if (type->base.qualifiers != 0)
		fputc(' ', out);
}

/**
 * Prints the postfix part of a pointer type.
 *
 * @param type   The pointer type.
 */
static void print_pointer_type_post(const pointer_type_t *type)
{
	intern_print_type_post(type->points_to, false);
}

/**
 * Prints the prefix part of an array type.
 *
 * @param type   The array type.
 */
static void print_array_type_pre(const array_type_t *type)
{
	intern_print_type_pre(type->element_type, false);
}

/**
 * Prints the postfix part of an array type.
 *
 * @param type   The array type.
 */
static void print_array_type_post(const array_type_t *type)
{
	fputc('[', out);
	if (type->is_static) {
		fputs("static ", out);
	}
	print_type_qualifiers(type->base.qualifiers);
	if (type->base.qualifiers != 0)
		fputc(' ', out);
	if (type->size_expression != NULL
			&& (print_implicit_array_size || !type->has_implicit_size)) {
		print_expression(type->size_expression);
	}
	fputc(']', out);
	intern_print_type_post(type->element_type, false);
}

/**
 * Prints the postfix part of a bitfield type.
 *
 * @param type   The array type.
 */
static void print_bitfield_type_post(const bitfield_type_t *type)
{
	fputs(" : ", out);
	print_expression(type->size);
	intern_print_type_post(type->base_type, false);
}

/**
 * Prints an enum definition.
 *
 * @param declaration  The enum's type declaration.
 */
void print_enum_definition(const declaration_t *declaration)
{
	fputs("{\n", out);

	change_indent(1);

	declaration_t *entry = declaration->next;
	for( ; entry != NULL && entry->storage_class == STORAGE_CLASS_ENUM_ENTRY;
	       entry = entry->next) {

		print_indent();
		fprintf(out, "%s", entry->symbol->string);
		if (entry->init.initializer != NULL) {
			fprintf(out, " = ");

			/* skip the implicit cast */
			expression_t *expression = entry->init.enum_value;
			if (expression->kind == EXPR_UNARY_CAST_IMPLICIT) {
				expression = expression->unary.value;
			}
			print_expression(expression);
		}
		fprintf(out, ",\n");
	}

	change_indent(-1);
	print_indent();
	fputs("}", out);
}

/**
 * Prints an enum type.
 *
 * @param type  The enum type.
 */
static void print_type_enum(const enum_type_t *type)
{
	int empty = type->base.qualifiers == 0;
	print_type_qualifiers(type->base.qualifiers);
	fputs(" enum " + empty, out);

	declaration_t *declaration = type->declaration;
	symbol_t      *symbol      = declaration->symbol;
	if (symbol != NULL) {
		fputs(symbol->string, out);
	} else {
		print_enum_definition(declaration);
	}
}

/**
 * Print the compound part of a compound type.
 *
 * @param declaration  The declaration of the compound type.
 */
void print_compound_definition(const declaration_t *declaration)
{
	fputs("{\n", out);
	change_indent(1);

	declaration_t *iter = declaration->scope.declarations;
	for( ; iter != NULL; iter = iter->next) {
		print_indent();
		print_declaration(iter);
		fputc('\n', out);
	}

	change_indent(-1);
	print_indent();
	fputs("}", out);
}

/**
 * Prints a compound type.
 *
 * @param type  The compound type.
 */
static void print_compound_type(const compound_type_t *type)
{
	int empty = type->base.qualifiers == 0;
	print_type_qualifiers(type->base.qualifiers);

	if (type->base.kind == TYPE_COMPOUND_STRUCT) {
		fputs(" struct " + empty, out);
	} else {
		assert(type->base.kind == TYPE_COMPOUND_UNION);
		fputs(" union " + empty, out);
	}

	declaration_t *declaration = type->declaration;
	symbol_t      *symbol      = declaration->symbol;
	if (symbol != NULL) {
		fputs(symbol->string, out);
	} else {
		print_compound_definition(declaration);
	}
}

/**
 * Prints the prefix part of a typedef type.
 *
 * @param type   The typedef type.
 */
static void print_typedef_type_pre(const typedef_type_t *const type)
{
	print_type_qualifiers(type->base.qualifiers);
	if (type->base.qualifiers != 0)
		fputc(' ', out);
	fputs(type->declaration->symbol->string, out);
}

/**
 * Prints the prefix part of a typeof type.
 *
 * @param type   The typeof type.
 */
static void print_typeof_type_pre(const typeof_type_t *const type)
{
	fputs("typeof(", out);
	if (type->expression != NULL) {
		assert(type->typeof_type == NULL);
		print_expression(type->expression);
	} else {
		print_type(type->typeof_type);
	}
	fputc(')', out);
}

/**
 * Prints the prefix part of a type.
 *
 * @param type   The type.
 * @param top    true if we print the toplevel type, false else.
 */
static void intern_print_type_pre(const type_t *const type, const bool top)
{
	switch(type->kind) {
	case TYPE_ERROR:
		fputs("<error>", out);
		return;
	case TYPE_INVALID:
		fputs("<invalid>", out);
		return;
	case TYPE_ENUM:
		print_type_enum(&type->enumt);
		return;
	case TYPE_ATOMIC:
		print_atomic_type(&type->atomic);
		return;
	case TYPE_COMPLEX:
		print_complex_type(&type->complex);
		return;
	case TYPE_IMAGINARY:
		print_imaginary_type(&type->imaginary);
		return;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		print_compound_type(&type->compound);
		return;
	case TYPE_BUILTIN:
		fputs(type->builtin.symbol->string, out);
		return;
	case TYPE_FUNCTION:
		print_function_type_pre(&type->function, top);
		return;
	case TYPE_POINTER:
		print_pointer_type_pre(&type->pointer);
		return;
	case TYPE_BITFIELD:
		intern_print_type_pre(type->bitfield.base_type, top);
		return;
	case TYPE_ARRAY:
		print_array_type_pre(&type->array);
		return;
	case TYPE_TYPEDEF:
		print_typedef_type_pre(&type->typedeft);
		return;
	case TYPE_TYPEOF:
		print_typeof_type_pre(&type->typeoft);
		return;
	}
	fputs("unknown", out);
}

/**
 * Prints the postfix part of a type.
 *
 * @param type   The type.
 * @param top    true if we print the toplevel type, false else.
 */
static void intern_print_type_post(const type_t *const type, const bool top)
{
	switch(type->kind) {
	case TYPE_FUNCTION:
		print_function_type_post(&type->function, NULL, top);
		return;
	case TYPE_POINTER:
		print_pointer_type_post(&type->pointer);
		return;
	case TYPE_ARRAY:
		print_array_type_post(&type->array);
		return;
	case TYPE_BITFIELD:
		print_bitfield_type_post(&type->bitfield);
		return;
	case TYPE_ERROR:
	case TYPE_INVALID:
	case TYPE_ATOMIC:
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
	case TYPE_ENUM:
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
	case TYPE_BUILTIN:
	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
		break;
	}
}

/**
 * Prints a type.
 *
 * @param type   The type.
 */
void print_type(const type_t *const type)
{
	print_type_ext(type, NULL, NULL);
}

void print_type_ext(const type_t *const type, const symbol_t *symbol,
                    const scope_t *scope)
{
	if (type == NULL) {
		fputs("nil type", out);
		return;
	}

	intern_print_type_pre(type, true);
	if (symbol != NULL) {
		fputc(' ', out);
		fputs(symbol->string, out);
	}
	if (type->kind == TYPE_FUNCTION) {
		print_function_type_post(&type->function, scope, true);
	} else {
		intern_print_type_post(type, true);
	}
}

/**
 * Return the size of a type AST node.
 *
 * @param type  The type.
 */
static size_t get_type_size(const type_t *type)
{
	switch(type->kind) {
	case TYPE_ATOMIC:          return sizeof(atomic_type_t);
	case TYPE_COMPLEX:         return sizeof(complex_type_t);
	case TYPE_IMAGINARY:       return sizeof(imaginary_type_t);
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:  return sizeof(compound_type_t);
	case TYPE_ENUM:            return sizeof(enum_type_t);
	case TYPE_FUNCTION:        return sizeof(function_type_t);
	case TYPE_POINTER:         return sizeof(pointer_type_t);
	case TYPE_ARRAY:           return sizeof(array_type_t);
	case TYPE_BUILTIN:         return sizeof(builtin_type_t);
	case TYPE_TYPEDEF:         return sizeof(typedef_type_t);
	case TYPE_TYPEOF:          return sizeof(typeof_type_t);
	case TYPE_BITFIELD:        return sizeof(bitfield_type_t);
	case TYPE_ERROR:           panic("error type found");
	case TYPE_INVALID:         panic("invalid type found");
	}
	panic("unknown type found");
}

/**
 * Duplicates a type.
 *
 * @param type  The type to copy.
 * @return A copy of the type.
 *
 * @note This does not produce a deep copy!
 */
type_t *duplicate_type(const type_t *type)
{
	size_t size = get_type_size(type);

	type_t *copy = obstack_alloc(type_obst, size);
	memcpy(copy, type, size);

	return copy;
}

/**
 * Returns the unqualified type of a given type.
 *
 * @param type  The type.
 * @returns The unqualified type.
 */
type_t *get_unqualified_type(type_t *type)
{
	if (type->base.qualifiers == TYPE_QUALIFIER_NONE)
		return type;

	type_t *unqualified_type          = duplicate_type(type);
	unqualified_type->base.qualifiers = TYPE_QUALIFIER_NONE;

	type_t *result = typehash_insert(unqualified_type);
	if (result != unqualified_type) {
		obstack_free(type_obst, unqualified_type);
	}

	return result;
}

type_t *get_qualified_type(type_t *type, type_qualifiers_t const qual)
{
	assert(!is_typeref(type));

	type_t *copy;
	if (is_type_array(type)) {
		/* For array types the element type has to be adjusted */
		type_t *element_type      = type->array.element_type;
		type_t *qual_element_type = get_qualified_type(element_type, qual);

		if (qual_element_type == element_type)
			return type;

		copy                     = duplicate_type(type);
		copy->array.element_type = qual_element_type;
	} else {
		if ((type->base.qualifiers & qual) == qual)
			return type;

		copy                   = duplicate_type(type);
		copy->base.qualifiers |= qual;
	}

	type = typehash_insert(copy);
	if (type != copy)
		obstack_free(type_obst, copy);

	return type;
}

/**
 * Check if a type is valid.
 *
 * @param type  The type to check.
 * @return true if type represents a valid type.
 */
bool type_valid(const type_t *type)
{
	return type->kind != TYPE_INVALID;
}

static bool test_atomic_type_flag(atomic_type_kind_t kind,
                                  atomic_type_flag_t flag)
{
	assert(kind <= ATOMIC_TYPE_LAST);
	return (atomic_type_properties[kind].flags & flag) != 0;
}

/**
 * Returns true if the given type is an integer type.
 *
 * @param type  The type to check.
 * @return True if type is an integer type.
 */
bool is_type_integer(const type_t *type)
{
	assert(!is_typeref(type));

	if (type->kind == TYPE_ENUM)
		return true;
	if (type->kind == TYPE_BITFIELD)
		return true;

	if (type->kind != TYPE_ATOMIC)
		return false;

	return test_atomic_type_flag(type->atomic.akind, ATOMIC_TYPE_FLAG_INTEGER);
}

/**
 * Returns true if the given type is an enum type.
 *
 * @param type  The type to check.
 * @return True if type is an enum type.
 */
bool is_type_enum(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_ENUM;
}

/**
 * Returns true if the given type is an floating point type.
 *
 * @param type  The type to check.
 * @return True if type is a floating point type.
 */
bool is_type_float(const type_t *type)
{
	assert(!is_typeref(type));

	if (type->kind != TYPE_ATOMIC)
		return false;

	return test_atomic_type_flag(type->atomic.akind, ATOMIC_TYPE_FLAG_FLOAT);
}

/**
 * Returns true if the given type is an complex type.
 *
 * @param type  The type to check.
 * @return True if type is a complex type.
 */
bool is_type_complex(const type_t *type)
{
	assert(!is_typeref(type));

	if (type->kind != TYPE_ATOMIC)
		return false;

	return test_atomic_type_flag(type->atomic.akind, ATOMIC_TYPE_FLAG_COMPLEX);
}

/**
 * Returns true if the given type is a signed type.
 *
 * @param type  The type to check.
 * @return True if type is a signed type.
 */
bool is_type_signed(const type_t *type)
{
	assert(!is_typeref(type));

	/* enum types are int for now */
	if (type->kind == TYPE_ENUM)
		return true;
	if (type->kind == TYPE_BITFIELD)
		return is_type_signed(type->bitfield.base_type);

	if (type->kind != TYPE_ATOMIC)
		return false;

	return test_atomic_type_flag(type->atomic.akind, ATOMIC_TYPE_FLAG_SIGNED);
}

/**
 * Returns true if the given type represents an arithmetic type.
 *
 * @param type  The type to check.
 * @return True if type represents an arithmetic type.
 */
bool is_type_arithmetic(const type_t *type)
{
	assert(!is_typeref(type));

	switch(type->kind) {
	case TYPE_BITFIELD:
	case TYPE_ENUM:
		return true;
	case TYPE_ATOMIC:
		return test_atomic_type_flag(type->atomic.akind, ATOMIC_TYPE_FLAG_ARITHMETIC);
	case TYPE_COMPLEX:
		return test_atomic_type_flag(type->complex.akind, ATOMIC_TYPE_FLAG_ARITHMETIC);
	case TYPE_IMAGINARY:
		return test_atomic_type_flag(type->imaginary.akind, ATOMIC_TYPE_FLAG_ARITHMETIC);
	default:
		return false;
	}
}

/**
 * Returns true if the given type is an integer or float type.
 *
 * @param type  The type to check.
 * @return True if type is an integer or float type.
 */
bool is_type_real(const type_t *type)
{
	/* 6.2.5.17 */
	return is_type_integer(type)
		|| (type->kind == TYPE_ATOMIC && is_type_float(type));
}

/**
 * Returns true if the given type represents a scalar type.
 *
 * @param type  The type to check.
 * @return True if type represents a scalar type.
 */
bool is_type_scalar(const type_t *type)
{
	assert(!is_typeref(type));

	switch (type->kind) {
		case TYPE_POINTER: return true;
		case TYPE_BUILTIN: return is_type_scalar(type->builtin.real_type);
		default:           break;
	}

	return is_type_arithmetic(type);
}

/**
 * Check if a given type is incomplete.
 *
 * @param type  The type to check.
 * @return True if the given type is incomplete (ie. just forward).
 */
bool is_type_incomplete(const type_t *type)
{
	assert(!is_typeref(type));

	switch(type->kind) {
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION: {
		const compound_type_t *compound_type = &type->compound;
		declaration_t         *declaration   = compound_type->declaration;
		return !declaration->init.complete;
	}
	case TYPE_ENUM: {
		const enum_type_t *enum_type   = &type->enumt;
		declaration_t     *declaration = enum_type->declaration;
		return !declaration->init.complete;
	}

	case TYPE_ARRAY:
		return type->array.size_expression == NULL
			&& !type->array.size_constant;

	case TYPE_ATOMIC:
		return type->atomic.akind == ATOMIC_TYPE_VOID;

	case TYPE_COMPLEX:
		return type->complex.akind == ATOMIC_TYPE_VOID;

	case TYPE_IMAGINARY:
		return type->imaginary.akind == ATOMIC_TYPE_VOID;

	case TYPE_BITFIELD:
	case TYPE_FUNCTION:
	case TYPE_POINTER:
	case TYPE_BUILTIN:
	case TYPE_ERROR:
		return false;

	case TYPE_TYPEDEF:
	case TYPE_TYPEOF:
		panic("is_type_incomplete called without typerefs skipped");
	case TYPE_INVALID:
		break;
	}

	panic("invalid type found");
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

	/* can parameters be compared? */
	if (func1->unspecified_parameters || func2->unspecified_parameters)
		return true;

	if (func1->variadic != func2->variadic)
		return false;

	if (func1->calling_convention != func2->calling_convention)
		return false;

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

/**
 * Check if two types are compatible.
 */
bool types_compatible(const type_t *type1, const type_t *type2)
{
	assert(!is_typeref(type1));
	assert(!is_typeref(type2));

	/* shortcut: the same type is always compatible */
	if (type1 == type2)
		return true;

	if (type1->base.qualifiers != type2->base.qualifiers)
		return false;
	if (type1->kind != type2->kind)
		return false;

	switch(type1->kind) {
	case TYPE_FUNCTION:
		return function_types_compatible(&type1->function, &type2->function);
	case TYPE_ATOMIC:
		return type1->atomic.akind == type2->atomic.akind;
	case TYPE_COMPLEX:
		return type1->complex.akind == type2->complex.akind;
	case TYPE_IMAGINARY:
		return type1->imaginary.akind == type2->imaginary.akind;
	case TYPE_ARRAY:
		return array_types_compatible(&type1->array, &type2->array);

	case TYPE_POINTER: {
		const type_t *const to1 = skip_typeref(type1->pointer.points_to);
		const type_t *const to2 = skip_typeref(type2->pointer.points_to);
		return types_compatible(to1, to2);
	}

	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
	case TYPE_ENUM:
	case TYPE_BUILTIN:
		/* TODO: not implemented */
		break;

	case TYPE_BITFIELD:
		/* not sure if this makes sense or is even needed, implement it if you
		 * really need it! */
		panic("type compatibility check for bitfield type");

	case TYPE_ERROR:
		/* Hmm, the error type should be compatible to all other types */
		return true;
	case TYPE_INVALID:
		panic("invalid type found in compatible types");
	case TYPE_TYPEDEF:
	case TYPE_TYPEOF:
		panic("typerefs not skipped in compatible types?!?");
	}

	/* TODO: incomplete */
	return false;
}

/**
 * Skip all typerefs and return the underlying type.
 */
type_t *skip_typeref(type_t *type)
{
	type_qualifiers_t qualifiers = TYPE_QUALIFIER_NONE;
	type_modifiers_t  modifiers  = TYPE_MODIFIER_NONE;

	while(true) {
		switch(type->kind) {
		case TYPE_ERROR:
			return type;
		case TYPE_TYPEDEF: {
			qualifiers |= type->base.qualifiers;
			modifiers  |= type->base.modifiers;
			const typedef_type_t *typedef_type = &type->typedeft;
			if (typedef_type->resolved_type != NULL) {
				type = typedef_type->resolved_type;
				break;
			}
			type = typedef_type->declaration->type;
			continue;
		}
		case TYPE_TYPEOF: {
			const typeof_type_t *typeof_type = &type->typeoft;
			if (typeof_type->typeof_type != NULL) {
				type = typeof_type->typeof_type;
			} else {
				type = typeof_type->expression->base.type;
			}
			continue;
		}
		default:
			break;
		}
		break;
	}

	if (qualifiers != TYPE_QUALIFIER_NONE || modifiers != TYPE_MODIFIER_NONE) {
		type_t *const copy = duplicate_type(type);

		/* for const with typedefed array type the element type has to be
		 * adjusted */
		if (is_type_array(copy)) {
			type_t *element_type           = copy->array.element_type;
			element_type                   = duplicate_type(element_type);
			element_type->base.qualifiers |= qualifiers;
			element_type->base.modifiers  |= modifiers;
			copy->array.element_type       = element_type;
		} else {
			copy->base.qualifiers |= qualifiers;
			copy->base.modifiers  |= modifiers;
		}

		type = typehash_insert(copy);
		if (type != copy) {
			obstack_free(type_obst, copy);
		}
	}

	return type;
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

atomic_type_kind_t get_intptr_kind(void)
{
	if (machine_size <= 32)
		return ATOMIC_TYPE_INT;
	else if (machine_size <= 64)
		return ATOMIC_TYPE_LONG;
	else
		return ATOMIC_TYPE_LONGLONG;
}

atomic_type_kind_t get_uintptr_kind(void)
{
	if (machine_size <= 32)
		return ATOMIC_TYPE_UINT;
	else if (machine_size <= 64)
		return ATOMIC_TYPE_ULONG;
	else
		return ATOMIC_TYPE_ULONGLONG;
}

/**
 * Find the atomic type kind representing a given size (signed).
 */
atomic_type_kind_t find_signed_int_atomic_type_kind_for_size(unsigned size) {
	static atomic_type_kind_t kinds[32];

	assert(size < 32);
	atomic_type_kind_t kind = kinds[size];
	if (kind == ATOMIC_TYPE_INVALID) {
		static const atomic_type_kind_t possible_kinds[] = {
			ATOMIC_TYPE_SCHAR,
			ATOMIC_TYPE_SHORT,
			ATOMIC_TYPE_INT,
			ATOMIC_TYPE_LONG,
			ATOMIC_TYPE_LONGLONG
		};
		for(unsigned i = 0; i < sizeof(possible_kinds)/sizeof(possible_kinds[0]); ++i) {
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
atomic_type_kind_t find_unsigned_int_atomic_type_kind_for_size(unsigned size) {
	static atomic_type_kind_t kinds[32];

	assert(size < 32);
	atomic_type_kind_t kind = kinds[size];
	if (kind == ATOMIC_TYPE_INVALID) {
		static const atomic_type_kind_t possible_kinds[] = {
			ATOMIC_TYPE_UCHAR,
			ATOMIC_TYPE_USHORT,
			ATOMIC_TYPE_UINT,
			ATOMIC_TYPE_ULONG,
			ATOMIC_TYPE_ULONGLONG
		};
		for(unsigned i = 0; i < sizeof(possible_kinds)/sizeof(possible_kinds[0]); ++i) {
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
static type_t *identify_new_type(type_t *type)
{
	type_t *result = typehash_insert(type);
	if (result != type) {
		obstack_free(type_obst, type);
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
	type_t *type = obstack_alloc(type_obst, sizeof(atomic_type_t));
	memset(type, 0, sizeof(atomic_type_t));

	type->kind            = TYPE_ATOMIC;
	type->base.qualifiers = qualifiers;
	type->base.alignment  = get_atomic_type_alignment(akind);
	type->atomic.akind    = akind;

	return identify_new_type(type);
}

/**
 * Creates a new complex type.
 *
 * @param akind       The kind of the atomic type.
 * @param qualifiers  Type qualifiers for the new type.
 */
type_t *make_complex_type(atomic_type_kind_t akind, type_qualifiers_t qualifiers)
{
	type_t *type = obstack_alloc(type_obst, sizeof(complex_type_t));
	memset(type, 0, sizeof(complex_type_t));

	type->kind            = TYPE_COMPLEX;
	type->base.qualifiers = qualifiers;
	type->base.alignment  = get_atomic_type_alignment(akind);
	type->complex.akind   = akind;

	return identify_new_type(type);
}

/**
 * Creates a new imaginary type.
 *
 * @param akind       The kind of the atomic type.
 * @param qualifiers  Type qualifiers for the new type.
 */
type_t *make_imaginary_type(atomic_type_kind_t akind, type_qualifiers_t qualifiers)
{
	type_t *type = obstack_alloc(type_obst, sizeof(imaginary_type_t));
	memset(type, 0, sizeof(imaginary_type_t));

	type->kind            = TYPE_IMAGINARY;
	type->base.qualifiers = qualifiers;
	type->base.alignment  = get_atomic_type_alignment(akind);
	type->imaginary.akind = akind;

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
	type_t *type = obstack_alloc(type_obst, sizeof(pointer_type_t));
	memset(type, 0, sizeof(pointer_type_t));

	type->kind              = TYPE_POINTER;
	type->base.qualifiers   = qualifiers;
	type->base.alignment    = 0;
	type->pointer.points_to = points_to;

	return identify_new_type(type);
}

type_t *make_array_type(type_t *element_type, size_t size,
                        type_qualifiers_t qualifiers)
{
	type_t *type = obstack_alloc(type_obst, sizeof(array_type_t));
	memset(type, 0, sizeof(array_type_t));

	type->kind                = TYPE_ARRAY;
	type->base.qualifiers     = qualifiers;
	type->base.alignment      = 0;
	type->array.element_type  = element_type;
	type->array.size          = size;
	type->array.size_constant = true;

	return identify_new_type(type);
}

/**
 * Debug helper. Prints the given type to stdout.
 */
static __attribute__((unused))
void dbg_type(const type_t *type)
{
	FILE *old_out = out;
	out = stderr;
	print_type(type);
	puts("\n");
	fflush(stderr);
	out = old_out;
}
