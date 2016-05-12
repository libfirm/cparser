/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "type_t.h"

#include <stdio.h>

#include "adt/bitfiddle.h"
#include "adt/panic.h"
#include "adt/separator_t.h"
#include "adt/util.h"
#include "dialect.h"
#include "driver/diagnostic.h"
#include "driver/target.h"
#include "driver/warning.h"
#include "entity_t.h"
#include "printer.h"
#include "symbol_t.h"
#include "type_hash.h"
#include "types.h"

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
		[TYPE_ATOMIC]           = sizeof(atomic_type_t),
		[TYPE_IMAGINARY]        = sizeof(atomic_type_t),
		[TYPE_COMPLEX]          = sizeof(atomic_type_t),
		[TYPE_COMPOUND_STRUCT]  = sizeof(compound_type_t),
		[TYPE_COMPOUND_UNION]   = sizeof(compound_type_t),
		[TYPE_ENUM]             = sizeof(enum_type_t),
		[TYPE_FUNCTION]         = sizeof(function_type_t),
		[TYPE_POINTER]          = sizeof(pointer_type_t),
		[TYPE_REFERENCE]        = sizeof(reference_type_t),
		[TYPE_ARRAY]            = sizeof(array_type_t),
		[TYPE_TYPEDEF]          = sizeof(typedef_type_t),
		[TYPE_TYPEOF]           = sizeof(typeof_type_t),
		[TYPE_VOID]             = sizeof(type_base_t),
		[TYPE_BUILTIN_TEMPLATE] = sizeof(type_base_t),
	};
	assert((size_t)kind < ARRAY_SIZE(sizes));
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
	[ATOMIC_TYPE_BOOL] = {
		.size       = 1,
		.alignment  = 1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER,
		.rank       = 1,
	},
	[ATOMIC_TYPE_CHAR] = {
		.size      = 1,
		.alignment = 1,
		.flags     = ATOMIC_TYPE_FLAG_INTEGER,
		.rank      = 2,
	},
	[ATOMIC_TYPE_SCHAR] = {
		.size      = 1,
		.alignment = 1,
		.flags     = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_SIGNED,
		.rank      = 2,
	},
	[ATOMIC_TYPE_UCHAR] = {
		.size      = 1,
		.alignment = 1,
		.flags     = ATOMIC_TYPE_FLAG_INTEGER,
		.rank      = 2,
	},
	[ATOMIC_TYPE_SHORT] = {
		.size       = 2,
		.alignment  = 2,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 3,
	},
	[ATOMIC_TYPE_USHORT] = {
		.size       = 2,
		.alignment  = 2,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER,
		.rank       = 3,
	},
	[ATOMIC_TYPE_INT] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 4,
	},
	[ATOMIC_TYPE_UINT] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER,
		.rank       = 4,
	},
	[ATOMIC_TYPE_LONG] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 5,
	},
	[ATOMIC_TYPE_ULONG] = {
		.size       = (unsigned) -1,
		.alignment  = (unsigned) -1,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER,
		.rank       = 5,
	},
	[ATOMIC_TYPE_LONGLONG] = {
		.size       = 8,
		.alignment  = 8,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 6,
	},
	[ATOMIC_TYPE_ULONGLONG] = {
		.size       = 8,
		.alignment  = 8,
		.flags      = ATOMIC_TYPE_FLAG_INTEGER,
		.rank       = 6,
	},
	[ATOMIC_TYPE_FLOAT] = {
		.size       = 4,
		.alignment  = 4,
		.flags      = ATOMIC_TYPE_FLAG_FLOAT | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 0,
	},
	[ATOMIC_TYPE_DOUBLE] = {
		.size       = 8,
		.alignment  = 8,
		.flags      = ATOMIC_TYPE_FLAG_FLOAT | ATOMIC_TYPE_FLAG_SIGNED,
		.rank       = 0,
	},
	[ATOMIC_TYPE_WCHAR_T] = {
		.size      = (unsigned)-1,
		.alignment = (unsigned)-1,
		.flags     = ATOMIC_TYPE_FLAG_INTEGER,
		.rank      = (unsigned)-1,
	},
};
atomic_type_properties_t pointer_properties = {
	.size      = 4,
	.alignment = 4,
	.flags     = ATOMIC_TYPE_FLAG_NONE,
};

void init_types(unsigned int_size, unsigned long_size, unsigned pointer_size)
{
	obstack_init(&type_obst);

	atomic_type_properties_t *props = atomic_type_properties;

	props[ATOMIC_TYPE_INT].size        = int_size;
	props[ATOMIC_TYPE_INT].alignment   = int_size;
	props[ATOMIC_TYPE_UINT].size       = int_size;
	props[ATOMIC_TYPE_UINT].alignment  = int_size;
	props[ATOMIC_TYPE_LONG].size       = long_size;
	props[ATOMIC_TYPE_LONG].alignment  = long_size;
	props[ATOMIC_TYPE_ULONG].size      = long_size;
	props[ATOMIC_TYPE_ULONG].alignment = long_size;
	pointer_properties.size            = pointer_size;
	pointer_properties.alignment       = pointer_size;

	props[ATOMIC_TYPE_LONG_DOUBLE] = props[ATOMIC_TYPE_DOUBLE];
	props[ATOMIC_TYPE_WCHAR_T]     = props[ATOMIC_TYPE_INT];

	/* set struct alignments to the same value as alignment */
	for (size_t i = 0; i != ARRAY_SIZE(atomic_type_properties); ++i) {
		props[i].struct_alignment = props[i].alignment;
	}
	pointer_properties.struct_alignment = pointer_size;
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
	case ATOMIC_TYPE_WCHAR_T:     return "wchar_t";
	case ATOMIC_TYPE_BOOL:        return dialect.cpp ? "bool" : "_Bool";
	case ATOMIC_TYPE_CHAR:        return "char";
	case ATOMIC_TYPE_SCHAR:       return "signed char";
	case ATOMIC_TYPE_UCHAR:       return "unsigned char";
	case ATOMIC_TYPE_INT:         return "int";
	case ATOMIC_TYPE_UINT:        return "unsigned int";
	case ATOMIC_TYPE_SHORT:       return "short int";
	case ATOMIC_TYPE_USHORT:      return "short unsigned int";
	case ATOMIC_TYPE_LONG:        return "long int";
	case ATOMIC_TYPE_ULONG:       return "long unsigned int";
	case ATOMIC_TYPE_LONGLONG:    return "long long int";
	case ATOMIC_TYPE_ULONGLONG:   return "long long unsigned int";
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
			if (dialect.cpp)
				print_string("extern \"C\" ");
			break;

		case LINKAGE_CXX:
			if (!dialect.cpp)
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

static void print_modifiers(decl_modifiers_t const modifiers)
{
	if (modifiers == 0)
		return;

	static const struct {
		unsigned bit;
		const char *name;
	} modifiernames[] = {
		{ DM_NAKED,             "naked"             },
		{ DM_NOTHROW,           "nothrow"           },
		{ DM_NORETURN,          "noreturn"          },
		{ DM_NOINLINE,          "noinline"          },
		{ DM_NOALIAS,           "noalias"           },
		{ DM_TRANSPARENT_UNION, "transparent_union" },
		{ DM_CONST,             "const"             },
		{ DM_PURE,              "pure"              },
		{ DM_CONSTRUCTOR,       "constructor"       },
		{ DM_DESTRUCTOR,        "destructor"        },
		{ DM_UNUSED,            "unused"            },
		{ DM_USED,              "used"              },
		{ DM_RETURNS_TWICE,     "returns_twice"     },
		{ DM_MALLOC,            "malloc"            },
		{ DM_WEAK,              "weak"              },
		{ DM_LEAF,              "leaf"              },
		{ DM_PACKED,            "packed"            },
	};
	separator_t sep = { " __attribute__((", ", " };
	for (size_t i = 0; i < ARRAY_SIZE(modifiernames); ++i) {
		if (modifiers & modifiernames[i].bit)
			print_format("%s__%s__", sep_next(&sep), modifiernames[i].name);
	}
	if (!sep_at_first(&sep))
		print_string("))");
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
		for (entity_t const *parameter = parameters->first_entity;
		     parameter != NULL; parameter = parameter->base.next) {
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
	print_modifiers(type->modifiers);

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

	for (const entity_t *entry = enume->first_value;
	     entry != NULL && entry->kind == ENTITY_ENUM_VALUE;
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

	for (entity_t const *entity = compound->members.first_entity;
	     entity != NULL; entity = entity->base.next) {
		if (entity->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		print_indent();
		print_entity(entity);
		print_char('\n');
	}

	change_indent(-1);
	print_indent();
	print_char('}');
	print_modifiers(compound->modifiers);
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
 * Prints the name of a void type.
 *
 * @param type  The type.
 */
static void print_void_type_pre(type_t const *const type)
{
	print_type_qualifiers(type->base.qualifiers, QUAL_SEP_END);
	print_string("void");
}

static void print_template_type_pre(type_t const *const type)
{
	print_type_qualifiers(type->base.qualifiers, QUAL_SEP_END);
	print_string("$T$");
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
	case TYPE_VOID:            print_void_type_pre(            type);            return;
	case TYPE_BUILTIN_TEMPLATE: print_template_type_pre(       type);            return;
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
	case TYPE_VOID:
	case TYPE_BUILTIN_TEMPLATE:
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
		if ((type->base.qualifiers & qual) == qual)
			return orig_type;

		copy                   = duplicate_type(type);
		copy->base.qualifiers |= qual;
	} else {
		return type;
	}

	return identify_new_type(copy);
}

static bool test_atomic_type_flag(atomic_type_kind_t kind,
                                  atomic_type_flags_t flag)
{
	assert(kind <= ATOMIC_TYPE_LAST);
	return (atomic_type_properties[kind].flags & flag) != 0;
}

bool is_type_integer(const type_t *type)
{
	assert(!is_typeref(type));
	if (!is_type_arithmetic(type))
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

bool is_type_complex(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_COMPLEX;
}

bool is_type_signed(const type_t *type)
{
	assert(!is_typeref(type));
	if (!is_type_arithmetic(type))
		return false;
	return test_atomic_type_flag(type->atomic.akind, ATOMIC_TYPE_FLAG_SIGNED);
}

bool is_type_arithmetic(const type_t *type)
{
	assert(!is_typeref(type));

	switch (type->kind) {
	case TYPE_ATOMIC:
	case TYPE_COMPLEX:
	case TYPE_ENUM:
	case TYPE_IMAGINARY:
		return true;

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
	case TYPE_ATOMIC:
	case TYPE_COMPLEX:
	case TYPE_ENUM:
	case TYPE_IMAGINARY:
	case TYPE_POINTER:
		return true;

	default:
		return false;
	}
}

bool is_type_complete(type_t const *const type)
{
	assert(!is_typeref(type));

	switch (type->kind) {
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return type->compound.compound->complete;

	case TYPE_ARRAY:
		return type->array.size_expression || type->array.size_constant;

	case TYPE_ENUM:
		return type->enumt.enume->complete;

	case TYPE_ATOMIC:
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
	case TYPE_FUNCTION:
	case TYPE_POINTER:
	case TYPE_REFERENCE:
		return true;

	case TYPE_ERROR:
	case TYPE_VOID:
		return false;

	case TYPE_TYPEDEF:
	case TYPE_TYPEOF:
	case TYPE_BUILTIN_TEMPLATE:
		break;
	}

	panic("invalid type");
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
	if (func1->unspecified_parameters && !func1->kr_style_parameters) {
		if (func2->unspecified_parameters && !func2->kr_style_parameters)
			return true;
		const function_type_t *const temp = func1;
		func1 = func2;
		func2 = temp;
		goto check_promoted_types;
	} else if (func2->unspecified_parameters && !func2->kr_style_parameters) {
check_promoted_types:
		/* all argument types must already be promoted */
		for (function_parameter_t *parameter = func1->parameters;
		     parameter != NULL; parameter = parameter->next) {
			type_t *const parameter_type = skip_typeref(parameter->type);
			type_t *const promoted
				= get_default_promoted_type(parameter_type);
			if (promoted != parameter_type)
				return false;
		}
		return true;
	}

	/* all argument types must be compatible */
	function_parameter_t *parameter1 = func1->parameters;
	function_parameter_t *parameter2 = func2->parameters;
	for ( ; parameter1 != NULL && parameter2 != NULL;
	     parameter1 = parameter1->next, parameter2 = parameter2->next) {
		type_t *parameter1_type = skip_typeref(parameter1->type);
		type_t *parameter2_type = skip_typeref(parameter2->type);

		if (!types_compatible_ignore_qualifiers(parameter1_type,
		                                        parameter2_type))
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

bool types_compatible_ignore_qualifiers(const type_t *type1,
                                        const type_t *type2)
{
	assert(!is_typeref(type1));
	assert(!is_typeref(type2));

	/* shortcut: the same type is always compatible */
	if (type1 == type2)
		return true;

	if (type1->kind != type2->kind) {
		/* enum types are compatible to their base integer type */
		if ((type1->kind == TYPE_ENUM
		     && is_type_atomic(type2, type1->enumt.base.akind))
		    || (type2->kind == TYPE_ENUM
		     && is_type_atomic(type1, type2->enumt.base.akind)))
		    return true;
		/* error types are compatible to everything to avoid follow-up errors */
		if (!is_type_valid(type1) || !is_type_valid(type2))
			return true;
		return false;
	}

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
		return type1->compound.compound == type2->compound.compound;
	case TYPE_ENUM:
		return type1->enumt.enume == type2->enumt.enume;
	case TYPE_ERROR:
	case TYPE_VOID:
		return true;
	case TYPE_TYPEDEF:
	case TYPE_TYPEOF:
		break; /* we already tested for is_typeref() above */
	case TYPE_BUILTIN_TEMPLATE:
		panic("unexpected type");
	}
	panic("invalid type kind");
}

bool types_compatible(const type_t *type1, const type_t *type2)
{
	assert(!is_typeref(type1));
	assert(!is_typeref(type2));

	/* shortcut: the same type is always compatible */
	if (type1 == type2)
		return true;
	if (type1->base.qualifiers != type2->base.qualifiers)
		return false;
	return types_compatible_ignore_qualifiers(type1, type2);
}

static bool type_base_same(const type_base_t *base1, const type_base_t *base2)
{
	return base1->kind == base2->kind && base1->qualifiers == base2->qualifiers;
}

static bool function_types_same(const function_type_t *func0,
                                const function_type_t *func1)
{
	if (!types_same(func0->return_type, func1->return_type))
		return false;
	for (function_parameter_t *param0 = func0->parameters,
	                          *param1 = func1->parameters;
	     param0 != NULL && param1 != NULL;
	     param0 = param0->next, param1 = param1->next) {
	    if (param0 == NULL || param1 == NULL)
			return false;
		if (!types_same(param0->type, param1->type))
			return false;
	}
	return func0->modifiers == func1->modifiers
	    && func0->variadic == func1->variadic
	    && func0->unspecified_parameters == func1->unspecified_parameters
	    && func0->kr_style_parameters == func1->kr_style_parameters
	    && func0->typegeneric == func1->typegeneric
	    && func0->linkage == func1->linkage
	    && func0->calling_convention == func1->calling_convention;
}

static bool array_types_same(const array_type_t *array0,
                             const array_type_t *array1)
{
	if (!types_same(array0->element_type, array1->element_type))
		return false;
	return array0->size == array1->size
	    && array0->is_static == array1->is_static
	    && array0->is_variable == array1->is_variable
	    && array0->has_implicit_size == array1->has_implicit_size
	    && array0->size_constant == array1->size_constant
	    && array0->is_vla == array1->is_vla;
}

bool types_same(type_t *type0, type_t *type1)
{
	if (type0 == type1)
		return true;
	type_t *skipped0 = skip_typeref(type0);
	type_t *skipped1 = skip_typeref(type1);
	if (skipped0 == skipped1)
		return true;
	if (!type_base_same(&skipped0->base, &skipped1->base))
		return false;

	switch (skipped0->kind) {
	case TYPE_ERROR:
	case TYPE_ATOMIC:
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
	case TYPE_ENUM:
	case TYPE_VOID:
	case TYPE_BUILTIN_TEMPLATE:
		/* if we are here, then the two types have different attributes or
		 * type_hash would have unified them. */
		return false;
	case TYPE_POINTER:
		return types_same(skipped0->pointer.points_to,
		                  skipped1->pointer.points_to);
	case TYPE_REFERENCE:
		return types_same(skipped0->reference.refers_to,
		                  skipped1->reference.refers_to);
	case TYPE_ARRAY:
		return array_types_same(&skipped0->array, &skipped1->array);
	case TYPE_FUNCTION:
		return function_types_same(&skipped0->function, &skipped1->function);
	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
		/* should be skipped */
		break;
	}
	panic("invalid type");
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

unsigned get_ctype_size(type_t const *const type)
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
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return type->compound.compound->size;
	case TYPE_FUNCTION:
	case TYPE_VOID:
		return 1; /* GCC extension. */
	case TYPE_REFERENCE:
	case TYPE_POINTER:
		return pointer_properties.size;
	case TYPE_ARRAY: {
		/* TODO: correct if element_type is aligned? */
		unsigned element_size = get_ctype_size(type->array.element_type);
		return type->array.size * element_size;
	}
	case TYPE_TYPEDEF:
		return get_ctype_size(type->typedeft.typedefe->type);
	case TYPE_TYPEOF:
		return get_ctype_size(type->typeoft.typeof_type);
	case TYPE_BUILTIN_TEMPLATE:
		break;
	}
	panic("invalid type");
}

unsigned get_ctype_alignment(type_t const *const type)
{
	switch (type->kind) {
	case TYPE_ERROR:
		return 0;
	case TYPE_ATOMIC:
	case TYPE_IMAGINARY:
	case TYPE_COMPLEX:
	case TYPE_ENUM:
		return get_atomic_type_alignment(type->atomic.akind);
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return type->compound.compound->alignment;
	case TYPE_FUNCTION:
	case TYPE_VOID:
		return 1; /* GCC extension. */
	case TYPE_REFERENCE:
	case TYPE_POINTER:
		return pointer_properties.alignment;
	case TYPE_ARRAY:
		return get_ctype_alignment(type->array.element_type);
	case TYPE_TYPEDEF: {
		unsigned const alignment = get_ctype_alignment(type->typedeft.typedefe->type);
		return MAX(alignment, type->typedeft.typedefe->alignment);
	}
	case TYPE_TYPEOF:
		return get_ctype_alignment(type->typeoft.typeof_type);
	case TYPE_BUILTIN_TEMPLATE:
		break;
	}
	panic("invalid type");
}

unsigned get_type_alignment_compound(type_t const *const type)
{
	switch (type->kind) {
	case TYPE_ATOMIC:
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
	case TYPE_ENUM:
		return atomic_type_properties[type->atomic.akind].struct_alignment;

	case TYPE_TYPEDEF: {
		unsigned const alignment = get_type_alignment_compound(type->typedeft.typedefe->type);
		return MAX(type->typedeft.typedefe->alignment, alignment);
	}

	case TYPE_TYPEOF:
		return get_type_alignment_compound(type->typeoft.typeof_type);

	case TYPE_ARRAY:
		return get_type_alignment_compound(type->array.element_type);

	default:
		return get_ctype_alignment(type);
	}
}

decl_modifiers_t get_type_modifiers(const type_t *type)
{
	switch (type->kind) {
	case TYPE_ERROR:
	case TYPE_BUILTIN_TEMPLATE:
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
	case TYPE_VOID:
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
		switch (type->kind) {
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

atomic_type_flags_t get_atomic_type_flags(atomic_type_kind_t kind)
{
	assert(kind <= ATOMIC_TYPE_LAST);
	return atomic_type_properties[kind].flags;
}

/**
 * Find the atomic type kind representing a given size (signed).
 */
atomic_type_kind_t find_signed_int_atomic_type_kind_for_size(unsigned size)
{
	static const atomic_type_kind_t possible_kinds[] = {
		ATOMIC_TYPE_SCHAR,
		ATOMIC_TYPE_SHORT,
		ATOMIC_TYPE_INT,
		ATOMIC_TYPE_LONG,
		ATOMIC_TYPE_LONGLONG
	};
	for (size_t i = 0; i < ARRAY_SIZE(possible_kinds); ++i) {
		if (get_atomic_type_size(possible_kinds[i]) == size) {
			return possible_kinds[i];
		}
	}
	panic("couldn't find atomic type");
}

/**
 * Find the atomic type kind representing a given size (signed).
 */
atomic_type_kind_t find_unsigned_int_atomic_type_kind_for_size(unsigned size)
{
	static const atomic_type_kind_t possible_kinds[] = {
		ATOMIC_TYPE_UCHAR,
		ATOMIC_TYPE_USHORT,
		ATOMIC_TYPE_UINT,
		ATOMIC_TYPE_ULONG,
		ATOMIC_TYPE_ULONGLONG
	};
	for (size_t i = 0; i < ARRAY_SIZE(possible_kinds); ++i) {
		if (get_atomic_type_size(possible_kinds[i]) == size) {
			return possible_kinds[i];
		}
	}
	panic("couldn't find atomic type");
}

type_t *promote_integer(type_t *type)
{
	atomic_type_kind_t akind = get_arithmetic_akind(type);
	if (get_akind_rank(akind) < get_akind_rank(ATOMIC_TYPE_INT))
		type = type_int;

	return type;
}

type_t *get_default_promoted_type(type_t *orig_type)
{
	type_t *result = orig_type;

	type_t *type = skip_typeref(orig_type);
	if (is_type_integer(type)) {
		result = promote_integer(type);
	} else if (is_type_atomic(type, ATOMIC_TYPE_FLOAT)) {
		result = type_double;
	}

	return result;
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
	type->base.qualifiers   = qualifiers;
	type->pointer.points_to = points_to;

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

type_t *make_void_type(type_qualifiers_t const qualifiers)
{
	type_t *const type = allocate_type_zero(TYPE_VOID);
	type->base.qualifiers = qualifiers;
	return identify_new_type(type);
}

void layout_compound(compound_t *const compound)
{
	bool     const is_union   = compound->base.kind == ENTITY_UNION;
	unsigned       alignment  = compound->alignment;
	size_t         bit_offset = 0;
	unsigned       size       = 0;
	bool           need_pad   = false;
	for (entity_t *entry = compound->members.first_entity; entry;
	     entry = entry->base.next) {
		if (entry->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		compound_member_t *const member = &entry->compound_member;
		type_t            *const m_type = skip_typeref(member->base.type);
		if (!is_type_valid(m_type))
			continue;

		unsigned m_alignment = get_declaration_alignment(&member->base);
		alignment = MAX(alignment, m_alignment);

		unsigned const m_size = get_ctype_size(m_type);
		if (is_union) {
			size = MAX(size, m_size);
		} else if (member->bitfield) {
			unsigned const alignment_mask = m_alignment - 1;
			size_t   const base_size      = m_size * BITS_PER_BYTE;
			size_t   const bit_size       = member->bit_size;

			bit_offset += (size & alignment_mask) * BITS_PER_BYTE;
			size       &= ~alignment_mask;
			if (bit_offset + bit_size > base_size
				|| (bit_size == 0 && !(member->base.modifiers & DM_PACKED))) {
				size      += (bit_offset + BITS_PER_BYTE - 1) / BITS_PER_BYTE;
				size       = round_up2(size, m_alignment);
				bit_offset = 0;
			}

			if (target.byte_order_big_endian) {
				member->offset     = size & ~alignment_mask;
				member->bit_offset = base_size - bit_offset - bit_size;
			} else {
				member->offset     = size;
				member->bit_offset = bit_offset;
			}

			bit_offset += bit_size;
			size       += bit_offset / BITS_PER_BYTE;
			bit_offset %= BITS_PER_BYTE;
		} else {
			if (bit_offset != 0) {
				bit_offset = 0;
				size      += 1;
			}

			unsigned const new_size = round_up2(size, m_alignment);
			if (new_size > size) {
				need_pad = true;
				size     = new_size;
			}

			member->offset = size;
			size          += m_size;
		}
	}

	if (bit_offset != 0)
		size += 1;

	unsigned const new_size = round_up2(size, alignment);
	if (new_size > size) {
		need_pad = true;
		size     = new_size;
	}

	position_t const *const pos = &compound->base.pos;
	if (need_pad) {
		warningf(WARN_PADDED, pos, "%N needs padding", compound);
	} else if (compound->packed) {
		warningf(WARN_PACKED, pos, "superfluous packed attribute on %N",
		         compound);
	}

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

static bool is_generic_type(type_t *const type)
{
	/* currently we only allow plain template types and pointers to them.
	 * They can't be part of a struct/class, typeof, typedef, ... */
	if (type->kind == TYPE_POINTER) {
		return is_generic_type(type->pointer.points_to);
	}

	return type->kind == TYPE_BUILTIN_TEMPLATE;
}

type_t *make_function_type(type_t *return_type, int n_types,
                           type_t *const *argument_types,
						   decl_modifiers_t modifiers)
{
	type_t *type               = allocate_type_zero(TYPE_FUNCTION);
	type->function.return_type = return_type;
	type->function.modifiers  |= modifiers;
	type->function.linkage     = LINKAGE_C;

	bool                  typegeneric = is_generic_type(return_type);
	function_parameter_t **anchor     = &type->function.parameters;
	for (int i = 0; i < n_types; ++i) {
		type_t *argtype = argument_types[i];
		typegeneric = typegeneric || is_generic_type(argtype);

		function_parameter_t *parameter = allocate_parameter(argtype);
		*anchor = parameter;
		anchor  = &parameter->next;
	}

	type->function.typegeneric = typegeneric;
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
