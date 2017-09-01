/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "mangle.h"

#include <libfirm/firm.h>
#include <string.h>

#include "adt/obst.h"
#include "adt/panic.h"
#include "ast/dialect.h"
#include "ast/entity_t.h"
#include "ast/symbol_t.h"
#include "ast/type_t.h"
#include "driver/target.h"

static struct obstack obst;

static void mangle_type(type_t *type);

static char get_atomic_type_mangle(atomic_type_kind_t kind)
{
	switch (kind) {
	case ATOMIC_TYPE_WCHAR_T:     return 'w';
	case ATOMIC_TYPE_BOOL:        return 'b';
	case ATOMIC_TYPE_CHAR:        return 'c';
	case ATOMIC_TYPE_SCHAR:       return 'a';
	case ATOMIC_TYPE_UCHAR:       return 'h';
	case ATOMIC_TYPE_INT:         return 'i';
	case ATOMIC_TYPE_UINT:        return 'j';
	case ATOMIC_TYPE_SHORT:       return 's';
	case ATOMIC_TYPE_USHORT:      return 't';
	case ATOMIC_TYPE_LONG:        return 'l';
	case ATOMIC_TYPE_ULONG:       return 'm';
	case ATOMIC_TYPE_LONGLONG:    return 'x';
	case ATOMIC_TYPE_ULONGLONG:   return 'y';
	case ATOMIC_TYPE_LONG_DOUBLE: return 'e';
	case ATOMIC_TYPE_FLOAT:       return 'f';
	case ATOMIC_TYPE_DOUBLE:      return 'd';
	}
	panic("invalid atomic type");
}

static void mangle_atomic_type(const atomic_type_t *type)
{
	obstack_1grow(&obst, get_atomic_type_mangle(type->akind));
}

static void mangle_pointer_type(const pointer_type_t *type)
{
	obstack_1grow(&obst, 'P');
	mangle_type(type->points_to);
}

static void mangle_reference_type(const reference_type_t *type)
{
	obstack_1grow(&obst, 'R');
	mangle_type(type->refers_to);
}

static void mangle_parameters(const function_type_t *type)
{
	if (type->unspecified_parameters)
		panic("can't mangle unspecified parameter types");
	if (type->kr_style_parameters)
		panic("can't mangle kr_style_parameters type");

	const function_parameter_t *parameter = type->parameters;
	if (parameter != NULL) {
		for ( ; parameter != NULL; parameter = parameter->next) {
			mangle_type(parameter->type);
		}
		if (type->variadic) {
			obstack_1grow(&obst, 'z');
		}
	} else {
		obstack_1grow(&obst, 'v');
	}
}

static void mangle_function_type(const function_type_t *type)
{
	obstack_1grow(&obst, 'F');
	if (type->linkage == LINKAGE_C) {
		obstack_1grow(&obst, 'Y');
	}

	mangle_type(type->return_type);
	mangle_parameters(type);

	obstack_1grow(&obst, 'E');
}

static void print_name(const char* name)
{
	obstack_printf(&obst, "%u%s", (unsigned)strlen(name), name);
}

static void mangle_class_type(const compound_type_t *type)
{
	const symbol_t *sym = type->compound->base.symbol;
	if (sym == NULL) {
		if (type->compound->alias == NULL)
			panic("mangling anonymous type");
		sym = type->compound->alias->base.symbol;
	}
	print_name(sym->string);
}

static void mangle_enum_type(const enum_type_t *type)
{
	const symbol_t *sym = type->enume->base.symbol;
	if (sym == NULL) {
		if (type->enume->alias == NULL)
			panic("mangling anonymous type");
		sym = type->enume->alias->base.symbol;
	}
	print_name(sym->string);
}

static void mangle_array_type(const array_type_t *type)
{
	if (type->is_vla) {
		obstack_1grow(&obst, 'A');
		obstack_1grow(&obst, '_');
	} else if (type->size_constant) {
		obstack_printf(&obst, "A%u_", (unsigned) type->size);
	} else {
		panic("mangling of non-constant sized array types not implemented yet");
	}
	mangle_type(type->element_type);
}

static void mangle_complex_type(const atomic_type_t *type)
{
	obstack_1grow(&obst, 'C');
	obstack_1grow(&obst, get_atomic_type_mangle(type->akind));
}

static void mangle_imaginary_type(const atomic_type_t *type)
{
	obstack_1grow(&obst, 'G');
	obstack_1grow(&obst, get_atomic_type_mangle(type->akind));
}

static void mangle_qualifiers(type_qualifiers_t qualifiers)
{
#if 0 /* Do not mangle restrict qualifiers.  GCC doesn't either */
	if (qualifiers & TYPE_QUALIFIER_RESTRICT)
		obstack_1grow(&obst, 'r');
#endif
	if (qualifiers & TYPE_QUALIFIER_VOLATILE)
		obstack_1grow(&obst, 'V');
	if (qualifiers & TYPE_QUALIFIER_CONST)
		obstack_1grow(&obst, 'K');

	/* handle MS extended qualifiers? */
}

static void mangle_type(type_t *orig_type)
{
	type_t *type = skip_typeref(orig_type);

	mangle_qualifiers(type->base.qualifiers);

	switch (type->kind) {
	case TYPE_ATOMIC:
		mangle_atomic_type(&type->atomic);
		return;
	case TYPE_POINTER:
		mangle_pointer_type(&type->pointer);
		return;
	case TYPE_REFERENCE:
		mangle_reference_type(&type->reference);
		return;
	case TYPE_FUNCTION:
		mangle_function_type(&type->function);
		return;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		mangle_class_type(&type->compound);
		return;
	case TYPE_ENUM:
		mangle_enum_type(&type->enumt);
		return;
	case TYPE_ARRAY:
		mangle_array_type(&type->array);
		return;
	case TYPE_COMPLEX:
		mangle_complex_type(&type->atomic);
		return;
	case TYPE_IMAGINARY:
		mangle_imaginary_type(&type->atomic);
		return;
	case TYPE_VOID:
		obstack_1grow(&obst, 'v');
		return;
	case TYPE_ERROR:
		panic("error type encountered while mangling");
	case TYPE_TYPEDEF:
	case TYPE_TYPEOF:
		panic("typeref not resolved while manging");
	case TYPE_BUILTIN_TEMPLATE:
		panic("builtin template not resolved while mangling");
	}
	panic("invalid type encountered while mangling");
}

static void mangle_namespace(entity_t const *const entity)
{
	for (entity_t const *e = entity->base.parent_entity; e != NULL;
	     e = e->base.parent_entity) {
	    /* TODO: we need something similar (or the same?) for classes */
		if (e->kind == ENTITY_NAMESPACE) {
			mangle_namespace(e);
			print_name(e->base.symbol->string);
			return;
		}
	}
}

static void mangle_cxx_entity(entity_t const *const entity)
{
	obstack_1grow(&obst, '_');
	obstack_1grow(&obst, 'Z');

	if (entity->base.parent_entity != NULL) {
		obstack_1grow(&obst, 'N');
		mangle_namespace(entity);
	}

	print_name(entity->base.symbol->string);

	if (entity->kind == ENTITY_FUNCTION) {
		mangle_parameters(&entity->declaration.type->function);
	}
}

static char get_cc_prefix(cc_kind_t const cc)
{
	/* calling convention prefix */
	switch (cc) {
	case CC_THISCALL:
	case CC_DEFAULT:
	case CC_CDECL:    return 0;
	case CC_STDCALL:  return '_';
	case CC_FASTCALL: return '@';
	}
	panic("unknown calling convention");
}

ident *create_ld_ident(entity_t const *const entity)
{
	assert(obstack_object_size(&obst) == 0);

	/* Special case: No mangling if actual_name is set */
	if (entity->kind == ENTITY_FUNCTION) {
		symbol_t *actual_name = entity->function.actual_name;
		if (actual_name != NULL && entity->function.btk != BUILTIN_LIBC
		                        && entity->function.btk != BUILTIN_LIBC_CHECK)
			return new_id_from_str(actual_name->string);
	}

	if (target.user_label_prefix != 0)
		obstack_1grow(&obst, target.user_label_prefix);

	bool mangle_cxx;
	unsigned size_suffix = ~0u;
	if (entity->kind == ENTITY_FUNCTION) {
		type_t *type = skip_typeref(entity->declaration.type);
		assert(is_type_function(type));

		if (entity->declaration.modifiers & DM_DLLIMPORT)
			/* add prefix for imported symbols */
			obstack_printf(&obst, "__imp_");

		cc_kind_t const cc = type->function.calling_convention;
		char const cc_prefix = get_cc_prefix(cc);
		if (cc_prefix != 0)
			obstack_1grow(&obst, cc_prefix);

		switch (type->function.linkage) {
		case LINKAGE_CXX:
			mangle_cxx = true;
			break;
		default:
			mangle_cxx = false;
		}

		if (cc == CC_STDCALL || cc == CC_FASTCALL) {
			size_suffix = 0;
			for (function_parameter_t const* i = type->function.parameters;
				 i != NULL; i = i->next) {
				size_suffix += get_ctype_size(i->type);
			}
		}
	} else {
		/* Mangle with C++ rules if it has a namespace */
		mangle_cxx = entity->base.parent_entity != NULL
		          && entity->base.parent_entity->kind == ENTITY_NAMESPACE;
	}

	/* Shortcut: No mangling necessary */
	if (mangle_cxx) {
		mangle_cxx_entity(entity);
	} else {
		const char *name = entity->base.symbol->string;
		if (entity->kind == ENTITY_FUNCTION
		 && (entity->function.btk == BUILTIN_LIBC
		  || entity->function.btk == BUILTIN_LIBC_CHECK)) {
			symbol_t *actual_name = entity->function.actual_name;
			assert(actual_name != NULL);
			name = actual_name->string;
		}
		/* Shortcut: No mangling at all */
		if (obstack_object_size(&obst) == 0 && size_suffix == 0)
			return new_id_from_str(name);

		size_t len = strlen(name);
		obstack_grow(&obst, name, len);
		if (size_suffix != ~0u)
			obstack_printf(&obst, "@%u", size_suffix);
	}

	/* Create libfirm ident from string on obstack */
	size_t  size = obstack_object_size(&obst);
	char   *str  = obstack_finish(&obst);
	ident  *id   = new_id_from_chars(str, size);
	obstack_free(&obst, str);
	return id;
}

void init_mangle(void)
{
	obstack_init(&obst);
}

void exit_mangle(void)
{
	obstack_free(&obst, NULL);
}
