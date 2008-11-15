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

#include <libfirm/firm.h>
#include <string.h>

#include "entity_t.h"
#include "type_t.h"
#include "symbol_t.h"
#include "mangle.h"
#include "diagnostic.h"
#include "ast2firm.h"
#include "lang_features.h"
#include "adt/error.h"

static ident          *id_underscore;
static struct obstack  obst;

static void mangle_type(type_t *type);

static char get_atomic_type_mangle(atomic_type_kind_t kind)
{
	switch (kind) {
	case ATOMIC_TYPE_INVALID: break;
	case ATOMIC_TYPE_VOID:        return 'v';
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
	panic("invalid atomic type in mangler");
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

static void mangle_function_type(const function_type_t *type)
{
	obstack_1grow(&obst, 'F');
	if (type->linkage == LINKAGE_C) {
		obstack_1grow(&obst, 'Y');
	}

	mangle_type(type->return_type);

	function_parameter_t *parameter = type->parameters;
	for ( ; parameter != NULL; parameter = parameter->next) {
		mangle_type(parameter->type);
	}
	if (type->variadic) {
		obstack_1grow(&obst, 'z');
	}
	if (type->unspecified_parameters)
		panic("can't mangle unspecified parameter types");
	if (type->kr_style_parameters)
		panic("can't mangle kr_style_parameters type");

	obstack_1grow(&obst, 'E');
}

static void mangle_qualifiers(type_qualifiers_t qualifiers)
{
	if (qualifiers & TYPE_QUALIFIER_CONST)
		obstack_1grow(&obst, 'K');
	if (qualifiers & TYPE_QUALIFIER_VOLATILE)
		obstack_1grow(&obst, 'V');
	if (qualifiers & TYPE_QUALIFIER_RESTRICT)
		obstack_1grow(&obst, 'r');

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
	case TYPE_FUNCTION:
		mangle_function_type(&type->function);
		return;
	case TYPE_INVALID:
		panic("invalid type encountered while mangling");
	case TYPE_ERROR:
		panic("error type encountered while mangling");
	case TYPE_BUILTIN:
	case TYPE_TYPEDEF:
	case TYPE_TYPEOF:
		panic("typeref not resolved while manging?!?");

	case TYPE_BITFIELD:
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
	case TYPE_ENUM:
	case TYPE_ARRAY:
		panic("no mangling for this type implemented yet");
		break;
	}
	panic("invalid type encountered while mangling");
}

static void mangle_entity(entity_t *entity)
{
	obstack_1grow(&obst, '_');
	obstack_1grow(&obst, 'Z');

	/* TODO: mangle scope */

	symbol_t *symbol = entity->base.symbol;
	obstack_printf(&obst, "%u%s", strlen(symbol->string), symbol->string);

	if (entity->kind == ENTITY_FUNCTION) {
		mangle_type(entity->declaration.type);
	}
}

/**
 * Mangles an entity linker (ld) name for win32 usage.
 *
 * @param ent          the entity to be mangled
 * @param declaration  the declaration
 */
ident *create_name_win32(entity_t *entity)
{
	struct obstack *o = &obst;

	assert(is_declaration(entity));

	if (entity->declaration.modifiers & DM_DLLIMPORT) {
		/* add prefix for imported symbols */
		obstack_printf(o, "__imp_");
	}

	if (entity->kind == ENTITY_FUNCTION) {
		cc_kind_t cc = entity->declaration.type->function.calling_convention;

		/* calling convention prefix */
		switch (cc) {
			case CC_DEFAULT:
			case CC_CDECL:
			case CC_STDCALL:  obstack_1grow(o, '_'); break;
			case CC_FASTCALL: obstack_1grow(o, '@'); break;
			default:          panic("unhandled calling convention");
		}

		switch (entity->declaration.type->function.linkage) {
			case LINKAGE_INVALID:
				break;

			case LINKAGE_C:
				obstack_printf(o, "%s", entity->base.symbol->string);
				break;

			case LINKAGE_CXX:
				mangle_entity(entity);
				break;
		}

		/* calling convention suffix */
		switch (cc) {
			case CC_DEFAULT:
			case CC_CDECL:
				break;

			case CC_STDCALL:
			case CC_FASTCALL: {
				ir_type *irtype = get_ir_type(entity->declaration.type);
				size_t   size   = 0;
				for (int i = get_method_n_params(irtype) - 1; i >= 0; --i) {
					size += get_type_size_bytes(get_method_param_type(irtype, i));
				}
				obstack_printf(o, "@%zu\n", size);
				break;
			}

			default:
				panic("unhandled calling convention");
		}
	} else {
		obstack_printf(o, "_%s", entity->base.symbol->string);
	}

	size_t  size = obstack_object_size(o);
	char   *str  = obstack_finish(o);
	ident  *id   = new_id_from_chars(str, size);
	obstack_free(o, str);
	return id;
}

/**
 * Mangles an entity linker (ld) name for Linux ELF usage.
 *
 * @param ent          the entity to be mangled
 * @param declaration  the declaration
 */
ident *create_name_linux_elf(entity_t *entity)
{
	bool needs_mangling = false;

	if (entity->kind == ENTITY_FUNCTION) {
		switch (entity->declaration.type->function.linkage) {
			case LINKAGE_INVALID: break;
			case LINKAGE_C:       break;
			case LINKAGE_CXX:     needs_mangling = true; break;
		}
	}

	if (needs_mangling) {
		mangle_entity(entity);
		size_t  size = obstack_object_size(&obst);
		char   *str  = obstack_finish(&obst);
		ident  *id   = new_id_from_chars(str, size);
		obstack_free(&obst, str);
		return id;
	}

	return new_id_from_str(entity->base.symbol->string);
}

/**
 * Mangles an entity linker (ld) name for Mach-O usage.
 *
 * @param ent          the entity to be mangled
 * @param declaration  the declaration
 */
ident *create_name_macho(entity_t *entity)
{
	ident *id = new_id_from_str(entity->base.symbol->string);
	return id_mangle(id_underscore, id);
}

void init_mangle(void)
{
	id_underscore = new_id_from_chars("_", 1);

	obstack_init(&obst);
}

void exit_mangle(void)
{
	obstack_free(&obst, NULL);
}
