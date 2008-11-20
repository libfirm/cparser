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

#include <errno.h>
#include <string.h>

#include "write_caml.h"
#include "symbol_t.h"
#include "ast_t.h"
#include "type_t.h"
#include "entity_t.h"
#include "type.h"
#include "adt/error.h"

static const scope_t *global_scope;
static FILE          *out;

static void write_type(const type_t *type);

static const char *get_atomic_type_string(const atomic_type_kind_t type)
{
	switch(type) {
	case ATOMIC_TYPE_VOID:        return "unit";
	case ATOMIC_TYPE_CHAR:        return "byte";
	case ATOMIC_TYPE_SCHAR:       return "byte";
	case ATOMIC_TYPE_UCHAR:       return "unsigned byte";
	case ATOMIC_TYPE_SHORT:	      return "short";
	case ATOMIC_TYPE_USHORT:      return "unsigned short";
	case ATOMIC_TYPE_INT:         return "int";
	case ATOMIC_TYPE_UINT:        return "unsigned int";
	case ATOMIC_TYPE_LONG:        return "int";
	case ATOMIC_TYPE_ULONG:       return "unsigned int";
	case ATOMIC_TYPE_LONGLONG:    return "long";
	case ATOMIC_TYPE_ULONGLONG:   return "unsigned long";
	case ATOMIC_TYPE_FLOAT:       return "float";
	case ATOMIC_TYPE_DOUBLE:      return "double";
	case ATOMIC_TYPE_LONG_DOUBLE: return "double";
	case ATOMIC_TYPE_BOOL:        return "bool";
	default:                      panic("unsupported atomic type");
	}
}

static void write_atomic_type(const atomic_type_t *type)
{
	fprintf(out, "%s", get_atomic_type_string(type->akind));
}

static void write_pointer_type(const pointer_type_t *type)
{
	type_t *points_to = type->points_to;
	if (points_to->kind == TYPE_ATOMIC &&
			is_type_atomic(points_to, ATOMIC_TYPE_CHAR)) {
		fputs("string", out);
		return;
	}

	write_type(type->points_to);
	fputc('*', out);
}

static entity_t *find_typedef(const type_t *type)
{
	/* first: search for a matching typedef in the global type... */
	entity_t *entity = global_scope->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		if (entity->kind != ENTITY_TYPEDEF)
			continue;
		if (entity->typedefe.type == type)
			break;
	}

	return entity;
}

static void write_compound_type(const compound_type_t *type)
{
	const entity_t *entity = find_typedef((const type_t*) type);
	if (entity != NULL) {
		fprintf(out, "%s", entity->base.symbol->string);
		return;
	}

	/* does the struct have a name? */
	symbol_t *symbol = type->compound->base.symbol;
	if (symbol != NULL) {
		/* TODO: make sure we create a struct for it... */
		fprintf(out, "%s", symbol->string);
		return;
	}
	/* TODO: create a struct and use its name here... */
	fprintf(out, "/* TODO anonymous struct */byte");
}

static void write_enum_type(const enum_type_t *type)
{
	const entity_t *entity = find_typedef((const type_t*) type);
	if (entity != NULL) {
		fprintf(out, "%s", entity->base.symbol->string);
		return;
	}

	/* does the enum have a name? */
	symbol_t *symbol = type->enume->base.symbol;
	if (symbol != NULL) {
		/* TODO: make sure we create an enum for it... */
		fprintf(out, "%s", symbol->string);
		return;
	}
	/* TODO: create a struct and use its name here... */
	fprintf(out, "/* TODO anonymous enum */byte");
}

static void write_function_type(const function_type_t *type)
{
	fprintf(out, "(func(");

	function_parameter_t *parameter = type->parameters;
	int                   first     = 1;
	while(parameter != NULL) {
		if (!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}

#if 0
		if (parameter->symbol != NULL) {
			fprintf(out, "%s : ", parameter->symbol->string);
		} else {
			/* TODO make up some unused names (or allow _ in fluffy?) */
			fprintf(out, "_ : ");
		}
#endif
		fputs("_ : ", out);
		write_type(parameter->type);

		parameter = parameter->next;
	}

	fprintf(out, ") : ");
	write_type(type->return_type);
	fprintf(out, ")");
}

static void write_type(const type_t *type)
{
	switch(type->kind) {
	case TYPE_ATOMIC:
		write_atomic_type(&type->atomic);
		return;
	case TYPE_POINTER:
		write_pointer_type(&type->pointer);
		return;
	case TYPE_COMPOUND_UNION:
	case TYPE_COMPOUND_STRUCT:
		write_compound_type(&type->compound);
		return;
	case TYPE_ENUM:
		write_enum_type(&type->enumt);
		return;
	case TYPE_FUNCTION:
		write_function_type(&type->function);
		return;
	case TYPE_INVALID:
		panic("invalid type found");
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
	default:
		fprintf(out, "/* TODO type */");
		break;
	}
}

static void write_function(const entity_t *entity)
{
	if (entity->function.statement != NULL) {
		fprintf(stderr, "Warning: can't convert function bodies (at %s)\n",
		        entity->base.symbol->string);
	}

	fprintf(out, "external %s: ", entity->base.symbol->string);

	const function_type_t *function_type
		= (const function_type_t*) entity->declaration.type;

	entity_t *parameter = entity->function.parameters.entities;
	for( ; parameter != NULL; parameter = parameter->base.next) {
		assert(parameter->kind == ENTITY_VARIABLE);
		write_type(parameter->declaration.type);
		fputs(" -> ", out);
	}
	if (function_type->variadic) {
		fprintf(stderr, "WARNING: Variadic function not supported yet\n");
	}
	if (function_type->unspecified_parameters) {
		fprintf(stderr, "WARNING: unspecified params not supported\n");
	}
	const type_t *return_type = function_type->return_type;
	write_type(return_type);

	fputs(" = \"", out);
	fputs(entity->base.symbol->string, out);
	fputs("\"", out);

	fputc('\n', out);
}

void write_caml_decls(FILE *output, const translation_unit_t *unit)
{
	out          = output;
	global_scope = &unit->scope;

	ast_set_output(out);
	fprintf(out, "(* WARNING: Automatically generated file - chaning is useless *)\n");

	/* write functions */
	entity_t *entity = unit->scope.entities;
	for( ; entity != NULL; entity = entity->base.next) {
		if (entity->kind != ENTITY_FUNCTION)
			continue;

		write_function(entity);
	}
}
