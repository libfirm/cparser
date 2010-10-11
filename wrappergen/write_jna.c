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

#include <errno.h>
#include <string.h>

#include "write_jna.h"
#include "symbol_t.h"
#include "ast_t.h"
#include "type_t.h"
#include "entity_t.h"
#include "type.h"
#include "printer.h"
#include "adt/error.h"
#include <libfirm/adt/pset_new.h>

static const scope_t *global_scope;
static FILE          *out;
static pset_new_t     avoid_symbols;

static void write_type(type_t *type);

static bool is_system_header(const char *fname)
{
	if (strncmp(fname, "/usr/include", 12) == 0)
		return true;
	if (fname == builtin_source_position.input_name)
		return true;
	return false;
}

static const char *fix_builtin_names(const char *name)
{
	if (strcmp(name, "class") == 0) {
		return "_class";
	} else if(strcmp(name, "this") == 0) {
		return "_this";
	} else if(strcmp(name, "public") == 0) {
		return "_public";
	} else if(strcmp(name, "protected") == 0) {
		return "_protected";
	} else if(strcmp(name, "private") == 0) {
		return "_private";
	} else if(strcmp(name, "final") == 0) {
		return "_final";
	}
	/* TODO put all reserved names here */
	return name;
}

static const char *get_atomic_type_string(const atomic_type_kind_t type)
{
	switch(type) {
	case ATOMIC_TYPE_VOID:        return "void";
	case ATOMIC_TYPE_CHAR:        return "byte";
	case ATOMIC_TYPE_SCHAR:       return "byte";
	case ATOMIC_TYPE_UCHAR:       return "byte";
	case ATOMIC_TYPE_SHORT:	      return "short";
	case ATOMIC_TYPE_USHORT:      return "short";
	case ATOMIC_TYPE_INT:         return "int";
	case ATOMIC_TYPE_UINT:        return "int";
	case ATOMIC_TYPE_LONG:        return "com.sun.jna.NativeLong";
	case ATOMIC_TYPE_ULONG:       return "com.sun.jna.NativeLong";
	case ATOMIC_TYPE_LONGLONG:    return "long";
	case ATOMIC_TYPE_ULONGLONG:   return "long";
	case ATOMIC_TYPE_FLOAT:       return "float";
	case ATOMIC_TYPE_DOUBLE:      return "double";
	case ATOMIC_TYPE_LONG_DOUBLE: return "double";
	case ATOMIC_TYPE_BOOL:        return "boolean";
	default:                      panic("unsupported atomic type");
	}
}

static void write_atomic_type(const atomic_type_t *type)
{
	fputs(get_atomic_type_string(type->akind), out);
}

static void write_pointer_type(const pointer_type_t *type)
{
	type_t *points_to = skip_typeref(type->points_to);
	if (is_type_atomic(points_to, ATOMIC_TYPE_CHAR)) {
		fputs("String", out);
		return;
	}
	if (is_type_pointer(points_to)) {
		/* hack... */
		fputs("Pointer[]", out);
		return;
	}
	fputs("Pointer", out);
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

static entity_t *find_enum_typedef(const enum_t *enume)
{
	/* first: search for a matching typedef in the global type... */
	entity_t *entity = global_scope->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		if (entity->kind != ENTITY_TYPEDEF)
			continue;
		type_t *type = entity->typedefe.type;
		if (type->kind != TYPE_ENUM)
			continue;

		enum_t *e_entity = type->enumt.enume;
		if (e_entity == enume)
			break;
	}

	return entity;
}

static void write_compound_type(const compound_type_t *type)
{
	entity_t *entity = find_typedef((const type_t*) type);
	if(entity != NULL) {
		fputs(entity->base.symbol->string, out);
		return;
	}

	/* does the struct have a name? */
	symbol_t *symbol = type->compound->base.symbol;
	if(symbol != NULL) {
		/* TODO: make sure we create a struct for it... */
		fputs(symbol->string, out);
		return;
	}
	/* TODO: create a struct and use its name here... */
	fputs("/* TODO anonymous struct */byte", out);
}

static void write_enum_name(const enum_type_t *type)
{
	entity_t *entity = find_typedef((const type_t*) type);
	if (entity != NULL) {
		fputs(entity->base.symbol->string, out);
		return;
	}

	/* does the enum have a name? */
	symbol_t *symbol = type->enume->base.symbol;
	if (symbol != NULL) {
		/* TODO: make sure we create an enum for it... */
		fputs(symbol->string, out);
		return;
	}

	/* now we have a problem as we don't know how we'll call the anonymous
	 * enum */
	panic("can't reference entries from anonymous enums yet");
}

static void write_enum_type(const enum_type_t *type)
{
	entity_t *entity = find_typedef((const type_t*) type);
	if (entity != NULL) {
		fprintf(out, "/* %s */int", entity->base.symbol->string);
		return;
	}

	/* does the enum have a name? */
	symbol_t *symbol = type->enume->base.symbol;
	if (symbol != NULL) {
		/* TODO: make sure we create an enum for it... */
		fprintf(out, "/* %s */int", symbol->string);
		return;
	}
	fprintf(out, "/* anonymous enum */int");
}

static void write_type(type_t *type)
{
	type = skip_typeref(type);
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
	case TYPE_BUILTIN:
		write_type(type->builtin.real_type);
		return;
	case TYPE_ERROR:
	case TYPE_INVALID:
	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
		panic("invalid type found");
	case TYPE_ARRAY:
	case TYPE_BITFIELD:
	case TYPE_REFERENCE:
	case TYPE_FUNCTION:
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
		fprintf(out, "/* TODO type */Pointer");
		break;
	}
}

#if 0
static void write_compound_entry(const entity_t *entity)
{
	fprintf(out, "\t%s : ", entity->base.symbol->string);
	write_type(entity->declaration.type);
	fprintf(out, "\n");
}

static void write_compound(const symbol_t *symbol, const compound_type_t *type)
{
	fprintf(out, "%s %s:\n",
	        type->base.kind == TYPE_COMPOUND_STRUCT ? "struct" : "union",
			symbol->string);

	const entity_t *entity = type->compound->members.entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		write_compound_entry(entity);
	}

	fprintf(out, "\n");
}
#endif

static void write_expression(const expression_t *expression);

static void write_unary_expression(const unary_expression_t *expression)
{
	switch(expression->base.kind) {
	case EXPR_UNARY_NEGATE:
		fputc('-', out);
		break;
	case EXPR_UNARY_NOT:
		fputc('!', out);
		break;
	case EXPR_UNARY_CAST_IMPLICIT:
		write_expression(expression->value);
		return;
	default:
		panic("unimeplemented unary expression found");
	}
	write_expression(expression->value);
}

static void write_binary_expression(const binary_expression_t *expression)
{
	fputs("(", out);
	write_expression(expression->left);
	switch(expression->base.kind) {
	case EXPR_BINARY_BITWISE_OR:  fputs("|", out); break;
	case EXPR_BINARY_BITWISE_AND: fputs("&", out); break;
	case EXPR_BINARY_BITWISE_XOR: fputs("^", out); break;
	case EXPR_BINARY_SHIFTLEFT:   fputs("<<", out); break;
	case EXPR_BINARY_SHIFTRIGHT:  fputs(">>", out); break;
	case EXPR_BINARY_ADD:         fputs("+", out); break;
	case EXPR_BINARY_SUB:         fputs("-", out); break;
	case EXPR_BINARY_MUL:         fputs("*", out); break;
	case EXPR_BINARY_DIV:         fputs("/", out); break;
	default:
		panic("unimplemented binexpr");
	}
	write_expression(expression->right);
	fputs(")", out);
}

static void write_expression(const expression_t *expression)
{
	/* TODO */
	switch(expression->kind) {
	case EXPR_LITERAL_INTEGER:
	case EXPR_LITERAL_INTEGER_OCTAL:
		fprintf(out, "%s", expression->literal.value.begin);
		break;
	case EXPR_LITERAL_INTEGER_HEXADECIMAL:
		fprintf(out, "0x%s", expression->literal.value.begin);
		break;
	case EXPR_REFERENCE_ENUM_VALUE: {
		/* UHOH... hacking */
		entity_t *entity = expression->reference.entity;
		write_enum_name(& entity->enum_value.enum_type->enumt);
		fprintf(out, ".%s.val", entity->base.symbol->string);
		break;
	}
	EXPR_UNARY_CASES
		write_unary_expression(&expression->unary);
		break;
	EXPR_BINARY_CASES
		write_binary_expression(&expression->binary);
		break;
	default:
		panic("not implemented expression");
	}
}

static void write_enum(const symbol_t *symbol, const enum_t *entity)
{
	char buf[128];
	const char *name;

	if (symbol == NULL) {
		static int lastenum = 0;
		snprintf(buf, sizeof(buf), "AnonEnum%d", lastenum++);
		name = buf;
	} else {
		name = symbol->string;
	}

	fprintf(out, "\tpublic static enum %s {\n", name);

	entity_t *entry = entity->base.next;
	for ( ; entry != NULL && entry->kind == ENTITY_ENUM_VALUE;
			entry = entry->base.next) {
		fprintf(out, "\t\t%s", entry->base.symbol->string);
		fprintf(out, "(");
		if(entry->enum_value.value != NULL) {
			write_expression(entry->enum_value.value);
		}
		fprintf(out, ")");
		if (entry->base.next != NULL
				&& entry->base.next->kind == ENTITY_ENUM_VALUE) {
			fputs(",\n", out);
		} else {
			fputs(";\n", out);
		}
	}
	fprintf(out, "\t\tpublic final int val;\n");
	fprintf(out, "\t\tprivate static class C { static int next_val; }\n\n");
	fprintf(out, "\t\t%s(int val) {\n", name);
	fprintf(out, "\t\t\tthis.val = val;\n");
	fprintf(out, "\t\t\tC.next_val = val + 1;\n");
	fprintf(out, "\t\t}\n");
	fprintf(out, "\t\t%s() {\n", name);
	fprintf(out, "\t\t\tthis.val = C.next_val++;\n");
	fprintf(out, "\t\t}\n");
	fprintf(out, "\t\t\n");
	fprintf(out, "\t\tpublic static %s getEnum(int val) {\n", name);
	fprintf(out, "\t\t\tfor(%s entry : values()) {\n", name);
	fprintf(out, "\t\t\t\tif (val == entry.val)\n");
	fprintf(out, "\t\t\t\t\treturn entry;\n");
	fprintf(out, "\t\t\t}\n");
	fprintf(out, "\t\t\treturn null;\n");
	fprintf(out, "\t\t}\n");
	fprintf(out, "\t}\n");
}

#if 0
static void write_variable(const entity_t *entity)
{
	fprintf(out, "var %s : ", entity->base.symbol->string);
	write_type(entity->declaration.type);
	fprintf(out, "\n");
}
#endif

static void write_function(const entity_t *entity)
{
	if (entity->function.statement != NULL) {
		fprintf(stderr, "Warning: can't convert function bodies (at %s)\n",
		        entity->base.symbol->string);
		return;
	}


	const function_type_t *function_type
		= (const function_type_t*) entity->declaration.type;

	fprintf(out, "\tpublic static native ");
	type_t *return_type = skip_typeref(function_type->return_type);
	write_type(return_type);
	fprintf(out, " %s(", entity->base.symbol->string);

	entity_t *parameter = entity->function.parameters.entities;
	int       first     = 1;
	int       n         = 0;
	for( ; parameter != NULL; parameter = parameter->base.next) {
		assert(parameter->kind == ENTITY_PARAMETER);
		if(!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}
		write_type(parameter->declaration.type);
		if(parameter->base.symbol != NULL) {
			fprintf(out, " %s", fix_builtin_names(parameter->base.symbol->string));
		} else {
			fprintf(out, " _%d", n++);
		}
	}
	if(function_type->variadic) {
		if(!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}
		fputs("Object ... args", out);
	}
	fprintf(out, ");\n");
}


void write_jna_decls(FILE *output, const translation_unit_t *unit)
{
	out          = output;
	global_scope = &unit->scope;

	pset_new_init(&avoid_symbols);

	print_to_file(out);
	fprintf(out, "/* WARNING: Automatically generated file */\n");
	fputs("import com.sun.jna.Native;\n", out);
	fputs("import com.sun.jna.Pointer;\n", out);
	fputs("\n\n", out);

	/* TODO: where to get the name from? */
	fputs("public class binding {\n", out);
	fputs("\tstatic { Native.register(\"firm\"); }\n", out);

	/* read the avoid list */
	FILE *avoid = fopen("avoid.config", "r");
	if (avoid != NULL) {
		while (!feof(avoid)) {
			char buf[1024];
			char *res = fgets(buf, sizeof(buf), avoid);
			if (res == NULL)
				break;
			if (buf[0] == 0)
				continue;

			size_t len = strlen(buf);
			if (buf[len-1] == '\n')
				buf[len-1] = 0;

			char *str = malloc(len+1);
			memcpy(str, buf, len+1);
			symbol_t *symbol = symbol_table_insert(str);
			pset_new_insert(&avoid_symbols, symbol);
		}
		fclose(avoid);
	}

	/* write structs,unions + enums */
	entity_t *entity = unit->scope.entities;
	for( ; entity != NULL; entity = entity->base.next) {
		if (entity->kind == ENTITY_ENUM) {
			if (find_enum_typedef(&entity->enume) != NULL)
				continue;
			write_enum(entity->base.symbol, &entity->enume);
		} else if (entity->kind == ENTITY_TYPEDEF) {
			type_t *type = entity->typedefe.type;
			if (type->kind == TYPE_ENUM) {
				write_enum(entity->base.symbol, type->enumt.enume);
			}
		}

#if 0
		if(type->kind == TYPE_COMPOUND_STRUCT
				|| type->kind == TYPE_COMPOUND_UNION) {
			write_compound(entity->base.symbol, &type->compound);
		}
#endif
	}

	/* write functions */
	entity = unit->scope.entities;
	for( ; entity != NULL; entity = entity->base.next) {
		if (entity->kind != ENTITY_FUNCTION)
			continue;
		if (is_system_header(entity->base.source_position.input_name))
			continue;

		if (pset_new_contains(&avoid_symbols, entity->base.symbol))
			continue;
		write_function(entity);
	}

	fputs("}\n", out);

	pset_new_destroy(&avoid_symbols);
}
