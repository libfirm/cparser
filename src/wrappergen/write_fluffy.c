/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "adt/error.h"
#include "adt/separator_t.h"
#include "ast/ast_t.h"
#include "ast/entity_t.h"
#include "ast/printer.h"
#include "ast/symbol_t.h"
#include "ast/type.h"
#include "ast/type_t.h"
#include "write_fluffy.h"

static const scope_t *global_scope;
static FILE          *out;

static void write_type(const type_t *type);

static const char *get_atomic_type_string(const atomic_type_kind_t type)
{
	switch(type) {
	case ATOMIC_TYPE_CHAR:        return "byte";
	case ATOMIC_TYPE_SCHAR:       return "byte";
	case ATOMIC_TYPE_UCHAR:       return "unsigned byte";
	case ATOMIC_TYPE_SHORT:       return "short";
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
	entity_t *entity = find_typedef((const type_t*) type);
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
	entity_t *entity = find_typedef((const type_t*) type);
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
	separator_t           sep       = { "", ", " };
	while(parameter != NULL) {
		fputs(sep_next(&sep), out);

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
	case TYPE_VOID:
		fputs("void", out);
		return;
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
	default:
		fprintf(out, "/* TODO type */");
		break;
	}
}

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
	default:
		panic("unimplemented unary expression");
	}
	write_expression(expression->value);
}

static void write_expression(const expression_t *expression)
{
	switch(expression->kind) {
	case EXPR_LITERAL_INTEGER:
		fprintf(out, "%s", expression->literal.value->begin);
		break;
	case EXPR_UNARY_CASES:
		write_unary_expression((const unary_expression_t*) expression);
		break;
	default:
		panic("not implemented expression");
	}
}

static void write_enum(const symbol_t *symbol, const enum_type_t *type)
{
	fprintf(out, "enum %s:\n", symbol->string);

	for (const entity_t *entry = type->enume->first_value;
	     entry != NULL && entry->kind == ENTITY_ENUM_VALUE;
	     entry = entry->base.next) {
		fprintf(out, "\t%s", entry->base.symbol->string);
		if (entry->enum_value.value != NULL) {
			fprintf(out, " <- ");
			write_expression(entry->enum_value.value);
		}
		fputc('\n', out);
	}
	fprintf(out, "typealias %s <- int\n", symbol->string);
	fprintf(out, "\n");
}

static void write_variable(const entity_t *entity)
{
	fprintf(out, "var %s : ", entity->base.symbol->string);
	write_type(entity->declaration.type);
	fprintf(out, "\n");
}

static void write_function(const entity_t *entity)
{
	if (entity->function.body != NULL) {
		fprintf(stderr, "Warning: can't convert function bodies (at %s)\n",
		        entity->base.symbol->string);
	}

	fprintf(out, "func extern %s(", entity->base.symbol->string);

	const function_type_t *function_type
		= (const function_type_t*) entity->declaration.type;

	entity_t   *parameter = entity->function.parameters.entities;
	separator_t sep       = { "", ", " };
	for( ; parameter != NULL; parameter = parameter->base.next) {
		assert(parameter->kind == ENTITY_PARAMETER);
		fputs(sep_next(&sep), out);
		if (parameter->base.symbol != NULL) {
			fprintf(out, "%s : ", parameter->base.symbol->string);
		} else {
			fputs("_ : ", out);
		}
		write_type(parameter->declaration.type);
	}
	if (function_type->variadic) {
		fputs(sep_next(&sep), out);
		fputs("...", out);
	}
	fprintf(out, ")");

	const type_t *return_type = skip_typeref(function_type->return_type);
	if (!is_type_void(return_type)) {
		fprintf(out, " : ");
		write_type(return_type);
	}
	fputc('\n', out);
}

void write_fluffy_decls(FILE *output, const translation_unit_t *unit)
{
	out            = output;
	global_scope = &unit->scope;

	print_to_file(out);
	fprintf(out, "/* WARNING: Automatically generated file */\n");

	/* write structs,unions + enums */
	entity_t *entity = unit->scope.entities;
	for( ; entity != NULL; entity = entity->base.next) {
		if (entity->kind != ENTITY_TYPEDEF)
			continue;

		type_t *type = entity->typedefe.type;
		if (is_type_compound(type)) {
			write_compound(entity->base.symbol, &type->compound);
		} else if (type->kind == TYPE_ENUM) {
			write_enum(entity->base.symbol, &type->enumt);
		}
	}

	/* write global variables */
	entity = unit->scope.entities;
	for( ; entity != NULL; entity = entity->base.next) {
		if (entity->kind != ENTITY_VARIABLE)
			continue;

		write_variable(entity);
	}

	/* write functions */
	entity = unit->scope.entities;
	for( ; entity != NULL; entity = entity->base.next) {
		if (entity->kind != ENTITY_FUNCTION)
			continue;

		write_function(entity);
	}
}
