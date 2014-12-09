/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include <string.h>

#include "adt/error.h"
#include "adt/pset_new.h"
#include "adt/separator_t.h"
#include "adt/strutil.h"
#include "adt/xmalloc.h"
#include "ast/ast_t.h"
#include "ast/entity_t.h"
#include "ast/printer.h"
#include "ast/symbol_t.h"
#include "ast/symbol_table.h"
#include "ast/type.h"
#include "ast/type_t.h"
#include "write_jna.h"

typedef struct output_limit {
	const char          *filename;
	struct output_limit *next;
} output_limit;

static const scope_t *global_scope;
static FILE          *out;
static pset_new_t     avoid_symbols;
static output_limit  *output_limits;
static const char    *libname;

static const char *fix_builtin_names(const char *name)
{
#define FIX(x) if (streq(name, x)) return "_" x
	FIX("class");
	FIX("final");
	FIX("private");
	FIX("protected");
	FIX("public");
	FIX("this");
	/* TODO put all reserved names here */
#undef FIX
	return name;
}

static const char *get_atomic_type_string(const atomic_type_kind_t type)
{
	switch(type) {
	case ATOMIC_TYPE_CHAR:        return "byte";
	case ATOMIC_TYPE_SCHAR:       return "byte";
	case ATOMIC_TYPE_UCHAR:       return "byte";
	case ATOMIC_TYPE_SHORT:       return "short";
	case ATOMIC_TYPE_USHORT:      return "short";
	case ATOMIC_TYPE_INT:         return "int";
	case ATOMIC_TYPE_UINT:        return "int";
	case ATOMIC_TYPE_LONG:        return "com.sun.jna.NativeLong";
	case ATOMIC_TYPE_ULONG:       return "com.sun.jna.NativeLong";
	case ATOMIC_TYPE_LONGLONG:    return "long";
	case ATOMIC_TYPE_ULONGLONG:   return "long";
	case ATOMIC_TYPE_FLOAT:       return "float";
	case ATOMIC_TYPE_DOUBLE:      return "double";
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
	type_t *orig_points_to = type->points_to;
	type_t *points_to      = skip_typeref(type->points_to);
	if (is_type_atomic(points_to, ATOMIC_TYPE_CHAR)
	    && orig_points_to->kind != TYPE_TYPEDEF) {
		fputs("String", out);
		return;
	}
	if (is_type_pointer(points_to)) {
		/* hack... */
		fputs("java.nio.Buffer", out);
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
	if (entity != NULL) {
		fputs(entity->base.symbol->string, out);
		return;
	}

	/* does the struct have a name? */
	symbol_t *symbol = type->compound->base.symbol;
	if (symbol != NULL) {
		/* TODO: make sure we create a struct for it... */
		fputs(symbol->string, out);
		return;
	}
	/* TODO: create a struct and use its name here... */
	fputs("/* TODO anonymous struct */byte", out);
}

static void write_enum_name(const enum_t *enume)
{
	/* Is there a typedef for this enum? */
	entity_t *entity = find_enum_typedef(enume);
	if (entity != NULL) {
		fputs(entity->base.symbol->string, out);
		return;
	}

	/* does the enum have a name? */
	symbol_t *symbol = enume->base.symbol;
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
	case TYPE_VOID:
		fputs("void", out);
		return;
	case TYPE_ERROR:
	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
	case TYPE_BUILTIN_TEMPLATE:
		panic("invalid type");
	case TYPE_ARRAY:
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
	case EXPR_UNARY_CAST:
		write_expression(expression->value);
		return;
	default:
		panic("unimplemented unary expression");
	}
	write_expression(expression->value);
}

static void write_binary_expression(const binary_expression_t *expression)
{
	fputs("(", out);
	write_expression(expression->left);
	fputc(' ', out);
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
	fputc(' ', out);
	write_expression(expression->right);
	fputs(")", out);
}

static void write_integer(const literal_expression_t *literal)
{
	fwrite(literal->value->begin, 1, literal->suffix - literal->value->begin,
	       out);
}

static void write_expression(const expression_t *expression)
{
	/* TODO */
	switch(expression->kind) {
	case EXPR_LITERAL_INTEGER:
		write_integer(&expression->literal);
		break;

	case EXPR_ENUM_CONSTANT: {
		/* UHOH... hacking */
		entity_t *entity = expression->reference.entity;
		write_enum_name(entity->enum_value.enume);
		fprintf(out, ".%s.val", entity->base.symbol->string);
		break;
	}
	case EXPR_UNARY_CASES:
		write_unary_expression(&expression->unary);
		break;
	case EXPR_BINARY_CASES:
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

	for (const entity_t *entry = entity->first_value;
	     entry != NULL && entry->kind == ENTITY_ENUM_VALUE;
	     entry = entry->base.next) {
		fprintf(out, "\t\t%s", entry->base.symbol->string);
		fprintf(out, "(");
		if (entry->enum_value.value != NULL) {
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
	fprintf(out, "\n");
	fprintf(out, "\t\tprivate static class C {\n");
	fprintf(out, "\t\t\tstatic int next_val;\n");
	fprintf(out, "\t\t}\n");
	fprintf(out, "\n");
	fprintf(out, "\t\t%s(int val) {\n", name);
	fprintf(out, "\t\t\tthis.val = val;\n");
	fprintf(out, "\t\t\tC.next_val = val + 1;\n");
	fprintf(out, "\t\t}\n");
	fprintf(out, "\n");
	fprintf(out, "\t\t%s() {\n", name);
	fprintf(out, "\t\t\tthis.val = C.next_val++;\n");
	fprintf(out, "\t\t}\n");
	fprintf(out, "\n");
	fprintf(out, "\t\tpublic static %s getEnum(int val) {\n", name);
	fprintf(out, "\t\t\tfor (%s entry : values()) {\n", name);
	fprintf(out, "\t\t\t\tif (val == entry.val)\n");
	fprintf(out, "\t\t\t\t\treturn entry;\n");
	fprintf(out, "\t\t\t}\n");
	fprintf(out, "\t\t\treturn null;\n");
	fprintf(out, "\t\t}\n");
	fprintf(out, "\t}\n");
	fprintf(out, "\n");
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
	if (entity->function.body != NULL) {
		fprintf(stderr, "Warning: can't convert function bodies (at %s)\n",
		        entity->base.symbol->string);
		return;
	}


	const function_type_t *function_type
		= (const function_type_t*) entity->declaration.type;

	fputc('\n', out);
	fprintf(out, "\tpublic static native ");
	type_t *return_type = skip_typeref(function_type->return_type);
	write_type(return_type);
	fprintf(out, " %s(", entity->base.symbol->string);

	entity_t   *parameter = entity->function.parameters.entities;
	separator_t sep       = { "", ", " };
	int         n         = 0;
	for ( ; parameter != NULL; parameter = parameter->base.next) {
		assert(parameter->kind == ENTITY_PARAMETER);
		fputs(sep_next(&sep), out);
		write_type(parameter->declaration.type);
		if (parameter->base.symbol != NULL) {
			fprintf(out, " %s", fix_builtin_names(parameter->base.symbol->string));
		} else {
			fprintf(out, " _%d", n++);
		}
	}
	if (function_type->variadic) {
		fputs(sep_next(&sep), out);
		fputs("Object ... args", out);
	}
	fprintf(out, ");\n");
}

void jna_limit_output(const char *filename)
{
	output_limit *limit = xmalloc(sizeof(limit[0]));
	limit->filename = filename;

	limit->next   = output_limits;
	output_limits = limit;
}

void jna_set_libname(const char *new_libname)
{
	libname = new_libname;
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
	fputs("\n", out);

	const char *register_libname = libname;
	if (register_libname == NULL)
		register_libname = "library";

	/* TODO: where to get the name from? */
	fputs("public class binding {\n", out);
	fputs("\tstatic {\n", out);
	fprintf(out, "\t\tNative.register(\"%s\");\n", register_libname);
	fputs("\t}\n", out);
	fputs("\n", out);

	/* read the avoid list */
	FILE *avoid = fopen("avoid.config", "r");
	if (avoid != NULL) {
		for (;;) {
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
	for ( ; entity != NULL; entity = entity->base.next) {
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
		if (is_type_compound(type)) {
			write_compound(entity->base.symbol, &type->compound);
		}
#endif
	}

	/* write functions */
	entity = unit->scope.entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		if (entity->kind != ENTITY_FUNCTION)
			continue;
		if (entity->base.pos.is_system_header)
			continue;
		if (entity->function.elf_visibility != ELF_VISIBILITY_DEFAULT)
			continue;
		if (output_limits != NULL) {
			bool              in_limits  = false;
			char const *const input_name = entity->base.pos.input_name;
			for (output_limit *limit = output_limits; limit != NULL;
			     limit = limit->next) {
			    if (streq(limit->filename, input_name)) {
					in_limits = true;
					break;
				}
			}
			if (!in_limits)
				continue;
		}

		if (pset_new_contains(&avoid_symbols, entity->base.symbol))
			continue;
		write_function(entity);
	}

	fputs("}\n", out);

	pset_new_destroy(&avoid_symbols);
}
