#include <config.h>

#include <errno.h>
#include <string.h>

#include "ast_t.h"
#include "type_t.h"
#include "adt/error.h"

static const context_t *global_context;
static FILE            *out;

static void write_type(const type_t *type);

static const char *get_atomic_type_string(const atomic_type_type_t type)
{
	switch(type) {
	case ATOMIC_TYPE_VOID:       return "void";
	case ATOMIC_TYPE_CHAR:       return "byte";
	case ATOMIC_TYPE_SCHAR:      return "byte";
	case ATOMIC_TYPE_UCHAR:      return "unsigned byte";
	case ATOMIC_TYPE_SHORT:	     return "short";
	case ATOMIC_TYPE_USHORT:     return "unsigned short";
	case ATOMIC_TYPE_INT:        return "int";
	case ATOMIC_TYPE_UINT:       return "unsigned int";
	case ATOMIC_TYPE_LONG:       return "int";
	case ATOMIC_TYPE_ULONG:      return "unsigned int";
	case ATOMIC_TYPE_LONGLONG:   return "long";
	case ATOMIC_TYPE_ULONGLONG:  return "unsigned long";
	case ATOMIC_TYPE_FLOAT:      return "float";
	case ATOMIC_TYPE_DOUBLE:     return "double";
	case ATOMIC_TYPE_LONG_DOUBLE: return "double";
	case ATOMIC_TYPE_BOOL:       return "bool";
	default:                     panic("unsupported atomic type");
	}
}

static void write_atomic_type(const atomic_type_t *type)
{
	fprintf(out, "%s", get_atomic_type_string(type->atype));
}

static void write_pointer_type(const pointer_type_t *type)
{
	write_type(type->points_to);
	fputc('*', out);
}

static void write_compound_type(const compound_type_t *type)
{
	/* first: search for a matching typedef in the global type... */
	declaration_t *declaration = global_context->declarations;
	while(declaration != NULL) {
		if(! (declaration->storage_class & STORAGE_CLASS_TYPEDEF)) {
			declaration = declaration->next;
			continue;
		}
		if(declaration->type == (type_t*) type)
			break;
		declaration = declaration->next;
	}

	if(declaration != NULL) {
		fprintf(out, "%s", declaration->symbol->string);
		return;
	}

	/* does the struct have a name? */
	if(type->symbol != NULL) {
		/* TODO: make sure we create a struct for it... */
		fprintf(out, "%s", type->symbol->string);
		return;
	}
	/* TODO: create a struct and use its name here... */
	fprintf(out, "/* TODO anonymous struct */byte");
}

static void write_method_type(const method_type_t *type)
{
	fprintf(out, "(func(");

	method_parameter_type_t *parameter_type = type->parameter_types;
	int                      first          = 1;
	while(parameter_type != NULL) {
		if(!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}

		if(parameter_type->symbol != NULL) {
			fprintf(out, "%s : ", parameter_type->symbol->string);
		} else {
			/* TODO make up some unused names (or allow _ in fluffy?) */
			fprintf(out, "_ : ");
		}
		write_type(parameter_type->type);

		parameter_type = parameter_type->next;
	}

	fprintf(out, ") : ");
	write_type(type->result_type);
	fprintf(out, ")");
}

static void write_type(const type_t *type)
{
	switch(type->type) {
	case TYPE_ATOMIC:
		write_atomic_type((const atomic_type_t*) type);
		return;
	case TYPE_POINTER:
		write_pointer_type((const pointer_type_t*) type);
		return;
	case TYPE_COMPOUND_UNION:
	case TYPE_COMPOUND_STRUCT:
		write_compound_type((const compound_type_t*) type);
		return;
	case TYPE_METHOD:
		write_method_type((const method_type_t*) type);
		return;
	case TYPE_INVALID:
		panic("invalid type found");
		break;
	default:
		fprintf(out, "/* TODO type */");
		break;
	}
}

static void write_struct_entry(const declaration_t *declaration)
{
	fprintf(out, "\t%s : ", declaration->symbol->string);
	write_type(declaration->type);
	fprintf(out, "\n");
}

static void write_struct(const symbol_t *symbol, const compound_type_t *type)
{
	(void) type;
	fprintf(out, "struct %s:\n", symbol->string);

	const declaration_t *declaration = type->context.declarations;
	while(declaration != NULL) {
		write_struct_entry(declaration);
		declaration = declaration->next;
	}

	fprintf(out, "\n");
}

static void write_variable(const declaration_t *declaration)
{
	fprintf(out, "var %s : ", declaration->symbol->string);
	write_type(declaration->type);
	/* TODO: initializers */
	fprintf(out, "\n");
}

static void write_function(const declaration_t *declaration)
{
	if(declaration->statement != NULL) {
		fprintf(stderr, "Warning: can't convert function bodies (at %s)\n",
		        declaration->symbol->string);
	}

	fprintf(out, "func extern %s(",
	        declaration->symbol->string);

	declaration_t *parameter = declaration->context.declarations;
	int            first     = 1;
	for( ; parameter != NULL; parameter = parameter->next) {
		if(!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}
		if(parameter->symbol != NULL) {
			fprintf(out, "%s : ", parameter->symbol->string);
		} else {
			fputs("_ : ", out);
		}
		write_type(parameter->type);
	}
	fprintf(out, ")");

	const method_type_t *method_type = (const method_type_t*) declaration->type;
	const type_t        *result_type = method_type->result_type;
	if(result_type->type != TYPE_ATOMIC ||
			((const atomic_type_t*) result_type)->atype != ATOMIC_TYPE_VOID) {
		fprintf(out, " : ");
		write_type(result_type);
	}
	fputc('\n', out);
}

void write_fluffy_decls(const translation_unit_t *unit)
{
#if 0
	out = fopen("out.fluffy", "w");
	if(out == NULL) {
		fprintf(stderr, "Couldn't open out.fluffy: %s\n", strerror(errno));
		exit(1);
	}
#endif
	out            = stdout;
	global_context = &unit->context;

	fprintf(out, "/* WARNING: Automatically generated file */\n");

	/* write structs + enums */
	declaration_t *declaration = unit->context.declarations;
	while(declaration != NULL) {
		//fprintf(out, "// Decl: %s\n", declaration->symbol->string);
		if(! (declaration->storage_class & STORAGE_CLASS_TYPEDEF)) {
			declaration = declaration->next;
			continue;
		}
		type_t *type = declaration->type;
		if(type->type == TYPE_COMPOUND_STRUCT) {
			write_struct(declaration->symbol, (compound_type_t*) type);
		} else if(type->type == TYPE_COMPOUND_UNION) {
			/* TODO */
		}

		declaration = declaration->next;
	}

	/* write global variables */
	declaration = unit->context.declarations;
	for( ; declaration != NULL; declaration = declaration->next) {
		if(declaration->storage_class & STORAGE_CLASS_TYPEDEF)
			continue;

		type_t *type = declaration->type;
		if(type->type == TYPE_METHOD)
			continue;

		write_variable(declaration);
	}

	/* write functions */
	declaration = unit->context.declarations;
	for( ; declaration != NULL; declaration = declaration->next) {
		if(declaration->storage_class & STORAGE_CLASS_TYPEDEF)
			continue;

		type_t *type = declaration->type;
		if(type->type != TYPE_METHOD)
			continue;

		write_function(declaration);
	}

	//fclose(out);
}
