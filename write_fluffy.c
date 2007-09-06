#include <config.h>

#include <errno.h>
#include <string.h>

#include "ast_t.h"
#include "type_t.h"
#include "adt/error.h"

static FILE *out;

static void write_type(const type_t *type);

static const char *get_atomic_type_string(const atomic_type_type_t type)
{
	switch(type) {
	case ATOMIC_TYPE_VOID:      return "void";
	case ATOMIC_TYPE_CHAR:      return "byte";
	case ATOMIC_TYPE_SCHAR:     return "byte";
	case ATOMIC_TYPE_UCHAR:     return "unsigned byte";
	case ATOMIC_TYPE_SHORT:	    return "short";
	case ATOMIC_TYPE_USHORT:    return "unsigned short";
	case ATOMIC_TYPE_INT:       return "int";
	case ATOMIC_TYPE_UINT:      return "unsigned int";
	case ATOMIC_TYPE_LONG:      return "int";
	case ATOMIC_TYPE_ULONG:     return "unsigned int";
	case ATOMIC_TYPE_LONGLONG:  return "long";
	case ATOMIC_TYPE_ULONGLONG: return "unsigned long";
	case ATOMIC_TYPE_FLOAT:     return "float";
	case ATOMIC_TYPE_DOUBLE:    return "double";
	case ATOMIC_TYPE_BOOL:      return "bool";
	default:
								panic("unsupported atomic type");
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

static void write_type(const type_t *type)
{
	switch(type->type) {
	case TYPE_ATOMIC:
		write_atomic_type((const atomic_type_t*) type);
		return;
	case TYPE_POINTER:
		write_pointer_type((const pointer_type_t*) type);
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

void write_fluffy_decls(const translation_unit_t *unit)
{
#if 0
	out = fopen("out.fluffy", "w");
	if(out == NULL) {
		fprintf(stderr, "Couldn't open out.fluffy: %s\n", strerror(errno));
		exit(1);
	}
#endif
	out = stdout;

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
	while(declaration != NULL) {
		if(declaration->storage_class & STORAGE_CLASS_TYPEDEF) {
			declaration = declaration->next;
			continue;
		}

		type_t *type = declaration->type;
		if(type->type != TYPE_METHOD) {
			write_variable(declaration);
		}
		declaration = declaration->next;
	}

	//fclose(out);
}
