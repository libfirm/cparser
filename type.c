#include <config.h>

#include "type_t.h"
#include "adt/error.h"

static struct obstack  _type_obst;
struct obstack        *type_obst = &_type_obst;

void init_types()
{
	obstack_init(type_obst);
}

void exit_types()
{
	obstack_free(type_obst, NULL);
}

static
void print_type_qualifiers(FILE *out, const type_t *type)
{
	unsigned qualifiers = type->qualifiers;
	if(qualifiers & TYPE_QUALIFIER_CONST) {
		fputs("const ", out);
	}
	if(qualifiers & TYPE_QUALIFIER_VOLATILE) {
		fputs("volatile ", out);
	}
	if(qualifiers & TYPE_QUALIFIER_RESTRICT) {
		fputs("restrict ", out);
	}
	if(qualifiers & TYPE_QUALIFIER_INLINE) {
		fputs("inline ", out);
	}
}

static
void print_atomic_type(FILE *out, const atomic_type_t *type)
{
	print_type_qualifiers(out, & type->type);

	switch(type->atype) {
	case ATOMIC_TYPE_INVALID:     fputs("INVALIDATOMIC", out); return;
	case ATOMIC_TYPE_VOID:        fputs("void", out); return;
	case ATOMIC_TYPE_BOOL:        fputs("bool", out); return;
	case ATOMIC_TYPE_CHAR:        fputs("char", out); return;
	case ATOMIC_TYPE_SCHAR:       fputs("signed char", out); return;
	case ATOMIC_TYPE_UCHAR:       fputs("unsigned char", out); return;
	case ATOMIC_TYPE_INT:         fputs("int", out); return;
	case ATOMIC_TYPE_UINT:        fputs("unsigned int", out); return;
	case ATOMIC_TYPE_SHORT:       fputs("short", out); return;
	case ATOMIC_TYPE_USHORT:      fputs("unsigned short", out); return;
	case ATOMIC_TYPE_LONG:        fputs("long", out); return;
	case ATOMIC_TYPE_ULONG:       fputs("unsigned long", out); return;
	case ATOMIC_TYPE_LONGLONG:    fputs("long long", out); return;
	case ATOMIC_TYPE_ULONGLONG:   fputs("unsigned long long", out); return;
	case ATOMIC_TYPE_LONG_DOUBLE: fputs("long double", out); return;
	case ATOMIC_TYPE_FLOAT:       fputs("float", out); return;
	case ATOMIC_TYPE_DOUBLE:      fputs("double", out); return;
	}
	fputs("UNKNOWNATOMIC", out);
}

static
void print_method_type(FILE *out, const method_type_t *type)
{
	print_type_qualifiers(out, & type->type);

	fputs("<", out);
	print_type(out, type->result_type);
	fputs(" ", out);

	if(type->abi_style != NULL) {
		fprintf(out, "\"%s\" ", type->abi_style);
	}
	fputs("method(", out);
	method_parameter_type_t *param_type = type->parameter_types;
	int first = 1;
	while(param_type != NULL) {
		if(first) {
			first = 0;
		} else {
			fputs(", ", out);
		}
		print_type(out, param_type->type);
		param_type = param_type->next;
	}
	fputs(")>", out);
}

static
void print_pointer_type(FILE *out, const pointer_type_t *type)
{
	print_type(out, type->points_to);
	fputs("*", out);
	print_type_qualifiers(out, &type->type);
}

void print_type(FILE *out, const type_t *type)
{
	if(type == NULL) {
		fputs("nil type", out);
		return;
	}

	switch(type->type) {
	case TYPE_INVALID:
		fputs("invalid", out);
		return;
	case TYPE_ENUM:
		print_type_qualifiers(out, type);
		fputs("enum (TODO)", out);
		return;
	case TYPE_ATOMIC:
		print_atomic_type(out, (const atomic_type_t*) type);
		return;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		print_type_qualifiers(out, type);
		if(((const compound_type_t*) type)->symbol != NULL) {
			fprintf(out, "%s", ((const compound_type_t*) type)->symbol->string);
		}
		return;
	case TYPE_BUILTIN:
		fputs(((builtin_type_t*) type)->symbol->string, out);
		return;
	case TYPE_METHOD:
		print_method_type(out, (const method_type_t*) type);
		return;
	case TYPE_POINTER:
		print_pointer_type(out, (const pointer_type_t*) type);
		return;
	}
	fputs("unknown", out);
}

int type_valid(const type_t *type)
{
	return type->type != TYPE_INVALID;
}

int is_type_int(const type_t *type)
{
	if(type->type != TYPE_ATOMIC)
		return 0;

	atomic_type_t *atomic_type = (atomic_type_t*) type;
	switch(atomic_type->atype) {
	case ATOMIC_TYPE_CHAR:
	case ATOMIC_TYPE_SCHAR:
	case ATOMIC_TYPE_UCHAR:
	case ATOMIC_TYPE_SHORT:
	case ATOMIC_TYPE_USHORT:
	case ATOMIC_TYPE_INT:
	case ATOMIC_TYPE_UINT:
	case ATOMIC_TYPE_LONG:
	case ATOMIC_TYPE_ULONG:
	case ATOMIC_TYPE_LONGLONG:
	case ATOMIC_TYPE_ULONGLONG:
		return 1;
	default:
		return 0;
	}
}

static __attribute__((unused))
void dbg_type(const type_t *type)
{
	print_type(stdout,type);
	puts("\n");
	fflush(stdout);
}
