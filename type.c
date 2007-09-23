#include <config.h>

#include <stdio.h>
#include <assert.h>
#include "type_t.h"
#include "type_hash.h"
#include "adt/error.h"

static struct obstack   _type_obst;
struct obstack         *type_obst = &_type_obst;
static FILE            *out;

static void intern_print_type_pre(const type_t *type);
static void intern_print_type_post(const type_t *type);

void init_types(void)
{
	obstack_init(type_obst);
}

void exit_types(void)
{
	obstack_free(type_obst, NULL);
}

void type_set_output(FILE *stream)
{
	out = stream;
}

static
void print_type_qualifiers(unsigned qualifiers)
{
	if(qualifiers & TYPE_QUALIFIER_CONST) {
		fputs("const ", out);
	}
	if(qualifiers & TYPE_QUALIFIER_VOLATILE) {
		fputs("volatile ", out);
	}
	if(qualifiers & TYPE_QUALIFIER_RESTRICT) {
		fputs("__restrict ", out);
	}
	if(qualifiers & TYPE_QUALIFIER_INLINE) {
		fputs("inline ", out);
	}
}

static
void print_atomic_type(const atomic_type_t *type)
{
	print_type_qualifiers(type->type.qualifiers);

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
void print_method_type_pre(const method_type_t *type)
{
	print_type_qualifiers(type->type.qualifiers);

	intern_print_type_pre(type->result_type);

	/* TODO: don't emit braces if we're the toplevel type... */
	fputc('(', out);
}

static
void print_method_type_post(const method_type_t *type, const context_t *context)
{
	/* TODO: don't emit braces if we're the toplevel type... */
	intern_print_type_post(type->result_type);
	fputc(')', out);

	fputc('(', out);

	int                 first     = 1;
	if(context == NULL) {
		method_parameter_t *parameter = type->parameters;
		for( ; parameter != NULL; parameter = parameter->next) {
			if(first) {
				first = 0;
			} else {
				fputs(", ", out);
			}
			print_type(parameter->type);
		}
	} else {
		declaration_t *parameter = context->declarations;
		for( ; parameter != NULL; parameter = parameter->next) {
			if(first) {
				first = 0;
			} else {
				fputs(", ", out);
			}
			print_type_ext(parameter->type, parameter->symbol,
			               &parameter->context);
		}
	}
	if(type->variadic) {
		if(first) {
			first = 0;
		} else {
			fputs(", ", out);
		}
		fputs("...", out);
	}
	if(first && !type->unspecified_parameters) {
		fputs("void", out);
	}
	fputc(')', out);
}

static
void print_pointer_type_pre(const pointer_type_t *type)
{
	intern_print_type_pre(type->points_to);
	fputs("*", out);
	print_type_qualifiers(type->type.qualifiers);
}

static void print_pointer_type_post(const pointer_type_t *type)
{
	intern_print_type_post(type->points_to);
}

static void print_type_enum(const enum_type_t *type)
{
	print_type_qualifiers(type->type.qualifiers);
	if(type->symbol != NULL) {
		fprintf(out, "enum %s", type->symbol->string);
	} else {
		fprintf(out, "enum {\n");

		declaration_t *entry = type->entries_begin;
		for( ; entry != type->entries_end->next; entry = entry->next) {
			fprintf(out, "\t%s", entry->symbol->string);
			if(entry->initializer != NULL) {
				fprintf(out, " = ");
				print_expression(entry->initializer);
			}
			fprintf(out, ",\n");
		}

		fprintf(out, "} ");
	}
}

static void intern_print_type_pre(const type_t *type)
{
	switch(type->type) {
	case TYPE_INVALID:
		fputs("invalid", out);
		return;
	case TYPE_ENUM:
		print_type_enum((const enum_type_t*) type);
		return;
	case TYPE_ATOMIC:
		print_atomic_type((const atomic_type_t*) type);
		return;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		print_type_qualifiers(type->qualifiers);
		if(((const compound_type_t*) type)->symbol != NULL) {
			fprintf(out, "%s", ((const compound_type_t*) type)->symbol->string);
		}
		return;
	case TYPE_BUILTIN:
		fputs(((builtin_type_t*) type)->symbol->string, out);
		return;
	case TYPE_METHOD:
		print_method_type_pre((const method_type_t*) type);
		return;
	case TYPE_POINTER:
		print_pointer_type_pre((const pointer_type_t*) type);
		return;
	}
	fputs("unknown", out);
}

static
void intern_print_type_post(const type_t *type)
{
	switch(type->type) {
	case TYPE_METHOD:
		print_method_type_post((const method_type_t*) type, NULL);
		return;
	case TYPE_POINTER:
		print_pointer_type_post((const pointer_type_t*) type);
		return;
	case TYPE_INVALID:
	case TYPE_ATOMIC:
	case TYPE_ENUM:
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
	case TYPE_BUILTIN:
		break;
	}
}

void print_type(const type_t *type)
{
	print_type_ext(type, NULL, NULL);
}

void print_type_ext(const type_t *type, const symbol_t *symbol,
                    const context_t *context)
{
	if(type == NULL) {
		fputs("nil type", out);
		return;
	}

	intern_print_type_pre(type);
	if(symbol != NULL) {
		fputc(' ', out);
		fputs(symbol->string, out);
	}
	if(type->type == TYPE_METHOD) {
		print_method_type_post((const method_type_t*) type, context);
	} else {
		intern_print_type_post(type);
	}
}

bool type_valid(const type_t *type)
{
	return type->type != TYPE_INVALID;
}

bool is_type_int(const type_t *type)
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

bool is_type_arithmetic(const type_t *type)
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
	case ATOMIC_TYPE_FLOAT:
	case ATOMIC_TYPE_DOUBLE:
	case ATOMIC_TYPE_LONG_DOUBLE:
#ifdef PROVIDE_COMPLEX
	case ATOMIC_TYPE_FLOAT_COMPLEX:
	case ATOMIC_TYPE_DOUBLE_COMPLEX:
	case ATOMIC_TYPE_LONG_DOUBLE_COMPLEX:
#endif
#ifdef PROVIDE_IMAGINARY
	case ATOMIC_TYPE_FLOAT_IMAGINARY:
	case ATOMIC_TYPE_DOUBLE_IMAGINARY:
	case ATOMIC_TYPE_LONG_DOUBLE_IMAGINARY:
#endif
		return 1;
	case ATOMIC_TYPE_INVALID:
	case ATOMIC_TYPE_VOID:
	case ATOMIC_TYPE_BOOL:
		return 0;
	}

	return 0;
}

bool is_type_scalar(const type_t *type)
{
	if(type->type == TYPE_POINTER)
		return 1;

	return is_type_arithmetic(type);
}

static type_t *identify_new_type(type_t *type)
{
	type_t *result = typehash_insert(type);
	if(result != type) {
		obstack_free(type_obst, type);
	}
	return result;
}

type_t *make_atomic_type(atomic_type_type_t type, unsigned qualifiers)
{
	atomic_type_t *atomic_type
		= obstack_alloc(type_obst, sizeof(atomic_type[0]));
	memset(atomic_type, 0, sizeof(atomic_type[0]));
	atomic_type->type.type       = TYPE_ATOMIC;
	atomic_type->type.qualifiers = qualifiers;
	atomic_type->atype           = type;

	return identify_new_type((type_t*) atomic_type);
}

type_t *make_pointer_type(type_t *points_to, unsigned qualifiers)
{
	pointer_type_t *pointer_type
		= obstack_alloc(type_obst, sizeof(pointer_type[0]));
	memset(pointer_type, 0, sizeof(pointer_type[0]));
	pointer_type->type.type       = TYPE_POINTER;
	pointer_type->type.qualifiers = qualifiers;
	pointer_type->points_to       = points_to;

	return identify_new_type((type_t*) pointer_type);
}

static __attribute__((unused))
void dbg_type(const type_t *type)
{
	FILE *old_out = out;
	out = stderr;
	print_type(type);
	puts("\n");
	fflush(stderr);
	out = old_out;
}
