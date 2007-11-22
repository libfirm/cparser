#include <config.h>

#include <stdio.h>
#include <assert.h>
#include "type_t.h"
#include "type_hash.h"
#include "adt/error.h"

static struct obstack   _type_obst;
struct obstack         *type_obst = &_type_obst;
static FILE            *out;
static int              type_visited = 0;
static bool             print_compound_entries;

static void intern_print_type_pre(type_t *type);
static void intern_print_type_post(type_t *type);

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

void set_print_compound_entries(bool enabled)
{
	print_compound_entries = enabled;
}

void inc_type_visited(void)
{
	type_visited++;
}

static
void print_type_qualifiers(unsigned qualifiers)
{
	if(qualifiers & TYPE_QUALIFIER_CONST)    fputs("const ",    out);
	if(qualifiers & TYPE_QUALIFIER_VOLATILE) fputs("volatile ", out);
	if(qualifiers & TYPE_QUALIFIER_RESTRICT) fputs("restrict ", out);
}

static
void print_atomic_type(const atomic_type_t *type)
{
	print_type_qualifiers(type->type.qualifiers);

	const char *s;
	switch(type->atype) {
	case ATOMIC_TYPE_INVALID:     s = "INVALIDATOMIC";      break;
	case ATOMIC_TYPE_VOID:        s = "void";               break;
	case ATOMIC_TYPE_BOOL:        s = "_Bool";              break;
	case ATOMIC_TYPE_CHAR:        s = "char";               break;
	case ATOMIC_TYPE_SCHAR:       s = "signed char";        break;
	case ATOMIC_TYPE_UCHAR:       s = "unsigned char";      break;
	case ATOMIC_TYPE_INT:         s = "int";                break;
	case ATOMIC_TYPE_UINT:        s = "unsigned int";       break;
	case ATOMIC_TYPE_SHORT:       s = "short";              break;
	case ATOMIC_TYPE_USHORT:      s = "unsigned short";     break;
	case ATOMIC_TYPE_LONG:        s = "long";               break;
	case ATOMIC_TYPE_ULONG:       s = "unsigned long";      break;
	case ATOMIC_TYPE_LONGLONG:    s = "long long";          break;
	case ATOMIC_TYPE_ULONGLONG:   s = "unsigned long long"; break;
	case ATOMIC_TYPE_LONG_DOUBLE: s = "long double";        break;
	case ATOMIC_TYPE_FLOAT:       s = "float";              break;
	case ATOMIC_TYPE_DOUBLE:      s = "double";             break;
	default:                      s = "UNKNOWNATOMIC";      break;
	}
	fputs(s, out);
}

static void print_function_type_pre(const function_type_t *type)
{
	print_type_qualifiers(type->type.qualifiers);

	intern_print_type_pre(type->result_type);

	/* TODO: don't emit braces if we're the toplevel type... */
	fputc('(', out);
}

static void print_function_type_post(const function_type_t *type,
                                     const context_t *context)
{
	/* TODO: don't emit braces if we're the toplevel type... */
	intern_print_type_post(type->result_type);
	fputc(')', out);

	fputc('(', out);

	int                 first     = 1;
	if(context == NULL) {
		function_parameter_t *parameter = type->parameters;
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

static void print_pointer_type_pre(const pointer_type_t *type)
{
	intern_print_type_pre(type->points_to);
	fputs("*", out);
	print_type_qualifiers(type->type.qualifiers);
}

static void print_pointer_type_post(const pointer_type_t *type)
{
	intern_print_type_post(type->points_to);
}

static void print_array_type_pre(const array_type_t *type)
{
	intern_print_type_pre(type->element_type);
}

static void print_array_type_post(const array_type_t *type)
{
	intern_print_type_post(type->element_type);
	fputc('[', out);
	if(type->is_static) {
		fputs("static ", out);
	}
	print_type_qualifiers(type->type.qualifiers);
	if(type->size != NULL) {
		print_expression(type->size);
	}
	fputc(']', out);
}

void print_enum_definition(const declaration_t *declaration)
{
	fputs("{\n", out);

	change_indent(1);

	declaration_t *entry = declaration->next;
	for( ; entry != NULL && entry->storage_class == STORAGE_CLASS_ENUM_ENTRY;
	       entry = entry->next) {

		print_indent();
		fprintf(out, "%s", entry->symbol->string);
		if(entry->init.initializer != NULL) {
			fprintf(out, " = ");
			print_expression(entry->init.enum_value);
		}
		fprintf(out, ",\n");
	}

	change_indent(-1);
	print_indent();
	fputs("}", out);
}

static void print_type_enum(const enum_type_t *type)
{
	print_type_qualifiers(type->type.qualifiers);
	fputs("enum ", out);

	declaration_t *declaration = type->declaration;
	symbol_t      *symbol      = declaration->symbol;
	if(symbol != NULL) {
		fputs(symbol->string, out);
	} else {
		print_enum_definition(declaration);
	}
}

void print_compound_definition(const declaration_t *declaration)
{
	fputs("{\n", out);
	change_indent(1);

	declaration_t *iter = declaration->context.declarations;
	for( ; iter != NULL; iter = iter->next) {
		print_indent();
		print_declaration(iter);
		fputc('\n', out);
	}

	change_indent(-1);
	print_indent();
	fputs("}", out);
}

static void print_compound_type(const compound_type_t *type)
{
	print_type_qualifiers(type->type.qualifiers);

	if(type->type.type == TYPE_COMPOUND_STRUCT) {
		fputs("struct ", out);
	} else {
		assert(type->type.type == TYPE_COMPOUND_UNION);
		fputs("union ", out);
	}

	declaration_t *declaration = type->declaration;
	symbol_t      *symbol      = declaration->symbol;
	if(symbol != NULL) {
		fputs(symbol->string, out);
	} else {
		print_compound_definition(declaration);
	}
}

static void print_typedef_type_pre(typedef_type_t *type)
{
	fputs(type->declaration->symbol->string, out);
}

static void print_typeof_type_pre(typeof_type_t *type)
{
	fputs("typeof(", out);
	if(type->expression != NULL) {
		assert(type->typeof_type == NULL);
		print_expression(type->expression);
	} else {
		print_type(type->typeof_type);
	}
	fputc(')', out);
}

static void intern_print_type_pre(type_t *type)
{
	switch(type->type) {
	case TYPE_INVALID:
		fputs("invalid", out);
		return;
	case TYPE_ENUM:
		print_type_enum((enum_type_t*) type);
		return;
	case TYPE_ATOMIC:
		print_atomic_type((atomic_type_t*) type);
		return;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		print_compound_type((compound_type_t*) type);
		return;
	case TYPE_BUILTIN:
		fputs(((builtin_type_t*) type)->symbol->string, out);
		return;
	case TYPE_FUNCTION:
		print_function_type_pre((function_type_t*) type);
		return;
	case TYPE_POINTER:
		print_pointer_type_pre((pointer_type_t*) type);
		return;
	case TYPE_ARRAY:
		print_array_type_pre((array_type_t*) type);
		return;
	case TYPE_TYPEDEF:
		print_typedef_type_pre((typedef_type_t*) type);
		return;
	case TYPE_TYPEOF:
		print_typeof_type_pre((typeof_type_t*) type);
		return;
	}
	fputs("unknown", out);
}

static void intern_print_type_post(type_t *type)
{
	switch(type->type) {
	case TYPE_FUNCTION:
		print_function_type_post((const function_type_t*) type, NULL);
		return;
	case TYPE_POINTER:
		print_pointer_type_post((const pointer_type_t*) type);
		return;
	case TYPE_ARRAY:
		print_array_type_post((const array_type_t*) type);
		return;
	case TYPE_INVALID:
	case TYPE_ATOMIC:
	case TYPE_ENUM:
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
	case TYPE_BUILTIN:
	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
		break;
	}
}

void print_type(type_t *type)
{
	print_type_ext(type, NULL, NULL);
}

void print_type_ext(type_t *type, const symbol_t *symbol,
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
	if(type->type == TYPE_FUNCTION) {
		print_function_type_post((const function_type_t*) type, context);
	} else {
		intern_print_type_post(type);
	}
}

bool type_valid(const type_t *type)
{
	return type->type != TYPE_INVALID;
}

bool is_type_integer(const type_t *type)
{
	if(type->type == TYPE_ENUM)
		return true;

	if(type->type != TYPE_ATOMIC)
		return false;

	atomic_type_t *atomic_type = (atomic_type_t*) type;
	switch(atomic_type->atype) {
	case ATOMIC_TYPE_BOOL:
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
		return true;
	default:
		return false;
	}
}

bool is_type_floating(const type_t *type)
{
	if(type->type != TYPE_ATOMIC)
		return false;

	atomic_type_t *atomic_type = (atomic_type_t*) type;
	switch(atomic_type->atype) {
	case ATOMIC_TYPE_FLOAT:
	case ATOMIC_TYPE_DOUBLE:
	case ATOMIC_TYPE_LONG_DOUBLE:
#ifdef PROVIDE_COMPLEX
	case ATOMIC_TYPE_FLOAT_COMPLEX:
	case ATOMIC_TYPE_DOUBLE_COMPLEX:
	case ATOMIC_TYPE_LONG_DOUBLE_COMPLEX:
	case ATOMIC_TYPE_FLOAT_IMAGINARY:
	case ATOMIC_TYPE_DOUBLE_IMAGINARY:
	case ATOMIC_TYPE_LONG_DOUBLE_IMAGINARY:
#endif
		return true;
	default:
		return false;
	}
}

bool is_type_signed(const type_t *type)
{
	/* enum types are int for now */
	if(type->type == TYPE_ENUM)
		return true;

	if(type->type != TYPE_ATOMIC)
		return false;

	atomic_type_t *atomic_type = (atomic_type_t*) type;
	switch(atomic_type->atype) {
	case ATOMIC_TYPE_CHAR:
	case ATOMIC_TYPE_SCHAR:
	case ATOMIC_TYPE_SHORT:
	case ATOMIC_TYPE_INT:
	case ATOMIC_TYPE_LONG:
	case ATOMIC_TYPE_LONGLONG:
	case ATOMIC_TYPE_FLOAT:
	case ATOMIC_TYPE_DOUBLE:
	case ATOMIC_TYPE_LONG_DOUBLE:
#ifdef PROVIDE_COMPLEX
	case ATOMIC_TYPE_FLOAT_COMPLEX:
	case ATOMIC_TYPE_DOUBLE_COMPLEX:
	case ATOMIC_TYPE_LONG_DOUBLE_COMPLEX:
	case ATOMIC_TYPE_FLOAT_IMAGINARY:
	case ATOMIC_TYPE_DOUBLE_IMAGINARY:
	case ATOMIC_TYPE_LONG_DOUBLE_IMAGINARY:
#endif
		return true;

	case ATOMIC_TYPE_BOOL:
	case ATOMIC_TYPE_UCHAR:
	case ATOMIC_TYPE_USHORT:
	case ATOMIC_TYPE_UINT:
	case ATOMIC_TYPE_ULONG:
	case ATOMIC_TYPE_ULONGLONG:
		return false;

	case ATOMIC_TYPE_INVALID:
	case ATOMIC_TYPE_VOID:
		return false;
	}

	panic("invalid atomic type found");
	return false;
}

bool is_type_arithmetic(const type_t *type)
{
	if(is_type_integer(type) || is_type_floating(type))
		return 1;

	return 0;
}

bool is_type_scalar(const type_t *type)
{
	if(type->type == TYPE_POINTER)
		return 1;

	return is_type_arithmetic(type);
}

bool is_type_incomplete(const type_t *type)
{
	switch(type->type) {
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION: {
		const compound_type_t *compound_type
			= (const compound_type_t*) type;
		declaration_t *declaration = compound_type->declaration;
		return !declaration->init.is_defined;
	}
	case TYPE_FUNCTION:
		return true;

	case TYPE_ARRAY:
	case TYPE_ATOMIC:
	case TYPE_POINTER:
	case TYPE_ENUM:
		return false;

	case TYPE_TYPEDEF:
	case TYPE_TYPEOF:
	case TYPE_BUILTIN:
		panic("is_type_incomplete called without typerefs skipped");
	case TYPE_INVALID:
		break;
	}

	panic("invalid type found");
}

bool types_compatible(const type_t *type1, const type_t *type2)
{
	(void) type1;
	(void) type2;
	return true;
}

bool pointers_compatible(const type_t *type1, const type_t *type2)
{
	assert(type1->type == TYPE_POINTER);
	assert(type2->type == TYPE_POINTER);
	pointer_type_t *pointer_type1 = (pointer_type_t*) type1;
	pointer_type_t *pointer_type2 = (pointer_type_t*) type2;
	return types_compatible(pointer_type1->points_to,
	                        pointer_type2->points_to);
}

type_t *skip_typeref(type_t *type)
{
	while(1) {
		switch(type->type) {
		case TYPE_TYPEDEF: {
			const typedef_type_t *typedef_type = (const typedef_type_t*) type;
			type = typedef_type->declaration->type;
			continue;
		}
		case TYPE_TYPEOF: {
			const typeof_type_t *typeof_type = (const typeof_type_t *) type;
			if(typeof_type->typeof_type != NULL) {
				type = typeof_type->typeof_type;
			} else {
				type = typeof_type->expression->datatype;
			}
			continue;
		}
		case TYPE_BUILTIN: {
			const builtin_type_t *builtin_type = (const builtin_type_t*) type;
			type = builtin_type->real_type;
			continue;
		}
		default:
			break;
		}
		break;
	}

	return type;
}



static type_t *identify_new_type(type_t *type)
{
	type_t *result = typehash_insert(type);
	if(result != type) {
		obstack_free(type_obst, type);
	}
	return result;
}

type_t *make_atomic_type(atomic_type_type_t type, type_qualifier_t qualifiers)
{
	atomic_type_t *atomic_type
		= obstack_alloc(type_obst, sizeof(atomic_type[0]));
	memset(atomic_type, 0, sizeof(atomic_type[0]));
	atomic_type->type.type       = TYPE_ATOMIC;
	atomic_type->type.qualifiers = qualifiers;
	atomic_type->atype           = type;

	return identify_new_type((type_t*) atomic_type);
}

type_t *make_pointer_type(type_t *points_to, type_qualifier_t qualifiers)
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
void dbg_type(type_t *type)
{
	FILE *old_out = out;
	out = stderr;
	print_type(type);
	puts("\n");
	fflush(stderr);
	out = old_out;
}
