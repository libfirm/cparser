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

void print_type_qualifiers(unsigned qualifiers)
{
	if(qualifiers & TYPE_QUALIFIER_CONST)    fputs("const ",    out);
	if(qualifiers & TYPE_QUALIFIER_VOLATILE) fputs("volatile ", out);
	if(qualifiers & TYPE_QUALIFIER_RESTRICT) fputs("restrict ", out);
}

static
void print_atomic_type(const type_t *type)
{
	print_type_qualifiers(type->qualifiers);

	const char *s;
	switch(type->v.atomic_type.atype) {
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

static void print_function_type_pre(const type_t *type)
{
	print_type_qualifiers(type->qualifiers);

	intern_print_type_pre(type->v.function_type.result_type);

	/* TODO: don't emit braces if we're the toplevel type... */
	fputc('(', out);
}

static void print_function_type_post(const type_t *type,
                                     const context_t *context)
{
	/* TODO: don't emit braces if we're the toplevel type... */
	intern_print_type_post(type->v.function_type.result_type);
	fputc(')', out);

	fputc('(', out);

	int                 first     = 1;
	if(context == NULL) {
		function_parameter_t *parameter = type->v.function_type.parameters;
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
	if(type->v.function_type.variadic) {
		if(first) {
			first = 0;
		} else {
			fputs(", ", out);
		}
		fputs("...", out);
	}
	if(first && !type->v.function_type.unspecified_parameters) {
		fputs("void", out);
	}
	fputc(')', out);
}

static void print_pointer_type_pre(const type_t *type)
{
	intern_print_type_pre(type->v.pointer_type.points_to);
	fputs("*", out);
	print_type_qualifiers(type->qualifiers);
}

static void print_pointer_type_post(const type_t *type)
{
	intern_print_type_post(type->v.pointer_type.points_to);
}

static void print_array_type_pre(const type_t *type)
{
	intern_print_type_pre(type->v.array_type.element_type);
}

static void print_array_type_post(const type_t *type)
{
	fputc('[', out);
	if(type->v.array_type.is_static) {
		fputs("static ", out);
	}
	print_type_qualifiers(type->qualifiers);
	if(type->v.array_type.size != NULL) {
		print_expression(type->v.array_type.size);
	}
	fputc(']', out);
	intern_print_type_post(type->v.array_type.element_type);
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

static void print_type_enum(const type_t *type)
{
	print_type_qualifiers(type->qualifiers);
	fputs("enum ", out);

	declaration_t *declaration = type->v.enum_type.declaration;
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

static void print_compound_type(const type_t *type)
{
	print_type_qualifiers(type->qualifiers);

	if(type->type == TYPE_COMPOUND_STRUCT) {
		fputs("struct ", out);
	} else {
		assert(type->type == TYPE_COMPOUND_UNION);
		fputs("union ", out);
	}

	declaration_t *declaration = type->v.compound_type.declaration;
	symbol_t      *symbol      = declaration->symbol;
	if(symbol != NULL) {
		fputs(symbol->string, out);
	} else {
		print_compound_definition(declaration);
	}
}

static void print_typedef_type_pre(type_t *type)
{
	fputs(type->v.typedef_type.declaration->symbol->string, out);
}

static void print_typeof_type_pre(type_t *type)
{
	fputs("typeof(", out);
	if(type->v.typeof_type.expression != NULL) {
		assert(type->v.typeof_type.typeof_type == NULL);
		print_expression(type->v.typeof_type.expression);
	} else {
		print_type(type->v.typeof_type.typeof_type);
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
		print_type_enum(type);
		return;
	case TYPE_ATOMIC:
		print_atomic_type(type);
		return;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		print_compound_type(type);
		return;
	case TYPE_BUILTIN:
		fputs(type->v.builtin_type.symbol->string, out);
		return;
	case TYPE_FUNCTION:
		print_function_type_pre(type);
		return;
	case TYPE_POINTER:
		print_pointer_type_pre(type);
		return;
	case TYPE_ARRAY:
		print_array_type_pre(type);
		return;
	case TYPE_TYPEDEF:
		print_typedef_type_pre(type);
		return;
	case TYPE_TYPEOF:
		print_typeof_type_pre(type);
		return;
	}
	fputs("unknown", out);
}

static void intern_print_type_post(type_t *type)
{
	switch(type->type) {
	case TYPE_FUNCTION:
		print_function_type_post(type, NULL);
		return;
	case TYPE_POINTER:
		print_pointer_type_post(type);
		return;
	case TYPE_ARRAY:
		print_array_type_post(type);
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
		print_function_type_post(type, context);
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

	switch(type->v.atomic_type.atype) {
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

	switch(type->v.atomic_type.atype) {
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

	switch(type->v.atomic_type.atype) {
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
		declaration_t *declaration = type->v.compound_type.declaration;
		return !declaration->init.is_defined;
	}
	case TYPE_FUNCTION:
		return true;

	case TYPE_ARRAY:
		return type->v.array_type.size == NULL;

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
	/* TODO: really incomplete */
	if(type1 == type2)
		return true;

	if(type1->type == TYPE_ATOMIC && type2->type == TYPE_ATOMIC)
		return type1->v.atomic_type.atype == type2->v.atomic_type.atype;

	return false;
}

bool pointers_compatible(const type_t *type1, const type_t *type2)
{
	assert(type1->type == TYPE_POINTER);
	assert(type2->type == TYPE_POINTER);
#if 0
	return types_compatible(type1->v.pointer_type.points_to,
	                        type2->v.pointer_type.points_to);
#endif
	return true;
}

/**
 * duplicates a type
 * note that this does not produce a deep copy!
 */
static type_t *duplicate_type(type_t *type)
{
	type_t *copy = obstack_alloc(type_obst, sizeof(*copy));
	memcpy(copy, type, sizeof(*copy));

	(void) duplicate_type;

	return type;
}

type_t *skip_typeref(type_t *type)
{
	unsigned qualifiers = type->qualifiers;

	while(1) {
		switch(type->type) {
		case TYPE_TYPEDEF:
			qualifiers |= type->qualifiers;
			if(type->v.typedef_type.resolved_type != NULL) {
				type = type->v.typedef_type.resolved_type;
				break;
			}
			type = type->v.typedef_type.declaration->type;
			continue;
		case TYPE_TYPEOF:
			if(type->v.typeof_type.typeof_type != NULL) {
				type = type->v.typeof_type.typeof_type;
			} else {
				type = type->v.typeof_type.expression->datatype;
			}
			continue;
		case TYPE_BUILTIN:
			type = type->v.builtin_type.real_type;
			continue;
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

type_t *make_atomic_type(atomic_type_type_t type, type_qualifiers_t qualifiers)
{
	type_t *atomic_type = obstack_alloc(type_obst, sizeof(atomic_type[0]));
	memset(atomic_type, 0, sizeof(atomic_type[0]));
	atomic_type->type                = TYPE_ATOMIC;
	atomic_type->qualifiers          = qualifiers;
	atomic_type->v.atomic_type.atype = type;

	return identify_new_type(atomic_type);
}

type_t *make_pointer_type(type_t *points_to, type_qualifiers_t qualifiers)
{
	type_t *pointer_type = obstack_alloc(type_obst, sizeof(pointer_type[0]));
	memset(pointer_type, 0, sizeof(pointer_type[0]));
	pointer_type->type                     = TYPE_POINTER;
	pointer_type->qualifiers               = qualifiers;
	pointer_type->v.pointer_type.points_to = points_to;

	return identify_new_type(pointer_type);
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
