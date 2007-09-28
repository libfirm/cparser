#include <config.h>

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "lexer.h"
#include "token_t.h"
#include "type_t.h"
#include "type_hash.h"
#include "ast_t.h"
#include "adt/bitfiddle.h"
#include "adt/error.h"
#include "adt/array.h"

//#define PRINT_TOKENS
//#define ABORT_ON_ERROR
#define MAX_LOOKAHEAD 2
//#define STRICT_C99

struct environment_entry_t {
	symbol_t      *symbol;
	declaration_t *old_declaration;
	const void    *old_context;
};

static token_t               token;
static token_t               lookahead_buffer[MAX_LOOKAHEAD];
static int                   lookahead_bufpos;
static struct obstack        environment_obstack;
static environment_entry_t **environment_stack = NULL;
static context_t            *context           = NULL;
static declaration_t        *last_declaration  = NULL;
static struct obstack        temp_obst;

static type_t               *type_int        = NULL;
static type_t               *type_const_char = NULL;
static type_t               *type_string     = NULL;
static type_t               *type_void       = NULL;
static type_t               *type_size_t     = NULL;

static statement_t *parse_compound_statement(void);
static statement_t *parse_statement(void);

static expression_t *parse_sub_expression(unsigned precedence);
static expression_t *parse_expression(void);
static type_t       *parse_typename(void);

#define STORAGE_CLASSES     \
	case T_typedef:         \
	case T_extern:          \
	case T_static:          \
	case T_auto:            \
	case T_register:

#define TYPE_QUALIFIERS     \
	case T_const:           \
	case T_restrict:        \
	case T_volatile:        \
	case T_inline:

#ifdef PROVIDE_COMPLEX
#define COMPLEX_SPECIFIERS  \
	case T__Complex:
#else
#define COMPLEX_SPECIFIERS
#endif

#ifdef PROVIDE_IMAGINARY
#define IMAGINARY_SPECIFIERS \
	case T__Imaginary:
#else
#define IMAGINARY_SPECIFIERS
#endif

#define TYPE_SPECIFIERS     \
	case T_void:            \
	case T_char:            \
	case T_short:           \
	case T_int:             \
	case T_long:            \
	case T_float:           \
	case T_double:          \
	case T_signed:          \
	case T_unsigned:        \
	case T__Bool:           \
	case T_struct:          \
	case T_union:           \
	case T_enum:            \
	case T___typeof__:      \
	COMPLEX_SPECIFIERS      \
	IMAGINARY_SPECIFIERS

#define DECLARATION_START   \
	STORAGE_CLASSES         \
	TYPE_QUALIFIERS         \
	TYPE_SPECIFIERS

#define TYPENAME_START      \
	TYPE_QUALIFIERS         \
	TYPE_SPECIFIERS

static inline void *allocate_ast_zero(size_t size)
{
	void *res = allocate_ast(size);
	memset(res, 0, size);
	return res;
}

static inline void *allocate_type_zero(size_t size)
{
	void *res = obstack_alloc(type_obst, size);
	memset(res, 0, size);
	return res;
}

/**
 * returns the top element of the environment stack
 */
static inline size_t environment_top(void)
{
	return ARR_LEN(environment_stack);
}



static inline void next_token(void)
{
	token                              = lookahead_buffer[lookahead_bufpos];
	lookahead_buffer[lookahead_bufpos] = lexer_token;
	lexer_next_token();

	lookahead_bufpos = (lookahead_bufpos+1) % MAX_LOOKAHEAD;

#ifdef PRINT_TOKENS
	print_token(stderr, &token);
	fprintf(stderr, "\n");
#endif
}

static inline const token_t *look_ahead(int num)
{
	assert(num > 0 && num <= MAX_LOOKAHEAD);
	int pos = (lookahead_bufpos+num-1) % MAX_LOOKAHEAD;
	return & lookahead_buffer[pos];
}

static inline void eat(token_type_t type)
{
	assert(token.type == type);
	next_token();
}

void error(void)
{
#ifdef ABORT_ON_ERROR
	abort();
#endif
}

void parser_print_prefix_pos(const source_position_t source_position)
{
    fputs(source_position.input_name, stderr);
    fputc(':', stderr);
    fprintf(stderr, "%d", source_position.linenr);
    fputs(": ", stderr);
}

void parser_print_error_prefix_pos(const source_position_t source_position)
{
	parser_print_prefix_pos(source_position);
	fputs("error: ", stderr);
	error();
}

void parser_print_error_prefix(void)
{
	parser_print_prefix_pos(token.source_position);
	error();
}

static void parse_error(const char *message)
{
	parser_print_error_prefix();
	fprintf(stderr, "parse error: %s\n", message);
}

__attribute__((unused))
static void parse_warning(const char *message)
{
	parser_print_prefix_pos(token.source_position);
	fprintf(stderr, "warning: %s\n", message);
}

static void parse_error_expected(const char *message, ...)
{
	va_list args;
	int first = 1;

	if(message != NULL) {
		parser_print_error_prefix();
		fprintf(stderr, "%s\n", message);
	}
	parser_print_error_prefix();
	fputs("Parse error: got ", stderr);
	print_token(stderr, &token);
	fputs(", expected ", stderr);

	va_start(args, message);
	token_type_t token_type = va_arg(args, token_type_t);
	while(token_type != 0) {
		if(first == 1) {
			first = 0;
		} else {
			fprintf(stderr, ", ");
		}
		print_token_type(stderr, token_type);
		token_type = va_arg(args, token_type_t);
	}
	va_end(args);
	fprintf(stderr, "\n");
}

static void eat_block(void)
{
	if(token.type == '{')
		next_token();

	while(token.type != '}') {
		if(token.type == T_EOF)
			return;
		if(token.type == '{') {
			eat_block();
			continue;
		}
		next_token();
	}
	eat('}');
}

static void eat_statement(void)
{
	while(token.type != ';') {
		if(token.type == T_EOF)
			return;
		if(token.type == '}')
			return;
		if(token.type == '{') {
			eat_block();
			continue;
		}
		next_token();
	}
	eat(';');
}

static void eat_brace(void)
{
	if(token.type == '(')
		next_token();

	while(token.type != ')') {
		if(token.type == T_EOF)
			return;
		if(token.type == ')' || token.type == ';' || token.type == '}') {
			return;
		}
		if(token.type == '(') {
			eat_brace();
			continue;
		}
		if(token.type == '{') {
			eat_block();
			continue;
		}
		next_token();
	}
	eat(')');
}

#define expect(expected)                           \
    if(UNLIKELY(token.type != (expected))) {       \
        parse_error_expected(NULL, (expected), 0); \
        eat_statement();                           \
        return NULL;                               \
    }                                              \
    next_token();

#define expect_void(expected)                      \
    if(UNLIKELY(token.type != (expected))) {       \
        parse_error_expected(NULL, (expected), 0); \
        eat_statement();                           \
        return;                                    \
    }                                              \
    next_token();

static void set_context(context_t *new_context)
{
	context = new_context;

	declaration_t *declaration = new_context->declarations;
	if(declaration != NULL) {
		while(true) {
			if(declaration->next == NULL)
				break;
			declaration = declaration->next;
		}
	}

	last_declaration = declaration;
}

/**
 * called when we find a 2nd declarator for an identifier we already have a
 * declarator for
 */
static bool is_compatible_declaration (declaration_t *declaration,
                                      declaration_t *previous)
{
	/* TODO: not correct yet */
	return declaration->type == previous->type;
}

/**
 * pushs an environment_entry on the environment stack and links the
 * corresponding symbol to the new entry
 */
static inline declaration_t *environment_push(declaration_t *declaration,
                                              const void *context)
{
	symbol_t *symbol = declaration->symbol;
	assert(declaration != symbol->declaration);
	assert(declaration->source_position.input_name != NULL);

	if(symbol->context == context) {
		declaration_t *previous_declaration = symbol->declaration;
		if(symbol->declaration != NULL) {
			if(!is_compatible_declaration(declaration, previous_declaration)) {
				parser_print_error_prefix_pos(declaration->source_position);
				fprintf(stderr, "definition of symbol '%s' with type ",
				        declaration->symbol->string);
				error();
				print_type(declaration->type);
				fputc('\n', stderr);
				parser_print_error_prefix_pos(
						previous_declaration->source_position);
				fprintf(stderr, "is incompatible with previous declaration "
				        "of type ");
				print_type(previous_declaration->type);
				fputc('\n', stderr);
			}
			return previous_declaration;
		}
	}

	environment_entry_t *entry
		= obstack_alloc(&environment_obstack, sizeof(entry[0]));
	memset(entry, 0, sizeof(entry[0]));

	int top = ARR_LEN(environment_stack);
	ARR_RESIZE(environment_stack, top + 1);
	environment_stack[top] = entry;

	entry->old_declaration = symbol->declaration;
	entry->old_context     = symbol->context;
	entry->symbol          = symbol;
	symbol->declaration    = declaration;
	symbol->context        = context;

	return declaration;
}

/**
 * pops symbols from the environment stack until @p new_top is the top element
 */
static inline void environment_pop_to(size_t new_top)
{
	environment_entry_t *entry = NULL;
	size_t top = ARR_LEN(environment_stack);
	size_t i;

	if(new_top == top)
		return;

	assert(new_top < top);
	i = top;
	do {
		entry = environment_stack[i - 1];

		symbol_t *symbol = entry->symbol;

		symbol->declaration = entry->old_declaration;
		symbol->context     = entry->old_context;

		--i;
	} while(i != new_top);
	obstack_free(&environment_obstack, entry);

	ARR_SHRINKLEN(environment_stack, (int) new_top);
}



static expression_t *parse_constant_expression(void)
{
	/* start parsing at precedence 7 (conditional expression) */
	return parse_sub_expression(7);
}

static expression_t *parse_assignment_expression(void)
{
	/* start parsing at precedence 2 (assignment expression) */
	return parse_sub_expression(2);
}

static void parse_compound_type_entries(void);
static void parse_declarator(declaration_t *declaration,
                             storage_class_t storage_class, type_t *type,
                             int may_be_abstract);
static declaration_t *record_declaration(declaration_t *declaration);

typedef struct declaration_specifiers_t  declaration_specifiers_t;
struct declaration_specifiers_t {
	storage_class_t  storage_class;
	type_t          *type;
};

static compound_type_t *find_compound_type(compound_type_t *types,
                                           const symbol_t *symbol)
{
	compound_type_t *type = types;
	for( ; type != NULL; type = type->next) {
		if(type->symbol == symbol)
			return type;
	}

	return NULL;
}

static type_t *parse_compound_type_specifier(bool is_struct)
{
	if(is_struct) {
		eat(T_struct);
	} else {
		eat(T_union);
	}

	symbol_t        *symbol        = NULL;
	compound_type_t *compound_type = NULL;

	if(token.type == T_IDENTIFIER) {
		symbol = token.v.symbol;
		next_token();

		if(context != NULL) {
			if(is_struct) {
				compound_type = find_compound_type(context->structs, symbol);
			} else {
				compound_type = find_compound_type(context->unions, symbol);
			}
		}
	} else if(token.type != '{') {
		if(is_struct) {
			parse_error_expected("problem while parsing struct type specifier",
			                     T_IDENTIFIER, '{', 0);
		} else {
			parse_error_expected("problem while parsing union type specifier",
			                     T_IDENTIFIER, '{', 0);
		}

		return NULL;
	}

	if(compound_type == NULL) {
		compound_type = allocate_type_zero(sizeof(compound_type[0]));

		if(is_struct) {
			compound_type->type.type = TYPE_COMPOUND_STRUCT;
		} else {
			compound_type->type.type = TYPE_COMPOUND_UNION;
		}
		compound_type->source_position = token.source_position;
		compound_type->symbol          = symbol;
	}

	if(token.type == '{') {
		if(compound_type->defined) {
			parser_print_error_prefix();
			fprintf(stderr, "multiple definition of %s %s\n",
					is_struct ? "struct" : "union", symbol->string);
			compound_type->context.declarations = NULL;
		}
		compound_type->defined = 1;

		int         top          = environment_top();
		context_t  *last_context = context;
		set_context(&compound_type->context);

		parse_compound_type_entries();

		assert(context == &compound_type->context);
		set_context(last_context);
		environment_pop_to(top);
	}

	return (type_t*) compound_type;
}

static void parse_enum_entries(void)
{
	eat('{');

	if(token.type == '}') {
		next_token();
		parse_error("empty enum not allowed");
		return;
	}

	do {
		declaration_t *entry = allocate_ast_zero(sizeof(entry[0]));

		if(token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing enum entry",
			                     T_IDENTIFIER, 0);
			eat_block();
			return;
		}
		entry->storage_class   = STORAGE_CLASS_ENUM_ENTRY;
		entry->symbol          = token.v.symbol;
		entry->source_position = token.source_position;
		next_token();

		if(token.type == '=') {
			next_token();
			entry->initializer = parse_constant_expression();
		}

		record_declaration(entry);

		if(token.type != ',')
			break;
		next_token();
	} while(token.type != '}');

	expect_void('}');
}

static enum_type_t *find_enum_type(enum_type_t *types, const symbol_t *symbol)
{
	enum_type_t *type = types;
	for( ; type != NULL; type = type->next) {
		if(type->symbol == symbol)
			return type;
	}

	return NULL;
}

static type_t *parse_enum_specifier(void)
{
	eat(T_enum);

	symbol_t    *symbol    = NULL;
	enum_type_t *enum_type = NULL;

	if(token.type == T_IDENTIFIER) {
		symbol = token.v.symbol;
		next_token();

		if(context != NULL) {
			enum_type = find_enum_type(context->enums, symbol);
		}
	} else if(token.type != '{') {
		parse_error_expected("problem while parsing enum type specifier",
		                     T_IDENTIFIER, '{', 0);
		return NULL;
	}

	if(enum_type == NULL) {
		enum_type                  = allocate_type_zero(sizeof(enum_type[0]));
		enum_type->type.type       = TYPE_ENUM;
		enum_type->source_position = token.source_position;
		enum_type->symbol          = symbol;
	}

	if(token.type == '{') {
		if(enum_type->defined) {
			parser_print_error_prefix();
			fprintf(stderr, "multiple definitions of enum %s\n",
			        symbol->string);
			enum_type->entries_begin = NULL;
			enum_type->entries_end   = NULL;
		}
		enum_type->defined = 1;

		declaration_t *before = last_declaration;

		parse_enum_entries();

		if(before == NULL) {
			enum_type->entries_begin = context->declarations;
		} else {
			enum_type->entries_begin = before->next;
		}
		enum_type->entries_end = last_declaration;
	}

	return (type_t*) enum_type;
}

static type_t *parse_typeof(void)
{
	eat(T___typeof__);

	type_t *result;

	expect('(');

	declaration_t *declaration;
	expression_t  *expression;

restart:
	switch(token.type) {
	case T___extension__:
		/* this can be a prefix to a typename or an expression */
		/* we simply eat it now. */
		do {
			next_token();
		} while(token.type == T___extension__);
		goto restart;

	case T_IDENTIFIER:
		declaration = token.v.symbol->declaration;
		if(declaration != NULL
				&& declaration->storage_class == STORAGE_CLASS_TYPEDEF) {
			result = parse_typename();
			break;
		}
		expression = parse_expression();
		result     = expression->datatype;
		break;

	TYPENAME_START
		result = parse_typename();
		break;

	default:
		expression = parse_expression();
		result     = expression->datatype;
		break;
	}

	expect(')');

	return result;
}

static const char *parse_string_literals(void)
{
	assert(token.type == T_STRING_LITERAL);
	const char *result = token.v.string;

	next_token();

	while(token.type == T_STRING_LITERAL) {
		result = concat_strings(result, token.v.string);
		next_token();
	}

	return result;
}

static void parse_attributes(void)
{
	while(true) {
		switch(token.type) {
		case T___attribute__:
			next_token();

			expect_void('(');
			int depth = 1;
			while(depth > 0) {
				switch(token.type) {
				case T_EOF:
					parse_error("EOF while parsing attribute");
					break;
				case '(':
					next_token();
					depth++;
					break;
				case ')':
					next_token();
					depth--;
					break;
				default:
					next_token();
				}
			}
			break;
		case T_asm:
			next_token();
			expect_void('(');
			if(token.type != T_STRING_LITERAL) {
				parse_error_expected("while parsing assembler attribute",
				                     T_STRING_LITERAL);
				eat_brace();
				break;
			} else {
				parse_string_literals();
			}
			expect_void(')');
			break;
		default:
			goto attributes_finished;
		}
	}

attributes_finished:
	;
}

typedef enum {
	SPECIFIER_SIGNED    = 1 << 0,
	SPECIFIER_UNSIGNED  = 1 << 1,
	SPECIFIER_LONG      = 1 << 2,
	SPECIFIER_INT       = 1 << 3,
	SPECIFIER_DOUBLE    = 1 << 4,
	SPECIFIER_CHAR      = 1 << 5,
	SPECIFIER_SHORT     = 1 << 6,
	SPECIFIER_LONG_LONG = 1 << 7,
	SPECIFIER_FLOAT     = 1 << 8,
	SPECIFIER_BOOL      = 1 << 9,
	SPECIFIER_VOID      = 1 << 10,
#ifdef PROVIDE_COMPLEX
	SPECIFIER_COMPLEX   = 1 << 11,
#endif
#ifdef PROVIDE_IMAGINARY
	SPECIFIER_IMAGINARY = 1 << 12,
#endif
} specifiers_t;

static type_t *create_builtin_type(symbol_t *symbol)
{
	builtin_type_t *type = allocate_type_zero(sizeof(type[0]));
	type->type.type      = TYPE_BUILTIN;
	type->symbol         = symbol;

	type_t *result = typehash_insert((type_t*) type);
	if(result != (type_t*) type) {
		obstack_free(type_obst, type);
	}

	return result;
}

static void parse_declaration_specifiers(declaration_specifiers_t *specifiers)
{
	declaration_t *declaration;
	type_t        *type            = NULL;
	unsigned       type_qualifiers = 0;
	unsigned       type_specifiers = 0;
	int            newtype         = 0;

	while(true) {
		switch(token.type) {

		/* storage class */
#define MATCH_STORAGE_CLASS(token, class)                                \
		case token:                                                      \
			if(specifiers->storage_class != STORAGE_CLASS_NONE) {        \
				parse_error("multiple storage classes in declaration "   \
				            "specifiers");                               \
			}                                                            \
			specifiers->storage_class = class;                           \
			next_token();                                                \
			break;

		MATCH_STORAGE_CLASS(T_typedef,  STORAGE_CLASS_TYPEDEF)
		MATCH_STORAGE_CLASS(T_extern,   STORAGE_CLASS_EXTERN)
		MATCH_STORAGE_CLASS(T_static,   STORAGE_CLASS_STATIC)
		MATCH_STORAGE_CLASS(T_auto,     STORAGE_CLASS_AUTO)
		MATCH_STORAGE_CLASS(T_register, STORAGE_CLASS_REGISTER)

		/* type qualifiers */
#define MATCH_TYPE_QUALIFIER(token, qualifier)                          \
		case token:                                                     \
			type_qualifiers |= qualifier;                               \
			next_token();                                               \
			break;

		MATCH_TYPE_QUALIFIER(T_const,    TYPE_QUALIFIER_CONST);
		MATCH_TYPE_QUALIFIER(T_restrict, TYPE_QUALIFIER_RESTRICT);
		MATCH_TYPE_QUALIFIER(T_volatile, TYPE_QUALIFIER_VOLATILE);
		MATCH_TYPE_QUALIFIER(T_inline,   TYPE_QUALIFIER_INLINE);

		case T___extension__:
			/* TODO */
			next_token();
			break;

		/* type specifiers */
#define MATCH_SPECIFIER(token, specifier, name)                         \
		case token:                                                     \
			next_token();                                               \
			if(type_specifiers & specifier) {                           \
				parse_error("multiple " name " type specifiers given"); \
			} else {                                                    \
				type_specifiers |= specifier;                           \
			}                                                           \
			break;

		MATCH_SPECIFIER(T_void,       SPECIFIER_VOID,      "void")
		MATCH_SPECIFIER(T_char,       SPECIFIER_CHAR,      "char")
		MATCH_SPECIFIER(T_short,      SPECIFIER_SHORT,     "short")
		MATCH_SPECIFIER(T_int,        SPECIFIER_INT,       "int")
		MATCH_SPECIFIER(T_float,      SPECIFIER_FLOAT,     "float")
		MATCH_SPECIFIER(T_double,     SPECIFIER_DOUBLE,    "double")
		MATCH_SPECIFIER(T_signed,     SPECIFIER_SIGNED,    "signed")
		MATCH_SPECIFIER(T_unsigned,   SPECIFIER_UNSIGNED,  "unsigned")
		MATCH_SPECIFIER(T__Bool,      SPECIFIER_BOOL,      "_Bool")
#ifdef PROVIDE_COMPLEX
		MATCH_SPECIFIER(T__Complex,   SPECIFIER_COMPLEX,   "_Complex")
#endif
#ifdef PROVIDE_IMAGINARY
		MATCH_SPECIFIER(T__Imaginary, SPECIFIER_IMAGINARY, "_Imaginary")
#endif
		case T_long:
			next_token();
			if(type_specifiers & SPECIFIER_LONG_LONG) {
				parse_error("multiple type specifiers given");
			} else if(type_specifiers & SPECIFIER_LONG) {
				type_specifiers |= SPECIFIER_LONG_LONG;
			} else {
				type_specifiers |= SPECIFIER_LONG;
			}
			break;

		/* TODO: if type != NULL for the following rules issue an error */
		case T_struct:
			type = parse_compound_type_specifier(true);
			break;
		case T_union:
			type = parse_compound_type_specifier(false);
			break;
		case T_enum:
			type = parse_enum_specifier();
			break;
		case T___typeof__:
			type = parse_typeof();
			break;
		case T___builtin_va_list:
			type = create_builtin_type(token.v.symbol);
			next_token();
			break;

		case T___attribute__:
			/* TODO */
			parse_attributes();
			break;

		case T_IDENTIFIER:
			declaration = token.v.symbol->declaration;
			if(declaration == NULL ||
					declaration->storage_class != STORAGE_CLASS_TYPEDEF) {
				goto finish_specifiers;
			}

			type = declaration->type;
			assert(type != NULL);
			next_token();
			break;

		/* function specifier */
		default:
			goto finish_specifiers;
		}
	}

finish_specifiers:

	if(type == NULL) {
		atomic_type_type_t atomic_type;

		/* match valid basic types */
		switch(type_specifiers) {
		case SPECIFIER_VOID:
			atomic_type = ATOMIC_TYPE_VOID;
			break;
		case SPECIFIER_CHAR:
			atomic_type = ATOMIC_TYPE_CHAR;
			break;
		case SPECIFIER_SIGNED | SPECIFIER_CHAR:
			atomic_type = ATOMIC_TYPE_SCHAR;
			break;
		case SPECIFIER_UNSIGNED | SPECIFIER_CHAR:
			atomic_type = ATOMIC_TYPE_UCHAR;
			break;
		case SPECIFIER_SHORT:
		case SPECIFIER_SIGNED | SPECIFIER_SHORT:
		case SPECIFIER_SHORT | SPECIFIER_INT:
		case SPECIFIER_SIGNED | SPECIFIER_SHORT | SPECIFIER_INT:
			atomic_type = ATOMIC_TYPE_SHORT;
			break;
		case SPECIFIER_UNSIGNED | SPECIFIER_SHORT:
		case SPECIFIER_UNSIGNED | SPECIFIER_SHORT | SPECIFIER_INT:
			atomic_type = ATOMIC_TYPE_USHORT;
			break;
		case SPECIFIER_INT:
		case SPECIFIER_SIGNED:
		case SPECIFIER_SIGNED | SPECIFIER_INT:
			atomic_type = ATOMIC_TYPE_INT;
			break;
		case SPECIFIER_UNSIGNED:
		case SPECIFIER_UNSIGNED | SPECIFIER_INT:
			atomic_type = ATOMIC_TYPE_UINT;
			break;
		case SPECIFIER_LONG:
		case SPECIFIER_SIGNED | SPECIFIER_LONG:
		case SPECIFIER_LONG | SPECIFIER_INT:
		case SPECIFIER_SIGNED | SPECIFIER_LONG | SPECIFIER_INT:
			atomic_type = ATOMIC_TYPE_LONG;
			break;
		case SPECIFIER_UNSIGNED | SPECIFIER_LONG:
		case SPECIFIER_UNSIGNED | SPECIFIER_LONG | SPECIFIER_INT:
			atomic_type = ATOMIC_TYPE_ULONG;
			break;
		case SPECIFIER_LONG | SPECIFIER_LONG_LONG:
		case SPECIFIER_SIGNED | SPECIFIER_LONG | SPECIFIER_LONG_LONG:
		case SPECIFIER_LONG | SPECIFIER_LONG_LONG | SPECIFIER_INT:
		case SPECIFIER_SIGNED | SPECIFIER_LONG | SPECIFIER_LONG_LONG
			| SPECIFIER_INT:
			atomic_type = ATOMIC_TYPE_LONGLONG;
			break;
		case SPECIFIER_UNSIGNED | SPECIFIER_LONG | SPECIFIER_LONG_LONG:
		case SPECIFIER_UNSIGNED | SPECIFIER_LONG | SPECIFIER_LONG_LONG
			| SPECIFIER_INT:
			atomic_type = ATOMIC_TYPE_ULONGLONG;
			break;
		case SPECIFIER_FLOAT:
			atomic_type = ATOMIC_TYPE_FLOAT;
			break;
		case SPECIFIER_DOUBLE:
			atomic_type = ATOMIC_TYPE_DOUBLE;
			break;
		case SPECIFIER_LONG | SPECIFIER_DOUBLE:
			atomic_type = ATOMIC_TYPE_LONG_DOUBLE;
			break;
		case SPECIFIER_BOOL:
			atomic_type = ATOMIC_TYPE_BOOL;
			break;
#ifdef PROVIDE_COMPLEX
		case SPECIFIER_FLOAT | SPECIFIER_COMPLEX:
			atomic_type = ATOMIC_TYPE_FLOAT_COMPLEX;
			break;
		case SPECIFIER_DOUBLE | SPECIFIER_COMPLEX:
			atomic_type = ATOMIC_TYPE_DOUBLE_COMPLEX;
			break;
		case SPECIFIER_LONG | SPECIFIER_DOUBLE | SPECIFIER_COMPLEX:
			atomic_type = ATOMIC_TYPE_LONG_DOUBLE_COMPLEX;
			break;
#endif
#ifdef PROVIDE_IMAGINARY
		case SPECIFIER_FLOAT | SPECIFIER_IMAGINARY:
			atomic_type = ATOMIC_TYPE_FLOAT_IMAGINARY;
			break;
		case SPECIFIER_DOUBLE | SPECIFIER_IMAGINARY:
			atomic_type = ATOMIC_TYPE_DOUBLE_IMAGINARY;
			break;
		case SPECIFIER_LONG | SPECIFIER_DOUBLE | SPECIFIER_IMAGINARY:
			atomic_type = ATOMIC_TYPE_LONG_DOUBLE_IMAGINARY;
			break;
#endif
		default:
			/* invalid specifier combination, give an error message */
			if(type_specifiers == 0) {
#ifndef STRICT_C99
				parse_warning("no type specifiers in declaration (using int)");
				atomic_type = ATOMIC_TYPE_INT;
				break;
#else
				parse_error("no type specifiers given in declaration");
#endif
			} else if((type_specifiers & SPECIFIER_SIGNED) &&
			          (type_specifiers & SPECIFIER_UNSIGNED)) {
				parse_error("signed and unsigned specifiers gives");
			} else if(type_specifiers & (SPECIFIER_SIGNED | SPECIFIER_UNSIGNED)) {
				parse_error("only integer types can be signed or unsigned");
			} else {
				parse_error("multiple datatypes in declaration");
			}
			atomic_type = ATOMIC_TYPE_INVALID;
		}

		atomic_type_t *atype = allocate_type_zero(sizeof(atype[0]));
		atype->type.type     = TYPE_ATOMIC;
		atype->atype         = atomic_type;
		newtype              = 1;

		type = (type_t*) atype;
	} else {
		if(type_specifiers != 0) {
			parse_error("multiple datatypes in declaration");
		}
	}

	type->qualifiers = type_qualifiers;

	type_t *result = typehash_insert(type);
	if(newtype && result != (type_t*) type) {
		obstack_free(type_obst, type);
	}

	specifiers->type = result;
}

static type_qualifier_t parse_type_qualifiers(void)
{
	type_qualifier_t type_qualifiers = 0;

	while(true) {
		switch(token.type) {
		/* type qualifiers */
		MATCH_TYPE_QUALIFIER(T_const,    TYPE_QUALIFIER_CONST);
		MATCH_TYPE_QUALIFIER(T_restrict, TYPE_QUALIFIER_RESTRICT);
		MATCH_TYPE_QUALIFIER(T_volatile, TYPE_QUALIFIER_VOLATILE);
		MATCH_TYPE_QUALIFIER(T_inline,   TYPE_QUALIFIER_INLINE);

		default:
			return type_qualifiers;
		}
	}
}

static void parse_identifier_list(void)
{
	while(true) {
		if(token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing parameter identifier "
			                     "list", T_IDENTIFIER, 0);
			return;
		}
		next_token();
		if(token.type != ',')
			break;
		next_token();
	}
}

static declaration_t *parse_parameter(void)
{
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));

	parse_declaration_specifiers(&specifiers);

	declaration_t *declaration = allocate_ast_zero(sizeof(declaration[0]));
	parse_declarator(declaration, specifiers.storage_class,
	                 specifiers.type, 1);

	return declaration;
}

static declaration_t *parse_parameters(method_type_t *type)
{
	if(token.type == T_IDENTIFIER) {
		symbol_t      *symbol      = token.v.symbol;
		declaration_t *declaration = symbol->declaration;
		if(declaration == NULL
				|| declaration->storage_class != STORAGE_CLASS_TYPEDEF) {
			/* TODO */
			parse_identifier_list();
			return NULL;
		}
	}

	if(token.type == ')') {
		type->unspecified_parameters = 1;
		return NULL;
	}
	if(token.type == T_void && look_ahead(1)->type == ')') {
		next_token();
		return NULL;
	}

	declaration_t      *declarations = NULL;
	declaration_t      *declaration;
	declaration_t      *last_declaration = NULL;
	method_parameter_t *parameter;
	method_parameter_t *last_parameter = NULL;

	while(true) {
		switch(token.type) {
		case T_DOTDOTDOT:
			next_token();
			type->variadic = 1;
			return declarations;

		case T_IDENTIFIER:
		case T___extension__:
		DECLARATION_START
			declaration = parse_parameter();

			parameter       = allocate_type_zero(sizeof(parameter[0]));
			parameter->type = declaration->type;

			if(last_parameter != NULL) {
				last_declaration->next = declaration;
				last_parameter->next   = parameter;
			} else {
				type->parameters = parameter;
				declarations     = declaration;
			}
			last_parameter   = parameter;
			last_declaration = declaration;
			break;

		default:
			return declarations;
		}
		if(token.type != ',')
			return declarations;
		next_token();
	}
}

typedef enum {
	CONSTRUCT_POINTER,
	CONSTRUCT_METHOD,
	CONSTRUCT_ARRAY
} construct_type_type_t;

typedef struct construct_type_t construct_type_t;
struct construct_type_t {
	construct_type_type_t  type;
	construct_type_t      *next;
};

typedef struct parsed_pointer_t parsed_pointer_t;
struct parsed_pointer_t {
	construct_type_t  construct_type;
	type_qualifier_t  type_qualifiers;
};

typedef struct construct_method_type_t construct_method_type_t;
struct construct_method_type_t {
	construct_type_t  construct_type;
	method_type_t    *method_type;
};

typedef struct parsed_array_t parsed_array_t;
struct parsed_array_t {
	construct_type_t  construct_type;
	type_qualifier_t  type_qualifiers;
	bool              is_static;
	bool              is_variable;
	expression_t     *size;
};

typedef struct construct_base_type_t construct_base_type_t;
struct construct_base_type_t {
	construct_type_t  construct_type;
	type_t           *type;
};

static construct_type_t *parse_pointer_declarator(void)
{
	eat('*');

	parsed_pointer_t *pointer = obstack_alloc(&temp_obst, sizeof(pointer[0]));
	memset(pointer, 0, sizeof(pointer[0]));
	pointer->type_qualifiers = parse_type_qualifiers();

	return (construct_type_t*) pointer;
}

static construct_type_t *parse_array_declarator(void)
{
	eat('[');

	parsed_array_t *array = obstack_alloc(&temp_obst, sizeof(array[0]));
	memset(array, 0, sizeof(array[0]));

	if(token.type == T_static) {
		array->is_static = true;
		next_token();
	}

	type_qualifier_t type_qualifiers = parse_type_qualifiers();
	if(type_qualifiers != 0) {
		if(token.type == T_static) {
			array->is_static = true;
			next_token();
		}
	}
	array->type_qualifiers = type_qualifiers;

	if(token.type == '*' && look_ahead(1)->type == ']') {
		array->is_variable = true;
		next_token();
	} else if(token.type != ']') {
		array->size = parse_assignment_expression();
	}

	expect(']');

	return (construct_type_t*) array;
}

static construct_type_t *parse_method_declarator(declaration_t *declaration)
{
	eat('(');

	method_type_t *method_type
		= allocate_type_zero(sizeof(method_type[0]));
	method_type->type.type   = TYPE_METHOD;

	declaration_t *parameters = parse_parameters(method_type);
	if(declaration != NULL) {
		declaration->context.declarations = parameters;
	}

	construct_method_type_t *construct_method_type =
		obstack_alloc(&temp_obst, sizeof(construct_method_type[0]));
	memset(construct_method_type, 0, sizeof(construct_method_type[0]));
	construct_method_type->construct_type.type = CONSTRUCT_METHOD;
	construct_method_type->method_type         = method_type;

	expect(')');

	return (construct_type_t*) construct_method_type;
}

static construct_type_t *parse_inner_declarator(declaration_t *declaration,
		int may_be_abstract)
{
	construct_type_t *result = NULL;
	construct_type_t *last   = NULL;

	while(token.type == '*') {
		construct_type_t *type = parse_pointer_declarator();
		if(last != NULL) {
			last->next = type;
		} else {
			result = type;
		}
		last = type;
	}

	/* TODO: find out if this is correct */
	parse_attributes();

	construct_type_t *inner_types = NULL;

	switch(token.type) {
	case T_IDENTIFIER:
		if(declaration == NULL) {
			parse_error("no identifier expected in typename");
		} else {
			declaration->symbol          = token.v.symbol;
			declaration->source_position = token.source_position;
		}
		next_token();
		break;
	case '(':
		next_token();
		inner_types = parse_inner_declarator(declaration, may_be_abstract);
		expect(')');
		break;
	default:
		if(may_be_abstract)
			break;
		parse_error_expected("problem while parsing declarator", T_IDENTIFIER,
		                     '(', 0);
	}

	while(true) {
		construct_type_t *type;
		switch(token.type) {
		case '(':
			type = parse_method_declarator(declaration);
			break;
		case '[':
			type = parse_array_declarator();
			break;
		default:
			goto declarator_finished;
		}

		if(last != NULL) {
			last->next = type;
		} else {
			result = type;
		}
		last = type;
	}

declarator_finished:
	parse_attributes();

	if(inner_types != NULL) {
		if(last != NULL) {
			last->next = inner_types;
		} else {
			result = inner_types;
		}
		last = inner_types;
	}

	return result;
}

static type_t *construct_declarator_type(construct_type_t *construct_list,
                                         type_t *type)
{
	construct_type_t *iter = construct_list;
	for( ; iter != NULL; iter = iter->next) {
		parsed_pointer_t        *parsed_pointer;
		parsed_array_t          *parsed_array;
		construct_method_type_t *construct_method_type;
		method_type_t           *method_type;
		pointer_type_t          *pointer_type;
		array_type_t            *array_type;

		switch(iter->type) {
		case CONSTRUCT_METHOD:
			construct_method_type = (construct_method_type_t*) iter;
			method_type           = construct_method_type->method_type;

			method_type->result_type = type;
			type                     = (type_t*) method_type;
			break;

		case CONSTRUCT_POINTER:
			parsed_pointer = (parsed_pointer_t*) iter;
			pointer_type   = allocate_type_zero(sizeof(pointer_type[0]));

			pointer_type->type.type       = TYPE_POINTER;
			pointer_type->points_to       = type;
			pointer_type->type.qualifiers = parsed_pointer->type_qualifiers;
			type                          = (type_t*) pointer_type;
			break;

		case CONSTRUCT_ARRAY:
			parsed_array  = (parsed_array_t*) iter;
			array_type    = allocate_type_zero(sizeof(array_type[0]));

			array_type->type.type       = TYPE_ARRAY;
			array_type->element_type    = type;
			array_type->type.qualifiers = parsed_array->type_qualifiers;
			array_type->is_static       = parsed_array->is_static;
			array_type->is_variable     = parsed_array->is_variable;
			array_type->size            = parsed_array->size;
			type                        = (type_t*) array_type;
			break;
		}

		type_t *hashed_type = typehash_insert((type_t*) type);
		if(hashed_type != type) {
			obstack_free(type_obst, type);
			type = hashed_type;
		}
	}

	return type;
}

static void parse_declarator(declaration_t *declaration,
                             storage_class_t storage_class, type_t *type,
                             int may_be_abstract)
{
	construct_type_t *construct_type
		= parse_inner_declarator(declaration, may_be_abstract);

	declaration->type         = construct_declarator_type(construct_type, type);
	declaration->storage_class = storage_class;
	if(construct_type != NULL) {
		obstack_free(&temp_obst, construct_type);
	}
}

static type_t *parse_abstract_declarator(type_t *base_type)
{
	construct_type_t *construct_type
		= parse_inner_declarator(NULL, 1);

	if(construct_type == NULL)
		return NULL;

	type_t *result = construct_declarator_type(construct_type, base_type);
	obstack_free(&temp_obst, construct_type);

	return result;
}

static declaration_t *record_declaration(declaration_t *declaration)
{
	if(context == NULL)
		return declaration;

	symbol_t *symbol = declaration->symbol;
	if(symbol != NULL) {
		declaration_t *alias = environment_push(declaration, context);
		if(alias != declaration)
			return alias;
	}

	if(last_declaration != NULL) {
		last_declaration->next = declaration;
	} else {
		context->declarations  = declaration;
	}
	last_declaration = declaration;

	return declaration;
}

static void parser_error_multiple_definition(declaration_t *previous,
                                             declaration_t *declaration)
{
	parser_print_error_prefix_pos(declaration->source_position);
	fprintf(stderr, "multiple definition of symbol '%s'\n",
	        declaration->symbol->string);
	parser_print_error_prefix_pos(previous->source_position);
	fprintf(stderr, "this is the location of the previous "
	        "definition.\n");
	error();
}

static void parse_init_declarators(const declaration_specifiers_t *specifiers)
{
	while(true) {
		declaration_t *ndeclaration
			= allocate_ast_zero(sizeof(ndeclaration[0]));

		parse_declarator(ndeclaration, specifiers->storage_class,
		                 specifiers->type, 0);
		declaration_t *declaration = record_declaration(ndeclaration);
		if(token.type == '=') {
			next_token();

			/* TODO: check that this is an allowed type (esp. no method type) */

			if(declaration->initializer != NULL) {
				parser_error_multiple_definition(declaration, ndeclaration);
			}

			if(token.type == '{') {
				// TODO
				expect_void('}');
			} else {
				declaration->initializer = parse_assignment_expression();
			}
		} else if(token.type == '{') {
			if(declaration->type->type != TYPE_METHOD) {
				parser_print_error_prefix();
				fprintf(stderr, "Declarator ");
				print_type_ext(declaration->type, declaration->symbol, NULL);
				fprintf(stderr, " is not a method type.\n");
			}

			if(declaration->initializer != NULL) {
				parser_error_multiple_definition(declaration, ndeclaration);
			}
			if(ndeclaration != declaration) {
				memcpy(&declaration->context, &ndeclaration->context,
				       sizeof(declaration->context));
			}

			int         top          = environment_top();
			context_t  *last_context = context;
			set_context(&declaration->context);

			/* push function parameters */
			declaration_t *parameter = declaration->context.declarations;
			for( ; parameter != NULL; parameter = parameter->next) {
				environment_push(parameter, context);
			}

			statement_t *statement = parse_compound_statement();

			assert(context == &declaration->context);
			set_context(last_context);
			environment_pop_to(top);

			declaration->statement = statement;
			return;
		}

		if(token.type != ',')
			break;
		next_token();
	}
	expect_void(';');
}

static void parse_struct_declarators(const declaration_specifiers_t *specifiers)
{
	while(1) {
		if(token.type == ':') {
			next_token();
			parse_constant_expression();
			/* TODO (bitfields) */
		} else {
			declaration_t *declaration
				= allocate_ast_zero(sizeof(declaration[0]));
			parse_declarator(declaration, specifiers->storage_class,
			                 specifiers->type, 1);

			/* TODO: check for doubled fields */
			record_declaration(declaration);

			if(token.type == ':') {
				next_token();
				parse_constant_expression();
				/* TODO (bitfields) */
			}
		}

		if(token.type != ',')
			break;
		next_token();
	}
	expect_void(';');
}

static void parse_compound_type_entries(void)
{
	eat('{');

	while(token.type != '}' && token.type != T_EOF) {
		declaration_specifiers_t specifiers;
		memset(&specifiers, 0, sizeof(specifiers));
		/* TODO not correct as this allows storage class stuff... but only
		 * specifiers and qualifiers sould be allowed here */
		parse_declaration_specifiers(&specifiers);

		parse_struct_declarators(&specifiers);
	}
	if(token.type == T_EOF) {
		parse_error("unexpected error while parsing struct");
	}
	next_token();
}

static void parse_declaration(void)
{
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));
	parse_declaration_specifiers(&specifiers);

	if(token.type == ';') {
		next_token();
		return;
	}
	parse_init_declarators(&specifiers);
}

static type_t *parse_typename(void)
{
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));
	parse_declaration_specifiers(&specifiers);
	if(specifiers.storage_class != STORAGE_CLASS_NONE) {
		/* TODO: improve error message, user does probably not know what a
		 * storage class is...
		 */
		parse_error("typename may not have a storage class");
	}

	type_t *result = parse_abstract_declarator(specifiers.type);

	return result;
}




typedef expression_t* (*parse_expression_function) (unsigned precedence);
typedef expression_t* (*parse_expression_infix_function) (unsigned precedence,
                                                          expression_t *left);

typedef struct expression_parser_function_t expression_parser_function_t;
struct expression_parser_function_t {
	unsigned                         precedence;
	parse_expression_function        parser;
	unsigned                         infix_precedence;
	parse_expression_infix_function  infix_parser;
};

expression_parser_function_t expression_parsers[T_LAST_TOKEN];

static expression_t *expected_expression_error(void)
{
	parser_print_error_prefix();
	fprintf(stderr, "expected expression, got token ");
	print_token(stderr, & token);
	fprintf(stderr, "\n");

	expression_t *expression = allocate_ast_zero(sizeof(expression[0]));
	expression->type = EXPR_INVALID;
	next_token();

	return expression;
}

static expression_t *parse_string_const(void)
{
	string_literal_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type     = EXPR_STRING_LITERAL;
	cnst->expression.datatype = type_string;
	cnst->value               = parse_string_literals();

	return (expression_t*) cnst;
}

static expression_t *parse_int_const(void)
{
	const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type     = EXPR_CONST;
	cnst->expression.datatype = type_int;
	cnst->v.int_value         = token.v.intvalue;

	next_token();

	return (expression_t*) cnst;
}

static expression_t *parse_float_const(void)
{
	const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type     = EXPR_CONST;
	cnst->expression.datatype = type_int;
	cnst->v.float_value       = token.v.floatvalue;

	next_token();

	return (expression_t*) cnst;
}

static expression_t *parse_reference(void)
{
	reference_expression_t *ref = allocate_ast_zero(sizeof(ref[0]));

	ref->expression.type = EXPR_REFERENCE;
	ref->symbol          = token.v.symbol;

	declaration_t *declaration = ref->symbol->declaration;
	next_token();

	if(declaration == NULL) {
#ifndef STRICT_C99
		/* is it an implicitely defined function */
		if(token.type == '(') {
			parser_print_prefix_pos(token.source_position);
			fprintf(stderr, "warning: implicit declaration of function '%s'\n",
			        ref->symbol->string);
			/* TODO: do this correctly */
			return (expression_t*) ref;
		}
#endif

		parser_print_error_prefix();
		fprintf(stderr, "unknown symbol '%s' found.\n", ref->symbol->string);
	} else {
		ref->declaration         = declaration;
		ref->expression.datatype = declaration->type;
	}


	return (expression_t*) ref;
}

static void check_cast_allowed(expression_t *expression, type_t *dest_type)
{
	(void) expression;
	(void) dest_type;
	/* TODO check if cast is allowed and issue warnings/errors */
}

static expression_t *parse_cast(void)
{
	unary_expression_t *cast = allocate_ast_zero(sizeof(cast[0]));

	cast->expression.type            = EXPR_UNARY;
	cast->type                       = UNEXPR_CAST;
	cast->expression.source_position = token.source_position;

	type_t *type  = parse_typename();

	expect(')');
	expression_t *value = parse_sub_expression(20);

	check_cast_allowed(value, type);

	cast->expression.datatype = type;
	cast->value               = value;

	return (expression_t*) cast;
}

static expression_t *parse_statement_expression(void)
{
	statement_expression_t *expression
		= allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type = EXPR_STATEMENT;
	expression->statement       = parse_compound_statement();

	/* find last statement and use it's type */
	const statement_t *last_statement = NULL;
	const statement_t *statement      = expression->statement;
	for( ; statement != NULL; statement = statement->next) {
		last_statement = statement;
	}

	if(last_statement->type == STATEMENT_EXPRESSION) {
		const expression_statement_t *expression_statement =
			(const expression_statement_t*) last_statement;
		expression->expression.datatype
			= expression_statement->expression->datatype;
	} else {
		expression->expression.datatype = type_void;
	}

	expect(')');

	return (expression_t*) expression;
}

static expression_t *parse_brace_expression(void)
{
	eat('(');

	declaration_t *declaration;
	switch(token.type) {
	case '{':
		/* gcc extension: a stement expression */
		return parse_statement_expression();

	TYPE_QUALIFIERS
	TYPE_SPECIFIERS
		return parse_cast();
	case T_IDENTIFIER:
		declaration = token.v.symbol->declaration;
		if(declaration != NULL &&
				(declaration->storage_class == STORAGE_CLASS_TYPEDEF)) {
			return parse_cast();
		}
	}

	expression_t *result = parse_expression();
	expect(')');

	return result;
}

static expression_t *parse_function_keyword(void)
{
	eat(T___FUNCTION__);
	/* TODO */

	string_literal_t *expression = allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type     = EXPR_FUNCTION;
	expression->expression.datatype = type_string;
	expression->value               = "TODO: FUNCTION";

	return (expression_t*) expression;
}

static expression_t *parse_pretty_function_keyword(void)
{
	eat(T___PRETTY_FUNCTION__);
	/* TODO */

	string_literal_t *expression = allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type     = EXPR_PRETTY_FUNCTION;
	expression->expression.datatype = type_string;
	expression->value               = "TODO: PRETTY FUNCTION";

	return (expression_t*) expression;
}

static member_designator_t *parse_member_designators(void)
{
	member_designator_t *result = allocate_ast_zero(sizeof(result[0]));

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing member designator",
		                     T_IDENTIFIER, 0);
		eat_brace();
		return NULL;
	}
	result->symbol = token.v.symbol;
	next_token();

	member_designator_t *last_designator = result;
	while(true) {
		if(token.type == '.') {
			next_token();
			if(token.type != T_IDENTIFIER) {
				parse_error_expected("problem while parsing member designator",
					T_IDENTIFIER, 0);
				eat_brace();
				return NULL;
			}
			member_designator_t *designator
				= allocate_ast_zero(sizeof(result[0]));
			designator->symbol = token.v.symbol;
			next_token();

			last_designator->next = designator;
			last_designator       = designator;
			continue;
		}
		if(token.type == '[') {
			next_token();
			member_designator_t *designator
				= allocate_ast_zero(sizeof(result[0]));
			designator->array_access = parse_expression();
			if(designator->array_access == NULL) {
				eat_brace();
				return NULL;
			}
			expect(']');

			last_designator->next = designator;
			last_designator       = designator;
			continue;
		}
		break;
	}

	return result;
}

static expression_t *parse_offsetof(void)
{
	eat(T___builtin_offsetof);

	offsetof_expression_t *expression
		= allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type     = EXPR_OFFSETOF;
	expression->expression.datatype = type_size_t;

	expect('(');
	expression->type = parse_typename();
	expect(',');
	expression->member_designators = parse_member_designators();
	expect(')');

	return (expression_t*) expression;
}

static expression_t *parse_builtin_symbol(void)
{
	builtin_symbol_expression_t *expression
		= allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type = EXPR_BUILTIN_SYMBOL;

	/* TODO: set datatype */

	expression->symbol = token.v.symbol;

	next_token();

	return (expression_t*) expression;
}

static expression_t *parse_primary_expression(void)
{
	switch(token.type) {
	case T_INTEGER:
		return parse_int_const();
	case T_FLOATINGPOINT:
		return parse_float_const();
	case T_STRING_LITERAL:
		return parse_string_const();
	case T_IDENTIFIER:
		return parse_reference();
	case T___FUNCTION__:
		return parse_function_keyword();
	case T___PRETTY_FUNCTION__:
		return parse_pretty_function_keyword();
	case T___builtin_offsetof:
		return parse_offsetof();
	case T___builtin_expect:
	case T___builtin_va_start:
	case T___builtin_va_arg:
	case T___builtin_va_end:
		return parse_builtin_symbol();

	case '(':
		return parse_brace_expression();
	}

	parser_print_error_prefix();
	fprintf(stderr, "unexpected token ");
	print_token(stderr, &token);
	fprintf(stderr, "\n");
	eat_statement();

	expression_t *expression = allocate_ast_zero(sizeof(expression[0]));
	expression->type     = EXPR_INVALID;
	expression->datatype = type_void;

	return expression;
}

static expression_t *parse_array_expression(unsigned precedence,
                                            expression_t *array_ref)
{
	(void) precedence;

	eat('[');

	array_access_expression_t *array_access
		= allocate_ast_zero(sizeof(array_access[0]));

	array_access->expression.type     = EXPR_ARRAY_ACCESS;
	array_access->array_ref           = array_ref;
	array_access->index               = parse_expression();

	type_t *array_type = array_ref->datatype;
	if(array_type != NULL) {
		if(array_type->type == TYPE_POINTER) {
			pointer_type_t *pointer           = (pointer_type_t*) array_type;
			array_access->expression.datatype = pointer->points_to;
		} else {
			parser_print_error_prefix();
			fprintf(stderr, "array access on object with non-pointer type ");
			print_type(array_type);
			fprintf(stderr, "\n");
		}
	}

	if(token.type != ']') {
		parse_error_expected("Problem while parsing array access", ']', 0);
		return (expression_t*) array_access;
	}
	next_token();

	return (expression_t*) array_access;
}

static bool is_declaration_specifier(const token_t *token,
                                     bool only_type_specifiers)
{
	declaration_t *declaration;

	switch(token->type) {
		TYPE_SPECIFIERS
			return 1;
		case T_IDENTIFIER:
			declaration = token->v.symbol->declaration;
			if(declaration == NULL)
				return 0;
			if(declaration->storage_class != STORAGE_CLASS_TYPEDEF)
				return 0;
			return 1;
		STORAGE_CLASSES
		TYPE_QUALIFIERS
			if(only_type_specifiers)
				return 0;
			return 1;

		default:
			return 0;
	}
}

static expression_t *parse_sizeof(unsigned precedence)
{
	eat(T_sizeof);

	sizeof_expression_t *sizeof_expression
		= allocate_ast_zero(sizeof(sizeof_expression[0]));
	sizeof_expression->expression.type     = EXPR_SIZEOF;
	sizeof_expression->expression.datatype = type_size_t;

	if(token.type == '(' && is_declaration_specifier(look_ahead(1), true)) {
		next_token();
		sizeof_expression->type = parse_typename();
		expect(')');
	} else {
		expression_t *expression           = parse_sub_expression(precedence);
		sizeof_expression->type            = expression->datatype;
		sizeof_expression->size_expression = expression;
	}

	return (expression_t*) sizeof_expression;
}

static expression_t *parse_select_expression(unsigned precedence,
                                             expression_t *compound)
{
	(void) precedence;

	assert(token.type == '.' || token.type == T_MINUSGREATER);
	next_token();

	select_expression_t *select = allocate_ast_zero(sizeof(select[0]));

	select->expression.type = EXPR_SELECT;
	select->compound        = compound;

	/* TODO: datatype */

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing select", T_IDENTIFIER, 0);
		return (expression_t*) select;
	}
	select->symbol = token.v.symbol;
	next_token();

	return (expression_t*) select;
}

static expression_t *parse_call_expression(unsigned precedence,
                                           expression_t *expression)
{
	(void) precedence;
	call_expression_t *call = allocate_ast_zero(sizeof(call[0]));

	call->expression.type     = EXPR_CALL;
	call->method              = expression;

	/* parse arguments */
	eat('(');

	if(token.type != ')') {
		call_argument_t *last_argument = NULL;

		while(true) {
			call_argument_t *argument = allocate_ast_zero(sizeof(argument[0]));

			argument->expression = parse_assignment_expression();
			if(last_argument == NULL) {
				call->arguments = argument;
			} else {
				last_argument->next = argument;
			}
			last_argument = argument;

			if(token.type != ',')
				break;
			next_token();
		}
	}
	expect(')');

	type_t *type = expression->datatype;
	if(type != NULL) {
		/* we can call pointer to function */
		if(type->type == TYPE_POINTER) {
			pointer_type_t *pointer = (pointer_type_t*) type;
			type = pointer->points_to;
		}

		if(type == NULL || type->type != TYPE_METHOD) {
			parser_print_error_prefix();
			fprintf(stderr, "expected a method type for call but found type ");
			print_type(expression->datatype);
			fprintf(stderr, "\n");
		} else {
			method_type_t *method_type = (method_type_t*) type;
			call->expression.datatype  = method_type->result_type;
		}
	}

	return (expression_t*) call;
}

static void type_error(const char *msg, const source_position_t source_position,
                       const type_t *type)
{
	parser_print_error_prefix_pos(source_position);
	fprintf(stderr, "%s, but found type ", msg);
	print_type(type);
	fputc('\n', stderr);
	error();
}

static void type_error_incompatible(const char *msg,
		const source_position_t source_position, const type_t *type1,
		const type_t *type2)
{
	parser_print_error_prefix_pos(source_position);
	fprintf(stderr, "%s, incompatible types: ", msg);
	print_type(type1);
	fprintf(stderr, " - ");
	print_type(type2);
	fprintf(stderr, ")\n");
	error();
}

static type_t *get_type_after_conversion(const type_t *type1,
                                         const type_t *type2)
{
	/* TODO... */
	(void) type2;
	return (type_t*) type1;
}

static expression_t *parse_conditional_expression(unsigned precedence,
                                                  expression_t *expression)
{
	eat('?');

	conditional_expression_t *conditional
		= allocate_ast_zero(sizeof(conditional[0]));
	conditional->expression.type = EXPR_CONDITIONAL;
	conditional->condition = expression;

	/* 6.5.15.2 */
	type_t *condition_type = conditional->condition->datatype;
	if(condition_type != NULL) {
		if(!is_type_scalar(condition_type)) {
			type_error("expected a scalar type", expression->source_position,
			           condition_type);
		}
	}

	conditional->true_expression = parse_expression();
	expect(':');
	conditional->false_expression = parse_sub_expression(precedence);

	type_t *true_type  = conditional->true_expression->datatype;
	if(true_type == NULL)
		return (expression_t*) conditional;
	type_t *false_type = conditional->false_expression->datatype;
	if(false_type == NULL)
		return (expression_t*) conditional;

	/* 6.4.15.3 */
	if(true_type == false_type) {
		conditional->expression.datatype = true_type;
	} else if(is_type_arithmetic(true_type) && is_type_arithmetic(false_type)) {
		type_t *result = get_type_after_conversion(true_type, false_type);
		/* TODO: create implicit convs if necessary */
		conditional->expression.datatype = result;
	} else if(true_type->type == TYPE_POINTER &&
	          false_type->type == TYPE_POINTER &&
			  true /* TODO compatible points_to types */) {
		/* TODO */
	} else if(/* (is_null_ptr_const(true_type) && false_type->type == TYPE_POINTER)
	       || (is_null_ptr_const(false_type) &&
	           true_type->type == TYPE_POINTER) TODO*/ false) {
		/* TODO */
	} else if(/* 1 is pointer to object type, other is void* */ false) {
		/* TODO */
	} else {
		type_error_incompatible("problem while parsing conditional",
		                        expression->source_position, true_type,
		                        false_type);
	}

	return (expression_t*) conditional;
}

static expression_t *parse_extension(unsigned precedence)
{
	eat(T___extension__);

	/* TODO enable extensions */

	return parse_sub_expression(precedence);
}

#define CREATE_UNARY_EXPRESSION_PARSER(token_type, unexpression_type)     \
static                                                                    \
expression_t *parse_##unexpression_type(unsigned precedence)              \
{                                                                         \
	eat(token_type);                                                      \
                                                                          \
	unary_expression_t *unary_expression                                  \
		= allocate_ast_zero(sizeof(unary_expression[0]));                 \
	unary_expression->expression.type = EXPR_UNARY;                       \
	unary_expression->type            = unexpression_type;                \
	unary_expression->value           = parse_sub_expression(precedence); \
                                                                          \
	return (expression_t*) unary_expression;                              \
}

CREATE_UNARY_EXPRESSION_PARSER('-', UNEXPR_NEGATE)
CREATE_UNARY_EXPRESSION_PARSER('+', UNEXPR_PLUS)
CREATE_UNARY_EXPRESSION_PARSER('!', UNEXPR_NOT)
CREATE_UNARY_EXPRESSION_PARSER('*', UNEXPR_DEREFERENCE)
CREATE_UNARY_EXPRESSION_PARSER('&', UNEXPR_TAKE_ADDRESS)
CREATE_UNARY_EXPRESSION_PARSER('~', UNEXPR_BITWISE_NEGATE)
CREATE_UNARY_EXPRESSION_PARSER(T_PLUSPLUS,   UNEXPR_PREFIX_INCREMENT)
CREATE_UNARY_EXPRESSION_PARSER(T_MINUSMINUS, UNEXPR_PREFIX_DECREMENT)

#define CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(token_type, unexpression_type) \
static                                                                        \
expression_t *parse_##unexpression_type(unsigned precedence,                  \
                                        expression_t *left)                   \
{                                                                             \
	(void) precedence;                                                        \
	eat(token_type);                                                          \
                                                                              \
	unary_expression_t *unary_expression                                      \
		= allocate_ast_zero(sizeof(unary_expression[0]));                     \
	unary_expression->expression.type = EXPR_UNARY;                           \
	unary_expression->type            = unexpression_type;                    \
	unary_expression->value           = left;                                 \
                                                                              \
	return (expression_t*) unary_expression;                                  \
}

CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(T_PLUSPLUS,   UNEXPR_POSTFIX_INCREMENT)
CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(T_MINUSMINUS, UNEXPR_POSTFIX_DECREMENT)

#define CREATE_BINEXPR_PARSER(token_type, binexpression_type)    \
static                                                           \
expression_t *parse_##binexpression_type(unsigned precedence,    \
                                         expression_t *left)     \
{                                                                \
	eat(token_type);                                             \
                                                                 \
	expression_t *right = parse_sub_expression(precedence);      \
                                                                 \
	binary_expression_t *binexpr                                 \
		= allocate_ast_zero(sizeof(binexpr[0]));                 \
	binexpr->expression.type = EXPR_BINARY;                      \
	binexpr->type            = binexpression_type;               \
	binexpr->left            = left;                             \
	binexpr->right           = right;                            \
                                                                 \
	return (expression_t*) binexpr;                              \
}

CREATE_BINEXPR_PARSER(',', BINEXPR_COMMA)
CREATE_BINEXPR_PARSER('*', BINEXPR_MUL)
CREATE_BINEXPR_PARSER('/', BINEXPR_DIV)
CREATE_BINEXPR_PARSER('%', BINEXPR_MOD)
CREATE_BINEXPR_PARSER('+', BINEXPR_ADD)
CREATE_BINEXPR_PARSER('-', BINEXPR_SUB)
CREATE_BINEXPR_PARSER('<', BINEXPR_LESS)
CREATE_BINEXPR_PARSER('>', BINEXPR_GREATER)
CREATE_BINEXPR_PARSER('=', BINEXPR_ASSIGN)
CREATE_BINEXPR_PARSER(T_EQUALEQUAL, BINEXPR_EQUAL)
CREATE_BINEXPR_PARSER(T_EXCLAMATIONMARKEQUAL, BINEXPR_NOTEQUAL)
CREATE_BINEXPR_PARSER(T_LESSEQUAL, BINEXPR_LESSEQUAL)
CREATE_BINEXPR_PARSER(T_GREATEREQUAL, BINEXPR_GREATEREQUAL)
CREATE_BINEXPR_PARSER('&', BINEXPR_BITWISE_AND)
CREATE_BINEXPR_PARSER('|', BINEXPR_BITWISE_OR)
CREATE_BINEXPR_PARSER('^', BINEXPR_BITWISE_XOR)
CREATE_BINEXPR_PARSER(T_ANDAND, BINEXPR_LOGICAL_AND)
CREATE_BINEXPR_PARSER(T_PIPEPIPE, BINEXPR_LOGICAL_OR)
CREATE_BINEXPR_PARSER(T_LESSLESS, BINEXPR_SHIFTLEFT)
CREATE_BINEXPR_PARSER(T_GREATERGREATER, BINEXPR_SHIFTRIGHT)
CREATE_BINEXPR_PARSER(T_PLUSEQUAL, BINEXPR_ADD_ASSIGN)
CREATE_BINEXPR_PARSER(T_MINUSEQUAL, BINEXPR_SUB_ASSIGN)
CREATE_BINEXPR_PARSER(T_ASTERISKEQUAL, BINEXPR_MUL_ASSIGN)
CREATE_BINEXPR_PARSER(T_SLASHEQUAL, BINEXPR_DIV_ASSIGN)
CREATE_BINEXPR_PARSER(T_PERCENTEQUAL, BINEXPR_MOD_ASSIGN)
CREATE_BINEXPR_PARSER(T_LESSLESSEQUAL, BINEXPR_SHIFTLEFT_ASSIGN)
CREATE_BINEXPR_PARSER(T_GREATERGREATEREQUAL, BINEXPR_SHIFTRIGHT_ASSIGN)
CREATE_BINEXPR_PARSER(T_ANDEQUAL, BINEXPR_BITWISE_AND_ASSIGN)
CREATE_BINEXPR_PARSER(T_PIPEEQUAL, BINEXPR_BITWISE_OR_ASSIGN)
CREATE_BINEXPR_PARSER(T_CARETEQUAL, BINEXPR_BITWISE_XOR_ASSIGN)

static expression_t *parse_sub_expression(unsigned precedence)
{
	if(token.type < 0) {
		return expected_expression_error();
	}

	expression_parser_function_t *parser
		= &expression_parsers[token.type];
	source_position_t             source_position = token.source_position;
	expression_t                 *left;

	if(parser->parser != NULL) {
		left = parser->parser(parser->precedence);
	} else {
		left = parse_primary_expression();
	}
	assert(left != NULL);
	left->source_position = source_position;

	while(true) {
		if(token.type < 0) {
			return expected_expression_error();
		}

		parser = &expression_parsers[token.type];
		if(parser->infix_parser == NULL)
			break;
		if(parser->infix_precedence < precedence)
			break;

		left = parser->infix_parser(parser->infix_precedence, left);

		assert(left != NULL);
		assert(left->type != EXPR_INVALID);
		left->source_position = source_position;
	}

	return left;
}

static expression_t *parse_expression(void)
{
	return parse_sub_expression(1);
}



void register_expression_parser(parse_expression_function parser,
                                int token_type, unsigned precedence)
{
	expression_parser_function_t *entry = &expression_parsers[token_type];

	if(entry->parser != NULL) {
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
		panic("trying to register multiple expression parsers for a token");
	}
	entry->parser     = parser;
	entry->precedence = precedence;
}

void register_expression_infix_parser(parse_expression_infix_function parser,
                                      int token_type, unsigned precedence)
{
	expression_parser_function_t *entry = &expression_parsers[token_type];

	if(entry->infix_parser != NULL) {
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
		panic("trying to register multiple infix expression parsers for a "
		      "token");
	}
	entry->infix_parser     = parser;
	entry->infix_precedence = precedence;
}

static void init_expression_parsers(void)
{
	memset(&expression_parsers, 0, sizeof(expression_parsers));

	register_expression_infix_parser(parse_BINEXPR_MUL,         '*',        16);
	register_expression_infix_parser(parse_BINEXPR_DIV,         '/',        16);
	register_expression_infix_parser(parse_BINEXPR_MOD,         '%',        16);
	register_expression_infix_parser(parse_BINEXPR_SHIFTLEFT,   T_LESSLESS, 16);
	register_expression_infix_parser(parse_BINEXPR_SHIFTRIGHT,
	                                                      T_GREATERGREATER, 16);
	register_expression_infix_parser(parse_BINEXPR_ADD,         '+',        15);
	register_expression_infix_parser(parse_BINEXPR_SUB,         '-',        15);
	register_expression_infix_parser(parse_BINEXPR_LESS,        '<',        14);
	register_expression_infix_parser(parse_BINEXPR_GREATER,     '>',        14);
	register_expression_infix_parser(parse_BINEXPR_LESSEQUAL, T_LESSEQUAL,  14);
	register_expression_infix_parser(parse_BINEXPR_GREATEREQUAL,
	                                                        T_GREATEREQUAL, 14);
	register_expression_infix_parser(parse_BINEXPR_EQUAL,     T_EQUALEQUAL, 13);
	register_expression_infix_parser(parse_BINEXPR_NOTEQUAL,
	                                                T_EXCLAMATIONMARKEQUAL, 13);
	register_expression_infix_parser(parse_BINEXPR_BITWISE_AND, '&',        12);
	register_expression_infix_parser(parse_BINEXPR_BITWISE_XOR, '^',        11);
	register_expression_infix_parser(parse_BINEXPR_BITWISE_OR,  '|',        10);
	register_expression_infix_parser(parse_BINEXPR_LOGICAL_AND, T_ANDAND,    9);
	register_expression_infix_parser(parse_BINEXPR_LOGICAL_OR,  T_PIPEPIPE,  8);
	register_expression_infix_parser(parse_conditional_expression, '?',      7);
	register_expression_infix_parser(parse_BINEXPR_ASSIGN,      '=',         2);
	register_expression_infix_parser(parse_BINEXPR_ADD_ASSIGN, T_PLUSEQUAL,  2);
	register_expression_infix_parser(parse_BINEXPR_SUB_ASSIGN, T_MINUSEQUAL, 2);
	register_expression_infix_parser(parse_BINEXPR_MUL_ASSIGN,
	                                                        T_ASTERISKEQUAL, 2);
	register_expression_infix_parser(parse_BINEXPR_DIV_ASSIGN, T_SLASHEQUAL, 2);
	register_expression_infix_parser(parse_BINEXPR_MOD_ASSIGN,
	                                                         T_PERCENTEQUAL, 2);
	register_expression_infix_parser(parse_BINEXPR_SHIFTLEFT_ASSIGN,
	                                                        T_LESSLESSEQUAL, 2);
	register_expression_infix_parser(parse_BINEXPR_SHIFTRIGHT_ASSIGN,
	                                                  T_GREATERGREATEREQUAL, 2);
	register_expression_infix_parser(parse_BINEXPR_BITWISE_AND_ASSIGN,
	                                                             T_ANDEQUAL, 2);
	register_expression_infix_parser(parse_BINEXPR_BITWISE_OR_ASSIGN,
	                                                            T_PIPEEQUAL, 2);
	register_expression_infix_parser(parse_BINEXPR_BITWISE_XOR_ASSIGN,
	                                                           T_CARETEQUAL, 2);

	register_expression_infix_parser(parse_BINEXPR_COMMA,       ',',         1);

	register_expression_infix_parser(parse_array_expression,        '[',    30);
	register_expression_infix_parser(parse_call_expression,         '(',    30);
	register_expression_infix_parser(parse_select_expression,       '.',    30);
	register_expression_infix_parser(parse_select_expression,
	                                                        T_MINUSGREATER, 30);
	register_expression_infix_parser(parse_UNEXPR_POSTFIX_INCREMENT,
	                                 T_PLUSPLUS, 30);
	register_expression_infix_parser(parse_UNEXPR_POSTFIX_DECREMENT,
	                                 T_MINUSMINUS, 30);

	register_expression_parser(parse_UNEXPR_NEGATE,           '-',          25);
	register_expression_parser(parse_UNEXPR_PLUS,             '+',          25);
	register_expression_parser(parse_UNEXPR_NOT,              '!',          25);
	register_expression_parser(parse_UNEXPR_BITWISE_NEGATE,   '~',          25);
	register_expression_parser(parse_UNEXPR_DEREFERENCE,      '*',          25);
	register_expression_parser(parse_UNEXPR_TAKE_ADDRESS,     '&',          25);
	register_expression_parser(parse_UNEXPR_PREFIX_INCREMENT, T_PLUSPLUS,   25);
	register_expression_parser(parse_UNEXPR_PREFIX_DECREMENT, T_MINUSMINUS, 25);
	register_expression_parser(parse_sizeof,                  T_sizeof,     25);
	register_expression_parser(parse_extension,            T___extension__, 25);
}


static statement_t *parse_case_statement(void)
{
	eat(T_case);
	case_label_statement_t *label = allocate_ast_zero(sizeof(label[0]));
	label->statement.type            = STATEMENT_CASE_LABEL;
	label->statement.source_position = token.source_position;

	label->expression = parse_expression();

	expect(':');
	label->statement.next = parse_statement();

	return (statement_t*) label;
}

static statement_t *parse_default_statement(void)
{
	eat(T_default);

	case_label_statement_t *label = allocate_ast_zero(sizeof(label[0]));
	label->statement.type            = STATEMENT_CASE_LABEL;
	label->statement.source_position = token.source_position;

	expect(':');
	label->statement.next = parse_statement();

	return (statement_t*) label;
}

static statement_t *parse_label_statement(void)
{
	eat(T_IDENTIFIER);
	expect(':');
	parse_statement();

	return NULL;
}

static statement_t *parse_if(void)
{
	eat(T_if);

	if_statement_t *statement = allocate_ast_zero(sizeof(statement[0]));
	statement->statement.type            = STATEMENT_IF;
	statement->statement.source_position = token.source_position;

	expect('(');
	statement->condition = parse_expression();
	expect(')');

	statement->true_statement = parse_statement();
	if(token.type == T_else) {
		next_token();
		statement->false_statement = parse_statement();
	}

	return (statement_t*) statement;
}

static statement_t *parse_switch(void)
{
	eat(T_switch);

	switch_statement_t *statement = allocate_ast_zero(sizeof(statement[0]));
	statement->statement.type            = STATEMENT_SWITCH;
	statement->statement.source_position = token.source_position;

	expect('(');
	statement->expression = parse_expression();
	expect(')');
	statement->body = parse_statement();

	return (statement_t*) statement;
}

static statement_t *parse_while(void)
{
	eat(T_while);

	while_statement_t *statement = allocate_ast_zero(sizeof(statement[0]));
	statement->statement.type            = STATEMENT_WHILE;
	statement->statement.source_position = token.source_position;

	expect('(');
	statement->condition = parse_expression();
	expect(')');
	statement->body = parse_statement();

	return (statement_t*) statement;
}

static statement_t *parse_do(void)
{
	eat(T_do);

	do_while_statement_t *statement = allocate_ast_zero(sizeof(statement[0]));
	statement->statement.type            = STATEMENT_DO_WHILE;
	statement->statement.source_position = token.source_position;

	statement->body = parse_statement();
	expect(T_while);
	expect('(');
	statement->condition = parse_expression();
	expect(')');
	expect(';');

	return (statement_t*) statement;
}

static statement_t *parse_for(void)
{
	eat(T_for);

	for_statement_t *statement = allocate_ast_zero(sizeof(statement[0]));
	statement->statement.type            = STATEMENT_FOR;
	statement->statement.source_position = token.source_position;

	expect('(');

	int         top          = environment_top();
	context_t  *last_context = context;
	set_context(&statement->context);

	if(token.type != ';') {
		if(is_declaration_specifier(&token, false)) {
			parse_declaration();
		} else {
			statement->initialisation = parse_expression();
			expect(';');
		}
	} else {
		expect(';');
	}

	if(token.type != ';') {
		statement->condition = parse_expression();
	}
	expect(';');
	if(token.type != ')') {
		statement->step = parse_expression();
	}
	expect(')');
	statement->body = parse_statement();

	assert(context == &statement->context);
	set_context(last_context);
	environment_pop_to(top);

	return (statement_t*) statement;
}

static statement_t *parse_goto(void)
{
	eat(T_goto);
	expect(T_IDENTIFIER);
	expect(';');

	return NULL;
}

static statement_t *parse_continue(void)
{
	eat(T_continue);
	expect(';');

	statement_t *statement     = allocate_ast_zero(sizeof(statement[0]));
	statement->source_position = token.source_position;
	statement->type            = STATEMENT_CONTINUE;

	return statement;
}

static statement_t *parse_break(void)
{
	eat(T_break);
	expect(';');

	statement_t *statement     = allocate_ast_zero(sizeof(statement[0]));
	statement->source_position = token.source_position;
	statement->type            = STATEMENT_BREAK;

	return statement;
}

static statement_t *parse_return(void)
{
	eat(T_return);

	return_statement_t *statement = allocate_ast_zero(sizeof(statement[0]));

	statement->statement.type            = STATEMENT_RETURN;
	statement->statement.source_position = token.source_position;
	if(token.type != ';') {
		statement->return_value = parse_expression();
	}
	expect(';');

	return (statement_t*) statement;
}

static statement_t *parse_declaration_statement(void)
{
	declaration_t *before = last_declaration;

	declaration_statement_t *statement
		= allocate_ast_zero(sizeof(statement[0]));
	statement->statement.type            = STATEMENT_DECLARATION;
	statement->statement.source_position = token.source_position;

	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));
	parse_declaration_specifiers(&specifiers);

	if(token.type == ';') {
		eat(';');
	} else {
		parse_init_declarators(&specifiers);
	}

	if(before == NULL) {
		statement->declarations_begin = context->declarations;
	} else {
		statement->declarations_begin = before->next;
	}
	statement->declarations_end = last_declaration;

	return (statement_t*) statement;
}

static statement_t *parse_expression_statement(void)
{
	expression_statement_t *statement = allocate_ast_zero(sizeof(statement[0]));
	statement->statement.type            = STATEMENT_EXPRESSION;
	statement->statement.source_position = token.source_position;

	statement->expression = parse_expression();

	expect(';');

	return (statement_t*) statement;
}

static statement_t *parse_statement(void)
{
	declaration_t *declaration;
	statement_t   *statement = NULL;

	/* declaration or statement */
	switch(token.type) {
	case T_case:
		statement = parse_case_statement();
		break;

	case T_default:
		statement = parse_default_statement();
		break;

	case '{':
		statement = parse_compound_statement();
		break;

	case T_if:
		statement = parse_if();
		break;

	case T_switch:
		statement = parse_switch();
		break;

	case T_while:
		statement = parse_while();
		break;

	case T_do:
		statement = parse_do();
		break;

	case T_for:
		statement = parse_for();
		break;

	case T_goto:
		statement = parse_goto();
		break;

	case T_continue:
		statement = parse_continue();
		break;

	case T_break:
		statement = parse_break();
		break;

	case T_return:
		statement = parse_return();
		break;

	case ';':
		next_token();
		statement = NULL;
		break;

	case T_IDENTIFIER:
		if(look_ahead(1)->type == ':') {
			statement = parse_label_statement();
			break;
		}

		declaration = token.v.symbol->declaration;
		if(declaration != NULL &&
				declaration->storage_class == STORAGE_CLASS_TYPEDEF) {
			statement = parse_declaration_statement();
			break;
		}

		statement = parse_expression_statement();
		break;

	case T___extension__:
		/* this can be a prefix to a declaration or an expression statement */
		/* we simply eat it now and parse the rest with tail recursion */
		do {
			next_token();
		} while(token.type == T___extension__);
		statement = parse_statement();
		break;

	DECLARATION_START
		statement = parse_declaration_statement();
		break;

	default:
		statement = parse_expression_statement();
		break;
	}

	assert(statement == NULL || statement->source_position.input_name != NULL);

	return statement;
}

static statement_t *parse_compound_statement(void)
{
	eat('{');

	compound_statement_t *compound_statement
		= allocate_ast_zero(sizeof(compound_statement[0]));
	compound_statement->statement.type            = STATEMENT_COMPOUND;
	compound_statement->statement.source_position = token.source_position;

	int        top          = environment_top();
	context_t *last_context = context;
	set_context(&compound_statement->context);

	statement_t *last_statement = NULL;

	while(token.type != '}' && token.type != T_EOF) {
		statement_t *statement = parse_statement();
		if(statement == NULL)
			continue;

		if(last_statement != NULL) {
			last_statement->next = statement;
		} else {
			compound_statement->statements = statement;
		}

		while(statement->next != NULL)
			statement = statement->next;

		last_statement = statement;
	}

	assert(context == &compound_statement->context);
	set_context(last_context);
	environment_pop_to(top);

	next_token();

	return (statement_t*) compound_statement;
}

static translation_unit_t *parse_translation_unit(void)
{
	translation_unit_t *unit = allocate_ast_zero(sizeof(unit[0]));

	assert(context == NULL);
	set_context(&unit->context);

	while(token.type != T_EOF) {
		parse_declaration();
	}

	assert(context == &unit->context);
	context          = NULL;
	last_declaration = NULL;

	return unit;
}

translation_unit_t *parse(void)
{
	obstack_init(&environment_obstack);
	environment_stack = NEW_ARR_F(environment_entry_t*, 0);

	type_set_output(stderr);

	lookahead_bufpos = 0;
	for(int i = 0; i < MAX_LOOKAHEAD + 2; ++i) {
		next_token();
	}
	translation_unit_t *unit = parse_translation_unit();

	DEL_ARR_F(environment_stack);
	obstack_free(&environment_obstack, NULL);

	return unit;
}

void init_parser(void)
{
	init_expression_parsers();
	obstack_init(&temp_obst);

	type_int        = make_atomic_type(ATOMIC_TYPE_INT, 0);
	type_size_t     = make_atomic_type(ATOMIC_TYPE_UINT, 0);
	type_const_char = make_atomic_type(ATOMIC_TYPE_CHAR, TYPE_QUALIFIER_CONST);
	type_void       = make_atomic_type(ATOMIC_TYPE_VOID, 0);
	type_string     = make_pointer_type(type_const_char, 0);
}

void exit_parser(void)
{
	obstack_free(&temp_obst, NULL);
}
