#include <config.h>

#include <assert.h>

#include "lexer_t.h"
#include "token_t.h"
#include "type_t.h"
#include "ast_t.h"
#include "adt/bitfiddle.h"

#define PRINT_TOKENS

static lexer_t lexer;
static token_t token;

static inline
void next_token()
{
	lexer_next_token(&lexer, &token);

#ifdef PRINT_TOKENS
	print_token(stderr, &token);
	fprintf(stderr, "\n");
#endif
}

static inline
void eat(token_type_t type)
{
	assert(token.type == type);
	next_token();
}

void parser_print_error_prefix()
{
    fputs(lexer.source_position.input_name, stderr);
    fputc(':', stderr);
    fprintf(stderr, "%d", lexer.source_position.linenr);
    fputs(": error: ", stderr);
}

static
void parse_error(const char *message)
{
	parser_print_error_prefix();
	fprintf(stderr, "parse error: %s\n", message);
}

#define expect(expected) \
    if(UNLIKELY(token.type != (expected))) { \
        /*parse_error_expected(NULL, (expected), 0);*/ \
        /*eat_until_semi();*/ \
        return NULL; \
    } \
    next_token();

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

typedef enum {
	TYPE_QUALIFIER_CONST    = 1 << 0,
	TYPE_QUALIFIER_RESTRICT = 1 << 1,
	TYPE_QUALIFIER_VOLATILE = 1 << 2,
	TYPE_QUALIFIER_INLINE   = 1 << 3,
} type_qualifier_t;

typedef enum {
	STORAGE_CLASS_NONE,
	STORAGE_CLASS_TYPEDEF,
	STORAGE_CLASS_EXTERN,
	STORAGE_CLASS_STATIC,
	STORAGE_CLASS_AUTO,
	STORAGE_CLASS_REGISTER
} storage_class_t;

typedef struct declaration_specifiers_t  declaration_specifiers_t;
struct declaration_specifiers_t {
	storage_class_t  storage_class;
	int              type_qualifiers;
};

static
void parse_declaration_specifiers(declaration_specifiers_t *specifiers)
{
	type_type_t        type_type       = TYPE_INVALID;
	atomic_type_type_t atomic_type     = ATOMIC_TYPE_INVALID;
	unsigned           type_specifiers = 0;

	while(1) {
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
			specifiers->type_qualifiers |= qualifier;                   \
			next_token();                                               \
			break;

		MATCH_TYPE_QUALIFIER(T_const,    TYPE_QUALIFIER_CONST);
		MATCH_TYPE_QUALIFIER(T_restrict, TYPE_QUALIFIER_RESTRICT);
		MATCH_TYPE_QUALIFIER(T_volatile, TYPE_QUALIFIER_VOLATILE);
		MATCH_TYPE_QUALIFIER(T_inline,   TYPE_QUALIFIER_INLINE);

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
				parse_error("too many long type specifiers given");
			} else if(type_specifiers & SPECIFIER_LONG) {
				type_specifiers |= SPECIFIER_LONG_LONG;
			} else {
				type_specifiers |= SPECIFIER_LONG;
			}
			break;

		/* struct or union specifier */
		/* enum specifier */
		/* typedef name */

		/* function specifier */
		default:
			return;;
		}
	}

	if(type_type == TYPE_INVALID) {
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
		case SPECIFIER_LONG_LONG:
		case SPECIFIER_SIGNED | SPECIFIER_LONG_LONG:
		case SPECIFIER_LONG_LONG | SPECIFIER_INT:
		case SPECIFIER_SIGNED | SPECIFIER_LONG_LONG | SPECIFIER_INT:
			atomic_type = ATOMIC_TYPE_LONGLONG;
			break;
		case SPECIFIER_UNSIGNED | SPECIFIER_LONG_LONG:
		case SPECIFIER_UNSIGNED | SPECIFIER_LONG_LONG | SPECIFIER_INT:
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
				parse_error("no type specifiers given in declaration");
			} else if((type_specifiers & SPECIFIER_SIGNED) &&
			          (type_specifiers & SPECIFIER_UNSIGNED)) {
				parse_error("signed and unsigned specifiers gives");
			} else if(type_specifiers & (SPECIFIER_SIGNED | SPECIFIER_UNSIGNED)) {
				parse_error("only integer types can be signed or unsigned");
			} else {
				parse_error("multiple datatypes in declaration");
			}
		}
	} else {
		if(type_specifiers != 0) {
			parse_error("multiple datatypes in declaration");
		}
	}
}

typedef struct declarator_t declarator_t;
struct declarator_t {
	/* pointer stuff... */
	symbol_t     *symbol;

	declarator_t *next;
};

declarator_t *parse_declarator()
{
	while(token.type == '*') {
		/* pointer */
		next_token();
		//parse_type_qualifiers();
	}

	declarator_t *declarator;

	switch(token.type) {
	case T_IDENTIFIER:
		declarator = allocate_ast(sizeof(declarator[0]));
		memset(declarator, 0, sizeof(declarator[0]));
		declarator->symbol = token.v.symbol;
		return declarator;
	case '(':
		next_token();
		declarator = parse_declarator();
		expect(')')
		return declarator;
	default:
		parse_error("problem while parsing declarator");
	}

	if(token.type == '(') {
		next_token();

		/* parse parameter-type-list or identifier-list */

		expect(')');
	} else if(token.type == '[') {
		next_token();

		/* multiple type qualifiers, and static */

		/* assignment_expression or '*' or nothing */

		expect(']');
	}

	return declarator;
}

declarator_t *parse_init_declarator()
{
	declarator_t *declarator = parse_declarator();
	if(token.type == '=') {
		next_token();
		//parse_initialize();
	}

	return declarator;
}

typedef struct declaration_t declaration_t;
struct declaration_t {
	declaration_specifiers_t  specifiers;
	declaration_t            *declarators;
};

void parse_declaration()
{
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));
	parse_declaration_specifiers(&specifiers);
}

#if 0
namespace_t *parse(FILE *in, const char *input_name)
{
	namespace_t *namespace = parse_namespace();

	return namespace;
}
#endif
