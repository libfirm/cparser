#include <config.h>

#include <assert.h>
#include <stdarg.h>

#include "lexer_t.h"
#include "token_t.h"
#include "type_t.h"
#include "type_hash.h"
#include "ast_t.h"
#include "adt/bitfiddle.h"
#include "adt/error.h"

#define PRINT_TOKENS

static token_t token;

static inline
void *allocate_ast_zero(size_t size)
{
	void *res = allocate_ast(size);
	memset(res, 0, size);
	return res;
}

static inline
void next_token(void)
{
	lexer_next_token(&token);

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

void parser_print_error_prefix(void)
{
    fputs(source_position.input_name, stderr);
    fputc(':', stderr);
    fprintf(stderr, "%d", source_position.linenr);
    fputs(": error: ", stderr);
}

static
void parse_error(const char *message)
{
	parser_print_error_prefix();
	fprintf(stderr, "parse error: %s\n", message);
}

static
void parse_error_expected(const char *message, ...)
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

static
void eat_until_semi(void)
{
	while(token.type != ';') {
		next_token();
		if(token.type == T_EOF)
			return;
	}
	next_token();
}

#define expect(expected)                           \
    if(UNLIKELY(token.type != (expected))) {       \
        parse_error_expected(NULL, (expected), 0); \
        eat_until_semi();                          \
        return NULL;                               \
    }                                              \
    next_token();

#define expect_void(expected)                      \
    if(UNLIKELY(token.type != (expected))) {       \
        parse_error_expected(NULL, (expected), 0); \
        eat_until_semi();                          \
        return;                                    \
    }                                              \
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
	type_t          *type;
};

static
void parse_declaration_specifiers(declaration_specifiers_t *specifiers)
{
	type_type_t        type_type       = TYPE_INVALID;
	atomic_type_type_t atomic_type     = ATOMIC_TYPE_INVALID;
	unsigned           type_qualifiers = 0;
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
			type_qualifiers |= qualifier;                               \
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

		case T_struct:
		case T_enum:
			/* TODO */
			assert(0);
			break;

		/* function specifier */
		default:
			goto finish_specifiers;
		}
	}

finish_specifiers:
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
			atomic_type = ATOMIC_TYPE_INVALID;
		}

		atomic_type_t *type = obstack_alloc(type_obst, sizeof(type[0]));
		memset(type, 0, sizeof(type[0]));
		type->type.type       = TYPE_ATOMIC;
		type->type.qualifiers = type_qualifiers;
		type->atype           = atomic_type;

		type_t *result = typehash_insert((type_t*) type);
		if(result != (type_t*) type) {
			obstack_free(type_obst, type);
		}

		specifiers->type = result;

		fprintf(stderr, "Specifiers type: ");
		print_type(stderr, result);
		fprintf(stderr, "\n");
	} else {
		if(type_specifiers != 0) {
			parse_error("multiple datatypes in declaration");
		}
	}
}

static
unsigned parse_type_qualifiers()
{
	unsigned type_qualifiers = 0;

	while(1) {
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

static
void parse_declarator(const declaration_specifiers_t *specifiers)
{
	type_t *type = specifiers->type;

	while(token.type == '*') {
		/* pointer */
		next_token();

		pointer_type_t *pointer_type
			= obstack_alloc(type_obst, sizeof(pointer_type[0]));
		memset(pointer_type, 0, sizeof(pointer_type[0]));
		pointer_type->type.type = TYPE_POINTER;
		pointer_type->points_to = type;

		pointer_type->type.qualifiers = parse_type_qualifiers();

		type_t *result = typehash_insert((type_t*) pointer_type);
		if(result != (type_t*) pointer_type) {
			obstack_free(type_obst, pointer_type);
		}

		type = result;
	}

	switch(token.type) {
	case T_IDENTIFIER:
		(void) token.v.symbol;
		next_token();
		break;
	case '(':
		next_token();
		parse_declarator(specifiers);
		expect_void(')');
		return;
	default:
		parse_error("problem while parsing declarator");
	}

	if(token.type == '(') {
		next_token();

		/* parse parameter-type-list or identifier-list */

		expect_void(')');
	} else if(token.type == '[') {
		next_token();

		/* multiple type qualifiers, and static */

		/* assignment_expression or '*' or nothing */

		expect_void(']');
	}

	fprintf(stderr, "Declarator type: ");
	print_type(stderr, type);
	fprintf(stderr, "\n");
}

void parse_init_declarators(const declaration_specifiers_t *specifiers)
{
	parse_declarator(specifiers);
	if(token.type == '=') {
		next_token();
		//parse_initialize();
	}
}

void parse_declaration(void)
{
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));
	parse_declaration_specifiers(&specifiers);

	parse_init_declarators(&specifiers);
}



static
expression_t *parse_sub_expression(unsigned precedence);
static
expression_t *parse_expression(void);

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

static
expression_t *expected_expression_error(void)
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

static
expression_t *parse_string_const(void)
{
	string_literal_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_STRING_LITERAL;
	cnst->value           = token.v.string;

	next_token();

	return (expression_t*) cnst;
}

static
expression_t *parse_int_const(void)
{
	const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_CONST;
	cnst->value           = token.v.intvalue;

	next_token();

	return (expression_t*) cnst;
}

static
expression_t *parse_reference(void)
{
	reference_expression_t *ref = allocate_ast_zero(sizeof(ref[0]));

	ref->expression.type            = EXPR_REFERENCE;
	ref->symbol                     = token.v.symbol;

	next_token();

	return (expression_t*) ref;
}

static
expression_t *parse_brace_expression(void)
{
	eat('(');

	expression_t *result = parse_expression();
	expect(')');

	return result;
}

static
expression_t *parse_primary_expression(void)
{
	switch(token.type) {
	case T_INTEGER:
		return parse_int_const();
	case T_STRING_LITERAL:
		return parse_string_const();
	case T_IDENTIFIER:
		return parse_reference();
	case '(':
		return parse_brace_expression();
	}

	/* TODO: error message */
	return NULL;
}

static
expression_t *parse_array_expression(unsigned precedence,
                                     expression_t *array_ref)
{
	(void) precedence;

	eat('[');

	array_access_expression_t *array_access
		= allocate_ast_zero(sizeof(array_access[0]));

	array_access->expression.type = EXPR_ARRAY_ACCESS;
	array_access->array_ref       = array_ref;
	array_access->index           = parse_expression();

	if(token.type != ']') {
		parse_error_expected("Problem while parsing array access", ']', 0);
		return NULL;
	}
	next_token();

	return (expression_t*) array_access;
}

static
expression_t *parse_sizeof(unsigned precedence)
{
	(void) precedence;
	eat(T_sizeof);
	/* TODO... */

	return NULL;
}

static
expression_t *parse_select_expression(unsigned precedence,
                                      expression_t *compound)
{
	(void) precedence;

	assert(token.type == '.' || token.type == T_SELECT);
	next_token();

	select_expression_t *select = allocate_ast_zero(sizeof(select[0]));

	select->expression.type = EXPR_SELECT;
	select->compound        = compound;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing compound select",
		                     T_IDENTIFIER, 0);
		return NULL;
	}
	select->symbol = token.v.symbol;
	next_token();

	return (expression_t*) select;
}

static
expression_t *parse_call_expression(unsigned precedence,
                                    expression_t *expression)
{
	(void) precedence;
	call_expression_t *call = allocate_ast_zero(sizeof(call[0]));

	call->expression.type            = EXPR_CALL;
	call->method                     = expression;

	/* parse arguments */
	eat('(');

	if(token.type != ')') {
		call_argument_t *last_argument = NULL;

		while(1) {
			call_argument_t *argument = allocate_ast_zero(sizeof(argument[0]));

			argument->expression = parse_expression();
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

	return (expression_t*) call;
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

CREATE_UNARY_EXPRESSION_PARSER('-', UNEXPR_NEGATE);
CREATE_UNARY_EXPRESSION_PARSER('+', UNEXPR_PLUS);
CREATE_UNARY_EXPRESSION_PARSER('!', UNEXPR_NOT);
CREATE_UNARY_EXPRESSION_PARSER('*', UNEXPR_DEREFERENCE);
CREATE_UNARY_EXPRESSION_PARSER('&', UNEXPR_TAKE_ADDRESS);
CREATE_UNARY_EXPRESSION_PARSER('~', UNEXPR_BITWISE_NEGATE);
CREATE_UNARY_EXPRESSION_PARSER(T_PLUSPLUS,   UNEXPR_PREFIX_INCREMENT);
CREATE_UNARY_EXPRESSION_PARSER(T_MINUSMINUS, UNEXPR_PREFIX_DECREMENT);

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

CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(T_PLUSPLUS,   UNEXPR_POSTFIX_INCREMENT);
CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(T_MINUSMINUS, UNEXPR_POSTFIX_DECREMENT);

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
	binexpr->expression.type            = EXPR_BINARY;           \
	binexpr->type                       = binexpression_type;    \
	binexpr->left                       = left;                  \
	binexpr->right                      = right;                 \
                                                                 \
	return (expression_t*) binexpr;                              \
}

CREATE_BINEXPR_PARSER('*', BINEXPR_MUL);
CREATE_BINEXPR_PARSER('/', BINEXPR_DIV);
CREATE_BINEXPR_PARSER('+', BINEXPR_ADD);
CREATE_BINEXPR_PARSER('-', BINEXPR_SUB);
CREATE_BINEXPR_PARSER('<', BINEXPR_LESS);
CREATE_BINEXPR_PARSER('>', BINEXPR_GREATER);
CREATE_BINEXPR_PARSER('=', BINEXPR_ASSIGN);
CREATE_BINEXPR_PARSER(T_EQUALEQUAL, BINEXPR_EQUAL);
CREATE_BINEXPR_PARSER(T_SLASHEQUAL, BINEXPR_NOTEQUAL);
CREATE_BINEXPR_PARSER(T_LESSEQUAL, BINEXPR_LESSEQUAL);
CREATE_BINEXPR_PARSER(T_GREATEREQUAL, BINEXPR_GREATEREQUAL);
CREATE_BINEXPR_PARSER('&', BINEXPR_BITWISE_AND);
CREATE_BINEXPR_PARSER('|', BINEXPR_BITWISE_OR);
CREATE_BINEXPR_PARSER('^', BINEXPR_BITWISE_XOR);
CREATE_BINEXPR_PARSER(T_LESSLESS, BINEXPR_SHIFTLEFT);
CREATE_BINEXPR_PARSER(T_GREATERGREATER, BINEXPR_SHIFTRIGHT);

static
expression_t *parse_sub_expression(unsigned precedence)
{
	if(token.type < 0) {
		return expected_expression_error();
	}

	expression_parser_function_t *parser
		= &expression_parsers[token.type];
	source_position_t             source_position = source_position;
	expression_t                 *left;

	if(parser->parser != NULL) {
		left = parser->parser(parser->precedence);
	} else {
		left = parse_primary_expression();
	}
	if(left != NULL)
		left->source_position = source_position;

	while(1) {
		if(token.type < 0) {
			return expected_expression_error();
		}

		parser = &expression_parsers[token.type];
		if(parser->infix_parser == NULL)
			break;
		if(parser->infix_precedence < precedence)
			break;

		left = parser->infix_parser(parser->infix_precedence, left);
		if(left != NULL)
			left->source_position = source_position;
	}

	return left;
}

static
expression_t *parse_expression(void)
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

static
void init_expression_parsers(void)
{
	memset(&expression_parsers, 0, sizeof(expression_parsers));

	register_expression_infix_parser(parse_BINEXPR_MUL,       '*', 16);
	register_expression_infix_parser(parse_BINEXPR_DIV,       '/', 16);
	register_expression_infix_parser(parse_BINEXPR_SHIFTLEFT,
	                           T_LESSLESS, 16);
	register_expression_infix_parser(parse_BINEXPR_SHIFTRIGHT,
	                           T_GREATERGREATER, 16);
	register_expression_infix_parser(parse_BINEXPR_ADD,       '+', 15);
	register_expression_infix_parser(parse_BINEXPR_SUB,       '-', 15);
	register_expression_infix_parser(parse_BINEXPR_LESS,      '<', 14);
	register_expression_infix_parser(parse_BINEXPR_GREATER,   '>', 14);
	register_expression_infix_parser(parse_BINEXPR_LESSEQUAL, T_LESSEQUAL, 14);
	register_expression_infix_parser(parse_BINEXPR_GREATEREQUAL,
	                           T_GREATEREQUAL, 14);
	register_expression_infix_parser(parse_BINEXPR_EQUAL,     T_EQUALEQUAL, 13);
	register_expression_infix_parser(parse_BINEXPR_NOTEQUAL,
	                                 T_EXCLAMATIONMARKEQUAL, 13);
	register_expression_infix_parser(parse_BINEXPR_BITWISE_AND, '&',        12);
	register_expression_infix_parser(parse_BINEXPR_BITWISE_XOR, '^',        11);
	register_expression_infix_parser(parse_BINEXPR_BITWISE_OR,  '|',        10);
	register_expression_infix_parser(parse_BINEXPR_ASSIGN,      T_EQUAL,     2);

	register_expression_infix_parser(parse_array_expression,        '[',    30);
	register_expression_infix_parser(parse_call_expression,         '(',    30);
	register_expression_infix_parser(parse_select_expression,       '.',    30);
	register_expression_infix_parser(parse_select_expression,  T_SELECT,    30);
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
}


static
statement_t *parse_compound_statement(void);

static
statement_t *parse_statement(void);

static
statement_t *parse_case_statement(void)
{
	eat(T_case);
	parse_expression();
	expect(':');
	parse_statement();

	return NULL;
}

static
statement_t *parse_default_statement(void)
{
	eat(T_default);
	expect(':');
	parse_statement();

	return NULL;
}

static
statement_t *parse_label_statement(void)
{
	eat(T_IDENTIFIER);
	expect(';');
	parse_statement();

	return NULL;
}

static
statement_t *parse_if(void)
{
	eat(T_if);
	expect('(');
	parse_expression();
	expect(')');

	parse_statement();
	if(token.type == T_else) {
		next_token();
		parse_statement();
	}

	return NULL;
}

static
statement_t *parse_switch(void)
{
	eat(T_switch);
	expect('(');
	parse_expression();
	expect(')');
	parse_statement();

	return NULL;
}

static
statement_t *parse_while(void)
{
	eat(T_while);
	expect('(');
	parse_expression();
	expect(')');
	parse_statement();

	return NULL;
}

static
statement_t *parse_do(void)
{
	eat(T_do);
	parse_statement();
	expect(T_while);
	expect('(');
	parse_expression();
	expect(')');

	return NULL;
}

static
statement_t *parse_for(void)
{
	eat(T_for);
	expect('(');
	if(token.type != ';') {
		/* TODO not correct... this could also be a declaration */
		parse_expression();
	}
	expect(';');
	if(token.type != ';') {
		parse_expression();
	}
	expect(';');
	if(token.type != ')') {
		parse_expression();
	}
	expect(')');
	parse_statement();

	return NULL;
}

static
statement_t *parse_goto(void)
{
	eat(T_goto);
	expect(T_IDENTIFIER);
	expect(';');

	return NULL;
}

static
statement_t *parse_continue(void)
{
	eat(T_continue);
	expect(';');

	return NULL;
}

static
statement_t *parse_break(void)
{
	eat(T_break);
	expect(';');

	return NULL;
}

static
statement_t *parse_return(void)
{
	eat(T_return);
	parse_expression();
	expect(';');

	return NULL;
}

static
statement_t *parse_statement(void)
{
	statement_t *statement = NULL;

	/* declaration or statement */
	switch(token.type) {
	case T_case:
		statement = parse_case_statement();
		break;

	case T_default:
		statement = parse_default_statement();
		break;

	case T_IDENTIFIER:
		statement = parse_label_statement();
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
		statement = NULL;
		break;
	}

	return statement;
}

static
statement_t *parse_compound_statement(void)
{
	expect('{');

	while(token.type != '}') {
		parse_statement();
	}
	next_token();

	return NULL;
}

static
void parse_translation_unit(void)
{
	/*
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));
	parse_declaration_specifiers(&specifiers);
	*/

	while(token.type != T_EOF) {
		if(token.type == '{') {
			next_token();
			continue;
		}

		parse_declaration();
		/* multiple declarations? */

		if(token.type == '{') {
			parse_compound_statement();
		} else if(token.type == ';') {
			next_token();
		} else {
			parse_error_expected("while parsing declarations", '{', ';', 0);
		}
	}
}

void parse(void)
{
	next_token();
	parse_translation_unit();
}

void init_parser(void)
{
	init_expression_parsers();
}

void exit_parser(void)
{
}
