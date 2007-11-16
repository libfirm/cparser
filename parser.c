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

typedef struct {
	declaration_t *old_declaration;
	symbol_t      *symbol;
	unsigned short namespace;
} stack_entry_t;

static token_t         token;
static token_t         lookahead_buffer[MAX_LOOKAHEAD];
static int             lookahead_bufpos;
static stack_entry_t  *environment_stack = NULL;
static stack_entry_t  *label_stack       = NULL;
static context_t      *global_context    = NULL;
static context_t      *context           = NULL;
static declaration_t  *last_declaration  = NULL;
static declaration_t  *current_function  = NULL;
static struct obstack  temp_obst;
static bool            found_error;

static type_t         *type_int         = NULL;
static type_t         *type_uint        = NULL;
static type_t         *type_long_double = NULL;
static type_t         *type_double      = NULL;
static type_t         *type_float       = NULL;
static type_t         *type_const_char  = NULL;
static type_t         *type_string      = NULL;
static type_t         *type_void        = NULL;
static type_t         *type_size_t      = NULL;
static type_t         *type_ptrdiff_t   = NULL;

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

static inline void free_type(void *type)
{
	obstack_free(type_obst, type);
}

/**
 * returns the top element of the environment stack
 */
static inline size_t environment_top(void)
{
	return ARR_LEN(environment_stack);
}

static inline size_t label_top(void)
{
	return ARR_LEN(label_stack);
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

#define eat(token_type)  do { assert(token.type == token_type); next_token(); } while(0)

static void error(void)
{
	found_error = true;
#ifdef ABORT_ON_ERROR
	abort();
#endif
}

static void parser_print_prefix_pos(const source_position_t source_position)
{
    fputs(source_position.input_name, stderr);
    fputc(':', stderr);
    fprintf(stderr, "%d", source_position.linenr);
    fputs(": ", stderr);
}

static void parser_print_error_prefix_pos(
		const source_position_t source_position)
{
	parser_print_prefix_pos(source_position);
	fputs("error: ", stderr);
	error();
}

static void parser_print_error_prefix(void)
{
	parser_print_error_prefix_pos(token.source_position);
}

static void parse_error(const char *message)
{
	parser_print_error_prefix();
	fprintf(stderr, "parse error: %s\n", message);
}

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

static void type_error(const char *msg, const source_position_t source_position,
                       type_t *type)
{
	parser_print_error_prefix_pos(source_position);
	fprintf(stderr, "%s, but found type ", msg);
	print_type(type);
	fputc('\n', stderr);
	error();
}

static void type_error_incompatible(const char *msg,
		const source_position_t source_position, type_t *type1, type_t *type2)
{
	parser_print_error_prefix_pos(source_position);
	fprintf(stderr, "%s, incompatible types: ", msg);
	print_type(type1);
	fprintf(stderr, " - ");
	print_type(type2);
	fprintf(stderr, ")\n");
	error();
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

	last_declaration = new_context->declarations;
	if(last_declaration != NULL) {
		while(last_declaration->next != NULL) {
			last_declaration = last_declaration->next;
		}
	}
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

static declaration_t *get_declaration(symbol_t *symbol, namespace_t namespace)
{
	declaration_t *declaration = symbol->declaration;
	for( ; declaration != NULL; declaration = declaration->symbol_next) {
		if(declaration->namespace == namespace)
			return declaration;
	}

	return NULL;
}

static const char *get_namespace_prefix(namespace_t namespace)
{
	switch(namespace) {
	case NAMESPACE_NORMAL:
		return "";
	case NAMESPACE_UNION:
		return "union ";
	case NAMESPACE_STRUCT:
		return "struct ";
	case NAMESPACE_ENUM:
		return "enum ";
	case NAMESPACE_LABEL:
		return "label ";
	}
	panic("invalid namespace found");
}

/**
 * pushs an environment_entry on the environment stack and links the
 * corresponding symbol to the new entry
 */
static declaration_t *stack_push(stack_entry_t **stack_ptr,
                                 declaration_t *declaration,
                                 context_t *parent_context)
{
	symbol_t    *symbol    = declaration->symbol;
	namespace_t  namespace = declaration->namespace;

	/* a declaration should be only pushed once */
	assert(declaration->parent_context == NULL);
	declaration->parent_context = parent_context;

	declaration_t *previous_declaration = get_declaration(symbol, namespace);
	assert(declaration != previous_declaration);
	if(previous_declaration != NULL
			&& previous_declaration->parent_context == context) {
		if(!is_compatible_declaration(declaration, previous_declaration)) {
			parser_print_error_prefix_pos(declaration->source_position);
			fprintf(stderr, "definition of symbol %s%s with type ",
					get_namespace_prefix(namespace), symbol->string);
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

	/* remember old declaration */
	stack_entry_t entry;
	entry.symbol          = symbol;
	entry.old_declaration = symbol->declaration;
	entry.namespace       = namespace;
	ARR_APP1(*stack_ptr, entry);

	/* replace/add declaration into declaration list of the symbol */
	if(symbol->declaration == NULL) {
		symbol->declaration = declaration;
	} else {
		declaration_t *iter_last = NULL;
		declaration_t *iter      = symbol->declaration;
		for( ; iter != NULL; iter_last = iter, iter = iter->symbol_next) {
			/* replace an entry? */
			if(iter->namespace == namespace) {
				if(iter_last == NULL) {
					symbol->declaration = declaration;
				} else {
					iter_last->symbol_next = declaration;
				}
				declaration->symbol_next = iter->symbol_next;
				break;
			}
		}
		if(iter == NULL) {
			assert(iter_last->symbol_next == NULL);
			iter_last->symbol_next = declaration;
		}
	}

	return declaration;
}

static declaration_t *environment_push(declaration_t *declaration)
{
	assert(declaration->source_position.input_name != NULL);
	return stack_push(&environment_stack, declaration, context);
}

static declaration_t *label_push(declaration_t *declaration)
{
	return stack_push(&label_stack, declaration, &current_function->context);
}

/**
 * pops symbols from the environment stack until @p new_top is the top element
 */
static void stack_pop_to(stack_entry_t **stack_ptr, size_t new_top)
{
	stack_entry_t *stack = *stack_ptr;
	size_t         top   = ARR_LEN(stack);
	size_t         i;

	assert(new_top <= top);
	if(new_top == top)
		return;

	for(i = top; i > new_top; --i) {
		stack_entry_t *entry = & stack[i - 1];

		declaration_t *old_declaration = entry->old_declaration;
		symbol_t      *symbol          = entry->symbol;
		namespace_t    namespace       = entry->namespace;

		/* replace/remove declaration */
		declaration_t *declaration = symbol->declaration;
		assert(declaration != NULL);
		if(declaration->namespace == namespace) {
			if(old_declaration == NULL) {
				symbol->declaration = declaration->symbol_next;
			} else {
				symbol->declaration = old_declaration;
				assert(old_declaration->symbol_next ==
				       declaration->symbol_next);
			}
		} else {
			for(; declaration != NULL; declaration = declaration->symbol_next) {
				declaration_t *symbol_next = declaration->symbol_next;
				if(symbol_next->namespace == namespace) {
					declaration->symbol_next = old_declaration;
					assert(old_declaration->symbol_next
					        == symbol_next->symbol_next);
					break;
				}
			}
			assert(declaration != NULL);
		}
	}

	ARR_SHRINKLEN(*stack_ptr, (int) new_top);
}

static void environment_pop_to(size_t new_top)
{
	stack_pop_to(&environment_stack, new_top);
}

static void label_pop_to(size_t new_top)
{
	stack_pop_to(&label_stack, new_top);
}


static int get_rank(const type_t *type)
{
	/* The C-standard allows promoting to int or unsigned int (see § 7.2.2
	 * and esp. footnote 108). However we can't fold constants (yet), so we
	 * can't decide wether unsigned int is possible, while int always works.
	 * (unsigned int would be preferable when possible... for stuff like
	 *  struct { enum { ... } bla : 4; } ) */
	if(type->type == TYPE_ENUM)
		return ATOMIC_TYPE_INT;

	assert(type->type == TYPE_ATOMIC);
	atomic_type_t      *atomic_type = (atomic_type_t*) type;
	atomic_type_type_t  atype       = atomic_type->atype;
	return atype;
}

static type_t *promote_integer(type_t *type)
{
	if(get_rank(type) < ATOMIC_TYPE_INT)
		type = type_int;

	return type;
}

static expression_t *create_cast_expression(expression_t *expression,
                                            type_t *dest_type)
{
	unary_expression_t *cast = allocate_ast_zero(sizeof(cast[0]));

	cast->expression.type     = EXPR_UNARY;
	cast->type                = UNEXPR_CAST;
	cast->value               = expression;
	cast->expression.datatype = dest_type;

	return (expression_t*) cast;
}

static expression_t *create_implicit_cast(expression_t *expression,
                                          type_t *dest_type)
{
	type_t *source_type = expression->datatype;

	if(source_type == NULL)
		return expression;

	source_type = skip_typeref(source_type);
	dest_type   = skip_typeref(dest_type);

	if(source_type == dest_type)
		return expression;

	if(dest_type->type == TYPE_ATOMIC) {
		if(source_type->type != TYPE_ATOMIC)
			panic("casting of non-atomic types not implemented yet");

		if(is_type_floating(dest_type) && !is_type_scalar(source_type)) {
			type_error_incompatible("can't cast types",
			                        expression->source_position,
			                        source_type, dest_type);
			return expression;
		}

		return create_cast_expression(expression, dest_type);
	}
	if(dest_type->type == TYPE_POINTER) {
		if(source_type->type == TYPE_POINTER) {
			if(!pointers_compatible(source_type, dest_type)) {
				type_error_incompatible("can't implicitely cast types",
			                        expression->source_position,
			                        source_type, dest_type);
			} else {
				return create_cast_expression(expression, dest_type);
			}
		}
	}

	panic("casting of non-atomic types not implemented yet");
}

static void semantic_assign(type_t *orig_type_left, expression_t **right,
                            const char *context)
{
	type_t *orig_type_right = (*right)->datatype;

	if(orig_type_right == NULL)
		return;

	type_t *type_left       = skip_typeref(orig_type_left);
	type_t *type_right      = skip_typeref(orig_type_right);

	if(type_left == type_right) {
		/* fine */
	} else if(is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		*right = create_implicit_cast(*right, type_left);
	} else if(type_left->type == TYPE_POINTER
			&& type_right->type == TYPE_POINTER) {
		/* TODO */
	} else {
		/* TODO: improve error message */
		parser_print_error_prefix();
		fprintf(stderr, "incompatible types in %s\n", context);
		parser_print_error_prefix();
		print_type(type_left);
		fputs(" <- ", stderr);
		print_type(type_right);
		fputs("\n", stderr);
	}

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
static declaration_t *parse_declarator(storage_class_t storage_class,
		type_t *type, int may_be_abstract);
static declaration_t *record_declaration(declaration_t *declaration);

typedef struct declaration_specifiers_t  declaration_specifiers_t;
struct declaration_specifiers_t {
	storage_class_t  storage_class;
	type_t          *type;
};

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

static designator_t *parse_designation(void)
{
	if(token.type != '[' && token.type != '.')
		return NULL;

	designator_t *result = NULL;
	designator_t *last   = NULL;

	while(1) {
		designator_t *designator;
		switch(token.type) {
		case '[':
			designator = allocate_ast_zero(sizeof(designator[0]));
			next_token();
			designator->array_access = parse_constant_expression();
			expect(']');
			break;
		case '.':
			designator = allocate_ast_zero(sizeof(designator[0]));
			next_token();
			if(token.type != T_IDENTIFIER) {
				parse_error_expected("problem while parsing designator",
				                     T_IDENTIFIER, 0);
				return NULL;
			}
			designator->symbol = token.v.symbol;
			next_token();
			break;
		default:
			expect('=');
			return result;
		}

		assert(designator != NULL);
		if(last != NULL) {
			last->next = designator;
		} else {
			result = designator;
		}
		last = designator;
	}
}

static initializer_t *parse_initializer_list(type_t *type);

static initializer_t *parse_initializer(type_t *type)
{
	designator_t *designator = parse_designation();

	initializer_t *result;
	if(token.type == '{') {
		result = parse_initializer_list(type);
	} else {
		result          = allocate_ast_zero(sizeof(result[0]));
		result->type    = INITIALIZER_VALUE;
		result->v.value = parse_assignment_expression();

		if(type != NULL) {
			semantic_assign(type, &result->v.value, "initializer");
		}
	}
	result->designator = designator;

	return result;
}

static initializer_t *parse_initializer_list(type_t *type)
{
	eat('{');

	/* TODO: semantic */
	(void) type;

	initializer_t *result = allocate_ast_zero(sizeof(result[0]));
	result->type = INITIALIZER_LIST;

	initializer_t *last = NULL;
	while(1) {
		initializer_t *initializer = parse_initializer(NULL);
		if(last != NULL) {
			last->next = initializer;
		} else {
			result->v.list = initializer;
		}
		last = initializer;

		if(token.type == '}')
			break;

		if(token.type != ',') {
			parse_error_expected("problem while parsing initializer list",
			                     ',', '}', 0);
			eat_block();
			return result;
		}
		eat(',');

		if(token.type == '}')
			break;
	}

	expect('}');

	return result;
}

static declaration_t *parse_compound_type_specifier(bool is_struct)
{
	if(is_struct) {
		eat(T_struct);
	} else {
		eat(T_union);
	}

	symbol_t      *symbol      = NULL;
	declaration_t *declaration = NULL;

	if(token.type == T_IDENTIFIER) {
		symbol = token.v.symbol;
		next_token();

		if(is_struct) {
			declaration = get_declaration(symbol, NAMESPACE_STRUCT);
		} else {
			declaration = get_declaration(symbol, NAMESPACE_UNION);
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

	if(declaration == NULL) {
		declaration = allocate_type_zero(sizeof(declaration[0]));

		if(is_struct) {
			declaration->namespace = NAMESPACE_STRUCT;
		} else {
			declaration->namespace = NAMESPACE_UNION;
		}
		declaration->source_position = token.source_position;
		declaration->symbol          = symbol;
	}

	if(token.type == '{') {
		if(declaration->init.is_defined) {
			assert(symbol != NULL);
			parser_print_error_prefix();
			fprintf(stderr, "multiple definition of %s %s\n",
					is_struct ? "struct" : "union", symbol->string);
			declaration->context.declarations = NULL;
		}
		record_declaration(declaration);
		declaration->init.is_defined = true;

		int         top          = environment_top();
		context_t  *last_context = context;
		set_context(& declaration->context);

		parse_compound_type_entries();
		parse_attributes();

		assert(context == & declaration->context);
		set_context(last_context);
		environment_pop_to(top);
	}

	return declaration;
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
			entry->init.initializer = parse_initializer(type_int);
		}

		record_declaration(entry);

		if(token.type != ',')
			break;
		next_token();
	} while(token.type != '}');

	expect_void('}');
}

static declaration_t *parse_enum_specifier(void)
{
	eat(T_enum);

	declaration_t *declaration;
	symbol_t      *symbol;

	if(token.type == T_IDENTIFIER) {
		symbol = token.v.symbol;
		next_token();

		declaration = get_declaration(symbol, NAMESPACE_ENUM);
	} else if(token.type != '{') {
		parse_error_expected("problem while parsing enum type specifier",
		                     T_IDENTIFIER, '{', 0);
		return NULL;
	} else {
		declaration = NULL;
		symbol      = NULL;
	}

	if(declaration == NULL) {
		declaration = allocate_type_zero(sizeof(declaration[0]));

		declaration->namespace       = NAMESPACE_ENUM;
		declaration->source_position = token.source_position;
		declaration->symbol          = symbol;
	}

	if(token.type == '{') {
		if(declaration->init.is_defined) {
			parser_print_error_prefix();
			fprintf(stderr, "multiple definitions of enum %s\n",
			        symbol->string);
		}
		record_declaration(declaration);
		declaration->init.is_defined = 1;

		parse_enum_entries();
		parse_attributes();
	}

	return declaration;
}

/**
 * if a symbol is a typedef to another type, return true
 */
static bool is_typedef_symbol(symbol_t *symbol)
{
	declaration_t *declaration = get_declaration(symbol, NAMESPACE_NORMAL);
	if(declaration == NULL
			|| declaration->storage_class != STORAGE_CLASS_TYPEDEF)
		return false;

	return true;
}

static type_t *parse_typeof(void)
{
	eat(T___typeof__);

	type_t *type;

	expect('(');

	expression_t *expression  = NULL;

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
		if(is_typedef_symbol(token.v.symbol)) {
			type = parse_typename();
		} else {
			expression = parse_expression();
			type       = expression->datatype;
		}
		break;

	TYPENAME_START
		type = parse_typename();
		break;

	default:
		expression = parse_expression();
		type       = expression->datatype;
		break;
	}

	expect(')');

	typeof_type_t *typeof = allocate_type_zero(sizeof(typeof[0]));
	typeof->type.type     = TYPE_TYPEOF;
	typeof->expression    = expression;
	typeof->typeof_type   = type;

	return (type_t*) typeof;
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
	/* TODO... */
	type->real_type      = type_int;

	return (type_t*) type;
}

static type_t *get_typedef_type(symbol_t *symbol)
{
	declaration_t *declaration = get_declaration(symbol, NAMESPACE_NORMAL);
	if(declaration == NULL
			|| declaration->storage_class != STORAGE_CLASS_TYPEDEF)
		return NULL;

	typedef_type_t *typedef_type = allocate_type_zero(sizeof(typedef_type[0]));
	typedef_type->type.type    = TYPE_TYPEDEF;
	typedef_type->declaration  = declaration;

	return (type_t*) typedef_type;
}

static void parse_declaration_specifiers(declaration_specifiers_t *specifiers)
{
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

		/* TODO: if type != NULL for the following rules should issue
		 * an error */
		case T_struct: {
			compound_type_t *compound_type
				= allocate_type_zero(sizeof(compound_type[0]));
			compound_type->type.type = TYPE_COMPOUND_STRUCT;
			compound_type->declaration = parse_compound_type_specifier(true);

			type = (type_t*) compound_type;
			break;
		}
		case T_union: {
			compound_type_t *compound_type
				= allocate_type_zero(sizeof(compound_type[0]));
			compound_type->type.type = TYPE_COMPOUND_UNION;
			compound_type->declaration = parse_compound_type_specifier(false);

			type = (type_t*) compound_type;
			break;
		}
		case T_enum: {
			enum_type_t *enum_type = allocate_type_zero(sizeof(enum_type[0]));
			enum_type->type.type   = TYPE_ENUM;
			enum_type->declaration = parse_enum_specifier();

			type = (type_t*) enum_type;
			break;
		}
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

		case T_IDENTIFIER: {
			type_t *typedef_type = get_typedef_type(token.v.symbol);

			if(typedef_type == NULL)
				goto finish_specifiers;

			next_token();
			type = typedef_type;
			break;
		}

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
		free_type(type);
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

	declaration_t *declaration = parse_declarator(specifiers.storage_class,
	                                              specifiers.type, 1);

	/* TODO check declaration constraints for parameters */
	if(declaration->storage_class == STORAGE_CLASS_TYPEDEF) {
		parse_error("typedef not allowed in parameter list");
	}

	return declaration;
}

static declaration_t *parse_parameters(function_type_t *type)
{
	if(token.type == T_IDENTIFIER) {
		symbol_t      *symbol = token.v.symbol;
		if(!is_typedef_symbol(symbol)) {
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

	declaration_t        *declarations = NULL;
	declaration_t        *declaration;
	declaration_t        *last_declaration = NULL;
	function_parameter_t *parameter;
	function_parameter_t *last_parameter = NULL;

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
	CONSTRUCT_INVALID,
	CONSTRUCT_POINTER,
	CONSTRUCT_FUNCTION,
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

typedef struct construct_function_type_t construct_function_type_t;
struct construct_function_type_t {
	construct_type_t    construct_type;
	function_type_t    *function_type;
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
	pointer->construct_type.type = CONSTRUCT_POINTER;
	pointer->type_qualifiers     = parse_type_qualifiers();

	return (construct_type_t*) pointer;
}

static construct_type_t *parse_array_declarator(void)
{
	eat('[');

	parsed_array_t *array = obstack_alloc(&temp_obst, sizeof(array[0]));
	memset(array, 0, sizeof(array[0]));
	array->construct_type.type = CONSTRUCT_ARRAY;

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

static construct_type_t *parse_function_declarator(declaration_t *declaration)
{
	eat('(');

	function_type_t *type = allocate_type_zero(sizeof(type[0]));
	type->type.type       = TYPE_FUNCTION;

	declaration_t *parameters = parse_parameters(type);
	if(declaration != NULL) {
		declaration->context.declarations = parameters;
	}

	construct_function_type_t *construct_function_type =
		obstack_alloc(&temp_obst, sizeof(construct_function_type[0]));
	memset(construct_function_type, 0, sizeof(construct_function_type[0]));
	construct_function_type->construct_type.type = CONSTRUCT_FUNCTION;
	construct_function_type->function_type       = type;

	expect(')');

	return (construct_type_t*) construct_function_type;
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
			type = parse_function_declarator(declaration);
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
		parsed_pointer_t          *parsed_pointer;
		parsed_array_t            *parsed_array;
		construct_function_type_t *construct_function_type;
		function_type_t           *function_type;
		pointer_type_t            *pointer_type;
		array_type_t              *array_type;

		switch(iter->type) {
		case CONSTRUCT_INVALID:
			panic("invalid type construction found");
		case CONSTRUCT_FUNCTION:
			construct_function_type = (construct_function_type_t*) iter;
			function_type           = construct_function_type->function_type;

			function_type->result_type = type;
			type                       = (type_t*) function_type;
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
			free_type(type);
			type = hashed_type;
		}
	}

	return type;
}

static declaration_t *parse_declarator(storage_class_t storage_class,
		type_t *type, int may_be_abstract)
{
	declaration_t *declaration = allocate_ast_zero(sizeof(declaration[0]));
	declaration->storage_class = storage_class;

	construct_type_t *construct_type
		= parse_inner_declarator(declaration, may_be_abstract);
	declaration->type = construct_declarator_type(construct_type, type);

	if(construct_type != NULL) {
		obstack_free(&temp_obst, construct_type);
	}

	return declaration;
}

static type_t *parse_abstract_declarator(type_t *base_type)
{
	construct_type_t *construct_type = parse_inner_declarator(NULL, 1);

	type_t *result = construct_declarator_type(construct_type, base_type);
	if(construct_type != NULL) {
		obstack_free(&temp_obst, construct_type);
	}

	return result;
}

static declaration_t *record_declaration(declaration_t *declaration)
{
	assert(context != NULL);

	symbol_t *symbol = declaration->symbol;
	if(symbol != NULL) {
		declaration_t *alias = environment_push(declaration);
		if(alias != declaration)
			return alias;
	} else {
		declaration->parent_context = context;
	}

	if(last_declaration != NULL) {
		last_declaration->next = declaration;
	} else {
		context->declarations = declaration;
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
			= parse_declarator(specifiers->storage_class, specifiers->type, 0);

		declaration_t *declaration = record_declaration(ndeclaration);
		if(token.type == '=') {
			next_token();

			/* TODO: check that this is an allowed type (no function type) */

			if(declaration->init.initializer != NULL) {
				parser_error_multiple_definition(declaration, ndeclaration);
			}

			ndeclaration->init.initializer = parse_initializer(declaration->type);
		} else if(token.type == '{') {
			if(declaration->type->type != TYPE_FUNCTION) {
				parser_print_error_prefix();
				fprintf(stderr, "Declarator ");
				print_type_ext(declaration->type, declaration->symbol, NULL);
				fprintf(stderr, " has a body but is not a function type.\n");
				eat_block();
				continue;
			}

			if(declaration->init.statement != NULL) {
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
				environment_push(parameter);
			}

			int            label_stack_top      = label_top();
			declaration_t *old_current_function = current_function;
			current_function                    = declaration;

			statement_t *statement = parse_compound_statement();

			assert(current_function == declaration);
			current_function = old_current_function;
			label_pop_to(label_stack_top);

			assert(context == &declaration->context);
			set_context(last_context);
			environment_pop_to(top);

			declaration->init.statement = statement;
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
				= parse_declarator(specifiers->storage_class,
				                   specifiers->type, 1);

			/* TODO: check constraints for struct declarations */
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
	source_position_t source_position = token.source_position;

	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));
	parse_declaration_specifiers(&specifiers);

	if(token.type == ';') {
		next_token();

		declaration_t *declaration = allocate_ast_zero(sizeof(declaration[0]));

		declaration->type            = specifiers.type;
		declaration->storage_class   = specifiers.storage_class;
		declaration->source_position = source_position;
		record_declaration(declaration);
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
	cnst->expression.datatype = type_double;
	cnst->v.float_value       = token.v.floatvalue;

	next_token();

	return (expression_t*) cnst;
}

static declaration_t *create_implicit_function(symbol_t *symbol,
		const source_position_t source_position)
{
	function_type_t *function_type
		= allocate_type_zero(sizeof(function_type[0]));

	function_type->type.type              = TYPE_FUNCTION;
	function_type->result_type            = type_int;
	function_type->unspecified_parameters = true;

	type_t *type = typehash_insert((type_t*) function_type);
	if(type != (type_t*) function_type) {
		free_type(function_type);
	}

	declaration_t *declaration = allocate_ast_zero(sizeof(declaration[0]));

	declaration->storage_class   = STORAGE_CLASS_EXTERN;
	declaration->type            = type;
	declaration->symbol          = symbol;
	declaration->source_position = source_position;

	/* prepend the implicit definition to the global context
	 * this is safe since the symbol wasn't declared as anything else yet
	 */
	assert(symbol->declaration == NULL);

	context_t *last_context = context;
	context = global_context;

	environment_push(declaration);
	declaration->next     = context->declarations;
	context->declarations = declaration;

	context = last_context;

	return declaration;
}

static expression_t *parse_reference(void)
{
	reference_expression_t *ref = allocate_ast_zero(sizeof(ref[0]));

	ref->expression.type = EXPR_REFERENCE;
	ref->symbol          = token.v.symbol;

	declaration_t *declaration = get_declaration(ref->symbol, NAMESPACE_NORMAL);

	source_position_t source_position = token.source_position;
	next_token();

	if(declaration == NULL) {
#ifndef STRICT_C99
		/* an implicitely defined function */
		if(token.type == '(') {
			parser_print_prefix_pos(token.source_position);
			fprintf(stderr, "warning: implicit declaration of function '%s'\n",
			        ref->symbol->string);

			declaration = create_implicit_function(ref->symbol,
			                                       source_position);
		} else
#endif
		{
			parser_print_error_prefix();
			fprintf(stderr, "unknown symbol '%s' found.\n", ref->symbol->string);
			return (expression_t*) ref;
		}
	}

	ref->declaration         = declaration;
	ref->expression.datatype = declaration->type;

	return (expression_t*) ref;
}

static void check_cast_allowed(expression_t *expression, type_t *dest_type)
{
	(void) expression;
	(void) dest_type;
	/* TODO check if explicit cast is allowed and issue warnings/errors */
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

	statement_t *statement = parse_compound_statement();
	expression->statement  = statement;
	if(statement == NULL) {
		expect(')');
		return NULL;
	}

	assert(statement->type == STATEMENT_COMPOUND);
	compound_statement_t *compound_statement
		= (compound_statement_t*) statement;

	/* find last statement and use it's type */
	const statement_t *last_statement = NULL;
	const statement_t *iter           = compound_statement->statements;
	for( ; iter != NULL; iter = iter->next) {
		last_statement = iter;
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

	switch(token.type) {
	case '{':
		/* gcc extension: a stement expression */
		return parse_statement_expression();

	TYPE_QUALIFIERS
	TYPE_SPECIFIERS
		return parse_cast();
	case T_IDENTIFIER:
		if(is_typedef_symbol(token.v.symbol)) {
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

static designator_t *parse_designator(void)
{
	designator_t *result = allocate_ast_zero(sizeof(result[0]));

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing member designator",
		                     T_IDENTIFIER, 0);
		eat_brace();
		return NULL;
	}
	result->symbol = token.v.symbol;
	next_token();

	designator_t *last_designator = result;
	while(true) {
		if(token.type == '.') {
			next_token();
			if(token.type != T_IDENTIFIER) {
				parse_error_expected("problem while parsing member designator",
					T_IDENTIFIER, 0);
				eat_brace();
				return NULL;
			}
			designator_t *designator = allocate_ast_zero(sizeof(result[0]));
			designator->symbol       = token.v.symbol;
			next_token();

			last_designator->next = designator;
			last_designator       = designator;
			continue;
		}
		if(token.type == '[') {
			next_token();
			designator_t *designator = allocate_ast_zero(sizeof(result[0]));
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
	expression->designator = parse_designator();
	expect(')');

	return (expression_t*) expression;
}

static expression_t *parse_va_arg(void)
{
	eat(T___builtin_va_arg);

	va_arg_expression_t *expression = allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type     = EXPR_VA_ARG;

	expect('(');
	expression->arg = parse_assignment_expression();
	expect(',');
	expression->expression.datatype = parse_typename();
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
	case T___builtin_va_arg:
		return parse_va_arg();
	case T___builtin_expect:
	case T___builtin_va_start:
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

	type_t *type = array_ref->datatype;
	if(type != NULL) {
		if(type->type == TYPE_POINTER) {
			pointer_type_t *pointer           = (pointer_type_t*) type;
			array_access->expression.datatype = pointer->points_to;
		} else if(type->type == TYPE_ARRAY) {
			array_type_t *array_type          = (array_type_t*) type;
			array_access->expression.datatype = array_type->element_type;
		} else {
			parser_print_error_prefix();
			fprintf(stderr, "array access on object with non-pointer type ");
			print_type(type);
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
	switch(token->type) {
		TYPE_SPECIFIERS
			return 1;
		case T_IDENTIFIER:
			return is_typedef_symbol(token->v.symbol);
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
	call->expression.type   = EXPR_CALL;
	call->function          = expression;

	function_type_t *function_type;
	type_t          *type = expression->datatype;
	if(type->type != TYPE_FUNCTION) {
		/* TODO calling pointers to functions is ok */
		parser_print_error_prefix();
		fputs("called object '", stderr);
		print_expression(expression);
		fputs("' (type ", stderr);
		print_type(type);
		fputs("is not a function\n", stderr);

		function_type             = NULL;
		call->expression.datatype = NULL;
	} else {
		function_type             = (function_type_t*) type;
		call->expression.datatype = function_type->result_type;
	}

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

	if(function_type != NULL) {
		function_parameter_t *parameter = function_type->parameters;
		call_argument_t      *argument  = call->arguments;
		for( ; parameter != NULL && argument != NULL;
				parameter = parameter->next, argument = argument->next) {
			type_t *expected_type = parameter->type;
			/* TODO report context in error messages */
			argument->expression = create_implicit_cast(argument->expression,
			                                            expected_type);
		}
		/* too few parameters */
		if(parameter != NULL) {
			parser_print_error_prefix();
			fprintf(stderr, "too few arguments to function '");
			print_expression(expression);
			fprintf(stderr, "'\n");
		} else if(argument != NULL) {
			/* too many parameters */
			if(!function_type->variadic
					&& !function_type->unspecified_parameters) {
				parser_print_error_prefix();
				fprintf(stderr, "too many arguments to function '");
				print_expression(expression);
				fprintf(stderr, "'\n");
			} else {
				/* do default promotion */
				for( ; argument != NULL; argument = argument->next) {
					type_t *type = argument->expression->datatype;

					if(type == NULL)
						continue;

					if(is_type_integer(type)) {
						type = promote_integer(type);
					} else if(type == type_float) {
						type = type_double;
					}
					argument->expression
						= create_implicit_cast(argument->expression, type);
				}
			}
		}
	}

	return (expression_t*) call;
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

	/* 6.5.15.3 */
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

static void semantic_incdec(unary_expression_t *expression)
{
	type_t *orig_type = expression->value->datatype;
	if(orig_type == NULL)
		return;

	type_t *type = skip_typeref(orig_type);
	if(!is_type_arithmetic(type) && type->type != TYPE_POINTER) {
		/* TODO: improve error message */
		parser_print_error_prefix();
		fprintf(stderr, "operation needs an arithmetic or pointer type\n");
		return;
	}

	expression->expression.datatype = orig_type;
}

static void semantic_unexpr_arithmetic(unary_expression_t *expression)
{
	type_t *orig_type = expression->value->datatype;
	if(orig_type == NULL)
		return;

	type_t *type = skip_typeref(orig_type);
	if(!is_type_arithmetic(type)) {
		/* TODO: improve error message */
		parser_print_error_prefix();
		fprintf(stderr, "operation needs an arithmetic type\n");
		return;
	}

	expression->expression.datatype = orig_type;
}

static void semantic_dereference(unary_expression_t *expression)
{
	type_t *orig_type = expression->value->datatype;
	if(orig_type == NULL)
		return;

	type_t *type = skip_typeref(orig_type);
	if(type->type != TYPE_POINTER) {
		/* TODO: improve error message */
		parser_print_error_prefix();
		fprintf(stderr, "operation needs a pointer type\n");
		return;
	}

	pointer_type_t *pointer_type    = (pointer_type_t*) type;
	expression->expression.datatype = pointer_type->points_to;
}

static void semantic_take_addr(unary_expression_t *expression)
{
	type_t *orig_type = expression->value->datatype;
	if(orig_type == NULL)
		return;

	expression->expression.datatype = make_pointer_type(orig_type, 0);
}

#define CREATE_UNARY_EXPRESSION_PARSER(token_type, unexpression_type, sfunc)   \
static expression_t *parse_##unexpression_type(unsigned precedence)            \
{                                                                              \
	eat(token_type);                                                           \
                                                                               \
	unary_expression_t *unary_expression                                       \
		= allocate_ast_zero(sizeof(unary_expression[0]));                      \
	unary_expression->expression.type     = EXPR_UNARY;                        \
	unary_expression->type                = unexpression_type;                 \
	unary_expression->value               = parse_sub_expression(precedence);  \
	                                                                           \
	sfunc(unary_expression);                                                   \
                                                                               \
	return (expression_t*) unary_expression;                                   \
}

CREATE_UNARY_EXPRESSION_PARSER('-', UNEXPR_NEGATE, semantic_unexpr_arithmetic)
CREATE_UNARY_EXPRESSION_PARSER('+', UNEXPR_PLUS,   semantic_unexpr_arithmetic)
CREATE_UNARY_EXPRESSION_PARSER('!', UNEXPR_NOT,    semantic_unexpr_arithmetic)
CREATE_UNARY_EXPRESSION_PARSER('*', UNEXPR_DEREFERENCE, semantic_dereference)
CREATE_UNARY_EXPRESSION_PARSER('&', UNEXPR_TAKE_ADDRESS, semantic_take_addr)
CREATE_UNARY_EXPRESSION_PARSER('~', UNEXPR_BITWISE_NEGATE,
                               semantic_unexpr_arithmetic)
CREATE_UNARY_EXPRESSION_PARSER(T_PLUSPLUS,   UNEXPR_PREFIX_INCREMENT,
                               semantic_incdec)
CREATE_UNARY_EXPRESSION_PARSER(T_MINUSMINUS, UNEXPR_PREFIX_DECREMENT,
                               semantic_incdec)

#define CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(token_type, unexpression_type, \
                                               sfunc)                         \
static expression_t *parse_##unexpression_type(unsigned precedence,           \
                                               expression_t *left)            \
{                                                                             \
	(void) precedence;                                                        \
	eat(token_type);                                                          \
                                                                              \
	unary_expression_t *unary_expression                                      \
		= allocate_ast_zero(sizeof(unary_expression[0]));                     \
	unary_expression->expression.type     = EXPR_UNARY;                       \
	unary_expression->type                = unexpression_type;                \
	unary_expression->value               = left;                             \
	                                                                          \
	sfunc(unary_expression);                                                  \
                                                                              \
	return (expression_t*) unary_expression;                                  \
}

CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(T_PLUSPLUS,   UNEXPR_POSTFIX_INCREMENT,
                                       semantic_incdec)
CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(T_MINUSMINUS, UNEXPR_POSTFIX_DECREMENT,
                                       semantic_incdec)

static type_t *semantic_arithmetic(type_t *type_left, type_t *type_right)
{
	/* TODO: handle complex + imaginary types */

	/* § 6.3.1.8 Usual arithmetic conversions */
	if(type_left == type_long_double || type_right == type_long_double) {
		return type_long_double;
	} else if(type_left == type_double || type_right == type_double) {
		return type_double;
	} else if(type_left == type_float || type_right == type_float) {
		return type_float;
	}

	type_right = promote_integer(type_right);
	type_left  = promote_integer(type_left);

	if(type_left == type_right)
		return type_left;

	bool signed_left  = is_type_signed(type_left);
	bool signed_right = is_type_signed(type_right);
	if(get_rank(type_left) < get_rank(type_right)) {
		if(signed_left == signed_right || !signed_right) {
			return type_right;
		} else {
			return type_left;
		}
	} else {
		if(signed_left == signed_right || !signed_left) {
			return type_left;
		} else {
			return type_right;
		}
	}
}

static void semantic_binexpr_arithmetic(binary_expression_t *expression)
{
	expression_t *left       = expression->left;
	expression_t *right      = expression->right;
	type_t       *orig_type_left  = left->datatype;
	type_t       *orig_type_right = right->datatype;

	if(orig_type_left == NULL || orig_type_right == NULL)
		return;

	type_t *type_left  = skip_typeref(orig_type_left);
	type_t *type_right = skip_typeref(orig_type_right);

	if(!is_type_arithmetic(type_left) || !is_type_arithmetic(type_right)) {
		/* TODO: improve error message */
		parser_print_error_prefix();
		fprintf(stderr, "operation needs arithmetic types\n");
		return;
	}

	type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
	expression->left  = create_implicit_cast(left, arithmetic_type);
	expression->right = create_implicit_cast(right, arithmetic_type);
	expression->expression.datatype = arithmetic_type;
}

static void semantic_add(binary_expression_t *expression)
{
	expression_t *left            = expression->left;
	expression_t *right           = expression->right;
	type_t       *orig_type_left  = left->datatype;
	type_t       *orig_type_right = right->datatype;

	if(orig_type_left == NULL || orig_type_right == NULL)
		return;

	type_t *type_left  = skip_typeref(orig_type_left);
	type_t *type_right = skip_typeref(orig_type_right);

	/* § 5.6.5 */
	if(is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
		expression->left  = create_implicit_cast(left, arithmetic_type);
		expression->right = create_implicit_cast(right, arithmetic_type);
		expression->expression.datatype = arithmetic_type;
		return;
	} else if(type_left->type == TYPE_POINTER && is_type_integer(type_right)) {
		expression->expression.datatype = type_left;
	} else if(type_right->type == TYPE_POINTER && is_type_integer(type_left)) {
		expression->expression.datatype = type_right;
	} else {
		parser_print_error_prefix();
		fprintf(stderr, "invalid operands to binary + (");
		print_type(orig_type_left);
		fprintf(stderr, ", ");
		print_type(orig_type_right);
		fprintf(stderr, ")\n");
	}
}

static void semantic_sub(binary_expression_t *expression)
{
	expression_t *left            = expression->left;
	expression_t *right           = expression->right;
	type_t       *orig_type_left  = left->datatype;
	type_t       *orig_type_right = right->datatype;

	if(orig_type_left == NULL || orig_type_right == NULL)
		return;

	type_t       *type_left       = skip_typeref(orig_type_left);
	type_t       *type_right      = skip_typeref(orig_type_right);

	/* § 5.6.5 */
	if(is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
		expression->left  = create_implicit_cast(left, arithmetic_type);
		expression->right = create_implicit_cast(right, arithmetic_type);
		expression->expression.datatype = arithmetic_type;
		return;
	} else if(type_left->type == TYPE_POINTER && is_type_integer(type_right)) {
		expression->expression.datatype = type_left;
	} else if(type_left->type == TYPE_POINTER &&
			type_right->type == TYPE_POINTER) {
		if(!pointers_compatible(type_left, type_right)) {
			parser_print_error_prefix();
			fprintf(stderr, "pointers to incompatible objects to binary - (");
			print_type(orig_type_left);
			fprintf(stderr, ", ");
			print_type(orig_type_right);
			fprintf(stderr, ")\n");
		} else {
			expression->expression.datatype = type_ptrdiff_t;
		}
	} else {
		parser_print_error_prefix();
		fprintf(stderr, "invalid operands to binary - (");
		print_type(orig_type_left);
		fprintf(stderr, ", ");
		print_type(orig_type_right);
		fprintf(stderr, ")\n");
	}
}

static void semantic_comparison(binary_expression_t *expression)
{
	expression_t *left            = expression->left;
	expression_t *right           = expression->right;
	type_t       *orig_type_left  = left->datatype;
	type_t       *orig_type_right = right->datatype;

	if(orig_type_left == NULL || orig_type_right == NULL)
		return;

	type_t *type_left  = skip_typeref(orig_type_left);
	type_t *type_right = skip_typeref(orig_type_right);

	/* TODO non-arithmetic types */
	if(is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
		expression->left  = create_implicit_cast(left, arithmetic_type);
		expression->right = create_implicit_cast(right, arithmetic_type);
		expression->expression.datatype = arithmetic_type;
	}
	expression->expression.datatype = type_int;
}

static void semantic_arithmetic_assign(binary_expression_t *expression)
{
	expression_t *left            = expression->left;
	expression_t *right           = expression->right;
	type_t       *orig_type_left  = left->datatype;
	type_t       *orig_type_right = right->datatype;

	if(orig_type_left == NULL || orig_type_right == NULL)
		return;

	type_t *type_left  = skip_typeref(orig_type_left);
	type_t *type_right = skip_typeref(orig_type_right);

	if(!is_type_arithmetic(type_left) || !is_type_arithmetic(type_right)) {
		/* TODO: improve error message */
		parser_print_error_prefix();
		fprintf(stderr, "operation needs arithmetic types\n");
		return;
	}

	/* combined instructions are tricky. We can't create an implicit cast on
	 * the left side, because we need the uncasted form for the store.
	 * The ast2firm pass has to know that left_type must be right_type
	 * for the arithmeitc operation and create a cast by itself */
	type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
	expression->right       = create_implicit_cast(right, arithmetic_type);
	expression->expression.datatype = type_left;
}

static void semantic_logical_op(binary_expression_t *expression)
{
	expression_t *left            = expression->left;
	expression_t *right           = expression->right;
	type_t       *orig_type_left  = left->datatype;
	type_t       *orig_type_right = right->datatype;

	if(orig_type_left == NULL || orig_type_right == NULL)
		return;

	type_t *type_left  = skip_typeref(orig_type_left);
	type_t *type_right = skip_typeref(orig_type_right);

	if(!is_type_arithmetic(type_left) || !is_type_arithmetic(type_right)) {
		/* TODO: improve error message */
		parser_print_error_prefix();
		fprintf(stderr, "operation needs arithmetic types\n");
		return;
	}

	expression->expression.datatype = type_int;
}

static void semantic_binexpr_assign(binary_expression_t *expression)
{
	expression_t *left       = expression->left;
	type_t       *type_left  = left->datatype;

	if(type_left != NULL) {
		semantic_assign(type_left, &expression->right, "assignment");
	}

	expression->expression.datatype = type_left;
}

static void semantic_comma(binary_expression_t *expression)
{
	expression->expression.datatype = expression->right->datatype;
}

#define CREATE_BINEXPR_PARSER(token_type, binexpression_type, sfunc, lr) \
static expression_t *parse_##binexpression_type(unsigned precedence,     \
                                                expression_t *left)      \
{                                                                        \
	eat(token_type);                                                     \
                                                                         \
	expression_t *right = parse_sub_expression(precedence + lr);         \
                                                                         \
	binary_expression_t *binexpr                                         \
		= allocate_ast_zero(sizeof(binexpr[0]));                         \
	binexpr->expression.type     = EXPR_BINARY;                          \
	binexpr->type                = binexpression_type;                   \
	binexpr->left                = left;                                 \
	binexpr->right               = right;                                \
	sfunc(binexpr);                                                      \
                                                                         \
	return (expression_t*) binexpr;                                      \
}

CREATE_BINEXPR_PARSER(',', BINEXPR_COMMA,          semantic_comma, 1)
CREATE_BINEXPR_PARSER('*', BINEXPR_MUL,            semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER('/', BINEXPR_DIV,            semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER('%', BINEXPR_MOD,            semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER('+', BINEXPR_ADD,            semantic_add, 1)
CREATE_BINEXPR_PARSER('-', BINEXPR_SUB,            semantic_sub, 1)
CREATE_BINEXPR_PARSER('<', BINEXPR_LESS,           semantic_comparison, 1)
CREATE_BINEXPR_PARSER('>', BINEXPR_GREATER,        semantic_comparison, 1)
CREATE_BINEXPR_PARSER('=', BINEXPR_ASSIGN,         semantic_binexpr_assign, 0)
CREATE_BINEXPR_PARSER(T_EQUALEQUAL, BINEXPR_EQUAL, semantic_comparison, 1)
CREATE_BINEXPR_PARSER(T_EXCLAMATIONMARKEQUAL, BINEXPR_NOTEQUAL,
                      semantic_comparison, 1)
CREATE_BINEXPR_PARSER(T_LESSEQUAL, BINEXPR_LESSEQUAL, semantic_comparison, 1)
CREATE_BINEXPR_PARSER(T_GREATEREQUAL, BINEXPR_GREATEREQUAL,
                      semantic_comparison, 1)
CREATE_BINEXPR_PARSER('&', BINEXPR_BITWISE_AND,    semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER('|', BINEXPR_BITWISE_OR,     semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER('^', BINEXPR_BITWISE_XOR,    semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER(T_ANDAND, BINEXPR_LOGICAL_AND,  semantic_logical_op, 1)
CREATE_BINEXPR_PARSER(T_PIPEPIPE, BINEXPR_LOGICAL_OR, semantic_logical_op, 1)
/* TODO shift has a bit special semantic */
CREATE_BINEXPR_PARSER(T_LESSLESS, BINEXPR_SHIFTLEFT,
                      semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER(T_GREATERGREATER, BINEXPR_SHIFTRIGHT,
                      semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER(T_PLUSEQUAL, BINEXPR_ADD_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_MINUSEQUAL, BINEXPR_SUB_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_ASTERISKEQUAL, BINEXPR_MUL_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_SLASHEQUAL, BINEXPR_DIV_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_PERCENTEQUAL, BINEXPR_MOD_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_LESSLESSEQUAL, BINEXPR_SHIFTLEFT_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_GREATERGREATEREQUAL, BINEXPR_SHIFTRIGHT_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_ANDEQUAL, BINEXPR_BITWISE_AND_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_PIPEEQUAL, BINEXPR_BITWISE_OR_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_CARETEQUAL, BINEXPR_BITWISE_XOR_ASSIGN,
                      semantic_arithmetic_assign, 0)

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



static void register_expression_parser(parse_expression_function parser,
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

static void register_expression_infix_parser(
		parse_expression_infix_function parser, int token_type,
		unsigned precedence)
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

static declaration_t *get_label(symbol_t *symbol)
{
	declaration_t *candidate = get_declaration(symbol, NAMESPACE_LABEL);
	assert(current_function != NULL);
	/* if we found a label in the same function, then we already created the
	 * declaration */
	if(candidate != NULL
			&& candidate->parent_context == &current_function->context) {
		return candidate;
	}

	/* otherwise we need to create a new one */
	declaration_t *declaration = allocate_ast_zero(sizeof(declaration[0]));
	declaration->namespace     = NAMESPACE_LABEL;
	declaration->symbol        = symbol;

	label_push(declaration);

	return declaration;
}

static statement_t *parse_label_statement(void)
{
	assert(token.type == T_IDENTIFIER);
	symbol_t *symbol = token.v.symbol;
	next_token();

	declaration_t *label = get_label(symbol);

	/* if source position is already set then the label is defined twice,
	 * otherwise it was just mentioned in a goto so far */
	if(label->source_position.input_name != NULL) {
		parser_print_error_prefix();
		fprintf(stderr, "duplicate label '%s'\n", symbol->string);
		parser_print_error_prefix_pos(label->source_position);
		fprintf(stderr, "previous definition of '%s' was here\n",
		        symbol->string);
	} else {
		label->source_position = token.source_position;
	}

	label_statement_t *label_statement = allocate_ast_zero(sizeof(label[0]));

	label_statement->statement.type            = STATEMENT_LABEL;
	label_statement->statement.source_position = token.source_position;
	label_statement->label                     = label;

	expect(':');

	if(token.type == '}') {
		parse_error("label at end of compound statement");
		return (statement_t*) label_statement;
	} else {
		label_statement->label_statement = parse_statement();
	}

	return (statement_t*) label_statement;
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

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("while parsing goto", T_IDENTIFIER, 0);
		eat_statement();
		return NULL;
	}
	symbol_t *symbol = token.v.symbol;
	next_token();

	declaration_t *label = get_label(symbol);

	goto_statement_t *statement = allocate_ast_zero(sizeof(statement[0]));

	statement->statement.type            = STATEMENT_GOTO;
	statement->statement.source_position = token.source_position;

	statement->label = label;

	expect(';');

	return (statement_t*) statement;
}

static statement_t *parse_continue(void)
{
	eat(T_continue);
	expect(';');

	statement_t *statement     = allocate_ast_zero(sizeof(statement[0]));
	statement->type            = STATEMENT_CONTINUE;
	statement->source_position = token.source_position;

	return statement;
}

static statement_t *parse_break(void)
{
	eat(T_break);
	expect(';');

	statement_t *statement     = allocate_ast_zero(sizeof(statement[0]));
	statement->type            = STATEMENT_BREAK;
	statement->source_position = token.source_position;

	return statement;
}

static statement_t *parse_return(void)
{
	eat(T_return);

	return_statement_t *statement = allocate_ast_zero(sizeof(statement[0]));

	statement->statement.type            = STATEMENT_RETURN;
	statement->statement.source_position = token.source_position;

	assert(current_function->type->type == TYPE_FUNCTION);
	function_type_t *function_type = (function_type_t*) current_function->type;
	type_t          *return_type   = function_type->result_type;

	expression_t *return_value;
	if(token.type != ';') {
		return_value = parse_expression();

		if(return_type == type_void && return_value->datatype != type_void) {
			parse_warning("'return' with a value, in function returning void");
			return_value = NULL;
		} else {
			if(return_type != NULL) {
				semantic_assign(return_type, &return_value, "'return'");
			}
		}
	} else {
		return_value = NULL;
		if(return_type != type_void) {
			parse_warning("'return' without value, in function returning "
			              "non-void");
		}
	}
	statement->return_value = return_value;

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

		if(is_typedef_symbol(token.v.symbol)) {
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
	compound_statement_t *compound_statement
		= allocate_ast_zero(sizeof(compound_statement[0]));
	compound_statement->statement.type            = STATEMENT_COMPOUND;
	compound_statement->statement.source_position = token.source_position;

	eat('{');

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

	if(token.type != '}') {
		parser_print_error_prefix_pos(
				compound_statement->statement.source_position);
		fprintf(stderr, "end of file while looking for closing '}'\n");
	}
	next_token();

	assert(context == &compound_statement->context);
	set_context(last_context);
	environment_pop_to(top);

	return (statement_t*) compound_statement;
}

static translation_unit_t *parse_translation_unit(void)
{
	translation_unit_t *unit = allocate_ast_zero(sizeof(unit[0]));

	assert(global_context == NULL);
	global_context = &unit->context;

	assert(context == NULL);
	set_context(&unit->context);

	while(token.type != T_EOF) {
		parse_declaration();
	}

	assert(context == &unit->context);
	context          = NULL;
	last_declaration = NULL;

	assert(global_context == &unit->context);
	global_context = NULL;

	return unit;
}

translation_unit_t *parse(void)
{
	environment_stack = NEW_ARR_F(stack_entry_t, 0);
	label_stack       = NEW_ARR_F(stack_entry_t, 0);
	found_error       = false;

	type_set_output(stderr);
	ast_set_output(stderr);

	lookahead_bufpos = 0;
	for(int i = 0; i < MAX_LOOKAHEAD + 2; ++i) {
		next_token();
	}
	translation_unit_t *unit = parse_translation_unit();

	DEL_ARR_F(environment_stack);
	DEL_ARR_F(label_stack);

	if(found_error)
		return NULL;

	return unit;
}

void init_parser(void)
{
	init_expression_parsers();
	obstack_init(&temp_obst);

	type_int         = make_atomic_type(ATOMIC_TYPE_INT, 0);
	type_uint        = make_atomic_type(ATOMIC_TYPE_UINT, 0);
	type_long_double = make_atomic_type(ATOMIC_TYPE_LONG_DOUBLE, 0);
	type_double      = make_atomic_type(ATOMIC_TYPE_DOUBLE, 0);
	type_float       = make_atomic_type(ATOMIC_TYPE_FLOAT, 0);
	type_size_t      = make_atomic_type(ATOMIC_TYPE_ULONG, 0);
	type_ptrdiff_t   = make_atomic_type(ATOMIC_TYPE_LONG, 0);
	type_const_char  = make_atomic_type(ATOMIC_TYPE_CHAR, TYPE_QUALIFIER_CONST);
	type_void        = make_atomic_type(ATOMIC_TYPE_VOID, 0);
	type_string      = make_pointer_type(type_const_char, 0);
}

void exit_parser(void)
{
	obstack_free(&temp_obst, NULL);
}
