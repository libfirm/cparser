/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "parser_t.h"

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>

#include "adt/array.h"
#include "adt/panic.h"
#include "adt/strutil.h"
#include "adt/util.h"
#include "ast/ast_t.h"
#include "ast/attribute_t.h"
#include "ast/constfoldbits.h"
#include "ast/constfold.h"
#include "ast/dialect.h"
#include "ast/printer.h"
#include "ast/string_hash.h"
#include "ast/symbol_table.h"
#include "ast/symbol_t.h"
#include "ast/type_hash.h"
#include "ast/types.h"
#include "ast/type_t.h"
#include "ast/walk.h"
#include "builtins.h"
#include "driver/diagnostic.h"
#include "driver/warning.h"
#include "format_check.h"
#include "preprocessor.h"
#include "token_t.h"

#define MAX_LOOKAHEAD 1

typedef struct {
	entity_t           *old_entity;
	symbol_t           *symbol;
	entity_namespace_t  namespc;
} stack_entry_t;

typedef struct declaration_specifiers_t  declaration_specifiers_t;
struct declaration_specifiers_t {
	position_t      pos;
	storage_class_t storage_class;
	bool            is_inline    : 1;
	bool            thread_local : 1;
	attribute_t    *attributes;        /**< list of attributes */
	type_t         *type;
};

/**
 * An environment for parsing initializers (and compound literals).
 */
typedef struct parse_initializer_env_t {
	type_t     *type;   /**< the type of the initializer. In case of an
	                         array type with unspecified size this gets
	                         adjusted to the actual size. */
	entity_t   *entity; /**< the variable that is initialized if any */
	position_t  pos;
	bool        must_be_constant;
} parse_initializer_env_t;

typedef entity_t* (*parsed_declaration_func) (entity_t *declaration,
                                              bool is_definition);

/** The current token. */
static token_t              token;
/** The lookahead ring-buffer. */
static token_t              lookahead_buffer[MAX_LOOKAHEAD];
/** Position of the next token in the lookahead buffer. */
static size_t               lookahead_bufpos;
static stack_entry_t       *environment_stack = NULL;
static stack_entry_t       *label_stack       = NULL;
static scope_t             *file_scope        = NULL;
static scope_t             *current_scope     = NULL;
/** Point to the current function declaration if inside a function. */
static function_t          *current_function  = NULL;
static entity_t            *current_entity    = NULL;
static switch_statement_t  *current_switch    = NULL;
static statement_t         *current_loop      = NULL;
static statement_t         *current_parent    = NULL;
static linkage_kind_t       current_linkage;
static label_t             *label_first       = NULL;
static label_t            **label_anchor      = NULL;
/** current translation unit. */
static translation_unit_t  *unit              = NULL;
/** true if we are in an __extension__ context. */
static bool                 in_gcc_extension  = false;
static symbol_t            *symbol_main;
static struct obstack       temp_obst;
static entity_t            *anonymous_entity;
static declaration_t      **incomplete_arrays;
static elf_visibility_t     default_visibility = ELF_VISIBILITY_DEFAULT;
static const string_t      *string_true;
static const string_t      *string_false;

#define PUSH_CURRENT_ENTITY(entity) \
	entity_t *const new_current_entity = (entity); \
	entity_t *const old_current_entity = current_entity; \
	((void)(current_entity = new_current_entity))
#define POP_CURRENT_ENTITY() (assert(current_entity == new_current_entity), (void)(current_entity = old_current_entity))

#define PUSH_PARENT(stmt) \
	statement_t *const new_parent = (stmt); \
	statement_t *const old_parent = current_parent; \
	((void)(current_parent = new_parent))
#define POP_PARENT() (assert(current_parent == new_parent), (void)(current_parent = old_parent))

#define PUSH_SCOPE(scope) \
	size_t   const top       = environment_top(); \
	scope_t *const new_scope = (scope); \
	scope_t *const old_scope = (new_scope ? scope_push(new_scope) : NULL)
#define PUSH_SCOPE_STATEMENT(scope) PUSH_SCOPE(dialect.c99 || dialect.cpp ? (scope) : NULL)
#define POP_SCOPE() (new_scope ? assert(current_scope == new_scope), scope_pop(old_scope), environment_pop_to(top) : (void)0)

#define PUSH_EXTENSION() \
	(void)0; \
	bool const old_gcc_extension = in_gcc_extension; \
	while (accept(T___extension__)) { \
		in_gcc_extension = true; \
	} \
	do {} while (0)
#define POP_EXTENSION() \
	((void)(in_gcc_extension = old_gcc_extension))

/** The token anchor set */
static unsigned short token_anchor_set[T_LAST_TOKEN];

/** The current source position. */
#define HERE (&token.base.pos)

/** true if we are in GCC mode. */
#define GNU_MODE (dialect.gnu || in_gcc_extension)

static statement_t *parse_compound_statement(bool inside_expression_statement);
static statement_t *parse_statement(void);

static expression_t *parse_subexpression(precedence_t);
static expression_t *parse_expression(void);
static type_t       *parse_typename(void);
static void          parse_externals(void);
static void          parse_external(void);

static void parse_compound_type_entries(compound_t *compound_declaration);

static void check_call_argument(type_t *expected_type,
                                call_argument_t *argument, unsigned pos);

typedef enum declarator_flags_t {
	DECL_FLAGS_NONE             = 0,
	DECL_MAY_BE_ABSTRACT        = 1U << 0,
	DECL_CREATE_COMPOUND_MEMBER = 1U << 1,
	DECL_IS_PARAMETER           = 1U << 2
} declarator_flags_t;

static entity_t *parse_declarator(const declaration_specifiers_t *specifiers,
                                  declarator_flags_t flags);

static void semantic_comparison(binary_expression_t *expression,
                                bool is_relational);

#define STORAGE_CLASSES \
	     STORAGE_CLASSES_NO_EXTERN: \
	case T_extern

#define STORAGE_CLASSES_NO_EXTERN \
	     T_typedef:         \
	case T_static:          \
	case T_auto:            \
	case T_register:        \
	case T__Thread_local

#define TYPE_QUALIFIERS_NO_ATTRIBUTE \
	     T_const:           \
	case T_restrict:        \
	case T_volatile:        \
	case T_inline:          \
	case T__forceinline

#define TYPE_QUALIFIERS                \
	     TYPE_QUALIFIERS_NO_ATTRIBUTE: \
	case T___attribute__

#define TYPE_SPECIFIERS       \
	     T__Bool:             \
	case T___builtin_va_list: \
	case T___typeof__:        \
	case T__declspec:         \
	case T_bool:              \
	case T_char:              \
	case T_double:            \
	case T_enum:              \
	case T_float:             \
	case T_int:               \
	case T_long:              \
	case T_short:             \
	case T_signed:            \
	case T_struct:            \
	case T_union:             \
	case T_unsigned:          \
	case T_void:              \
	case T_wchar_t:           \
	case T__int8:             \
	case T__int16:            \
	case T__int32:            \
	case T__int64:            \
	case T__Complex:          \
	case T__Imaginary

#define DECLARATION_SPECIFIERS \
	     STORAGE_CLASSES: \
	case TYPE_QUALIFIERS: \
	case TYPE_SPECIFIERS

#define DECLARATION_START \
		DECLARATION_SPECIFIERS: \
	case T__Static_assert

#define DECLARATION_START_NO_EXTERN \
	     STORAGE_CLASSES_NO_EXTERN: \
	case TYPE_QUALIFIERS: \
	case TYPE_SPECIFIERS: \
	case T__Static_assert

#define DECLARATION_START_NO_ATTRIBUTE \
	     STORAGE_CLASSES:              \
	case TYPE_QUALIFIERS_NO_ATTRIBUTE: \
	case TYPE_SPECIFIERS:              \
	case T__Static_assert

#define EXPRESSION_START              \
	     '!':                         \
	case '&':                         \
	case '(':                         \
	case '*':                         \
	case '+':                         \
	case '-':                         \
	case '~':                         \
	case T_ANDAND:                    \
	case T_CHARACTER_CONSTANT:        \
	case T_NUMBER:                    \
	case T_MINUSMINUS:                \
	case T_PLUSPLUS:                  \
	case T_STRING_LITERAL:            \
	case T__Alignof:                  \
	case T___FUNCDNAME__:             \
	case T___FUNCSIG__:               \
	case T___PRETTY_FUNCTION__:       \
	case T___builtin_classify_type:   \
	case T___builtin_constant_p:      \
	case T___builtin_isgreater:       \
	case T___builtin_isgreaterequal:  \
	case T___builtin_isless:          \
	case T___builtin_islessequal:     \
	case T___builtin_islessgreater:   \
	case T___builtin_isunordered:     \
	case T___builtin_offsetof:        \
	case T___builtin_va_arg:          \
	case T___builtin_va_copy:         \
	case T___builtin_va_start:        \
	case T___func__:                  \
	case T___noop:                    \
	case T__assume:                   \
	case T_delete:                    \
	case T_false:                     \
	case T_sizeof:                    \
	case T_throw:                     \
	case T_true:                      \
	case T___imag__:                  \
	case T___real__

/**
 * Returns the size of a statement node.
 *
 * @param kind  the statement kind
 */
static size_t get_statement_struct_size(statement_kind_t kind)
{
	static const size_t sizes[] = {
		[STATEMENT_ERROR]         = sizeof(statement_base_t),
		[STATEMENT_EMPTY]         = sizeof(statement_base_t),
		[STATEMENT_COMPOUND]      = sizeof(compound_statement_t),
		[STATEMENT_RETURN]        = sizeof(return_statement_t),
		[STATEMENT_DECLARATION]   = sizeof(declaration_statement_t),
		[STATEMENT_IF]            = sizeof(if_statement_t),
		[STATEMENT_SWITCH]        = sizeof(switch_statement_t),
		[STATEMENT_EXPRESSION]    = sizeof(expression_statement_t),
		[STATEMENT_CONTINUE]      = sizeof(statement_base_t),
		[STATEMENT_BREAK]         = sizeof(statement_base_t),
		[STATEMENT_COMPUTED_GOTO] = sizeof(computed_goto_statement_t),
		[STATEMENT_GOTO]          = sizeof(goto_statement_t),
		[STATEMENT_LABEL]         = sizeof(label_statement_t),
		[STATEMENT_CASE_LABEL]    = sizeof(case_label_statement_t),
		[STATEMENT_DO_WHILE]      = sizeof(do_while_statement_t),
		[STATEMENT_FOR]           = sizeof(for_statement_t),
		[STATEMENT_ASM]           = sizeof(asm_statement_t),
	};
	assert((size_t)kind < ARRAY_SIZE(sizes));
	assert(sizes[kind] != 0);
	return sizes[kind];
}

/**
 * Returns the size of an expression node.
 *
 * @param kind  the expression kind
 */
static size_t get_expression_struct_size(expression_kind_t kind)
{
	static const size_t sizes[] = {
		[EXPR_ERROR]                      = sizeof(expression_base_t),
		[EXPR_REFERENCE]                  = sizeof(reference_expression_t),
		[EXPR_ENUM_CONSTANT]              = sizeof(reference_expression_t),
		[EXPR_LITERAL_BOOLEAN]            = sizeof(literal_expression_t),
		[EXPR_LITERAL_INTEGER]            = sizeof(literal_expression_t),
		[EXPR_LITERAL_FLOATINGPOINT]      = sizeof(literal_expression_t),
		[EXPR_LITERAL_CHARACTER]          = sizeof(string_literal_expression_t),
		[EXPR_LITERAL_MS_NOOP]            = sizeof(literal_expression_t),
		[EXPR_STRING_LITERAL]             = sizeof(string_literal_expression_t),
		[EXPR_COMPOUND_LITERAL]           = sizeof(compound_literal_expression_t),
		[EXPR_CALL]                       = sizeof(call_expression_t),
		[EXPR_UNARY_FIRST]                = sizeof(unary_expression_t),
		[EXPR_BINARY_FIRST]               = sizeof(binary_expression_t),
		[EXPR_CONDITIONAL]                = sizeof(conditional_expression_t),
		[EXPR_SELECT]                     = sizeof(select_expression_t),
		[EXPR_ARRAY_ACCESS]               = sizeof(array_access_expression_t),
		[EXPR_SIZEOF]                     = sizeof(typeprop_expression_t),
		[EXPR_ALIGNOF]                    = sizeof(typeprop_expression_t),
		[EXPR_CLASSIFY_TYPE]              = sizeof(classify_type_expression_t),
		[EXPR_FUNCNAME]                   = sizeof(funcname_expression_t),
		[EXPR_BUILTIN_CONSTANT_P]         = sizeof(builtin_constant_expression_t),
		[EXPR_BUILTIN_TYPES_COMPATIBLE_P] = sizeof(builtin_types_compatible_expression_t),
		[EXPR_OFFSETOF]                   = sizeof(offsetof_expression_t),
		[EXPR_VA_START]                   = sizeof(va_start_expression_t),
		[EXPR_VA_ARG]                     = sizeof(va_arg_expression_t),
		[EXPR_VA_COPY]                    = sizeof(va_copy_expression_t),
		[EXPR_STATEMENT]                  = sizeof(statement_expression_t),
		[EXPR_LABEL_ADDRESS]              = sizeof(label_address_expression_t),
	};
	if (kind >= EXPR_UNARY_FIRST && kind <= EXPR_UNARY_LAST)
		return sizes[EXPR_UNARY_FIRST];
	if (kind >= EXPR_BINARY_FIRST && kind <= EXPR_BINARY_LAST)
		return sizes[EXPR_BINARY_FIRST];
	assert((size_t)kind < ARRAY_SIZE(sizes));
	assert(sizes[kind] != 0);
	return sizes[kind];
}

/**
 * Allocate a statement node of given kind and initialize all
 * fields with zero. Sets its source position to the position
 * of the current token.
 */
static statement_t *allocate_statement_zero(statement_kind_t kind)
{
	size_t       size = get_statement_struct_size(kind);
	statement_t *res  = allocate_ast_zero(size);

	res->base.kind   = kind;
	res->base.parent = current_parent;
	res->base.pos    = *HERE;
	return res;
}

/**
 * Allocate an expression node of given kind and initialize all
 * fields with zero.
 *
 * @param kind  the kind of the expression to allocate
 */
static expression_t *allocate_expression_zero(expression_kind_t kind)
{
	size_t        size = get_expression_struct_size(kind);
	expression_t *res  = allocate_ast_zero(size);

	res->base.kind = kind;
	res->base.type = type_error_type;
	res->base.pos  = *HERE;
	return res;
}

/**
 * Creates a new invalid expression at the source position
 * of the current token.
 */
static expression_t *create_error_expression(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_ERROR);
	expression->base.type = type_error_type;
	return expression;
}

/**
 * Creates a new invalid statement.
 */
static statement_t *create_error_statement(void)
{
	return allocate_statement_zero(STATEMENT_ERROR);
}

/**
 * Allocate a new empty statement.
 */
static statement_t *create_empty_statement(void)
{
	return allocate_statement_zero(STATEMENT_EMPTY);
}

/**
 * Returns the size of an initializer node.
 *
 * @param kind  the initializer kind
 */
static size_t get_initializer_size(initializer_kind_t kind)
{
	static const size_t sizes[] = {
		[INITIALIZER_VALUE]      = sizeof(initializer_value_t),
		[INITIALIZER_STRING]     = sizeof(initializer_value_t),
		[INITIALIZER_LIST]       = sizeof(initializer_list_t),
		[INITIALIZER_DESIGNATOR] = sizeof(initializer_designator_t)
	};
	assert((size_t)kind < ARRAY_SIZE(sizes));
	assert(sizes[kind] != 0);
	return sizes[kind];
}

/**
 * Allocate an initializer node of given kind and initialize all
 * fields with zero.
 */
static initializer_t *allocate_initializer_zero(initializer_kind_t kind,
                                                position_t const *const pos)
{
	initializer_t *result = allocate_ast_zero(get_initializer_size(kind));
	result->kind          = kind;
	result->base.pos      = *pos;
	return result;
}

static initializer_t *allocate_initializer_list(size_t len,
                                                position_t const *const pos)
{
	initializer_t *result;
	size_t size = sizeof(initializer_list_t)
	            + len * sizeof(result->list.initializers[0]);
	result           = allocate_ast_zero(size);
	result->kind     = INITIALIZER_LIST;
	result->base.pos = *pos;
	result->list.len = len;
	return result;
}

/**
 * Returns the index of the top element of the environment stack.
 */
static size_t environment_top(void)
{
	return ARR_LEN(environment_stack);
}

/**
 * Returns the index of the top element of the global label stack.
 */
static size_t label_top(void)
{
	return ARR_LEN(label_stack);
}

/**
 * Return the next token.
 */
static inline void next_token(void)
{
	token                              = lookahead_buffer[lookahead_bufpos];
	lookahead_buffer[lookahead_bufpos] = pp_token;
	next_preprocessing_token();

	lookahead_bufpos = (lookahead_bufpos + 1) % MAX_LOOKAHEAD;
}

static inline bool peek(token_kind_t const kind)
{
	return token.kind == kind;
}

static inline void eat(token_kind_t const kind)
{
	assert(peek(kind));
	(void)kind;
	next_token();
}

/**
 * Consume the current token, if it is of the expected kind.
 *
 * @param  kind  The kind of token to consume.
 * @return Whether the token was consumed.
 */
static inline bool accept(token_kind_t const kind)
{
	return peek(kind) ? next_token(), true : false;
}

/**
 * Return the next token with a given lookahead.
 */
static inline const token_t *look_ahead(size_t num)
{
	assert(0 < num && num <= MAX_LOOKAHEAD);
	size_t pos = (lookahead_bufpos + num - 1) % MAX_LOOKAHEAD;
	return &lookahead_buffer[pos];
}

static inline bool peek_ahead(token_kind_t const kind)
{
	return look_ahead(1)->kind == kind;
}

/**
 * Adds a token type to the token type anchor set (a multi-set).
 */
static void add_anchor_token(token_kind_t const token_kind)
{
	assert(token_kind < T_LAST_TOKEN);
	++token_anchor_set[token_kind];
}

/**
 * Remove a token type from the token type anchor set (a multi-set).
 */
static void rem_anchor_token(token_kind_t const token_kind)
{
	assert(token_kind < T_LAST_TOKEN);
	assert(token_anchor_set[token_kind] != 0);
	--token_anchor_set[token_kind];
}

/**
 * Eat tokens until a matching token type is found.
 */
static void eat_until_matching_token(token_kind_t const type)
{
	token_kind_t end_token;
	switch (type) {
	case '(': end_token = ')';  break;
	case '{': end_token = '}';  break;
	case '[': end_token = ']';  break;
	default:  end_token = type; break;
	}

	unsigned parenthesis_count = 0;
	unsigned brace_count       = 0;
	unsigned bracket_count     = 0;
	while (!peek(end_token)
	    || parenthesis_count != 0
	    || brace_count       != 0
	    || bracket_count     != 0) {
		switch (token.kind) {
		case T_EOF: return;
		case '(': ++parenthesis_count; break;
		case '{': ++brace_count;       break;
		case '[': ++bracket_count;     break;

		case ')':
			if (parenthesis_count > 0)
				--parenthesis_count;
			goto check_stop;

		case '}':
			if (brace_count > 0)
				--brace_count;
			goto check_stop;

		case ']':
			if (bracket_count > 0)
				--bracket_count;
check_stop:
			if (peek(end_token)
			 && parenthesis_count == 0
			 && brace_count       == 0
			 && bracket_count     == 0)
				return;
			break;

		default:
			break;
		}
		next_token();
	}
}

/**
 * Eat input tokens until an anchor is found.
 */
static void eat_until_anchor(void)
{
	while (token_anchor_set[token.kind] == 0) {
		if (peek('(') || peek('{') || peek('['))
			eat_until_matching_token(token.kind);
		next_token();
	}
}

/**
 * Eat a whole block from input tokens.
 */
static void eat_block(void)
{
	eat_until_matching_token('{');
	accept('}');
}

/**
 * Report a parse error because an expected token was not found.
 */
static
#if defined __GNUC__ && __GNUC__ >= 4
__attribute__((sentinel))
#endif
void parse_error_expected(const char *message, ...)
{
	va_list ap;
	va_start(ap, message);
	if (message) {
		errorf(HERE, "expected %lk while parsing %s, got %K", &ap, message,
		       &token);
	} else {
		errorf(HERE, "expected %lk, got %K", &ap, &token);
	}
	va_end(ap);
}

/**
 * Report an incompatible type.
 */
static void type_error_incompatible(const char *msg, const position_t *pos,
                                    type_t *type1, type_t *type2)
{
	errorf(pos, "%s, incompatible types: '%T' - '%T'", msg, type1, type2);
}

static bool skip_till(token_kind_t const expected, char const *const context)
{
	if (UNLIKELY(!peek(expected))) {
		parse_error_expected(context, expected, NULL);
		add_anchor_token(expected);
		eat_until_anchor();
		rem_anchor_token(expected);
		if (!peek(expected))
			return false;
	}
	return true;
}

/**
 * Expect the current token is the expected token.
 * If not, generate an error and skip until the next anchor.
 */
static void expect(token_kind_t const expected)
{
	if (skip_till(expected, NULL))
		eat(expected);
}

static symbol_t *expect_identifier(char const *const context,
                                   position_t *const pos)
{
	if (!skip_till(T_IDENTIFIER, context))
		return sym_anonymous;
	symbol_t *const sym = token.base.symbol;
	if (pos)
		*pos = *HERE;
	eat(T_IDENTIFIER);
	return sym;
}

/**
 * Push a given scope on the scope stack and make it the
 * current scope
 */
static scope_t *scope_push(scope_t *new_scope)
{
	if (current_scope != NULL)
		new_scope->depth = current_scope->depth + 1;

	scope_t *old_scope = current_scope;
	current_scope      = new_scope;
	return old_scope;
}

/**
 * Pop the current scope from the scope stack.
 */
static void scope_pop(scope_t *old_scope)
{
	current_scope = old_scope;
}

entity_t *get_entity(const symbol_t *const symbol, entity_namespace_t namespc)
{
	for (entity_t *entity = symbol->entity; entity != NULL;
	     entity = entity->base.symbol_next) {
		if (entity->base.namespc == namespc)
			return entity;
	}

	return NULL;
}

static entity_t *set_entity(entity_t *entity)
{
	symbol_t           *symbol  = entity->base.symbol;
	entity_namespace_t  namespc = entity->base.namespc;

	entity_t **anchor;
	entity_t  *iter;
	for (anchor = &symbol->entity; ; anchor = &iter->base.symbol_next) {
		iter = *anchor;
		if (iter == NULL)
			break;

		/* replace an entry? */
		if (iter->base.namespc == namespc) {
			entity->base.symbol_next = iter->base.symbol_next;
			break;
		}
	}
	*anchor = entity;
	return iter;
}

static void reset_symbol(symbol_t *symbol, entity_namespace_t namespc)
{
	/* replace with old_entity/remove */
	entity_t **anchor;
	entity_t  *iter;
	for (anchor = &symbol->entity; ; anchor = &iter->base.symbol_next) {
		iter = *anchor;
		if (iter == NULL)
			return;
		/* replace an entry? */
		if (iter->base.namespc == namespc)
			break;
	}
	*anchor = iter->base.symbol_next;
}

static void note_prev_decl(entity_t const *const entity)
{
	notef(&entity->base.pos, "previous declaration of '%N' was here", entity);
}

/* §6.2.3:1 24)  There is only one name space for tags even though three are
 * possible. */
static entity_t *get_tag(symbol_t const *const symbol,
                         entity_kind_t const kind, position_t const *const pos)
{
	entity_t *entity = get_entity(symbol, NAMESPACE_TAG);
	if (entity != NULL && entity->kind != kind) {
		char const *const tag = get_entity_kind_name(kind);
		errorf(pos, "'%s %Y' defined as wrong kind of tag, should be '%N'", tag,
		       symbol, entity);
		note_prev_decl(entity);
		entity = NULL;
	}
	return entity;
}

/**
 * pushs an entity on the environment stack and links the corresponding symbol
 * it.
 */
static void stack_push(stack_entry_t **stack_ptr, entity_t *entity)
{
	symbol_t           *symbol  = entity->base.symbol;
	entity_namespace_t  namespc = entity->base.namespc;
	assert(namespc != 0);

	/* replace/add entity into entity list of the symbol */
	entity_t *old_entity = set_entity(entity);

	/* remember old declaration */
	stack_entry_t entry;
	entry.symbol     = symbol;
	entry.old_entity = old_entity;
	entry.namespc    = namespc;
	ARR_APP1(stack_entry_t, *stack_ptr, entry);
}

/**
 * Push an entity on the environment stack.
 */
static void environment_push(entity_t *entity)
{
	assert(entity->base.pos.input_name != NULL);
	assert(entity->base.parent_scope != NULL);
	stack_push(&environment_stack, entity);
}

/**
 * Push a declaration on the global label stack.
 *
 * @param declaration  the declaration
 */
static void label_push(entity_t *label)
{
	/* we abuse the parameters scope as parent for the labels */
	label->base.parent_scope = &current_function->parameters;
	stack_push(&label_stack, label);
}

/**
 * pops symbols from the environment stack until @p new_top is the top element
 */
static void stack_pop_to(stack_entry_t **stack_ptr, size_t new_top)
{
	stack_entry_t *stack = *stack_ptr;
	size_t         top   = ARR_LEN(stack);
	assert(new_top <= top);
	if (new_top == top)
		return;

	for (size_t i = top; i > new_top; --i) {
		stack_entry_t *entry = &stack[i - 1];

		entity_t           *old_entity = entry->old_entity;
		symbol_t           *symbol     = entry->symbol;
		entity_namespace_t  namespc    = entry->namespc;

		/* replace with old_entity/remove */
		if (old_entity != NULL) {
			set_entity(old_entity);
		} else {
			reset_symbol(symbol, namespc);
		}
	}

	ARR_SHRINKLEN(*stack_ptr, new_top);
}

/**
 * Pop all entries from the environment stack until the new_top
 * is reached.
 *
 * @param new_top  the new stack top
 */
static void environment_pop_to(size_t new_top)
{
	stack_pop_to(&environment_stack, new_top);
}

/**
 * Pop all entries from the global label stack until the new_top
 * is reached.
 *
 * @param new_top  the new stack top
 */
static void label_pop_to(size_t new_top)
{
	stack_pop_to(&label_stack, new_top);
}

/**
 * Check if a given expression represents a null pointer constant.
 *
 * @param expression  the expression to check
 */
static bool is_null_pointer_constant(const expression_t *expression)
{
	/* skip void* cast */
	if (expression->kind == EXPR_UNARY_CAST) {
		type_t *const type = skip_typeref(expression->base.type);
		if (type->kind == TYPE_POINTER
		    && type->base.qualifiers == TYPE_QUALIFIER_NONE
		    && skip_typeref(type->pointer.points_to) == type_void) {
			expression = expression->unary.value;
		}
	}

	return is_constant_expression(expression) == EXPR_CLASS_INTEGER_CONSTANT
	    && !fold_expression_to_bool(expression);
}

/**
 * Try to get an enum type for an expression (instead of "int" and similar).
 */
static enum_t *determine_enum(expression_t *expression)
{
	if (expression->kind == EXPR_ENUM_CONSTANT) {
		const entity_t *enume = expression->reference.entity;
		return enume->enum_value.enume;
	}
	type_t *type;
	if (expression->kind == EXPR_SELECT) {
		const entity_t *member = expression->select.compound_entry;
		assert(member->kind == ENTITY_COMPOUND_MEMBER);
		/* in case of bitfields, this will get us the base type */
		type = member->declaration.type;
	} else {
		type = expression->base.type;
	}
	type = skip_typeref(type);
	if (is_type_enum(type))
		return type->enumt.enume;
	return NULL;
}

static void warn_enum_conversion(type_t *dest_type, expression_t *expression)
{
	const enum_t *const expression_enum = determine_enum(expression);
	if (expression_enum == NULL)
		return;

	const type_t *skipped_dest = skip_typeref(dest_type);
	assert(is_type_enum(skipped_dest));
	const enum_t *const dest_enum = skipped_dest->enumt.enume;
	if (expression_enum != dest_enum) {
		warningf(WARN_ENUM_CONVERSION, &expression->base.pos,
				 "implicit conversion from '%N' to different enumeration type '%T'",
				 expression_enum, dest_type);
	}
}

/**
 * Create an implicit cast expression.
 *
 * @param expression  the expression to cast
 * @param dest_type   the destination type
 */
static expression_t *create_implicit_cast(expression_t *expression,
                                          type_t *dest_type)
{
	const type_t *const source_type = skip_typeref(expression->base.type);
	const type_t *const skipped_dest = skip_typeref(dest_type);
	if (source_type == skipped_dest)
		return expression;

	if (is_type_enum(skipped_dest))
		warn_enum_conversion(dest_type, expression);

	expression_t *cast = allocate_expression_zero(EXPR_UNARY_CAST);
	cast->unary.value   = expression;
	cast->base.pos      = expression->base.pos;
	cast->base.type     = dest_type;
	cast->base.implicit = true;
	return cast;
}

typedef enum assign_error_t {
	ASSIGN_SUCCESS,
	ASSIGN_ERROR_INCOMPATIBLE,
	ASSIGN_ERROR_POINTER_QUALIFIER_MISSING,
	ASSIGN_WARNING_POINTER_INCOMPATIBLE,
	ASSIGN_WARNING_POINTER_SIGNEDNESS,
	ASSIGN_WARNING_POINTER_FROM_INT,
	ASSIGN_WARNING_INT_FROM_POINTER
} assign_error_t;

static void report_assign_error(assign_error_t error, type_t *orig_type_left,
                                expression_t const *const right,
                                char const *const context,
                                position_t const *const pos)
{
	type_t *const orig_type_right = right->base.type;
	type_t *const type_left       = skip_typeref(orig_type_left);
	type_t *const type_right      = skip_typeref(orig_type_right);

	switch (error) {
	case ASSIGN_SUCCESS:
		return;
	case ASSIGN_ERROR_INCOMPATIBLE:
		errorf(pos,
		       "destination type '%T' in %s is incompatible with type '%T'",
		       orig_type_left, context, orig_type_right);
		return;

	case ASSIGN_ERROR_POINTER_QUALIFIER_MISSING: {
		type_t *points_to_left  = skip_typeref(type_left->pointer.points_to);
		type_t *points_to_right = skip_typeref(type_right->pointer.points_to);

		/* the left type has all qualifiers from the right type */
		unsigned missing_qualifiers = points_to_right->base.qualifiers & ~points_to_left->base.qualifiers;
		warningf(WARN_OTHER, pos,
		         "destination type '%T' in %s from type '%T' lacks qualifiers '%Q' in pointer target type",
		         orig_type_left, context, orig_type_right, missing_qualifiers);
		return;
	}

	case ASSIGN_WARNING_POINTER_INCOMPATIBLE:
		warningf(WARN_OTHER, pos,
		         "destination type '%T' in %s is incompatible with '%E' of type '%T'",
		         orig_type_left, context, right, orig_type_right);
		return;

	case ASSIGN_WARNING_POINTER_SIGNEDNESS:
		warningf(WARN_POINTER_SIGN, pos,
		         "destination type '%T' in %s is incompatible with '%E' of type '%T'",
		         orig_type_left, context, right, orig_type_right);
		return;

	case ASSIGN_WARNING_POINTER_FROM_INT:
		warningf(WARN_OTHER, pos,
		         "%s makes pointer '%T' from integer '%T' without a cast",
		         context, orig_type_left, orig_type_right);
		return;

	case ASSIGN_WARNING_INT_FROM_POINTER:
		warningf(WARN_OTHER, pos,
		         "%s makes integer '%T' from pointer '%T' without a cast",
		         context, orig_type_left, orig_type_right);
		return;

	default:
		panic("invalid error value");
	}
}

/** Implements the rules from §6.5.16.1 */
static assign_error_t semantic_assign(type_t *orig_type_left,
                                      const expression_t *const right)
{
	type_t *const orig_type_right = right->base.type;
	type_t *const type_left       = skip_typeref(orig_type_left);
	type_t *const type_right      = skip_typeref(orig_type_right);

	if (is_type_pointer(type_left)) {
		if (is_null_pointer_constant(right)) {
			return ASSIGN_SUCCESS;
		} else if (is_type_pointer(type_right)) {
			type_t *points_to_left
				= skip_typeref(type_left->pointer.points_to);
			type_t *points_to_right
				= skip_typeref(type_right->pointer.points_to);
			assign_error_t res = ASSIGN_SUCCESS;

			/* the left type has all qualifiers from the right type */
			unsigned missing_qualifiers
				= points_to_right->base.qualifiers & ~points_to_left->base.qualifiers;
			if (missing_qualifiers != 0)
				res = ASSIGN_ERROR_POINTER_QUALIFIER_MISSING;

			if (is_type_void(points_to_left))
				return res;

			if (is_type_void(points_to_right))
				/* ISO/IEC 14882:1998(E) §C.1.2:6 */
				return dialect.cpp ? ASSIGN_ERROR_INCOMPATIBLE : res;

			if (!types_compatible_ignore_qualifiers(points_to_left,
			                                        points_to_right)) {
				if (is_type_integer(points_to_left)
				 && is_type_integer(points_to_right)
				 && get_type_size(points_to_left)
				    == get_type_size(points_to_right))
					return ASSIGN_WARNING_POINTER_SIGNEDNESS;

				return ASSIGN_WARNING_POINTER_INCOMPATIBLE;
			}

			return res;
		} else if (is_type_integer(type_right)) {
			return ASSIGN_WARNING_POINTER_FROM_INT;
		}
	} else if ((is_type_arithmetic(type_left)
	        && is_type_arithmetic(type_right))
	        || (is_type_atomic(type_left, ATOMIC_TYPE_BOOL)
	            && is_type_pointer(type_right))) {
		return ASSIGN_SUCCESS;
	} else if (is_type_compound(type_left) && is_type_compound(type_right)) {
		if (types_compatible_ignore_qualifiers(type_left, type_right))
			return ASSIGN_SUCCESS;
	} else if (is_type_integer(type_left) && is_type_pointer(type_right)) {
		return ASSIGN_WARNING_INT_FROM_POINTER;
	}

	if (!is_type_valid(type_left) || !is_type_valid(type_right))
		return ASSIGN_SUCCESS;

	return ASSIGN_ERROR_INCOMPATIBLE;
}

static expression_t *parse_integer_constant_expression(const char *description)
{
	expression_t *result = parse_subexpression(PREC_CONDITIONAL);

	switch (is_constant_expression(result)) {
	case EXPR_CLASS_VARIABLE:
		errorf(&result->base.pos, "expression '%E' is not constant", result);
		result->base.type = type_error_type;
		break;
	case EXPR_CLASS_CONSTANT:
		if (!is_type_integer(skip_typeref(result->base.type))) {
			errorf(&result->base.pos, "%s '%E' has non-integer type",
			       description, result);
			result->base.type = type_error_type;
		} else {
			warningf(WARN_PEDANTIC, &result->base.pos,
			         "%s '%E' is not an integer constant expression",
			         description, result);
		}
		break;
	case EXPR_CLASS_INTEGER_CONSTANT:
	case EXPR_CLASS_ERROR:
		break;
	}
	return result;
}

static expression_t *parse_assignment_expression(void)
{
	return parse_subexpression(PREC_ASSIGNMENT);
}

static void append_string(string_t const *const s)
{
	obstack_grow(&ast_obstack, s->begin, s->size);
}

static string_t *concat_string_literals(void)
{
	assert(peek(T_STRING_LITERAL));

	string_t *result;
	if (peek_ahead(T_STRING_LITERAL)) {
		/* construct new string on ast_obstack, as string_obstack may be used
		 * simulatneously be the preprocessor */
		begin_string_construction_on(&ast_obstack);
		append_string(token.literal.string);
		eat(T_STRING_LITERAL);
		warningf(WARN_TRADITIONAL, HERE,
		         "traditional C rejects string constant concatenation");
		string_encoding_t enc = token.literal.string->encoding;
		do {
			string_encoding_t const new_enc = token.literal.string->encoding;
			if (new_enc != enc && new_enc != STRING_ENCODING_CHAR) {
				if (enc == STRING_ENCODING_CHAR) {
					enc = new_enc;
				} else {
					errorf(HERE,
					       "concatenating string literals with encodings %s and %s",
					       get_string_encoding_prefix(enc),
					       get_string_encoding_prefix(new_enc));
				}
			}
			append_string(token.literal.string);
			eat(T_STRING_LITERAL);
		} while (peek(T_STRING_LITERAL));
		result = finish_string_construction_on(&ast_obstack, enc);
	} else {
		result = token.literal.string;
		eat(T_STRING_LITERAL);
	}

	return result;
}

static string_t *parse_string_literals(char const *const context)
{
	if (!skip_till(T_STRING_LITERAL, context)) {
		begin_string_construction();
		return finish_string_construction(STRING_ENCODING_CHAR);
	}

	position_t const pos = *HERE;
	string_t        *res = concat_string_literals();
	if (res->encoding != STRING_ENCODING_CHAR)
		errorf(&pos, "expected plain string literal, got %s string literal",
		       get_string_encoding_prefix(res->encoding));

	return res;
}

static attribute_t *allocate_attribute_zero(attribute_kind_t kind)
{
	attribute_t *attribute = allocate_ast_zero(sizeof(*attribute));
	attribute->kind = kind;
	attribute->pos  = *HERE;
	return attribute;
}

/**
 * Parse (gcc) attribute argument. From gcc comments in gcc source:
 *
 *  attribute:
 *    __attribute__ ( ( attribute-list ) )
 *
 *  attribute-list:
 *    attrib
 *    attribute_list , attrib
 *
 *  attrib:
 *    empty
 *    any-word
 *    any-word ( identifier )
 *    any-word ( identifier , nonempty-expr-list )
 *    any-word ( expr-list )
 *
 *  where the "identifier" must not be declared as a type, and
 *  "any-word" may be any identifier (including one declared as a
 *  type), a reserved word storage class specifier, type specifier or
 *  type qualifier.  ??? This still leaves out most reserved keywords
 *  (following the old parser), shouldn't we include them, and why not
 *  allow identifiers declared as types to start the arguments?
 *
 *  Matze: this all looks confusing and little systematic, so we're even less
 *  strict and parse any list of things which are identifiers or
 *  (assignment-)expressions.
 */
static attribute_argument_t *parse_attribute_arguments(void)
{
	attribute_argument_t  *first  = NULL;
	attribute_argument_t **anchor = &first;
	if (!peek(')')) do {
		attribute_argument_t *argument = allocate_ast_zero(sizeof(*argument));

		/* is it an identifier */
		if (peek(T_IDENTIFIER) && (peek_ahead(',') || peek_ahead(')'))) {
			argument->kind     = ATTRIBUTE_ARGUMENT_SYMBOL;
			argument->v.symbol = token.base.symbol;
			eat(T_IDENTIFIER);
		} else {
			/* must be an expression */
			expression_t *expression = parse_assignment_expression();

			argument->kind         = ATTRIBUTE_ARGUMENT_EXPRESSION;
			argument->v.expression = expression;
		}

		/* append argument */
		*anchor = argument;
		anchor  = &argument->next;
	} while (accept(','));
	expect(')');
	return first;
}

static attribute_format_argument_t *parse_attribute_format(void)
{
	char const *const ctx = "attribute 'custom_format'";

	attribute_format_argument_t *const a = allocate_ast_zero(sizeof(*a));

	add_anchor_token(',');
	add_anchor_token(T_STRING_LITERAL);
	a->fmt_idx = parse_integer_constant_expression(ctx);
	expect(',');
	rem_anchor_token(T_STRING_LITERAL);
	a->flags   = parse_string_literals(ctx);
	rem_anchor_token(',');

	if (accept(',')) {
		add_anchor_token(')');
		add_anchor_token(',');
		attribute_format_specifier_t **anchor = &a->specifiers;
		do {
			attribute_format_specifier_t *const s = allocate_ast_zero(sizeof(*s));

			add_anchor_token(':');
			s->specifier = parse_string_literals(ctx);
			rem_anchor_token(':');
			expect(':');
			s->type = parse_typename();

			*anchor = s;
			anchor  = &s->next;
		} while (accept(','));
		rem_anchor_token(',');
		rem_anchor_token(')');
	}
	expect(')');

	return a;
}

static attribute_t *parse_attribute_asm(void)
{
	attribute_t *attribute = allocate_attribute_zero(ATTRIBUTE_GNU_ASM);
	eat(T_asm);
	expect('(');
	attribute->a.arguments = parse_attribute_arguments();
	return attribute;
}

static attribute_t *parse_attribute_gnu_single(void)
{
	/* parse "any-word" */
	symbol_t *const symbol = token.base.symbol;
	if (symbol == NULL) {
		parse_error_expected("attribute((", T_IDENTIFIER, NULL);
		return NULL;
	}

	attribute_kind_t  kind;
	char const *const name = symbol->string;
	for (kind = ATTRIBUTE_GNU_FIRST;; ++kind) {
		if (kind > ATTRIBUTE_GNU_LAST) {
			/* special case for "__const" */
			if (peek(T_const)) {
				kind = ATTRIBUTE_GNU_CONST;
				break;
			}

			warningf(WARN_ATTRIBUTE, HERE,
			         "unknown attribute '%hs' ignored", name);
			/* TODO: we should still save the attribute in the list... */
			kind = ATTRIBUTE_UNKNOWN;
			break;
		}

		const char *attribute_name = get_attribute_name(kind);
		if (attribute_name != NULL && streq_underscore(attribute_name, name))
			break;
	}

	attribute_t *attribute = allocate_attribute_zero(kind);
	next_token();

	/* parse arguments */
	if (accept('(')) {
		if (kind == ATTRIBUTE_CPARSER_CUSTOM_FORMAT) {
			attribute->a.format = parse_attribute_format();
		} else {
			attribute->a.arguments = parse_attribute_arguments();
		}
	}

	return attribute;
}

static attribute_t *parse_attribute_gnu(void)
{
	attribute_t  *first  = NULL;
	attribute_t **anchor = &first;

	eat(T___attribute__);
	add_anchor_token(')');
	add_anchor_token(',');
	expect('(');
	expect('(');

	if (!peek(')')) do {
		attribute_t *attribute = parse_attribute_gnu_single();
		if (attribute) {
			*anchor = attribute;
			anchor  = &attribute->next;
		}
	} while (accept(','));
	rem_anchor_token(',');
	rem_anchor_token(')');

	expect(')');
	expect(')');
	return first;
}

/** Parse attributes. */
static attribute_t *parse_attributes(attribute_t *first)
{
	attribute_t **anchor = &first;
	for (;;) {
		while (*anchor != NULL)
			anchor = &(*anchor)->next;

		attribute_t *attribute;
		switch (token.kind) {
		case T___attribute__:
			attribute = parse_attribute_gnu();
			if (attribute == NULL)
				continue;
			break;

		case T_asm:
			attribute = parse_attribute_asm();
			break;

		case T_cdecl:
			attribute = allocate_attribute_zero(ATTRIBUTE_MS_CDECL);
			eat(T_cdecl);
			break;

		case T__fastcall:
			attribute = allocate_attribute_zero(ATTRIBUTE_MS_FASTCALL);
			eat(T__fastcall);
			break;

		case T__forceinline:
			attribute = allocate_attribute_zero(ATTRIBUTE_MS_FORCEINLINE);
			eat(T__forceinline);
			break;

		case T__stdcall:
			attribute = allocate_attribute_zero(ATTRIBUTE_MS_STDCALL);
			eat(T__stdcall);
			break;

		case T___thiscall:
			attribute = allocate_attribute_zero(ATTRIBUTE_MS_THISCALL);
			eat(T___thiscall);
			break;

		default:
			return first;
		}

		*anchor = attribute;
		anchor  = &attribute->next;
	}
}

#define ENT_ANY ((entity_t*)-1)

static void mark_vars_read(expression_t *expr, entity_t *lhs_ent);
static entity_t *determine_lhs_ent(expression_t *const expr,
                                   entity_t *lhs_ent);

static designator_t *parse_designation(void)
{
	designator_t  *result = NULL;
	designator_t **anchor = &result;

	for (;;) {
		designator_t *designator;
		switch (token.kind) {
		case '[':
			designator      = allocate_ast_zero(sizeof(designator[0]));
			designator->pos = *HERE;
			eat('[');
			add_anchor_token(']');
			add_anchor_token(T_DOTDOTDOT);
			designator->array_index
				= parse_integer_constant_expression("array index");
			rem_anchor_token(T_DOTDOTDOT);
			if (accept(T_DOTDOTDOT)) {
				designator->range_last
					= parse_integer_constant_expression("array range end");
				errorf(&designator->pos, "range initializer not supported");
			}
			rem_anchor_token(']');
			expect(']');
			break;
		case '.':
			designator      = allocate_ast_zero(sizeof(designator[0]));
			designator->pos = *HERE;
			eat('.');
			designator->symbol = expect_identifier("designator", NULL);
			if (designator->symbol == sym_anonymous)
				return NULL;
			break;
		default:
			expect('=');
			return result;
		}

		assert(designator != NULL);
		*anchor = designator;
		anchor  = &designator->next;
	}
}

/**
 * Build an initializer from a given expression.
 */
static initializer_t *initializer_from_expression(type_t *orig_type,
                                                  expression_t *expression)
{
	/* TODO check that expression is a constant expression */

	type_t *const type = skip_typeref(orig_type);

	/* §6.7.8.14/15 char array may be initialized by string literals */
	if (expression->kind == EXPR_STRING_LITERAL && is_type_array(type)) {
		array_type_t *const array_type   = &type->array;
		type_t       *const element_type = skip_typeref(array_type->element_type);
		switch (expression->string_literal.value->encoding) {
		case STRING_ENCODING_CHAR:
		case STRING_ENCODING_UTF8: {
			if (is_type_atomic(element_type, ATOMIC_TYPE_CHAR)
			 || is_type_atomic(element_type, ATOMIC_TYPE_SCHAR)
			 || is_type_atomic(element_type, ATOMIC_TYPE_UCHAR))
				goto make_string_init;
			break;
		}

		case STRING_ENCODING_CHAR16:
		case STRING_ENCODING_CHAR32:
		case STRING_ENCODING_WIDE: {
			assert(is_type_pointer(expression->base.type));
			type_t *const init_type = expression->base.type->pointer.points_to;
			if (types_compatible_ignore_qualifiers(element_type, init_type)) {
make_string_init:;
				position_t const *const pos = &expression->base.pos;
				initializer_t *const init
					= allocate_initializer_zero(INITIALIZER_STRING, pos);
				init->value.value = expression;
				return init;
			}
			break;
		}
		}
	}

	assign_error_t error = semantic_assign(type, expression);
	if (error == ASSIGN_ERROR_INCOMPATIBLE)
		return NULL;
	position_t const *const pos = &expression->base.pos;
	report_assign_error(error, type, expression, "initializer", pos);

	initializer_t *const result
		= allocate_initializer_zero(INITIALIZER_VALUE, pos);
	result->base.pos = expression->base.pos;
	result->value.value = create_implicit_cast(expression, type);

	return result;
}

/** initializers (and compound literals) are allowed for object types
 * or array types with unknown size (which are not variable initialized) */
static bool is_initializable_type(const type_t *const type)
{
	if (is_type_array(type))
		return type->array.size_expression == NULL || !type->array.is_vla;
	return is_type_complete(type) && type->kind != TYPE_FUNCTION
	    && is_type_valid(type);
}

/**
 * Parses an scalar initializer.
 *
 * §6.7.8.11; eat {} without warning
 */
static initializer_t *parse_scalar_initializer(type_t *type,
                                               bool must_be_constant,
                                               bool may_have_braces)
{
	/* there might be extra {} hierarchies */
	unsigned braces = 0;
	if (peek('{')) {
		do {
			if (braces == (unsigned)may_have_braces
			 && is_initializable_type(type))
				warningf(WARN_OTHER, HERE,
				         "extra curly braces around scalar initializer");
			eat('{');
			++braces;
		} while (peek('{'));
	}

	expression_t *expression = parse_assignment_expression();
	mark_vars_read(expression, NULL);
	if (must_be_constant && !is_linker_constant(expression))
		errorf(&expression->base.pos,
		       "initialisation expression '%E' is not constant",
		       expression);

	initializer_t *initializer = initializer_from_expression(type, expression);

	if (initializer == NULL) {
		errorf(&expression->base.pos,
		       "expression '%E' (type '%T') doesn't match expected type '%T'",
		       expression, expression->base.type, type);
		/* TODO */
		return NULL;
	}

	bool additional_warning_displayed = false;
	for (; braces-- > 0;) {
		accept(',');
		if (!peek('}') && !additional_warning_displayed) {
			warningf(WARN_OTHER, HERE,
			         "additional elements in scalar initializer");
			additional_warning_displayed = true;
		}
		eat_block();
	}

	return initializer;
}

/**
 * An entry in the type path.
 */
typedef struct type_path_entry_t type_path_entry_t;
struct type_path_entry_t {
	type_t *type;       /**< the upper top type. restored to path->top_tye if this entry is popped. */
	union {
		size_t         index;          /**< For array types: the current index. */
		declaration_t *compound_entry; /**< For compound types: the current declaration. */
	} v;
};

/**
 * A type path expression a position inside compound or array types.
 */
typedef struct type_path_t type_path_t;
struct type_path_t {
	type_path_entry_t *path;      /**< An flexible array containing the current
	                                   path. */
	type_t            *top_type;  /**< type of the element the path points */
	size_t             size;      /**< size of outermost array */
};

/**
 * Prints a type path for debugging.
 */
static __attribute__((unused)) void debug_print_type_path(
		const type_path_t *path)
{
	size_t len = ARR_LEN(path->path);

	for (size_t i = 0; i < len; ++i) {
		const type_path_entry_t *entry = & path->path[i];

		type_t *type = skip_typeref(entry->type);
		if (is_type_compound(type)) {
			/* in gcc mode structs can have no members */
			if (entry->v.compound_entry == NULL) {
				assert(i == len-1);
				continue;
			}
			fprintf(stderr, ".%s",
			        entry->v.compound_entry->base.symbol->string);
		} else if (is_type_array(type)) {
			fprintf(stderr, "[%u]", (unsigned) entry->v.index);
		} else {
			fprintf(stderr, "-INVALID-");
		}
	}
	if (path->top_type != NULL) {
		fprintf(stderr, "  (");
		print_type(path->top_type);
		fprintf(stderr, ")");
	}
}

/**
 * Return the top type path entry, i.e. in a path
 * (type).a.b returns the b.
 */
static type_path_entry_t *get_type_path_top(const type_path_t *path)
{
	size_t len = ARR_LEN(path->path);
	assert(len > 0);
	return &path->path[len-1];
}

/**
 * Enlarge the type path by an (empty) element.
 */
static type_path_entry_t *append_to_type_path(type_path_t *path)
{
	size_t len = ARR_LEN(path->path);
	ARR_RESIZE(type_path_entry_t, path->path, len+1);

	type_path_entry_t *result = & path->path[len];
	memset(result, 0, sizeof(result[0]));
	return result;
}

/**
 * Descending into a sub-type. Enter the scope of the current top_type.
 */
static void descend_into_subtype(type_path_t *path)
{
	type_t *orig_top_type = path->top_type;
	type_t *top_type      = skip_typeref(orig_top_type);

	type_path_entry_t *top = append_to_type_path(path);
	top->type              = top_type;

	if (!is_initializable_type(top_type))
		return;

	if (is_type_compound(top_type)) {
		compound_t *const compound = top_type->compound.compound;
		entity_t   *const entry
			= skip_unnamed_bitfields(compound->members.first_entity);

		if (entry != NULL) {
			top->v.compound_entry = &entry->declaration;
			path->top_type = entry->declaration.type;
		} else {
			path->top_type = NULL;
		}
	} else {
		assert(is_type_array(top_type));
		top->v.index   = 0;
		path->top_type = top_type->array.element_type;
	}
}

/**
 * Pop an entry from the given type path, i.e. returning from
 * (type).a.b to (type).a
 */
static void ascend_from_subtype(type_path_t *path)
{
	type_path_entry_t *top = get_type_path_top(path);

	path->top_type = top->type;

	size_t len = ARR_LEN(path->path);
	ARR_RESIZE(type_path_entry_t, path->path, len-1);
}

/**
 * Pop entries from the given type path until the given
 * path level is reached.
 */
static void ascend_to(type_path_t *path, size_t top_path_level)
{
	size_t len = ARR_LEN(path->path);

	while (len > top_path_level) {
		ascend_from_subtype(path);
		len = ARR_LEN(path->path);
	}
}

static bool walk_designator(type_path_t *path, const designator_t *designator,
                            bool used_in_offsetof)
{
	for (; designator != NULL; designator = designator->next) {
		type_path_entry_t *top       = get_type_path_top(path);
		type_t            *orig_type = top->type;

		type_t *type = skip_typeref(orig_type);

		if (designator->symbol != NULL) {
			symbol_t *symbol = designator->symbol;
			if (!is_type_compound(type)) {
				if (is_type_valid(type))
					errorf(&designator->pos,
					       "'.%Y' designator used for non-compound type '%T'",
					       symbol, orig_type);

				top->type             = type_error_type;
				top->v.compound_entry = NULL;
				orig_type             = type_error_type;
			} else {
				compound_t *compound = type->compound.compound;
				entity_t   *iter     = compound->members.first_entity;
				for (; iter != NULL; iter = iter->base.next) {
					if (iter->base.symbol == symbol)
						break;
				}
				if (iter == NULL) {
					errorf(&designator->pos,
					       "'%T' has no member named '%Y'", orig_type, symbol);
					return false;
				}
				assert(iter->kind == ENTITY_COMPOUND_MEMBER);
				if (used_in_offsetof && iter->compound_member.bitfield) {
					errorf(&designator->pos,
					       "offsetof designator '%Y' must not specify bitfield",
					       symbol);
					return false;
				}

				top->type             = orig_type;
				top->v.compound_entry = &iter->declaration;
				orig_type             = iter->declaration.type;
			}
		} else {
			expression_t *array_index = designator->array_index;
			if (array_index->base.type == type_error_type)
				return true;

			if (!is_type_array(type)) {
				if (is_type_valid(type))
					errorf(&designator->pos,
					       "[%E] designator used for non-array type '%T'",
					       array_index, orig_type);

				return false;
			}

			long index_long = fold_expression_to_int(array_index);
			if (!used_in_offsetof) {
				if (index_long < 0) {
					errorf(&designator->pos,
					       "array index [%E] must be positive", array_index);
				} else if (type->array.size_constant) {
					long array_size = type->array.size;
					if (index_long >= array_size)
						errorf(&designator->pos,
						       "designator [%E] (%ld) exceeds array size %ld",
						       array_index, index_long, array_size);
				}
			}

			top->type    = orig_type;
			top->v.index = (size_t) index_long;
			orig_type    = type->array.element_type;
		}
		path->top_type = orig_type;

		if (designator->next != NULL)
			descend_into_subtype(path);
	}
	return true;
}

static void advance_current_object(type_path_t *path, size_t top_path_level)
{
	type_path_entry_t *top = get_type_path_top(path);

	type_t *type = skip_typeref(top->type);
	if (is_type_union(type)) {
		/* in unions only the first element is initialized */
		top->v.compound_entry = NULL;
	} else if (is_type_struct(type)) {
		declaration_t *entry = top->v.compound_entry;

		entity_t *const next_entity = skip_unnamed_bitfields(entry->base.next);
		if (next_entity != NULL) {
			assert(is_declaration(next_entity));
			entry = &next_entity->declaration;
		} else {
			entry = NULL;
		}

		top->v.compound_entry = entry;
		if (entry != NULL) {
			path->top_type = entry->type;
			return;
		}
	} else if (is_type_array(type)) {
		top->v.index++;

		if (!type->array.size_constant || top->v.index < type->array.size)
			return;
	} else {
		assert(!is_type_valid(type));
		return;
	}

	/* we're past the last member of the current sub-aggregate, try if we
	 * can ascend in the type hierarchy and continue with another subobject */
	size_t len = ARR_LEN(path->path);

	if (len > top_path_level) {
		ascend_from_subtype(path);
		advance_current_object(path, top_path_level);
	} else {
		path->top_type = NULL;
	}
}

/**
 * skip any {...} blocks until a closing bracket is reached.
 */
static void skip_initializers(void)
{
	accept('{');

	while (!peek('}')) {
		if (peek(T_EOF))
			return;
		if (peek('{')) {
			eat_block();
			continue;
		}
		next_token();
	}
}

/**
 * Parse a part of an initialiser for a struct or union,
 */
static initializer_t *parse_sub_initializer(type_path_t *path,
		type_t *outer_type, parse_initializer_env_t *env)
{
	/* empty initializer? */
	if (peek('}'))
		return allocate_initializer_list(0, &env->pos);

	initializer_t *result = NULL;

	type_t *orig_type = path->top_type;
	type_t *type      = NULL;

	if (orig_type == NULL) {
		/* We are initializing an empty compound. */
	} else {
		type = skip_typeref(orig_type);
	}

	size_t top_path_level = ARR_LEN(path->path);
	initializer_t **initializers = NEW_ARR_F(initializer_t*, 0);

	while (true) {
		designator_t *designator = NULL;
		if (peek('.') || peek('[')) {
			designator = parse_designation();
			goto finish_designator;
		} else if (peek(T_IDENTIFIER) && peek_ahead(':')) {
			/* GNU-style designator ("identifier: value") */
			if (!GNU_MODE)
				errorf(HERE, "designator with ':' is a GCC extension");
			designator         = allocate_ast_zero(sizeof(designator[0]));
			designator->pos    = *HERE;
			designator->symbol = token.base.symbol;
			eat(T_IDENTIFIER);
			eat(':');

finish_designator:
			/* reset path to toplevel, evaluate designator from there */
			ascend_to(path, top_path_level);
			/* can't continue after designation error */
			if (!walk_designator(path, designator, false))
				goto end_error;

			position_t const *const pos = &designator->pos;
			initializer_t *designator_initializer
				= allocate_initializer_zero(INITIALIZER_DESIGNATOR, pos);
			designator_initializer->designator.designator = designator;
			ARR_APP1(initializer_t*, initializers, designator_initializer);

			orig_type = path->top_type;
			type      = orig_type != NULL ? skip_typeref(orig_type) : NULL;
		}

		initializer_t *sub;

		if (peek('{')) {
			if (type != NULL && is_type_scalar(type)) {
				sub = parse_scalar_initializer(type, env->must_be_constant, false);
			} else {
				if (type == NULL) {
					if (env->entity != NULL) {
						errorf(HERE,
						       "extra brace group at end of initializer for '%N'",
						       env->entity);
					} else {
						errorf(HERE, "extra brace group at end of initializer");
					}
					eat('{');
				} else {
					eat('{');
					descend_into_subtype(path);
				}

				add_anchor_token('}');
				sub = parse_sub_initializer(path, orig_type, env);
				rem_anchor_token('}');

				expect('}');

				if (!type)
					goto error_parse_next;

				ascend_from_subtype(path);
			}
		} else {
			/* must be an expression */
			expression_t *expression = parse_assignment_expression();
			mark_vars_read(expression, NULL);

			if (type == NULL) {
				/* we are already outside, ... */
				if (outer_type == NULL)
					goto error_parse_next;
				type_t *const outer_type_skip = skip_typeref(outer_type);
				if (is_type_compound(outer_type_skip)
				 && !outer_type_skip->compound.compound->complete)
					goto error_parse_next;

excess_elements:;
				position_t const* const pos = &expression->base.pos;
				if (env->entity != NULL) {
					warningf(WARN_OTHER, pos,
					         "excess elements in initializer for '%N'",
					         env->entity);
				} else {
					warningf(WARN_OTHER, pos, "excess elements in initializer");
				}
				goto error_parse_next;
			}

			if (env->must_be_constant && !is_linker_constant(expression))
				errorf(&expression->base.pos,
				       "Initialisation expression '%E' is not constant",
				       expression);

			/* handle { "string" } special case */
			if (expression->kind == EXPR_STRING_LITERAL && outer_type != NULL) {
				result = initializer_from_expression(outer_type, expression);
				if (result != NULL) {
					accept(',');
					if (!peek('}'))
						warningf(WARN_OTHER, HERE,
						         "excessive elements in initializer for type '%T'",
						         outer_type);

					/* TODO: eat , ... */
					goto out;
				}
			}

			/* descend into subtypes until expression matches type */
			while (true) {
				orig_type = path->top_type;
				type      = skip_typeref(orig_type);

				sub = initializer_from_expression(orig_type, expression);
				if (sub != NULL)
					break;
				if (!is_initializable_type(type))
					goto end_error;
				if (is_type_scalar(type)) {
					errorf(&expression->base.pos,
					       "expression '%E' doesn't match expected type '%T'",
					       expression, orig_type);
					goto end_error;
				}

				descend_into_subtype(path);
				if (path->top_type == NULL)
					goto excess_elements;
			}
		}

		/* update largest index of top array */
		const type_path_entry_t *first      = &path->path[0];
		type_t                  *first_type = first->type;
		first_type                          = skip_typeref(first_type);
		if (is_type_array(first_type))
			path->size = MAX(path->size, first->v.index + 1);

		/* append to initializers list */
		ARR_APP1(initializer_t*, initializers, sub);

error_parse_next:
		if (!accept(','))
			break;
		if (peek('}'))
			break;

		if (type != NULL) {
			/* advance to the next declaration if we are not at the end */
			advance_current_object(path, top_path_level);
			orig_type = path->top_type;
			if (orig_type != NULL)
				type = skip_typeref(orig_type);
			else
				type = NULL;
		}
	}

	size_t const len = ARR_LEN(initializers);
	result = allocate_initializer_list(len, &env->pos);
	memcpy(&result->list.initializers, initializers,
	       len * sizeof(initializers[0]));
	goto out;

end_error:
	skip_initializers();
	if (result == NULL)
		result = allocate_initializer_list(0, &env->pos);
out:
	DEL_ARR_F(initializers);
	ascend_to(path, top_path_level+1);
	return result;
}

static expression_t *make_size_literal(size_t value)
{
	expression_t *literal = allocate_expression_zero(EXPR_LITERAL_INTEGER);
	literal->base.type    = type_size_t;

	char buf[128];
	snprintf(buf, sizeof(buf), "%u", (unsigned) value);
	literal->literal.value = make_string(buf);

	return literal;
}

/**
 * Parses an initializer. Parsers either a compound literal
 * (env->declaration == NULL) or an initializer of a declaration.
 */
static initializer_t *parse_initializer(parse_initializer_env_t *env)
{
	type_t        *type = skip_typeref(env->type);
	size_t         size = 0;
	initializer_t *result;

	if (is_type_scalar(type)) {
		result = parse_scalar_initializer(type, env->must_be_constant, true);
	} else if (accept('{')) {
		type_path_t path;
		memset(&path, 0, sizeof(path));
		path.top_type = env->type;
		path.path     = NEW_ARR_F(type_path_entry_t, 0);

		descend_into_subtype(&path);

		add_anchor_token('}');
		result = parse_sub_initializer(&path, env->type, env);
		rem_anchor_token('}');

		size = path.size;
		DEL_ARR_F(path.path);

		expect('}');
	} else {
		/* parse_scalar_initializer() also works in this case: we simply
		 * have an expression without {} around it */
		result = parse_scalar_initializer(type, env->must_be_constant, false);
	}

	/* §6.7.8:22 array initializers for arrays with unknown size determine
	 * the array type size */
	if (is_type_array(type) && type->array.size_expression == NULL
	 && result != NULL) {
		switch (result->kind) {
		case INITIALIZER_LIST:
			break;

		case INITIALIZER_STRING: {
			size = get_string_len(get_init_string(result)->value) + 1;
			break;
		}

		case INITIALIZER_DESIGNATOR:
		case INITIALIZER_VALUE:
			/* can happen for parse errors */
			break;

		default:
			panic("invalid initializer type");
		}

		type_t *new_type = duplicate_type(type);

		new_type->array.size_expression   = make_size_literal(size);
		new_type->array.size_constant     = true;
		new_type->array.has_implicit_size = true;
		new_type->array.size              = size;
		env->type = new_type;
	}

	return result;
}

static void append_entity(scope_t *scope, entity_t *entity)
{
	if (scope->last_entity != NULL) {
		scope->last_entity->base.next = entity;
	} else {
		scope->first_entity = entity;
	}
	entity->base.parent_entity = current_entity;
	scope->last_entity         = entity;
}


static type_t *parse_compound_type_specifier(bool const is_struct)
{
	position_t const pos = *HERE;
	eat(is_struct ? T_struct : T_union);

	symbol_t    *symbol     = NULL;
	entity_t    *entity     = NULL;
	attribute_t *attributes = NULL;

	if (peek(T___attribute__))
		attributes = parse_attributes(NULL);

	entity_kind_t const kind = is_struct ? ENTITY_STRUCT : ENTITY_UNION;
	if (peek(T_IDENTIFIER)) {
		/* the compound has a name, check if we have seen it already */
		symbol = token.base.symbol;
		entity = get_tag(symbol, kind, &pos);
		eat(T_IDENTIFIER);

		if (entity != NULL) {
			if (entity->base.parent_scope != current_scope
			 && (peek('{') || peek(';'))) {
				/* we're in an inner scope and have a definition. Shadow
				 * existing definition in outer scope */
				entity = NULL;
			} else if (entity->compound.complete && peek('{')) {
				errorf(&pos, "multiple definitions of '%N'", entity);
				note_prev_decl(entity);
				/* clear members in the hope to avoid further errors */
				entity->compound.members.first_entity = NULL;
			}
		}
	} else if (!peek('{')) {
		char const *const msg = is_struct ? "struct type specifier"
		                                  : "union type specifier";
		parse_error_expected(msg, T_IDENTIFIER, '{', NULL);

		return NULL;
	}

	if (entity == NULL) {
		entity = allocate_entity_zero(kind, NAMESPACE_TAG, symbol, &pos);
		entity->compound.alignment = 1;
		entity->base.parent_scope  = current_scope;
		if (symbol != NULL)
			environment_push(entity);
		append_entity(current_scope, entity);
	}

	if (peek('{')) {
		parse_compound_type_entries(&entity->compound);
		attributes = parse_attributes(attributes);

		if (attributes != NULL) {
			entity->compound.attributes = attributes;
			handle_entity_attributes(attributes, entity);
		}

		layout_compound(&entity->compound);

		/* ISO/IEC 14882:1998(E) §7.1.3:5 */
		if (symbol == NULL) {
			assert(anonymous_entity == NULL);
			anonymous_entity = entity;
		}
	}

	type_t *const type = allocate_type_zero(is_struct ? TYPE_COMPOUND_STRUCT
	                                                  : TYPE_COMPOUND_UNION);
	type->compound.compound = &entity->compound;
	return type;
}

static void parse_enum_entries(enum_t *const enume)
{
	eat('{');

	if (peek('}')) {
		errorf(HERE, "empty enum not allowed");
		eat('}');
		return;
	}

	add_anchor_token('}');
	add_anchor_token(',');
	do {
		add_anchor_token('=');
		position_t pos;
		symbol_t *const symbol = expect_identifier("enum entry", &pos);
		entity_t *const entity = allocate_entity_zero(ENTITY_ENUM_VALUE, NAMESPACE_NORMAL, symbol, &pos);
		entity->enum_value.enume = enume;
		if (enume->first_value == NULL)
			enume->first_value = entity;
		rem_anchor_token('=');

		if (accept('=')) {
			expression_t *value
				= parse_integer_constant_expression("enumeration value");
			if (value->base.type != type_error_type) {
				value = create_implicit_cast(value, type_int);
			} else {
				enume->error = true;
			}
			entity->enum_value.value = value;
		}

		record_entity(entity, true);
	} while (accept(',') && !peek('}'));
	rem_anchor_token(',');
	rem_anchor_token('}');
	expect('}');
}

static type_t *parse_enum_specifier(void)
{
	position_t const pos = *HERE;
	entity_t        *entity;
	symbol_t        *symbol;

	eat(T_enum);
	switch (token.kind) {
	case T_IDENTIFIER:
		symbol = token.base.symbol;
		entity = get_tag(symbol, ENTITY_ENUM, &pos);
		eat(T_IDENTIFIER);

		if (entity != NULL) {
			if (entity->base.parent_scope != current_scope
			 && (peek('{') || peek(';'))) {
				/* we're in an inner scope and have a definition. Shadow
				 * existing definition in outer scope */
				entity = NULL;
			} else if (entity->enume.complete && peek('{')) {
				errorf(&pos, "multiple definitions of '%N'", entity);
				note_prev_decl(entity);
			}
		}
		break;

	case '{':
		entity = NULL;
		symbol = NULL;
		break;

	default:
		parse_error_expected("enum type specifier", T_IDENTIFIER, '{', NULL);
		return NULL;
	}

	if (entity == NULL) {
		entity = allocate_entity_zero(ENTITY_ENUM, NAMESPACE_TAG, symbol, &pos);
		entity->base.parent_scope = current_scope;
		entity->enume.akind       = ATOMIC_TYPE_INT;
		append_entity(current_scope, entity);
		if (symbol != NULL)
			environment_push(entity);
	}

	if (peek('{')) {
		entity->enume.complete = true;

		parse_enum_entries(&entity->enume);
		parse_attributes(NULL);

		/* ISO/IEC 14882:1998(E) §7.1.3:5 */
		if (symbol == NULL) {
			assert(anonymous_entity == NULL);
			anonymous_entity = entity;
		}

		/* choose base kind */
		bool has_negative_value = false;
		for (const entity_t *entry = entity->enume.first_value;
		     entry != NULL && entry->kind == ENTITY_ENUM_VALUE;
			 entry = entry->base.next) {
			ir_tarval *val = get_enum_value(&entry->enum_value);
			/* in error cases val may be NULL */
			if (val == NULL)
				continue;
			if (tarval_is_negative(val)) {
				has_negative_value = true;
				break;
			}
		}
		if (!has_negative_value)
			entity->enume.akind = ATOMIC_TYPE_UINT;
	} else if (!entity->enume.complete && !dialect.gnu) {
		errorf(&pos,
			   "'%N' used before definition (incomplete enums are a GNU extension)",
			   entity);
	}

	type_t *type = allocate_type_zero(TYPE_ENUM);
	type->enumt.enume      = &entity->enume;
	type->enumt.base.akind = entity->enume.akind;
	return type;
}

/**
 * if a symbol is a typedef to another type, return true
 */
static bool is_typedef_symbol(symbol_t *symbol)
{
	const entity_t *const entity = get_entity(symbol, NAMESPACE_NORMAL);
	return entity != NULL && entity->kind == ENTITY_TYPEDEF;
}

static bool is_declaration_specifier(token_t const *const tk)
{
	switch (tk->kind) {
	case DECLARATION_SPECIFIERS: return true;
	case T_IDENTIFIER:           return is_typedef_symbol(tk->base.symbol);
	default:                     return false;
	}
}

static bool starts_declaration(token_t const *const tk)
{
	switch (tk->kind) {
	case DECLARATION_START: return true;
	case T_IDENTIFIER:      return is_typedef_symbol(tk->base.symbol);
	default:                return false;
	}
}

static type_t *parse_typeof(void)
{
	eat(T___typeof__);

	add_anchor_token(')');
	expect('(');

	expression_t *expression;
	type_t       *type;
	if (is_declaration_specifier(&token)) {
		expression = NULL;
		type       = parse_typename();
	} else {
		expression = parse_expression();
		type       = revert_automatic_type_conversion(expression);
	}

	rem_anchor_token(')');
	expect(')');

	type_t *typeof_type              = allocate_type_zero(TYPE_TYPEOF);
	typeof_type->typeoft.expression  = expression;
	typeof_type->typeoft.typeof_type = type;

	return typeof_type;
}

typedef enum specifiers_t {
	SPECIFIER_NONE      = 0,
	SPECIFIER_SIGNED    = 1 << 0,
	SPECIFIER_UNSIGNED  = 1 << 1,
	SPECIFIER_LONG      = 1 << 2,
	SPECIFIER_INT       = 1 << 3,
	SPECIFIER_DOUBLE    = 1 << 4,
	SPECIFIER_CHAR      = 1 << 5,
	SPECIFIER_WCHAR_T   = 1 << 6,
	SPECIFIER_SHORT     = 1 << 7,
	SPECIFIER_LONG_LONG = 1 << 8,
	SPECIFIER_FLOAT     = 1 << 9,
	SPECIFIER_BOOL      = 1 << 10,
	SPECIFIER_INT8      = 1 << 11,
	SPECIFIER_INT16     = 1 << 12,
	SPECIFIER_INT32     = 1 << 13,
	SPECIFIER_INT64     = 1 << 14,
	SPECIFIER_COMPLEX   = 1 << 15,
	SPECIFIER_IMAGINARY = 1 << 16,
} specifiers_t;

static entity_t *create_error_entity(symbol_t *const symbol,
                                     entity_kind_t const kind)
{
	entity_t *const entity
		= allocate_entity_zero(kind, NAMESPACE_NORMAL, symbol, HERE);
	if (is_declaration(entity)) {
		entity->declaration.type     = type_error_type;
		entity->declaration.implicit = true;
	}
	if (kind != ENTITY_COMPOUND_MEMBER)
		record_entity(entity, false);
	return entity;
}

static type_t *parse_typedef_name(void)
{
	symbol_t *const symbol = token.base.symbol;
	entity_t *const entity = get_entity(symbol, NAMESPACE_NORMAL);
	if (entity && entity->kind == ENTITY_TYPEDEF) {
		type_t *type             = allocate_type_zero(TYPE_TYPEDEF);
		type->typedeft.typedefe  = &entity->declaration;
		entity->declaration.used = true;
		return type;
	}

	/* Be somewhat resilient to typos like 'vodi f()' at the beginning of a
	 * declaration, so it doesn't generate 'implicit int' followed by more
	 * errors later on. */
	token_kind_t const la1_type = look_ahead(1)->kind;
	switch (la1_type) {
	case DECLARATION_SPECIFIERS:
	case T_IDENTIFIER:
	case '&':
	case '*':
		break;

	default:
		return NULL;
	}

	/* Error handling: Look in the tag namespace if no typedef name was found. */
	entity_t *const tagent = get_entity(symbol, NAMESPACE_TAG);
	if (tagent) {
		errorf(HERE, "'%Y' does not name a type; assuming '%N'", symbol, tagent);
		switch (tagent->kind) {
		case ENTITY_ENUM: {
			type_t *const type = allocate_type_zero(TYPE_ENUM);
			type->enumt.enume      = &tagent->enume;
			type->enumt.base.akind = ATOMIC_TYPE_INT;
			return type;
		}

		case ENTITY_STRUCT: {
			type_t *const type = allocate_type_zero(TYPE_COMPOUND_STRUCT);
			type->compound.compound = &tagent->compound;
			return type;
		}

		case ENTITY_UNION: {
			type_t *const type = allocate_type_zero(TYPE_COMPOUND_UNION);
			type->compound.compound = &tagent->compound;
			return type;
		}

		default:
			panic("invalid entity kind");
		}
	} else {
		errorf(HERE, "'%Y' does not name a type", symbol);
		entity_t *const errent = create_error_entity(symbol, ENTITY_TYPEDEF);
		type_t   *const type   = allocate_type_zero(TYPE_TYPEDEF);
		type->typedeft.typedefe = &errent->declaration;
		return type;
	}
}

static attribute_t *parse_attribute_ms_property(attribute_t *attribute)
{
	attribute_property_argument_t *const property
		= allocate_ast_zero(sizeof(*property));

	add_anchor_token(')');
	add_anchor_token(',');
	expect('(');

	char const *const context = "property declspec";
	do {
		add_anchor_token('=');
		position_t pos;
		symbol_t *const prop_sym = expect_identifier(context, &pos);
		rem_anchor_token('=');

		symbol_t **prop = NULL;
		if (streq(prop_sym->string, "put")) {
			prop = &property->put_symbol;
		} else if (streq(prop_sym->string, "get")) {
			prop = &property->get_symbol;
		} else if (prop_sym != sym_anonymous) {
			errorf(&pos,
				   "expected put or get in property declspec, but got '%Y'",
				   prop_sym);
		}

		add_anchor_token(T_IDENTIFIER);
		expect('=');
		rem_anchor_token(T_IDENTIFIER);

		symbol_t *const id = expect_identifier(context, NULL);
		if (prop)
			*prop = id;
	} while (accept(','));
	rem_anchor_token(',');
	rem_anchor_token(')');

	attribute->a.property = property;

	expect(')');
	return attribute;
}

static attribute_t *parse_microsoft_extended_decl_modifier_single(void)
{
	/* parse "any-word" */
	symbol_t const *const symbol = token.base.symbol;
	if (!symbol) {
		parse_error_expected("__declspec", T_IDENTIFIER, NULL);
		return NULL;
	}

	attribute_kind_t  kind;
	char const *const name = symbol->string;
	for (attribute_kind_t k = ATTRIBUTE_MS_FIRST;; ++k) {
		if (k > ATTRIBUTE_MS_LAST) {
			warningf(WARN_ATTRIBUTE, HERE, "unknown __declspec '%s' ignored", name);
			kind = ATTRIBUTE_UNKNOWN;
			break;
		}

		char const *const attribute_name = get_attribute_name(k);
		if (attribute_name && streq(attribute_name, name)) {
			kind = k;
			break;
		}
	}

	attribute_t *attribute = allocate_attribute_zero(kind);
	next_token();

	if (kind == ATTRIBUTE_MS_PROPERTY)
		return parse_attribute_ms_property(attribute);

	/* parse arguments */
	if (accept('('))
		attribute->a.arguments = parse_attribute_arguments();

	return attribute;
}

static attribute_t *parse_microsoft_extended_decl_modifier(attribute_t *first)
{
	eat(T__declspec);

	add_anchor_token(')');
	expect('(');
	if (!peek(')')) {
		attribute_t **anchor = &first;
		do {
			while (*anchor != NULL)
				anchor = &(*anchor)->next;

			attribute_t *attribute
				= parse_microsoft_extended_decl_modifier_single();
			if (attribute == NULL)
				break;

			*anchor = attribute;
			anchor  = &attribute->next;
		} while (accept(','));
	}
	rem_anchor_token(')');
	expect(')');
	return first;
}

static void parse_declaration_specifiers(declaration_specifiers_t *specifiers)
{
	type_t            *type            = NULL;
	type_qualifiers_t  qualifiers      = TYPE_QUALIFIER_NONE;
	unsigned           type_specifiers = 0;
	bool               newtype         = false;

	memset(specifiers, 0, sizeof(*specifiers));
	specifiers->pos = *HERE;

	while (true) {
		specifiers->attributes = parse_attributes(specifiers->attributes);

		switch (token.kind) {
		/* storage class */
#define MATCH_STORAGE_CLASS(token, class)                                  \
		case token:                                                        \
			if (specifiers->storage_class != STORAGE_CLASS_NONE)           \
				errorf(HERE, "multiple storage classes in declaration specifiers"); \
			specifiers->storage_class = class;                             \
			if (specifiers->thread_local)                                  \
				goto check_thread_storage_class;                           \
			eat(token); \
			break;

		MATCH_STORAGE_CLASS(T_typedef,  STORAGE_CLASS_TYPEDEF)
		MATCH_STORAGE_CLASS(T_extern,   STORAGE_CLASS_EXTERN)
		MATCH_STORAGE_CLASS(T_static,   STORAGE_CLASS_STATIC)
		MATCH_STORAGE_CLASS(T_auto,     STORAGE_CLASS_AUTO)
		MATCH_STORAGE_CLASS(T_register, STORAGE_CLASS_REGISTER)

		case T__declspec:
			specifiers->attributes
				= parse_microsoft_extended_decl_modifier(specifiers->attributes);
			break;

		case T__Thread_local:
			if (specifiers->thread_local) {
				errorf(HERE, "duplicate %K", &token);
			} else {
				specifiers->thread_local = true;
check_thread_storage_class:
				switch (specifiers->storage_class) {
				case STORAGE_CLASS_EXTERN:
				case STORAGE_CLASS_NONE:
				case STORAGE_CLASS_STATIC:
					break;

					char const* wrong;
				case STORAGE_CLASS_AUTO:     wrong = "auto";     goto wrong_thread_storage_class;
				case STORAGE_CLASS_REGISTER: wrong = "register"; goto wrong_thread_storage_class;
				case STORAGE_CLASS_TYPEDEF:  wrong = "typedef";  goto wrong_thread_storage_class;
wrong_thread_storage_class:
					errorf(HERE, "%K used with '%s'", &token, wrong);
					break;
				}
			}
			next_token();
			break;

		/* type qualifiers */
#define MATCH_TYPE_QUALIFIER(token, qualifier)                          \
		case token:                                                     \
			qualifiers |= qualifier;                                    \
			eat(token); \
			break

		MATCH_TYPE_QUALIFIER(T_const,    TYPE_QUALIFIER_CONST);
		MATCH_TYPE_QUALIFIER(T_restrict, TYPE_QUALIFIER_RESTRICT);
		MATCH_TYPE_QUALIFIER(T_volatile, TYPE_QUALIFIER_VOLATILE);
		MATCH_TYPE_QUALIFIER(T__w64,     TYPE_QUALIFIER_W64);
		MATCH_TYPE_QUALIFIER(T___ptr32,  TYPE_QUALIFIER_PTR32);
		MATCH_TYPE_QUALIFIER(T___ptr64,  TYPE_QUALIFIER_PTR64);
		MATCH_TYPE_QUALIFIER(T___uptr,   TYPE_QUALIFIER_UPTR);
		MATCH_TYPE_QUALIFIER(T___sptr,   TYPE_QUALIFIER_SPTR);

		{ /* type specifiers */
			specifiers_t specifier;
		case T__Bool:      specifier = SPECIFIER_BOOL;      goto check_specifier;
		case T__Complex:   specifier = SPECIFIER_COMPLEX;   goto check_specifier;
		case T__Imaginary: specifier = SPECIFIER_IMAGINARY; goto check_specifier;
		case T__int16:     specifier = SPECIFIER_INT16;     goto check_specifier;
		case T__int32:     specifier = SPECIFIER_INT32;     goto check_specifier;
		case T__int64:     specifier = SPECIFIER_INT64;     goto check_specifier;
		case T__int8:      specifier = SPECIFIER_INT8;      goto check_specifier;
		case T_bool:       specifier = SPECIFIER_BOOL;      goto check_specifier;
		case T_char:       specifier = SPECIFIER_CHAR;      goto check_specifier;
		case T_double:     specifier = SPECIFIER_DOUBLE;    goto check_specifier;
		case T_float:      specifier = SPECIFIER_FLOAT;     goto check_specifier;
		case T_int:        specifier = SPECIFIER_INT;       goto check_specifier;
		case T_short:      specifier = SPECIFIER_SHORT;     goto check_specifier;
		case T_signed:     specifier = SPECIFIER_SIGNED;    goto check_specifier;
		case T_unsigned:   specifier = SPECIFIER_UNSIGNED;  goto check_specifier;
		case T_wchar_t:    specifier = SPECIFIER_WCHAR_T;   goto check_specifier;
check_specifier:
			if (type_specifiers & specifier)
				errorf(HERE, "multiple %K type specifiers given", &token);
			type_specifiers |= specifier;
			next_token();
			break;
		}

		case T_inline:
			eat(T_inline);
			specifiers->is_inline = true;
			break;

		case T_long:
			if (type_specifiers & SPECIFIER_LONG_LONG) {
				errorf(HERE, "too many long type specifiers given");
			} else if (type_specifiers & SPECIFIER_LONG) {
				type_specifiers |= SPECIFIER_LONG_LONG;
			} else {
				type_specifiers |= SPECIFIER_LONG;
			}
			eat(T_long);
			break;

#define CHECK_DOUBLE_TYPE() \
	(type != NULL ? errorf(HERE, "multiple types in declaration specifiers") : (void)0)

		case T_struct:
			CHECK_DOUBLE_TYPE();
			newtype = true;
			type    = parse_compound_type_specifier(true);
			break;
		case T_union:
			CHECK_DOUBLE_TYPE();
			newtype = true;
			type    = parse_compound_type_specifier(false);
			break;
		case T_enum:
			CHECK_DOUBLE_TYPE();
			newtype = true;
			type    = parse_enum_specifier();
			break;
		case T___typeof__:
			CHECK_DOUBLE_TYPE();
			newtype = true;
			type    = parse_typeof();
			break;
		case T___builtin_va_list:
			CHECK_DOUBLE_TYPE();
			newtype = false;
			type    = type_valist;
			eat(T___builtin_va_list);
			break;

		case T_void:
			CHECK_DOUBLE_TYPE();
			eat(T_void);
			newtype = false;
			type    = type_void;
			break;

		case T_IDENTIFIER: {
			/* only parse identifier if we haven't found a type yet */
			if (type != NULL || type_specifiers != 0) {
				/* Be somewhat resilient to typos like 'unsigned lng* f()' in a
				 * declaration, so it doesn't generate errors about expecting
				 * '(' or '{' later on. */
				switch (look_ahead(1)->kind) {
				case DECLARATION_START_NO_ATTRIBUTE:
				case T_IDENTIFIER:
				case '&':
				case '*':
					errorf(HERE,
					       "discarding stray %K in declaration specifier",
					       &token);
					eat(T_IDENTIFIER);
					continue;

				default:
					goto finish_specifiers;
				}
			}

			newtype = true;
			type    = parse_typedef_name();
			if (!type)
				goto finish_specifiers;

			eat(T_IDENTIFIER);
			break;
		}

		/* function specifier */
		default:
			goto finish_specifiers;
		}
	}

finish_specifiers:
	specifiers->attributes = parse_attributes(specifiers->attributes);

	position_t const* const pos = &specifiers->pos;
	if (type_specifiers == SPECIFIER_NONE) {
		if (!type) {
			/* ISO/IEC 14882:1998(E) §C.1.5:4 */
			if (dialect.cpp || dialect.strict) {
				errorf(pos, "type specifier missing");
				goto error_type;
			}
			warningf(WARN_IMPLICIT_INT, pos,
			         "type specifier missing; assuming 'int'");
			newtype = false;
			type    = type_int;
		}
	} else if (!type || !is_type_valid(skip_typeref(type))) {
		atomic_type_kind_t atomic_type;

		/* match valid basic types */
		switch (type_specifiers & ~(SPECIFIER_COMPLEX|SPECIFIER_IMAGINARY)) {
		case SPECIFIER_WCHAR_T:
			atomic_type = ATOMIC_TYPE_WCHAR_T;
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
			goto warn_about_long_long;

		case SPECIFIER_UNSIGNED | SPECIFIER_LONG | SPECIFIER_LONG_LONG:
		case SPECIFIER_UNSIGNED | SPECIFIER_LONG | SPECIFIER_LONG_LONG
			| SPECIFIER_INT:
			atomic_type = ATOMIC_TYPE_ULONGLONG;
warn_about_long_long:
			warningf(WARN_LONG_LONG, pos,
			         "ISO C90 does not support 'long long'");
			break;

		case SPECIFIER_UNSIGNED | SPECIFIER_INT8:
			atomic_type = unsigned_int8_type_kind;
			break;

		case SPECIFIER_UNSIGNED | SPECIFIER_INT16:
			atomic_type = unsigned_int16_type_kind;
			break;

		case SPECIFIER_UNSIGNED | SPECIFIER_INT32:
			atomic_type = unsigned_int32_type_kind;
			break;

		case SPECIFIER_UNSIGNED | SPECIFIER_INT64:
			atomic_type = unsigned_int64_type_kind;
			break;

		case SPECIFIER_INT8:
		case SPECIFIER_SIGNED | SPECIFIER_INT8:
			atomic_type = int8_type_kind;
			break;

		case SPECIFIER_INT16:
		case SPECIFIER_SIGNED | SPECIFIER_INT16:
			atomic_type = int16_type_kind;
			break;

		case SPECIFIER_INT32:
		case SPECIFIER_SIGNED | SPECIFIER_INT32:
			atomic_type = int32_type_kind;
			break;

		case SPECIFIER_INT64:
		case SPECIFIER_SIGNED | SPECIFIER_INT64:
			atomic_type = int64_type_kind;
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
			if (type_specifiers & (SPECIFIER_COMPLEX|SPECIFIER_IMAGINARY)) {
				if (type_specifiers & SPECIFIER_COMPLEX)
					errorf(pos, "_Complex specifier is invalid for _Bool");
				if (type_specifiers & SPECIFIER_IMAGINARY)
					errorf(pos, "_Imaginary specifier is invalid for _Bool");
				type_specifiers &= ~(SPECIFIER_COMPLEX|SPECIFIER_IMAGINARY);
			}
			atomic_type = ATOMIC_TYPE_BOOL;
			break;

		case SPECIFIER_NONE:
			warningf(WARN_OTHER, pos,
			         "plain complex type requires a type specifier; assuming 'double'");
			atomic_type = ATOMIC_TYPE_DOUBLE;
			break;

		default:
			/* invalid specifier combination, give an error message */
			if ((type_specifiers & SPECIFIER_SIGNED)
			 && (type_specifiers & SPECIFIER_UNSIGNED)) {
				errorf(pos, "signed and unsigned specifiers given");
			} else if (type_specifiers & (SPECIFIER_SIGNED|SPECIFIER_UNSIGNED)) {
				errorf(pos, "only integer types can be signed or unsigned");
			} else {
				errorf(pos, "invalid type specifier");
			}
			goto error_type;
		}

		switch (type_specifiers & (SPECIFIER_COMPLEX | SPECIFIER_IMAGINARY)) {
		case SPECIFIER_COMPLEX:   type = allocate_type_zero(TYPE_COMPLEX);   break;
		case SPECIFIER_IMAGINARY: type = allocate_type_zero(TYPE_IMAGINARY); break;
		case SPECIFIER_NONE:      type = allocate_type_zero(TYPE_ATOMIC);    break;

		default:
			errorf(pos, "type cannot be both complex and imaginary");
error_type:
			specifiers->type = type_error_type;
			return;
		}

		type->atomic.akind = atomic_type;
		newtype = true;
	} else {
		errorf(&specifiers->pos, "multiple types specified");
	}

	if (qualifiers != TYPE_QUALIFIER_NONE) {
		if (!newtype) {
			type    = duplicate_type(type);
			newtype = true;
		}
		/* FIXME: check type qualifiers here */
		type->base.qualifiers = qualifiers;
	}
	if (newtype)
		type = identify_new_type(type);

	if (specifiers->attributes != NULL)
		type = handle_type_attributes(specifiers->attributes, type);
	specifiers->type = type;
}

static type_qualifiers_t parse_type_qualifiers(void)
{
	type_qualifiers_t qualifiers = TYPE_QUALIFIER_NONE;

	while (true) {
		switch (token.kind) {
		/* type qualifiers */
		MATCH_TYPE_QUALIFIER(T_const,    TYPE_QUALIFIER_CONST);
		MATCH_TYPE_QUALIFIER(T_restrict, TYPE_QUALIFIER_RESTRICT);
		MATCH_TYPE_QUALIFIER(T_volatile, TYPE_QUALIFIER_VOLATILE);
		/* microsoft extended type modifiers */
		MATCH_TYPE_QUALIFIER(T__w64,     TYPE_QUALIFIER_W64);
		MATCH_TYPE_QUALIFIER(T___ptr32,  TYPE_QUALIFIER_PTR32);
		MATCH_TYPE_QUALIFIER(T___ptr64,  TYPE_QUALIFIER_PTR64);
		MATCH_TYPE_QUALIFIER(T___uptr,   TYPE_QUALIFIER_UPTR);
		MATCH_TYPE_QUALIFIER(T___sptr,   TYPE_QUALIFIER_SPTR);

		default:
			return qualifiers;
		}
	}
}

/**
 * Parses an K&R identifier list
 */
static void parse_identifier_list(scope_t *scope)
{
	assert(peek(T_IDENTIFIER));
	do {
		entity_t *const entity = allocate_entity_zero(ENTITY_PARAMETER, NAMESPACE_NORMAL, token.base.symbol, HERE);
		/* a K&R parameter has no type, yet */
		eat(T_IDENTIFIER);

		if (scope != NULL)
			append_entity(scope, entity);
	} while (accept(',') && peek(T_IDENTIFIER));
}

static entity_t *parse_parameter(void)
{
	declaration_specifiers_t specifiers;
	parse_declaration_specifiers(&specifiers);

	entity_t *entity = parse_declarator(&specifiers,
			DECL_MAY_BE_ABSTRACT | DECL_IS_PARAMETER);
	anonymous_entity = NULL;
	return entity;
}

static void semantic_parameter_complete(const entity_t *entity)
{
	assert(entity->kind == ENTITY_PARAMETER);

	/* §6.7.5.3:4  After adjustment, the parameters in a parameter type
	 *             list in a function declarator that is part of a
	 *             definition of that function shall not have
	 *             incomplete type. */
	type_t *type = skip_typeref(entity->declaration.type);
	if (!is_type_complete(type) && is_type_valid(type))
		errorf(&entity->base.pos, "'%N' has incomplete type", entity);
}

static bool has_parameters(void)
{
	/* func(void) is not a parameter */
	if (!peek_ahead(')'))
		return true;
	if (peek(T_IDENTIFIER)) {
		entity_t const *const entity = get_entity(token.base.symbol, NAMESPACE_NORMAL);
		if (entity == NULL)
			return true;
		if (entity->kind != ENTITY_TYPEDEF)
			return true;
		type_t const *const type = skip_typeref(entity->declaration.type);
		if (!is_type_void(type))
			return true;
		if (dialect.cpp) {
			/* ISO/IEC 14882:1998(E) §8.3.5:2  It must be literally (void).  A typedef
			 * is not allowed. */
			errorf(HERE, "empty parameter list defined with a typedef of 'void' not allowed in C++");
		} else if (type->base.qualifiers != TYPE_QUALIFIER_NONE) {
			/* §6.7.5.3:10  Qualification is not allowed here. */
			errorf(HERE, "'void' as parameter must not have type qualifiers");
		}
	} else if (!peek(T_void)) {
		return true;
	}
	next_token();
	return false;
}

/**
 * Parses function type parameters (and optionally creates variable_t entities
 * for them in a scope)
 */
static void parse_parameters(function_type_t *type, scope_t *scope)
{
	add_anchor_token(')');
	eat('(');

	if (peek(T_IDENTIFIER)
	 && !is_typedef_symbol(token.base.symbol)
	 && (peek_ahead(',') || peek_ahead(')'))) {
		type->kr_style_parameters = true;
		parse_identifier_list(scope);
	} else if (peek(')')) {
		/* ISO/IEC 14882:1998(E) §C.1.6:1 */
		if (!dialect.cpp)
			type->unspecified_parameters = true;
	} else if (has_parameters()) {
		function_parameter_t **anchor = &type->parameters;
		add_anchor_token(',');
		do {
			switch (token.kind) {
			case T_DOTDOTDOT:
				eat(T_DOTDOTDOT);
				type->variadic = true;
				goto parameters_finished;

			case T_IDENTIFIER:
			case DECLARATION_START: {
				entity_t *entity = parse_parameter();
				if (entity->kind == ENTITY_TYPEDEF) {
					errorf(&entity->base.pos,
							"typedef not allowed as function parameter");
					break;
				}
				assert(is_declaration(entity));

				semantic_parameter_complete(entity);

				function_parameter_t *const parameter =
					allocate_parameter(entity->declaration.type);

				if (scope != NULL)
					append_entity(scope, entity);

				*anchor = parameter;
				anchor  = &parameter->next;
				break;
			}

			default:
				goto parameters_finished;
			}
		} while (accept(','));
parameters_finished:
		rem_anchor_token(',');
	}

	rem_anchor_token(')');
	expect(')');
}

typedef enum construct_type_kind_t {
	CONSTRUCT_POINTER = 1,
	CONSTRUCT_REFERENCE,
	CONSTRUCT_FUNCTION,
	CONSTRUCT_ARRAY
} construct_type_kind_t;

typedef union construct_type_t construct_type_t;

typedef struct construct_type_base_t {
	construct_type_kind_t kind;
	position_t            pos;
	construct_type_t     *next;
} construct_type_base_t;

typedef struct parsed_pointer_t {
	construct_type_base_t base;
	type_qualifiers_t     type_qualifiers;
} parsed_pointer_t;

typedef struct parsed_reference_t {
	construct_type_base_t base;
} parsed_reference_t;

typedef struct construct_function_type_t {
	construct_type_base_t  base;
	type_t                *function_type;
} construct_function_type_t;

typedef struct parsed_array_t {
	construct_type_base_t  base;
	type_qualifiers_t      type_qualifiers;
	bool                   is_static;
	bool                   is_variable;
	expression_t          *size;
} parsed_array_t;

union construct_type_t {
	construct_type_kind_t     kind;
	construct_type_base_t     base;
	parsed_pointer_t          pointer;
	parsed_reference_t        reference;
	construct_function_type_t function;
	parsed_array_t            array;
};

static construct_type_t *allocate_declarator_zero(construct_type_kind_t const kind, size_t const size)
{
	construct_type_t *const cons = obstack_alloc(&temp_obst, size);
	memset(cons, 0, size);
	cons->kind     = kind;
	cons->base.pos = *HERE;
	return cons;
}

/* §6.7.5.1 */
static construct_type_t *parse_pointer_declarator(void)
{
	construct_type_t *const cons = allocate_declarator_zero(CONSTRUCT_POINTER, sizeof(parsed_pointer_t));
	eat('*');
	cons->pointer.type_qualifiers = parse_type_qualifiers();

	return cons;
}

/* ISO/IEC 14882:1998(E) §8.3.2 */
static construct_type_t *parse_reference_declarator(void)
{
	if (!dialect.cpp)
		errorf(HERE, "references are only available for C++");

	construct_type_t *const cons = allocate_declarator_zero(CONSTRUCT_REFERENCE, sizeof(parsed_reference_t));
	eat('&');

	return cons;
}

/* §6.7.5.2 */
static construct_type_t *parse_array_declarator(void)
{
	construct_type_t *const cons  = allocate_declarator_zero(CONSTRUCT_ARRAY, sizeof(parsed_array_t));
	parsed_array_t   *const array = &cons->array;

	eat('[');
	add_anchor_token(']');

	bool is_static = accept(T_static);

	type_qualifiers_t type_qualifiers = parse_type_qualifiers();

	if (!is_static)
		is_static = accept(T_static);

	array->type_qualifiers = type_qualifiers;
	array->is_static       = is_static;

	expression_t *size = NULL;
	if (peek('*') && peek_ahead(']')) {
		array->is_variable = true;
		eat('*');
	} else if (!peek(']')) {
		size = parse_assignment_expression();

		/* §6.7.5.2:1  Array size must have integer type */
		type_t *const orig_type = size->base.type;
		type_t *const type      = skip_typeref(orig_type);
		if (!is_type_integer(type) && is_type_valid(type))
			errorf(&size->base.pos,
			       "array size '%E' must have integer type but has type '%T'",
			       size, orig_type);

		array->size = size;
		mark_vars_read(size, NULL);
	}

	if (is_static && size == NULL)
		errorf(&array->base.pos, "static array parameters require a size");

	rem_anchor_token(']');
	expect(']');
	return cons;
}

/* §6.7.5.3 */
static construct_type_t *parse_function_declarator(scope_t *scope)
{
	construct_type_t *const cons = allocate_declarator_zero(CONSTRUCT_FUNCTION, sizeof(construct_function_type_t));

	type_t          *type  = allocate_type_zero(TYPE_FUNCTION);
	function_type_t *ftype = &type->function;

	ftype->linkage            = current_linkage;
	ftype->calling_convention = CC_DEFAULT;

	parse_parameters(ftype, scope);

	cons->function.function_type = type;

	return cons;
}

typedef struct parse_declarator_env_t {
	bool              may_be_abstract : 1;
	bool              must_be_abstract : 1;
	decl_modifiers_t  modifiers;
	symbol_t         *symbol;
	position_t        pos;
	scope_t           parameters;
	attribute_t      *attributes;
} parse_declarator_env_t;

/* §6.7.5 */
static construct_type_t *parse_inner_declarator(parse_declarator_env_t *env)
{
	/* construct a single linked list of construct_type_t's which describe
	 * how to construct the final declarator type */
	construct_type_t  *first  = NULL;
	construct_type_t **anchor = &first;

	env->attributes = parse_attributes(env->attributes);

	for (;;) {
		construct_type_t *type;
		switch (token.kind) {
		case '&':
			type = parse_reference_declarator();
			break;

		case T__based:
			errorf(HERE, "%K not supported", &token);
			eat(T__based);
			add_anchor_token(')');
			add_anchor_token(T_IDENTIFIER);
			expect('(');
			rem_anchor_token(T_IDENTIFIER);
			expect_identifier("based pointer", NULL);
			rem_anchor_token(')');
			expect(')');
			continue;

		case '*':
			type = parse_pointer_declarator();
			break;

		default:
			goto ptr_operator_end;
		}

		*anchor = type;
		anchor  = &type->base.next;

		/* TODO: find out if this is correct */
		env->attributes = parse_attributes(env->attributes);
	}

ptr_operator_end: ;
	construct_type_t *inner_types = NULL;

	switch (token.kind) {
	case T_IDENTIFIER:
		if (env->must_be_abstract) {
			errorf(HERE, "type name must not have an identifier");
		} else {
			env->symbol = token.base.symbol;
			env->pos    = *HERE;
		}
		eat(T_IDENTIFIER);
		break;

	case '(': {
		/* Parenthesized declarator or function declarator? */
		token_t const *const la1 = look_ahead(1);
		switch (la1->kind) {
		case T_IDENTIFIER:
			if (is_typedef_symbol(la1->base.symbol)) {
		case ')':
				/* §6.7.6:2 footnote 126:  Empty parentheses in a type name are
				 * interpreted as ``function with no parameter specification'',
				 * rather than redundant parentheses around the omitted
				 * identifier. */
		default:
				/* Function declarator. */
				if (!env->may_be_abstract) {
					env->symbol = sym_anonymous;
					env->pos    = *HERE;
					errorf(HERE, "function declarator must have a name");
				}
			} else {
		case '&':
		case '(':
		case '*':
		case '[':
		case T_cdecl:
		case T__fastcall:
		case T__stdcall:
		case T___thiscall:
		case T___attribute__: /* FIXME __attribute__ might also introduce a parameter of a function declarator. */
				/* Paranthesized declarator. */
				eat('(');
				add_anchor_token(')');
				inner_types = parse_inner_declarator(env);
				if (inner_types != NULL)
					/* All later declarators only modify the return type */
					env->must_be_abstract = true;

				rem_anchor_token(')');
				expect(')');
			}
			break;
		}
		break;
	}

	default:
		if (env->may_be_abstract)
			break;
		parse_error_expected("declarator", T_IDENTIFIER, '(', NULL);
		env->symbol = sym_anonymous;
		env->pos    = *HERE;
		eat_until_anchor();
		return NULL;
	}

	construct_type_t **const p = anchor;

	for (;;) {
		construct_type_t *type;
		switch (token.kind) {
		case '(': {
			scope_t *scope = NULL;
			if (!env->must_be_abstract)
				scope = &env->parameters;

			type = parse_function_declarator(scope);
			break;
		}
		case '[':
			type = parse_array_declarator();
			break;
		default:
			goto declarator_finished;
		}

		/* insert in the middle of the list (at p) */
		type->base.next = *p;
		*p              = type;
		if (anchor == p)
			anchor = &type->base.next;
	}

declarator_finished:
	/* append inner_types at the end of the list, we don't to set anchor anymore
	 * as it's not needed anymore */
	*anchor = inner_types;

	return first;
}

static type_t *construct_declarator_type(construct_type_t *construct_list,
                                         type_t *type)
{
	construct_type_t *iter = construct_list;
	for (; iter != NULL; iter = iter->base.next) {
		position_t const* const pos = &iter->base.pos;
		switch (iter->kind) {
		case CONSTRUCT_FUNCTION: {
			construct_function_type_t *function      = &iter->function;
			type_t                    *function_type = function->function_type;

			function_type->function.return_type = type;

			type_t *skipped_return_type = skip_typeref(type);
			/* §6.7.5.3:1 */
			if (is_type_function(skipped_return_type)) {
				errorf(pos, "function returning function is not allowed");
			} else if (is_type_array(skipped_return_type)) {
				errorf(pos, "function returning array is not allowed");
			} else if (skipped_return_type->base.qualifiers != 0) {
				warningf(WARN_IGNORED_QUALIFIERS, pos,
						 "type qualifiers in return type of function type are meaningless");
			}

			/* The function type was constructed earlier.  Freeing it here will
			 * destroy other types. */
			type = typehash_insert(function_type);
			continue;
		}

		case CONSTRUCT_POINTER: {
			if (is_type_reference(skip_typeref(type)))
				errorf(pos, "cannot declare a pointer to reference");

			parsed_pointer_t *pointer = &iter->pointer;
			type = make_pointer_type(type, pointer->type_qualifiers);
			continue;
		}

		case CONSTRUCT_REFERENCE:
			if (is_type_reference(skip_typeref(type)))
				errorf(pos, "cannot declare a reference to reference");

			type = make_reference_type(type);
			continue;

		case CONSTRUCT_ARRAY: {
			if (is_type_reference(skip_typeref(type)))
				errorf(pos, "cannot declare an array of references");

			parsed_array_t *array      = &iter->array;
			type_t         *array_type = allocate_type_zero(TYPE_ARRAY);
			array_type->base.qualifiers    = array->type_qualifiers;
			array_type->array.element_type = type;
			array_type->array.is_static    = array->is_static;
			array_type->array.is_variable  = array->is_variable;

			expression_t *const size_expression = array->size;
			if (size_expression) {
				array_type->array.size_expression = create_implicit_cast(size_expression, type_size_t);

				switch (is_constant_expression(size_expression)) {
				case EXPR_CLASS_INTEGER_CONSTANT: {
					long const size = fold_expression_to_int(size_expression);
					array_type->array.size          = size;
					array_type->array.size_constant = true;
					/* §6.7.5.2:1  If the expression is a constant expression,
					 * it shall have a value greater than zero. */
					if (size < 0) {
						errorf(&size_expression->base.pos,
							   "size of array must be greater than zero");
					} else if (size == 0 && !GNU_MODE) {
						errorf(&size_expression->base.pos,
							   "size of array must be greater than zero (zero length arrays are a GCC extension)");
					}
					break;
				}

				case EXPR_CLASS_CONSTANT:
				case EXPR_CLASS_VARIABLE:
					if (is_type_integer(skip_typeref(size_expression->base.type))) {
						array_type->array.is_vla = true;
						if (current_scope == file_scope)
							errorf(&size_expression->base.pos,
							       "variable length array '%T' at file scope",
							       array_type);
					}
					break;

				case EXPR_CLASS_ERROR:
					break;
				}
			}

			type_t *skipped_type = skip_typeref(type);
			/* §6.7.5.2:1 */
			if (!is_type_complete(skipped_type)) {
				errorf(pos, "array of incomplete type '%T' is not allowed",
				       type);
			} else if (is_type_function(skipped_type)) {
				errorf(pos, "array of functions is not allowed");
			}
			type = identify_new_type(array_type);
			continue;
		}
		}
		panic("invalid type construction found");
	}

	return type;
}

static type_t *semantic_parameter(const position_t *pos, type_t *type,
                                  const declaration_specifiers_t *specifiers,
                                  entity_t const *const param)
{
	/* §6.7.5.3:7  A declaration of a parameter as ``array of type''
	 *             shall be adjusted to ``qualified pointer to type'',
	 *             [...]
	 * §6.7.5.3:8  A declaration of a parameter as ``function returning
	 *             type'' shall be adjusted to ``pointer to function
	 *             returning type'', as in 6.3.2.1. */
	type = automatic_type_conversion(type);

	if (specifiers->is_inline && is_type_valid(type))
		errorf(pos, "'%N' declared 'inline'", param);

	/* §6.9.1:6  The declarations in the declaration list shall contain
	 *           no storage-class specifier other than register and no
	 *           initializations. */
	if (specifiers->thread_local
	 || (specifiers->storage_class != STORAGE_CLASS_NONE
	  && specifiers->storage_class != STORAGE_CLASS_REGISTER))
		errorf(pos, "invalid storage class for '%N'", param);

	/* delay test for incomplete type, because we might have (void)
	 * which is legal but incomplete... */
	return type;
}

static entity_t *parse_declarator(const declaration_specifiers_t *specifiers,
                                  declarator_flags_t flags)
{
	parse_declarator_env_t env;
	memset(&env, 0, sizeof(env));
	env.may_be_abstract = (flags & DECL_MAY_BE_ABSTRACT) != 0;

	construct_type_t *construct_type = parse_inner_declarator(&env);
	type_t           *orig_type      =
		construct_declarator_type(construct_type, specifiers->type);
	type_t           *type           = skip_typeref(orig_type);

	if (construct_type != NULL)
		obstack_free(&temp_obst, construct_type);

	attribute_t *attributes = parse_attributes(env.attributes);
	/* append (shared) specifier attribute behind attributes of this
	 * declarator */
	attribute_t **anchor = &attributes;
	while (*anchor != NULL)
		anchor = &(*anchor)->next;
	*anchor = specifiers->attributes;

	entity_t *entity;
	/* create a declaration type entity */
	position_t const *const pos = env.symbol ? &env.pos : &specifiers->pos;
	if (specifiers->storage_class == STORAGE_CLASS_TYPEDEF) {
		entity = allocate_entity_zero(ENTITY_TYPEDEF, NAMESPACE_NORMAL,
									  env.symbol, &env.pos);

		if (anonymous_entity != NULL) {
			if (is_type_compound(type)) {
				assert(anonymous_entity->compound.alias == NULL);
				assert(anonymous_entity->kind == ENTITY_STRUCT
				    || anonymous_entity->kind == ENTITY_UNION);
				anonymous_entity->compound.alias = entity;
				anonymous_entity = NULL;
			} else if (is_type_enum(type)) {
				assert(anonymous_entity->enume.alias == NULL);
				assert(anonymous_entity->kind == ENTITY_ENUM);
				anonymous_entity->enume.alias = entity;
				anonymous_entity = NULL;
			}
		}
	} else if (flags & DECL_CREATE_COMPOUND_MEMBER) {
		entity = allocate_entity_zero(ENTITY_COMPOUND_MEMBER,
									  NAMESPACE_NORMAL, env.symbol, pos);

		if (env.symbol != NULL) {
			if (specifiers->is_inline && is_type_valid(type))
				errorf(&env.pos, "'%N' declared 'inline'", entity);

			if (specifiers->thread_local
			 || specifiers->storage_class != STORAGE_CLASS_NONE)
				errorf(&env.pos, "'%N' must have no storage class", entity);
		}
	} else {
		if (flags & DECL_IS_PARAMETER) {
			entity    = allocate_entity_zero(ENTITY_PARAMETER, NAMESPACE_NORMAL, env.symbol, pos);
			orig_type = semantic_parameter(&env.pos, orig_type, specifiers, entity);
		} else if (is_type_function(type)) {
			entity = allocate_entity_zero(ENTITY_FUNCTION, NAMESPACE_NORMAL, env.symbol, pos);
			entity->function.is_inline        = specifiers->is_inline;
			entity->function.all_decls_inline = specifiers->is_inline;
			entity->function.elf_visibility   = default_visibility;
			entity->function.parameters       = env.parameters;

			if (env.symbol != NULL) {
				/* this needs fixes for C++ */
				bool in_function_scope = current_function != NULL;

				if (specifiers->thread_local
				 || (specifiers->storage_class != STORAGE_CLASS_EXTERN
				  && specifiers->storage_class != STORAGE_CLASS_NONE
				  && (in_function_scope || specifiers->storage_class != STORAGE_CLASS_STATIC)
					))
					errorf(&env.pos, "invalid storage class for '%N'", entity);
			}
		} else {
			entity = allocate_entity_zero(ENTITY_VARIABLE, NAMESPACE_NORMAL, env.symbol, pos);
			entity->variable.elf_visibility = default_visibility;
			entity->variable.thread_local   = specifiers->thread_local;

			if (env.symbol != NULL) {
				if (specifiers->is_inline && is_type_valid(type))
					errorf(&env.pos, "'%N' declared 'inline'", entity);

				if (current_scope == file_scope) {
					if (specifiers->storage_class != STORAGE_CLASS_EXTERN
					 && specifiers->storage_class != STORAGE_CLASS_NONE
					 && specifiers->storage_class != STORAGE_CLASS_STATIC)
						goto invalid_storage_class;
				} else if (specifiers->thread_local
				        && specifiers->storage_class == STORAGE_CLASS_NONE) {
invalid_storage_class:
					errorf(&env.pos, "invalid storage class for '%N'", entity);
				}
			}
		}
	}

	entity->declaration.type       = orig_type;
	entity->declaration.modifiers  = env.modifiers;
	entity->declaration.attributes = attributes;

	storage_class_t storage_class = specifiers->storage_class;
	entity->declaration.declared_storage_class = storage_class;

	if (storage_class == STORAGE_CLASS_NONE && (current_function || flags & DECL_IS_PARAMETER))
		storage_class = STORAGE_CLASS_AUTO;
	entity->declaration.storage_class = storage_class;

	if (attributes != NULL)
		handle_entity_attributes(attributes, entity);

	if (entity->kind == ENTITY_FUNCTION && !dialect.freestanding)
		adapt_special_functions(&entity->function);

	return entity;
}

static type_t *parse_abstract_declarator(type_t *base_type)
{
	parse_declarator_env_t env;
	memset(&env, 0, sizeof(env));
	env.may_be_abstract = true;
	env.must_be_abstract = true;

	construct_type_t *construct_type = parse_inner_declarator(&env);

	type_t *result = construct_declarator_type(construct_type, base_type);
	if (construct_type != NULL)
		obstack_free(&temp_obst, construct_type);
	result = handle_type_attributes(env.attributes, result);

	return result;
}

/**
 * Check if the declaration of main is suspicious.  main should be a
 * function with external linkage, returning int, taking either zero
 * arguments, two, or three arguments of appropriate types, ie.
 *
 * int main([ int argc, char **argv [, char **env ] ]).
 *
 * @param decl    the declaration to check
 * @param type    the function type of the declaration
 */
static void check_main(const entity_t *entity)
{
	const position_t *pos = &entity->base.pos;
	if (entity->kind != ENTITY_FUNCTION) {
		warningf(WARN_MAIN, pos, "'main' is not a function");
		return;
	}

	if (entity->declaration.storage_class == STORAGE_CLASS_STATIC)
		warningf(WARN_MAIN, pos, "'main' is normally a non-static function");

	type_t *type = skip_typeref(entity->declaration.type);
	assert(is_type_function(type));

	function_type_t const *const func_type = &type->function;
	type_t                *const ret_type  = func_type->return_type;
	if (!types_compatible(skip_typeref(ret_type), type_int))
		warningf(WARN_MAIN, pos,
		         "return type of 'main' should be 'int', but is '%T'",
		         ret_type);
	const function_parameter_t *parm = func_type->parameters;
	if (parm != NULL) {
		type_t *const first_type = skip_typeref(parm->type);
		if (!types_compatible_ignore_qualifiers(first_type, type_int))
			warningf(WARN_MAIN, pos,
			         "first argument of 'main' should be 'int', but is '%T'",
			         parm->type);

		parm = parm->next;
		if (parm != NULL) {
			type_t *const second_type = skip_typeref(parm->type);
			if (!types_compatible_ignore_qualifiers(second_type,
			                                        type_char_ptr_ptr))
				warningf(WARN_MAIN, pos,
				         "second argument of 'main' should be 'char**', but is '%T'",
				         parm->type);

			parm = parm->next;
			if (parm != NULL) {
				type_t *const third_type = skip_typeref(parm->type);
				if (!types_compatible_ignore_qualifiers(third_type,
				                                        type_char_ptr_ptr))
					warningf(WARN_MAIN, pos,
					         "third argument of 'main' should be 'char**', but is '%T'",
					         parm->type);

				parm = parm->next;
				if (parm != NULL)
					goto warn_arg_count;
			}
		} else {
warn_arg_count:
			warningf(WARN_MAIN, pos,
			         "'main' takes only zero, two or three arguments");
		}
	}
}

static void error_redefined_as_different_kind(const position_t *pos,
		const entity_t *old, entity_kind_t new_kind)
{
	char const *const what = get_entity_kind_name(new_kind);
	errorf(pos, "redeclaration of '%N' as %s", old, what);
	note_prev_decl(old);
}

static bool is_entity_valid(entity_t *const ent)
{
	if (is_declaration(ent))
		return is_type_valid(skip_typeref(ent->declaration.type));
	return true;
}

static bool contains_attribute(const attribute_t *list, const attribute_t *attr)
{
	for (const attribute_t *tattr = list; tattr != NULL; tattr = tattr->next) {
		if (attributes_equal(tattr, attr))
			return true;
	}
	return false;
}

/**
 * Tests whether new_list contains any attributes not included in old_list
 */
static bool has_new_attributes(const attribute_t *old_list,
                               const attribute_t *new_list)
{
	for (const attribute_t *attr = new_list; attr != NULL; attr = attr->next) {
		if (!contains_attribute(old_list, attr))
			return true;
	}
	return false;
}

/**
 * Merge in attributes from an attribute list (probably from a previous
 * declaration with the same name). Warning: destroys the old structure
 * of the attribute list - don't reuse attributes after this call.
 */
static void merge_in_attributes(declaration_t *decl, attribute_t *attributes)
{
	attribute_t *next;
	for (attribute_t *attr = attributes; attr != NULL; attr = next) {
		next = attr->next;
		if (contains_attribute(decl->attributes, attr))
			continue;

		/* move attribute to new declarations attributes list */
		attr->next       = decl->attributes;
		decl->attributes = attr;
	}
}

static void parse_error_multiple_definition(entity_t *previous,
                                            const position_t *pos)
{
	errorf(pos, "redefinition of '%N'", previous);
	note_prev_decl(previous);
}

static bool is_main(const entity_t *entity)
{
	if (entity->base.symbol != symbol_main)
		return false;
	/* must be in outermost scope */
	if (entity->base.parent_scope != file_scope)
		return false;

	return true;
}

static bool is_variable_definition(const entity_t *entity)
{
	assert(entity->kind == ENTITY_VARIABLE);
	return entity->variable.initializer != NULL
	    || entity->variable.alias.symbol != NULL;
}

static bool is_function_definition(const entity_t *entity)
{
	assert(entity->kind == ENTITY_FUNCTION);
	return entity->function.body != NULL
	    || entity->function.alias.symbol != NULL;
}

static void warn_missing_declaration(const entity_t *entity, bool is_definition)
{
	if (entity->kind == ENTITY_FUNCTION) {
		if (is_definition
		 && entity->declaration.storage_class != STORAGE_CLASS_STATIC
		 && !is_main(entity))
			warningf(WARN_MISSING_PROTOTYPES, &entity->base.pos,
			         "no previous prototype for '%#N'", entity);
	} else if (entity->kind                      == ENTITY_VARIABLE
	        && current_scope                     == file_scope
	        && entity->declaration.storage_class == STORAGE_CLASS_NONE
	        && !entity->declaration.implicit) {
		if (is_type_valid(skip_typeref(entity->declaration.type)))
			warningf(WARN_MISSING_VARIABLE_DECLARATIONS, &entity->base.pos,
			         "no previous declaration for '%#N'", entity);
	}
}

void merge_into_decl(entity_t *decl, const entity_t *other)
{
	assert(decl->kind == other->kind);
	decl->declaration.modifiers |= other->declaration.modifiers;
	if (decl->kind == ENTITY_FUNCTION) {
		decl->function.is_inline        |= other->function.is_inline;
		decl->function.all_decls_inline &= other->function.all_decls_inline;
		if (other->function.alias.symbol != NULL) {
			assert(decl->function.alias.symbol == NULL);
			decl->function.alias.symbol = other->function.alias.symbol;
			ARR_APP1(entity_t*, alias_entities, decl);
		}
		if (other->function.btk != BUILTIN_NONE) {
			decl->function.btk            = other->function.btk;
			decl->function.builtin_in_lib = other->function.builtin_in_lib;
			if (other->function.btk == BUILTIN_FIRM)
				decl->function.b.firm_builtin_kind =
					other->function.b.firm_builtin_kind;
		}
	} else if (decl->kind == ENTITY_VARIABLE) {
		if (other->variable.alias.symbol != NULL) {
			assert(decl->variable.alias.symbol == NULL);
			decl->variable.alias.symbol = other->variable.alias.symbol;
			ARR_APP1(entity_t*, alias_entities, decl);
		}
	}
}

entity_t *record_entity(entity_t *entity, const bool is_definition)
{
	const symbol_t    *const symbol  = entity->base.symbol;
	const entity_namespace_t namespc = entity->base.namespc;
	const position_t        *pos     = &entity->base.pos;

	assert(symbol != NULL);
	assert(!entity->base.parent_scope);
	assert(current_scope);
	entity->base.parent_scope = current_scope;

	entity_t *const previous = get_entity(symbol, namespc);
	/* pushing the same entity twice will break the stack structure */
	assert(previous != entity);

	entity_kind_t const kind = entity->kind;
	if (kind == ENTITY_FUNCTION) {
		type_t *const orig_type = entity->declaration.type;
		type_t *const type      = skip_typeref(orig_type);

		assert(is_type_function(type));
		if (type->function.unspecified_parameters && previous == NULL
		 && !entity->declaration.implicit)
			warningf(WARN_STRICT_PROTOTYPES, pos,
			         "function declaration '%#N' is not a prototype", entity);

		if (is_main(entity))
			check_main(entity);
	}

	if (is_declaration(entity)
	 && entity->declaration.storage_class == STORAGE_CLASS_EXTERN
	 && current_scope != file_scope && !entity->declaration.implicit)
		warningf(WARN_NESTED_EXTERNS, pos, "nested extern declaration of '%#N'",
		         entity);

	if (previous != NULL) {
		if (current_function != NULL
		 && previous->base.parent_scope == &current_function->parameters
		 && previous->base.parent_scope->depth+1 == current_scope->depth) {
			assert(previous->kind == ENTITY_PARAMETER);
			errorf(pos, "declaration of '%N' redeclares '%N'", entity, previous);
			note_prev_decl(previous);
			goto finish;
		}

		if (previous->base.parent_scope == current_scope) {
			if (previous->kind != kind) {
				if (is_entity_valid(previous) && is_entity_valid(entity))
					error_redefined_as_different_kind(pos, previous, kind);
				goto finish;
			}
			if (is_definition
			 && (kind == ENTITY_ENUM_VALUE
			  || (kind == ENTITY_FUNCTION && is_function_definition(previous))
			  || (kind == ENTITY_VARIABLE && is_variable_definition(previous)))) {
				parse_error_multiple_definition(previous, pos);
				goto finish;
			}
			if (kind == ENTITY_TYPEDEF) {
				type_t *const type      = skip_typeref(entity->declaration.type);
				type_t *const prev_type = skip_typeref(previous->declaration.type);
				if (dialect.cpp) {
					/* C++ allows double typedef if they are identical
					 * (after skipping typedefs) */
					if (types_same(type, prev_type))
						goto finish;
				} else {
					/* GCC extension: redef in system headers is allowed */
					if ((pos->is_system_header || previous->base.pos.is_system_header)
					 && types_compatible(type, prev_type))
						goto finish;
				}
				errorf(pos, "redefinition of '%N'", entity);
				note_prev_decl(previous);
				goto finish;
			}

			/* at this point we should have only VARIABLES or FUNCTIONS */
			assert(is_declaration(previous) && is_declaration(entity));

			declaration_t *const prev_decl = &previous->declaration;
			declaration_t *const decl      = &entity->declaration;

			/* can happen for K&R style declarations */
			if (prev_decl->type == NULL && kind == ENTITY_PARAMETER) {
				prev_decl->type                   = decl->type;
				prev_decl->storage_class          = decl->storage_class;
				prev_decl->declared_storage_class = decl->declared_storage_class;
				prev_decl->modifiers              = decl->modifiers;
				return previous;
			}

			type_t *const type      = skip_typeref(decl->type);
			type_t *const prev_type = skip_typeref(prev_decl->type);

			if (!types_compatible(type, prev_type)) {
				errorf(pos, "declaration '%#N' is incompatible with '%#N'", entity, previous);
				note_prev_decl(previous);
			} else {
				unsigned old_storage_class = prev_decl->storage_class;
				if (is_definition
				 && !prev_decl->used
				 && !(prev_decl->modifiers & DM_USED)
				 && prev_decl->storage_class == STORAGE_CLASS_STATIC)
					warningf(WARN_REDUNDANT_DECLS, &previous->base.pos,
					         "unnecessary static forward declaration for '%#N'",
					         previous);

				storage_class_t new_storage_class = decl->storage_class;

				/* pretend no storage class means extern for function
				 * declarations (except if the previous declaration is neither
				 * none nor extern) */
				if (kind == ENTITY_FUNCTION) {
					/* the previous declaration could have unspecified
					 * parameters or be a typedef, so use the new type */
					if (prev_type->function.unspecified_parameters
					 || is_definition)
						prev_decl->type = type;

					switch (old_storage_class) {
					case STORAGE_CLASS_NONE:
						old_storage_class = STORAGE_CLASS_EXTERN;
						/* FALLTHROUGH */

					case STORAGE_CLASS_EXTERN:
						if (is_definition) {
							if (prev_type->function.unspecified_parameters
							 && !is_main(entity))
								warningf(WARN_MISSING_PROTOTYPES, pos,
								         "no previous prototype for '%#N'",
								         entity);
						} else if (new_storage_class == STORAGE_CLASS_NONE) {
							new_storage_class = STORAGE_CLASS_EXTERN;
						}
						break;

					default:
						break;
					}
				} else if (!is_type_complete(prev_type)) {
					prev_decl->type = type;
				}

				if (old_storage_class == STORAGE_CLASS_EXTERN
				 && new_storage_class == STORAGE_CLASS_EXTERN) {

warn_redundant_declaration: ;
					bool has_new_attrs
						= has_new_attributes(prev_decl->attributes,
						                     decl->attributes);
					if (has_new_attrs) {
						merge_in_attributes(decl, prev_decl->attributes);
					} else if (!is_definition && is_type_valid(prev_type)) {
						if (warningf(WARN_REDUNDANT_DECLS, pos, "redundant declaration for '%N'", entity))
							note_prev_decl(previous);
					}
				} else if (current_function == NULL) {
					if (old_storage_class != STORAGE_CLASS_STATIC
					 && new_storage_class == STORAGE_CLASS_STATIC) {
						errorf(pos, "static declaration of '%N' follows non-static declaration", entity);
						note_prev_decl(previous);
					} else if (old_storage_class == STORAGE_CLASS_EXTERN) {
						prev_decl->storage_class          = STORAGE_CLASS_NONE;
						prev_decl->declared_storage_class = STORAGE_CLASS_NONE;
					} else {
						/* ISO/IEC 14882:1998(E) §C.1.2:1 */
						if (dialect.cpp)
							goto error_redeclaration;
						goto warn_redundant_declaration;
					}
				} else if (is_type_valid(prev_type)) {
					if (old_storage_class == new_storage_class) {
error_redeclaration:
						errorf(pos, "redeclaration of '%N'", entity);
					} else {
						errorf(pos, "redeclaration of '%N' with different linkage", entity);
					}
					note_prev_decl(previous);
				}
			}

			merge_into_decl(previous, entity);
			return previous;
		}

		warning_t why;
		if (is_warn_on(why = WARN_SHADOW)
		 || (is_warn_on(why = WARN_SHADOW_LOCAL)
		 && previous->base.parent_scope != file_scope)) {
			if (warningf(why, pos, "'%N' shadows '%N'", entity, previous))
				note_prev_decl(previous);
		}
	}

	warn_missing_declaration(entity, is_definition);

finish:
	environment_push(entity);
	append_entity(current_scope, entity);

	return entity;
}

static void parse_init_declarator_rest(entity_t *entity)
{
	type_t *orig_type = type_error_type;

	if (entity->kind == ENTITY_TYPEDEF) {
		errorf(&entity->base.pos,
		       "'%N' is initialized (use __typeof__ instead)", entity);
	} else {
		assert(is_declaration(entity));
		orig_type = entity->declaration.type;
	}

	if (entity->kind == ENTITY_VARIABLE
	 && entity->variable.alias.symbol != NULL)
		errorf(&entity->base.pos,
		       "'%N' has an initializer but is declared as an alias for '%Y'",
		       entity, entity->variable.alias.symbol);

	type_t *type = skip_typeref(orig_type);
	eat('=');

	declaration_t *const declaration = &entity->declaration;
	bool must_be_constant = false;
	if (declaration->storage_class == STORAGE_CLASS_STATIC
	 || entity->base.parent_scope  == file_scope)
		must_be_constant = true;

	if (is_type_function(type)) {
		errorf(&entity->base.pos,
		       "'%N' is initialized like a variable", entity);
		orig_type = type_error_type;
	} else if (is_type_array(type)) {
		if (type->array.is_vla)
			errorf(&entity->base.pos,
			       "variable sized '%N' may not be initialized", entity);
	} else if (!is_type_complete(type) && is_type_valid(type)) {
		errorf(&entity->base.pos,
		       "'%N' has incomplete type '%T' but is initialized", entity,
		       orig_type);
	}

	parse_initializer_env_t env;
	env.type             = orig_type;
	env.must_be_constant = must_be_constant;
	env.entity           = entity;
	env.pos              = *HERE;

	initializer_t *initializer = parse_initializer(&env);

	if (entity->kind == ENTITY_VARIABLE) {
		/* §6.7.5:22  array initializers for arrays with unknown size
		 * determine the array type size */
		declaration->type            = env.type;
		entity->variable.initializer = initializer;
	}
}

/* parse rest of a declaration without any declarator */
static void parse_anonymous_declaration_rest(
		const declaration_specifiers_t *specifiers)
{
	eat(';');
	anonymous_entity = NULL;

	position_t const *const pos = &specifiers->pos;
	if (specifiers->storage_class != STORAGE_CLASS_NONE
	 || specifiers->thread_local)
		warningf(WARN_OTHER, pos, "useless storage class in empty declaration");

	type_t *type = specifiers->type;
	switch (type->kind) {
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION: {
		if (type->compound.compound->base.symbol == NULL)
			warningf(WARN_OTHER, pos,
			         "unnamed struct/union that defines no instances");
		break;
	}

	case TYPE_ENUM:
		break;

	default:
		warningf(WARN_OTHER, pos, "empty declaration");
		break;
	}
}

static void check_variable_type_complete(entity_t *ent)
{
	if (ent->kind != ENTITY_VARIABLE)
		return;

	/* §6.7:7  If an identifier for an object is declared with no linkage, the
	 *         type for the object shall be complete [...] */
	declaration_t *decl = &ent->declaration;
	if (decl->storage_class == STORAGE_CLASS_EXTERN
	 || decl->storage_class == STORAGE_CLASS_STATIC)
		return;

	type_t *const type = skip_typeref(decl->type);
	if (is_type_complete(type))
		return;

	/* §6.9.2:2 and §6.9.2:5: At the end of the translation incomplete arrays
	 * are given length one. */
	if (is_type_array(type) && ent->base.parent_scope == file_scope) {
		ARR_APP1(declaration_t*, incomplete_arrays, decl);
		return;
	}

	if (is_type_valid(type))
		errorf(&ent->base.pos, "variable '%#N' has incomplete type", ent);
}

static bool is_common_variable(const entity_t *entity)
{
	return entity->declaration.storage_class == STORAGE_CLASS_NONE
	    && entity->variable.initializer == NULL
	    && !entity->variable.thread_local
	    && !(entity->declaration.modifiers & DM_WEAK);
}

static void parse_declaration_rest(entity_t *ndeclaration,
		const declaration_specifiers_t *specifiers,
		parsed_declaration_func         finished_declaration,
		declarator_flags_t              flags)
{
	add_anchor_token(';');
	add_anchor_token(',');
	while (true) {
		bool is_definition
		   = peek('=')
		  || (ndeclaration->kind == ENTITY_VARIABLE
		      && ndeclaration->variable.alias.symbol != NULL)
		  || (ndeclaration->kind == ENTITY_FUNCTION
			  && ndeclaration->function.alias.symbol != NULL);
		entity_t *entity = finished_declaration(ndeclaration, is_definition);

		if (peek('=')) {
			parse_init_declarator_rest(entity);
		} else if (entity->kind == ENTITY_VARIABLE) {
			/* ISO/IEC 14882:1998(E) §8.5.3:3  The initializer can be omitted
			 * [...] where the extern specifier is explicitly used. */
			declaration_t *decl = &entity->declaration;
			if (decl->storage_class != STORAGE_CLASS_EXTERN
			 && is_type_reference(skip_typeref(decl->type))) {
				position_t const *const pos = &entity->base.pos;
				errorf(pos, "reference '%#N' must be initialized", entity);
			}

			if (entity->variable.alias.symbol != NULL
			 && is_common_variable(entity))
				errorf(&entity->base.pos,
					   "'%N' is a definition but declared as an alias for '%Y'",
					   entity, entity->variable.alias.symbol);
		}

		check_variable_type_complete(entity);

		if (!accept(','))
			break;

		add_anchor_token('=');
		ndeclaration = parse_declarator(specifiers, flags);
		rem_anchor_token('=');
	}
	rem_anchor_token(',');
	rem_anchor_token(';');
	expect(';');

	anonymous_entity = NULL;
}

static entity_t *finished_kr_declaration(entity_t *entity, bool is_definition)
{
	symbol_t *symbol = entity->base.symbol;
	if (symbol == NULL)
		return entity;

	assert(entity->base.namespc == NAMESPACE_NORMAL);
	entity_t *previous_entity = get_entity(symbol, NAMESPACE_NORMAL);
	if (previous_entity == NULL
	    || previous_entity->base.parent_scope != current_scope) {
		errorf(&entity->base.pos,
		       "expected declaration of a function parameter, found '%Y'",
		       symbol);
		return entity;
	}

	if (is_definition)
		errorf(HERE, "'%N' is initialised", entity);

	return record_entity(entity, false);
}

static void parse_static_assert(void)
{
	position_t pos = *HERE;
	eat(T__Static_assert);
	add_anchor_token(';');
	add_anchor_token(')');
	add_anchor_token(',');
	expect('(');
	expression_t *expr
		= parse_integer_constant_expression("static assert expression");
	rem_anchor_token(',');
	expect(',');

	const string_t *message = parse_string_literals("static assert");

	rem_anchor_token(')');
	expect(')');

	/* semantic */
	if (expr->base.type != type_error_type && !fold_expression_to_bool(expr))
		errorf(&pos, "assertion '%E' failed: \"%S\"", expr, message);

	rem_anchor_token(';');
	expect(';');
}

static void parse_declaration(parsed_declaration_func finished_declaration,
                              declarator_flags_t      flags)
{
	if (peek(T__Static_assert)) {
		parse_static_assert();
		return;
	}

	add_anchor_token(';');
	declaration_specifiers_t specifiers;
	parse_declaration_specifiers(&specifiers);
	rem_anchor_token(';');

	if (peek(';')) {
		parse_anonymous_declaration_rest(&specifiers);
	} else {
		entity_t *entity = parse_declarator(&specifiers, flags);
		parse_declaration_rest(entity, &specifiers, finished_declaration, flags);
	}
}

static void parse_kr_declaration_list(entity_t *entity)
{
	if (entity->kind != ENTITY_FUNCTION)
		return;

	type_t *type = skip_typeref(entity->declaration.type);
	assert(is_type_function(type));
	if (!type->function.kr_style_parameters)
		return;

	add_anchor_token('{');

	PUSH_SCOPE(&entity->function.parameters);

	for (entity_t *parameter = entity->function.parameters.first_entity;
	     parameter != NULL; parameter = parameter->base.next) {
		assert(parameter->base.parent_scope == NULL);
		parameter->base.parent_scope = current_scope;
		environment_push(parameter);
	}

	/* parse declaration list */
	for (;;) {
		switch (token.kind) {
		case DECLARATION_START:
		/* This covers symbols, which are no type, too, and results in
		 * better error messages.  The typical cases are misspelled type
		 * names and missing includes. */
		case T_IDENTIFIER:
			parse_declaration(finished_kr_declaration, DECL_IS_PARAMETER);
			break;
		default:
			goto decl_list_end;
		}
	}
decl_list_end:

	POP_SCOPE();

	/* update function type */
	type_t *const new_type = duplicate_type(type);

	function_parameter_t  *parameters = NULL;
	function_parameter_t **anchor     = &parameters;

	/* did we have an earlier prototype? */
	entity_t *proto_type = get_entity(entity->base.symbol, NAMESPACE_NORMAL);
	if (proto_type != NULL && proto_type->kind != ENTITY_FUNCTION)
		proto_type = NULL;

	function_parameter_t *proto_parameter = NULL;
	if (proto_type != NULL) {
		type_t *proto_type_type = proto_type->declaration.type;
		proto_parameter         = proto_type_type->function.parameters;
		/* If a K&R function definition has a variadic prototype earlier, then
		 * make the function definition variadic, too. This should conform to
		 * §6.7.5.3:15 and §6.9.1:8. */
		new_type->function.variadic = proto_type_type->function.variadic;
	} else {
		/* §6.9.1.7: A K&R style parameter list does NOT act as a function
		 * prototype */
		new_type->function.unspecified_parameters = true;
	}

	bool need_incompatible_warning = false;
	for (entity_t *parameter = entity->function.parameters.first_entity;
	     parameter != NULL; parameter = parameter->base.next,
	     proto_parameter = proto_parameter == NULL ? NULL
	                                               : proto_parameter->next) {
		if (parameter->kind != ENTITY_PARAMETER)
			continue;

		type_t *parameter_type = parameter->declaration.type;
		if (parameter_type == NULL) {
			position_t const* const pos = &parameter->base.pos;
			if (dialect.strict) {
				errorf(pos, "no type specified for function '%N'", parameter);
				parameter_type = type_error_type;
			} else {
				warningf(WARN_IMPLICIT_INT, pos,
				         "no type specified for function parameter '%N', using 'int'",
				         parameter);
				parameter_type = type_int;
			}
			parameter->declaration.type = parameter_type;
		}

		semantic_parameter_complete(parameter);

		/* we need the default promoted types for the function type */
		type_t *not_promoted = parameter_type;
		parameter_type       = get_default_promoted_type(parameter_type);

		/* gcc special: if the type of the prototype matches the unpromoted
		 * type don't promote */
		if (!dialect.strict && proto_parameter != NULL) {
			type_t *proto_p_type = skip_typeref(proto_parameter->type);
			type_t *promo_skip   = skip_typeref(parameter_type);
			type_t *param_skip   = skip_typeref(not_promoted);
			if (!types_compatible(proto_p_type, promo_skip)
			 && types_compatible(proto_p_type, param_skip)) {
				/* don't promote */
				need_incompatible_warning = true;
				parameter_type = not_promoted;
			}
		}
		function_parameter_t *const function_parameter
			= allocate_parameter(parameter_type);

		*anchor = function_parameter;
		anchor  = &function_parameter->next;
	}

	new_type->function.parameters = parameters;
	entity->declaration.type      = identify_new_type(new_type);

	if (need_incompatible_warning) {
		if (warningf(WARN_OTHER, &entity->base.pos,
		             "declaration '%#N' is incompatible with '%#N'", proto_type,
		             entity))
			note_prev_decl(proto_type);
	}

	rem_anchor_token('{');
}

/**
 * Check if all labels are defined in the current function.
 * Check if all labels are used in the current function.
 */
static void check_labels(void)
{
	for (label_t const *label = label_first; label; label = label->next) {
		if (!label->statement) {
			position_t const *const pos = &label->base.pos;
			errorf(pos, "'%N' used but not defined", (entity_t const*)label);
		} else if (!label->used) {
			position_t const *const pos = &label->base.pos;
			warningf(WARN_UNUSED_LABEL, pos, "'%N' defined but not used",
			         (entity_t const*)label);
		}
	}
}

static void warn_unused_entity(warning_t const why, entity_t *entity,
                               entity_t *const last)
{
	entity_t const *const end = last != NULL ? last->base.next : NULL;
	for (; entity != end; entity = entity->base.next) {
		if (!is_declaration(entity))
			continue;

		declaration_t *declaration = &entity->declaration;
		if (declaration->implicit)
			continue;

		if (!declaration->used) {
			warningf(why, &entity->base.pos, "'%N' is unused", entity);
		} else if ((entity->kind == ENTITY_VARIABLE
		            || entity->kind == ENTITY_PARAMETER)
		           && !entity->variable.read) {
			warningf(why, &entity->base.pos, "'%N' is never read", entity);
		}
	}
}

static void check_unused_variables(statement_t *const stmt, void *const env)
{
	(void)env;

	switch (stmt->kind) {
	case STATEMENT_DECLARATION: {
		declaration_statement_t const *const decls = &stmt->declaration;
		warn_unused_entity(WARN_UNUSED_VARIABLE, decls->declarations_begin,
		                   decls->declarations_end);
		return;
	}

	case STATEMENT_FOR:
		warn_unused_entity(WARN_UNUSED_VARIABLE, stmt->fors.scope.first_entity,
		                   NULL);
		return;

	default:
		return;
	}
}

/**
 * Check declarations of current_function for unused entities.
 */
static void check_declarations(void)
{
	if (is_warn_on(WARN_UNUSED_PARAMETER)) {
		const scope_t *scope = &current_function->parameters;
		warn_unused_entity(WARN_UNUSED_PARAMETER, scope->first_entity, NULL);
	}
	if (is_warn_on(WARN_UNUSED_VARIABLE))
		walk_statements(current_function->body, check_unused_variables, NULL);
}

static int determine_truth(expression_t const* const cond)
{
	return is_constant_expression(cond) < EXPR_CLASS_CONSTANT ? 0
	     : fold_expression_to_bool(cond)                      ? 1
	     : -1;
}

static void check_reachable(statement_t *);
static bool reaches_end;

static bool expression_returns(expression_t const *const expr)
{
	switch (expr->kind) {
	case EXPR_CALL: {
		expression_t const *const func = expr->call.function;
		type_t       const *const type = skip_typeref(func->base.type);
		if (type->kind == TYPE_POINTER) {
			type_t const *const points_to
				= skip_typeref(type->pointer.points_to);
			if (points_to->kind == TYPE_FUNCTION
			 && points_to->function.modifiers & DM_NORETURN)
				return false;
		}

		if (!expression_returns(func))
			return false;

		for (call_argument_t const* arg = expr->call.arguments; arg != NULL;
		     arg = arg->next) {
			if (!expression_returns(arg->expression))
				return false;
		}

		return true;
	}

	case EXPR_REFERENCE:
	case EXPR_ENUM_CONSTANT:
	case EXPR_LITERAL_CASES:
	case EXPR_LITERAL_CHARACTER:
	case EXPR_STRING_LITERAL:
	case EXPR_COMPOUND_LITERAL: // TODO descend into initialisers
	case EXPR_LABEL_ADDRESS:
	case EXPR_CLASSIFY_TYPE:
	case EXPR_SIZEOF: // TODO handle obscure VLA case
	case EXPR_ALIGNOF:
	case EXPR_FUNCNAME:
	case EXPR_BUILTIN_CONSTANT_P:
	case EXPR_BUILTIN_TYPES_COMPATIBLE_P:
	case EXPR_OFFSETOF:
	case EXPR_ERROR:
		return true;

	case EXPR_STATEMENT: {
		bool old_reaches_end = reaches_end;
		reaches_end = false;
		check_reachable(expr->statement.statement);
		bool returns = reaches_end;
		reaches_end = old_reaches_end;
		return returns;
	}

	case EXPR_CONDITIONAL:
		// TODO handle constant expression

		if (!expression_returns(expr->conditional.condition))
			return false;

		if (expr->conditional.true_expression != NULL
		 && expression_returns(expr->conditional.true_expression))
			return true;

		return expression_returns(expr->conditional.false_expression);

	case EXPR_SELECT:
		return expression_returns(expr->select.compound);

	case EXPR_ARRAY_ACCESS:
		return expression_returns(expr->array_access.array_ref)
		    && expression_returns(expr->array_access.index);

	case EXPR_VA_START:
		return expression_returns(expr->va_starte.ap);

	case EXPR_VA_ARG:
		return expression_returns(expr->va_arge.ap);

	case EXPR_VA_COPY:
		return expression_returns(expr->va_copye.src);

	case EXPR_UNARY_CASES_MANDATORY:
		return expression_returns(expr->unary.value);

	case EXPR_UNARY_THROW:
		return false;

	case EXPR_BINARY_CASES:
		// TODO handle constant lhs of && and ||
		return expression_returns(expr->binary.left)
		    && expression_returns(expr->binary.right);
	}

	panic("unhandled expression");
}

static bool initializer_returns(initializer_t const *const init)
{
	switch (init->kind) {
	case INITIALIZER_VALUE:
		return expression_returns(init->value.value);

	case INITIALIZER_LIST:
		for (initializer_t *const *i   = init->list.initializers,
		                   *const *end = i + init->list.len; i != end; ++i) {
			if (!initializer_returns(*i))
				return false;
		}
		return true;

	case INITIALIZER_STRING:
	case INITIALIZER_DESIGNATOR: // designators have no payload
		return true;
	}
	panic("unhandled initializer");
}

static bool noreturn_candidate;

static void check_reachable(statement_t *const stmt)
{
	if (stmt->base.reachable)
		return;
	if (stmt->kind != STATEMENT_DO_WHILE)
		stmt->base.reachable = true;

	statement_t *last = stmt;
	statement_t *next;
	switch (stmt->kind) {
	case STATEMENT_ERROR:
	case STATEMENT_EMPTY:
	case STATEMENT_ASM:
		next = stmt->base.next;
		break;

	case STATEMENT_DECLARATION: {
		declaration_statement_t const *const decl = &stmt->declaration;
		entity_t                const *      ent  = decl->declarations_begin;
		entity_t                const *const last_decl = decl->declarations_end;
		if (ent != NULL) {
			for (;; ent = ent->base.next) {
				if (ent->kind                 == ENTITY_VARIABLE
				 && ent->variable.initializer != NULL
				 && !initializer_returns(ent->variable.initializer))
					return;

				if (ent == last_decl)
					break;
			}
		}
		next = stmt->base.next;
		break;
	}

	case STATEMENT_COMPOUND:
		next = stmt->compound.statements;
		if (next == NULL)
			next = stmt->base.next;
		break;

	case STATEMENT_RETURN: {
		expression_t const *const val = stmt->returns.value;
		if (val == NULL || expression_returns(val))
			noreturn_candidate = false;
		return;
	}

	case STATEMENT_IF: {
		if_statement_t const *const ifs  = &stmt->ifs;
		expression_t   const *const cond = ifs->condition;

		if (!expression_returns(cond))
			return;

		int const val = determine_truth(cond);

		if (val >= 0)
			check_reachable(ifs->true_statement);

		if (val > 0)
			return;

		if (ifs->false_statement != NULL) {
			check_reachable(ifs->false_statement);
			return;
		}

		next = stmt->base.next;
		break;
	}

	case STATEMENT_SWITCH: {
		switch_statement_t const *const switchs = &stmt->switchs;
		expression_t       const *const expr    = switchs->expression;

		if (!expression_returns(expr))
			return;

		if (is_constant_expression(expr) >= EXPR_CLASS_CONSTANT) {
			ir_tarval              *const val      = fold_expression(expr);
			case_label_statement_t *      defaults = NULL;
			for (case_label_statement_t *i = switchs->first_case; i != NULL;
			     i = i->next) {
				if (i->expression == NULL) {
					defaults = i;
					continue;
				}
				if (i->is_bad)
					continue;

				if (i->first_case == val || i->last_case == val
				 || ((tarval_cmp(i->first_case, val) & ir_relation_less_equal)
				  && (tarval_cmp(val, i->last_case) & ir_relation_less_equal))) {
					check_reachable((statement_t*)i);
					return;
				}
			}

			if (defaults != NULL) {
				check_reachable((statement_t*)defaults);
				return;
			}
		} else {
			bool has_default = false;
			for (case_label_statement_t *i = switchs->first_case; i != NULL;
			     i = i->next) {
				if (i->expression == NULL)
					has_default = true;

				check_reachable((statement_t*)i);
			}

			if (has_default)
				return;
		}

		next = stmt->base.next;
		break;
	}

	case STATEMENT_EXPRESSION: {
		/* Check for noreturn function call */
		expression_t const *const expr = stmt->expression.expression;
		if (!expression_returns(expr))
			return;

		next = stmt->base.next;
		break;
	}

	case STATEMENT_CONTINUE:
		for (statement_t *parent = stmt;;) {
			parent = parent->base.parent;
			if (parent == NULL) /* continue not within loop */
				return;

			next = parent;
			switch (parent->kind) {
			case STATEMENT_DO_WHILE: goto continue_do_while;
			case STATEMENT_FOR:      goto continue_for;

			default: break;
			}
		}

	case STATEMENT_BREAK:
		for (statement_t *parent = stmt;;) {
			parent = parent->base.parent;
			if (parent == NULL) /* break not within loop/switch */
				return;

			switch (parent->kind) {
			case STATEMENT_SWITCH:
			case STATEMENT_DO_WHILE:
			case STATEMENT_FOR:
				last = parent;
				next = parent->base.next;
				goto found_break_parent;

			default: break;
			}
		}
found_break_parent:
		break;

	case STATEMENT_COMPUTED_GOTO: {
		if (!expression_returns(stmt->computed_goto.expression))
			return;

		statement_t *parent = stmt->base.parent;
		if (parent == NULL) /* top level goto */
			return;
		next = parent;
		break;
	}

	case STATEMENT_GOTO:
		next = stmt->gotos.label->statement;
		if (next == NULL) /* missing label */
			return;
		break;

	case STATEMENT_LABEL:
		next = stmt->label.statement;
		break;

	case STATEMENT_CASE_LABEL:
		next = stmt->case_label.statement;
		break;

	case STATEMENT_DO_WHILE:
		next = stmt->do_while.body;
		break;

	case STATEMENT_FOR: {
		for_statement_t *const fors = &stmt->fors;

		if (fors->condition_reachable)
			return;
		fors->condition_reachable = true;

		expression_t const *const cond = fors->condition;

		int val;
		if (cond == NULL) {
			val = 1;
		} else if (expression_returns(cond)) {
			val = determine_truth(cond);
		} else {
			return;
		}

		if (val >= 0)
			check_reachable(fors->body);

		if (val > 0)
			return;

		next = stmt->base.next;
		break;
	}

	default:
		panic("invalid statement kind");
	}

	while (next == NULL) {
		next = last->base.parent;
		if (next == NULL) {
			noreturn_candidate = false;

			type_t *const type = skip_typeref(current_function->base.type);
			assert(is_type_function(type));
			type_t *const ret  = skip_typeref(type->function.return_type);
			if (!is_type_void(ret) && is_type_valid(ret)
			 && !is_main(current_entity)) {
				position_t const *const pos = &stmt->base.pos;
				warningf(WARN_RETURN_TYPE, pos,
				         "control reaches end of non-void function");
			}
			return;
		}

		switch (next->kind) {
		case STATEMENT_ERROR:
		case STATEMENT_EMPTY:
		case STATEMENT_DECLARATION:
		case STATEMENT_EXPRESSION:
		case STATEMENT_ASM:
		case STATEMENT_RETURN:
		case STATEMENT_CONTINUE:
		case STATEMENT_BREAK:
		case STATEMENT_COMPUTED_GOTO:
		case STATEMENT_GOTO:
			panic("invalid control flow in function");

		case STATEMENT_COMPOUND:
			if (next->compound.stmt_expr) {
				reaches_end = true;
				return;
			}
			/* FALLTHROUGH */
		case STATEMENT_IF:
		case STATEMENT_SWITCH:
		case STATEMENT_LABEL:
		case STATEMENT_CASE_LABEL:
			last = next;
			next = next->base.next;
			break;

		case STATEMENT_DO_WHILE: {
continue_do_while:
			if (next->base.reachable)
				return;
			next->base.reachable = true;

			do_while_statement_t const *const dw   = &next->do_while;
			expression_t         const *const cond = dw->condition;

			if (!expression_returns(cond))
				return;

			int const val = determine_truth(cond);

			if (val >= 0)
				check_reachable(dw->body);

			if (val > 0)
				return;

			last = next;
			next = next->base.next;
			break;
		}

		case STATEMENT_FOR: {
continue_for:;
			for_statement_t *const fors = &next->fors;

			fors->step_reachable = true;

			if (fors->condition_reachable)
				return;
			fors->condition_reachable = true;

			expression_t const *const cond = fors->condition;

			int val;
			if (cond == NULL) {
				val = 1;
			} else if (expression_returns(cond)) {
				val = determine_truth(cond);
			} else {
				return;
			}

			if (val >= 0)
				check_reachable(fors->body);

			if (val > 0)
				return;

			last = next;
			next = next->base.next;
			break;
		}
		}
	}

	check_reachable(next);
}

static void check_unreachable(statement_t* const stmt, void *const env)
{
	(void)env;

	switch (stmt->kind) {
	case STATEMENT_DO_WHILE:
		if (!stmt->base.reachable) {
			expression_t const *const cond = stmt->do_while.condition;
			if (determine_truth(cond) >= 0) {
				position_t const *const pos = &cond->base.pos;
				warningf(WARN_UNREACHABLE_CODE, pos,
				         "condition of do-while-loop is unreachable");
			}
		}
		return;

	case STATEMENT_FOR: {
		for_statement_t const* const fors = &stmt->fors;

		// if init and step are unreachable, cond is unreachable, too
		if (!stmt->base.reachable && !fors->step_reachable) {
			goto warn_unreachable;
		} else {
			if (!stmt->base.reachable && fors->initialisation != NULL) {
				position_t const *const pos = &fors->initialisation->base.pos;
				warningf(WARN_UNREACHABLE_CODE, pos,
				         "initialisation of for-statement is unreachable");
			}

			if (!fors->condition_reachable && fors->condition != NULL) {
				position_t const *const pos = &fors->condition->base.pos;
				warningf(WARN_UNREACHABLE_CODE, pos,
				         "condition of for-statement is unreachable");
			}

			if (!fors->step_reachable && fors->step != NULL) {
				position_t const *const pos = &fors->step->base.pos;
				warningf(WARN_UNREACHABLE_CODE, pos,
				         "step of for-statement is unreachable");
			}
		}
		return;
	}

	case STATEMENT_COMPOUND:
		if (stmt->compound.statements != NULL)
			return;
		goto warn_unreachable;

	case STATEMENT_DECLARATION: {
		/* Only warn if there is at least one declarator with an initializer.
		 * This typically occurs in switch statements. */
		declaration_statement_t const *const decl = &stmt->declaration;
		entity_t                const *      ent  = decl->declarations_begin;
		entity_t                const *const last = decl->declarations_end;
		if (ent != NULL) {
			for (;; ent = ent->base.next) {
				if (ent->kind == ENTITY_VARIABLE
				 && ent->variable.initializer != NULL)
					goto warn_unreachable;

				if (ent == last)
					return;
			}
		}
	}

	default:
warn_unreachable:
		if (!stmt->base.reachable) {
			position_t const *const pos = &stmt->base.pos;
			warningf(WARN_UNREACHABLE_CODE, pos, "statement is unreachable");
		}
		return;
	}
}

static void prepare_main_collect2(entity_t*);

static void parse_external_declaration(void)
{
	if (peek(T__Static_assert)) {
		parse_static_assert();
		return;
	}

	/* function-definitions and declarations both start with declaration
	 * specifiers */
	add_anchor_token(';');
	declaration_specifiers_t specifiers;
	parse_declaration_specifiers(&specifiers);
	rem_anchor_token(';');

	/* must be a declaration */
	if (peek(';')) {
		parse_anonymous_declaration_rest(&specifiers);
		return;
	}

	add_anchor_token(',');
	add_anchor_token('=');
	add_anchor_token(';');
	add_anchor_token('{');

	/* declarator is common to both function-definitions and declarations */
	entity_t *ndeclaration = parse_declarator(&specifiers, DECL_FLAGS_NONE);

	rem_anchor_token('{');
	rem_anchor_token(';');
	rem_anchor_token('=');
	rem_anchor_token(',');

	/* must be a declaration */
	switch (token.kind) {
	case ',':
	case ';':
	case '=':
		parse_declaration_rest(ndeclaration, &specifiers, record_entity,
		                       DECL_FLAGS_NONE);
		return;
	default:
		break;
	}

	/* must be a function definition */
	parse_kr_declaration_list(ndeclaration);

	if (!peek('{')) {
		parse_error_expected("function definition", '{', NULL);
		eat_until_matching_token(';');
		return;
	}

	assert(is_declaration(ndeclaration));
	type_t *const orig_type = ndeclaration->declaration.type;
	type_t *const type      = skip_typeref(orig_type);

	if (!is_type_function(type)) {
		if (is_type_valid(type))
			errorf(HERE,
			       "declarator '%#N' has a body but is not a function type",
			       ndeclaration);
		eat_block();
		return;
	}
	symbol_t *const alias = ndeclaration->function.alias.symbol;
	if (alias != NULL)
		errorf(HERE,
		       "'%#N' has a body but is declared as an alias for '%Y'",
		       ndeclaration, alias);

	position_t const *const pos = &ndeclaration->base.pos;
	if (current_scope != file_scope) {
		if (GNU_MODE) {
			warningf(WARN_PEDANTIC, pos,
			         "nested function '%N' is a GCC extension", ndeclaration);
		} else {
			errorf(pos,
			       "nested function '%N' is a GCC extension", ndeclaration);
		}
	}

	if (is_typeref(orig_type))
		/* §6.9.1:2 */
		errorf(pos, "type of function definition '%#N' is a typedef",
		       ndeclaration);

	type_t *const orig_return_type = type->function.return_type;
	type_t *const return_type      = skip_typeref(orig_return_type);
	if (!is_type_complete(return_type) && !is_type_void(return_type)
	 && is_type_valid(return_type))
		errorf(pos, "incomplete return type '%T' in definition of '%N'",
		       orig_return_type, ndeclaration);
	if (is_type_compound(return_type))
		warningf(WARN_AGGREGATE_RETURN, pos, "'%N' returns an aggregate",
		         ndeclaration);
	if (type->function.unspecified_parameters) {
		warningf(WARN_OLD_STYLE_DEFINITION, pos, "old-style definition of '%N'",
		         ndeclaration);
	} else {
		warningf(WARN_TRADITIONAL, pos,
		         "traditional C rejects ISO C style definition of '%N'",
		         ndeclaration);
	}

	/* §6.7.5.3:14 a function definition with () means no
	 * parameters (and not unspecified parameters) */
	if (type->function.unspecified_parameters
	 && type->function.parameters == NULL) {
		type_t *copy                          = duplicate_type(type);
		copy->function.unspecified_parameters = false;
		ndeclaration->declaration.type = identify_new_type(copy);
	}

	entity_t *const entity = record_entity(ndeclaration, true);
	assert(entity->kind == ENTITY_FUNCTION);
	assert(ndeclaration->kind == ENTITY_FUNCTION);

	function_t *const function = &entity->function;
	if (ndeclaration != entity)
		function->parameters = ndeclaration->function.parameters;

	PUSH_SCOPE(&function->parameters);

	for (entity_t *parameter = function->parameters.first_entity;
	     parameter != NULL; parameter = parameter->base.next) {
		if (parameter->base.parent_scope == &ndeclaration->function.parameters)
			parameter->base.parent_scope = current_scope;

		assert(parameter->base.parent_scope == NULL
				|| parameter->base.parent_scope == current_scope);
		parameter->base.parent_scope = current_scope;
		if (parameter->base.symbol == NULL) {
			errorf(&parameter->base.pos, "parameter name omitted");
			continue;
		}
		environment_push(parameter);
	}

	/* we have a fresh function. If there was a previous definition
	 * record_entity() reported the error and returned the fresh one. */
	assert(function->body == NULL);

	/* parse function body */
	int         label_stack_top      = label_top();
	function_t *old_current_function = current_function;
	current_function                 = function;
	PUSH_CURRENT_ENTITY(entity);
	PUSH_PARENT(NULL);

	label_first  = NULL;
	label_anchor = &label_first;

	statement_t *const body = parse_compound_statement(false);
	function->body = body;
	check_labels();
	check_declarations();
	if (is_warn_on(WARN_RETURN_TYPE)
	 || is_warn_on(WARN_UNREACHABLE_CODE)
	 || (is_warn_on(WARN_MISSING_NORETURN)
	     && !(function->base.modifiers & DM_NORETURN))) {
		noreturn_candidate = true;
		check_reachable(body);
		if (is_warn_on(WARN_UNREACHABLE_CODE))
			walk_statements(body, check_unreachable, NULL);
		if (noreturn_candidate
		 && !(function->base.modifiers & DM_NORETURN))
			warningf(WARN_MISSING_NORETURN, &body->base.pos,
					 "function '%#N' is candidate for attribute 'noreturn'",
					 entity);
	}

	if (is_main(entity)) {
		/* Force main to C linkage. */
		type_t *const main_type = entity->declaration.type;
		assert(is_type_function(main_type));
		if (main_type->function.linkage != LINKAGE_C) {
			type_t *new_type           = duplicate_type(main_type);
			new_type->function.linkage = LINKAGE_C;
			entity->declaration.type   = identify_new_type(new_type);
		}

		if (dialect.enable_main_collect2_hack)
			prepare_main_collect2(entity);
	}

	POP_CURRENT_ENTITY();
	POP_PARENT();
	assert(current_function == function);
	current_function = old_current_function;
	label_pop_to(label_stack_top);

	POP_SCOPE();
}

static entity_t *find_compound_entry(compound_t *compound, symbol_t *symbol)
{
	for (entity_t *iter = compound->members.first_entity; iter != NULL;
	     iter = iter->base.next) {
		if (iter->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		if (iter->base.symbol == symbol) {
			return iter;
		} else if (iter->base.symbol == NULL) {
			/* search in anonymous structs and unions */
			type_t *type = skip_typeref(iter->declaration.type);
			if (is_type_compound(type)
			 && find_compound_entry(type->compound.compound, symbol) != NULL)
				return iter;

			continue;
		}
	}

	return NULL;
}

static void check_deprecated(const position_t *pos, const entity_t *entity)
{
	if (!is_declaration(entity))
		return;
	declaration_t const *const decl = &entity->declaration;
	if (!(decl->modifiers & DM_DEPRECATED))
		return;

	string_t const *const msg = get_deprecated_string(decl->attributes);
	char     const *const fmt = msg ? "'%N' is deprecated: \"%S\""
	                                : "'%N' is deprecated";
	if (warningf(WARN_DEPRECATED_DECLARATIONS, pos, fmt, entity, msg))
		notef(&entity->base.pos, "declaration of '%N' was here", entity);
}


static expression_t *create_select(const position_t *pos, expression_t *addr,
                                   type_qualifiers_t qualifiers,
								   entity_t *entry)
{
	assert(entry->kind == ENTITY_COMPOUND_MEMBER);

	check_deprecated(pos, entry);

	expression_t *selecte          = allocate_expression_zero(EXPR_SELECT);
	selecte->base.pos              = *pos;
	selecte->select.compound       = addr;
	selecte->select.compound_entry = entry;

	type_t *entry_type = entry->declaration.type;
	type_t *res_type   = get_qualified_type(entry_type, qualifiers);

	/* handle bitfield integer promotion here */
	if (entry->compound_member.bitfield) {
		unsigned bit_size = entry->compound_member.bit_size;
		/* if fewer bits than an int, convert to int (see §6.3.1.1) */
		if (bit_size < get_atomic_type_size(ATOMIC_TYPE_INT) * BITS_PER_BYTE)
			res_type = type_int;
	}

	/* we always do the auto-type conversions; the & and sizeof parser contains
	 * code to revert this! */
	selecte->base.type = automatic_type_conversion(res_type);
	return selecte;
}

/**
 * Find entry with symbol in compound. Search anonymous structs and unions and
 * creates implicit select expressions for them.
 * Returns the adress for the innermost compound.
 */
static expression_t *find_create_select(const position_t *pos,
                                        expression_t *addr,
                                        type_qualifiers_t qualifiers,
                                        compound_t *compound, symbol_t *symbol)
{
	for (entity_t *iter = compound->members.first_entity; iter != NULL;
	     iter = iter->base.next) {
		if (iter->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		symbol_t *iter_symbol = iter->base.symbol;
		if (iter_symbol == NULL) {
			type_t *type = skip_typeref(iter->declaration.type);
			if (!is_type_compound(type))
				continue;

			compound_t *sub_compound = type->compound.compound;
			if (find_compound_entry(sub_compound, symbol) == NULL)
				continue;

			expression_t *sub_addr = create_select(pos, addr, qualifiers, iter);
			sub_addr->base.implicit = true;
			return find_create_select(pos, sub_addr, qualifiers, sub_compound,
			                          symbol);
		}

		if (iter_symbol == symbol)
			return create_select(pos, addr, qualifiers, iter);
	}

	return NULL;
}

static void parse_bitfield_member(entity_t *entity)
{
	eat(':');

	expression_t *size = parse_integer_constant_expression("bitfield size");

	assert(entity->kind == ENTITY_COMPOUND_MEMBER);
	type_t *type    = entity->declaration.type;
	type_t *skipped = skip_typeref(type);
	if (is_type_integer(skipped)) {
		if (!GNU_MODE && is_type_enum(skipped))
			errorf(&entity->base.pos,
			       "enum type '%T' as bitfield base is a GCC extension", type);

		long size_long;
		if (size->base.type == type_error_type) {
			/* just a dummy value */
			size_long = get_type_size(type) * 8;
		} else {
			size_long = fold_expression_to_int(size);

			symbol_t   const *const symbol      = entity->base.symbol;
			symbol_t   const *const user_symbol = symbol ? symbol
			                                             : sym_anonymous;
			unsigned          const bit_size    = get_type_size(type) * 8;
			position_t const *const pos         = &size->base.pos;
			if (size_long < 0) {
				errorf(pos, "negative width in bit-field '%Y'", user_symbol);
			} else if (size_long == 0 && symbol != NULL) {
				errorf(pos, "zero width for bit-field '%Y'", user_symbol);
			} else if (bit_size < (unsigned)size_long) {
				errorf(pos, "width of bitfield '%Y' exceeds its type",
				       user_symbol);
			} else {
				/* hope that people don't invent crazy types with more bits
				 * than our struct can hold */
				assert(size_long <
						(1 << sizeof(entity->compound_member.bit_size)*8));
				if (is_type_enum(skipped) && is_warn_on(WARN_BITFIELD_SIZE)) {
					enum_t *enume = skipped->enumt.enume;
					if (!enum_bitfield_big_enough(enume, skipped,
					                              (unsigned)size_long))
						warningf(WARN_BITFIELD_SIZE, pos,
						         "bitfield '%Y' is too small for enum '%T'",
						         user_symbol, type);
				}
			}
		}

		entity->compound_member.bitfield = true;
		entity->compound_member.bit_size = (unsigned char)size_long;
	} else if (is_type_valid(skipped)) {
		errorf(&entity->base.pos,
		       "bitfield base type '%T' is not an integer type", type);
	}
}

static void parse_compound_declarators(compound_t *compound,
		const declaration_specifiers_t *specifiers)
{
	add_anchor_token(';');
	add_anchor_token(',');
	do {
		entity_t         *const entity = parse_declarator(specifiers, DECL_MAY_BE_ABSTRACT | DECL_CREATE_COMPOUND_MEMBER);
		position_t const *const pos    = &entity->base.pos;
		if (entity->kind == ENTITY_TYPEDEF) {
			errorf(pos, "typedef not allowed as compound member");
			continue;
		}

		assert(entity->kind == ENTITY_COMPOUND_MEMBER);

		/* make sure we don't define a symbol multiple times */
		symbol_t *symbol = entity->base.symbol;
		if (symbol != NULL) {
			entity_t *prev = find_compound_entry(compound, symbol);
			if (prev != NULL) {
				errorf(pos, "multiple declarations of '%N'", entity);
				note_prev_decl(prev);
			}
		}

		if (peek(':')) {
			parse_bitfield_member(entity);

			attribute_t *attributes = parse_attributes(NULL);
			handle_entity_attributes(attributes, entity);
		} else {
			type_t *orig_type = entity->declaration.type;
			type_t *type      = skip_typeref(orig_type);
			if (is_type_function(type)) {
				errorf(pos, "'%N' must not have function type '%T'",
				       entity, orig_type);
			} else if (!is_type_complete(type)) {
				/* §6.7.2.1:16 flexible array member */
				if (!is_type_array(type) || !peek(';') || !peek_ahead('}')) {
					if (is_type_valid(type))
						errorf(pos, "'%N' has incomplete type '%T'", entity,
						       orig_type);
				} else if (compound->members.first_entity == NULL) {
					errorf(pos,
					       "flexible array member in otherwise empty struct");
				}
			}
		}

		append_entity(&compound->members, entity);
	} while (accept(','));
	rem_anchor_token(',');
	rem_anchor_token(';');
	expect(';');

	anonymous_entity = NULL;
}

static void parse_compound_type_entries(compound_t *compound)
{
	eat('{');
	add_anchor_token('}');

	for (;;) {
		switch (token.kind) {
		case DECLARATION_SPECIFIERS:
		case T___extension__:
		case T_IDENTIFIER:
			PUSH_EXTENSION();
			declaration_specifiers_t specifiers;
			parse_declaration_specifiers(&specifiers);
			parse_compound_declarators(compound, &specifiers);
			POP_EXTENSION();
			break;

		case T__Static_assert:
			parse_static_assert();
			break;

		case ';':
			warningf(WARN_PEDANTIC, HERE, "extra ';' in %N",
			         (entity_t*)compound);
			eat(';');
			break;

		default:
			rem_anchor_token('}');
			expect('}');
			/* §6.7.2.1:7 */
			compound->complete = true;
			return;
		}
	}
}

static type_t *parse_typename(void)
{
	declaration_specifiers_t specifiers;
	parse_declaration_specifiers(&specifiers);
	if (specifiers.storage_class != STORAGE_CLASS_NONE
	 || specifiers.thread_local)
		/* TODO: improve error message, user does probably not know what a
		 * storage class is... */
		errorf(&specifiers.pos, "type name must not have a storage class");

	type_t *result = parse_abstract_declarator(specifiers.type);
	anonymous_entity = NULL;

	return result;
}


typedef expression_t* (*parse_expression_function)(void);
typedef expression_t* (*parse_expression_infix_function)(expression_t *left);

typedef struct expression_parser_function_t expression_parser_function_t;
struct expression_parser_function_t {
	parse_expression_function        parser;
	precedence_t                     infix_precedence;
	parse_expression_infix_function  infix_parser;
};

static expression_parser_function_t expression_parsers[T_LAST_TOKEN];

/**
 * Parse a string constant.
 */
static expression_t *parse_string_literal(void)
{
	expression_t *const expr = allocate_expression_zero(EXPR_STRING_LITERAL);
	expr->string_literal.value = concat_string_literals();
	expr->base.type            = get_string_type(expr->string_literal.value->encoding);
	return expr;
}

/**
 * Parse a boolean constant.
 */
static expression_t *parse_boolean_literal(bool value)
{
	expression_t *literal = allocate_expression_zero(EXPR_LITERAL_BOOLEAN);
	literal->base.type     = type_bool;
	literal->literal.value = value ? string_true : string_false;

	eat(value ? T_true : T_false);
	return literal;
}

static bool try_create_integer(literal_expression_t *literal, type_t *type)
{
	assert(type->kind == TYPE_ATOMIC || type->kind == TYPE_COMPLEX);
	atomic_type_kind_t akind = type->atomic.akind;

	ir_mode    *const mode = atomic_modes[akind];
	char const *const str  = literal->value->begin;
	ir_tarval  *const tv   = new_tarval_from_str(str, literal->suffix - str, mode);
	if (tv == tarval_bad)
		return false;

	literal->base.type    = type;
	literal->target_value = tv;
	return true;
}

/**
 * the type of a literal is usually the smallest type that can hold the value.
 * Since this is backend dependent the parses needs this call exposed.
 * Works for EXPR_LITERAL_* expressions.
 */
static void determine_literal_type(literal_expression_t *const literal)
{
	assert(literal->base.kind == EXPR_LITERAL_INTEGER);

	/* -1: signed only, 0: any, 1: unsigned only */
	int const sign = !is_type_signed(literal->base.type) ? 1
	               : literal->value->begin[0] == '0'     ? 0
	               : -1; /* Decimal literals only try signed types. */

	int old_wrap_on_overflow = tarval_get_wrap_on_overflow();
	tarval_set_wrap_on_overflow(false);

	/* First try, if the constant fits into the type specified by the suffix.
	 * Otherwise check, if it is small enough for some type. */
	if (!try_create_integer(literal, literal->base.type)
	 && (sign < 0 || !try_create_integer(literal, type_unsigned_int))
	 && (sign > 0 || !try_create_integer(literal, type_long))
	 && (sign < 0 || !try_create_integer(literal, type_unsigned_long))
	 && (sign > 0 || !try_create_integer(literal, type_long_long))
	 && (sign < 0 || !try_create_integer(literal, type_unsigned_long_long))) {
		char const *const signedness = sign < 0 ? "signed" : "unsigned";
		errorf(&literal->base.pos,
		     "integer constant '%E' is larger than the largest %s integer type",
		       literal, signedness);
	}

	tarval_set_wrap_on_overflow(old_wrap_on_overflow);
}

static void check_number_suffix(expression_t *const expr,
                                char const *const suffix, bool const is_float)
{
	unsigned spec = SPECIFIER_NONE;
	for (char const *c = suffix; *c != '\0'; ++c) {
		specifiers_t add;
		switch (*c) {
		case 'F':
		case 'f':
			add = SPECIFIER_FLOAT;
			break;

		case 'L':
		case 'l':
			add = SPECIFIER_LONG;
			if (*c == c[1] && !is_float) {
				add |= SPECIFIER_LONG_LONG;
				++c;
			}
			break;

		case 'U':
		case 'u':
			add = SPECIFIER_UNSIGNED;
			break;

		case 'I':
		case 'i':
		case 'J':
		case 'j':
			add = SPECIFIER_COMPLEX;
			break;

		default:
			goto error;
		}
		if (spec & add)
			goto error;
		spec |= add;
	}

	if (!(spec & SPECIFIER_FLOAT) && is_float)
		spec |= SPECIFIER_DOUBLE;

	if (!(spec & (SPECIFIER_FLOAT | SPECIFIER_DOUBLE)) == is_float)
		goto error;

	type_t *type;
	switch (spec & ~SPECIFIER_COMPLEX) {
	case SPECIFIER_NONE:                                            type = type_int;                break;
	case                      SPECIFIER_LONG:                       type = type_long;               break;
	case                      SPECIFIER_LONG | SPECIFIER_LONG_LONG: type = type_long_long;          break;
	case SPECIFIER_UNSIGNED:                                        type = type_unsigned_int;       break;
	case SPECIFIER_UNSIGNED | SPECIFIER_LONG:                       type = type_unsigned_long;      break;
	case SPECIFIER_UNSIGNED | SPECIFIER_LONG | SPECIFIER_LONG_LONG: type = type_unsigned_long_long; break;
	case SPECIFIER_FLOAT:                                           type = type_float;              break;
	case SPECIFIER_DOUBLE:                                          type = type_double;             break;
	case SPECIFIER_DOUBLE   | SPECIFIER_LONG:                       type = type_long_double;        break;

	default:
error:
		errorf(HERE, "invalid suffix '%s' on %s constant", suffix,
		       is_float ? "floatingpoint" : "integer");
		return;
	}

	if (spec != SPECIFIER_NONE && spec != SPECIFIER_LONG
	 && spec != SPECIFIER_DOUBLE)
		warningf(WARN_TRADITIONAL, HERE,
		         "traditional C rejects the '%s' suffix", suffix);

	if (spec & SPECIFIER_COMPLEX)
		type = make_complex_type(get_arithmetic_akind(type), TYPE_QUALIFIER_NONE);

	expr->base.type = type;
	if (!is_float)
		/* Integer type depends on the size of the number and the size
		 * representable by the types. The backend/codegeneration has to
		 * determine that. */
		determine_literal_type(&expr->literal);
}

static expression_t *parse_number_literal(void)
{
	string_t const *const str      = token.literal.string;
	char     const *      i        = str->begin;
	unsigned              digits   = 0;
	bool                  is_float = false;

	/* Parse base prefix. */
	unsigned base;
	if (*i == '0') {
		switch (*++i) {
		case 'B': case 'b': base =  2; ++i;               break;
		case 'X': case 'x': base = 16; ++i;               break;
		default:            base =  8; digits |= 1U << 0; break;
		}
	} else {
		base = 10;
	}

	/* Parse mantissa. */
	for (;; ++i) {
		unsigned digit;
		switch (*i) {
		case '.':
			if (is_float) {
				errorf(HERE, "multiple decimal points in %K", &token);
				i = 0;
				goto done;
			}
			is_float = true;
			if (base == 8)
				base = 10;
			continue;

		case '0':           digit =  0; break;
		case '1':           digit =  1; break;
		case '2':           digit =  2; break;
		case '3':           digit =  3; break;
		case '4':           digit =  4; break;
		case '5':           digit =  5; break;
		case '6':           digit =  6; break;
		case '7':           digit =  7; break;
		case '8':           digit =  8; break;
		case '9':           digit =  9; break;
		case 'A': case 'a': digit = 10; break;
		case 'B': case 'b': digit = 11; break;
		case 'C': case 'c': digit = 12; break;
		case 'D': case 'd': digit = 13; break;
		case 'E': case 'e': digit = 14; break;
		case 'F': case 'f': digit = 15; break;

		default: goto done_mantissa;
		}

		if (digit >= 10 && base != 16)
			goto done_mantissa;

		digits |= 1U << digit;
	}
done_mantissa:

	/* Parse exponent. */
	switch (base) {
	case 2:
		if (is_float)
			errorf(HERE, "binary floating %K not allowed", &token);
		break;

	case  8:
	case 10:
		if (*i == 'E' || *i == 'e') {
			base = 10;
			goto parse_exponent;
		}
		break;

	case 16:
		if (*i == 'P' || *i == 'p') {
parse_exponent:
			++i;
			is_float = true;

			if (*i == '-' || *i == '+')
				++i;

			if (is_digit(*i)) {
				do {
					++i;
				} while (is_digit(*i));
			} else {
				errorf(HERE, "exponent of %K has no digits", &token);
			}
		} else if (is_float) {
			errorf(HERE, "hexadecimal floating %K requires an exponent", &token);
			i = 0;
		}
		break;

	default:
		panic("invalid base");
	}

done:;
	expression_t *const expr = allocate_expression_zero(is_float ? EXPR_LITERAL_FLOATINGPOINT : EXPR_LITERAL_INTEGER);
	expr->literal.value = str;

	if (i != NULL) {
		if (digits == 0) {
			errorf(HERE, "%K has no digits", &token);
		} else if (digits & ~((1U << base) - 1)) {
			errorf(HERE, "invalid digit in %K", &token);
		} else {
			expr->literal.suffix = i;
			check_number_suffix(expr, i, is_float);
		}
	}

	eat(T_NUMBER);
	return expr;
}

/**
 * Parse a character constant.
 */
static expression_t *parse_character_constant(void)
{
	expression_t *const literal = allocate_expression_zero(EXPR_LITERAL_CHARACTER);
	literal->string_literal.value = token.literal.string;

	size_t const size = get_string_len(token.literal.string);
	switch (token.literal.string->encoding) {
	case STRING_ENCODING_CHAR:
	case STRING_ENCODING_UTF8:
		literal->base.type = dialect.cpp ? type_char : type_int;
		if (size > 1) {
			if (!GNU_MODE && !dialect.c99) {
				errorf(HERE, "more than 1 character in character constant");
			} else {
				literal->base.type = type_int;
				warningf(WARN_MULTICHAR, HERE,
				         "multi-character character constant");
			}
		}
		break;

	case STRING_ENCODING_CHAR16: literal->base.type = type_char16_t; goto warn_multi;
	case STRING_ENCODING_CHAR32: literal->base.type = type_char32_t; goto warn_multi;
	case STRING_ENCODING_WIDE:   literal->base.type = type_wchar_t;  goto warn_multi;
warn_multi:
		if (size > 1)
			warningf(WARN_MULTICHAR, HERE,
			         "multi-character character constant");
		break;
	}

	eat(T_CHARACTER_CONSTANT);
	return literal;
}

static entity_t *create_implicit_function(symbol_t *symbol,
                                          position_t const *const pos)
{
	type_t *ntype                          = allocate_type_zero(TYPE_FUNCTION);
	ntype->function.return_type            = type_int;
	ntype->function.unspecified_parameters = true;
	ntype->function.linkage                = LINKAGE_C;
	type_t *type                           = identify_new_type(ntype);

	entity_t *const entity
		= allocate_entity_zero(ENTITY_FUNCTION, NAMESPACE_NORMAL, symbol, pos);
	entity->declaration.storage_class          = STORAGE_CLASS_EXTERN;
	entity->declaration.declared_storage_class = STORAGE_CLASS_EXTERN;
	entity->declaration.type                   = type;
	entity->declaration.implicit               = true;

	if (current_scope != NULL)
		record_entity(entity, false);

	return entity;
}

type_t *automatic_type_conversion(type_t *orig_type)
{
	type_t *type = skip_typeref(orig_type);
	if (is_type_array(type)) {
		array_type_t *array_type   = &type->array;
		type_t       *element_type = array_type->element_type;
		unsigned      qualifiers   = array_type->base.qualifiers;

		return make_pointer_type(element_type, qualifiers);
	}

	if (is_type_function(type))
		return make_pointer_type(orig_type, TYPE_QUALIFIER_NONE);

	return orig_type;
}

/**
 * Find an entity matching a symbol in a scope.
 * Uses current scope if scope is NULL
 */
static entity_t *lookup_entity(const scope_t *scope, symbol_t *symbol,
                               entity_namespace_t namespc)
{
	if (scope == NULL)
		return get_entity(symbol, namespc);

	/* we should optimize here, if scope grows above a certain size we should
	   construct a hashmap here... */
	for (entity_t *entity = scope->first_entity; entity != NULL;
	     entity = entity->base.next) {
		if (entity->base.symbol == symbol && entity->base.namespc == namespc)
			return entity;
	}
	return NULL;
}

static entity_t *parse_qualified_identifier(void)
{
	/* namespace containing the symbol */
	symbol_t      *symbol;
	position_t     pos;
	const scope_t *lookup_scope = NULL;

	if (accept(T_COLONCOLON))
		lookup_scope = &unit->scope;

	entity_t *entity;
	while (true) {
		symbol = expect_identifier("identifier", &pos);
		if (symbol == sym_anonymous)
			return create_error_entity(symbol, ENTITY_VARIABLE);

		/* lookup entity */
		entity = lookup_entity(lookup_scope, symbol, NAMESPACE_NORMAL);

		if (!accept(T_COLONCOLON))
			break;

		switch (entity->kind) {
		case ENTITY_NAMESPACE:
			lookup_scope = &entity->namespacee.members;
			break;
		case ENTITY_STRUCT:
		case ENTITY_UNION:
		case ENTITY_CLASS:
			lookup_scope = &entity->compound.members;
			break;
		default:
			errorf(&pos, "'%Y' must be a namespace, class, struct or union (but is a %s)",
			       symbol, get_entity_kind_name(entity->kind));

			/* skip further qualifications */
			while (accept(T_IDENTIFIER) && accept(T_COLONCOLON)) {}

			return create_error_entity(sym_anonymous, ENTITY_VARIABLE);
		}
	}

	if (entity == NULL) {
		if (!dialect.strict && peek('(')) {
			/* an implicitly declared function */
			entity = create_implicit_function(symbol, &pos);
			warningf(WARN_IMPLICIT_FUNCTION_DECLARATION, &pos,
			         "implicit declaration of '%N'", entity);
		} else {
			errorf(&pos, "identifier '%Y' is unknown.", symbol);
			entity = create_error_entity(symbol, ENTITY_VARIABLE);
		}
	}

	return entity;
}

static expression_t *parse_reference(void)
{
	position_t const pos    = *HERE;
	entity_t  *const entity = parse_qualified_identifier();

	expression_kind_t kind;
	type_t           *orig_type;
	if (is_declaration(entity) && entity->kind != ENTITY_TYPEDEF) {
		entity->declaration.used = true;
		kind      = EXPR_REFERENCE;
		orig_type = entity->declaration.type;
	} else if (entity->kind == ENTITY_ENUM_VALUE) {
		kind      = EXPR_ENUM_CONSTANT;
		orig_type = type_int;
	} else {
		panic("expected declaration or enum value in reference");
	}

	expression_t *expression     = allocate_expression_zero(kind);
	expression->base.pos         = pos;
	/* We always do the auto-type conversions; the & and sizeof parser contains
	 * code to revert this! */
	expression->base.type        = automatic_type_conversion(orig_type);
	expression->reference.entity = entity;

	if (entity->base.parent_scope != file_scope
	 && (current_function != NULL
	     && entity->base.parent_scope->depth < current_function->parameters.depth)
	     && (entity->kind == ENTITY_VARIABLE || entity->kind == ENTITY_PARAMETER)) {
		/* access of a variable from an outer function */
		entity->variable.address_taken = true;
		current_function->need_closure = true;
	}

	check_deprecated(&pos, entity);

	return expression;
}

static bool semantic_cast(expression_t *cast)
{
	expression_t     *expression      = cast->unary.value;
	type_t           *orig_dest_type  = cast->base.type;
	type_t           *orig_type_right = expression->base.type;
	type_t     const *dst_type        = skip_typeref(orig_dest_type);
	type_t     const *src_type        = skip_typeref(orig_type_right);
	position_t const *pos             = &cast->base.pos;

	/* §6.5.4 A (void) cast is explicitly permitted, more for documentation
	 * than for utility. */
	if (is_type_void(dst_type))
		return true;

	/* only integer and pointer can be casted to pointer */
	if (is_type_pointer(dst_type) && !is_type_pointer(src_type)
	 && !is_type_integer(src_type) && is_type_valid(src_type)) {
		errorf(pos, "cannot convert type '%T' to a pointer type",
		       orig_type_right);
		return false;
	}

	if (!is_type_scalar(dst_type) && is_type_valid(dst_type)) {
		errorf(pos, "conversion to non-scalar type '%T' requested",
		       orig_dest_type);
		return false;
	}

	if (!is_type_scalar(src_type) && is_type_valid(src_type)) {
		errorf(pos, "conversion from non-scalar type '%T' requested",
		       orig_type_right);
		return false;
	}

	if (is_type_pointer(src_type) && is_type_pointer(dst_type)) {
		type_t *src = skip_typeref(src_type->pointer.points_to);
		type_t *dst = skip_typeref(dst_type->pointer.points_to);
		unsigned missing_qualifiers =
			src->base.qualifiers & ~dst->base.qualifiers;
		if (missing_qualifiers != 0)
			warningf(WARN_CAST_QUAL, pos,
			         "cast discards qualifiers '%Q' in pointer target type of '%T'",
			         missing_qualifiers, orig_type_right);
	}
	return true;
}

static void semantic_complex_extract(unary_expression_t *extract)
{
	type_t *orig_value_type = extract->value->base.type;
	type_t *value_type      = skip_typeref(orig_value_type);
	if (!is_type_valid(value_type)) {
		extract->base.type = type_error_type;
		return;
	}

	type_t *type = value_type;
	if (!is_type_complex(type)) {
		if (!is_type_arithmetic(type)) {
			errorf(&extract->base.pos,
			       "%s requires an argument with complex or arithmetic type, got '%T'",
			       extract->base.kind == EXPR_UNARY_IMAG ? "__imag__" : "__real__",
			       orig_value_type);
			extract->base.type = type_error_type;
			return;
		}
		atomic_type_kind_t const akind = get_arithmetic_akind(type);
		type = make_complex_type(akind, TYPE_QUALIFIER_NONE);
		extract->value = create_implicit_cast(extract->value, type);
	}
	assert(type->kind == TYPE_COMPLEX);
	type = make_atomic_type(type->atomic.akind, TYPE_QUALIFIER_NONE);
	extract->base.type = type;
}

static expression_t *parse_compound_literal(position_t const *const pos,
                                            type_t *type)
{
	type_t *skipped = skip_typeref(type);
	if (!is_initializable_type(skipped) && is_type_valid(skipped))
		errorf(pos, "type '%T' invalid for compound literals", type);

	expression_t *expression = allocate_expression_zero(EXPR_COMPOUND_LITERAL);
	expression->base.pos = *pos;
	bool global_scope = current_scope == file_scope;

	parse_initializer_env_t env;
	env.type             = type;
	env.entity           = NULL;
	env.pos              = *HERE;
	env.must_be_constant = global_scope;
	initializer_t *initializer = parse_initializer(&env);
	type = env.type;

	expression->base.type                     = automatic_type_conversion(type);
	expression->compound_literal.initializer  = initializer;
	expression->compound_literal.type         = type;
	expression->compound_literal.global_scope = global_scope;

	return expression;
}

/**
 * Parse a cast expression.
 */
static expression_t *parse_cast(void)
{
	position_t const pos = *HERE;

	eat('(');
	add_anchor_token(')');

	type_t *type = parse_typename();

	rem_anchor_token(')');
	expect(')');

	if (peek('{'))
		return parse_compound_literal(&pos, type);

	expression_t *cast = allocate_expression_zero(EXPR_UNARY_CAST);
	cast->base.pos     = pos;

	expression_t *value = parse_subexpression(PREC_CAST);
	cast->base.type   = type;
	cast->unary.value = value;

	if (!semantic_cast(cast))
		cast->base.type = type_error_type;
	return cast;
}

static expression_t *parse_complex_extract_expression(
		expression_kind_t const kind)
{
	expression_t *extract = allocate_expression_zero(kind);
	next_token();

	extract->unary.value = parse_subexpression(PREC_CAST);
	semantic_complex_extract(&extract->unary);
	return extract;
}

static expression_t *get_statement_expression_last(
		const statement_expression_t *const expr)
{
	statement_t *statement = expr->statement;
	assert(statement->kind == STATEMENT_COMPOUND);
	statement_t *last = NULL;
	for (statement_t *s = statement->compound.statements; s != NULL;
	     s = s->base.next) {
		last = s;
	}
	if (last->kind == STATEMENT_EXPRESSION)
		return last->expression.expression;
	return NULL;
}

static void semantic_statement_expression(statement_expression_t *expr)
{
	assert(expr->statement->kind == STATEMENT_COMPOUND);
	type_t *type = type_void;
	if (expr->statement->compound.statements == NULL) {
		position_t const *const pos = &expr->base.pos;
		warningf(WARN_OTHER, pos, "empty statement expression ({})");
	} else {
		expression_t *expression = get_statement_expression_last(expr);
		if (expression != NULL)
			type = expression->base.type;
	}
	expr->base.type = type;
}

/**
 * Parse a statement expression.
 */
static expression_t *parse_statement_expression(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_STATEMENT);

	eat('(');
	add_anchor_token(')');

	statement_t *statement          = parse_compound_statement(true);
	statement->compound.stmt_expr   = true;
	expression->statement.statement = statement;
	rem_anchor_token(')');
	expect(')');

	semantic_statement_expression(&expression->statement);

	return expression;
}

/**
 * Parse a parenthesized expression.
 */
static expression_t *parse_parenthesized_expression(void)
{
	token_t const* const la1 = look_ahead(1);
	if (is_declaration_specifier(la1)) {
		return parse_cast();
	} else if (la1->kind == '{') {
		/* gcc extension: a statement expression */
		return parse_statement_expression();
	} else {
		eat('(');
		add_anchor_token(')');
		expression_t *result = parse_expression();
		result->base.parenthesized = true;
		rem_anchor_token(')');
		expect(')');
		return result;
	}
}

static expression_t *parse_function_keyword(funcname_kind_t const kind)
{
	if (current_function == NULL)
		errorf(HERE, "%K used outside of a function", &token);

	expression_t *expression  = allocate_expression_zero(EXPR_FUNCNAME);
	expression->base.type     = type_char_ptr;
	expression->funcname.kind = kind;

	next_token();

	return expression;
}

static designator_t *parse_designator(void)
{
	char   const *const context = "member designator";
	designator_t *const result  = allocate_ast_zero(sizeof(result[0]));
	result->symbol = expect_identifier(context, &result->pos);
	if (result->symbol == sym_anonymous)
		return NULL;

	designator_t *last_designator = result;
	while (true) {
		if (accept('.')) {
			designator_t *const designator = allocate_ast_zero(sizeof(result[0]));
			designator->symbol = expect_identifier(context, &designator->pos);
			if (designator->symbol == sym_anonymous)
				return NULL;

			last_designator->next = designator;
			last_designator       = designator;
			continue;
		}
		if (accept('[')) {
			add_anchor_token(']');
			designator_t *designator = allocate_ast_zero(sizeof(result[0]));
			designator->pos          = *HERE;
			designator->array_index  = parse_expression();
			rem_anchor_token(']');
			expect(']');

			last_designator->next = designator;
			last_designator       = designator;
			continue;
		}
		break;
	}

	return result;
}

/**
 * Parse the __builtin_offsetof() expression.
 */
static expression_t *parse_offsetof(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_OFFSETOF);
	expression->base.type    = type_size_t;

	eat(T___builtin_offsetof);

	add_anchor_token(')');
	add_anchor_token(',');
	expect('(');
	type_t *type = parse_typename();
	rem_anchor_token(',');
	expect(',');
	designator_t *designator = parse_designator();
	rem_anchor_token(')');
	expect(')');

	expression->offsetofe.type       = type;
	expression->offsetofe.designator = designator;

	type_t *type_skipped = skip_typeref(type);
	if (is_type_scalar(type_skipped)) {
		errorf(&expression->base.pos, "offsetof called on scalar type '%T'",
		       type);
		return create_error_expression();
	}

	type_path_t path;
	memset(&path, 0, sizeof(path));
	path.top_type = type;
	path.path     = NEW_ARR_F(type_path_entry_t, 0);

	descend_into_subtype(&path);

	if (!walk_designator(&path, designator, true)) {
		DEL_ARR_F(path.path);
		return create_error_expression();
	}

	DEL_ARR_F(path.path);
	return expression;
}

static bool is_last_parameter(expression_t *const param)
{
	if (param->kind == EXPR_REFERENCE) {
		entity_t *const entity = param->reference.entity;
		if (entity->kind == ENTITY_PARAMETER
		 && !entity->base.next
		 && entity->base.parent_scope == &current_function->parameters)
			return true;
	}

	if (!is_type_valid(skip_typeref(param->base.type)))
		return true;

	return false;
}

static void check_vararg_support(expression_t const *const expr)
{
	if (!is_type_valid(type_valist))
		errorf(&expr->base.pos, "backend does not support varargs");
}

/**
 * Parses a __builtin_va_start() expression.
 */
static expression_t *parse_va_start(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_VA_START);

	eat(T___builtin_va_start);

	add_anchor_token(')');
	add_anchor_token(',');
	expect('(');
	expression->va_starte.ap = parse_assignment_expression();
	rem_anchor_token(',');
	expect(',');
	expression_t *const param = parse_assignment_expression();
	expression->va_starte.parameter = param;
	rem_anchor_token(')');
	expect(')');

	if (!current_function) {
		errorf(&expression->base.pos, "'va_start' used outside of function");
	} else if (!current_function->base.type->function.variadic) {
		errorf(&expression->base.pos,
		       "'va_start' used in non-variadic function");
	} else if (!is_last_parameter(param)) {
		errorf(&param->base.pos,
		       "second argument of 'va_start' must be last parameter of the current function");
	} else {
		check_vararg_support(expression);
	}

	return expression;
}

/**
 * Parses a __builtin_va_arg() expression.
 */
static expression_t *parse_va_arg(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_VA_ARG);

	eat(T___builtin_va_arg);

	add_anchor_token(')');
	add_anchor_token(',');
	expect('(');
	call_argument_t ap;
	ap.expression = parse_assignment_expression();
	expression->va_arge.ap = ap.expression;
	check_call_argument(type_valist_arg, &ap, 1);

	rem_anchor_token(',');
	expect(',');
	expression->base.type = parse_typename();
	rem_anchor_token(')');
	expect(')');

	check_vararg_support(expression);
	return expression;
}

/**
 * Parses a __builtin_va_copy() expression.
 */
static expression_t *parse_va_copy(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_VA_COPY);

	eat(T___builtin_va_copy);

	add_anchor_token(')');
	add_anchor_token(',');
	expect('(');
	expression_t *dst = parse_assignment_expression();
	assign_error_t error = semantic_assign(type_valist_arg, dst);
	report_assign_error(error, type_valist, dst, "call argument 1",
	                    &dst->base.pos);
	expression->va_copye.dst = dst;

	rem_anchor_token(',');
	expect(',');

	call_argument_t src;
	src.expression = parse_assignment_expression();
	check_call_argument(type_valist_arg, &src, 2);
	expression->va_copye.src = src.expression;
	rem_anchor_token(')');
	expect(')');

	check_vararg_support(expression);
	return expression;
}

/**
 * Parses a __builtin_constant_p() expression.
 */
static expression_t *parse_builtin_constant(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_BUILTIN_CONSTANT_P);

	eat(T___builtin_constant_p);

	add_anchor_token(')');
	expect('(');
	expression->builtin_constant.value = parse_expression();
	rem_anchor_token(')');
	expect(')');
	expression->base.type = type_int;

	return expression;
}

/**
 * Parses a __builtin_types_compatible_p() expression.
 */
static expression_t *parse_builtin_types_compatible(void)
{
	expression_t *expression
		= allocate_expression_zero(EXPR_BUILTIN_TYPES_COMPATIBLE_P);

	eat(T___builtin_types_compatible_p);

	add_anchor_token(')');
	add_anchor_token(',');
	expect('(');
	expression->builtin_types_compatible.left = parse_typename();
	rem_anchor_token(',');
	expect(',');
	expression->builtin_types_compatible.right = parse_typename();
	rem_anchor_token(')');
	expect(')');
	expression->base.type = type_int;

	return expression;
}

/**
 * Parses a __builtin_is_*() compare expression.
 */
static expression_t *parse_compare_builtin(expression_kind_t const kind)
{
	expression_t *const expression = allocate_expression_zero(kind);
	next_token();

	add_anchor_token(')');
	add_anchor_token(',');
	expect('(');
	expression->binary.left = parse_assignment_expression();
	rem_anchor_token(',');
	expect(',');
	expression->binary.right = parse_assignment_expression();
	rem_anchor_token(')');
	expect(')');

	type_t *const orig_type_left  = expression->binary.left->base.type;
	type_t *const orig_type_right = expression->binary.right->base.type;

	type_t *const type_left  = skip_typeref(orig_type_left);
	type_t *const type_right = skip_typeref(orig_type_right);
	if (!is_type_float(type_left) && !is_type_float(type_right)) {
		if (is_type_valid(type_left) && is_type_valid(type_right))
			type_error_incompatible("invalid operands in comparison",
			                        &expression->base.pos, orig_type_left,
			                        orig_type_right);
	} else {
		semantic_comparison(&expression->binary, true);
	}

	return expression;
}

/**
 * Parses a MS assume() expression.
 */
static expression_t *parse_assume(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_UNARY_ASSUME);

	eat(T__assume);

	add_anchor_token(')');
	expect('(');
	expression->unary.value = parse_expression();
	rem_anchor_token(')');
	expect(')');

	expression->base.type = type_void;
	return expression;
}

/**
 * Return the label for the current symbol or create a new one.
 */
static label_t *get_label(char const *const context)
{
	assert(current_function != NULL);

	position_t      pos;
	symbol_t *const sym = expect_identifier(context, &pos);
	if (sym == sym_anonymous)
		return NULL;

	entity_t *label = get_entity(sym, NAMESPACE_LABEL);
	/* If we find a local label, we already created the declaration. */
	if (label != NULL && label->kind == ENTITY_LOCAL_LABEL) {
		if (label->base.parent_scope != current_scope) {
			assert(label->base.parent_scope->depth < current_scope->depth);
			current_function->goto_to_outer = true;
		}
	} else if (label == NULL
	        || label->base.parent_scope != &current_function->parameters) {
		/* There is no matching label in the same function, so create a new one. */
		label = allocate_entity_zero(ENTITY_LABEL, NAMESPACE_LABEL, sym, &pos);
		label_push(label);

		/* Remember the labels in a list for later checking. */
		*label_anchor = &label->label;
		label_anchor  = &label->label.next;
	}

	return &label->label;
}

/**
 * Parses a GNU && label address expression.
 */
static expression_t *parse_label_address(void)
{
	if (!GNU_MODE)
		errorf(HERE, "taking address of a label is a GCC extension");

	position_t const pos = *HERE;
	eat(T_ANDAND);

	label_t *const label = get_label("label address");
	if (!label)
		return create_error_expression();

	label->used          = true;
	label->address_taken = true;

	expression_t *expression = allocate_expression_zero(EXPR_LABEL_ADDRESS);
	expression->base.pos     = pos;

	/* label address is treated as a void pointer */
	expression->base.type           = type_void_ptr;
	expression->label_address.label = label;
	return expression;
}

/**
 * Parse a microsoft __noop expression.
 */
static expression_t *parse_noop_expression(void)
{
	/* the result is a (int)0 */
	expression_t *literal = allocate_expression_zero(EXPR_LITERAL_MS_NOOP);
	literal->base.type     = type_int;
	literal->literal.value = make_string("__noop");

	eat(T___noop);

	if (accept('(')) {
		/* parse arguments */
		add_anchor_token(')');
		add_anchor_token(',');

		if (!peek(')')) do {
			(void)parse_assignment_expression();
		} while (accept(','));

		rem_anchor_token(',');
		rem_anchor_token(')');
	}
	expect(')');

	return literal;
}

/**
 * Parses a primary expression.
 */
static expression_t *parse_primary_expression(void)
{
	switch (token.kind) {
	case T_false:                        return parse_boolean_literal(false);
	case T_true:                         return parse_boolean_literal(true);
	case T_NUMBER:                       return parse_number_literal();
	case T_CHARACTER_CONSTANT:           return parse_character_constant();
	case T_STRING_LITERAL:               return parse_string_literal();
	case T___func__:                     return parse_function_keyword(FUNCNAME_FUNCTION);
	case T___PRETTY_FUNCTION__:          return parse_function_keyword(FUNCNAME_PRETTY_FUNCTION);
	case T___FUNCSIG__:                  return parse_function_keyword(FUNCNAME_FUNCSIG);
	case T___FUNCDNAME__:                return parse_function_keyword(FUNCNAME_FUNCDNAME);
	case T___builtin_isgreater:          return parse_compare_builtin(EXPR_BINARY_ISGREATER);
	case T___builtin_isgreaterequal:     return parse_compare_builtin(EXPR_BINARY_ISGREATEREQUAL);
	case T___builtin_isless:             return parse_compare_builtin(EXPR_BINARY_ISLESS);
	case T___builtin_islessequal:        return parse_compare_builtin(EXPR_BINARY_ISLESSEQUAL);
	case T___builtin_islessgreater:      return parse_compare_builtin(EXPR_BINARY_ISLESSGREATER);
	case T___builtin_isunordered:        return parse_compare_builtin(EXPR_BINARY_ISUNORDERED);
	case T___builtin_offsetof:           return parse_offsetof();
	case T___builtin_va_start:           return parse_va_start();
	case T___builtin_va_arg:             return parse_va_arg();
	case T___builtin_va_copy:            return parse_va_copy();
	case T___builtin_constant_p:         return parse_builtin_constant();
	case T___builtin_types_compatible_p: return parse_builtin_types_compatible();
	case T__assume:                      return parse_assume();
	case T_ANDAND:                       return parse_label_address();
	case '(':                            return parse_parenthesized_expression();
	case T___noop:                       return parse_noop_expression();
	case T___imag__:                     return parse_complex_extract_expression(EXPR_UNARY_IMAG);
	case T___real__:                     return parse_complex_extract_expression(EXPR_UNARY_REAL);

	/* Gracefully handle type names while parsing expressions. */
	case T_IDENTIFIER:
		if (!is_typedef_symbol(token.base.symbol)) {
	case T_COLONCOLON:
			return parse_reference();
		}
		/* FALLTHROUGH */
	case DECLARATION_START: {
		position_t const pos = *HERE;
		declaration_specifiers_t specifiers;
		parse_declaration_specifiers(&specifiers);
		type_t const *const type = parse_abstract_declarator(specifiers.type);
		errorf(&pos, "encountered type '%T' while parsing expression", type);
		return create_error_expression();
	}
	default:
		break;
	}

	errorf(HERE, "unexpected token %K, expected an expression", &token);
	eat_until_anchor();
	return create_error_expression();
}

static expression_t *parse_array_expression(expression_t *left)
{
	expression_t              *const expr = allocate_expression_zero(EXPR_ARRAY_ACCESS);
	array_access_expression_t *const arr  = &expr->array_access;

	eat('[');
	add_anchor_token(']');

	expression_t *const inside = parse_expression();

	type_t *const orig_type_left   = left->base.type;
	type_t *const orig_type_inside = inside->base.type;

	type_t *const type_left   = skip_typeref(orig_type_left);
	type_t *const type_inside = skip_typeref(orig_type_inside);

	expression_t *ref;
	expression_t *idx;
	type_t       *idx_type;
	type_t       *res_type;
	if (is_type_pointer(type_left)) {
		ref      = left;
		idx      = inside;
		idx_type = type_inside;
		res_type = type_left->pointer.points_to;
		goto check_idx;
	} else if (is_type_pointer(type_inside)) {
		arr->flipped = true;
		ref      = inside;
		idx      = left;
		idx_type = type_left;
		res_type = type_inside->pointer.points_to;
check_idx:
		res_type = automatic_type_conversion(res_type);
		if (!is_type_integer(idx_type)) {
			if (is_type_valid(idx_type))
				errorf(&idx->base.pos,
				       "array subscript must have integer type");
		} else if (is_type_atomic(idx_type, ATOMIC_TYPE_CHAR)) {
			position_t const *const pos = &idx->base.pos;
			warningf(WARN_CHAR_SUBSCRIPTS, pos,
			         "array subscript has char type");
		}
	} else {
		if (is_type_valid(type_left) && is_type_valid(type_inside))
			errorf(&expr->base.pos,
			       "invalid types '%T[%T]' for array access", orig_type_left,
			       orig_type_inside);
		res_type = type_error_type;
		ref      = left;
		idx      = inside;
	}

	arr->array_ref = ref;
	arr->index     = idx;
	arr->base.type = res_type;

	rem_anchor_token(']');
	expect(']');
	return expr;
}

static bool is_bitfield(const expression_t *expression)
{
	return expression->kind == EXPR_SELECT
	    && expression->select.compound_entry->compound_member.bitfield;
}

static expression_t *parse_typeprop(expression_kind_t const kind)
{
	expression_t  *tp_expression = allocate_expression_zero(kind);
	tp_expression->base.type     = type_size_t;

	eat(kind == EXPR_SIZEOF ? T_sizeof : T__Alignof);

	type_t       *orig_type;
	expression_t *expression;
	if (peek('(') && is_declaration_specifier(look_ahead(1))) {
		position_t const pos = *HERE;
		eat('(');
		add_anchor_token(')');
		orig_type = parse_typename();
		rem_anchor_token(')');
		expect(')');

		if (peek('{')) {
			/* It was not sizeof(type) after all.  It is sizeof of an expression
			 * starting with a compound literal */
			expression = parse_compound_literal(&pos, orig_type);
			goto typeprop_expression;
		}
	} else {
		expression = parse_subexpression(PREC_UNARY);

typeprop_expression:
		if (is_bitfield(expression)) {
			char const* const what = kind == EXPR_SIZEOF ? "sizeof" : "alignof";
			errorf(&tp_expression->base.pos,
			       "operand of %s expression must not be a bitfield", what);
		}

		tp_expression->typeprop.tp_expression = expression;

		orig_type = revert_automatic_type_conversion(expression);
		expression->base.type = orig_type;
	}

	tp_expression->typeprop.type   = orig_type;
	type_t const* const type       = skip_typeref(orig_type);
	char   const*       wrong_type = NULL;
	if (!is_type_complete(type)) {
		if (!is_type_void(type) || !GNU_MODE)
			wrong_type = "incomplete";
	} else if (type->kind == TYPE_FUNCTION) {
		if (GNU_MODE) {
			/* function types are allowed (and return 1) */
			position_t const *const pos  = &tp_expression->base.pos;
			char       const *const what = kind == EXPR_SIZEOF ? "sizeof" : "alignof";
			warningf(WARN_OTHER, pos,
			         "%s expression with function argument returns invalid result",
			         what);
		} else {
			wrong_type = "function";
		}
	}

	if (wrong_type && is_type_valid(type)) {
		char const* const what = kind == EXPR_SIZEOF ? "sizeof" : "alignof";
		errorf(&tp_expression->base.pos,
		       "operand of %s expression must not be of %s type '%T'",
		       what, wrong_type, orig_type);
	}

	return tp_expression;
}

static expression_t *parse_sizeof(void)
{
	return parse_typeprop(EXPR_SIZEOF);
}

static expression_t *parse_alignof(void)
{
	return parse_typeprop(EXPR_ALIGNOF);
}

static expression_t *parse_select_expression(expression_t *addr)
{
	assert(peek('.') || peek(T_MINUSGREATER));
	bool       const select_left_arrow = peek(T_MINUSGREATER);
	position_t const pos               = *HERE;
	next_token();

	symbol_t *const symbol = expect_identifier("select", NULL);
	if (symbol == sym_anonymous)
		return create_error_expression();

	type_t *const orig_type = addr->base.type;
	type_t *const type      = skip_typeref(orig_type);

	type_t *type_left;
	bool    saw_error = false;
	if (is_type_pointer(type)) {
		if (!select_left_arrow) {
			errorf(&pos,
			       "request for member '%Y' in something not a struct or union, but '%T'",
			       symbol, orig_type);
			saw_error = true;
		}
		type_left = skip_typeref(type->pointer.points_to);
	} else {
		if (select_left_arrow && is_type_valid(type)) {
			errorf(&pos, "left hand side of '->' is not a pointer, but '%T'",
			       orig_type);
			saw_error = true;
		}
		type_left = type;
	}

	if (!is_type_compound(type_left)) {
		if (is_type_valid(type_left) && !saw_error) {
			errorf(&pos,
			       "request for member '%Y' in something not a struct or union, but '%T'",
			       symbol, type_left);
		}
		return create_error_expression();
	}

	compound_t *compound = type_left->compound.compound;
	if (!compound->complete) {
		errorf(&pos, "request for member '%Y' in incomplete type '%T'",
		       symbol, type_left);
		return create_error_expression();
	}

	type_qualifiers_t  qualifiers = type_left->base.qualifiers;
	expression_t      *result
		= find_create_select(&pos, addr, qualifiers, compound, symbol);

	if (result == NULL) {
		errorf(&pos, "'%T' has no member named '%Y'", orig_type, symbol);
		return create_error_expression();
	}

	return result;
}

static void check_call_argument(type_t          *expected_type,
                                call_argument_t *argument, unsigned pos)
{
	type_t         *expected_type_skip = skip_typeref(expected_type);
	assign_error_t  error              = ASSIGN_ERROR_INCOMPATIBLE;
	expression_t   *arg_expr           = argument->expression;
	type_t         *arg_type           = skip_typeref(arg_expr->base.type);

	/* handle transparent union gnu extension */
	if (is_type_union(expected_type_skip)
	    && (get_type_modifiers(expected_type) & DM_TRANSPARENT_UNION)) {
		compound_t *union_decl = expected_type_skip->compound.compound;
		type_t     *best_type  = NULL;
		for (entity_t const *entry = union_decl->members.first_entity;
		     entry != NULL; entry = entry->base.next) {
			assert(is_declaration(entry));
			type_t *decl_type = entry->declaration.type;
			error = semantic_assign(decl_type, arg_expr);
			if (error == ASSIGN_ERROR_INCOMPATIBLE
			 || error == ASSIGN_ERROR_POINTER_QUALIFIER_MISSING)
				continue;

			if (error == ASSIGN_SUCCESS) {
				best_type = decl_type;
			} else if (best_type == NULL) {
				best_type = decl_type;
			}
		}

		if (best_type != NULL)
			expected_type = best_type;
	}

	error                = semantic_assign(expected_type, arg_expr);
	argument->expression = create_implicit_cast(arg_expr, expected_type);

	if (error != ASSIGN_SUCCESS) {
		/* report exact scope in error messages (like "in argument 3") */
		char buf[64];
		snprintf(buf, sizeof(buf), "call argument %u", pos);
		report_assign_error(error, expected_type, arg_expr, buf,
		                    &arg_expr->base.pos);
	} else {
		type_t *const promoted_type = get_default_promoted_type(arg_type);
		if (!types_compatible(expected_type_skip, promoted_type)
		 && !types_compatible(expected_type_skip, type_void_ptr)
		 && !types_compatible(type_void_ptr,      promoted_type)) {
			/* Deliberately show the skipped types in this warning */
			position_t const *const apos = &arg_expr->base.pos;
			warningf(WARN_TRADITIONAL, apos,
			         "passing call argument %u as '%T' rather than '%T' due to prototype",
			         pos, expected_type_skip, promoted_type);
		}
	}
}

/**
 * Handle the semantic restrictions of builtin calls
 */
static void handle_builtin_argument_restrictions(call_expression_t *const call,
                                                 entity_t *const entity)
{
	switch (entity->function.btk) {
	case BUILTIN_FIRM:
		switch (entity->function.b.firm_builtin_kind) {
		case ir_bk_return_address:
		case ir_bk_frame_address: {
			/* argument must be constant */
			call_argument_t *argument = call->arguments;

			if (is_constant_expression(argument->expression) == EXPR_CLASS_VARIABLE)
				errorf(&call->base.pos,
				       "argument of '%Y' must be a constant expression",
				       call->function->reference.entity->base.symbol);
			break;
		}
		case ir_bk_prefetch:
			/* second and third argument must be constant if existent */
			if (call->arguments == NULL)
				break;
			call_argument_t *rw = call->arguments->next;
			call_argument_t *locality = NULL;

			if (rw != NULL) {
				if (is_constant_expression(rw->expression) == EXPR_CLASS_VARIABLE)
					errorf(&call->base.pos,
					       "second argument of '%Y' must be a constant expression",
					       call->function->reference.entity->base.symbol);
				locality = rw->next;
			}
			if (locality != NULL
			 && is_constant_expression(locality->expression) == EXPR_CLASS_VARIABLE)
				errorf(&call->base.pos,
				       "third argument of '%Y' must be a constant expression",
				       call->function->reference.entity->base.symbol);
			break;
		default:
			break;
		}
		break;

	case BUILTIN_OBJECT_SIZE:
		if (call->arguments == NULL)
			break;

		call_argument_t *arg = call->arguments->next;
		if (arg != NULL
		 && is_constant_expression(arg->expression) == EXPR_CLASS_VARIABLE)
			errorf(&call->base.pos,
			       "second argument of '%Y' must be a constant expression",
			       call->function->reference.entity->base.symbol);
		break;
	default:
		break;
	}
}

static type_t *check_generic_parameter(type_t **const replacement_ref,
                                       type_t *const parameter_type,
                                       type_t *const argument_type)
{
	if (parameter_type->kind == TYPE_BUILTIN_TEMPLATE) {
		type_t *replacement = *replacement_ref;
		if (replacement == NULL) {
			if (argument_type == NULL)
				return NULL;
			replacement = skip_typeref(argument_type);
			*replacement_ref = replacement;
		}
		return replacement;
	} else if (parameter_type->kind == TYPE_POINTER) {
		type_t *argument_points_to = NULL;
		if (argument_type != NULL) {
			type_t *skipped = skip_typeref(argument_type);
			if (argument_type->kind != TYPE_POINTER)
				/* TODO: error message */
				return NULL;
			argument_points_to = skipped->pointer.points_to;
		}
		type_t *old_points_to = parameter_type->pointer.points_to;
		type_t *new_points_to = check_generic_parameter(replacement_ref,
			old_points_to, argument_points_to);
		if (old_points_to != new_points_to)
			return make_pointer_type(new_points_to,
			                         parameter_type->base.qualifiers);
	}

	return parameter_type;
}

static function_type_t *handle_builtin_overload(call_expression_t *const call,
		function_type_t *const function_type,
		expression_t *const function)
{
	if (!function_type->typegeneric)
		return function_type;

	/* construct an ad-hoc call type... */
	type_t *new_function_type = allocate_type_zero(TYPE_FUNCTION);
	new_function_type->function.linkage   = function_type->linkage;
	new_function_type->function.modifiers = function_type->modifiers;

	function_parameter_t  *parameter = function_type->parameters;
	call_argument_t       *argument  = call->arguments;
	function_parameter_t **anchor    = &new_function_type->function.parameters;
	type_t                *template_replacement = NULL;
	for ( ; parameter != NULL && argument != NULL;
		 parameter = parameter->next, argument = argument->next) {
		type_t *parameter_type = parameter->type;
		type_t *new_type
			= check_generic_parameter(&template_replacement,
			                          parameter_type,
			                          argument->expression->base.type);
		if (new_type == NULL)
			return NULL;
		function_parameter_t *new_parameter = allocate_parameter(new_type);
		*anchor = new_parameter;
		anchor  = &new_parameter->next;
	}
	new_function_type->function.return_type
		= check_generic_parameter(&template_replacement,
		                          function_type->return_type, NULL);

	if (parameter != NULL) {
		errorf(&call->base.pos, "too few arguments to function '%E'",
		       function);
		return NULL;
	} else if (argument != NULL && !function_type->variadic) {
		errorf(&argument->expression->base.pos,
		       "too many arguments to function '%E'", function);
		return NULL;
	}

	type_t *identified = identify_new_type(new_function_type);
	assert(identified->kind == TYPE_FUNCTION);
	return &identified->function;
}

static void check_library_call_ctype(const call_expression_t *call)
{
	if (dialect.freestanding || !is_warn_on(WARN_CHAR_CTYPE))
		return;
	expression_t *const function = call->function;
	if (function->kind != EXPR_REFERENCE)
		return;
	const entity_t *entity = function->reference.entity;
	if (entity->kind != ENTITY_FUNCTION)
		return;
	symbol_t *symbol = entity->base.symbol;
	/* we need an isXXX or toXXX function */
	const char *string = symbol->string;
	if ((string[0] != 'i' || string[1] != 's')
	 && (string[0] != 't' || string[1] != 'o'))
		return;
	const char *const ctype_funcs[] = { "isalnum", "isalpha", "isblank",
	    "iscntrl", "isdigit", "isgraph", "islower", "isprint", "ispunct",
	    "isspace", "isupper", "isxdigit", "tolower", "toupper" };
	bool is_a_ctype_func = false;
	for (size_t i = 0; i < ARRAY_SIZE(ctype_funcs); ++i) {
		if (streq(string, ctype_funcs[i])) {
			is_a_ctype_func = true;
			break;
		}
	}
	if (!is_a_ctype_func)
		return;
	/* we should have exactly 1 argument otherwise something else is broken */
	call_argument_t *argument = call->arguments;
	if (argument == NULL || argument->next != NULL)
		return;
	/* skip implicit casts */
	expression_t *expression = argument->expression;
	if (expression->kind == EXPR_UNARY_CAST && expression->base.implicit)
		expression = expression->unary.value;
	/* Note that casting a char to an int does not solve anything here
	 * (casting to unsigned char would be the proper solution), so still
	 * warn people that try to silence the warning by casting to int. */
	if (expression->kind == EXPR_UNARY_CAST) {
		type_t *dest = skip_typeref(expression->base.type);
		if (is_type_atomic(dest, ATOMIC_TYPE_INT))
			expression = expression->unary.value;
	}

	type_t *orig_type = expression->base.type;
	type_t *type      = skip_typeref(orig_type);
	/* In case of char or signed char, chances are pretty good that something
	 * is wrong: EOF can't be encoded in char and all other negative values
	 * will result in undefined behaviour as they are not "representable
	 * as a unsigned char" as required by the C standard. */
	if (is_type_atomic(type, ATOMIC_TYPE_CHAR)
	 || is_type_atomic(type, ATOMIC_TYPE_SCHAR)) {
		warningf(WARN_CHAR_CTYPE, &expression->base.pos,
		         "Argument of type %T to %Y gives undefined behaviour on negative values != EOF",
		         orig_type, symbol);
	}
}

static void semantic_call(call_expression_t *call)
{
	expression_t *const function  = call->function;
	type_t       *const orig_type = function->base.type;
	type_t       *const type      = skip_typeref(orig_type);

	function_type_t *function_type = NULL;
	if (is_type_pointer(type)) {
		type_t *const to_type = skip_typeref(type->pointer.points_to);

		if (is_type_function(to_type)) {
			function_type   = &to_type->function;
			call->base.type = function_type->return_type;
		}
	}

	if (function_type == NULL) {
		if (is_type_valid(type))
			errorf(&call->base.pos,
			       "called object '%E' (type '%T') is not a function or function pointer",
			       function, orig_type);
		goto error;
	}

	if (function_type->typegeneric) {
		function_type = handle_builtin_overload(call, function_type, function);
		if (function_type == NULL)
			goto error;
		call->concrete_type = function_type;
	}
	call->base.type = function_type->return_type;

	/* check type and count of call arguments */
	function_parameter_t *parameter = function_type->parameters;
	call_argument_t      *argument  = call->arguments;
	if (!function_type->unspecified_parameters) {
		for (unsigned pos = 0; parameter != NULL && argument != NULL;
		     parameter = parameter->next, argument = argument->next) {
			check_call_argument(parameter->type, argument, ++pos);
		}

		if (parameter != NULL) {
			errorf(&call->base.pos, "too few arguments to function '%E'",
			       function);
		} else if (argument != NULL && !function_type->variadic) {
			errorf(&argument->expression->base.pos,
			       "too many arguments to function '%E'", function);
		}
	}

	/* do default promotion for other arguments */
	for (; argument != NULL; argument = argument->next) {
		type_t *const orig_arg_type = argument->expression->base.type;
		type_t *const arg_type      = skip_typeref(orig_arg_type);
		if (!is_type_complete(arg_type) && is_type_valid(arg_type))
			errorf(&argument->expression->base.pos,
			       "call argument '%E' has incomplete type",
			       argument->expression);

		type_t *const promoted_arg_type = get_default_promoted_type(orig_arg_type);
		argument->expression = create_implicit_cast(argument->expression, promoted_arg_type);
	}

	check_format(call);
	check_library_call_ctype(call);

	if (is_type_compound(skip_typeref(function_type->return_type)))
		warningf(WARN_AGGREGATE_RETURN, &call->base.pos,
		         "function call has aggregate value");

	if (function->kind == EXPR_REFERENCE) {
		entity_t *entity = function->reference.entity;
		if (entity->kind == ENTITY_FUNCTION
		 && entity->function.btk != BUILTIN_NONE)
			handle_builtin_argument_restrictions(call, entity);
	}
	return;
error:
	call->base.type = type_error_type;
}

/**
 * Parse a call expression, i.e. expression '( ... )'.
 *
 * @param expression  the function address
 */
static expression_t *parse_call_expression(expression_t *expression)
{
	expression_t      *result = allocate_expression_zero(EXPR_CALL);
	call_expression_t *call   = &result->call;
	call->function            = expression;

	/* parse arguments */
	eat('(');
	add_anchor_token(')');
	add_anchor_token(',');

	if (!peek(')')) {
		call_argument_t **anchor = &call->arguments;
		do {
			call_argument_t *argument = allocate_ast_zero(sizeof(*argument));
			argument->expression = parse_assignment_expression();

			*anchor = argument;
			anchor  = &argument->next;
		} while (accept(','));
	}
	rem_anchor_token(',');
	rem_anchor_token(')');
	expect(')');

	semantic_call(call);

	return result;
}

static type_t *semantic_arithmetic(type_t *type_left, type_t *type_right);

static bool same_compound_type(const type_t *type1, const type_t *type2)
{
	return is_type_compound(type1)
	    && type1->kind == type2->kind
	    && type1->compound.compound == type2->compound.compound;
}

static expression_t const *get_reference_address(expression_t const *expr)
{
	bool regular_take_address = true;
	for (;;) {
		if (expr->kind == EXPR_UNARY_TAKE_ADDRESS) {
			expr = expr->unary.value;
		} else {
			regular_take_address = false;
		}

		if (expr->kind != EXPR_UNARY_DEREFERENCE)
			break;

		expr = expr->unary.value;
	}

	if (expr->kind != EXPR_REFERENCE)
		return NULL;

	/* special case for functions which are automatically converted to a
	 * pointer to function without an extra TAKE_ADDRESS operation */
	if (!regular_take_address
	 && expr->reference.entity->kind != ENTITY_FUNCTION)
		return NULL;

	return expr;
}

static void warn_reference_address_as_bool(expression_t const* expr)
{
	expr = get_reference_address(expr);
	if (expr != NULL) {
		position_t const *const pos = &expr->base.pos;
		entity_t   const *const ent = expr->reference.entity;
		warningf(WARN_ADDRESS, pos,
		         "the address of '%N' will always evaluate as 'true'", ent);
	}
}

static void warn_assignment_in_condition(const expression_t *const expr)
{
	if (expr->base.kind != EXPR_BINARY_ASSIGN)
		return;
	if (expr->base.parenthesized)
		return;
	position_t const *const pos = &expr->base.pos;
	warningf(WARN_PARENTHESES, pos,
	         "suggest parentheses around assignment used as truth value");
}

static void semantic_condition(expression_t const *const expr,
                               char const *const context)
{
	type_t *const type = skip_typeref(expr->base.type);
	if (is_type_scalar(type)) {
		warn_reference_address_as_bool(expr);
		warn_assignment_in_condition(expr);
	} else if (is_type_valid(type)) {
		errorf(&expr->base.pos, "%s must have scalar type", context);
	}
}

/**
 * Parse a conditional expression, i.e. 'expression ? ... : ...'.
 *
 * @param expression  the conditional expression
 */
static expression_t *parse_conditional_expression(expression_t *expression)
{
	expression_t *result = allocate_expression_zero(EXPR_CONDITIONAL);

	conditional_expression_t *conditional = &result->conditional;
	conditional->condition                = expression;

	eat('?');
	add_anchor_token(':');

	/* §6.5.15:2  The first operand shall have scalar type. */
	semantic_condition(expression, "condition of conditional operator");

	expression_t *true_expression = expression;
	bool          gnu_cond = false;
	if (peek(':')) {
		if (!GNU_MODE)
			errorf(HERE, "omitting consequence of conditional expression is a GCC extension");
		gnu_cond = true;
	} else {
		true_expression = parse_expression();
	}
	rem_anchor_token(':');
	expect(':');
	expression_t *false_expression =
		parse_subexpression(dialect.cpp ? PREC_ASSIGNMENT : PREC_CONDITIONAL);

	type_t *const orig_true_type  = true_expression->base.type;
	type_t *const orig_false_type = false_expression->base.type;
	type_t *const true_type       = skip_typeref(orig_true_type);
	type_t *const false_type      = skip_typeref(orig_false_type);

	/* 6.5.15.3 */
	position_t const *const pos = &conditional->base.pos;
	type_t                 *result_type;
	if (is_type_void(true_type) || is_type_void(false_type)) {
		/* ISO/IEC 14882:1998(E) §5.16:2 */
		if (true_expression->kind == EXPR_UNARY_THROW) {
			result_type = false_type;
		} else if (false_expression->kind == EXPR_UNARY_THROW) {
			result_type = true_type;
		} else {
			if (!is_type_void(true_type) || !is_type_void(false_type))
				warningf(WARN_OTHER, pos,
				         "ISO C forbids conditional expression with only one void side");
			result_type = type_void;
		}
	} else if (is_type_arithmetic(true_type)
	           && is_type_arithmetic(false_type)) {
		result_type = semantic_arithmetic(true_type, false_type);
	} else if (same_compound_type(true_type, false_type)) {
		/* just take 1 of the 2 types */
		result_type = true_type;
	} else if (is_type_pointer(true_type) || is_type_pointer(false_type)) {
		type_t *pointer_type;
		type_t *other_type;
		expression_t *other_expression;
		if (is_type_pointer(true_type)
		    && (!is_type_pointer(false_type)
		        || is_null_pointer_constant(false_expression))) {
			pointer_type     = true_type;
			other_type       = false_type;
			other_expression = false_expression;
		} else {
			pointer_type     = false_type;
			other_type       = true_type;
			other_expression = true_expression;
		}

		if (is_null_pointer_constant(other_expression)) {
			result_type = pointer_type;
		} else if (is_type_pointer(other_type)) {
			type_t *to1 = skip_typeref(pointer_type->pointer.points_to);
			type_t *to2 = skip_typeref(other_type->pointer.points_to);

			type_t *to;
			if (is_type_void(to1) || is_type_void(to2)) {
				to = type_void;
			} else if (types_compatible_ignore_qualifiers(to1, to2)) {
				to = to1;
			} else {
				warningf(WARN_OTHER, pos,
				         "pointer types '%T' and '%T' in conditional expression are incompatible",
				         true_type, false_type);
				to = type_void;
			}

			type_t *const type =
				get_qualified_type(to, to1->base.qualifiers | to2->base.qualifiers);
			result_type = make_pointer_type(type, TYPE_QUALIFIER_NONE);
		} else if (is_type_integer(other_type)) {
			warningf(WARN_OTHER, pos,
			         "pointer/integer type mismatch in conditional expression ('%T' and '%T')",
			         true_type, false_type);
			result_type = pointer_type;
		} else {
			goto types_incompatible;
		}
	} else {
types_incompatible:
		if (is_type_valid(true_type) && is_type_valid(false_type))
			type_error_incompatible("while parsing conditional", pos,
			                        true_type, false_type);
		result_type = type_error_type;
	}

	conditional->true_expression
		= gnu_cond ? NULL : create_implicit_cast(true_expression, result_type);
	conditional->false_expression
		= create_implicit_cast(false_expression, result_type);
	conditional->base.type = result_type;
	return result;
}

/**
 * Parse an extension expression.
 */
static expression_t *parse_extension(void)
{
	PUSH_EXTENSION();
	expression_t *expression = parse_subexpression(PREC_UNARY);
	POP_EXTENSION();
	return expression;
}

/**
 * Parse a __builtin_classify_type() expression.
 */
static expression_t *parse_builtin_classify_type(void)
{
	expression_t *result = allocate_expression_zero(EXPR_CLASSIFY_TYPE);
	result->base.type    = type_int;

	eat(T___builtin_classify_type);

	add_anchor_token(')');
	expect('(');
	expression_t *expression = parse_expression();
	rem_anchor_token(')');
	expect(')');
	result->classify_type.type_expression = expression;

	return result;
}

/**
 * Parse a delete expression
 * ISO/IEC 14882:1998(E) §5.3.5
 */
static expression_t *parse_delete(void)
{
	expression_t *const result = allocate_expression_zero(EXPR_UNARY_DELETE);
	result->base.type          = type_void;

	eat(T_delete);

	if (accept('[')) {
		result->kind = EXPR_UNARY_DELETE_ARRAY;
		expect(']');
	}

	expression_t *const value = parse_subexpression(PREC_CAST);
	result->unary.value = value;

	type_t *const type = skip_typeref(value->base.type);
	if (!is_type_pointer(type)) {
		if (is_type_valid(type))
			errorf(&value->base.pos,
			       "operand of delete must have pointer type");
	} else if (is_type_void(skip_typeref(type->pointer.points_to))) {
		position_t const *const pos = &value->base.pos;
		warningf(WARN_OTHER, pos, "deleting 'void*' is undefined");
	}

	return result;
}

/**
 * Parse a throw expression
 * ISO/IEC 14882:1998(E) §15:1
 */
static expression_t *parse_throw(void)
{
	expression_t *const result = allocate_expression_zero(EXPR_UNARY_THROW);
	result->base.type          = type_void;

	eat(T_throw);

	expression_t *value = NULL;
	switch (token.kind) {
	case EXPRESSION_START:
	case T_IDENTIFIER: {
		value = parse_assignment_expression();
		/* ISO/IEC 14882:1998(E) §15.1:3 */
		type_t *const orig_type = value->base.type;
		type_t *const type      = skip_typeref(orig_type);
		if (!is_type_complete(type)) {
			errorf(&value->base.pos,
			       "cannot throw object of incomplete type '%T'", orig_type);
		} else if (is_type_pointer(type)) {
			type_t *const points_to = skip_typeref(type->pointer.points_to);
			if (!is_type_complete(points_to) && !is_type_void(points_to))
				errorf(&value->base.pos,
				       "cannot throw pointer to incomplete type '%T'",
				       orig_type);
		}
	}

	default:
		break;
	}
	result->unary.value = value;

	return result;
}

static bool check_pointer_arithmetic(const position_t *pos,
                                     type_t *pointer_type,
                                     type_t *orig_pointer_type)
{
	type_t *points_to = pointer_type->pointer.points_to;
	points_to = skip_typeref(points_to);

	if (!is_type_complete(points_to)) {
		if (!GNU_MODE || !is_type_void(points_to)) {
			errorf(pos,
			       "arithmetic with pointer to incomplete type '%T' not allowed",
			       orig_pointer_type);
			return false;
		} else {
			warningf(WARN_POINTER_ARITH, pos,
			         "pointer of type '%T' used in arithmetic",
			         orig_pointer_type);
		}
	} else if (is_type_function(points_to)) {
		if (!GNU_MODE) {
			errorf(pos,
			       "arithmetic with pointer to function type '%T' not allowed",
			       orig_pointer_type);
			return false;
		} else {
			warningf(WARN_POINTER_ARITH, pos,
			         "pointer to a function '%T' used in arithmetic",
			         orig_pointer_type);
		}
	}
	return true;
}

static bool is_lvalue(const expression_t *expression)
{
	/* TODO: doesn't seem to be consistent with §6.3.2.1:1 */
	switch (expression->kind) {
	case EXPR_ARRAY_ACCESS:
	case EXPR_COMPOUND_LITERAL:
	case EXPR_REFERENCE:
	case EXPR_SELECT:
	case EXPR_STRING_LITERAL:
	case EXPR_UNARY_DEREFERENCE:
		return true;

	default: {
		type_t *type = skip_typeref(expression->base.type);
		/* ISO/IEC 14882:1998(E) §3.10:3 */
		return is_type_reference(type)
			/* Claim it is an lvalue, if the type is invalid.  There was a parse
			 * error before, which maybe prevented properly recognizing it as
			 * lvalue. */
		    || !is_type_valid(type);
	}
	}
}

static void semantic_incdec(unary_expression_t *expression)
{
	type_t *orig_type = expression->value->base.type;
	type_t *type      = skip_typeref(orig_type);
	if (is_type_pointer(type)) {
		if (!check_pointer_arithmetic(&expression->base.pos, type, orig_type))
			return;
	} else if (!is_type_real(type)
	        && (!GNU_MODE || !is_type_complex(type)) && is_type_valid(type)) {
		/* TODO: improve error message */
		position_t const *const pos = &expression->base.pos;
		if (!is_type_complex(type)) {
			errorf(pos, "operation needs an arithmetic or pointer type, but got '%T'", type);
			orig_type = type = type_error_type;
		} else if (!GNU_MODE) {
			errorf(pos, "operation on '%T' is a GCC extension", type);
		}
	}
	if (!is_lvalue(expression->value))
		/* TODO: improve error message */
		errorf(&expression->base.pos, "lvalue required as operand");
	expression->base.type = orig_type;
}

static void promote_unary_int_expr(unary_expression_t *const expr,
                                   type_t *const type)
{
	atomic_type_kind_t akind = get_arithmetic_akind(type);
	type_t *res_type;
	if (get_akind_rank(akind) < get_akind_rank(ATOMIC_TYPE_INT)) {
		if (type->kind == TYPE_COMPLEX)
			res_type = make_complex_type(ATOMIC_TYPE_INT, TYPE_QUALIFIER_NONE);
		else
			res_type = type_int;
	} else {
		res_type = type;
	}
	expr->base.type = res_type;
	expr->value     = create_implicit_cast(expr->value, res_type);
}

static void semantic_unexpr_arithmetic(unary_expression_t *expression)
{
	type_t *const orig_type = expression->value->base.type;
	type_t *const type      = skip_typeref(orig_type);
	if (!is_type_arithmetic(type)) {
		if (is_type_valid(type)) {
			position_t const *const pos = &expression->base.pos;
			errorf(pos,
			       "operand of unary expression must have arithmetic type, but is '%T'",
			       orig_type);
		}
		return;
	} else if (is_type_integer(type)) {
		promote_unary_int_expr(expression, type);
	} else {
		expression->base.type = orig_type;
	}
}

static void semantic_unexpr_plus(unary_expression_t *expression)
{
	semantic_unexpr_arithmetic(expression);
	position_t const *const pos = &expression->base.pos;
	warningf(WARN_TRADITIONAL, pos,
	         "traditional C rejects the unary plus operator");
}

static void semantic_not(unary_expression_t *expression)
{
	/* §6.5.3.3:1  The operand [...] of the ! operator, scalar type. */
	semantic_condition(expression->value, "operand of !");
	expression->base.type = dialect.cpp ? type_bool : type_int;
}

static void semantic_complement(unary_expression_t *expression)
{
	type_t *const orig_type = expression->value->base.type;
	type_t *const type      = skip_typeref(orig_type);
	if (!is_type_integer(type) && (!GNU_MODE || !is_type_complex(type))) {
		if (is_type_valid(type))
			errorf(&expression->base.pos,
			       "operand of ~ must be of integer type");
		return;
	}

	if (is_type_integer(type)) {
		promote_unary_int_expr(expression, type);
	} else {
		expression->base.type = orig_type;
	}
}

static void semantic_dereference(unary_expression_t *expression)
{
	type_t *const orig_type = expression->value->base.type;
	type_t *const type      = skip_typeref(orig_type);
	if (!is_type_pointer(type)) {
		if (is_type_valid(type))
			errorf(&expression->base.pos,
			       "Unary '*' needs pointer or array type, but type '%T' given",
			       orig_type);
		return;
	}

	type_t *result_type   = type->pointer.points_to;
	result_type           = automatic_type_conversion(result_type);
	expression->base.type = result_type;
}

/**
 * Record that an address is taken (expression represents an lvalue).
 *
 * @param expression       the expression
 * @param may_be_register  if true, the expression might be a register
 */
static void set_address_taken(expression_t *expression, bool may_be_register)
{
	if (expression->kind != EXPR_REFERENCE)
		return;

	entity_t *const entity = expression->reference.entity;

	if (entity->kind != ENTITY_VARIABLE && entity->kind != ENTITY_PARAMETER)
		return;

	if (entity->declaration.storage_class == STORAGE_CLASS_REGISTER
	 && !may_be_register) {
		position_t const *const pos = &expression->base.pos;
		errorf(pos, "address of register '%N' requested", entity);
	}

	entity->variable.address_taken = true;
}

/**
 * Check the semantic of the address taken expression.
 */
static void semantic_take_addr(unary_expression_t *expression)
{
	expression_t *value = expression->value;
	value->base.type    = revert_automatic_type_conversion(value);

	type_t *orig_type = value->base.type;
	type_t *type      = skip_typeref(orig_type);
	if (!is_type_valid(type))
		return;

	/* §6.5.3.2 */
	if (!is_lvalue(value))
		errorf(&expression->base.pos, "'&' requires an lvalue");
	if (is_bitfield(value))
		errorf(&expression->base.pos, "'&' not allowed on bitfield");

	set_address_taken(value, false);

	expression->base.type = make_pointer_type(orig_type, TYPE_QUALIFIER_NONE);
}

#define CREATE_UNARY_EXPRESSION_PARSER(token_kind, unexpression_type, sfunc) \
static expression_t *parse_##unexpression_type(void)                         \
{                                                                            \
	expression_t *unary_expression                                           \
		= allocate_expression_zero(unexpression_type);                       \
	eat(token_kind);                                                         \
	unary_expression->unary.value = parse_subexpression(PREC_UNARY);         \
	                                                                         \
	sfunc(&unary_expression->unary);                                         \
	                                                                         \
	return unary_expression;                                                 \
}

CREATE_UNARY_EXPRESSION_PARSER('-', EXPR_UNARY_NEGATE,
                               semantic_unexpr_arithmetic)
CREATE_UNARY_EXPRESSION_PARSER('+', EXPR_UNARY_PLUS,
                               semantic_unexpr_plus)
CREATE_UNARY_EXPRESSION_PARSER('!', EXPR_UNARY_NOT,
                               semantic_not)
CREATE_UNARY_EXPRESSION_PARSER('*', EXPR_UNARY_DEREFERENCE,
                               semantic_dereference)
CREATE_UNARY_EXPRESSION_PARSER('&', EXPR_UNARY_TAKE_ADDRESS,
                               semantic_take_addr)
CREATE_UNARY_EXPRESSION_PARSER('~', EXPR_UNARY_COMPLEMENT,
                               semantic_complement)
CREATE_UNARY_EXPRESSION_PARSER(T_PLUSPLUS,   EXPR_UNARY_PREFIX_INCREMENT,
                               semantic_incdec)
CREATE_UNARY_EXPRESSION_PARSER(T_MINUSMINUS, EXPR_UNARY_PREFIX_DECREMENT,
                               semantic_incdec)

#define CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(token_kind, unexpression_type, \
                                               sfunc)                         \
static expression_t *parse_##unexpression_type(expression_t *left)            \
{                                                                             \
	expression_t *unary_expression                                            \
		= allocate_expression_zero(unexpression_type);                        \
	eat(token_kind);                                                          \
	unary_expression->unary.value = left;                                     \
	                                                                          \
	sfunc(&unary_expression->unary);                                          \
                                                                              \
	return unary_expression;                                                  \
}

CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(T_PLUSPLUS,
                                       EXPR_UNARY_POSTFIX_INCREMENT,
                                       semantic_incdec)
CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(T_MINUSMINUS,
                                       EXPR_UNARY_POSTFIX_DECREMENT,
                                       semantic_incdec)

static atomic_type_kind_t semantic_arithmetic_(atomic_type_kind_t kind_left,
                                               atomic_type_kind_t kind_right)
{
	/* §6.3.1.8 Usual arithmetic conversions */
	if (kind_left == ATOMIC_TYPE_LONG_DOUBLE
	 || kind_right == ATOMIC_TYPE_LONG_DOUBLE) {
		return ATOMIC_TYPE_LONG_DOUBLE;
	} else if (kind_left == ATOMIC_TYPE_DOUBLE
	        || kind_right == ATOMIC_TYPE_DOUBLE) {
	    return ATOMIC_TYPE_DOUBLE;
	} else if (kind_left == ATOMIC_TYPE_FLOAT
	        || kind_right == ATOMIC_TYPE_FLOAT) {
		return ATOMIC_TYPE_FLOAT;
	}

	unsigned       rank_left  = get_akind_rank(kind_left);
	unsigned       rank_right = get_akind_rank(kind_right);
	unsigned const rank_int   = get_akind_rank(ATOMIC_TYPE_INT);
	if (rank_left < rank_int) {
		kind_left = ATOMIC_TYPE_INT;
		rank_left = rank_int;
	}
	if (rank_right < rank_int) {
		kind_right = ATOMIC_TYPE_INT;
		rank_right = rank_int;
	}
	if (kind_left == kind_right)
		return kind_left;

	bool const signed_left  = is_akind_signed(kind_left);
	bool const signed_right = is_akind_signed(kind_right);
	if (signed_left == signed_right)
		return rank_left >= rank_right ? kind_left : kind_right;

	unsigned           s_rank;
	unsigned           u_rank;
	atomic_type_kind_t s_kind;
	atomic_type_kind_t u_kind;
	if (signed_left) {
		s_kind = kind_left;
		s_rank = rank_left;
		u_kind = kind_right;
		u_rank = rank_right;
	} else {
		s_kind = kind_right;
		s_rank = rank_right;
		u_kind = kind_left;
		u_rank = rank_left;
	}
	if (u_rank >= s_rank)
		return u_kind;
	if (get_atomic_type_size(s_kind) > get_atomic_type_size(u_kind))
		return s_kind;

	switch (s_kind) {
	case ATOMIC_TYPE_INT:      return ATOMIC_TYPE_UINT;
	case ATOMIC_TYPE_LONG:     return ATOMIC_TYPE_ULONG;
	case ATOMIC_TYPE_LONGLONG: return ATOMIC_TYPE_ULONGLONG;
	default: panic("invalid atomic type");
	}
}

static type_t *semantic_arithmetic(type_t *type_left, type_t *type_right)
{
	atomic_type_kind_t kind_left  = get_arithmetic_akind(type_left);
	atomic_type_kind_t kind_right = get_arithmetic_akind(type_right);
	atomic_type_kind_t kind_res   = semantic_arithmetic_(kind_left, kind_right);

	if (type_left->kind == TYPE_COMPLEX || type_right->kind == TYPE_COMPLEX)
		return make_complex_type(kind_res, TYPE_QUALIFIER_NONE);
	return make_atomic_type(kind_res, TYPE_QUALIFIER_NONE);
}

static type_t *set_arithmetic_bin_expr_type(binary_expression_t *const bin,
                                            type_t *const type_left,
                                            type_t *const type_right)
{
	type_t *const res_type = semantic_arithmetic(type_left, type_right);
	bin->base.type = res_type;
	bin->left      = create_implicit_cast(bin->left,  res_type);
	bin->right     = create_implicit_cast(bin->right, res_type);
	return res_type;
}

/**
 * Check the semantic restrictions for a binary expression.
 */
static void semantic_binexpr_arithmetic(binary_expression_t *expression)
{
	expression_t *const left            = expression->left;
	expression_t *const right           = expression->right;
	type_t       *const orig_type_left  = left->base.type;
	type_t       *const orig_type_right = right->base.type;
	type_t       *const type_left       = skip_typeref(orig_type_left);
	type_t       *const type_right      = skip_typeref(orig_type_right);

	if (is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		set_arithmetic_bin_expr_type(expression, type_left, type_right);
	} else if (is_type_valid(type_left) && is_type_valid(type_right)) {
		position_t const *const pos = &expression->base.pos;
		errorf(pos, "operands of binary expression must have arithmetic types, but are '%T' and '%T'", orig_type_left, orig_type_right);
	}
}

static void semantic_binexpr_integer(binary_expression_t *const expression)
{
	expression_t *const left            = expression->left;
	expression_t *const right           = expression->right;
	type_t       *const orig_type_left  = left->base.type;
	type_t       *const orig_type_right = right->base.type;
	type_t       *const type_left       = skip_typeref(orig_type_left);
	type_t       *const type_right      = skip_typeref(orig_type_right);

	if (is_type_integer(type_left)  && !is_type_complex(type_left)
	 && is_type_integer(type_right) && !is_type_complex(type_right)) {
		set_arithmetic_bin_expr_type(expression, type_left, type_right);
	} else {
		if (is_type_valid(type_left) && is_type_valid(type_right)) {
			position_t const *const pos = &expression->base.pos;
			errorf(pos, "operands of binary expression must have integer types, but are '%T' and '%T'", orig_type_left, orig_type_right);
		}
	}
}

static void warn_div_by_zero(binary_expression_t const *const expression)
{
	if (!is_type_integer(expression->base.type))
		return;

	expression_t const *const right = expression->right;
	/* The type of the right operand can be different for /= */
	if (is_type_integer(skip_typeref(right->base.type))
	 && is_constant_expression(right) >= EXPR_CLASS_CONSTANT
	 && !fold_expression_to_bool(right)) {
		position_t const *const pos = &expression->base.pos;
		warningf(WARN_DIV_BY_ZERO, pos, "division by zero");
	}
}

/**
 * Check the semantic restrictions for a div expression.
 */
static void semantic_div(binary_expression_t *expression)
{
	semantic_binexpr_arithmetic(expression);
	warn_div_by_zero(expression);
}

/**
 * Check the semantic restrictions for a mod expression.
 */
static void semantic_mod(binary_expression_t *expression)
{
	semantic_binexpr_integer(expression);
	warn_div_by_zero(expression);
}

static void warn_addsub_in_shift(const expression_t *const expr)
{
	if (expr->base.parenthesized)
		return;

	char op;
	switch (expr->kind) {
	case EXPR_BINARY_ADD: op = '+'; break;
	case EXPR_BINARY_SUB: op = '-'; break;
	default:
		return;
	}

	position_t const *const pos = &expr->base.pos;
	warningf(WARN_PARENTHESES, pos,
	         "suggest parentheses around '%c' inside shift", op);
}

static bool semantic_shift(binary_expression_t *expression)
{
	expression_t *const left            = expression->left;
	expression_t *const right           = expression->right;
	type_t       *const orig_type_left  = left->base.type;
	type_t       *const orig_type_right = right->base.type;
	type_t       *      type_left       = skip_typeref(orig_type_left);
	type_t       *      type_right      = skip_typeref(orig_type_right);

	if (!is_type_integer(type_left) || !is_type_integer(type_right)) {
		if (is_type_valid(type_left) && is_type_valid(type_right)) {
			position_t const *const pos = &expression->base.pos;
			errorf(pos,
			       "operands of shift expression must have integer types, but are '%T' and '%T'",
			       orig_type_left, orig_type_right);
		}
		return false;
	}

	type_left = promote_integer(type_left);

	if (is_constant_expression(right) >= EXPR_CLASS_CONSTANT) {
		position_t const *const pos   = &right->base.pos;
		long              const count = fold_expression_to_int(right);
		if (count < 0) {
			warningf(WARN_OTHER, pos, "shift count must be non-negative");
		} else if ((unsigned long)count >=
		           get_atomic_type_size(type_left->atomic.akind) * 8) {
			warningf(WARN_OTHER, pos,
			         "shift count must be less than type width");
		}
	}

	type_right        = promote_integer(type_right);
	expression->right = create_implicit_cast(right, type_right);

	return true;
}

static void semantic_shift_op(binary_expression_t *expression)
{
	expression_t *const left  = expression->left;
	expression_t *const right = expression->right;

	if (!semantic_shift(expression))
		return;

	warn_addsub_in_shift(left);
	warn_addsub_in_shift(right);

	type_t *const orig_type_left = left->base.type;
	type_t *      type_left      = skip_typeref(orig_type_left);

	type_left             = promote_integer(type_left);
	expression->left      = create_implicit_cast(left, type_left);
	expression->base.type = type_left;
}

static void semantic_add(binary_expression_t *expression)
{
	expression_t *const left            = expression->left;
	expression_t *const right           = expression->right;
	type_t       *const orig_type_left  = left->base.type;
	type_t       *const orig_type_right = right->base.type;
	type_t       *const type_left       = skip_typeref(orig_type_left);
	type_t       *const type_right      = skip_typeref(orig_type_right);

	/* §6.5.6 */
	if (is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		set_arithmetic_bin_expr_type(expression, type_left, type_right);
	} else if (is_type_pointer(type_left) && is_type_integer(type_right)) {
		check_pointer_arithmetic(&expression->base.pos, type_left,
		                         orig_type_left);
		expression->base.type = type_left;
	} else if (is_type_pointer(type_right) && is_type_integer(type_left)) {
		check_pointer_arithmetic(&expression->base.pos, type_right,
		                         orig_type_right);
		expression->base.type = type_right;
	} else if (is_type_valid(type_left) && is_type_valid(type_right)) {
		errorf(&expression->base.pos,
		       "invalid operands to binary + ('%T', '%T')",
		       orig_type_left, orig_type_right);
	}
}

static void semantic_sub(binary_expression_t *expression)
{
	expression_t     *const left            = expression->left;
	expression_t     *const right           = expression->right;
	type_t           *const orig_type_left  = left->base.type;
	type_t           *const orig_type_right = right->base.type;
	type_t           *const type_left       = skip_typeref(orig_type_left);
	type_t           *const type_right      = skip_typeref(orig_type_right);
	position_t const *const pos             = &expression->base.pos;

	/* §5.6.5 */
	if (is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		set_arithmetic_bin_expr_type(expression, type_left, type_right);
	} else if (is_type_pointer(type_left) && is_type_integer(type_right)) {
		check_pointer_arithmetic(&expression->base.pos, type_left,
		                         orig_type_left);
		expression->base.type = type_left;
	} else if (is_type_pointer(type_left) && is_type_pointer(type_right)) {
		const type_t *const left_pt  = skip_typeref(type_left->pointer.points_to);
		const type_t *const right_pt = skip_typeref(type_right->pointer.points_to);
		if (!types_compatible_ignore_qualifiers(left_pt, right_pt)) {
			errorf(pos,
			       "subtracting pointers to incompatible types '%T' and '%T'",
			       orig_type_left, orig_type_right);
		} else if (!is_type_complete(type_left)) {
			if (is_type_void(type_left)) {
				warningf(WARN_OTHER, pos,
				         "pointer of type 'void*' used in subtraction");
			} else {
				errorf(pos, "arithmetic on pointer to an incomplete type");
			}
		} else if (is_type_function(type_left)) {
			warningf(WARN_OTHER, pos,
			         "pointer to a function used in subtraction");
		}
		expression->base.type = type_ptrdiff_t;
	} else if (is_type_valid(type_left) && is_type_valid(type_right)) {
		errorf(pos, "invalid operands of types '%T' and '%T' to binary '-'",
		       orig_type_left, orig_type_right);
	}
}

static void warn_string_literal_address(expression_t const* expr)
{
	while (expr->kind == EXPR_UNARY_TAKE_ADDRESS) {
		expr = expr->unary.value;
		if (expr->kind != EXPR_UNARY_DEREFERENCE)
			return;
		expr = expr->unary.value;
	}

	if (expr->kind == EXPR_STRING_LITERAL) {
		position_t const *const pos = &expr->base.pos;
		warningf(WARN_ADDRESS, pos,
		         "comparison with string literal results in unspecified behaviour");
	}
}

static bool maybe_negative(expression_t const *const expr)
{
	switch (is_constant_expression(expr)) {
	case EXPR_CLASS_ERROR:
		return false;

	case EXPR_CLASS_CONSTANT:
	case EXPR_CLASS_INTEGER_CONSTANT:
		return folded_expression_is_negative(expr);

	case EXPR_CLASS_VARIABLE:
		switch (expr->kind) {
		case EXPR_BINARY_EQUAL:
		case EXPR_BINARY_GREATER:
		case EXPR_BINARY_GREATEREQUAL:
		case EXPR_BINARY_ISGREATER:
		case EXPR_BINARY_ISGREATEREQUAL:
		case EXPR_BINARY_ISLESS:
		case EXPR_BINARY_ISLESSEQUAL:
		case EXPR_BINARY_ISLESSGREATER:
		case EXPR_BINARY_ISUNORDERED:
		case EXPR_BINARY_LESS:
		case EXPR_BINARY_LESSEQUAL:
		case EXPR_BINARY_LOGICAL_AND:
		case EXPR_BINARY_LOGICAL_OR:
		case EXPR_BINARY_NOTEQUAL:
		case EXPR_UNARY_NOT:
			/* The result of comparison and logical operators never is negative. */
			return false;

		case EXPR_BINARY_ASSIGN:
		case EXPR_BINARY_COMMA:
			return maybe_negative(expr->binary.right);

		case EXPR_CONDITIONAL: {
			conditional_expression_t const *const c = &expr->conditional;
			expression_t             const *const t = c->true_expression;
			return maybe_negative(t ? t : c->condition)
			    || maybe_negative(c->false_expression);
		}

		case EXPR_STATEMENT: {
			expression_t *const value_expr
				= get_statement_expression_last(&expr->statement);
			return value_expr && maybe_negative(value_expr);
		}

		case EXPR_SELECT: {
			/* A bitfield select from an unsigned type cannot become negative. */
			const entity_t *member = expr->select.compound_entry;
			assert(member->kind == ENTITY_COMPOUND_MEMBER);
			if (member->compound_member.bitfield) {
				const type_t *skipped = skip_typeref(member->declaration.type);
				return is_type_signed(skipped);
			}
			/* FALLTHROUGH */
		}

		default:
			return true;
		}
	}
	panic("invalid expression classification");
}

static void warn_comparison(position_t const *const pos,
                            expression_t const *const expr,
                            expression_t const *const other)
{
	warn_string_literal_address(expr);

	expression_t const* const ref = get_reference_address(expr);
	if (ref != NULL && is_null_pointer_constant(other)) {
		entity_t const *const ent = ref->reference.entity;
		warningf(WARN_ADDRESS, pos,
		         "the address of '%N' will never be NULL", ent);
	}

	if (!expr->base.parenthesized) {
		switch (expr->base.kind) {
		case EXPR_BINARY_LESS:
		case EXPR_BINARY_GREATER:
		case EXPR_BINARY_LESSEQUAL:
		case EXPR_BINARY_GREATEREQUAL:
		case EXPR_BINARY_NOTEQUAL:
		case EXPR_BINARY_EQUAL:
			warningf(WARN_PARENTHESES, pos,
			         "comparisons like 'x <= y < z' do not have their mathematical meaning");
			break;
		default:
			break;
		}
	}
}

/**
 * Check the semantics of comparison expressions.
 */
static void semantic_comparison(binary_expression_t *expression,
                                bool is_relational)
{
	position_t const *const pos   = &expression->base.pos;
	expression_t     *const left  = expression->left;
	expression_t     *const right = expression->right;

	warn_comparison(pos, left, right);
	warn_comparison(pos, right, left);

	type_t *orig_type_left  = left->base.type;
	type_t *orig_type_right = right->base.type;
	type_t *type_left       = skip_typeref(orig_type_left);
	type_t *type_right      = skip_typeref(orig_type_right);

	/* TODO non-arithmetic types */
	if (is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		type_t *const arithmetic_type
			= set_arithmetic_bin_expr_type(expression, type_left, type_right);

		/* test for signed vs unsigned compares */
		if (is_type_integer(arithmetic_type)
		 && !is_type_signed(arithmetic_type)) {
			bool const signed_left  = is_type_signed(type_left);
			bool const signed_right = is_type_signed(type_right);
			if (signed_left != signed_right
			 && ((signed_left && maybe_negative(left))
				 || (signed_right && maybe_negative(right))))
				warningf(WARN_SIGN_COMPARE, pos,
						 "comparison between signed and unsigned");
		}

		if (!is_relational && is_type_float(arithmetic_type))
			warningf(WARN_FLOAT_EQUAL, pos,
			         "comparing floating point with == or != is unsafe");
		/* for relational ops we need real types, not just arithmetic */
		if (is_relational
		 && (!is_type_real(type_left) || !is_type_real(type_right)))
			type_error_incompatible("invalid operands for relational operator",
			                        pos, type_left, type_right);
	} else if (is_type_pointer(type_left) && is_type_pointer(type_right)) {
		type_t *points_to_left  = type_left->pointer.points_to;
		type_t *points_to_right = type_right->pointer.points_to;
		type_t *skipped_pt_left  = skip_typeref(points_to_left);
		type_t *skipped_pt_right = skip_typeref(points_to_right);
		if (!is_type_void(skipped_pt_left) && !is_type_void(skipped_pt_right)
		 && !types_compatible_ignore_qualifiers(skipped_pt_left,
		                                        skipped_pt_right))
			warningf(WARN_DISTINCT_POINTER_TYPES, pos,
			         "comparison of distinct pointer types: '%T' and '%T'",
			         orig_type_left, orig_type_right);
	} else if (is_type_pointer(type_left) && is_type_integer(type_right)) {
		expression->right = create_implicit_cast(right, type_left);
		if (is_relational || !is_null_pointer_constant(right))
			goto int_ptr_warning;
	} else if (is_type_pointer(type_right) && is_type_integer(type_left)) {
		expression->left = create_implicit_cast(left, type_right);
		if (is_relational || !is_null_pointer_constant(left)) {
int_ptr_warning:
			warningf(WARN_OTHER, pos,
					 "comparison between pointer and integer: '%T' and '%T'",
					 orig_type_left, orig_type_right);
		}
	} else if (is_type_valid(type_left) && is_type_valid(type_right)) {
		type_error_incompatible("invalid operands in comparison", pos,
		                        type_left, type_right);
	}
	expression->base.type = dialect.cpp ? type_bool : type_int;
}

static void semantic_relational(binary_expression_t *expression)
{
	semantic_comparison(expression, true);
}

static void semantic_equality(binary_expression_t *expression)
{
	semantic_comparison(expression, false);
}

/**
 * Checks if a compound type has constant fields.
 */
static bool has_const_fields(const compound_type_t *type)
{
	compound_t const *compound = type->compound;
	for (entity_t const *entry = compound->members.first_entity; entry != NULL;
	     entry = entry->base.next) {
		if (!is_declaration(entry) || entry->kind == ENTITY_TYPEDEF)
			continue;

		const type_t *decl_type = skip_typeref(entry->declaration.type);
		if (decl_type->base.qualifiers & TYPE_QUALIFIER_CONST)
			return true;
	}

	return false;
}

static bool is_valid_assignment_lhs(expression_t const* const left)
{
	type_t *const orig_type_left = revert_automatic_type_conversion(left);
	type_t *const type_left      = skip_typeref(orig_type_left);

	if (!is_lvalue(left)) {
		errorf(&left->base.pos,
		       "left hand side '%E' of assignment is not an lvalue", left);
		return false;
	}

	if (left->kind == EXPR_REFERENCE
	 && left->reference.entity->kind == ENTITY_FUNCTION) {
		errorf(&left->base.pos, "cannot assign to function '%E'", left);
		return false;
	}

	if (is_type_array(type_left)) {
		errorf(&left->base.pos, "cannot assign to array '%E'", left);
		return false;
	}
	if (type_left->base.qualifiers & TYPE_QUALIFIER_CONST) {
		errorf(&left->base.pos,
		       "assignment to read-only location '%E' (type '%T')", left,
		       orig_type_left);
		return false;
	}
	if (!is_type_complete(type_left)) {
		if (is_type_valid(type_left))
			errorf(&left->base.pos, "left-hand side '%E' of assignment has incomplete type '%T'", left, orig_type_left);
		return false;
	}
	if (is_type_compound(type_left) && has_const_fields(&type_left->compound)) {
		errorf(&left->base.pos,
		       "cannot assign to '%E' because compound type '%T' has read-only fields",
		       left, orig_type_left);
		return false;
	}

	return true;
}

static void semantic_arithmetic_assign(binary_expression_t *expression)
{
	expression_t *left            = expression->left;
	expression_t *right           = expression->right;
	type_t       *orig_type_left  = left->base.type;
	type_t       *orig_type_right = right->base.type;

	if (!is_valid_assignment_lhs(left))
		return;

	type_t *type_left  = skip_typeref(orig_type_left);
	type_t *type_right = skip_typeref(orig_type_right);

	if (!is_type_arithmetic(type_left) || !is_type_arithmetic(type_right)) {
		/* TODO: improve error message */
		if (is_type_valid(type_left) && is_type_valid(type_right))
			errorf(&expression->base.pos, "operation needs arithmetic types");
		return;
	}

	/* combined instructions are tricky. We can't create an implicit cast on
	 * the left side, because we need the uncasted form for the store.
	 * The ast2firm pass has to know that left_type must be right_type
	 * for the arithmetic operation and create a cast by itself */
	type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
	expression->right       = create_implicit_cast(right, arithmetic_type);
	expression->base.type   = type_left;
}

static void semantic_divmod_assign(binary_expression_t *expression)
{
	semantic_arithmetic_assign(expression);
	warn_div_by_zero(expression);
}

static void semantic_arithmetic_addsubb_assign(binary_expression_t *expression)
{
	expression_t *const left            = expression->left;
	expression_t *const right           = expression->right;
	type_t       *const orig_type_left  = left->base.type;
	type_t       *const orig_type_right = right->base.type;
	type_t       *const type_left       = skip_typeref(orig_type_left);
	type_t       *const type_right      = skip_typeref(orig_type_right);

	if (!is_valid_assignment_lhs(left))
		return;

	if (is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		/* combined instructions are tricky. We can't create an implicit cast on
		 * the left side, because we need the uncasted form for the store.
		 * The ast2firm pass has to know that left_type must be right_type
		 * for the arithmetic operation and create a cast by itself */
		type_t *const arithmetic_type = semantic_arithmetic(type_left, type_right);
		expression->right     = create_implicit_cast(right, arithmetic_type);
		expression->base.type = type_left;
	} else if (is_type_pointer(type_left) && is_type_integer(type_right)) {
		check_pointer_arithmetic(&expression->base.pos, type_left,
		                         orig_type_left);
		expression->base.type = type_left;
	} else if (is_type_valid(type_left) && is_type_valid(type_right)) {
		errorf(&expression->base.pos,
		       "incompatible types '%T' and '%T' in assignment",
		       orig_type_left, orig_type_right);
	}
}

static void semantic_integer_assign(binary_expression_t *expression)
{
	expression_t *left            = expression->left;
	expression_t *right           = expression->right;
	type_t       *orig_type_left  = left->base.type;
	type_t       *orig_type_right = right->base.type;

	if (!is_valid_assignment_lhs(left))
		return;

	type_t *type_left  = skip_typeref(orig_type_left);
	type_t *type_right = skip_typeref(orig_type_right);

	if (!is_type_integer(type_left) || !is_type_integer(type_right)) {
		/* TODO: improve error message */
		if (is_type_valid(type_left) && is_type_valid(type_right))
			errorf(&expression->base.pos, "operation needs integer types");
		return;
	}

	/* combined instructions are tricky. We can't create an implicit cast on
	 * the left side, because we need the uncasted form for the store.
	 * The ast2firm pass has to know that left_type must be right_type
	 * for the arithmetic operation and create a cast by itself */
	type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
	expression->right       = create_implicit_cast(right, arithmetic_type);
	expression->base.type   = type_left;
}

static void semantic_shift_assign(binary_expression_t *expression)
{
	expression_t *left           = expression->left;

	if (!is_valid_assignment_lhs(left))
		return;

	if (!semantic_shift(expression))
		return;

	expression->base.type = skip_typeref(left->base.type);
}

static void warn_logical_and_within_or(const expression_t *const expr)
{
	if (expr->base.kind != EXPR_BINARY_LOGICAL_AND)
		return;
	if (expr->base.parenthesized)
		return;
	position_t const *const pos = &expr->base.pos;
	warningf(WARN_PARENTHESES, pos, "suggest parentheses around && within ||");
}

/**
 * Check the semantic restrictions of a logical expression.
 */
static void semantic_logical_op(binary_expression_t *expression)
{
	/* §6.5.13:2  Each of the operands shall have scalar type.
	 * §6.5.14:2  Each of the operands shall have scalar type. */
	semantic_condition(expression->left, "left operand of logical operator");
	semantic_condition(expression->right, "right operand of logical operator");
	if (expression->base.kind == EXPR_BINARY_LOGICAL_OR) {
		warn_logical_and_within_or(expression->left);
		warn_logical_and_within_or(expression->right);
	}
	expression->base.type = dialect.cpp ? type_bool : type_int;
}

static bool is_ambiguous_unary_expression_kind(expression_kind_t expression_kind)
{
	switch (expression_kind) {
	case EXPR_UNARY_DEREFERENCE:
	case EXPR_UNARY_PLUS:
	case EXPR_UNARY_NEGATE:
		return true;
	default:
		return false;
	}
}

static bool are_positions_contiguous(const position_t *pos_first,
                                     const position_t *pos_second)
{
	return pos_first->colno + 1  == pos_second->colno
	    && pos_first->lineno     == pos_second->lineno
	    && pos_first->input_name == pos_second->input_name;
}

/**
 * Normally expressions like bitfield selects undergo automatic type
 * promotion when used as a value, however when used as an lhs values we want
 * to see a cast to the unconverted type.
 */
static type_t *get_lhs_type(expression_t *expression)
{
	if (is_bitfield(expression)) {
		assert(expression->kind == EXPR_SELECT);
		const entity_t *member   = expression->select.compound_entry;
		type_t         *lhs_type = member->declaration.type;
		return lhs_type;
	}
	return expression->base.type;
}

/**
 * Check the semantic restrictions of a binary assign expression.
 */
static void semantic_binexpr_assign(binary_expression_t *expression)
{
	/* If an equal sign is followed by an infix operator without spaces then it
	 * was probably intended to be a compound assignment */
	if (expression->base.kind == EXPR_BINARY_ASSIGN
	 && is_ambiguous_unary_expression_kind(expression->right->kind)
	 && are_positions_contiguous(&expression->base.pos,
	                             &expression->right->base.pos))
		warningf(WARN_NOT_COMPOUND_ASSIGN, &expression->base.pos,
				 "use of unary operator that may be intended as compound assignment (%hs%E)",
				 "=", expression->right);

	expression_t *left = expression->left;
	if (!is_valid_assignment_lhs(left))
		return;


	type_t        *assign_type = get_lhs_type(left);
	assign_error_t error       = semantic_assign(assign_type, expression->right);
	report_assign_error(error, assign_type, expression->right, "assignment",
	                    &left->base.pos);
	expression->right = create_implicit_cast(expression->right, assign_type);
	expression->base.type = left->base.type;
}

/**
 * Determine if the outermost operation (or parts thereof) of the given
 * expression has no effect in order to generate a warning about this fact.
 * Therefore in some cases this only examines some of the operands of the
 * expression (see comments in the function and examples below).
 * Examples:
 *   f() + 23;    // warning, because + has no effect
 *   x || f();    // no warning, because x controls execution of f()
 *   x ? y : f(); // warning, because y has no effect
 *   (void)x;     // no warning to be able to suppress the warning
 * This function can NOT be used for an "expression has definitely no effect"-
 * analysis. */
static bool expression_has_effect(const expression_t *const expr)
{
	switch (expr->kind) {
	case EXPR_ERROR:                      return true; /* do NOT warn */
	case EXPR_REFERENCE:                  return false;
	case EXPR_ENUM_CONSTANT:              return false;
	case EXPR_LABEL_ADDRESS:              return false;

	/* suppress the warning for microsoft __noop operations */
	case EXPR_LITERAL_MS_NOOP:            return true;
	case EXPR_LITERAL_BOOLEAN:
	case EXPR_LITERAL_CHARACTER:
	case EXPR_LITERAL_INTEGER:
	case EXPR_LITERAL_FLOATINGPOINT:
	case EXPR_STRING_LITERAL:             return false;

	case EXPR_CALL: {
		const call_expression_t *const call = &expr->call;
		if (call->function->kind != EXPR_REFERENCE)
			return true;

		switch (call->function->reference.entity->function.btk) {
		/* FIXME: which builtins have no effect? */
		default:                      return true;
		}
	}

	/* Generate the warning if either the left or right hand side of a
	 * conditional expression has no effect */
	case EXPR_CONDITIONAL: {
		conditional_expression_t const *const cond = &expr->conditional;
		expression_t             const *const t    = cond->true_expression;
		return (t == NULL || expression_has_effect(t))
		    && expression_has_effect(cond->false_expression);
	}

	case EXPR_SELECT:                     return false;
	case EXPR_ARRAY_ACCESS:               return false;
	case EXPR_SIZEOF:                     return false;
	case EXPR_CLASSIFY_TYPE:              return false;
	case EXPR_ALIGNOF:                    return false;

	case EXPR_FUNCNAME:                   return false;
	case EXPR_BUILTIN_CONSTANT_P:         return false;
	case EXPR_BUILTIN_TYPES_COMPATIBLE_P: return false;
	case EXPR_OFFSETOF:                   return false;
	case EXPR_VA_START:                   return true;
	case EXPR_VA_ARG:                     return true;
	case EXPR_VA_COPY:                    return true;
	case EXPR_STATEMENT:                  return true; // TODO
	case EXPR_COMPOUND_LITERAL:           return false;

	case EXPR_UNARY_NEGATE:               return false;
	case EXPR_UNARY_PLUS:                 return false;
	case EXPR_UNARY_COMPLEMENT:           return false;
	case EXPR_UNARY_NOT:                  return false;
	case EXPR_UNARY_DEREFERENCE:          return false;
	case EXPR_UNARY_TAKE_ADDRESS:         return false;
	case EXPR_UNARY_REAL:                 return false;
	case EXPR_UNARY_IMAG:                 return false;
	case EXPR_UNARY_POSTFIX_INCREMENT:    return true;
	case EXPR_UNARY_POSTFIX_DECREMENT:    return true;
	case EXPR_UNARY_PREFIX_INCREMENT:     return true;
	case EXPR_UNARY_PREFIX_DECREMENT:     return true;

	/* Treat void casts as if they have an effect in order to being able to
	 * suppress the warning */
	case EXPR_UNARY_CAST: {
		type_t *const type = skip_typeref(expr->base.type);
		return is_type_void(type);
	}

	case EXPR_UNARY_ASSUME:               return true;
	case EXPR_UNARY_DELETE:               return true;
	case EXPR_UNARY_DELETE_ARRAY:         return true;
	case EXPR_UNARY_THROW:                return true;

	case EXPR_BINARY_ADD:                 return false;
	case EXPR_BINARY_SUB:                 return false;
	case EXPR_BINARY_MUL:                 return false;
	case EXPR_BINARY_DIV:                 return false;
	case EXPR_BINARY_MOD:                 return false;
	case EXPR_BINARY_EQUAL:               return false;
	case EXPR_BINARY_NOTEQUAL:            return false;
	case EXPR_BINARY_LESS:                return false;
	case EXPR_BINARY_LESSEQUAL:           return false;
	case EXPR_BINARY_GREATER:             return false;
	case EXPR_BINARY_GREATEREQUAL:        return false;
	case EXPR_BINARY_BITWISE_AND:         return false;
	case EXPR_BINARY_BITWISE_OR:          return false;
	case EXPR_BINARY_BITWISE_XOR:         return false;
	case EXPR_BINARY_SHIFTLEFT:           return false;
	case EXPR_BINARY_SHIFTRIGHT:          return false;
	case EXPR_BINARY_ASSIGN:              return true;
	case EXPR_BINARY_MUL_ASSIGN:          return true;
	case EXPR_BINARY_DIV_ASSIGN:          return true;
	case EXPR_BINARY_MOD_ASSIGN:          return true;
	case EXPR_BINARY_ADD_ASSIGN:          return true;
	case EXPR_BINARY_SUB_ASSIGN:          return true;
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:    return true;
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:   return true;
	case EXPR_BINARY_BITWISE_AND_ASSIGN:  return true;
	case EXPR_BINARY_BITWISE_XOR_ASSIGN:  return true;
	case EXPR_BINARY_BITWISE_OR_ASSIGN:   return true;

	/* Only examine the right hand side of && and ||, because the left hand
	 * side already has the effect of controlling the execution of the right
	 * hand side */
	case EXPR_BINARY_LOGICAL_AND:
	case EXPR_BINARY_LOGICAL_OR:
	/* Only examine the right hand side of a comma expression, because the left
	 * hand side has a separate warning */
	case EXPR_BINARY_COMMA:
		return expression_has_effect(expr->binary.right);

	case EXPR_BINARY_ISGREATER:           return false;
	case EXPR_BINARY_ISGREATEREQUAL:      return false;
	case EXPR_BINARY_ISLESS:              return false;
	case EXPR_BINARY_ISLESSEQUAL:         return false;
	case EXPR_BINARY_ISLESSGREATER:       return false;
	case EXPR_BINARY_ISUNORDERED:         return false;
	}

	panic("unexpected expression");
}

static void semantic_comma(binary_expression_t *expression)
{
	const expression_t *const left = expression->left;
	if (!expression_has_effect(left)) {
		position_t const *const pos = &left->base.pos;
		warningf(WARN_UNUSED_VALUE, pos,
		         "left-hand operand of comma expression has no effect");
	}
	expression->base.type = expression->right->base.type;
}

/**
 * @param prec_r precedence of the right operand
 */
#define CREATE_BINEXPR_PARSER(token_kind, binexpression_type, prec_r, sfunc) \
static expression_t *parse_##binexpression_type(expression_t *left)          \
{                                                                            \
	expression_t *binexpr = allocate_expression_zero(binexpression_type);    \
	binexpr->binary.left  = left;                                            \
	eat(token_kind);                                                         \
                                                                             \
	expression_t *right = parse_subexpression(prec_r);                       \
                                                                             \
	binexpr->binary.right = right;                                           \
	sfunc(&binexpr->binary);                                                 \
                                                                             \
	return binexpr;                                                          \
}

CREATE_BINEXPR_PARSER('*',                    EXPR_BINARY_MUL,                PREC_CAST,           semantic_binexpr_arithmetic)
CREATE_BINEXPR_PARSER('/',                    EXPR_BINARY_DIV,                PREC_CAST,           semantic_div)
CREATE_BINEXPR_PARSER('%',                    EXPR_BINARY_MOD,                PREC_CAST,           semantic_mod)
CREATE_BINEXPR_PARSER('+',                    EXPR_BINARY_ADD,                PREC_MULTIPLICATIVE, semantic_add)
CREATE_BINEXPR_PARSER('-',                    EXPR_BINARY_SUB,                PREC_MULTIPLICATIVE, semantic_sub)
CREATE_BINEXPR_PARSER(T_LESSLESS,             EXPR_BINARY_SHIFTLEFT,          PREC_ADDITIVE,       semantic_shift_op)
CREATE_BINEXPR_PARSER(T_GREATERGREATER,       EXPR_BINARY_SHIFTRIGHT,         PREC_ADDITIVE,       semantic_shift_op)
CREATE_BINEXPR_PARSER('<',                    EXPR_BINARY_LESS,               PREC_SHIFT,          semantic_relational)
CREATE_BINEXPR_PARSER('>',                    EXPR_BINARY_GREATER,            PREC_SHIFT,          semantic_relational)
CREATE_BINEXPR_PARSER(T_LESSEQUAL,            EXPR_BINARY_LESSEQUAL,          PREC_SHIFT,          semantic_relational)
CREATE_BINEXPR_PARSER(T_GREATEREQUAL,         EXPR_BINARY_GREATEREQUAL,       PREC_SHIFT,          semantic_relational)
CREATE_BINEXPR_PARSER(T_EXCLAMATIONMARKEQUAL, EXPR_BINARY_NOTEQUAL,           PREC_RELATIONAL,     semantic_equality)
CREATE_BINEXPR_PARSER(T_EQUALEQUAL,           EXPR_BINARY_EQUAL,              PREC_RELATIONAL,     semantic_equality)
CREATE_BINEXPR_PARSER('&',                    EXPR_BINARY_BITWISE_AND,        PREC_EQUALITY,       semantic_binexpr_integer)
CREATE_BINEXPR_PARSER('^',                    EXPR_BINARY_BITWISE_XOR,        PREC_AND,            semantic_binexpr_integer)
CREATE_BINEXPR_PARSER('|',                    EXPR_BINARY_BITWISE_OR,         PREC_XOR,            semantic_binexpr_integer)
CREATE_BINEXPR_PARSER(T_ANDAND,               EXPR_BINARY_LOGICAL_AND,        PREC_OR,             semantic_logical_op)
CREATE_BINEXPR_PARSER(T_PIPEPIPE,             EXPR_BINARY_LOGICAL_OR,         PREC_LOGICAL_AND,    semantic_logical_op)
CREATE_BINEXPR_PARSER('=',                    EXPR_BINARY_ASSIGN,             PREC_ASSIGNMENT,     semantic_binexpr_assign)
CREATE_BINEXPR_PARSER(T_PLUSEQUAL,            EXPR_BINARY_ADD_ASSIGN,         PREC_ASSIGNMENT,     semantic_arithmetic_addsubb_assign)
CREATE_BINEXPR_PARSER(T_MINUSEQUAL,           EXPR_BINARY_SUB_ASSIGN,         PREC_ASSIGNMENT,     semantic_arithmetic_addsubb_assign)
CREATE_BINEXPR_PARSER(T_ASTERISKEQUAL,        EXPR_BINARY_MUL_ASSIGN,         PREC_ASSIGNMENT,     semantic_arithmetic_assign)
CREATE_BINEXPR_PARSER(T_SLASHEQUAL,           EXPR_BINARY_DIV_ASSIGN,         PREC_ASSIGNMENT,     semantic_divmod_assign)
CREATE_BINEXPR_PARSER(T_PERCENTEQUAL,         EXPR_BINARY_MOD_ASSIGN,         PREC_ASSIGNMENT,     semantic_divmod_assign)
CREATE_BINEXPR_PARSER(T_LESSLESSEQUAL,        EXPR_BINARY_SHIFTLEFT_ASSIGN,   PREC_ASSIGNMENT,     semantic_shift_assign)
CREATE_BINEXPR_PARSER(T_GREATERGREATEREQUAL,  EXPR_BINARY_SHIFTRIGHT_ASSIGN,  PREC_ASSIGNMENT,     semantic_shift_assign)
CREATE_BINEXPR_PARSER(T_ANDEQUAL,             EXPR_BINARY_BITWISE_AND_ASSIGN, PREC_ASSIGNMENT,     semantic_integer_assign)
CREATE_BINEXPR_PARSER(T_PIPEEQUAL,            EXPR_BINARY_BITWISE_OR_ASSIGN,  PREC_ASSIGNMENT,     semantic_integer_assign)
CREATE_BINEXPR_PARSER(T_CARETEQUAL,           EXPR_BINARY_BITWISE_XOR_ASSIGN, PREC_ASSIGNMENT,     semantic_integer_assign)
CREATE_BINEXPR_PARSER(',',                    EXPR_BINARY_COMMA,              PREC_ASSIGNMENT,     semantic_comma)


static expression_t *parse_subexpression(precedence_t precedence)
{
	expression_parser_function_t *parser = &expression_parsers[token.kind];
	expression_t                 *left;

	if (parser->parser != NULL) {
		left = parser->parser();
	} else {
		left = parse_primary_expression();
	}
	assert(left != NULL);

	while (true) {
		parser = &expression_parsers[token.kind];
		if (parser->infix_parser == NULL)
			break;
		if (parser->infix_precedence < precedence)
			break;

		left = parser->infix_parser(left);

		assert(left != NULL);
	}

	return left;
}

/**
 * Parse an expression.
 */
static expression_t *parse_expression(void)
{
	return parse_subexpression(PREC_EXPRESSION);
}

/**
 * Register a parser for a prefix-like operator.
 *
 * @param parser      the parser function
 * @param token_kind  the token type of the prefix token
 */
static void register_expression_parser(parse_expression_function parser,
                                       int token_kind)
{
	expression_parser_function_t *entry = &expression_parsers[token_kind];

	assert(!entry->parser);
	entry->parser = parser;
}

/**
 * Register a parser for an infix operator with given precedence.
 *
 * @param parser      the parser function
 * @param token_kind  the token type of the infix operator
 * @param precedence  the precedence of the operator
 */
static void register_infix_parser(parse_expression_infix_function parser,
                                  int token_kind, precedence_t precedence)
{
	expression_parser_function_t *entry = &expression_parsers[token_kind];

	assert(!entry->infix_parser);
	entry->infix_parser     = parser;
	entry->infix_precedence = precedence;
}

/**
 * Initialize the expression parsers.
 */
static void init_expression_parsers(void)
{
	memset(&expression_parsers, 0, sizeof(expression_parsers));

	register_infix_parser(parse_array_expression,               '[',                    PREC_POSTFIX);
	register_infix_parser(parse_call_expression,                '(',                    PREC_POSTFIX);
	register_infix_parser(parse_select_expression,              '.',                    PREC_POSTFIX);
	register_infix_parser(parse_select_expression,              T_MINUSGREATER,         PREC_POSTFIX);
	register_infix_parser(parse_EXPR_UNARY_POSTFIX_INCREMENT,   T_PLUSPLUS,             PREC_POSTFIX);
	register_infix_parser(parse_EXPR_UNARY_POSTFIX_DECREMENT,   T_MINUSMINUS,           PREC_POSTFIX);
	register_infix_parser(parse_EXPR_BINARY_MUL,                '*',                    PREC_MULTIPLICATIVE);
	register_infix_parser(parse_EXPR_BINARY_DIV,                '/',                    PREC_MULTIPLICATIVE);
	register_infix_parser(parse_EXPR_BINARY_MOD,                '%',                    PREC_MULTIPLICATIVE);
	register_infix_parser(parse_EXPR_BINARY_ADD,                '+',                    PREC_ADDITIVE);
	register_infix_parser(parse_EXPR_BINARY_SUB,                '-',                    PREC_ADDITIVE);
	register_infix_parser(parse_EXPR_BINARY_SHIFTLEFT,          T_LESSLESS,             PREC_SHIFT);
	register_infix_parser(parse_EXPR_BINARY_SHIFTRIGHT,         T_GREATERGREATER,       PREC_SHIFT);
	register_infix_parser(parse_EXPR_BINARY_LESS,               '<',                    PREC_RELATIONAL);
	register_infix_parser(parse_EXPR_BINARY_GREATER,            '>',                    PREC_RELATIONAL);
	register_infix_parser(parse_EXPR_BINARY_LESSEQUAL,          T_LESSEQUAL,            PREC_RELATIONAL);
	register_infix_parser(parse_EXPR_BINARY_GREATEREQUAL,       T_GREATEREQUAL,         PREC_RELATIONAL);
	register_infix_parser(parse_EXPR_BINARY_EQUAL,              T_EQUALEQUAL,           PREC_EQUALITY);
	register_infix_parser(parse_EXPR_BINARY_NOTEQUAL,           T_EXCLAMATIONMARKEQUAL, PREC_EQUALITY);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_AND,        '&',                    PREC_AND);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_XOR,        '^',                    PREC_XOR);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_OR,         '|',                    PREC_OR);
	register_infix_parser(parse_EXPR_BINARY_LOGICAL_AND,        T_ANDAND,               PREC_LOGICAL_AND);
	register_infix_parser(parse_EXPR_BINARY_LOGICAL_OR,         T_PIPEPIPE,             PREC_LOGICAL_OR);
	register_infix_parser(parse_conditional_expression,         '?',                    PREC_CONDITIONAL);
	register_infix_parser(parse_EXPR_BINARY_ASSIGN,             '=',                    PREC_ASSIGNMENT);
	register_infix_parser(parse_EXPR_BINARY_ADD_ASSIGN,         T_PLUSEQUAL,            PREC_ASSIGNMENT);
	register_infix_parser(parse_EXPR_BINARY_SUB_ASSIGN,         T_MINUSEQUAL,           PREC_ASSIGNMENT);
	register_infix_parser(parse_EXPR_BINARY_MUL_ASSIGN,         T_ASTERISKEQUAL,        PREC_ASSIGNMENT);
	register_infix_parser(parse_EXPR_BINARY_DIV_ASSIGN,         T_SLASHEQUAL,           PREC_ASSIGNMENT);
	register_infix_parser(parse_EXPR_BINARY_MOD_ASSIGN,         T_PERCENTEQUAL,         PREC_ASSIGNMENT);
	register_infix_parser(parse_EXPR_BINARY_SHIFTLEFT_ASSIGN,   T_LESSLESSEQUAL,        PREC_ASSIGNMENT);
	register_infix_parser(parse_EXPR_BINARY_SHIFTRIGHT_ASSIGN,  T_GREATERGREATEREQUAL,  PREC_ASSIGNMENT);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_AND_ASSIGN, T_ANDEQUAL,             PREC_ASSIGNMENT);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_OR_ASSIGN,  T_PIPEEQUAL,            PREC_ASSIGNMENT);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_XOR_ASSIGN, T_CARETEQUAL,           PREC_ASSIGNMENT);
	register_infix_parser(parse_EXPR_BINARY_COMMA,              ',',                    PREC_EXPRESSION);

	register_expression_parser(parse_EXPR_UNARY_NEGATE,           '-');
	register_expression_parser(parse_EXPR_UNARY_PLUS,             '+');
	register_expression_parser(parse_EXPR_UNARY_NOT,              '!');
	register_expression_parser(parse_EXPR_UNARY_COMPLEMENT,       '~');
	register_expression_parser(parse_EXPR_UNARY_DEREFERENCE,      '*');
	register_expression_parser(parse_EXPR_UNARY_TAKE_ADDRESS,     '&');
	register_expression_parser(parse_EXPR_UNARY_PREFIX_INCREMENT, T_PLUSPLUS);
	register_expression_parser(parse_EXPR_UNARY_PREFIX_DECREMENT, T_MINUSMINUS);
	register_expression_parser(parse_sizeof,                      T_sizeof);
	register_expression_parser(parse_alignof,                     T__Alignof);
	register_expression_parser(parse_extension,                   T___extension__);
	register_expression_parser(parse_builtin_classify_type,       T___builtin_classify_type);
	register_expression_parser(parse_delete,                      T_delete);
	register_expression_parser(parse_throw,                       T_throw);
}

static void semantic_asm_argument(asm_argument_t *argument, bool is_out)
{
	expression_t *expression = argument->expression;
	type_t       *type       = skip_typeref(expression->base.type);
	if (!is_type_valid(type))
		return;

	const char *constraints = argument->constraints->begin;
	asm_constraint_flags_t asm_flags = be_parse_asm_constraints(constraints);
	if (asm_flags & ASM_CONSTRAINT_FLAG_INVALID) {
		errorf(&argument->base.pos, "some constraints in '%s' are invalid",
		       constraints);
		return;
	}
	if (asm_flags & ASM_CONSTRAINT_FLAG_NO_SUPPORT) {
		errorf(&argument->base.pos,
		       "some constraints in '%s' are not supported on target",
		       constraints);
		return;
	}

	if (is_out) {
		if ((asm_flags & ASM_CONSTRAINT_FLAG_MODIFIER_WRITE) == 0)
			errorf(&argument->base.pos,
			       "constraints '%s' for output operand do not indicate write",
			       constraints);

		/* Ugly GCC stuff: Allow lvalue casts.  Skip casts, when they do
		 * not change size or type representation (e.g. int -> long is
		 * ok, but int -> float is not) */
		if (expression->kind == EXPR_UNARY_CAST) {
			type_kind_t const kind = type->kind;
			if (kind == TYPE_ATOMIC || kind == TYPE_POINTER) {
				unsigned flags;
				unsigned size;
				if (kind == TYPE_ATOMIC) {
					atomic_type_kind_t const akind = type->atomic.akind;
					flags = get_atomic_type_flags(akind) & ~ATOMIC_TYPE_FLAG_SIGNED;
					size  = get_atomic_type_size(akind);
				} else {
					flags = ATOMIC_TYPE_FLAG_INTEGER;
					size  = get_type_size(type_void_ptr);
				}

				do {
					expression_t *const value      = expression->unary.value;
					type_t       *const value_type = value->base.type;
					type_kind_t   const value_kind = value_type->kind;

					unsigned value_flags;
					unsigned value_size;
					if (value_kind == TYPE_ATOMIC) {
						atomic_type_kind_t const value_akind = value_type->atomic.akind;
						value_flags = get_atomic_type_flags(value_akind) & ~ATOMIC_TYPE_FLAG_SIGNED;
						value_size  = get_atomic_type_size(value_akind);
					} else if (value_kind == TYPE_POINTER) {
						value_flags = ATOMIC_TYPE_FLAG_INTEGER;
						value_size  = get_type_size(type_void_ptr);
					} else {
						break;
					}

					if (value_flags != flags || value_size != size)
						break;

					expression = value;
				} while (expression->kind == EXPR_UNARY_CAST);
				argument->expression = expression;
			}
		}

		if (!is_lvalue(expression))
			errorf(&expression->base.pos,
			       "asm output argument is not an lvalue");
	} else if (asm_flags & ASM_CONSTRAINT_FLAG_MODIFIER_WRITE) {
		errorf(&argument->base.pos,
			   "constraints '%s' for input operand indicate write",
			   constraints);
	}

	if (asm_flags & ASM_CONSTRAINT_FLAG_MODIFIER_WRITE) {
		if (asm_flags & (ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER|ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE)) {
			determine_lhs_ent(expression, NULL);
			argument->direct_write = true;
		} else {
			mark_vars_read(expression, NULL);
		}
		if (asm_flags & ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP)
			argument->indirect_write = true;
	}
	if (asm_flags & ASM_CONSTRAINT_FLAG_MODIFIER_READ) {
		mark_vars_read(expression, NULL);
		if (asm_flags & (ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER|ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE)) {
			determine_lhs_ent(expression, NULL);
			argument->direct_read = true;
		} else {
			mark_vars_read(expression, NULL);
		}
		if (asm_flags & ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP)
			argument->indirect_read = true;
	}
	/* in case of only indirect accesses we are forced to use an address
	 * as argument, and therefore must mark the value as address taken */
	if (asm_flags & ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP
	 && (argument->direct_write | argument->direct_read) == 0)
		set_address_taken(expression, true);
}

/**
 * Parse a asm statement arguments specification.
 */
static entity_t *parse_asm_arguments(bool const is_out)
{
	char const *const context = "asm argument";

	entity_t  *result = NULL;
	entity_t **anchor = &result;
	if (peek(T_STRING_LITERAL) || peek('[')) {
		add_anchor_token(',');
		do {
			position_t pos = *HERE;

			add_anchor_token(')');
			add_anchor_token('(');
			add_anchor_token(T_STRING_LITERAL);

			symbol_t *symbol = NULL;
			if (accept('[')) {
				add_anchor_token(']');
				symbol = expect_identifier(context, NULL);
				rem_anchor_token(']');
				expect(']');
			}

			entity_t *argument
				= allocate_entity_zero(ENTITY_ASM_ARGUMENT,
				                       NAMESPACE_ASM_ARGUMENT, symbol, &pos);

			rem_anchor_token(T_STRING_LITERAL);
			argument->asm_argument.constraints = parse_string_literals(context);
			rem_anchor_token('(');
			expect('(');
			argument->asm_argument.expression = parse_expression();
			rem_anchor_token(')');
			expect(')');

			semantic_asm_argument(&argument->asm_argument, is_out);

			*anchor = argument;
			anchor  = &argument->base.next;
		} while (accept(','));
		rem_anchor_token(',');
	}
	return result;
}

/**
 * Parse a asm statement clobber specification.
 */
static void parse_asm_clobbers(asm_clobber_t **anchor)
{
	if (peek(T_STRING_LITERAL)) {
		add_anchor_token(',');
		do {
			asm_clobber_t   *clobber = allocate_ast_zero(sizeof(clobber[0]));
			position_t const pos     = *HERE;
			clobber->clobber         = parse_string_literals(NULL);

			const char *const clobber_string = clobber->clobber->begin;
			if (!be_is_valid_clobber(clobber_string))
				errorf(&pos, "invalid register '%s' specified in clobbers",
				       clobber_string);

			*anchor = clobber;
			anchor  = &clobber->next;
		} while (accept(','));
		rem_anchor_token(',');
	}
}

static void parse_asm_labels(asm_label_t **anchor)
{
	if (peek(T_IDENTIFIER)) {
		add_anchor_token(',');
		do {
			label_t *const label = get_label("'asm goto' labels");
			if (label) {
				asm_label_t *const asm_label = allocate_ast_zero(sizeof(*asm_label));
				asm_label->label = label;

				*anchor = asm_label;
				anchor  = &asm_label->next;
			}
		} while (accept(','));
		rem_anchor_token(',');
	}
}

static void normalize_asm_text(asm_statement_t *asm_statement)
{
	if (!asm_statement->has_arguments) {
		/* escape % signs */
		begin_string_construction();
		for (const char *c = asm_statement->asm_text->begin; *c != '\0'; ++c) {
			if (*c == '%')
				obstack_1grow(&string_obst, '%');
			obstack_1grow(&string_obst, *c);
		}
		asm_statement->normalized_text
			= finish_string_construction(asm_statement->asm_text->encoding);
		return;
	}

	/* normalize asm text if necessary */
	unsigned pos                = 0;
	bool     need_normalization = false;
	for (entity_t *output = asm_statement->outputs; output != NULL;
	     output = output->base.next) {
		output->asm_argument.pos = pos++;
		symbol_t *symbol = output->base.symbol;
		if (symbol != NULL) {
			need_normalization = true;
			entity_t *old = set_entity(output);
			if (old != NULL) {
				errorf(&output->base.pos, "multiple declarations of '%N'",
				       output);
				note_prev_decl(old);
			}
		}
	}
	for (entity_t *input = asm_statement->inputs; input != NULL;
	     input = input->base.next) {
		input->asm_argument.pos = pos++;
		symbol_t *symbol = input->base.symbol;
		if (symbol != NULL) {
			need_normalization = true;
			entity_t *old = set_entity(input);
			if (old != NULL) {
				errorf(&input->base.pos, "multiple declarations of '%N'",
				       input);
				note_prev_decl(old);
			}
		}
	}
	if (!need_normalization) {
		asm_statement->normalized_text = asm_statement->asm_text;
		return;
	}

	begin_string_construction();
	for (const char *c = asm_statement->asm_text->begin; *c != '\0'; ++c) {
		if (*c == '%') {
			/* scan forward to see if we can find a [] */
			const char *b = c+1;
			while (*b != '\0' && *b != '%' && *b != '[' && !is_digit(*b)) {
				++b;
			}
			if (*b == '[') {
				/* parse identifier */
				assert(obstack_object_size(&symbol_obstack) == 0);
				const char *e = b+1;
				while (*e != '\0' && *e != ']') {
					obstack_1grow(&symbol_obstack, *e);
					++e;
				}
				char *const identifier = obstack_nul_finish(&symbol_obstack);
				if (*e == ']') {
					symbol_t *symbol   = symbol_table_insert(identifier);
					entity_t *argument = get_entity(symbol, NAMESPACE_ASM_ARGUMENT);
					if (argument == NULL) {
						position_t errorpos = asm_statement->textpos;
						errorpos.colno += b + 1 - asm_statement->asm_text->begin;
						errorf(&errorpos, "undefined assembler operand '%Y'",
						       symbol);
					} else {
						for ( ; c < b; ++c) {
							obstack_1grow(&string_obst, *c);
						}
						obstack_printf(&string_obst, "%u",
						               argument->asm_argument.pos);
						c = e;
						continue;
					}
				}
			}
		}
		obstack_1grow(&string_obst, *c);
	}
	asm_statement->normalized_text
		= finish_string_construction(asm_statement->asm_text->encoding);

	for (entity_t *input = asm_statement->inputs; input != NULL;
	     input = input->base.next) {
		symbol_t *symbol = input->base.symbol;
		if (symbol != NULL)
			reset_symbol(symbol, NAMESPACE_ASM_ARGUMENT);
	}
	for (entity_t *output = asm_statement->outputs; output != NULL;
	     output = output->base.next) {
		symbol_t *symbol = output->base.symbol;
		if (symbol != NULL)
			reset_symbol(symbol, NAMESPACE_ASM_ARGUMENT);
	}
}

/**
 * Parse a GCC-style asm statement.
 */
static statement_t *parse_gcc_asm_statement(void)
{
	statement_t     *statement     = allocate_statement_zero(STATEMENT_ASM);
	asm_statement_t *asm_statement = &statement->asms;

	eat(T_asm);
	add_anchor_token(')');
	add_anchor_token(':');
	add_anchor_token(T_STRING_LITERAL);

	if (accept(T_volatile))
		asm_statement->is_volatile = true;

	bool const asm_goto = accept(T_goto);

	expect('(');
	rem_anchor_token(T_STRING_LITERAL);
	asm_statement->textpos  = *HERE;
	asm_statement->asm_text = parse_string_literals("asm statement");

	if (accept(':')) {
		asm_statement->has_arguments = true;
		asm_statement->outputs = parse_asm_arguments(true);
	}
	if (accept(':'))
		asm_statement->inputs = parse_asm_arguments(false);
	if (accept(':'))
		parse_asm_clobbers( &asm_statement->clobbers);

	rem_anchor_token(':');
	if (accept(':')) {
		if (!asm_goto)
			warningf(WARN_OTHER, &statement->base.pos,
			         "assembler statement with labels should be 'asm goto'");
		parse_asm_labels(&asm_statement->labels);
		if (asm_statement->labels)
			errorf(&statement->base.pos, "'asm goto' not supported");
	} else if (asm_goto)
		warningf(WARN_OTHER, &statement->base.pos,
		         "'asm goto' without labels");

	rem_anchor_token(')');
	expect(')');
	expect(';');

	/* GCC: An 'asm' instruction without any output operands will be treated
	 * identically to a volatile 'asm' instruction. */
	if (asm_statement->outputs == NULL)
		asm_statement->is_volatile = true;

	normalize_asm_text(asm_statement);

	return statement;
}

/** Parse an MSC-style asm statement. */
static statement_t *parse_ms_asm_statement(void)
{
	statement_t *const stmt = allocate_statement_zero(STATEMENT_ASM);
	begin_string_construction();
	stmt->asms.asm_text = finish_string_construction(STRING_ENCODING_CHAR);

	errorf(HERE, "'asm { ... }' not supported");

	eat(T_asm);
	eat('{');

	/* Skip body. */
	while (!peek('}') && !peek(T_EOF)) {
		next_token();
	}
	expect('}');
	accept(';'); // Optional ';' at the end

	return stmt;
}

static statement_t *parse_asm_statement(void)
{
	if (peek_ahead('{')) {
		return parse_ms_asm_statement();
	} else {
		return parse_gcc_asm_statement();
	}
}

static statement_t *parse_label_inner_statement(statement_t const *const label,
                                                char const *const label_kind)
{
	statement_t *inner_stmt;
	switch (token.kind) {
	case '}':
		errorf(&label->base.pos, "%s at end of compound statement", label_kind);
		inner_stmt = create_error_statement();
		break;

	case ';':
		if (label->kind == STATEMENT_LABEL) {
			/* Eat an empty statement here, to avoid the warning about an empty
			 * statement after a label.  label:; is commonly used to have a
			 * label before a closing brace. */
			inner_stmt = create_empty_statement();
			eat(';');
			break;
		}
		/* FALLTHROUGH */

	default:
		inner_stmt = parse_statement();
		/* ISO/IEC  9899:1999(E) §6.8:1/6.8.2:1  Declarations are no statements */
		/* ISO/IEC 14882:1998(E) §6:1/§6.7       Declarations are statements */
		if (inner_stmt->kind == STATEMENT_DECLARATION && !dialect.cpp)
			errorf(&inner_stmt->base.pos, "declaration after %s", label_kind);
		break;
	}
	return inner_stmt;
}

/**
 * Parse a case statement.
 */
static statement_t *parse_case_statement(void)
{
	statement_t *const statement = allocate_statement_zero(STATEMENT_CASE_LABEL);
	position_t  *const pos       = &statement->base.pos;

	eat(T_case);
	add_anchor_token(':');
	add_anchor_token(T_DOTDOTDOT);

	expression_t *expression = parse_integer_constant_expression("case label");
	type_t       *type       = expression->base.type;
	if (type == type_error_type) {
		statement->case_label.is_bad = true;
	} else {
		if (current_switch != NULL) {
			type_t *switch_type = current_switch->expression->base.type;
			if (is_type_valid(skip_typeref(switch_type))) {
				type       = switch_type;
				expression = create_implicit_cast(expression, switch_type);
			}
		}

		ir_tarval *val = fold_expression(expression);
		statement->case_label.first_case = val;
		statement->case_label.last_case  = val;
	}
	statement->case_label.expression = expression;

	rem_anchor_token(T_DOTDOTDOT);
	if (accept(T_DOTDOTDOT)) {
		if (!GNU_MODE)
			errorf(pos, "case ranges are a GCC extension");
		expression_t *end_range
			= parse_integer_constant_expression("case range end");
		if (end_range->base.type == type_error_type) {
			statement->case_label.is_bad = true;
		} else {
			end_range = create_implicit_cast(end_range, type);
			ir_tarval *val = fold_expression(end_range);
			statement->case_label.last_case = val;
			if (tarval_cmp(val, statement->case_label.first_case)
					== ir_relation_less) {
				statement->case_label.is_empty_range = true;
				warningf(WARN_OTHER, pos, "empty range specified");
			}
		}
		statement->case_label.end_range = end_range;
	}

	PUSH_PARENT(statement);

	rem_anchor_token(':');
	expect(':');

	if (current_switch != NULL) {
		if (!statement->case_label.is_bad) {
			/* Check for duplicate case values */
			case_label_statement_t *c = &statement->case_label;
			for (case_label_statement_t *l = current_switch->first_case;
			     l != NULL; l = l->next) {
				if (l->is_bad || l->is_empty_range || l->expression == NULL)
					continue;

				if (tarval_cmp(c->last_case, l->first_case) == ir_relation_less
				 || tarval_cmp(l->last_case, c->first_case) == ir_relation_less)
					continue;

				errorf(pos, "duplicate case value");
				notef(&l->base.pos, "previous case label was here");
				break;
			}
		}
		/* link all cases into the switch statement */
		if (current_switch->last_case == NULL) {
			current_switch->first_case      = &statement->case_label;
		} else {
			current_switch->last_case->next = &statement->case_label;
		}
		current_switch->last_case = &statement->case_label;
	} else {
		errorf(pos, "case label not within a switch statement");
	}

	statement->case_label.statement
		= parse_label_inner_statement(statement, "case label");

	POP_PARENT();
	return statement;
}

/**
 * Parse a default statement.
 */
static statement_t *parse_default_statement(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_CASE_LABEL);

	eat(T_default);

	PUSH_PARENT(statement);

	expect(':');

	if (current_switch != NULL) {
		const case_label_statement_t *def_label = current_switch->default_label;
		if (def_label != NULL) {
			errorf(&statement->base.pos,
			       "multiple default labels in one switch");
			notef(&def_label->base.pos, "previous default label was here");
		} else {
			current_switch->default_label = &statement->case_label;

			/* link all cases into the switch statement */
			if (current_switch->last_case == NULL) {
				current_switch->first_case      = &statement->case_label;
			} else {
				current_switch->last_case->next = &statement->case_label;
			}
			current_switch->last_case = &statement->case_label;
		}
	} else {
		errorf(&statement->base.pos,
		       "'default' label not within a switch statement");
	}

	statement->case_label.statement
		= parse_label_inner_statement(statement, "default label");

	POP_PARENT();
	return statement;
}

/**
 * Parse a label statement.
 */
static statement_t *parse_label_statement(void)
{
	statement_t *const statement = allocate_statement_zero(STATEMENT_LABEL);
	label_t     *const label     = get_label(NULL /* Cannot fail, token is T_IDENTIFIER. */);
	statement->label.label = label;

	PUSH_PARENT(statement);

	/* if statement is already set then the label is defined twice,
	 * otherwise it was just mentioned in a goto/local label declaration so far
	 */
	position_t const* const pos = &statement->base.pos;
	if (label->statement != NULL) {
		errorf(pos, "duplicate '%N'", (entity_t const*)label);
		note_prev_decl((entity_t const*)label);
	} else {
		label->base.pos  = *pos;
		label->statement = statement;
		label->n_users  += 1;
	}

	eat(':');

	if (peek(T___attribute__) && !dialect.cpp)
		parse_attributes(NULL); // TODO process attributes

	statement->label.statement = parse_label_inner_statement(statement, "label");

	POP_PARENT();
	return statement;
}

static statement_t *parse_inner_statement(void)
{
	statement_t *const stmt = parse_statement();
	/* ISO/IEC  9899:1999(E) §6.8:1/6.8.2:1  Declarations are no statements */
	/* ISO/IEC 14882:1998(E) §6:1/§6.7       Declarations are statements */
	if (stmt->kind == STATEMENT_DECLARATION && !dialect.cpp) {
		errorf(&stmt->base.pos, "declaration as inner statement, use {}");
	}
	return stmt;
}

/**
 * Parse an expression in parentheses and mark its variables as read.
 */
static expression_t *parse_condition(void)
{
	add_anchor_token(')');
	expect('(');
	expression_t *const expr = parse_expression();
	mark_vars_read(expr, NULL);
	rem_anchor_token(')');
	expect(')');
	return expr;
}

static void warn_empty_body(statement_t const *const stmt)
{
	if (stmt->kind == STATEMENT_EMPTY)
		warningf(WARN_EMPTY_BODY, &stmt->base.pos,
		         "suggest braces around empty body in ‘if’ statement");
}

/**
 * Parse an if statement.
 */
static statement_t *parse_if(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_IF);

	eat(T_if);

	PUSH_PARENT(statement);
	PUSH_SCOPE_STATEMENT(&statement->ifs.scope);

	add_anchor_token(T_else);

	expression_t *const expr = parse_condition();
	statement->ifs.condition = expr;
	/* §6.8.4.1:1  The controlling expression of an if statement shall have
	 *             scalar type. */
	semantic_condition(expr, "condition of 'if'-statment");

	statement_t *const true_stmt = parse_inner_statement();
	statement->ifs.true_statement = true_stmt;
	warn_empty_body(true_stmt);

	rem_anchor_token(T_else);
	if (accept(T_else)) {
		statement_t *const false_stmt = parse_inner_statement();
		statement->ifs.false_statement = false_stmt;
		warn_empty_body(false_stmt);
	} else if (true_stmt->kind == STATEMENT_IF
	        && true_stmt->ifs.false_statement != NULL) {
		position_t const *const pos = &true_stmt->base.pos;
		warningf(WARN_PARENTHESES, pos,
		         "suggest explicit braces to avoid ambiguous 'else'");
	}

	POP_SCOPE();
	POP_PARENT();
	return statement;
}

/**
 * Check that all enums are handled in a switch.
 *
 * @param statement  the switch statement to check
 */
static void check_enum_cases(const switch_statement_t *statement)
{
	if (!is_warn_on(WARN_SWITCH_ENUM))
		return;
	type_t *type = skip_typeref(statement->expression->base.type);
	if (!is_type_enum(type))
		return;
	enum_type_t *enumt = &type->enumt;

	/* if we have a default, no warnings */
	if (statement->default_label != NULL)
		return;

	/* FIXME: calculation of value should be done while parsing */
	/* TODO: quadratic algorithm here. Change to an n log n one */
	for (const entity_t *entry = enumt->enume->first_value;
		 entry != NULL && entry->kind == ENTITY_ENUM_VALUE;
	     entry = entry->base.next) {
		ir_mode   *mode  = get_ir_mode_arithmetic(type);
		ir_tarval *value = get_enum_value(&entry->enum_value);
		value = tarval_convert_to(value, mode);
		bool       found = false;
		for (const case_label_statement_t *l = statement->first_case; l != NULL;
		     l = l->next) {
			if (l->expression == NULL)
				continue;
			if (l->first_case == l->last_case && l->first_case != value)
				continue;
			if ((tarval_cmp(l->first_case, value) & ir_relation_less_equal)
			 && (tarval_cmp(value, l->last_case) & ir_relation_less_equal)) {
				found = true;
				break;
			}
		}
		if (!found) {
			position_t const *const pos = &statement->base.pos;
			warningf(WARN_SWITCH_ENUM, pos,
			         "'%N' not handled in switch", entry);
		}
	}
}

/**
 * Parse a switch statement.
 */
static statement_t *parse_switch(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_SWITCH);

	eat(T_switch);

	PUSH_PARENT(statement);
	PUSH_SCOPE_STATEMENT(&statement->switchs.scope);

	expression_t *const expr = parse_condition();
	type_t       *      type = skip_typeref(expr->base.type);
	if (is_type_integer(type)) {
		type = promote_integer(type);
		if (get_akind_rank(get_arithmetic_akind(type)) >= get_akind_rank(ATOMIC_TYPE_LONG)) {
			warningf(WARN_TRADITIONAL, &expr->base.pos,
			         "'%T' switch expression not converted to '%T' in ISO C",
			         type, type_int);
		}
	} else if (is_type_valid(type)) {
		errorf(&expr->base.pos, "switch quantity is not an integer, but '%T'",
		       type);
		type = type_error_type;
	}
	statement->switchs.expression = create_implicit_cast(expr, type);

	switch_statement_t *rem = current_switch;
	current_switch          = &statement->switchs;
	statement->switchs.body = parse_inner_statement();
	current_switch          = rem;

	if (statement->switchs.default_label == NULL)
		warningf(WARN_SWITCH_DEFAULT, &statement->base.pos,
		         "switch has no default case");
	check_enum_cases(&statement->switchs);

	POP_SCOPE();
	POP_PARENT();
	return statement;
}

static statement_t *parse_loop_body(statement_t *const loop)
{
	statement_t *const rem = current_loop;
	current_loop = loop;

	statement_t *const body = parse_inner_statement();

	current_loop = rem;
	return body;
}

/**
 * Parse a while statement.
 */
static statement_t *parse_while(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_FOR);

	eat(T_while);

	PUSH_PARENT(statement);
	PUSH_SCOPE_STATEMENT(&statement->fors.scope);

	expression_t *const cond = parse_condition();
	statement->fors.condition = cond;
	/* §6.8.5:2    The controlling expression of an iteration statement shall
	 *             have scalar type. */
	semantic_condition(cond, "condition of 'while'-statement");

	statement->fors.body = parse_loop_body(statement);

	POP_SCOPE();
	POP_PARENT();
	return statement;
}

/**
 * Parse a do statement.
 */
static statement_t *parse_do(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_DO_WHILE);

	eat(T_do);

	PUSH_PARENT(statement);
	PUSH_SCOPE_STATEMENT(&statement->do_while.scope);

	add_anchor_token(T_while);
	statement->do_while.body = parse_loop_body(statement);
	rem_anchor_token(T_while);

	expect(T_while);
	expression_t *const cond = parse_condition();
	statement->do_while.condition = cond;
	/* §6.8.5:2    The controlling expression of an iteration statement shall
	 *             have scalar type. */
	semantic_condition(cond, "condition of 'do-while'-statement");
	expect(';');

	POP_SCOPE();
	POP_PARENT();
	return statement;
}

/**
 * Parse a for statement.
 */
static statement_t *parse_for(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_FOR);

	eat(T_for);

	PUSH_PARENT(statement);
	PUSH_SCOPE_STATEMENT(&statement->fors.scope);

	add_anchor_token(')');
	expect('(');

	PUSH_EXTENSION();

	if (accept(';')) {
	} else if (starts_declaration(&token)) {
		parse_declaration(record_entity, DECL_FLAGS_NONE);
	} else {
		add_anchor_token(';');
		expression_t *const init = parse_expression();
		statement->fors.initialisation = init;
		mark_vars_read(init, ENT_ANY);
		if (!expression_has_effect(init)) {
			warningf(WARN_UNUSED_VALUE, &init->base.pos,
			         "initialisation of 'for'-statement has no effect");
		}
		rem_anchor_token(';');
		expect(';');
	}

	POP_EXTENSION();

	if (!peek(';')) {
		add_anchor_token(';');
		expression_t *const cond = parse_expression();
		statement->fors.condition = cond;
		/* §6.8.5:2    The controlling expression of an iteration statement
		 *             shall have scalar type. */
		semantic_condition(cond, "condition of 'for'-statement");
		mark_vars_read(cond, NULL);
		rem_anchor_token(';');
	}
	expect(';');
	if (!peek(')')) {
		expression_t *const step = parse_expression();
		statement->fors.step = step;
		mark_vars_read(step, ENT_ANY);
		if (!expression_has_effect(step))
			warningf(WARN_UNUSED_VALUE, &step->base.pos,
			         "step of 'for'-statement has no effect");
	}
	rem_anchor_token(')');
	expect(')');
	statement->fors.body = parse_loop_body(statement);

	POP_SCOPE();
	POP_PARENT();
	return statement;
}

/**
 * Parse a goto statement.
 */
static statement_t *parse_goto(void)
{
	statement_t *statement;
	if (peek_ahead('*')) {
		if (!GNU_MODE)
			errorf(HERE, "computed goto is a GCC extension");
		statement = allocate_statement_zero(STATEMENT_COMPUTED_GOTO);
		eat(T_goto);
		eat('*');

		expression_t *expression = parse_expression();
		mark_vars_read(expression, NULL);

		/* Argh: although documentation says the expression must be of type
		 * void*, gcc accepts anything that can be casted into void* without
		 * error */
		type_t *type = expression->base.type;

		if (type != type_error_type) {
			if (!is_type_pointer(type) && !is_type_integer(type)) {
				errorf(&expression->base.pos,
				       "cannot convert to a pointer type");
			} else if (type != type_void_ptr) {
				warningf(WARN_OTHER, &expression->base.pos,
				         "type of computed goto expression should be 'void*' not '%T'",
				         type);
			}
			expression = create_implicit_cast(expression, type_void_ptr);
		}

		statement->computed_goto.expression = expression;
	} else {
		statement = allocate_statement_zero(STATEMENT_GOTO);
		eat(T_goto);

		label_t *const label = get_label("goto");
		if (label) {
			label->n_users        += 1;
			label->used            = true;
			statement->gotos.label = label;
		} else {
			statement->gotos.label = &allocate_entity_zero(ENTITY_LABEL, NAMESPACE_LABEL, sym_anonymous, &builtin_position)->label;
		}
	}

	expect(';');
	return statement;
}

/**
 * Parse a continue statement.
 */
static statement_t *parse_continue(void)
{
	if (current_loop == NULL)
		errorf(HERE, "continue statement not within loop");

	statement_t *statement = allocate_statement_zero(STATEMENT_CONTINUE);

	eat(T_continue);
	expect(';');
	return statement;
}

/**
 * Parse a break statement.
 */
static statement_t *parse_break(void)
{
	if (current_switch == NULL && current_loop == NULL)
		errorf(HERE, "break statement not within loop or switch");

	statement_t *statement = allocate_statement_zero(STATEMENT_BREAK);

	eat(T_break);
	expect(';');
	return statement;
}

/**
 * Check if a given entity represents a local variable.
 */
static bool is_local_variable(const entity_t *entity)
{
	if (entity->kind != ENTITY_VARIABLE)
		return false;

	switch (entity->declaration.storage_class) {
	case STORAGE_CLASS_AUTO:
	case STORAGE_CLASS_REGISTER: {
		const type_t *type = skip_typeref(entity->declaration.type);
		if (is_type_function(type)) {
			return false;
		} else {
			return true;
		}
	}
	default:
		return false;
	}
}

/**
 * Check if a given expression represents a local variable.
 */
static bool expression_is_local_variable(const expression_t *expression)
{
	if (expression->base.kind != EXPR_REFERENCE)
		return false;
	const entity_t *entity = expression->reference.entity;
	return is_local_variable(entity);
}

/**
 * Parse a return statement.
 */
static statement_t *parse_return(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_RETURN);
	eat(T_return);

	expression_t *return_value = NULL;
	if (!peek(';')) {
		return_value = parse_expression();
		mark_vars_read(return_value, NULL);
	}

	const type_t *const func_type = skip_typeref(current_function->base.type);
	assert(is_type_function(func_type));
	type_t *const return_type = skip_typeref(func_type->function.return_type);

	position_t const *const pos = &statement->base.pos;
	if (return_value != NULL) {

		if (is_type_void(return_type)) {
			type_t *return_value_type = skip_typeref(return_value->base.type);
			if (is_type_void(return_value_type)) {
				warningf(WARN_PEDANTIC, pos,
				         "void function should not return void expression");
			} else {
				/* ISO/IEC 14882:1998(E) §6.6.3:2 */
				warningf(WARN_RETURN_TYPE, pos,
				         "'return' with a value, in function returning 'void'");
			}
		} else {
			assign_error_t error = semantic_assign(return_type, return_value);
			report_assign_error(error, return_type, return_value, "'return'",
			                    pos);
		}
		return_value = create_implicit_cast(return_value, return_type);
		/* check for returning address of a local var */
		if (return_value != NULL
		 && return_value->base.kind == EXPR_UNARY_TAKE_ADDRESS) {
			const expression_t *expression = return_value->unary.value;
			if (expression_is_local_variable(expression))
				warningf(WARN_OTHER, pos,
				         "function returns address of local variable");
		}
	} else if (!is_type_void(return_type)) {
		/* ISO/IEC 14882:1998(E) §6.6.3:3 */
		warningf(WARN_RETURN_TYPE, pos,
		         "'return' without value, in function returning non-void");
	}
	statement->returns.value = return_value;

	expect(';');
	return statement;
}

/**
 * Parse a declaration statement.
 */
static statement_t *parse_declaration_statement(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_DECLARATION);

	entity_t *before = current_scope->last_entity;
	parse_external_declaration();

	declaration_statement_t *const decl  = &statement->declaration;
	entity_t                *const begin
		= before != NULL ? before->base.next : current_scope->first_entity;
	decl->declarations_begin = begin;
	decl->declarations_end   = begin != NULL ? current_scope->last_entity
	                                         : NULL;

	return statement;
}

/**
 * Parse an expression statement, i.e. expr ';'.
 */
static statement_t *parse_expression_statement(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_EXPRESSION);

	expression_t *const expr         = parse_expression();
	statement->expression.expression = expr;
	mark_vars_read(expr, ENT_ANY);

	expect(';');
	return statement;
}

static statement_t *parse_empty_statement(void)
{
	warningf(WARN_EMPTY_STATEMENT, HERE, "statement is empty");
	statement_t *const statement = create_empty_statement();
	eat(';');
	return statement;
}

static statement_t *parse_local_label_declaration(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_DECLARATION);

	eat(T___label__);

	entity_t *begin   = NULL;
	entity_t *end     = NULL;
	entity_t **anchor = &begin;
	add_anchor_token(';');
	add_anchor_token(',');
	do {
		position_t pos;
		symbol_t *const symbol
			= expect_identifier("local label declaration", &pos);
		if (symbol != sym_anonymous) {
			entity_t *entity = get_entity(symbol, NAMESPACE_LABEL);
			if (entity != NULL && entity->base.parent_scope == current_scope) {
				errorf(&pos, "multiple definitions of '%N'", entity);
				note_prev_decl(entity);
			} else {
				entity = allocate_entity_zero(ENTITY_LOCAL_LABEL, NAMESPACE_LABEL, symbol, &pos);
				entity->base.parent_scope = current_scope;

				*anchor = entity;
				anchor  = &entity->base.next;
				end     = entity;

				environment_push(entity);
			}
		}
	} while (accept(','));
	rem_anchor_token(',');
	rem_anchor_token(';');
	expect(';');
	statement->declaration.declarations_begin = begin;
	statement->declaration.declarations_end   = end;
	return statement;
}

static void parse_namespace_definition(void)
{
	eat(T_namespace);

	entity_t *entity = NULL;
	symbol_t *symbol = NULL;

	if (peek(T_IDENTIFIER)) {
		symbol = token.base.symbol;
		entity = get_entity(symbol, NAMESPACE_NORMAL);
		if (entity && entity->kind != ENTITY_NAMESPACE) {
			if (entity->base.parent_scope == current_scope
			 && is_entity_valid(entity)) {
				error_redefined_as_different_kind(HERE, entity, ENTITY_NAMESPACE);
			}
			entity = NULL;
		}
		eat(T_IDENTIFIER);
	}

	if (entity == NULL) {
		entity = allocate_entity_zero(ENTITY_NAMESPACE, NAMESPACE_NORMAL, symbol, HERE);
		entity->base.parent_scope = current_scope;
	}

	if (peek('='))
		/* TODO: parse namespace alias */
		panic("namespace alias definition not supported yet");

	environment_push(entity);
	append_entity(current_scope, entity);

	PUSH_SCOPE(&entity->namespacee.members);
	PUSH_CURRENT_ENTITY(entity);

	add_anchor_token('}');
	expect('{');
	parse_externals();
	rem_anchor_token('}');
	expect('}');

	POP_CURRENT_ENTITY();
	POP_SCOPE();
}

/**
 * Parse a statement.
 * There's also parse_statement() which additionally checks for
 * "statement has no effect" warnings
 */
static statement_t *intern_parse_statement(void)
{
	/* declaration or statement */
	statement_t *statement;
	switch (token.kind) {
	case T_IDENTIFIER: {
		token_kind_t const la1_type = look_ahead(1)->kind;
		if (la1_type == ':') {
			statement = parse_label_statement();
		} else if (is_typedef_symbol(token.base.symbol)) {
	case DECLARATION_START:
			statement = parse_declaration_statement();
		} else {
			/* it's an identifier, the grammar says this must be an
			 * expression statement. However it is common that users mistype
			 * declaration types, so we guess a bit here to improve robustness
			 * for incorrect programs */
			switch (la1_type) {
			case '&':
			case '*':
				if (get_entity(token.base.symbol, NAMESPACE_NORMAL) != NULL) {
			default:
					statement = parse_expression_statement();
				} else {
			case DECLARATION_START:
			case T_IDENTIFIER:
					statement = parse_declaration_statement();
				}
				break;
			}
		}
		break;
	}

	case T___extension__: {
		/* This can be a prefix to a declaration or an expression statement.
		 * We simply eat it now and parse the rest with tail recursion. */
		PUSH_EXTENSION();
		statement = intern_parse_statement();
		POP_EXTENSION();
		break;
	}

	case T___label__:
		statement = parse_local_label_declaration();
		break;

	case ';':         statement = parse_empty_statement();         break;
	case '{':         statement = parse_compound_statement(false); break;
	case T_asm:       statement = parse_asm_statement();           break;
	case T_break:     statement = parse_break();                   break;
	case T_case:      statement = parse_case_statement();          break;
	case T_continue:  statement = parse_continue();                break;
	case T_default:   statement = parse_default_statement();       break;
	case T_do:        statement = parse_do();                      break;
	case T_for:       statement = parse_for();                     break;
	case T_goto:      statement = parse_goto();                    break;
	case T_if:        statement = parse_if();                      break;
	case T_return:    statement = parse_return();                  break;
	case T_switch:    statement = parse_switch();                  break;
	case T_while:     statement = parse_while();                   break;

	case EXPRESSION_START:
		statement = parse_expression_statement();
		break;

	default:
		errorf(HERE, "unexpected token %K while parsing statement", &token);
		statement = create_error_statement();
		eat_until_anchor();
		break;
	}

	return statement;
}

/**
 * parse a statement and emits "statement has no effect" warning if needed
 * (This is really a wrapper around intern_parse_statement with check for 1
 *  single warning. It is needed, because for statement expressions we have
 *  to avoid the warning on the last statement)
 */
static statement_t *parse_statement(void)
{
	statement_t *statement = intern_parse_statement();

	if (statement->kind == STATEMENT_EXPRESSION) {
		expression_t *expression = statement->expression.expression;
		if (!expression_has_effect(expression))
			warningf(WARN_UNUSED_VALUE, &expression->base.pos,
			         "statement has no effect");
	}

	return statement;
}

/**
 * Parse a compound statement.
 */
static statement_t *parse_compound_statement(bool inside_expression_statement)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_COMPOUND);

	PUSH_PARENT(statement);
	PUSH_SCOPE(&statement->compound.scope);

	eat('{');
	add_anchor_token('}');
	/* tokens, which can start a statement */
	/* TODO MS, __builtin_FOO */
	add_anchor_token('!');
	add_anchor_token('&');
	add_anchor_token('(');
	add_anchor_token('*');
	add_anchor_token('+');
	add_anchor_token('-');
	add_anchor_token(';');
	add_anchor_token('{');
	add_anchor_token('~');
	add_anchor_token(T_CHARACTER_CONSTANT);
	add_anchor_token(T_COLONCOLON);
	add_anchor_token(T_IDENTIFIER);
	add_anchor_token(T_MINUSMINUS);
	add_anchor_token(T_NUMBER);
	add_anchor_token(T_PLUSPLUS);
	add_anchor_token(T_STRING_LITERAL);
	add_anchor_token(T__Alignof);
	add_anchor_token(T__Bool);
	add_anchor_token(T__Complex);
	add_anchor_token(T__Imaginary);
	add_anchor_token(T__Thread_local);
	add_anchor_token(T___PRETTY_FUNCTION__);
	add_anchor_token(T___attribute__);
	add_anchor_token(T___builtin_va_start);
	add_anchor_token(T___extension__);
	add_anchor_token(T___func__);
	add_anchor_token(T___imag__);
	add_anchor_token(T___label__);
	add_anchor_token(T___real__);
	add_anchor_token(T_asm);
	add_anchor_token(T_auto);
	add_anchor_token(T_bool);
	add_anchor_token(T_break);
	add_anchor_token(T_case);
	add_anchor_token(T_char);
	add_anchor_token(T_class);
	add_anchor_token(T_const);
	add_anchor_token(T_const_cast);
	add_anchor_token(T_continue);
	add_anchor_token(T_default);
	add_anchor_token(T_delete);
	add_anchor_token(T_double);
	add_anchor_token(T_do);
	add_anchor_token(T_dynamic_cast);
	add_anchor_token(T_enum);
	add_anchor_token(T_extern);
	add_anchor_token(T_false);
	add_anchor_token(T_float);
	add_anchor_token(T_for);
	add_anchor_token(T_goto);
	add_anchor_token(T_if);
	add_anchor_token(T_inline);
	add_anchor_token(T_int);
	add_anchor_token(T_long);
	add_anchor_token(T_new);
	add_anchor_token(T_operator);
	add_anchor_token(T_register);
	add_anchor_token(T_reinterpret_cast);
	add_anchor_token(T_restrict);
	add_anchor_token(T_return);
	add_anchor_token(T_short);
	add_anchor_token(T_signed);
	add_anchor_token(T_sizeof);
	add_anchor_token(T_static);
	add_anchor_token(T_static_cast);
	add_anchor_token(T_struct);
	add_anchor_token(T_switch);
	add_anchor_token(T_template);
	add_anchor_token(T_this);
	add_anchor_token(T_throw);
	add_anchor_token(T_true);
	add_anchor_token(T_try);
	add_anchor_token(T_typedef);
	add_anchor_token(T_typeid);
	add_anchor_token(T_typename);
	add_anchor_token(T_typeof);
	add_anchor_token(T_union);
	add_anchor_token(T_unsigned);
	add_anchor_token(T_using);
	add_anchor_token(T_void);
	add_anchor_token(T_volatile);
	add_anchor_token(T_wchar_t);
	add_anchor_token(T_while);

	statement_t **anchor            = &statement->compound.statements;
	bool          only_decls_so_far = true;
	while (!peek('}') && !peek(T_EOF)) {
		statement_t *sub_statement = intern_parse_statement();
		if (sub_statement->kind == STATEMENT_ERROR)
			break;

		if (sub_statement->kind != STATEMENT_DECLARATION) {
			only_decls_so_far = false;
		} else if (!only_decls_so_far) {
			position_t const *const pos = &sub_statement->base.pos;
			warningf(WARN_DECLARATION_AFTER_STATEMENT, pos,
			         "ISO C90 forbids mixed declarations and code");
		}

		*anchor = sub_statement;
		anchor  = &sub_statement->base.next;
	}
	expect('}');

	/* look over all statements again to produce no effect warnings */
	if (is_warn_on(WARN_UNUSED_VALUE)) {
		for (statement_t *sub_statement = statement->compound.statements;
			 sub_statement != NULL; sub_statement = sub_statement->base.next) {
			if (sub_statement->kind != STATEMENT_EXPRESSION)
				continue;
			/* don't emit a warning for the last expression in an expression
			 * statement as it has always an effect */
			if (inside_expression_statement && sub_statement->base.next == NULL)
				continue;

			expression_t *expression = sub_statement->expression.expression;
			if (!expression_has_effect(expression))
				warningf(WARN_UNUSED_VALUE, &expression->base.pos,
				         "statement has no effect");
		}
	}

	rem_anchor_token(T_while);
	rem_anchor_token(T_wchar_t);
	rem_anchor_token(T_volatile);
	rem_anchor_token(T_void);
	rem_anchor_token(T_using);
	rem_anchor_token(T_unsigned);
	rem_anchor_token(T_union);
	rem_anchor_token(T_typeof);
	rem_anchor_token(T_typename);
	rem_anchor_token(T_typeid);
	rem_anchor_token(T_typedef);
	rem_anchor_token(T_try);
	rem_anchor_token(T_true);
	rem_anchor_token(T_throw);
	rem_anchor_token(T_this);
	rem_anchor_token(T_template);
	rem_anchor_token(T_switch);
	rem_anchor_token(T_struct);
	rem_anchor_token(T_static_cast);
	rem_anchor_token(T_static);
	rem_anchor_token(T_sizeof);
	rem_anchor_token(T_signed);
	rem_anchor_token(T_short);
	rem_anchor_token(T_return);
	rem_anchor_token(T_restrict);
	rem_anchor_token(T_reinterpret_cast);
	rem_anchor_token(T_register);
	rem_anchor_token(T_operator);
	rem_anchor_token(T_new);
	rem_anchor_token(T_long);
	rem_anchor_token(T_int);
	rem_anchor_token(T_inline);
	rem_anchor_token(T_if);
	rem_anchor_token(T_goto);
	rem_anchor_token(T_for);
	rem_anchor_token(T_float);
	rem_anchor_token(T_false);
	rem_anchor_token(T_extern);
	rem_anchor_token(T_enum);
	rem_anchor_token(T_dynamic_cast);
	rem_anchor_token(T_do);
	rem_anchor_token(T_double);
	rem_anchor_token(T_delete);
	rem_anchor_token(T_default);
	rem_anchor_token(T_continue);
	rem_anchor_token(T_const_cast);
	rem_anchor_token(T_const);
	rem_anchor_token(T_class);
	rem_anchor_token(T_char);
	rem_anchor_token(T_case);
	rem_anchor_token(T_break);
	rem_anchor_token(T_bool);
	rem_anchor_token(T_auto);
	rem_anchor_token(T_asm);
	rem_anchor_token(T___real__);
	rem_anchor_token(T___label__);
	rem_anchor_token(T___imag__);
	rem_anchor_token(T___func__);
	rem_anchor_token(T___extension__);
	rem_anchor_token(T___builtin_va_start);
	rem_anchor_token(T___attribute__);
	rem_anchor_token(T___PRETTY_FUNCTION__);
	rem_anchor_token(T__Thread_local);
	rem_anchor_token(T__Imaginary);
	rem_anchor_token(T__Complex);
	rem_anchor_token(T__Bool);
	rem_anchor_token(T__Alignof);
	rem_anchor_token(T_STRING_LITERAL);
	rem_anchor_token(T_PLUSPLUS);
	rem_anchor_token(T_NUMBER);
	rem_anchor_token(T_MINUSMINUS);
	rem_anchor_token(T_IDENTIFIER);
	rem_anchor_token(T_COLONCOLON);
	rem_anchor_token(T_CHARACTER_CONSTANT);
	rem_anchor_token('~');
	rem_anchor_token('{');
	rem_anchor_token(';');
	rem_anchor_token('-');
	rem_anchor_token('+');
	rem_anchor_token('*');
	rem_anchor_token('(');
	rem_anchor_token('&');
	rem_anchor_token('!');
	rem_anchor_token('}');

	POP_SCOPE();
	POP_PARENT();
	return statement;
}

static entity_t *determine_lhs_ent(expression_t *const expr,
                                   entity_t *lhs_ent)
{
	switch (expr->kind) {
	case EXPR_REFERENCE: {
		entity_t *const entity = expr->reference.entity;
		/* we should only find variables as lvalues... */
		if (entity->base.kind != ENTITY_VARIABLE
		 && entity->base.kind != ENTITY_PARAMETER)
			return NULL;

		return entity;
	}

	case EXPR_ARRAY_ACCESS: {
		expression_t *const ref = expr->array_access.array_ref;
		entity_t     *      ent = NULL;
		if (is_type_array(skip_typeref(revert_automatic_type_conversion(ref)))) {
			ent     = determine_lhs_ent(ref, lhs_ent);
			lhs_ent = ent;
		} else {
			mark_vars_read(ref, lhs_ent);
		}
		mark_vars_read(expr->array_access.index, lhs_ent);
		return ent;
	}

	case EXPR_SELECT: {
		mark_vars_read(expr->select.compound, lhs_ent);
		if (is_type_compound(skip_typeref(expr->base.type)))
			return determine_lhs_ent(expr->select.compound, lhs_ent);
		return NULL;
	}

	case EXPR_UNARY_DEREFERENCE: {
		expression_t *const val = expr->unary.value;
		if (val->kind == EXPR_UNARY_TAKE_ADDRESS) {
			/* *&x is a NOP */
			return determine_lhs_ent(val->unary.value, lhs_ent);
		} else {
			mark_vars_read(val, NULL);
			return NULL;
		}
	}

	default:
		mark_vars_read(expr, NULL);
		return NULL;
	}
}

/**
 * Mark declarations, which are read.  This is used to detect variables, which
 * are never read.
 * Example:
 * x = x + 1;
 *   x is not marked as "read", because it is only read to calculate its own new
 *   value.
 *
 * x += y; y += x;
 *   x and y are not detected as "not read", because multiple variables are
 *   involved.
 */
static void mark_vars_read(expression_t *const expr, entity_t *lhs_ent)
{
	switch (expr->kind) {
	case EXPR_REFERENCE: {
		entity_t *const entity = expr->reference.entity;
		if (entity->kind != ENTITY_VARIABLE && entity->kind != ENTITY_PARAMETER)
			return;

		if (lhs_ent != entity && lhs_ent != ENT_ANY)
			entity->variable.read = true;
		return;
	}

	case EXPR_CALL:
		// TODO respect pure/const
		mark_vars_read(expr->call.function, NULL);
		for (call_argument_t *arg = expr->call.arguments; arg != NULL;
		     arg = arg->next) {
			mark_vars_read(arg->expression, NULL);
		}
		return;

	case EXPR_CONDITIONAL:
		// TODO lhs_decl should depend on whether true/false have an effect
		mark_vars_read(expr->conditional.condition, NULL);
		if (expr->conditional.true_expression != NULL)
			mark_vars_read(expr->conditional.true_expression, lhs_ent);
		mark_vars_read(expr->conditional.false_expression, lhs_ent);
		return;

	case EXPR_SELECT:
		if (lhs_ent == ENT_ANY
		 && !is_type_compound(skip_typeref(expr->base.type)))
			lhs_ent = NULL;
		mark_vars_read(expr->select.compound, lhs_ent);
		return;

	case EXPR_ARRAY_ACCESS: {
		mark_vars_read(expr->array_access.index,
		               lhs_ent == ENT_ANY ? NULL : lhs_ent);
		expression_t *const ref = expr->array_access.array_ref;
		if (!is_type_array(skip_typeref(revert_automatic_type_conversion(ref)))
		 && lhs_ent == ENT_ANY)
			lhs_ent = NULL;
		mark_vars_read(ref, lhs_ent);
		return;
	}

	case EXPR_VA_ARG:
		mark_vars_read(expr->va_arge.ap, lhs_ent);
		return;

	case EXPR_VA_COPY:
		if (lhs_ent == ENT_ANY)
			lhs_ent = NULL;
		determine_lhs_ent(expr->va_copye.dst, lhs_ent);
		mark_vars_read(expr->va_copye.src, lhs_ent);
		return;

	case EXPR_UNARY_CAST:
		/* Special case: Use void cast to mark a variable as "read" */
		if (is_type_void(skip_typeref(expr->base.type)))
			lhs_ent = NULL;
		goto unary;

	case EXPR_UNARY_THROW:
		if (expr->unary.value == NULL)
			return;
		/* FALLTHROUGH */
	case EXPR_UNARY_DEREFERENCE:
	case EXPR_UNARY_DELETE:
	case EXPR_UNARY_DELETE_ARRAY:
		if (lhs_ent == ENT_ANY)
			lhs_ent = NULL;
		goto unary;

	case EXPR_UNARY_NEGATE:
	case EXPR_UNARY_PLUS:
	case EXPR_UNARY_COMPLEMENT:
	case EXPR_UNARY_NOT:
	case EXPR_UNARY_TAKE_ADDRESS:
	case EXPR_UNARY_POSTFIX_INCREMENT:
	case EXPR_UNARY_POSTFIX_DECREMENT:
	case EXPR_UNARY_PREFIX_INCREMENT:
	case EXPR_UNARY_PREFIX_DECREMENT:
	case EXPR_UNARY_ASSUME:
	case EXPR_UNARY_IMAG:
	case EXPR_UNARY_REAL:
unary:
		mark_vars_read(expr->unary.value, lhs_ent);
		return;

	case EXPR_BINARY_ADD:
	case EXPR_BINARY_SUB:
	case EXPR_BINARY_MUL:
	case EXPR_BINARY_DIV:
	case EXPR_BINARY_MOD:
	case EXPR_BINARY_EQUAL:
	case EXPR_BINARY_NOTEQUAL:
	case EXPR_BINARY_LESS:
	case EXPR_BINARY_LESSEQUAL:
	case EXPR_BINARY_GREATER:
	case EXPR_BINARY_GREATEREQUAL:
	case EXPR_BINARY_BITWISE_AND:
	case EXPR_BINARY_BITWISE_OR:
	case EXPR_BINARY_BITWISE_XOR:
	case EXPR_BINARY_LOGICAL_AND:
	case EXPR_BINARY_LOGICAL_OR:
	case EXPR_BINARY_SHIFTLEFT:
	case EXPR_BINARY_SHIFTRIGHT:
	case EXPR_BINARY_ISGREATER:
	case EXPR_BINARY_ISGREATEREQUAL:
	case EXPR_BINARY_ISLESS:
	case EXPR_BINARY_ISLESSEQUAL:
	case EXPR_BINARY_ISLESSGREATER:
	case EXPR_BINARY_ISUNORDERED:
binary:
		mark_vars_read(expr->binary.left,  lhs_ent);
		mark_vars_read(expr->binary.right, lhs_ent);
		return;

	case EXPR_BINARY_COMMA:
		mark_vars_read(expr->binary.left, NULL);
		mark_vars_read(expr->binary.right, lhs_ent);
		return;

	case EXPR_BINARY_ASSIGN:
	case EXPR_BINARY_MUL_ASSIGN:
	case EXPR_BINARY_DIV_ASSIGN:
	case EXPR_BINARY_MOD_ASSIGN:
	case EXPR_BINARY_ADD_ASSIGN:
	case EXPR_BINARY_SUB_ASSIGN:
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:
	case EXPR_BINARY_BITWISE_AND_ASSIGN:
	case EXPR_BINARY_BITWISE_XOR_ASSIGN:
	case EXPR_BINARY_BITWISE_OR_ASSIGN: {
		if (lhs_ent == NULL)
			goto binary;
		if (lhs_ent == ENT_ANY)
			lhs_ent = NULL;
		lhs_ent = determine_lhs_ent(expr->binary.left, lhs_ent);
		mark_vars_read(expr->binary.right, lhs_ent);
		return;
	}

	case EXPR_VA_START:
		determine_lhs_ent(expr->va_starte.ap, lhs_ent);
		return;

	case EXPR_STATEMENT: {
		expression_t *last = get_statement_expression_last(&expr->statement);
		if (last != NULL)
			mark_vars_read(last, lhs_ent);
		return;
	}

	case EXPR_LITERAL_CASES:
	case EXPR_LITERAL_CHARACTER:
	case EXPR_ERROR:
	case EXPR_STRING_LITERAL:
	case EXPR_COMPOUND_LITERAL: // TODO init?
	case EXPR_SIZEOF:
	case EXPR_CLASSIFY_TYPE:
	case EXPR_ALIGNOF:
	case EXPR_FUNCNAME:
	case EXPR_BUILTIN_CONSTANT_P:
	case EXPR_BUILTIN_TYPES_COMPATIBLE_P:
	case EXPR_OFFSETOF:
	case EXPR_LABEL_ADDRESS:
	case EXPR_ENUM_CONSTANT:
		return;
	}

	panic("unhandled expression");
}

/**
 * Check for unused global static functions and variables
 */
static void check_unused_globals(void)
{
	if (!is_warn_on(WARN_UNUSED_FUNCTION) && !is_warn_on(WARN_UNUSED_VARIABLE))
		return;

	for (entity_t const *entity = file_scope->first_entity; entity != NULL;
	     entity = entity->base.next) {
		if (!is_declaration(entity) || entity->kind == ENTITY_TYPEDEF)
			continue;

		const declaration_t *declaration = &entity->declaration;
		if (declaration->used
		 || declaration->modifiers & DM_UNUSED
		 || declaration->modifiers & DM_USED
		 || declaration->storage_class != STORAGE_CLASS_STATIC)
			continue;

		warning_t   why;
		char const *s;
		if (entity->kind == ENTITY_FUNCTION) {
			/* inhibit warning for static inline functions */
			if (entity->function.is_inline)
				continue;

			why = WARN_UNUSED_FUNCTION;
			s   = entity->function.body != NULL ? "defined" : "declared";
		} else {
			why = WARN_UNUSED_VARIABLE;
			s   = "defined";
		}

		warningf(why, &declaration->base.pos, "'%#N' %s but not used",
		         entity, s);
	}
}

static void parse_global_asm(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_ASM);

	eat(T_asm);
	add_anchor_token(';');
	add_anchor_token(')');
	add_anchor_token(T_STRING_LITERAL);
	expect('(');

	rem_anchor_token(T_STRING_LITERAL);
	statement->asms.asm_text = parse_string_literals("global asm");
	statement->base.next     = unit->global_asm;
	unit->global_asm         = statement;

	rem_anchor_token(')');
	expect(')');
	rem_anchor_token(';');
	expect(';');
}

static void parse_linkage_specification(void)
{
	eat(T_extern);

	position_t  const pos     = *HERE;
	char const *const linkage = parse_string_literals(NULL)->begin;

	linkage_kind_t old_linkage = current_linkage;
	linkage_kind_t new_linkage;
	if (streq(linkage, "C")) {
		new_linkage = LINKAGE_C;
	} else if (streq(linkage, "C++")) {
		new_linkage = LINKAGE_CXX;
	} else {
		errorf(&pos, "linkage string \"%s\" not recognized", linkage);
		new_linkage = LINKAGE_C;
	}
	current_linkage = new_linkage;

	if (accept('{')) {
		parse_externals();
		expect('}');
	} else {
		parse_external();
	}

	assert(current_linkage == new_linkage);
	current_linkage = old_linkage;
}

static void parse_external(void)
{
	switch (token.kind) {
	case T_extern:
		if (peek_ahead(T_STRING_LITERAL)) {
			parse_linkage_specification();
		} else {
	case DECLARATION_START_NO_EXTERN:
	case T_IDENTIFIER:
	case T___extension__:
	/* tokens below are for implicit int */
	case '&':  /* & x; -> int& x; (and error later, because C++ has no
				  implicit int) */
	case '*':  /* * x; -> int* x; */
	case '(':  /* (x); -> int (x); */
			PUSH_EXTENSION();
			parse_external_declaration();
			POP_EXTENSION();
		}
		return;

	case T_asm:
		parse_global_asm();
		return;

	case T_namespace:
		parse_namespace_definition();
		return;

	case ';':
		if (!dialect.strict) {
			warningf(WARN_STRAY_SEMICOLON, HERE, "stray %k outside of function", T_SEMICOLON);
			eat(';');
			return;
		}
		/* FALLTHROUGH */

	default:
		errorf(HERE, "stray %K outside of function", &token);
		if (peek('(') || peek('{') || peek('['))
			eat_until_matching_token(token.kind);
		next_token();
		return;
	}
}

static void parse_externals(void)
{
	add_anchor_token('}');
	add_anchor_token(T_EOF);

#ifndef NDEBUG
	/* make a copy of the anchor set, so we can check if it is restored after
	 * parsing */
	unsigned short token_anchor_copy[T_LAST_TOKEN];
	memcpy(token_anchor_copy, token_anchor_set, sizeof(token_anchor_copy));
#endif

	while (!peek(T_EOF) && !peek('}')) {
#ifndef NDEBUG
		for (int i = 0; i < T_LAST_TOKEN; ++i) {
			unsigned short count = token_anchor_set[i] - token_anchor_copy[i];
			if (count != 0)
				/* the anchor set and its copy differs */
				panic("Leaked anchor token");
		}
		if (in_gcc_extension)
			/* an gcc extension scope was not closed */
			panic("Leaked __extension__");
#endif

		parse_external();
	}

	rem_anchor_token(T_EOF);
	rem_anchor_token('}');
}

/**
 * Parse a translation unit.
 */
static void parse_translation_unit(void)
{
	add_anchor_token(T_EOF);

	while (true) {
		parse_externals();

		if (peek(T_EOF))
			break;

		errorf(HERE, "stray %K outside of function", &token);
		if (peek('(') || peek('{') || peek('['))
			eat_until_matching_token(token.kind);
		next_token();
	}
}

static void resolve_aliases(void)
{
	for (size_t i = 0, n_aliases = ARR_LEN(alias_entities);
	     i < n_aliases; ++i) {
		entity_t *entity = alias_entities[i];
		assert(entity->kind == ENTITY_VARIABLE
		    || entity->kind == ENTITY_FUNCTION);
		symbol_t *symbol = entity->kind == ENTITY_VARIABLE
		                 ? entity->variable.alias.symbol
		                 : entity->function.alias.symbol;
		/* resolve alias if possible and mark the aliased entity as used */
		entity_t *def = get_entity(symbol, NAMESPACE_NORMAL);
		if (def == NULL) {
			errorf(&entity->base.pos, "'%N' aliased to unknown entity '%Y'",
			       entity, symbol);
		} else {
			def->declaration.used = true;
		}
		if (entity->kind == ENTITY_VARIABLE)
			entity->variable.alias.entity = def;
		else
			entity->function.alias.entity = def;
	}
}

void set_default_visibility(elf_visibility_t visibility)
{
	default_visibility = visibility;
}

void start_parsing(void)
{
	environment_stack = NEW_ARR_F(stack_entry_t, 0);
	label_stack       = NEW_ARR_F(stack_entry_t, 0);
	alias_entities    = NEW_ARR_F(entity_t*, 0);

	print_to_file(stderr);

	assert(unit == NULL);
	unit = allocate_ast_zero(sizeof(unit[0]));

	assert(file_scope == NULL);
	file_scope = &unit->scope;

	assert(current_scope == NULL);
	scope_push(&unit->scope);

	create_gnu_builtins();
	if (dialect.ms)
		create_microsoft_intrinsics();

	symbol_main = symbol_table_insert("main");
}

translation_unit_t *finish_parsing(void)
{
	find_known_libc_functions();

	resolve_aliases();
	DEL_ARR_F(alias_entities);
	alias_entities = NULL;

	assert(current_scope == &unit->scope);
	scope_pop(NULL);

	assert(file_scope == &unit->scope);
	check_unused_globals();
	file_scope = NULL;

	DEL_ARR_F(environment_stack);
	DEL_ARR_F(label_stack);

	translation_unit_t *result = unit;
	unit = NULL;
	return result;
}

/* §6.9.2:2 and §6.9.2:5: At the end of the translation incomplete arrays
 * are given length one. */
static void complete_incomplete_arrays(void)
{
	size_t n = ARR_LEN(incomplete_arrays);
	for (size_t i = 0; i != n; ++i) {
		declaration_t *const decl = incomplete_arrays[i];
		type_t        *const type = skip_typeref(decl->type);

		if (is_type_complete(type))
			continue;

		position_t const *const pos = &decl->base.pos;
		warningf(WARN_OTHER, pos, "array '%#N' assumed to have one element",
		         (entity_t const*)decl);

		type_t *const new_type = duplicate_type(type);
		new_type->array.size_constant     = true;
		new_type->array.has_implicit_size = true;
		new_type->array.size              = 1;

		type_t *const result = identify_new_type(new_type);

		decl->type = result;
	}
}

static void prepare_main_collect2(entity_t *const entity)
{
	PUSH_SCOPE(&entity->function.body->compound.scope);

	// create call to __main
	symbol_t *symbol         = symbol_table_insert("__main");
	entity_t *subsubmain_ent
		= create_implicit_function(symbol, &builtin_position);

	expression_t *ref     = allocate_expression_zero(EXPR_REFERENCE);
	type_t       *ftype   = subsubmain_ent->declaration.type;
	ref->base.pos         = builtin_position;
	ref->base.type        = make_pointer_type(ftype, TYPE_QUALIFIER_NONE);
	ref->reference.entity = subsubmain_ent;

	expression_t *call  = allocate_expression_zero(EXPR_CALL);
	call->base.pos      = builtin_position;
	call->base.type     = type_void;
	call->call.function = ref;

	statement_t *expr_statement = allocate_statement_zero(STATEMENT_EXPRESSION);
	expr_statement->base.pos              = builtin_position;
	expr_statement->expression.expression = call;

	statement_t *const body = entity->function.body;
	assert(body->kind == STATEMENT_COMPOUND);
	compound_statement_t *compounds = &body->compound;

	expr_statement->base.next = compounds->statements;
	compounds->statements     = expr_statement;

	POP_SCOPE();
}

void parse(void)
{
	lookahead_bufpos = 0;
	for (int i = 0; i < MAX_LOOKAHEAD + 2; ++i) {
		next_token();
	}
	current_linkage   = dialect.cpp ? LINKAGE_CXX : LINKAGE_C;
	incomplete_arrays = NEW_ARR_F(declaration_t*, 0);
	parse_translation_unit();
	complete_incomplete_arrays();
	DEL_ARR_F(incomplete_arrays);
	incomplete_arrays = NULL;
}

void init_parser(void)
{
	memset(token_anchor_set, 0, sizeof(token_anchor_set));

	init_expression_parsers();
	obstack_init(&temp_obst);

	string_true  = make_string("true");
	string_false = make_string("false");
}

void exit_parser(void)
{
	obstack_free(&temp_obst, NULL);
}
