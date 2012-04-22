/*
 * This file is part of cparser.
 * Copyright (C) 2007-2009 Matthias Braun <matze@braunis.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
#include <config.h>

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>

#include "adt/strutil.h"
#include "parser.h"
#include "diagnostic.h"
#include "format_check.h"
#include "lexer.h"
#include "symbol_t.h"
#include "token_t.h"
#include "types.h"
#include "type_t.h"
#include "type_hash.h"
#include "ast_t.h"
#include "entity_t.h"
#include "attribute_t.h"
#include "lang_features.h"
#include "walk.h"
#include "warning.h"
#include "printer.h"
#include "adt/bitfiddle.h"
#include "adt/error.h"
#include "adt/array.h"

//#define PRINT_TOKENS
#define MAX_LOOKAHEAD 1

typedef struct {
	entity_t           *old_entity;
	symbol_t           *symbol;
	entity_namespace_t  namespc;
} stack_entry_t;

typedef struct declaration_specifiers_t  declaration_specifiers_t;
struct declaration_specifiers_t {
	source_position_t  source_position;
	storage_class_t    storage_class;
	unsigned char      alignment;         /**< Alignment, 0 if not set. */
	bool               is_inline    : 1;
	bool               thread_local : 1;  /**< GCC __thread */
	attribute_t       *attributes;        /**< list of attributes */
	type_t            *type;
};

/**
 * An environment for parsing initializers (and compound literals).
 */
typedef struct parse_initializer_env_t {
	type_t     *type;   /**< the type of the initializer. In case of an
	                         array type with unspecified size this gets
	                         adjusted to the actual size. */
	entity_t   *entity; /**< the variable that is initialized if any */
	bool        must_be_constant;
} parse_initializer_env_t;

typedef entity_t* (*parsed_declaration_func) (entity_t *declaration, bool is_definition);

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
static ms_try_statement_t  *current_try       = NULL;
static linkage_kind_t       current_linkage;
static goto_statement_t    *goto_first        = NULL;
static goto_statement_t   **goto_anchor       = NULL;
static label_statement_t   *label_first       = NULL;
static label_statement_t  **label_anchor      = NULL;
/** current translation unit. */
static translation_unit_t  *unit              = NULL;
/** true if we are in an __extension__ context. */
static bool                 in_gcc_extension  = false;
static struct obstack       temp_obst;
static entity_t            *anonymous_entity;
static declaration_t      **incomplete_arrays;
static elf_visibility_tag_t default_visibility = ELF_VISIBILITY_DEFAULT;


#define PUSH_PARENT(stmt) \
	statement_t *const new_parent = (stmt); \
	statement_t *const old_parent = current_parent; \
	((void)(current_parent = new_parent))
#define POP_PARENT() (assert(current_parent == new_parent), (void)(current_parent = old_parent))

#define PUSH_SCOPE(scope) \
	size_t   const top       = environment_top(); \
	scope_t *const new_scope = (scope); \
	scope_t *const old_scope = scope_push(new_scope)
#define POP_SCOPE() (assert(current_scope == new_scope), scope_pop(old_scope), environment_pop_to(top))

#define PUSH_EXTENSION() \
	(void)0; \
	bool const old_gcc_extension = in_gcc_extension; \
	while (next_if(T___extension__)) { \
		in_gcc_extension = true; \
	} \
	do {} while (0)
#define POP_EXTENSION() \
	((void)(in_gcc_extension = old_gcc_extension))

/** special symbol used for anonymous entities. */
static symbol_t *sym_anonymous = NULL;

/** The token anchor set */
static unsigned short token_anchor_set[T_LAST_TOKEN];

/** The current source position. */
#define HERE (&token.base.source_position)

/** true if we are in GCC mode. */
#define GNU_MODE ((c_mode & _GNUC) || in_gcc_extension)

static statement_t *parse_compound_statement(bool inside_expression_statement);
static statement_t *parse_statement(void);

static expression_t *parse_subexpression(precedence_t);
static expression_t *parse_expression(void);
static type_t       *parse_typename(void);
static void          parse_externals(void);
static void          parse_external(void);

static void parse_compound_type_entries(compound_t *compound_declaration);

static void check_call_argument(type_t          *expected_type,
								call_argument_t *argument, unsigned pos);

typedef enum declarator_flags_t {
	DECL_FLAGS_NONE             = 0,
	DECL_MAY_BE_ABSTRACT        = 1U << 0,
	DECL_CREATE_COMPOUND_MEMBER = 1U << 1,
	DECL_IS_PARAMETER           = 1U << 2
} declarator_flags_t;

static entity_t *parse_declarator(const declaration_specifiers_t *specifiers,
                                  declarator_flags_t flags);

static void semantic_comparison(binary_expression_t *expression);

#define STORAGE_CLASSES       \
	STORAGE_CLASSES_NO_EXTERN \
	case T_extern:

#define STORAGE_CLASSES_NO_EXTERN \
	case T_typedef:         \
	case T_static:          \
	case T_auto:            \
	case T_register:        \
	case T___thread:

#define TYPE_QUALIFIERS     \
	case T_const:           \
	case T_restrict:        \
	case T_volatile:        \
	case T_inline:          \
	case T__forceinline:    \
	case T___attribute__:

#define COMPLEX_SPECIFIERS  \
	case T__Complex:
#define IMAGINARY_SPECIFIERS \
	case T__Imaginary:

#define TYPE_SPECIFIERS       \
	case T__Bool:             \
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
	case T__int128:           \
	COMPLEX_SPECIFIERS        \
	IMAGINARY_SPECIFIERS

#define DECLARATION_START   \
	STORAGE_CLASSES         \
	TYPE_QUALIFIERS         \
	TYPE_SPECIFIERS

#define DECLARATION_START_NO_EXTERN \
	STORAGE_CLASSES_NO_EXTERN       \
	TYPE_QUALIFIERS                 \
	TYPE_SPECIFIERS

#define EXPRESSION_START              \
	case '!':                         \
	case '&':                         \
	case '(':                         \
	case '*':                         \
	case '+':                         \
	case '-':                         \
	case '~':                         \
	case T_ANDAND:                    \
	case T_CHARACTER_CONSTANT:        \
	case T_FLOATINGPOINT:             \
	case T_FLOATINGPOINT_HEXADECIMAL: \
	case T_INTEGER:                   \
	case T_INTEGER_HEXADECIMAL:       \
	case T_INTEGER_OCTAL:             \
	case T_MINUSMINUS:                \
	case T_PLUSPLUS:                  \
	case T_STRING_LITERAL:            \
	case T_WIDE_CHARACTER_CONSTANT:   \
	case T_WIDE_STRING_LITERAL:       \
	case T___FUNCDNAME__:             \
	case T___FUNCSIG__:               \
	case T___FUNCTION__:              \
	case T___PRETTY_FUNCTION__:       \
	case T___alignof__:               \
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
	case T_true:

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
		[STATEMENT_WHILE]         = sizeof(while_statement_t),
		[STATEMENT_DO_WHILE]      = sizeof(do_while_statement_t),
		[STATEMENT_FOR]           = sizeof(for_statement_t),
		[STATEMENT_ASM]           = sizeof(asm_statement_t),
		[STATEMENT_MS_TRY]        = sizeof(ms_try_statement_t),
		[STATEMENT_LEAVE]         = sizeof(leave_statement_t)
	};
	assert((size_t)kind < lengthof(sizes));
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
		[EXPR_LITERAL_INTEGER_OCTAL]      = sizeof(literal_expression_t),
		[EXPR_LITERAL_INTEGER_HEXADECIMAL]= sizeof(literal_expression_t),
		[EXPR_LITERAL_FLOATINGPOINT]      = sizeof(literal_expression_t),
		[EXPR_LITERAL_FLOATINGPOINT_HEXADECIMAL] = sizeof(literal_expression_t),
		[EXPR_LITERAL_CHARACTER]          = sizeof(literal_expression_t),
		[EXPR_LITERAL_WIDE_CHARACTER]     = sizeof(literal_expression_t),
		[EXPR_STRING_LITERAL]             = sizeof(string_literal_expression_t),
		[EXPR_WIDE_STRING_LITERAL]        = sizeof(string_literal_expression_t),
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
	if (kind >= EXPR_UNARY_FIRST && kind <= EXPR_UNARY_LAST) {
		return sizes[EXPR_UNARY_FIRST];
	}
	if (kind >= EXPR_BINARY_FIRST && kind <= EXPR_BINARY_LAST) {
		return sizes[EXPR_BINARY_FIRST];
	}
	assert((size_t)kind < lengthof(sizes));
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

	res->base.kind            = kind;
	res->base.parent          = current_parent;
	res->base.source_position = token.base.source_position;
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

	res->base.kind            = kind;
	res->base.type            = type_error_type;
	res->base.source_position = token.base.source_position;
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
		[INITIALIZER_VALUE]       = sizeof(initializer_value_t),
		[INITIALIZER_STRING]      = sizeof(initializer_string_t),
		[INITIALIZER_WIDE_STRING] = sizeof(initializer_wide_string_t),
		[INITIALIZER_LIST]        = sizeof(initializer_list_t),
		[INITIALIZER_DESIGNATOR]  = sizeof(initializer_designator_t)
	};
	assert((size_t)kind < lengthof(sizes));
	assert(sizes[kind] != 0);
	return sizes[kind];
}

/**
 * Allocate an initializer node of given kind and initialize all
 * fields with zero.
 */
static initializer_t *allocate_initializer_zero(initializer_kind_t kind)
{
	initializer_t *result = allocate_ast_zero(get_initializer_size(kind));
	result->kind          = kind;

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
	lookahead_buffer[lookahead_bufpos] = lexer_token;
	lexer_next_token();

	lookahead_bufpos = (lookahead_bufpos + 1) % MAX_LOOKAHEAD;

#ifdef PRINT_TOKENS
	print_token(stderr, &token);
	fprintf(stderr, "\n");
#endif
}

static inline bool next_if(int const type)
{
	if (token.kind == type) {
		next_token();
		return true;
	} else {
		return false;
	}
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

/**
 * Adds a token type to the token type anchor set (a multi-set).
 */
static void add_anchor_token(int token_kind)
{
	assert(0 <= token_kind && token_kind < T_LAST_TOKEN);
	++token_anchor_set[token_kind];
}

/**
 * Set the number of tokens types of the given type
 * to zero and return the old count.
 */
static int save_and_reset_anchor_state(int token_kind)
{
	assert(0 <= token_kind && token_kind < T_LAST_TOKEN);
	int count = token_anchor_set[token_kind];
	token_anchor_set[token_kind] = 0;
	return count;
}

/**
 * Restore the number of token types to the given count.
 */
static void restore_anchor_state(int token_kind, int count)
{
	assert(0 <= token_kind && token_kind < T_LAST_TOKEN);
	token_anchor_set[token_kind] = count;
}

/**
 * Remove a token type from the token type anchor set (a multi-set).
 */
static void rem_anchor_token(int token_kind)
{
	assert(0 <= token_kind && token_kind < T_LAST_TOKEN);
	assert(token_anchor_set[token_kind] != 0);
	--token_anchor_set[token_kind];
}

/**
 * Return true if the token type of the current token is
 * in the anchor set.
 */
static bool at_anchor(void)
{
	if (token.kind < 0)
		return false;
	return token_anchor_set[token.kind];
}

/**
 * Eat tokens until a matching token type is found.
 */
static void eat_until_matching_token(int type)
{
	int end_token;
	switch (type) {
		case '(': end_token = ')';  break;
		case '{': end_token = '}';  break;
		case '[': end_token = ']';  break;
		default:  end_token = type; break;
	}

	unsigned parenthesis_count = 0;
	unsigned brace_count       = 0;
	unsigned bracket_count     = 0;
	while (token.kind        != end_token ||
	       parenthesis_count != 0         ||
	       brace_count       != 0         ||
	       bracket_count     != 0) {
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
			if (token.kind        == end_token &&
			    parenthesis_count == 0         &&
			    brace_count       == 0         &&
			    bracket_count     == 0)
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
		if (token.kind == '(' || token.kind == '{' || token.kind == '[')
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
	next_if('}');
}

#define eat(token_kind) (assert(token.kind == (token_kind)), next_token())

/**
 * Report a parse error because an expected token was not found.
 */
static
#if defined __GNUC__ && __GNUC__ >= 4
__attribute__((sentinel))
#endif
void parse_error_expected(const char *message, ...)
{
	if (message != NULL) {
		errorf(HERE, "%s", message);
	}
	va_list ap;
	va_start(ap, message);
	errorf(HERE, "got %K, expected %#k", &token, &ap, ", ");
	va_end(ap);
}

/**
 * Report an incompatible type.
 */
static void type_error_incompatible(const char *msg,
		const source_position_t *source_position, type_t *type1, type_t *type2)
{
	errorf(source_position, "%s, incompatible types: '%T' - '%T'",
	       msg, type1, type2);
}

/**
 * Expect the current token is the expected token.
 * If not, generate an error, eat the current statement,
 * and goto the error_label label.
 */
#define expect(expected, error_label)                     \
	do {                                                  \
		if (UNLIKELY(token.kind != (expected))) {         \
			parse_error_expected(NULL, (expected), NULL); \
			add_anchor_token(expected);                   \
			eat_until_anchor();                           \
			rem_anchor_token(expected);                   \
			if (token.kind != (expected))                 \
			  goto error_label;                           \
		}                                                 \
		next_token();                                     \
	} while (0)

/**
 * Push a given scope on the scope stack and make it the
 * current scope
 */
static scope_t *scope_push(scope_t *new_scope)
{
	if (current_scope != NULL) {
		new_scope->depth = current_scope->depth + 1;
	}

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

/**
 * Search an entity by its symbol in a given namespace.
 */
static entity_t *get_entity(const symbol_t *const symbol,
                            namespace_tag_t namespc)
{
	entity_t *entity = symbol->entity;
	for (; entity != NULL; entity = entity->base.symbol_next) {
		if ((namespace_tag_t)entity->base.namespc == namespc)
			return entity;
	}

	return NULL;
}

/* §6.2.3:1 24)  There is only one name space for tags even though three are
 * possible. */
static entity_t *get_tag(symbol_t const *const symbol,
                         entity_kind_tag_t const kind)
{
	entity_t *entity = get_entity(symbol, NAMESPACE_TAG);
	if (entity != NULL && (entity_kind_tag_t)entity->kind != kind) {
		errorf(HERE,
				"'%Y' defined as wrong kind of tag (previous definition %P)",
				symbol, &entity->base.source_position);
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

	/* remember old declaration */
	stack_entry_t entry;
	entry.symbol     = symbol;
	entry.old_entity = iter;
	entry.namespc    = namespc;
	ARR_APP1(stack_entry_t, *stack_ptr, entry);
}

/**
 * Push an entity on the environment stack.
 */
static void environment_push(entity_t *entity)
{
	assert(entity->base.source_position.input_name != NULL);
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
	size_t         i;

	assert(new_top <= top);
	if (new_top == top)
		return;

	for (i = top; i > new_top; --i) {
		stack_entry_t *entry = &stack[i - 1];

		entity_t           *old_entity = entry->old_entity;
		symbol_t           *symbol     = entry->symbol;
		entity_namespace_t  namespc    = entry->namespc;

		/* replace with old_entity/remove */
		entity_t **anchor;
		entity_t  *iter;
		for (anchor = &symbol->entity; ; anchor = &iter->base.symbol_next) {
			iter = *anchor;
			assert(iter != NULL);
			/* replace an entry? */
			if (iter->base.namespc == namespc)
				break;
		}

		/* restore definition from outer scopes (if there was one) */
		if (old_entity != NULL) {
			old_entity->base.symbol_next = iter->base.symbol_next;
			*anchor                      = old_entity;
		} else {
			/* remove entry from list */
			*anchor = iter->base.symbol_next;
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

static atomic_type_kind_t get_akind(const type_t *type)
{
	assert(type->kind == TYPE_ATOMIC || type->kind == TYPE_COMPLEX
	       || type->kind == TYPE_IMAGINARY || type->kind == TYPE_ENUM);
	return type->atomic.akind;
}

/**
 * §6.3.1.1:2  Do integer promotion for a given type.
 *
 * @param type  the type to promote
 * @return the promoted type
 */
static type_t *promote_integer(type_t *type)
{
	if (get_akind_rank(get_akind(type)) < get_akind_rank(ATOMIC_TYPE_INT))
		type = type_int;

	return type;
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
		if (types_compatible(type, type_void_ptr))
			expression = expression->unary.value;
	}

	type_t *const type = skip_typeref(expression->base.type);
	if (!is_type_integer(type))
		return false;
	switch (is_constant_expression(expression)) {
		case EXPR_CLASS_ERROR:    return true;
		case EXPR_CLASS_CONSTANT: return !fold_constant_to_bool(expression);
		default:                  return false;
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
	type_t *const source_type = expression->base.type;

	if (source_type == dest_type)
		return expression;

	expression_t *cast = allocate_expression_zero(EXPR_UNARY_CAST);
	cast->unary.value   = expression;
	cast->base.type     = dest_type;
	cast->base.implicit = true;

	return cast;
}

typedef enum assign_error_t {
	ASSIGN_SUCCESS,
	ASSIGN_ERROR_INCOMPATIBLE,
	ASSIGN_ERROR_POINTER_QUALIFIER_MISSING,
	ASSIGN_WARNING_POINTER_INCOMPATIBLE,
	ASSIGN_WARNING_POINTER_FROM_INT,
	ASSIGN_WARNING_INT_FROM_POINTER
} assign_error_t;

static void report_assign_error(assign_error_t error, type_t *orig_type_left, expression_t const *const right, char const *const context, source_position_t const *const pos)
{
	type_t *const orig_type_right = right->base.type;
	type_t *const type_left       = skip_typeref(orig_type_left);
	type_t *const type_right      = skip_typeref(orig_type_right);

	switch (error) {
	case ASSIGN_SUCCESS:
		return;
	case ASSIGN_ERROR_INCOMPATIBLE:
		errorf(pos, "destination type '%T' in %s is incompatible with type '%T'", orig_type_left, context, orig_type_right);
		return;

	case ASSIGN_ERROR_POINTER_QUALIFIER_MISSING: {
		type_t *points_to_left  = skip_typeref(type_left->pointer.points_to);
		type_t *points_to_right = skip_typeref(type_right->pointer.points_to);

		/* the left type has all qualifiers from the right type */
		unsigned missing_qualifiers = points_to_right->base.qualifiers & ~points_to_left->base.qualifiers;
		warningf(WARN_OTHER, pos, "destination type '%T' in %s from type '%T' lacks qualifiers '%Q' in pointer target type", orig_type_left, context, orig_type_right, missing_qualifiers);
		return;
	}

	case ASSIGN_WARNING_POINTER_INCOMPATIBLE:
		warningf(WARN_OTHER, pos, "destination type '%T' in %s is incompatible with '%E' of type '%T'", orig_type_left, context, right, orig_type_right);
		return;

	case ASSIGN_WARNING_POINTER_FROM_INT:
		warningf(WARN_OTHER, pos, "%s makes pointer '%T' from integer '%T' without a cast", context, orig_type_left, orig_type_right);
		return;

	case ASSIGN_WARNING_INT_FROM_POINTER:
		warningf(WARN_OTHER, pos, "%s makes integer '%T' from pointer '%T' without a cast", context, orig_type_left, orig_type_right);
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
			if (missing_qualifiers != 0) {
				res = ASSIGN_ERROR_POINTER_QUALIFIER_MISSING;
			}

			points_to_left  = get_unqualified_type(points_to_left);
			points_to_right = get_unqualified_type(points_to_right);

			if (is_type_void(points_to_left))
				return res;

			if (is_type_void(points_to_right)) {
				/* ISO/IEC 14882:1998(E) §C.1.2:6 */
				return c_mode & _CXX ? ASSIGN_ERROR_INCOMPATIBLE : res;
			}

			if (!types_compatible(points_to_left, points_to_right)) {
				return ASSIGN_WARNING_POINTER_INCOMPATIBLE;
			}

			return res;
		} else if (is_type_integer(type_right)) {
			return ASSIGN_WARNING_POINTER_FROM_INT;
		}
	} else if ((is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) ||
			(is_type_atomic(type_left, ATOMIC_TYPE_BOOL)
				&& is_type_pointer(type_right))) {
		return ASSIGN_SUCCESS;
	} else if (is_type_compound(type_left) && is_type_compound(type_right)) {
		type_t *const unqual_type_left  = get_unqualified_type(type_left);
		type_t *const unqual_type_right = get_unqualified_type(type_right);
		if (types_compatible(unqual_type_left, unqual_type_right)) {
			return ASSIGN_SUCCESS;
		}
	} else if (is_type_integer(type_left) && is_type_pointer(type_right)) {
		return ASSIGN_WARNING_INT_FROM_POINTER;
	}

	if (!is_type_valid(type_left) || !is_type_valid(type_right))
		return ASSIGN_SUCCESS;

	return ASSIGN_ERROR_INCOMPATIBLE;
}

static expression_t *parse_constant_expression(void)
{
	expression_t *result = parse_subexpression(PREC_CONDITIONAL);

	if (is_constant_expression(result) == EXPR_CLASS_VARIABLE) {
		errorf(&result->base.source_position,
		       "expression '%E' is not constant", result);
	}

	return result;
}

static expression_t *parse_assignment_expression(void)
{
	return parse_subexpression(PREC_ASSIGNMENT);
}

static void warn_string_concat(const source_position_t *pos)
{
	warningf(WARN_TRADITIONAL, pos, "traditional C rejects string constant concatenation");
}

static string_t parse_string_literals(void)
{
	assert(token.kind == T_STRING_LITERAL);
	string_t result = token.string.string;

	next_token();

	while (token.kind == T_STRING_LITERAL) {
		warn_string_concat(&token.base.source_position);
		result = concat_strings(&result, &token.string.string);
		next_token();
	}

	return result;
}

static attribute_t *allocate_attribute_zero(attribute_kind_t kind)
{
	attribute_t *attribute = allocate_ast_zero(sizeof(*attribute));
	attribute->kind            = kind;
	attribute->source_position = *HERE;
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
	if (token.kind != ')') do {
		attribute_argument_t *argument = allocate_ast_zero(sizeof(*argument));

		/* is it an identifier */
		if (token.kind == T_IDENTIFIER
				&& (look_ahead(1)->kind == ',' || look_ahead(1)->kind == ')')) {
			symbol_t *symbol   = token.identifier.symbol;
			argument->kind     = ATTRIBUTE_ARGUMENT_SYMBOL;
			argument->v.symbol = symbol;
			next_token();
		} else {
			/* must be an expression */
			expression_t *expression = parse_assignment_expression();

			argument->kind         = ATTRIBUTE_ARGUMENT_EXPRESSION;
			argument->v.expression = expression;
		}

		/* append argument */
		*anchor = argument;
		anchor  = &argument->next;
	} while (next_if(','));
	expect(')', end_error);

	return first;

end_error:
	/* TODO... */
	return first;
}

static attribute_t *parse_attribute_asm(void)
{
	attribute_t *attribute = allocate_attribute_zero(ATTRIBUTE_GNU_ASM);
	eat(T_asm);

	expect('(', end_error);
	attribute->a.arguments = parse_attribute_arguments();
	return attribute;

end_error:
	return NULL;
}

static symbol_t *get_symbol_from_token(void)
{
	switch(token.kind) {
	case T_IDENTIFIER:
		return token.identifier.symbol;
	case T_auto:
	case T_char:
	case T_double:
	case T_enum:
	case T_extern:
	case T_float:
	case T_int:
	case T_long:
	case T_register:
	case T_short:
	case T_static:
	case T_struct:
	case T_union:
	case T_unsigned:
	case T_void:
	case T_bool:
	case T__Bool:
	case T_class:
	case T_explicit:
	case T_export:
	case T_wchar_t:
	case T_const:
	case T_signed:
	case T___real__:
	case T___imag__:
	case T_restrict:
	case T_volatile:
	case T_inline:
		/* maybe we need more tokens ... add them on demand */
		return get_token_kind_symbol(token.kind);
	default:
		return NULL;
	}
}

static attribute_t *parse_attribute_gnu_single(void)
{
	/* parse "any-word" */
	symbol_t *symbol = get_symbol_from_token();
	if (symbol == NULL) {
		parse_error_expected("while parsing attribute((", T_IDENTIFIER, NULL);
		return NULL;
	}

	attribute_kind_t  kind;
	char const *const name = symbol->string;
	for (kind = ATTRIBUTE_GNU_FIRST;; ++kind) {
		if (kind > ATTRIBUTE_GNU_LAST) {
			warningf(WARN_ATTRIBUTE, HERE, "unknown attribute '%s' ignored", name);
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
	if (next_if('('))
		attribute->a.arguments = parse_attribute_arguments();

	return attribute;
}

static attribute_t *parse_attribute_gnu(void)
{
	attribute_t  *first  = NULL;
	attribute_t **anchor = &first;

	eat(T___attribute__);
	expect('(', end_error);
	expect('(', end_error);

	if (token.kind != ')') do {
		attribute_t *attribute = parse_attribute_gnu_single();
		if (attribute == NULL)
			goto end_error;

		*anchor = attribute;
		anchor  = &attribute->next;
	} while (next_if(','));
	expect(')', end_error);
	expect(')', end_error);

end_error:
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
			/* TODO record modifier */
			warningf(WARN_OTHER, HERE, "Ignoring declaration modifier %K", &token);
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

static void mark_vars_read(expression_t *expr, entity_t *lhs_ent);

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

#define ENT_ANY ((entity_t*)-1)

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
			if (entity->kind != ENTITY_VARIABLE
					&& entity->kind != ENTITY_PARAMETER)
				return;

			if (lhs_ent != entity && lhs_ent != ENT_ANY) {
				if (entity->kind == ENTITY_VARIABLE) {
					entity->variable.read = true;
				} else {
					entity->parameter.read = true;
				}
			}
			return;
		}

		case EXPR_CALL:
			// TODO respect pure/const
			mark_vars_read(expr->call.function, NULL);
			for (call_argument_t *arg = expr->call.arguments; arg != NULL; arg = arg->next) {
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
			mark_vars_read(expr->array_access.index, lhs_ent);
			expression_t *const ref = expr->array_access.array_ref;
			if (!is_type_array(skip_typeref(revert_automatic_type_conversion(ref)))) {
				if (lhs_ent == ENT_ANY)
					lhs_ent = NULL;
			}
			mark_vars_read(ref, lhs_ent);
			return;
		}

		case EXPR_VA_ARG:
			mark_vars_read(expr->va_arge.ap, lhs_ent);
			return;

		case EXPR_VA_COPY:
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
		case EXPR_UNARY_BITWISE_NEGATE:
		case EXPR_UNARY_NOT:
		case EXPR_UNARY_TAKE_ADDRESS:
		case EXPR_UNARY_POSTFIX_INCREMENT:
		case EXPR_UNARY_POSTFIX_DECREMENT:
		case EXPR_UNARY_PREFIX_INCREMENT:
		case EXPR_UNARY_PREFIX_DECREMENT:
		case EXPR_UNARY_ASSUME:
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
		case EXPR_BINARY_COMMA:
		case EXPR_BINARY_ISGREATER:
		case EXPR_BINARY_ISGREATEREQUAL:
		case EXPR_BINARY_ISLESS:
		case EXPR_BINARY_ISLESSEQUAL:
		case EXPR_BINARY_ISLESSGREATER:
		case EXPR_BINARY_ISUNORDERED:
			mark_vars_read(expr->binary.left,  lhs_ent);
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
			if (lhs_ent == ENT_ANY)
				lhs_ent = NULL;
			lhs_ent = determine_lhs_ent(expr->binary.left, lhs_ent);
			mark_vars_read(expr->binary.right, lhs_ent);
			return;
		}

		case EXPR_VA_START:
			determine_lhs_ent(expr->va_starte.ap, lhs_ent);
			return;

		case EXPR_LITERAL_CASES:
		case EXPR_ERROR:
		case EXPR_STRING_LITERAL:
		case EXPR_WIDE_STRING_LITERAL:
		case EXPR_COMPOUND_LITERAL: // TODO init?
		case EXPR_SIZEOF:
		case EXPR_CLASSIFY_TYPE:
		case EXPR_ALIGNOF:
		case EXPR_FUNCNAME:
		case EXPR_BUILTIN_CONSTANT_P:
		case EXPR_BUILTIN_TYPES_COMPATIBLE_P:
		case EXPR_OFFSETOF:
		case EXPR_STATEMENT: // TODO
		case EXPR_LABEL_ADDRESS:
		case EXPR_ENUM_CONSTANT:
			return;
	}

	panic("unhandled expression");
}

static designator_t *parse_designation(void)
{
	designator_t  *result = NULL;
	designator_t **anchor = &result;

	for (;;) {
		designator_t *designator;
		switch (token.kind) {
		case '[':
			designator = allocate_ast_zero(sizeof(designator[0]));
			designator->source_position = token.base.source_position;
			next_token();
			add_anchor_token(']');
			designator->array_index = parse_constant_expression();
			rem_anchor_token(']');
			expect(']', end_error);
			break;
		case '.':
			designator = allocate_ast_zero(sizeof(designator[0]));
			designator->source_position = token.base.source_position;
			next_token();
			if (token.kind != T_IDENTIFIER) {
				parse_error_expected("while parsing designator",
				                     T_IDENTIFIER, NULL);
				return NULL;
			}
			designator->symbol = token.identifier.symbol;
			next_token();
			break;
		default:
			expect('=', end_error);
			return result;
		}

		assert(designator != NULL);
		*anchor = designator;
		anchor  = &designator->next;
	}
end_error:
	return NULL;
}

static initializer_t *initializer_from_string(array_type_t *const type,
                                              const string_t *const string)
{
	/* TODO: check len vs. size of array type */
	(void) type;

	initializer_t *initializer = allocate_initializer_zero(INITIALIZER_STRING);
	initializer->string.string = *string;

	return initializer;
}

static initializer_t *initializer_from_wide_string(array_type_t *const type,
                                                   const string_t *const string)
{
	/* TODO: check len vs. size of array type */
	(void) type;

	initializer_t *const initializer =
		allocate_initializer_zero(INITIALIZER_WIDE_STRING);
	initializer->wide_string.string = *string;

	return initializer;
}

/**
 * Build an initializer from a given expression.
 */
static initializer_t *initializer_from_expression(type_t *orig_type,
                                                  expression_t *expression)
{
	/* TODO check that expression is a constant expression */

	/* §6.7.8.14/15 char array may be initialized by string literals */
	type_t *type           = skip_typeref(orig_type);
	type_t *expr_type_orig = expression->base.type;
	type_t *expr_type      = skip_typeref(expr_type_orig);

	if (is_type_array(type) && expr_type->kind == TYPE_POINTER) {
		array_type_t *const array_type   = &type->array;
		type_t       *const element_type = skip_typeref(array_type->element_type);

		if (element_type->kind == TYPE_ATOMIC) {
			atomic_type_kind_t akind = element_type->atomic.akind;
			switch (expression->kind) {
			case EXPR_STRING_LITERAL:
				if (akind == ATOMIC_TYPE_CHAR
						|| akind == ATOMIC_TYPE_SCHAR
						|| akind == ATOMIC_TYPE_UCHAR) {
					return initializer_from_string(array_type,
							&expression->string_literal.value);
				}
				break;

			case EXPR_WIDE_STRING_LITERAL: {
				type_t *bare_wchar_type = skip_typeref(type_wchar_t);
				if (get_unqualified_type(element_type) == bare_wchar_type) {
					return initializer_from_wide_string(array_type,
							&expression->string_literal.value);
				}
				break;
			}

			default:
				break;
			}
		}
	}

	assign_error_t error = semantic_assign(type, expression);
	if (error == ASSIGN_ERROR_INCOMPATIBLE)
		return NULL;
	report_assign_error(error, type, expression, "initializer",
	                    &expression->base.source_position);

	initializer_t *const result = allocate_initializer_zero(INITIALIZER_VALUE);
	result->value.value = create_implicit_cast(expression, type);

	return result;
}

/**
 * Parses an scalar initializer.
 *
 * §6.7.8.11; eat {} without warning
 */
static initializer_t *parse_scalar_initializer(type_t *type,
                                               bool must_be_constant)
{
	/* there might be extra {} hierarchies */
	int braces = 0;
	if (token.kind == '{') {
		warningf(WARN_OTHER, HERE, "extra curly braces around scalar initializer");
		do {
			eat('{');
			++braces;
		} while (token.kind == '{');
	}

	expression_t *expression = parse_assignment_expression();
	mark_vars_read(expression, NULL);
	if (must_be_constant && !is_linker_constant(expression)) {
		errorf(&expression->base.source_position,
		       "initialisation expression '%E' is not constant",
		       expression);
	}

	initializer_t *initializer = initializer_from_expression(type, expression);

	if (initializer == NULL) {
		errorf(&expression->base.source_position,
		       "expression '%E' (type '%T') doesn't match expected type '%T'",
		       expression, expression->base.type, type);
		/* TODO */
		return NULL;
	}

	bool additional_warning_displayed = false;
	while (braces > 0) {
		next_if(',');
		if (token.kind != '}') {
			if (!additional_warning_displayed) {
				warningf(WARN_OTHER, HERE, "additional elements in scalar initializer");
				additional_warning_displayed = true;
			}
		}
		eat_block();
		braces--;
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
	type_path_entry_t *path;         /**< An flexible array containing the current path. */
	type_t            *top_type;     /**< type of the element the path points */
	size_t             max_index;    /**< largest index in outermost array */
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
 * Return the top type path entry, ie. in a path
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

	if (is_type_compound(top_type)) {
		compound_t *const compound = top_type->compound.compound;
		entity_t   *const entry    = skip_unnamed_bitfields(compound->members.entities);

		if (entry != NULL) {
			top->v.compound_entry = &entry->declaration;
			path->top_type = entry->declaration.type;
		} else {
			path->top_type = NULL;
		}
	} else if (is_type_array(top_type)) {
		top->v.index   = 0;
		path->top_type = top_type->array.element_type;
	} else {
		assert(!is_type_valid(top_type));
	}
}

/**
 * Pop an entry from the given type path, ie. returning from
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
				if (is_type_valid(type)) {
					errorf(&designator->source_position,
					       "'.%Y' designator used for non-compound type '%T'",
					       symbol, orig_type);
				}

				top->type             = type_error_type;
				top->v.compound_entry = NULL;
				orig_type             = type_error_type;
			} else {
				compound_t *compound = type->compound.compound;
				entity_t   *iter     = compound->members.entities;
				for (; iter != NULL; iter = iter->base.next) {
					if (iter->base.symbol == symbol) {
						break;
					}
				}
				if (iter == NULL) {
					errorf(&designator->source_position,
					       "'%T' has no member named '%Y'", orig_type, symbol);
					return false;
				}
				assert(iter->kind == ENTITY_COMPOUND_MEMBER);
				if (used_in_offsetof && iter->compound_member.bitfield) {
					errorf(&designator->source_position,
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
			assert(designator->array_index != NULL);

			if (!is_type_array(type)) {
				if (is_type_valid(type)) {
					errorf(&designator->source_position,
					       "[%E] designator used for non-array type '%T'",
					       array_index, orig_type);
				}
				return false;
			}

			long index = fold_constant_to_int(array_index);
			if (!used_in_offsetof) {
				if (index < 0) {
					errorf(&designator->source_position,
					       "array index [%E] must be positive", array_index);
				} else if (type->array.size_constant) {
					long array_size = type->array.size;
					if (index >= array_size) {
						errorf(&designator->source_position,
						       "designator [%E] (%d) exceeds array size %d",
						       array_index, index, array_size);
					}
				}
			}

			top->type    = orig_type;
			top->v.index = (size_t) index;
			orig_type    = type->array.element_type;
		}
		path->top_type = orig_type;

		if (designator->next != NULL) {
			descend_into_subtype(path);
		}
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
		assert(is_type_array(type));

		top->v.index++;

		if (!type->array.size_constant || top->v.index < type->array.size) {
			return;
		}
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
	next_if('{');

	while (token.kind != '}') {
		if (token.kind == T_EOF)
			return;
		if (token.kind == '{') {
			eat_block();
			continue;
		}
		next_token();
	}
}

static initializer_t *create_empty_initializer(void)
{
	static initializer_t empty_initializer
		= { .list = { { INITIALIZER_LIST }, 0 } };
	return &empty_initializer;
}

/**
 * Parse a part of an initialiser for a struct or union,
 */
static initializer_t *parse_sub_initializer(type_path_t *path,
		type_t *outer_type, size_t top_path_level,
		parse_initializer_env_t *env)
{
	if (token.kind == '}') {
		/* empty initializer */
		return create_empty_initializer();
	}

	type_t *orig_type = path->top_type;
	type_t *type      = NULL;

	if (orig_type == NULL) {
		/* We are initializing an empty compound. */
	} else {
		type = skip_typeref(orig_type);
	}

	initializer_t **initializers = NEW_ARR_F(initializer_t*, 0);

	while (true) {
		designator_t *designator = NULL;
		if (token.kind == '.' || token.kind == '[') {
			designator = parse_designation();
			goto finish_designator;
		} else if (token.kind == T_IDENTIFIER && look_ahead(1)->kind == ':') {
			/* GNU-style designator ("identifier: value") */
			designator = allocate_ast_zero(sizeof(designator[0]));
			designator->source_position = token.base.source_position;
			designator->symbol          = token.identifier.symbol;
			eat(T_IDENTIFIER);
			eat(':');

finish_designator:
			/* reset path to toplevel, evaluate designator from there */
			ascend_to(path, top_path_level);
			if (!walk_designator(path, designator, false)) {
				/* can't continue after designation error */
				goto end_error;
			}

			initializer_t *designator_initializer
				= allocate_initializer_zero(INITIALIZER_DESIGNATOR);
			designator_initializer->designator.designator = designator;
			ARR_APP1(initializer_t*, initializers, designator_initializer);

			orig_type = path->top_type;
			type      = orig_type != NULL ? skip_typeref(orig_type) : NULL;
		}

		initializer_t *sub;

		if (token.kind == '{') {
			if (type != NULL && is_type_scalar(type)) {
				sub = parse_scalar_initializer(type, env->must_be_constant);
			} else {
				if (type == NULL) {
					if (env->entity != NULL) {
						errorf(HERE,
						     "extra brace group at end of initializer for '%Y'",
						     env->entity->base.symbol);
					} else {
						errorf(HERE, "extra brace group at end of initializer");
					}
					eat('{');
				} else {
					eat('{');
					descend_into_subtype(path);
				}

				add_anchor_token('}');
				sub = parse_sub_initializer(path, orig_type, top_path_level+1,
				                            env);
				rem_anchor_token('}');

				if (type != NULL) {
					ascend_from_subtype(path);
					expect('}', end_error);
				} else {
					expect('}', end_error);
					goto error_parse_next;
				}
			}
		} else {
			/* must be an expression */
			expression_t *expression = parse_assignment_expression();
			mark_vars_read(expression, NULL);

			if (env->must_be_constant && !is_linker_constant(expression)) {
				errorf(&expression->base.source_position,
				       "Initialisation expression '%E' is not constant",
				       expression);
			}

			if (type == NULL) {
				/* we are already outside, ... */
				if (outer_type == NULL)
					goto error_parse_next;
				type_t *const outer_type_skip = skip_typeref(outer_type);
				if (is_type_compound(outer_type_skip) &&
						!outer_type_skip->compound.compound->complete) {
					goto error_parse_next;
				}

				source_position_t const* const pos = &expression->base.source_position;
				if (env->entity != NULL) {
					warningf(WARN_OTHER, pos, "excess elements in initializer for '%Y'", env->entity->base.symbol);
				} else {
					warningf(WARN_OTHER, pos, "excess elements in initializer");
				}
				goto error_parse_next;
			}

			/* handle { "string" } special case */
			if ((expression->kind == EXPR_STRING_LITERAL
					|| expression->kind == EXPR_WIDE_STRING_LITERAL)
					&& outer_type != NULL) {
				sub = initializer_from_expression(outer_type, expression);
				if (sub != NULL) {
					next_if(',');
					if (token.kind != '}') {
						warningf(WARN_OTHER, HERE, "excessive elements in initializer for type '%T'", orig_type);
					}
					/* TODO: eat , ... */
					return sub;
				}
			}

			/* descend into subtypes until expression matches type */
			while (true) {
				orig_type = path->top_type;
				type      = skip_typeref(orig_type);

				sub = initializer_from_expression(orig_type, expression);
				if (sub != NULL) {
					break;
				}
				if (!is_type_valid(type)) {
					goto end_error;
				}
				if (is_type_scalar(type)) {
					errorf(&expression->base.source_position,
							"expression '%E' doesn't match expected type '%T'",
							expression, orig_type);
					goto end_error;
				}

				descend_into_subtype(path);
			}
		}

		/* update largest index of top array */
		const type_path_entry_t *first      = &path->path[0];
		type_t                  *first_type = first->type;
		first_type                          = skip_typeref(first_type);
		if (is_type_array(first_type)) {
			size_t index = first->v.index;
			if (index > path->max_index)
				path->max_index = index;
		}

		/* append to initializers list */
		ARR_APP1(initializer_t*, initializers, sub);

error_parse_next:
		if (token.kind == '}') {
			break;
		}
		expect(',', end_error);
		if (token.kind == '}') {
			break;
		}

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

	size_t len  = ARR_LEN(initializers);
	size_t size = sizeof(initializer_list_t) + len * sizeof(initializers[0]);
	initializer_t *result = allocate_ast_zero(size);
	result->kind          = INITIALIZER_LIST;
	result->list.len      = len;
	memcpy(&result->list.initializers, initializers,
	       len * sizeof(initializers[0]));

	DEL_ARR_F(initializers);
	ascend_to(path, top_path_level+1);

	return result;

end_error:
	skip_initializers();
	DEL_ARR_F(initializers);
	ascend_to(path, top_path_level+1);
	return NULL;
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
	type_t        *type      = skip_typeref(env->type);
	size_t         max_index = 0;
	initializer_t *result;

	if (is_type_scalar(type)) {
		result = parse_scalar_initializer(type, env->must_be_constant);
	} else if (token.kind == '{') {
		eat('{');

		type_path_t path;
		memset(&path, 0, sizeof(path));
		path.top_type = env->type;
		path.path     = NEW_ARR_F(type_path_entry_t, 0);

		descend_into_subtype(&path);

		add_anchor_token('}');
		result = parse_sub_initializer(&path, env->type, 1, env);
		rem_anchor_token('}');

		max_index = path.max_index;
		DEL_ARR_F(path.path);

		expect('}', end_error);
end_error:;
	} else {
		/* parse_scalar_initializer() also works in this case: we simply
		 * have an expression without {} around it */
		result = parse_scalar_initializer(type, env->must_be_constant);
	}

	/* §6.7.8:22 array initializers for arrays with unknown size determine
	 * the array type size */
	if (is_type_array(type) && type->array.size_expression == NULL
			&& result != NULL) {
		size_t size;
		switch (result->kind) {
		case INITIALIZER_LIST:
			assert(max_index != 0xdeadbeaf);
			size = max_index + 1;
			break;

		case INITIALIZER_STRING:
			size = result->string.string.size;
			break;

		case INITIALIZER_WIDE_STRING:
			size = result->wide_string.string.size;
			break;

		case INITIALIZER_DESIGNATOR:
		case INITIALIZER_VALUE:
			/* can happen for parse errors */
			size = 0;
			break;

		default:
			internal_errorf(HERE, "invalid initializer type");
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
		scope->entities = entity;
	}
	entity->base.parent_entity = current_entity;
	scope->last_entity         = entity;
}


static compound_t *parse_compound_type_specifier(bool is_struct)
{
	source_position_t const pos = *HERE;
	eat(is_struct ? T_struct : T_union);

	symbol_t    *symbol     = NULL;
	entity_t    *entity     = NULL;
	attribute_t *attributes = NULL;

	if (token.kind == T___attribute__) {
		attributes = parse_attributes(NULL);
	}

	entity_kind_tag_t const kind = is_struct ? ENTITY_STRUCT : ENTITY_UNION;
	if (token.kind == T_IDENTIFIER) {
		/* the compound has a name, check if we have seen it already */
		symbol = token.identifier.symbol;
		entity = get_tag(symbol, kind);
		next_token();

		if (entity != NULL) {
			if (entity->base.parent_scope != current_scope &&
			    (token.kind == '{' || token.kind == ';')) {
				/* we're in an inner scope and have a definition. Shadow
				 * existing definition in outer scope */
				entity = NULL;
			} else if (entity->compound.complete && token.kind == '{') {
				source_position_t const *const ppos = &entity->base.source_position;
				errorf(&pos, "multiple definitions of '%N' (previous definition %P)", entity, ppos);
				/* clear members in the hope to avoid further errors */
				entity->compound.members.entities = NULL;
			}
		}
	} else if (token.kind != '{') {
		char const *const msg =
			is_struct ? "while parsing struct type specifier" :
			            "while parsing union type specifier";
		parse_error_expected(msg, T_IDENTIFIER, '{', NULL);

		return NULL;
	}

	if (entity == NULL) {
		entity = allocate_entity_zero(kind, NAMESPACE_TAG, symbol);
		entity->compound.alignment   = 1;
		entity->base.source_position = pos;
		entity->base.parent_scope    = current_scope;
		if (symbol != NULL) {
			environment_push(entity);
		}
		append_entity(current_scope, entity);
	}

	if (token.kind == '{') {
		parse_compound_type_entries(&entity->compound);

		/* ISO/IEC 14882:1998(E) §7.1.3:5 */
		if (symbol == NULL) {
			assert(anonymous_entity == NULL);
			anonymous_entity = entity;
		}
	}

	if (attributes != NULL) {
		handle_entity_attributes(attributes, entity);
	}

	return &entity->compound;
}

static void parse_enum_entries(type_t *const enum_type)
{
	eat('{');

	if (token.kind == '}') {
		errorf(HERE, "empty enum not allowed");
		next_token();
		return;
	}

	add_anchor_token('}');
	do {
		if (token.kind != T_IDENTIFIER) {
			parse_error_expected("while parsing enum entry", T_IDENTIFIER, NULL);
			eat_block();
			rem_anchor_token('}');
			return;
		}

		symbol_t *symbol       = token.identifier.symbol;
		entity_t *const entity
			= allocate_entity_zero(ENTITY_ENUM_VALUE, NAMESPACE_NORMAL, symbol);
		entity->enum_value.enum_type = enum_type;
		entity->base.source_position = token.base.source_position;
		next_token();

		if (next_if('=')) {
			expression_t *value = parse_constant_expression();

			value = create_implicit_cast(value, enum_type);
			entity->enum_value.value = value;

			/* TODO semantic */
		}

		record_entity(entity, false);
	} while (next_if(',') && token.kind != '}');
	rem_anchor_token('}');

	expect('}', end_error);

end_error:
	;
}

static type_t *parse_enum_specifier(void)
{
	source_position_t const pos = *HERE;
	entity_t               *entity;
	symbol_t               *symbol;

	eat(T_enum);
	switch (token.kind) {
		case T_IDENTIFIER:
			symbol = token.identifier.symbol;
			entity = get_tag(symbol, ENTITY_ENUM);
			next_token();

			if (entity != NULL) {
				if (entity->base.parent_scope != current_scope &&
						(token.kind == '{' || token.kind == ';')) {
					/* we're in an inner scope and have a definition. Shadow
					 * existing definition in outer scope */
					entity = NULL;
				} else if (entity->enume.complete && token.kind == '{') {
					source_position_t const *const ppos = &entity->base.source_position;
					errorf(&pos, "multiple definitions of '%N' (previous definition %P)", entity, ppos);
				}
			}
			break;

		case '{':
			entity = NULL;
			symbol = NULL;
			break;

		default:
			parse_error_expected("while parsing enum type specifier",
					T_IDENTIFIER, '{', NULL);
			return NULL;
	}

	if (entity == NULL) {
		entity = allocate_entity_zero(ENTITY_ENUM, NAMESPACE_TAG, symbol);
		entity->base.source_position = pos;
		entity->base.parent_scope    = current_scope;
	}

	type_t *const type     = allocate_type_zero(TYPE_ENUM);
	type->enumt.enume      = &entity->enume;
	type->enumt.base.akind = ATOMIC_TYPE_INT;

	if (token.kind == '{') {
		if (symbol != NULL) {
			environment_push(entity);
		}
		append_entity(current_scope, entity);
		entity->enume.complete = true;

		parse_enum_entries(type);
		parse_attributes(NULL);

		/* ISO/IEC 14882:1998(E) §7.1.3:5 */
		if (symbol == NULL) {
			assert(anonymous_entity == NULL);
			anonymous_entity = entity;
		}
	} else if (!entity->enume.complete && !(c_mode & _GNUC)) {
		errorf(HERE, "'%T' used before definition (incomplete enums are a GNU extension)", type);
	}

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

static type_t *parse_typeof(void)
{
	eat(T___typeof__);

	type_t *type;

	expect('(', end_error);
	add_anchor_token(')');

	expression_t *expression  = NULL;

	switch (token.kind) {
	case T_IDENTIFIER:
		if (is_typedef_symbol(token.identifier.symbol)) {
	DECLARATION_START
			type = parse_typename();
		} else {
	default:
			expression = parse_expression();
			type       = revert_automatic_type_conversion(expression);
		}
		break;
	}

	rem_anchor_token(')');
	expect(')', end_error);

	type_t *typeof_type              = allocate_type_zero(TYPE_TYPEOF);
	typeof_type->typeoft.expression  = expression;
	typeof_type->typeoft.typeof_type = type;

	return typeof_type;
end_error:
	return NULL;
}

typedef enum specifiers_t {
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
	SPECIFIER_VOID      = 1 << 11,
	SPECIFIER_INT8      = 1 << 12,
	SPECIFIER_INT16     = 1 << 13,
	SPECIFIER_INT32     = 1 << 14,
	SPECIFIER_INT64     = 1 << 15,
	SPECIFIER_INT128    = 1 << 16,
	SPECIFIER_COMPLEX   = 1 << 17,
	SPECIFIER_IMAGINARY = 1 << 18,
} specifiers_t;

static type_t *get_typedef_type(symbol_t *symbol)
{
	entity_t *entity = get_entity(symbol, NAMESPACE_NORMAL);
	if (entity == NULL || entity->kind != ENTITY_TYPEDEF)
		return NULL;

	type_t *type            = allocate_type_zero(TYPE_TYPEDEF);
	type->typedeft.typedefe = &entity->typedefe;

	return type;
}

static attribute_t *parse_attribute_ms_property(attribute_t *attribute)
{
	expect('(', end_error);

	attribute_property_argument_t *property
		= allocate_ast_zero(sizeof(*property));

	do {
		if (token.kind != T_IDENTIFIER) {
			parse_error_expected("while parsing property declspec",
			                     T_IDENTIFIER, NULL);
			goto end_error;
		}

		symbol_t **prop;
		symbol_t  *symbol = token.identifier.symbol;
		if (streq(symbol->string, "put")) {
			prop = &property->put_symbol;
		} else if (streq(symbol->string, "get")) {
			prop = &property->get_symbol;
		} else {
			errorf(HERE, "expected put or get in property declspec");
			prop = NULL;
		}
		eat(T_IDENTIFIER);
		expect('=', end_error);
		if (token.kind != T_IDENTIFIER) {
			parse_error_expected("while parsing property declspec",
			                     T_IDENTIFIER, NULL);
			goto end_error;
		}
		if (prop != NULL)
			*prop = token.identifier.symbol;
		next_token();
	} while (next_if(','));

	attribute->a.property = property;

	expect(')', end_error);

end_error:
	return attribute;
}

static attribute_t *parse_microsoft_extended_decl_modifier_single(void)
{
	attribute_kind_t kind = ATTRIBUTE_UNKNOWN;
	if (next_if(T_restrict)) {
		kind = ATTRIBUTE_MS_RESTRICT;
	} else if (token.kind == T_IDENTIFIER) {
		const char *name = token.identifier.symbol->string;
		for (attribute_kind_t k = ATTRIBUTE_MS_FIRST; k <= ATTRIBUTE_MS_LAST;
		     ++k) {
			const char *attribute_name = get_attribute_name(k);
			if (attribute_name != NULL && streq(attribute_name, name)) {
				kind = k;
				break;
			}
		}

		if (kind == ATTRIBUTE_UNKNOWN) {
			warningf(WARN_ATTRIBUTE, HERE, "unknown __declspec '%s' ignored", name);
		}
	} else {
		parse_error_expected("while parsing __declspec", T_IDENTIFIER, NULL);
		return NULL;
	}

	attribute_t *attribute = allocate_attribute_zero(kind);
	eat(T_IDENTIFIER);

	if (kind == ATTRIBUTE_MS_PROPERTY) {
		return parse_attribute_ms_property(attribute);
	}

	/* parse arguments */
	if (next_if('('))
		attribute->a.arguments = parse_attribute_arguments();

	return attribute;
}

static attribute_t *parse_microsoft_extended_decl_modifier(attribute_t *first)
{
	eat(T__declspec);

	expect('(', end_error);

	if (next_if(')'))
		return NULL;

	add_anchor_token(')');

	attribute_t **anchor = &first;
	do {
		while (*anchor != NULL)
			anchor = &(*anchor)->next;

		attribute_t *attribute
			= parse_microsoft_extended_decl_modifier_single();
		if (attribute == NULL)
			goto end_error;

		*anchor = attribute;
		anchor  = &attribute->next;
	} while (next_if(','));

	rem_anchor_token(')');
	expect(')', end_error);
	return first;

end_error:
	rem_anchor_token(')');
	return first;
}

static entity_t *create_error_entity(symbol_t *symbol, entity_kind_tag_t kind)
{
	entity_t *const entity = allocate_entity_zero(kind, NAMESPACE_NORMAL, symbol);
	entity->base.source_position = *HERE;
	if (is_declaration(entity)) {
		entity->declaration.type     = type_error_type;
		entity->declaration.implicit = true;
	} else if (kind == ENTITY_TYPEDEF) {
		entity->typedefe.type    = type_error_type;
		entity->typedefe.builtin = true;
	}
	if (kind != ENTITY_COMPOUND_MEMBER)
		record_entity(entity, false);
	return entity;
}

static void parse_declaration_specifiers(declaration_specifiers_t *specifiers)
{
	type_t            *type            = NULL;
	type_qualifiers_t  qualifiers      = TYPE_QUALIFIER_NONE;
	unsigned           type_specifiers = 0;
	bool               newtype         = false;
	bool               saw_error       = false;

	memset(specifiers, 0, sizeof(*specifiers));
	specifiers->source_position = token.base.source_position;

	while (true) {
		specifiers->attributes = parse_attributes(specifiers->attributes);

		switch (token.kind) {
		/* storage class */
#define MATCH_STORAGE_CLASS(token, class)                                  \
		case token:                                                        \
			if (specifiers->storage_class != STORAGE_CLASS_NONE) {         \
				errorf(HERE, "multiple storage classes in declaration specifiers"); \
			}                                                              \
			specifiers->storage_class = class;                             \
			if (specifiers->thread_local)                                  \
				goto check_thread_storage_class;                           \
			next_token();                                                  \
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

		case T___thread:
			if (specifiers->thread_local) {
				errorf(HERE, "duplicate '__thread'");
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
						errorf(HERE, "'__thread' used with '%s'", wrong);
						break;
				}
			}
			next_token();
			break;

		/* type qualifiers */
#define MATCH_TYPE_QUALIFIER(token, qualifier)                          \
		case token:                                                     \
			qualifiers |= qualifier;                                    \
			next_token();                                               \
			break

		MATCH_TYPE_QUALIFIER(T_const,    TYPE_QUALIFIER_CONST);
		MATCH_TYPE_QUALIFIER(T_restrict, TYPE_QUALIFIER_RESTRICT);
		MATCH_TYPE_QUALIFIER(T_volatile, TYPE_QUALIFIER_VOLATILE);
		MATCH_TYPE_QUALIFIER(T__w64,     TYPE_QUALIFIER_W64);
		MATCH_TYPE_QUALIFIER(T___ptr32,  TYPE_QUALIFIER_PTR32);
		MATCH_TYPE_QUALIFIER(T___ptr64,  TYPE_QUALIFIER_PTR64);
		MATCH_TYPE_QUALIFIER(T___uptr,   TYPE_QUALIFIER_UPTR);
		MATCH_TYPE_QUALIFIER(T___sptr,   TYPE_QUALIFIER_SPTR);

		/* type specifiers */
#define MATCH_SPECIFIER(token, specifier, name)                         \
		case token:                                                     \
			if (type_specifiers & specifier) {                           \
				errorf(HERE, "multiple " name " type specifiers given"); \
			} else {                                                    \
				type_specifiers |= specifier;                           \
			}                                                           \
			next_token();                                               \
			break

		MATCH_SPECIFIER(T__Bool,      SPECIFIER_BOOL,      "_Bool");
		MATCH_SPECIFIER(T__Complex,   SPECIFIER_COMPLEX,   "_Complex");
		MATCH_SPECIFIER(T__Imaginary, SPECIFIER_IMAGINARY, "_Imaginary");
		MATCH_SPECIFIER(T__int128,    SPECIFIER_INT128,    "_int128");
		MATCH_SPECIFIER(T__int16,     SPECIFIER_INT16,     "_int16");
		MATCH_SPECIFIER(T__int32,     SPECIFIER_INT32,     "_int32");
		MATCH_SPECIFIER(T__int64,     SPECIFIER_INT64,     "_int64");
		MATCH_SPECIFIER(T__int8,      SPECIFIER_INT8,      "_int8");
		MATCH_SPECIFIER(T_bool,       SPECIFIER_BOOL,      "bool");
		MATCH_SPECIFIER(T_char,       SPECIFIER_CHAR,      "char");
		MATCH_SPECIFIER(T_double,     SPECIFIER_DOUBLE,    "double");
		MATCH_SPECIFIER(T_float,      SPECIFIER_FLOAT,     "float");
		MATCH_SPECIFIER(T_int,        SPECIFIER_INT,       "int");
		MATCH_SPECIFIER(T_short,      SPECIFIER_SHORT,     "short");
		MATCH_SPECIFIER(T_signed,     SPECIFIER_SIGNED,    "signed");
		MATCH_SPECIFIER(T_unsigned,   SPECIFIER_UNSIGNED,  "unsigned");
		MATCH_SPECIFIER(T_void,       SPECIFIER_VOID,      "void");
		MATCH_SPECIFIER(T_wchar_t,    SPECIFIER_WCHAR_T,   "wchar_t");

		case T_inline:
			next_token();
			specifiers->is_inline = true;
			break;

#if 0
		case T__forceinline:
			next_token();
			specifiers->modifiers |= DM_FORCEINLINE;
			break;
#endif

		case T_long:
			if (type_specifiers & SPECIFIER_LONG_LONG) {
				errorf(HERE, "too many long type specifiers given");
			} else if (type_specifiers & SPECIFIER_LONG) {
				type_specifiers |= SPECIFIER_LONG_LONG;
			} else {
				type_specifiers |= SPECIFIER_LONG;
			}
			next_token();
			break;

#define CHECK_DOUBLE_TYPE() \
	(type != NULL ? errorf(HERE, "multiple types in declaration specifiers") : (void)0)

		case T_struct:
			CHECK_DOUBLE_TYPE();
			type = allocate_type_zero(TYPE_COMPOUND_STRUCT);

			type->compound.compound = parse_compound_type_specifier(true);
			break;
		case T_union:
			CHECK_DOUBLE_TYPE();
			type = allocate_type_zero(TYPE_COMPOUND_UNION);
			type->compound.compound = parse_compound_type_specifier(false);
			break;
		case T_enum:
			CHECK_DOUBLE_TYPE();
			type = parse_enum_specifier();
			break;
		case T___typeof__:
			CHECK_DOUBLE_TYPE();
			type = parse_typeof();
			break;
		case T___builtin_va_list:
			CHECK_DOUBLE_TYPE();
			type = duplicate_type(type_valist);
			next_token();
			break;

		case T_IDENTIFIER: {
			/* only parse identifier if we haven't found a type yet */
			if (type != NULL || type_specifiers != 0) {
				/* Be somewhat resilient to typos like 'unsigned lng* f()' in a
				 * declaration, so it doesn't generate errors about expecting '(' or
				 * '{' later on. */
				switch (look_ahead(1)->kind) {
					STORAGE_CLASSES
					TYPE_SPECIFIERS
					case T_const:
					case T_restrict:
					case T_volatile:
					case T_inline:
					case T__forceinline: /* ^ DECLARATION_START except for __attribute__ */
					case T_IDENTIFIER:
					case '&':
					case '*':
						errorf(HERE, "discarding stray %K in declaration specifier", &token);
						next_token();
						continue;

					default:
						goto finish_specifiers;
				}
			}

			type_t *const typedef_type = get_typedef_type(token.identifier.symbol);
			if (typedef_type == NULL) {
				/* Be somewhat resilient to typos like 'vodi f()' at the beginning of a
				 * declaration, so it doesn't generate 'implicit int' followed by more
				 * errors later on. */
				token_kind_t const la1_type = (token_kind_t)look_ahead(1)->kind;
				switch (la1_type) {
					DECLARATION_START
					case T_IDENTIFIER:
					case '&':
					case '*': {
						errorf(HERE, "%K does not name a type", &token);

						symbol_t *symbol = token.identifier.symbol;
						entity_t *entity
							= create_error_entity(symbol, ENTITY_TYPEDEF);

						type = allocate_type_zero(TYPE_TYPEDEF);
						type->typedeft.typedefe = &entity->typedefe;

						next_token();
						saw_error = true;
						continue;
					}

					default:
						goto finish_specifiers;
				}
			}

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
	specifiers->attributes = parse_attributes(specifiers->attributes);

	if (type == NULL || (saw_error && type_specifiers != 0)) {
		atomic_type_kind_t atomic_type;

		/* match valid basic types */
		switch (type_specifiers) {
		case SPECIFIER_VOID:
			atomic_type = ATOMIC_TYPE_VOID;
			break;
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
			warningf(WARN_LONG_LONG, &specifiers->source_position, "ISO C90 does not support 'long long'");
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

		case SPECIFIER_UNSIGNED | SPECIFIER_INT128:
			atomic_type = unsigned_int128_type_kind;
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

		case SPECIFIER_INT128:
		case SPECIFIER_SIGNED | SPECIFIER_INT128:
			atomic_type = int128_type_kind;
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
		case SPECIFIER_FLOAT | SPECIFIER_COMPLEX:
		case SPECIFIER_FLOAT | SPECIFIER_IMAGINARY:
			atomic_type = ATOMIC_TYPE_FLOAT;
			break;
		case SPECIFIER_DOUBLE | SPECIFIER_COMPLEX:
		case SPECIFIER_DOUBLE | SPECIFIER_IMAGINARY:
			atomic_type = ATOMIC_TYPE_DOUBLE;
			break;
		case SPECIFIER_LONG | SPECIFIER_DOUBLE | SPECIFIER_COMPLEX:
		case SPECIFIER_LONG | SPECIFIER_DOUBLE | SPECIFIER_IMAGINARY:
			atomic_type = ATOMIC_TYPE_LONG_DOUBLE;
			break;
		default: {
			/* invalid specifier combination, give an error message */
			source_position_t const* const pos = &specifiers->source_position;
			if (type_specifiers == 0) {
				if (!saw_error) {
					/* ISO/IEC 14882:1998(E) §C.1.5:4 */
					if (!(c_mode & _CXX) && !strict_mode) {
						warningf(WARN_IMPLICIT_INT, pos, "no type specifiers in declaration, using 'int'");
						atomic_type = ATOMIC_TYPE_INT;
						break;
					} else {
						errorf(pos, "no type specifiers given in declaration");
					}
				}
			} else if ((type_specifiers & SPECIFIER_SIGNED) &&
			          (type_specifiers & SPECIFIER_UNSIGNED)) {
				errorf(pos, "signed and unsigned specifiers given");
			} else if (type_specifiers & (SPECIFIER_SIGNED | SPECIFIER_UNSIGNED)) {
				errorf(pos, "only integer types can be signed or unsigned");
			} else {
				errorf(pos, "multiple datatypes in declaration");
			}
			goto end_error;
		}
		}

		if (type_specifiers & SPECIFIER_COMPLEX) {
			type = allocate_type_zero(TYPE_COMPLEX);
		} else if (type_specifiers & SPECIFIER_IMAGINARY) {
			type = allocate_type_zero(TYPE_IMAGINARY);
		} else {
			type = allocate_type_zero(TYPE_ATOMIC);
		}
		type->atomic.akind = atomic_type;
		newtype = true;
	} else if (type_specifiers != 0) {
		errorf(&specifiers->source_position, "multiple datatypes in declaration");
	}

	/* FIXME: check type qualifiers here */
	type->base.qualifiers = qualifiers;

	if (newtype) {
		type = identify_new_type(type);
	} else {
		type = typehash_insert(type);
	}

	if (specifiers->attributes != NULL)
		type = handle_type_attributes(specifiers->attributes, type);
	specifiers->type = type;
	return;

end_error:
	specifiers->type = type_error_type;
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
	assert(token.kind == T_IDENTIFIER);
	do {
		entity_t *const entity = allocate_entity_zero(ENTITY_PARAMETER, NAMESPACE_NORMAL, token.identifier.symbol);
		entity->base.source_position = token.base.source_position;
		/* a K&R parameter has no type, yet */
		next_token();

		if (scope != NULL)
			append_entity(scope, entity);
	} while (next_if(',') && token.kind == T_IDENTIFIER);
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

static void semantic_parameter_incomplete(const entity_t *entity)
{
	assert(entity->kind == ENTITY_PARAMETER);

	/* §6.7.5.3:4  After adjustment, the parameters in a parameter type
	 *             list in a function declarator that is part of a
	 *             definition of that function shall not have
	 *             incomplete type. */
	type_t *type = skip_typeref(entity->declaration.type);
	if (is_type_incomplete(type)) {
		errorf(&entity->base.source_position, "'%N' has incomplete type", entity);
	}
}

static bool has_parameters(void)
{
	/* func(void) is not a parameter */
	if (look_ahead(1)->kind != ')')
		return true;
	if (token.kind == T_IDENTIFIER) {
		entity_t const *const entity
			= get_entity(token.identifier.symbol, NAMESPACE_NORMAL);
		if (entity == NULL)
			return true;
		if (entity->kind != ENTITY_TYPEDEF)
			return true;
		type_t const *const type = skip_typeref(entity->typedefe.type);
		if (!is_type_void(type))
			return true;
		if (type->base.qualifiers != TYPE_QUALIFIER_NONE) {
			/* §6.7.5.3:10  Qualification is not allowed here. */
			errorf(HERE, "'void' as parameter must not have type qualifiers");
		}
	} else if (token.kind != T_void) {
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
	eat('(');
	add_anchor_token(')');
	int saved_comma_state = save_and_reset_anchor_state(',');

	if (token.kind == T_IDENTIFIER
	    && !is_typedef_symbol(token.identifier.symbol)) {
		token_kind_t la1_type = (token_kind_t)look_ahead(1)->kind;
		if (la1_type == ',' || la1_type == ')') {
			type->kr_style_parameters = true;
			parse_identifier_list(scope);
			goto parameters_finished;
		}
	}

	if (token.kind == ')') {
		/* ISO/IEC 14882:1998(E) §C.1.6:1 */
		if (!(c_mode & _CXX))
			type->unspecified_parameters = true;
	} else if (has_parameters()) {
		function_parameter_t **anchor = &type->parameters;
		do {
			switch (token.kind) {
			case T_DOTDOTDOT:
				next_token();
				type->variadic = true;
				goto parameters_finished;

			case T_IDENTIFIER:
			DECLARATION_START
			{
				entity_t *entity = parse_parameter();
				if (entity->kind == ENTITY_TYPEDEF) {
					errorf(&entity->base.source_position,
							"typedef not allowed as function parameter");
					break;
				}
				assert(is_declaration(entity));

				semantic_parameter_incomplete(entity);

				function_parameter_t *const parameter =
					allocate_parameter(entity->declaration.type);

				if (scope != NULL) {
					append_entity(scope, entity);
				}

				*anchor = parameter;
				anchor  = &parameter->next;
				break;
			}

			default:
				goto parameters_finished;
			}
		} while (next_if(','));
	}

parameters_finished:
	rem_anchor_token(')');
	expect(')', end_error);

end_error:
	restore_anchor_state(',', saved_comma_state);
}

typedef enum construct_type_kind_t {
	CONSTRUCT_POINTER = 1,
	CONSTRUCT_REFERENCE,
	CONSTRUCT_FUNCTION,
	CONSTRUCT_ARRAY
} construct_type_kind_t;

typedef union construct_type_t construct_type_t;

typedef struct construct_type_base_t {
	construct_type_kind_t  kind;
	source_position_t      pos;
	construct_type_t      *next;
} construct_type_base_t;

typedef struct parsed_pointer_t {
	construct_type_base_t  base;
	type_qualifiers_t      type_qualifiers;
	variable_t            *base_variable;  /**< MS __based extension. */
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
	//cons->pointer.base_variable   = base_variable;

	return cons;
}

/* ISO/IEC 14882:1998(E) §8.3.2 */
static construct_type_t *parse_reference_declarator(void)
{
	if (!(c_mode & _CXX))
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

	bool is_static = next_if(T_static);

	type_qualifiers_t type_qualifiers = parse_type_qualifiers();

	if (!is_static)
		is_static = next_if(T_static);

	array->type_qualifiers = type_qualifiers;
	array->is_static       = is_static;

	expression_t *size = NULL;
	if (token.kind == '*' && look_ahead(1)->kind == ']') {
		array->is_variable = true;
		next_token();
	} else if (token.kind != ']') {
		size = parse_assignment_expression();

		/* §6.7.5.2:1  Array size must have integer type */
		type_t *const orig_type = size->base.type;
		type_t *const type      = skip_typeref(orig_type);
		if (!is_type_integer(type) && is_type_valid(type)) {
			errorf(&size->base.source_position,
			       "array size '%E' must have integer type but has type '%T'",
			       size, orig_type);
		}

		array->size = size;
		mark_vars_read(size, NULL);
	}

	if (is_static && size == NULL)
		errorf(&array->base.pos, "static array parameters require a size");

	rem_anchor_token(']');
	expect(']', end_error);

end_error:
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
	bool               may_be_abstract : 1;
	bool               must_be_abstract : 1;
	decl_modifiers_t   modifiers;
	symbol_t          *symbol;
	source_position_t  source_position;
	scope_t            parameters;
	attribute_t       *attributes;
} parse_declarator_env_t;

/* §6.7.5 */
static construct_type_t *parse_inner_declarator(parse_declarator_env_t *env)
{
	/* construct a single linked list of construct_type_t's which describe
	 * how to construct the final declarator type */
	construct_type_t  *first      = NULL;
	construct_type_t **anchor     = &first;

	env->attributes = parse_attributes(env->attributes);

	for (;;) {
		construct_type_t *type;
		//variable_t       *based = NULL; /* MS __based extension */
		switch (token.kind) {
			case '&':
				type = parse_reference_declarator();
				break;

			case T__based: {
				panic("based not supported anymore");
				/* FALLTHROUGH */
			}

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
			errorf(HERE, "no identifier expected in typename");
		} else {
			env->symbol          = token.identifier.symbol;
			env->source_position = token.base.source_position;
		}
		next_token();
		break;

	case '(': {
		/* Parenthesized declarator or function declarator? */
		token_t const *const la1 = look_ahead(1);
		switch (la1->kind) {
			case T_IDENTIFIER:
				if (is_typedef_symbol(la1->identifier.symbol)) {
			case ')':
					/* §6.7.6:2 footnote 126:  Empty parentheses in a type name are
					 * interpreted as ``function with no parameter specification'', rather
					 * than redundant parentheses around the omitted identifier. */
			default:
					/* Function declarator. */
					if (!env->may_be_abstract) {
						errorf(HERE, "function declarator must have a name");
					}
				} else {
			case '&':
			case '(':
			case '*':
			case '[':
			case T___attribute__: /* FIXME __attribute__ might also introduce a parameter of a function declarator. */
					/* Paranthesized declarator. */
					next_token();
					add_anchor_token(')');
					inner_types = parse_inner_declarator(env);
					if (inner_types != NULL) {
						/* All later declarators only modify the return type */
						env->must_be_abstract = true;
					}
					rem_anchor_token(')');
					expect(')', end_error);
				}
				break;
		}
		break;
	}

	default:
		if (env->may_be_abstract)
			break;
		parse_error_expected("while parsing declarator", T_IDENTIFIER, '(', NULL);
		eat_until_anchor();
		return NULL;
	}

	construct_type_t **const p = anchor;

	for (;;) {
		construct_type_t *type;
		switch (token.kind) {
		case '(': {
			scope_t *scope = NULL;
			if (!env->must_be_abstract) {
				scope = &env->parameters;
			}

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
end_error:
	return NULL;
}

static type_t *construct_declarator_type(construct_type_t *construct_list,
                                         type_t *type)
{
	construct_type_t *iter = construct_list;
	for (; iter != NULL; iter = iter->base.next) {
		source_position_t const* const pos = &iter->base.pos;
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
			} else {
				if (skipped_return_type->base.qualifiers != 0) {
					warningf(WARN_IGNORED_QUALIFIERS, pos, "type qualifiers in return type of function type are meaningless");
				}
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
			type = make_based_pointer_type(type, pointer->type_qualifiers, pointer->base_variable);
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

			expression_t *size_expression = array->size;
			if (size_expression != NULL) {
				size_expression
					= create_implicit_cast(size_expression, type_size_t);
			}

			array_type->base.qualifiers       = array->type_qualifiers;
			array_type->array.element_type    = type;
			array_type->array.is_static       = array->is_static;
			array_type->array.is_variable     = array->is_variable;
			array_type->array.size_expression = size_expression;

			if (size_expression != NULL) {
				switch (is_constant_expression(size_expression)) {
				case EXPR_CLASS_CONSTANT: {
					long const size = fold_constant_to_int(size_expression);
					array_type->array.size          = size;
					array_type->array.size_constant = true;
					/* §6.7.5.2:1  If the expression is a constant expression,
					 * it shall have a value greater than zero. */
					if (size < 0) {
						errorf(&size_expression->base.source_position,
							   "size of array must be greater than zero");
					} else if (size == 0 && !GNU_MODE) {
						errorf(&size_expression->base.source_position,
							   "size of array must be greater than zero (zero length arrays are a GCC extension)");
					}
					break;
				}

				case EXPR_CLASS_VARIABLE:
					array_type->array.is_vla = true;
					break;

				case EXPR_CLASS_ERROR:
					break;
				}
			}

			type_t *skipped_type = skip_typeref(type);
			/* §6.7.5.2:1 */
			if (is_type_incomplete(skipped_type)) {
				errorf(pos, "array of incomplete type '%T' is not allowed", type);
			} else if (is_type_function(skipped_type)) {
				errorf(pos, "array of functions is not allowed");
			}
			type = identify_new_type(array_type);
			continue;
		}
		}
		internal_errorf(pos, "invalid type construction found");
	}

	return type;
}

static type_t *automatic_type_conversion(type_t *orig_type);

static type_t *semantic_parameter(const source_position_t *pos,
                                  type_t *type,
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

	if (specifiers->is_inline && is_type_valid(type)) {
		errorf(pos, "'%N' declared 'inline'", param);
	}

	/* §6.9.1:6  The declarations in the declaration list shall contain
	 *           no storage-class specifier other than register and no
	 *           initializations. */
	if (specifiers->thread_local || (
			specifiers->storage_class != STORAGE_CLASS_NONE   &&
			specifiers->storage_class != STORAGE_CLASS_REGISTER)
	   ) {
		errorf(pos, "invalid storage class for '%N'", param);
	}

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

	if (construct_type != NULL) {
		obstack_free(&temp_obst, construct_type);
	}

	attribute_t *attributes = parse_attributes(env.attributes);
	/* append (shared) specifier attribute behind attributes of this
	 * declarator */
	attribute_t **anchor = &attributes;
	while (*anchor != NULL)
		anchor = &(*anchor)->next;
	*anchor = specifiers->attributes;

	entity_t *entity;
	if (specifiers->storage_class == STORAGE_CLASS_TYPEDEF) {
		entity = allocate_entity_zero(ENTITY_TYPEDEF, NAMESPACE_NORMAL, env.symbol);
		entity->base.source_position = env.source_position;
		entity->typedefe.type        = orig_type;

		if (anonymous_entity != NULL) {
			if (is_type_compound(type)) {
				assert(anonymous_entity->compound.alias == NULL);
				assert(anonymous_entity->kind == ENTITY_STRUCT ||
				       anonymous_entity->kind == ENTITY_UNION);
				anonymous_entity->compound.alias = entity;
				anonymous_entity = NULL;
			} else if (is_type_enum(type)) {
				assert(anonymous_entity->enume.alias == NULL);
				assert(anonymous_entity->kind == ENTITY_ENUM);
				anonymous_entity->enume.alias = entity;
				anonymous_entity = NULL;
			}
		}
	} else {
		/* create a declaration type entity */
		if (flags & DECL_CREATE_COMPOUND_MEMBER) {
			entity = allocate_entity_zero(ENTITY_COMPOUND_MEMBER, NAMESPACE_NORMAL, env.symbol);

			if (env.symbol != NULL) {
				if (specifiers->is_inline && is_type_valid(type)) {
					errorf(&env.source_position,
							"compound member '%Y' declared 'inline'", env.symbol);
				}

				if (specifiers->thread_local ||
						specifiers->storage_class != STORAGE_CLASS_NONE) {
					errorf(&env.source_position,
							"compound member '%Y' must have no storage class",
							env.symbol);
				}
			}
		} else if (flags & DECL_IS_PARAMETER) {
			entity    = allocate_entity_zero(ENTITY_PARAMETER, NAMESPACE_NORMAL, env.symbol);
			orig_type = semantic_parameter(&env.source_position, orig_type, specifiers, entity);
		} else if (is_type_function(type)) {
			entity = allocate_entity_zero(ENTITY_FUNCTION, NAMESPACE_NORMAL, env.symbol);
			entity->function.is_inline      = specifiers->is_inline;
			entity->function.elf_visibility = default_visibility;
			entity->function.parameters     = env.parameters;

			if (env.symbol != NULL) {
				/* this needs fixes for C++ */
				bool in_function_scope = current_function != NULL;

				if (specifiers->thread_local || (
							specifiers->storage_class != STORAGE_CLASS_EXTERN &&
							specifiers->storage_class != STORAGE_CLASS_NONE   &&
							(in_function_scope || specifiers->storage_class != STORAGE_CLASS_STATIC)
						)) {
					errorf(&env.source_position, "invalid storage class for '%N'", entity);
				}
			}
		} else {
			entity = allocate_entity_zero(ENTITY_VARIABLE, NAMESPACE_NORMAL, env.symbol);
			entity->variable.elf_visibility = default_visibility;
			entity->variable.thread_local   = specifiers->thread_local;

			if (env.symbol != NULL) {
				if (specifiers->is_inline && is_type_valid(type)) {
					errorf(&env.source_position, "'%N' declared 'inline'", entity);
				}

				bool invalid_storage_class = false;
				if (current_scope == file_scope) {
					if (specifiers->storage_class != STORAGE_CLASS_EXTERN &&
							specifiers->storage_class != STORAGE_CLASS_NONE   &&
							specifiers->storage_class != STORAGE_CLASS_STATIC) {
						invalid_storage_class = true;
					}
				} else {
					if (specifiers->thread_local &&
							specifiers->storage_class == STORAGE_CLASS_NONE) {
						invalid_storage_class = true;
					}
				}
				if (invalid_storage_class) {
					errorf(&env.source_position, "invalid storage class for variable '%N'", entity);
				}
			}
		}

		entity->base.source_position   = env.symbol != NULL ? env.source_position : specifiers->source_position;
		entity->declaration.type       = orig_type;
		entity->declaration.alignment  = get_type_alignment(orig_type);
		entity->declaration.modifiers  = env.modifiers;
		entity->declaration.attributes = attributes;

		storage_class_t storage_class = specifiers->storage_class;
		entity->declaration.declared_storage_class = storage_class;

		if (storage_class == STORAGE_CLASS_NONE && current_function != NULL)
			storage_class = STORAGE_CLASS_AUTO;
		entity->declaration.storage_class = storage_class;
	}

	if (attributes != NULL) {
		handle_entity_attributes(attributes, entity);
	}

	if (entity->kind == ENTITY_FUNCTION && !freestanding) {
		adapt_special_functions(&entity->function);
	}

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
	if (construct_type != NULL) {
		obstack_free(&temp_obst, construct_type);
	}
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
	const source_position_t *pos = &entity->base.source_position;
	if (entity->kind != ENTITY_FUNCTION) {
		warningf(WARN_MAIN, pos, "'main' is not a function");
		return;
	}

	if (entity->declaration.storage_class == STORAGE_CLASS_STATIC) {
		warningf(WARN_MAIN, pos, "'main' is normally a non-static function");
	}

	type_t *type = skip_typeref(entity->declaration.type);
	assert(is_type_function(type));

	function_type_t const *const func_type = &type->function;
	type_t                *const ret_type  = func_type->return_type;
	if (!types_compatible(skip_typeref(ret_type), type_int)) {
		warningf(WARN_MAIN, pos, "return type of 'main' should be 'int', but is '%T'", ret_type);
	}
	const function_parameter_t *parm = func_type->parameters;
	if (parm != NULL) {
		type_t *const first_type        = skip_typeref(parm->type);
		type_t *const first_type_unqual = get_unqualified_type(first_type);
		if (!types_compatible(first_type_unqual, type_int)) {
			warningf(WARN_MAIN, pos, "first argument of 'main' should be 'int', but is '%T'", parm->type);
		}
		parm = parm->next;
		if (parm != NULL) {
			type_t *const second_type = skip_typeref(parm->type);
			type_t *const second_type_unqual
				= get_unqualified_type(second_type);
			if (!types_compatible(second_type_unqual, type_char_ptr_ptr)) {
				warningf(WARN_MAIN, pos, "second argument of 'main' should be 'char**', but is '%T'", parm->type);
			}
			parm = parm->next;
			if (parm != NULL) {
				type_t *const third_type = skip_typeref(parm->type);
				type_t *const third_type_unqual
					= get_unqualified_type(third_type);
				if (!types_compatible(third_type_unqual, type_char_ptr_ptr)) {
					warningf(WARN_MAIN, pos, "third argument of 'main' should be 'char**', but is '%T'", parm->type);
				}
				parm = parm->next;
				if (parm != NULL)
					goto warn_arg_count;
			}
		} else {
warn_arg_count:
			warningf(WARN_MAIN, pos, "'main' takes only zero, two or three arguments");
		}
	}
}

/**
 * Check if a symbol is the equal to "main".
 */
static bool is_sym_main(const symbol_t *const sym)
{
	return streq(sym->string, "main");
}

static void error_redefined_as_different_kind(const source_position_t *pos,
		const entity_t *old, entity_kind_t new_kind)
{
	char              const *const what = get_entity_kind_name(new_kind);
	source_position_t const *const ppos = &old->base.source_position;
	errorf(pos, "redeclaration of '%N' as %s (declared %P)", old, what, ppos);
}

static bool is_entity_valid(entity_t *const ent)
{
	if (is_declaration(ent)) {
		return is_type_valid(skip_typeref(ent->declaration.type));
	} else if (ent->kind == ENTITY_TYPEDEF) {
		return is_type_valid(skip_typeref(ent->typedefe.type));
	}
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
 * test wether new_list contains any attributes not included in old_list
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

/**
 * record entities for the NAMESPACE_NORMAL, and produce error messages/warnings
 * for various problems that occur for multiple definitions
 */
entity_t *record_entity(entity_t *entity, const bool is_definition)
{
	const symbol_t *const    symbol  = entity->base.symbol;
	const namespace_tag_t    namespc = (namespace_tag_t)entity->base.namespc;
	const source_position_t *pos     = &entity->base.source_position;

	/* can happen in error cases */
	if (symbol == NULL)
		return entity;

	entity_t *const previous_entity = get_entity(symbol, namespc);
	/* pushing the same entity twice will break the stack structure */
	assert(previous_entity != entity);

	if (entity->kind == ENTITY_FUNCTION) {
		type_t *const orig_type = entity->declaration.type;
		type_t *const type      = skip_typeref(orig_type);

		assert(is_type_function(type));
		if (type->function.unspecified_parameters &&
		    previous_entity == NULL               &&
		    !entity->declaration.implicit) {
			warningf(WARN_STRICT_PROTOTYPES, pos, "function declaration '%#N' is not a prototype", entity);
		}

		if (current_scope == file_scope && is_sym_main(symbol)) {
			check_main(entity);
		}
	}

	if (is_declaration(entity)                                    &&
	    entity->declaration.storage_class == STORAGE_CLASS_EXTERN &&
	    current_scope != file_scope                               &&
	    !entity->declaration.implicit) {
		warningf(WARN_NESTED_EXTERNS, pos, "nested extern declaration of '%#N'", entity);
	}

	if (previous_entity != NULL) {
		source_position_t const *const ppos = &previous_entity->base.source_position;

		if (previous_entity->base.parent_scope == &current_function->parameters &&
				previous_entity->base.parent_scope->depth + 1 == current_scope->depth) {
			assert(previous_entity->kind == ENTITY_PARAMETER);
			errorf(pos, "declaration of '%N' redeclares the '%N' (declared %P)", entity, previous_entity, ppos);
			goto finish;
		}

		if (previous_entity->base.parent_scope == current_scope) {
			if (previous_entity->kind != entity->kind) {
				if (is_entity_valid(previous_entity) && is_entity_valid(entity)) {
					error_redefined_as_different_kind(pos, previous_entity,
							entity->kind);
				}
				goto finish;
			}
			if (previous_entity->kind == ENTITY_ENUM_VALUE) {
				errorf(pos, "redeclaration of '%N' (declared %P)", entity, ppos);
				goto finish;
			}
			if (previous_entity->kind == ENTITY_TYPEDEF) {
				type_t *const type      = skip_typeref(entity->typedefe.type);
				type_t *const prev_type
					= skip_typeref(previous_entity->typedefe.type);
				if (c_mode & _CXX) {
					/* C++ allows double typedef if they are identical
					 * (after skipping typedefs) */
					if (type == prev_type)
						goto finish;
				} else {
					/* GCC extension: redef in system headers is allowed */
					if ((pos->is_system_header || ppos->is_system_header) &&
					    types_compatible(type, prev_type))
						goto finish;
				}
				errorf(pos, "redefinition of '%N' (declared %P)",
				       entity, ppos);
				goto finish;
			}

			/* at this point we should have only VARIABLES or FUNCTIONS */
			assert(is_declaration(previous_entity) && is_declaration(entity));

			declaration_t *const prev_decl = &previous_entity->declaration;
			declaration_t *const decl      = &entity->declaration;

			/* can happen for K&R style declarations */
			if (prev_decl->type       == NULL             &&
					previous_entity->kind == ENTITY_PARAMETER &&
					entity->kind          == ENTITY_PARAMETER) {
				prev_decl->type                   = decl->type;
				prev_decl->storage_class          = decl->storage_class;
				prev_decl->declared_storage_class = decl->declared_storage_class;
				prev_decl->modifiers              = decl->modifiers;
				return previous_entity;
			}

			type_t *const type      = skip_typeref(decl->type);
			type_t *const prev_type = skip_typeref(prev_decl->type);

			if (!types_compatible(type, prev_type)) {
				errorf(pos, "declaration '%#N' is incompatible with '%#N' (declared %P)", entity, previous_entity, ppos);
			} else {
				unsigned old_storage_class = prev_decl->storage_class;

				if (is_definition                     &&
						!prev_decl->used                  &&
						!(prev_decl->modifiers & DM_USED) &&
						prev_decl->storage_class == STORAGE_CLASS_STATIC) {
					warningf(WARN_REDUNDANT_DECLS, ppos, "unnecessary static forward declaration for '%#N'", previous_entity);
				}

				storage_class_t new_storage_class = decl->storage_class;

				/* pretend no storage class means extern for function
				 * declarations (except if the previous declaration is neither
				 * none nor extern) */
				if (entity->kind == ENTITY_FUNCTION) {
					/* the previous declaration could have unspecified parameters or
					 * be a typedef, so use the new type */
					if (prev_type->function.unspecified_parameters || is_definition)
						prev_decl->type = type;

					switch (old_storage_class) {
						case STORAGE_CLASS_NONE:
							old_storage_class = STORAGE_CLASS_EXTERN;
							/* FALLTHROUGH */

						case STORAGE_CLASS_EXTERN:
							if (is_definition) {
								if (prev_type->function.unspecified_parameters && !is_sym_main(symbol)) {
									warningf(WARN_MISSING_PROTOTYPES, pos, "no previous prototype for '%#N'", entity);
								}
							} else if (new_storage_class == STORAGE_CLASS_NONE) {
								new_storage_class = STORAGE_CLASS_EXTERN;
							}
							break;

						default:
							break;
					}
				} else if (is_type_incomplete(prev_type)) {
					prev_decl->type = type;
				}

				if (old_storage_class == STORAGE_CLASS_EXTERN &&
						new_storage_class == STORAGE_CLASS_EXTERN) {

warn_redundant_declaration: ;
					bool has_new_attrs
						= has_new_attributes(prev_decl->attributes,
						                     decl->attributes);
					if (has_new_attrs) {
						merge_in_attributes(decl, prev_decl->attributes);
					} else if (!is_definition        &&
							is_type_valid(prev_type) &&
							!pos->is_system_header) {
						warningf(WARN_REDUNDANT_DECLS, pos, "redundant declaration for '%Y' (declared %P)", symbol, ppos);
					}
				} else if (current_function == NULL) {
					if (old_storage_class != STORAGE_CLASS_STATIC &&
							new_storage_class == STORAGE_CLASS_STATIC) {
						errorf(pos, "static declaration of '%Y' follows non-static declaration (declared %P)", symbol, ppos);
					} else if (old_storage_class == STORAGE_CLASS_EXTERN) {
						prev_decl->storage_class          = STORAGE_CLASS_NONE;
						prev_decl->declared_storage_class = STORAGE_CLASS_NONE;
					} else {
						/* ISO/IEC 14882:1998(E) §C.1.2:1 */
						if (c_mode & _CXX)
							goto error_redeclaration;
						goto warn_redundant_declaration;
					}
				} else if (is_type_valid(prev_type)) {
					if (old_storage_class == new_storage_class) {
error_redeclaration:
						errorf(pos, "redeclaration of '%Y' (declared %P)", symbol, ppos);
					} else {
						errorf(pos, "redeclaration of '%Y' with different linkage (declared %P)", symbol, ppos);
					}
				}
			}

			prev_decl->modifiers |= decl->modifiers;
			if (entity->kind == ENTITY_FUNCTION) {
				previous_entity->function.is_inline |= entity->function.is_inline;
			}
			return previous_entity;
		}

		warning_t why;
		if (is_warn_on(why = WARN_SHADOW) ||
		    (is_warn_on(why = WARN_SHADOW_LOCAL) && previous_entity->base.parent_scope != file_scope)) {
			char const *const what = get_entity_kind_name(previous_entity->kind);
			warningf(why, pos, "'%N' shadows %s (declared %P)", entity, what, ppos);
		}
	}

	if (entity->kind == ENTITY_FUNCTION) {
		if (is_definition &&
				entity->declaration.storage_class != STORAGE_CLASS_STATIC &&
				!is_sym_main(symbol)) {
			if (is_warn_on(WARN_MISSING_PROTOTYPES)) {
				warningf(WARN_MISSING_PROTOTYPES, pos, "no previous prototype for '%#N'", entity);
			} else {
				goto warn_missing_declaration;
			}
		}
	} else if (entity->kind == ENTITY_VARIABLE) {
		if (current_scope                     == file_scope &&
				entity->declaration.storage_class == STORAGE_CLASS_NONE &&
				!entity->declaration.implicit) {
warn_missing_declaration:
			warningf(WARN_MISSING_DECLARATIONS, pos, "no previous declaration for '%#N'", entity);
		}
	}

finish:
	assert(entity->base.parent_scope == NULL);
	assert(current_scope != NULL);

	entity->base.parent_scope = current_scope;
	environment_push(entity);
	append_entity(current_scope, entity);

	return entity;
}

static void parser_error_multiple_definition(entity_t *entity,
		const source_position_t *source_position)
{
	errorf(source_position, "multiple definition of '%Y' (declared %P)",
	       entity->base.symbol, &entity->base.source_position);
}

static bool is_declaration_specifier(const token_t *token)
{
	switch (token->kind) {
		DECLARATION_START
			return true;
		case T_IDENTIFIER:
			return is_typedef_symbol(token->identifier.symbol);

		default:
			return false;
	}
}

static void parse_init_declarator_rest(entity_t *entity)
{
	type_t *orig_type = type_error_type;

	if (entity->base.kind == ENTITY_TYPEDEF) {
		source_position_t const *const pos = &entity->base.source_position;
		errorf(pos, "'%N' is initialized (use __typeof__ instead)", entity);
	} else {
		assert(is_declaration(entity));
		orig_type = entity->declaration.type;
	}

	type_t *type = skip_typeref(orig_type);

	if (entity->kind == ENTITY_VARIABLE
			&& entity->variable.initializer != NULL) {
		parser_error_multiple_definition(entity, HERE);
	}
	eat('=');

	declaration_t *const declaration = &entity->declaration;
	bool must_be_constant = false;
	if (declaration->storage_class == STORAGE_CLASS_STATIC ||
	    entity->base.parent_scope  == file_scope) {
		must_be_constant = true;
	}

	if (is_type_function(type)) {
		source_position_t const *const pos = &entity->base.source_position;
		errorf(pos, "'%N' is initialized like a variable", entity);
		orig_type = type_error_type;
	}

	parse_initializer_env_t env;
	env.type             = orig_type;
	env.must_be_constant = must_be_constant;
	env.entity           = entity;

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

	source_position_t const *const pos = &specifiers->source_position;
	if (specifiers->storage_class != STORAGE_CLASS_NONE ||
			specifiers->thread_local) {
		warningf(WARN_OTHER, pos, "useless storage class in empty declaration");
	}

	type_t *type = specifiers->type;
	switch (type->kind) {
		case TYPE_COMPOUND_STRUCT:
		case TYPE_COMPOUND_UNION: {
			if (type->compound.compound->base.symbol == NULL) {
				warningf(WARN_OTHER, pos, "unnamed struct/union that defines no instances");
			}
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
	if (decl->storage_class == STORAGE_CLASS_EXTERN ||
			decl->storage_class == STORAGE_CLASS_STATIC)
		return;

	type_t *const type = skip_typeref(decl->type);
	if (!is_type_incomplete(type))
		return;

	/* §6.9.2:2 and §6.9.2:5: At the end of the translation incomplete arrays
	 * are given length one. */
	if (is_type_array(type) && ent->base.parent_scope == file_scope) {
		ARR_APP1(declaration_t*, incomplete_arrays, decl);
		return;
	}

	errorf(&ent->base.source_position, "variable '%#N' has incomplete type", ent);
}


static void parse_declaration_rest(entity_t *ndeclaration,
		const declaration_specifiers_t *specifiers,
		parsed_declaration_func         finished_declaration,
		declarator_flags_t              flags)
{
	add_anchor_token(';');
	add_anchor_token(',');
	while (true) {
		entity_t *entity = finished_declaration(ndeclaration, token.kind == '=');

		if (token.kind == '=') {
			parse_init_declarator_rest(entity);
		} else if (entity->kind == ENTITY_VARIABLE) {
			/* ISO/IEC 14882:1998(E) §8.5.3:3  The initializer can be omitted
			 * [...] where the extern specifier is explicitly used. */
			declaration_t *decl = &entity->declaration;
			if (decl->storage_class != STORAGE_CLASS_EXTERN) {
				type_t *type = decl->type;
				if (is_type_reference(skip_typeref(type))) {
					source_position_t const *const pos = &entity->base.source_position;
					errorf(pos, "reference '%#N' must be initialized", entity);
				}
			}
		}

		check_variable_type_complete(entity);

		if (!next_if(','))
			break;

		add_anchor_token('=');
		ndeclaration = parse_declarator(specifiers, flags);
		rem_anchor_token('=');
	}
	expect(';', end_error);

end_error:
	anonymous_entity = NULL;
	rem_anchor_token(';');
	rem_anchor_token(',');
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
		errorf(&entity->base.source_position, "expected declaration of a function parameter, found '%Y'",
		       symbol);
		return entity;
	}

	if (is_definition) {
		errorf(HERE, "'%N' is initialised", entity);
	}

	return record_entity(entity, false);
}

static void parse_declaration(parsed_declaration_func finished_declaration,
                              declarator_flags_t      flags)
{
	add_anchor_token(';');
	declaration_specifiers_t specifiers;
	parse_declaration_specifiers(&specifiers);
	rem_anchor_token(';');

	if (token.kind == ';') {
		parse_anonymous_declaration_rest(&specifiers);
	} else {
		entity_t *entity = parse_declarator(&specifiers, flags);
		parse_declaration_rest(entity, &specifiers, finished_declaration, flags);
	}
}

/* §6.5.2.2:6 */
static type_t *get_default_promoted_type(type_t *orig_type)
{
	type_t *result = orig_type;

	type_t *type = skip_typeref(orig_type);
	if (is_type_integer(type)) {
		result = promote_integer(type);
	} else if (is_type_atomic(type, ATOMIC_TYPE_FLOAT)) {
		result = type_double;
	}

	return result;
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

	entity_t *parameter = entity->function.parameters.entities;
	for ( ; parameter != NULL; parameter = parameter->base.next) {
		assert(parameter->base.parent_scope == NULL);
		parameter->base.parent_scope = current_scope;
		environment_push(parameter);
	}

	/* parse declaration list */
	for (;;) {
		switch (token.kind) {
			DECLARATION_START
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
	type_t *new_type = duplicate_type(type);

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
	parameter = entity->function.parameters.entities;
	for (; parameter != NULL; parameter = parameter->base.next,
			proto_parameter =
				proto_parameter == NULL ? NULL : proto_parameter->next) {
		if (parameter->kind != ENTITY_PARAMETER)
			continue;

		type_t *parameter_type = parameter->declaration.type;
		if (parameter_type == NULL) {
			source_position_t const* const pos = &parameter->base.source_position;
			if (strict_mode) {
				errorf(pos, "no type specified for function '%N'", parameter);
				parameter_type = type_error_type;
			} else {
				warningf(WARN_IMPLICIT_INT, pos, "no type specified for function parameter '%N', using 'int'", parameter);
				parameter_type = type_int;
			}
			parameter->declaration.type = parameter_type;
		}

		semantic_parameter_incomplete(parameter);

		/* we need the default promoted types for the function type */
		type_t *not_promoted = parameter_type;
		parameter_type       = get_default_promoted_type(parameter_type);

		/* gcc special: if the type of the prototype matches the unpromoted
		 * type don't promote */
		if (!strict_mode && proto_parameter != NULL) {
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
	new_type = identify_new_type(new_type);

	if (need_incompatible_warning) {
		symbol_t          const *const sym  = entity->base.symbol;
		source_position_t const *const pos  = &entity->base.source_position;
		source_position_t const *const ppos = &proto_type->base.source_position;
		warningf(WARN_OTHER, pos, "declaration '%#N' is incompatible with '%#T' (declared %P)", proto_type, new_type, sym, ppos);
	}
	entity->declaration.type = new_type;

	rem_anchor_token('{');
}

static bool first_err = true;

/**
 * When called with first_err set, prints the name of the current function,
 * else does noting.
 */
static void print_in_function(void)
{
	if (first_err) {
		first_err = false;
		char const *const file = current_function->base.base.source_position.input_name;
		diagnosticf("%s: In '%N':\n", file, (entity_t const*)current_function);
	}
}

/**
 * Check if all labels are defined in the current function.
 * Check if all labels are used in the current function.
 */
static void check_labels(void)
{
	for (const goto_statement_t *goto_statement = goto_first;
	    goto_statement != NULL;
	    goto_statement = goto_statement->next) {
		label_t *label = goto_statement->label;
		if (label->base.source_position.input_name == NULL) {
			print_in_function();
			source_position_t const *const pos = &goto_statement->base.source_position;
			errorf(pos, "'%N' used but not defined", (entity_t const*)label);
		 }
	}

	if (is_warn_on(WARN_UNUSED_LABEL)) {
		for (const label_statement_t *label_statement = label_first;
			 label_statement != NULL;
			 label_statement = label_statement->next) {
			label_t *label = label_statement->label;

			if (! label->used) {
				print_in_function();
				source_position_t const *const pos = &label_statement->base.source_position;
				warningf(WARN_UNUSED_LABEL, pos, "'%N' defined but not used", (entity_t const*)label);
			}
		}
	}
}

static void warn_unused_entity(warning_t const why, entity_t *entity, entity_t *const last)
{
	entity_t const *const end = last != NULL ? last->base.next : NULL;
	for (; entity != end; entity = entity->base.next) {
		if (!is_declaration(entity))
			continue;

		declaration_t *declaration = &entity->declaration;
		if (declaration->implicit)
			continue;

		if (!declaration->used) {
			print_in_function();
			warningf(why, &entity->base.source_position, "'%N' is unused", entity);
		} else if (entity->kind == ENTITY_VARIABLE && !entity->variable.read) {
			print_in_function();
			warningf(why, &entity->base.source_position, "'%N' is never read", entity);
		}
	}
}

static void check_unused_variables(statement_t *const stmt, void *const env)
{
	(void)env;

	switch (stmt->kind) {
		case STATEMENT_DECLARATION: {
			declaration_statement_t const *const decls = &stmt->declaration;
			warn_unused_entity(WARN_UNUSED_VARIABLE, decls->declarations_begin, decls->declarations_end);
			return;
		}

		case STATEMENT_FOR:
			warn_unused_entity(WARN_UNUSED_VARIABLE, stmt->fors.scope.entities, NULL);
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

		/* do not issue unused warnings for main */
		if (!is_sym_main(current_function->base.base.symbol)) {
			warn_unused_entity(WARN_UNUSED_PARAMETER, scope->entities, NULL);
		}
	}
	if (is_warn_on(WARN_UNUSED_VARIABLE)) {
		walk_statements(current_function->statement, check_unused_variables,
		                NULL);
	}
}

static int determine_truth(expression_t const* const cond)
{
	return
		is_constant_expression(cond) != EXPR_CLASS_CONSTANT ? 0 :
		fold_constant_to_bool(cond)                         ? 1 :
		-1;
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

			for (call_argument_t const* arg = expr->call.arguments; arg != NULL; arg = arg->next) {
				if (!expression_returns(arg->expression))
					return false;
			}

			return true;
		}

		case EXPR_REFERENCE:
		case EXPR_ENUM_CONSTANT:
		case EXPR_LITERAL_CASES:
		case EXPR_STRING_LITERAL:
		case EXPR_WIDE_STRING_LITERAL:
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
			return
				expression_returns(expr->array_access.array_ref) &&
				expression_returns(expr->array_access.index);

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
			return
				expression_returns(expr->binary.left) &&
				expression_returns(expr->binary.right);
	}

	panic("unhandled expression");
}

static bool initializer_returns(initializer_t const *const init)
{
	switch (init->kind) {
		case INITIALIZER_VALUE:
			return expression_returns(init->value.value);

		case INITIALIZER_LIST: {
			initializer_t * const*       i       = init->list.initializers;
			initializer_t * const* const end     = i + init->list.len;
			bool                         returns = true;
			for (; i != end; ++i) {
				if (!initializer_returns(*i))
					returns = false;
			}
			return returns;
		}

		case INITIALIZER_STRING:
		case INITIALIZER_WIDE_STRING:
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
					if (ent->kind                 == ENTITY_VARIABLE &&
					    ent->variable.initializer != NULL            &&
					    !initializer_returns(ent->variable.initializer)) {
						return;
					}
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

			if (is_constant_expression(expr) == EXPR_CLASS_CONSTANT) {
				long                    const val      = fold_constant_to_int(expr);
				case_label_statement_t *      defaults = NULL;
				for (case_label_statement_t *i = switchs->first_case; i != NULL; i = i->next) {
					if (i->expression == NULL) {
						defaults = i;
						continue;
					}

					if (i->first_case <= val && val <= i->last_case) {
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
				for (case_label_statement_t *i = switchs->first_case; i != NULL; i = i->next) {
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
					case STATEMENT_WHILE:    goto continue_while;
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
					case STATEMENT_WHILE:
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

		case STATEMENT_WHILE: {
			while_statement_t const *const whiles = &stmt->whiles;
			expression_t      const *const cond   = whiles->condition;

			if (!expression_returns(cond))
				return;

			int const val = determine_truth(cond);

			if (val >= 0)
				check_reachable(whiles->body);

			if (val > 0)
				return;

			next = stmt->base.next;
			break;
		}

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

		case STATEMENT_MS_TRY: {
			ms_try_statement_t const *const ms_try = &stmt->ms_try;
			check_reachable(ms_try->try_statement);
			next = ms_try->final_statement;
			break;
		}

		case STATEMENT_LEAVE: {
			statement_t *parent = stmt;
			for (;;) {
				parent = parent->base.parent;
				if (parent == NULL) /* __leave not within __try */
					return;

				if (parent->kind == STATEMENT_MS_TRY) {
					last = parent;
					next = parent->ms_try.final_statement;
					break;
				}
			}
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
			if (!is_type_void(ret) &&
			    is_type_valid(ret) &&
			    !is_sym_main(current_function->base.base.symbol)) {
				source_position_t const *const pos = &stmt->base.source_position;
				warningf(WARN_RETURN_TYPE, pos, "control reaches end of non-void function");
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
			case STATEMENT_LEAVE:
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

			case STATEMENT_WHILE: {
continue_while:
				if (next->base.reachable)
					return;
				next->base.reachable = true;

				while_statement_t const *const whiles = &next->whiles;
				expression_t      const *const cond   = whiles->condition;

				if (!expression_returns(cond))
					return;

				int const val = determine_truth(cond);

				if (val >= 0)
					check_reachable(whiles->body);

				if (val > 0)
					return;

				last = next;
				next = next->base.next;
				break;
			}

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

			case STATEMENT_MS_TRY:
				last = next;
				next = next->ms_try.final_statement;
				break;
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
					source_position_t const *const pos = &cond->base.source_position;
					warningf(WARN_UNREACHABLE_CODE, pos, "condition of do-while-loop is unreachable");
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
					source_position_t const *const pos = &fors->initialisation->base.source_position;
					warningf(WARN_UNREACHABLE_CODE, pos, "initialisation of for-statement is unreachable");
				}

				if (!fors->condition_reachable && fors->condition != NULL) {
					source_position_t const *const pos = &fors->condition->base.source_position;
					warningf(WARN_UNREACHABLE_CODE, pos, "condition of for-statement is unreachable");
				}

				if (!fors->step_reachable && fors->step != NULL) {
					source_position_t const *const pos = &fors->step->base.source_position;
					warningf(WARN_UNREACHABLE_CODE, pos, "step of for-statement is unreachable");
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
					if (ent->kind                 == ENTITY_VARIABLE &&
							ent->variable.initializer != NULL) {
						goto warn_unreachable;
					}
					if (ent == last)
						return;
				}
			}
		}

		default:
warn_unreachable:
			if (!stmt->base.reachable) {
				source_position_t const *const pos = &stmt->base.source_position;
				warningf(WARN_UNREACHABLE_CODE, pos, "statement is unreachable");
			}
			return;
	}
}

static bool is_main(entity_t *entity)
{
	static symbol_t *sym_main = NULL;
	if (sym_main == NULL) {
		sym_main = symbol_table_insert("main");
	}

	if (entity->base.symbol != sym_main)
		return false;
	/* must be in outermost scope */
	if (entity->base.parent_scope != file_scope)
		return false;

	return true;
}

static void parse_external_declaration(void)
{
	/* function-definitions and declarations both start with declaration
	 * specifiers */
	add_anchor_token(';');
	declaration_specifiers_t specifiers;
	parse_declaration_specifiers(&specifiers);
	rem_anchor_token(';');

	/* must be a declaration */
	if (token.kind == ';') {
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
	}

	/* must be a function definition */
	parse_kr_declaration_list(ndeclaration);

	if (token.kind != '{') {
		parse_error_expected("while parsing function definition", '{', NULL);
		eat_until_matching_token(';');
		return;
	}

	assert(is_declaration(ndeclaration));
	type_t *const orig_type = ndeclaration->declaration.type;
	type_t *      type      = skip_typeref(orig_type);

	if (!is_type_function(type)) {
		if (is_type_valid(type)) {
			errorf(HERE, "declarator '%#N' has a body but is not a function type", ndeclaration);
		}
		eat_block();
		return;
	}

	source_position_t const *const pos = &ndeclaration->base.source_position;
	if (is_typeref(orig_type)) {
		/* §6.9.1:2 */
		errorf(pos, "type of function definition '%#N' is a typedef", ndeclaration);
	}

	if (is_type_compound(skip_typeref(type->function.return_type))) {
		warningf(WARN_AGGREGATE_RETURN, pos, "'%N' returns an aggregate", ndeclaration);
	}
	if (type->function.unspecified_parameters) {
		warningf(WARN_OLD_STYLE_DEFINITION, pos, "old-style definition of '%N'", ndeclaration);
	} else {
		warningf(WARN_TRADITIONAL, pos, "traditional C rejects ISO C style definition of '%N'", ndeclaration);
	}

	/* §6.7.5.3:14 a function definition with () means no
	 * parameters (and not unspecified parameters) */
	if (type->function.unspecified_parameters &&
			type->function.parameters == NULL) {
		type_t *copy                          = duplicate_type(type);
		copy->function.unspecified_parameters = false;
		type                                  = identify_new_type(copy);

		ndeclaration->declaration.type = type;
	}

	entity_t *const entity = record_entity(ndeclaration, true);
	assert(entity->kind == ENTITY_FUNCTION);
	assert(ndeclaration->kind == ENTITY_FUNCTION);

	function_t *const function = &entity->function;
	if (ndeclaration != entity) {
		function->parameters = ndeclaration->function.parameters;
	}
	assert(is_declaration(entity));
	type = skip_typeref(entity->declaration.type);

	PUSH_SCOPE(&function->parameters);

	entity_t *parameter = function->parameters.entities;
	for (; parameter != NULL; parameter = parameter->base.next) {
		if (parameter->base.parent_scope == &ndeclaration->function.parameters) {
			parameter->base.parent_scope = current_scope;
		}
		assert(parameter->base.parent_scope == NULL
				|| parameter->base.parent_scope == current_scope);
		parameter->base.parent_scope = current_scope;
		if (parameter->base.symbol == NULL) {
			errorf(&parameter->base.source_position, "parameter name omitted");
			continue;
		}
		environment_push(parameter);
	}

	if (function->statement != NULL) {
		parser_error_multiple_definition(entity, HERE);
		eat_block();
	} else {
		/* parse function body */
		int         label_stack_top      = label_top();
		function_t *old_current_function = current_function;
		entity_t   *old_current_entity   = current_entity;
		current_function                 = function;
		current_entity                   = entity;
		PUSH_PARENT(NULL);

		goto_first   = NULL;
		goto_anchor  = &goto_first;
		label_first  = NULL;
		label_anchor = &label_first;

		statement_t *const body = parse_compound_statement(false);
		function->statement = body;
		first_err = true;
		check_labels();
		check_declarations();
		if (is_warn_on(WARN_RETURN_TYPE)      ||
		    is_warn_on(WARN_UNREACHABLE_CODE) ||
		    (is_warn_on(WARN_MISSING_NORETURN) && !(function->base.modifiers & DM_NORETURN))) {
			noreturn_candidate = true;
			check_reachable(body);
			if (is_warn_on(WARN_UNREACHABLE_CODE))
				walk_statements(body, check_unreachable, NULL);
			if (noreturn_candidate &&
			    !(function->base.modifiers & DM_NORETURN)) {
				source_position_t const *const pos = &body->base.source_position;
				warningf(WARN_MISSING_NORETURN, pos, "function '%#N' is candidate for attribute 'noreturn'", entity);
			}
		}

		if (is_main(entity) && enable_main_collect2_hack)
			prepare_main_collect2(entity);

		POP_PARENT();
		assert(current_function == function);
		assert(current_entity   == entity);
		current_entity   = old_current_entity;
		current_function = old_current_function;
		label_pop_to(label_stack_top);
	}

	POP_SCOPE();
}

static entity_t *find_compound_entry(compound_t *compound, symbol_t *symbol)
{
	entity_t *iter = compound->members.entities;
	for (; iter != NULL; iter = iter->base.next) {
		if (iter->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		if (iter->base.symbol == symbol) {
			return iter;
		} else if (iter->base.symbol == NULL) {
			/* search in anonymous structs and unions */
			type_t *type = skip_typeref(iter->declaration.type);
			if (is_type_compound(type)) {
				if (find_compound_entry(type->compound.compound, symbol)
						!= NULL)
					return iter;
			}
			continue;
		}
	}

	return NULL;
}

static void check_deprecated(const source_position_t *source_position,
                             const entity_t *entity)
{
	if (!is_declaration(entity))
		return;
	if ((entity->declaration.modifiers & DM_DEPRECATED) == 0)
		return;

	source_position_t const *const epos = &entity->base.source_position;
	char              const *const msg  = get_deprecated_string(entity->declaration.attributes);
	if (msg != NULL) {
		warningf(WARN_DEPRECATED_DECLARATIONS, source_position, "'%N' is deprecated (declared %P): \"%s\"", entity, epos, msg);
	} else {
		warningf(WARN_DEPRECATED_DECLARATIONS, source_position, "'%N' is deprecated (declared %P)", entity, epos);
	}
}


static expression_t *create_select(const source_position_t *pos,
                                   expression_t *addr,
                                   type_qualifiers_t qualifiers,
								   entity_t *entry)
{
	assert(entry->kind == ENTITY_COMPOUND_MEMBER);

	check_deprecated(pos, entry);

	expression_t *select          = allocate_expression_zero(EXPR_SELECT);
	select->select.compound       = addr;
	select->select.compound_entry = entry;

	type_t *entry_type = entry->declaration.type;
	type_t *res_type   = get_qualified_type(entry_type, qualifiers);

	/* bitfields need special treatment */
	if (entry->compound_member.bitfield) {
		unsigned bit_size = entry->compound_member.bit_size;
		/* if fewer bits than an int, convert to int (see §6.3.1.1) */
		if (bit_size < get_atomic_type_size(ATOMIC_TYPE_INT) * BITS_PER_BYTE) {
			res_type = type_int;
		}
	}

	/* we always do the auto-type conversions; the & and sizeof parser contains
	 * code to revert this! */
	select->base.type = automatic_type_conversion(res_type);


	return select;
}

/**
 * Find entry with symbol in compound. Search anonymous structs and unions and
 * creates implicit select expressions for them.
 * Returns the adress for the innermost compound.
 */
static expression_t *find_create_select(const source_position_t *pos,
                                        expression_t *addr,
                                        type_qualifiers_t qualifiers,
                                        compound_t *compound, symbol_t *symbol)
{
	entity_t *iter = compound->members.entities;
	for (; iter != NULL; iter = iter->base.next) {
		if (iter->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		symbol_t *iter_symbol = iter->base.symbol;
		if (iter_symbol == NULL) {
			type_t *type = iter->declaration.type;
			if (type->kind != TYPE_COMPOUND_STRUCT
					&& type->kind != TYPE_COMPOUND_UNION)
				continue;

			compound_t *sub_compound = type->compound.compound;

			if (find_compound_entry(sub_compound, symbol) == NULL)
				continue;

			expression_t *sub_addr = create_select(pos, addr, qualifiers, iter);
			sub_addr->base.source_position = *pos;
			sub_addr->base.implicit        = true;
			return find_create_select(pos, sub_addr, qualifiers, sub_compound,
			                          symbol);
		}

		if (iter_symbol == symbol) {
			return create_select(pos, addr, qualifiers, iter);
		}
	}

	return NULL;
}

static void parse_bitfield_member(entity_t *entity)
{
	eat(':');

	expression_t *size = parse_constant_expression();
	long          size_long;

	assert(entity->kind == ENTITY_COMPOUND_MEMBER);
	type_t *type = entity->declaration.type;
	if (!is_type_integer(skip_typeref(type))) {
		errorf(HERE, "bitfield base type '%T' is not an integer type",
			   type);
	}

	if (is_constant_expression(size) != EXPR_CLASS_CONSTANT) {
		/* error already reported by parse_constant_expression */
		size_long = get_type_size(type) * 8;
	} else {
		size_long = fold_constant_to_int(size);

		const symbol_t *symbol = entity->base.symbol;
		const symbol_t *user_symbol
			= symbol == NULL ? sym_anonymous : symbol;
		unsigned bit_size = get_type_size(type) * 8;
		if (size_long < 0) {
			errorf(HERE, "negative width in bit-field '%Y'", user_symbol);
		} else if (size_long == 0 && symbol != NULL) {
			errorf(HERE, "zero width for bit-field '%Y'", user_symbol);
		} else if (bit_size > 0 && (unsigned)size_long > bit_size) {
			errorf(HERE, "width of bitfield '%Y' exceeds its type",
				   user_symbol);
		} else {
			/* hope that people don't invent crazy types with more bits
			 * than our struct can hold */
			assert(size_long <
				   (1 << sizeof(entity->compound_member.bit_size)*8));
		}
	}

	entity->compound_member.bitfield = true;
	entity->compound_member.bit_size = (unsigned char)size_long;
}

static void parse_compound_declarators(compound_t *compound,
		const declaration_specifiers_t *specifiers)
{
	do {
		entity_t *entity;

		if (token.kind == ':') {
			/* anonymous bitfield */
			type_t *type = specifiers->type;
			entity_t *entity = allocate_entity_zero(ENTITY_COMPOUND_MEMBER,
			                                        NAMESPACE_NORMAL, NULL);
			entity->base.source_position               = *HERE;
			entity->declaration.declared_storage_class = STORAGE_CLASS_NONE;
			entity->declaration.storage_class          = STORAGE_CLASS_NONE;
			entity->declaration.type                   = type;

			parse_bitfield_member(entity);

			attribute_t  *attributes = parse_attributes(NULL);
			attribute_t **anchor     = &attributes;
			while (*anchor != NULL)
				anchor = &(*anchor)->next;
			*anchor = specifiers->attributes;
			if (attributes != NULL) {
				handle_entity_attributes(attributes, entity);
			}
			entity->declaration.attributes = attributes;

			append_entity(&compound->members, entity);
		} else {
			entity = parse_declarator(specifiers,
					DECL_MAY_BE_ABSTRACT | DECL_CREATE_COMPOUND_MEMBER);
			source_position_t const *const pos = &entity->base.source_position;
			if (entity->kind == ENTITY_TYPEDEF) {
				errorf(pos, "typedef not allowed as compound member");
			} else {
				assert(entity->kind == ENTITY_COMPOUND_MEMBER);

				/* make sure we don't define a symbol multiple times */
				symbol_t *symbol = entity->base.symbol;
				if (symbol != NULL) {
					entity_t *prev = find_compound_entry(compound, symbol);
					if (prev != NULL) {
						source_position_t const *const ppos = &prev->base.source_position;
						errorf(pos, "multiple declarations of symbol '%Y' (declared %P)", symbol, ppos);
					}
				}

				if (token.kind == ':') {
					parse_bitfield_member(entity);

					attribute_t *attributes = parse_attributes(NULL);
					handle_entity_attributes(attributes, entity);
				} else {
					type_t *orig_type = entity->declaration.type;
					type_t *type      = skip_typeref(orig_type);
					if (is_type_function(type)) {
						errorf(pos, "'%N' must not have function type '%T'", entity, orig_type);
					} else if (is_type_incomplete(type)) {
						/* §6.7.2.1:16 flexible array member */
						if (!is_type_array(type)       ||
								token.kind          != ';' ||
								look_ahead(1)->kind != '}') {
							errorf(pos, "'%N' has incomplete type '%T'", entity, orig_type);
						} else if (compound->members.entities == NULL) {
							errorf(pos, "flexible array member in otherwise empty struct");
						}
					}
				}

				append_entity(&compound->members, entity);
			}
		}
	} while (next_if(','));
	expect(';', end_error);

end_error:
	anonymous_entity = NULL;
}

static void parse_compound_type_entries(compound_t *compound)
{
	eat('{');
	add_anchor_token('}');

	for (;;) {
		switch (token.kind) {
			DECLARATION_START
			case T___extension__:
			case T_IDENTIFIER: {
				PUSH_EXTENSION();
				declaration_specifiers_t specifiers;
				parse_declaration_specifiers(&specifiers);
				parse_compound_declarators(compound, &specifiers);
				POP_EXTENSION();
				break;
			}

			default:
				rem_anchor_token('}');
				expect('}', end_error);
end_error:
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
			|| specifiers.thread_local) {
		/* TODO: improve error message, user does probably not know what a
		 * storage class is...
		 */
		errorf(&specifiers.source_position, "typename must not have a storage class");
	}

	type_t *result = parse_abstract_declarator(specifiers.type);

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
 * Prints an error message if an expression was expected but not read
 */
static expression_t *expected_expression_error(void)
{
	/* skip the error message if the error token was read */
	if (token.kind != T_ERROR) {
		errorf(HERE, "expected expression, got token %K", &token);
	}
	next_token();

	return create_error_expression();
}

static type_t *get_string_type(void)
{
	return is_warn_on(WARN_WRITE_STRINGS) ? type_const_char_ptr : type_char_ptr;
}

static type_t *get_wide_string_type(void)
{
	return is_warn_on(WARN_WRITE_STRINGS) ? type_const_wchar_t_ptr : type_wchar_t_ptr;
}

/**
 * Parse a string constant.
 */
static expression_t *parse_string_literal(void)
{
	source_position_t begin   = token.base.source_position;
	string_t          res     = token.string.string;
	bool              is_wide = (token.kind == T_WIDE_STRING_LITERAL);

	next_token();
	while (token.kind == T_STRING_LITERAL
			|| token.kind == T_WIDE_STRING_LITERAL) {
		warn_string_concat(&token.base.source_position);
		res = concat_strings(&res, &token.string.string);
		next_token();
		is_wide |= token.kind == T_WIDE_STRING_LITERAL;
	}

	expression_t *literal;
	if (is_wide) {
		literal = allocate_expression_zero(EXPR_WIDE_STRING_LITERAL);
		literal->base.type = get_wide_string_type();
	} else {
		literal = allocate_expression_zero(EXPR_STRING_LITERAL);
		literal->base.type = get_string_type();
	}
	literal->base.source_position = begin;
	literal->literal.value        = res;

	return literal;
}

/**
 * Parse a boolean constant.
 */
static expression_t *parse_boolean_literal(bool value)
{
	expression_t *literal = allocate_expression_zero(EXPR_LITERAL_BOOLEAN);
	literal->base.type           = type_bool;
	literal->literal.value.begin = value ? "true" : "false";
	literal->literal.value.size  = value ? 4 : 5;

	next_token();
	return literal;
}

static void warn_traditional_suffix(void)
{
	warningf(WARN_TRADITIONAL, HERE, "traditional C rejects the '%S' suffix",
	         &token.number.suffix);
}

static void check_integer_suffix(void)
{
	const string_t *suffix = &token.number.suffix;
	if (suffix->size == 0)
		return;

	bool not_traditional = false;
	const char *c = suffix->begin;
	if (*c == 'l' || *c == 'L') {
		++c;
		if (*c == *(c-1)) {
			not_traditional = true;
			++c;
			if (*c == 'u' || *c == 'U') {
				++c;
			}
		} else if (*c == 'u' || *c == 'U') {
			not_traditional = true;
			++c;
		}
	} else if (*c == 'u' || *c == 'U') {
		not_traditional = true;
		++c;
		if (*c == 'l' || *c == 'L') {
			++c;
			if (*c == *(c-1)) {
				++c;
			}
		}
	}
	if (*c != '\0') {
		errorf(&token.base.source_position,
		       "invalid suffix '%S' on integer constant", suffix);
	} else if (not_traditional) {
		warn_traditional_suffix();
	}
}

static type_t *check_floatingpoint_suffix(void)
{
	const string_t *suffix = &token.number.suffix;
	type_t         *type   = type_double;
	if (suffix->size == 0)
		return type;

	bool not_traditional = false;
	const char *c = suffix->begin;
	if (*c == 'f' || *c == 'F') {
		++c;
		type = type_float;
	} else if (*c == 'l' || *c == 'L') {
		++c;
		type = type_long_double;
	}
	if (*c != '\0') {
		errorf(&token.base.source_position,
		       "invalid suffix '%S' on floatingpoint constant", suffix);
	} else if (not_traditional) {
		warn_traditional_suffix();
	}

	return type;
}

/**
 * Parse an integer constant.
 */
static expression_t *parse_number_literal(void)
{
	expression_kind_t  kind;
	type_t            *type;

	switch (token.kind) {
	case T_INTEGER:
		kind = EXPR_LITERAL_INTEGER;
		check_integer_suffix();
		type = type_int;
		break;
	case T_INTEGER_OCTAL:
		kind = EXPR_LITERAL_INTEGER_OCTAL;
		check_integer_suffix();
		type = type_int;
		break;
	case T_INTEGER_HEXADECIMAL:
		kind = EXPR_LITERAL_INTEGER_HEXADECIMAL;
		check_integer_suffix();
		type = type_int;
		break;
	case T_FLOATINGPOINT:
		kind = EXPR_LITERAL_FLOATINGPOINT;
		type = check_floatingpoint_suffix();
		break;
	case T_FLOATINGPOINT_HEXADECIMAL:
		kind = EXPR_LITERAL_FLOATINGPOINT_HEXADECIMAL;
		type = check_floatingpoint_suffix();
		break;
	default:
		panic("unexpected token type in parse_number_literal");
	}

	expression_t *literal = allocate_expression_zero(kind);
	literal->base.type      = type;
	literal->literal.value  = token.number.number;
	literal->literal.suffix = token.number.suffix;
	next_token();

	/* integer type depends on the size of the number and the size
	 * representable by the types. The backend/codegeneration has to determine
	 * that
	 */
	determine_literal_type(&literal->literal);
	return literal;
}

/**
 * Parse a character constant.
 */
static expression_t *parse_character_constant(void)
{
	expression_t *literal = allocate_expression_zero(EXPR_LITERAL_CHARACTER);
	literal->base.type     = c_mode & _CXX ? type_char : type_int;
	literal->literal.value = token.string.string;

	size_t len = literal->literal.value.size;
	if (len > 1) {
		if (!GNU_MODE && !(c_mode & _C99)) {
			errorf(HERE, "more than 1 character in character constant");
		} else {
			literal->base.type = type_int;
			warningf(WARN_MULTICHAR, HERE, "multi-character character constant");
		}
	}

	next_token();
	return literal;
}

/**
 * Parse a wide character constant.
 */
static expression_t *parse_wide_character_constant(void)
{
	expression_t *literal = allocate_expression_zero(EXPR_LITERAL_WIDE_CHARACTER);
	literal->base.type     = type_int;
	literal->literal.value = token.string.string;

	size_t len = wstrlen(&literal->literal.value);
	if (len > 1) {
		warningf(WARN_MULTICHAR, HERE, "multi-character character constant");
	}

	next_token();
	return literal;
}

static entity_t *create_implicit_function(symbol_t *symbol,
		const source_position_t *source_position)
{
	type_t *ntype                          = allocate_type_zero(TYPE_FUNCTION);
	ntype->function.return_type            = type_int;
	ntype->function.unspecified_parameters = true;
	ntype->function.linkage                = LINKAGE_C;
	type_t *type                           = identify_new_type(ntype);

	entity_t *const entity = allocate_entity_zero(ENTITY_FUNCTION, NAMESPACE_NORMAL, symbol);
	entity->declaration.storage_class          = STORAGE_CLASS_EXTERN;
	entity->declaration.declared_storage_class = STORAGE_CLASS_EXTERN;
	entity->declaration.type                   = type;
	entity->declaration.implicit               = true;
	entity->base.source_position               = *source_position;

	if (current_scope != NULL)
		record_entity(entity, false);

	return entity;
}

/**
 * Performs automatic type cast as described in §6.3.2.1.
 *
 * @param orig_type  the original type
 */
static type_t *automatic_type_conversion(type_t *orig_type)
{
	type_t *type = skip_typeref(orig_type);
	if (is_type_array(type)) {
		array_type_t *array_type   = &type->array;
		type_t       *element_type = array_type->element_type;
		unsigned      qualifiers   = array_type->base.qualifiers;

		return make_pointer_type(element_type, qualifiers);
	}

	if (is_type_function(type)) {
		return make_pointer_type(orig_type, TYPE_QUALIFIER_NONE);
	}

	return orig_type;
}

/**
 * reverts the automatic casts of array to pointer types and function
 * to function-pointer types as defined §6.3.2.1
 */
type_t *revert_automatic_type_conversion(const expression_t *expression)
{
	switch (expression->kind) {
	case EXPR_REFERENCE: {
		entity_t *entity = expression->reference.entity;
		if (is_declaration(entity)) {
			return entity->declaration.type;
		} else if (entity->kind == ENTITY_ENUM_VALUE) {
			return entity->enum_value.enum_type;
		} else {
			panic("no declaration or enum in reference");
		}
	}

	case EXPR_SELECT: {
		entity_t *entity = expression->select.compound_entry;
		assert(is_declaration(entity));
		type_t   *type   = entity->declaration.type;
		return get_qualified_type(type, expression->base.type->base.qualifiers);
	}

	case EXPR_UNARY_DEREFERENCE: {
		const expression_t *const value = expression->unary.value;
		type_t             *const type  = skip_typeref(value->base.type);
		if (!is_type_pointer(type))
			return type_error_type;
		return type->pointer.points_to;
	}

	case EXPR_ARRAY_ACCESS: {
		const expression_t *array_ref = expression->array_access.array_ref;
		type_t             *type_left = skip_typeref(array_ref->base.type);
		if (!is_type_pointer(type_left))
			return type_error_type;
		return type_left->pointer.points_to;
	}

	case EXPR_STRING_LITERAL: {
		size_t size = expression->string_literal.value.size;
		return make_array_type(type_char, size, TYPE_QUALIFIER_NONE);
	}

	case EXPR_WIDE_STRING_LITERAL: {
		size_t size = wstrlen(&expression->string_literal.value);
		return make_array_type(type_wchar_t, size, TYPE_QUALIFIER_NONE);
	}

	case EXPR_COMPOUND_LITERAL:
		return expression->compound_literal.type;

	default:
		break;
	}
	return expression->base.type;
}

/**
 * Find an entity matching a symbol in a scope.
 * Uses current scope if scope is NULL
 */
static entity_t *lookup_entity(const scope_t *scope, symbol_t *symbol,
                               namespace_tag_t namespc)
{
	if (scope == NULL) {
		return get_entity(symbol, namespc);
	}

	/* we should optimize here, if scope grows above a certain size we should
	   construct a hashmap here... */
	entity_t *entity = scope->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		if (entity->base.symbol == symbol
		    && (namespace_tag_t)entity->base.namespc == namespc)
			break;
	}

	return entity;
}

static entity_t *parse_qualified_identifier(void)
{
	/* namespace containing the symbol */
	symbol_t          *symbol;
	source_position_t  pos;
	const scope_t     *lookup_scope = NULL;

	if (next_if(T_COLONCOLON))
		lookup_scope = &unit->scope;

	entity_t *entity;
	while (true) {
		if (token.kind != T_IDENTIFIER) {
			parse_error_expected("while parsing identifier", T_IDENTIFIER, NULL);
			return create_error_entity(sym_anonymous, ENTITY_VARIABLE);
		}
		symbol = token.identifier.symbol;
		pos    = *HERE;
		next_token();

		/* lookup entity */
		entity = lookup_entity(lookup_scope, symbol, NAMESPACE_NORMAL);

		if (!next_if(T_COLONCOLON))
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
			while (next_if(T_IDENTIFIER) && next_if(T_COLONCOLON)) {}

			return create_error_entity(sym_anonymous, ENTITY_VARIABLE);
		}
	}

	if (entity == NULL) {
		if (!strict_mode && token.kind == '(') {
			/* an implicitly declared function */
			warningf(WARN_IMPLICIT_FUNCTION_DECLARATION, &pos,
			         "implicit declaration of function '%Y'", symbol);
			entity = create_implicit_function(symbol, &pos);
		} else {
			errorf(&pos, "unknown identifier '%Y' found.", symbol);
			entity = create_error_entity(symbol, ENTITY_VARIABLE);
		}
	}

	return entity;
}

static expression_t *parse_reference(void)
{
	source_position_t const pos    = token.base.source_position;
	entity_t         *const entity = parse_qualified_identifier();

	type_t *orig_type;
	if (is_declaration(entity)) {
		orig_type = entity->declaration.type;
	} else if (entity->kind == ENTITY_ENUM_VALUE) {
		orig_type = entity->enum_value.enum_type;
	} else {
		panic("expected declaration or enum value in reference");
	}

	/* we always do the auto-type conversions; the & and sizeof parser contains
	 * code to revert this! */
	type_t *type = automatic_type_conversion(orig_type);

	expression_kind_t kind = EXPR_REFERENCE;
	if (entity->kind == ENTITY_ENUM_VALUE)
		kind = EXPR_ENUM_CONSTANT;

	expression_t *expression         = allocate_expression_zero(kind);
	expression->base.source_position = pos;
	expression->base.type            = type;
	expression->reference.entity     = entity;

	/* this declaration is used */
	if (is_declaration(entity)) {
		entity->declaration.used = true;
	}

	if (entity->base.parent_scope != file_scope
		&& (current_function != NULL
			&& entity->base.parent_scope->depth < current_function->parameters.depth)
		&& (entity->kind == ENTITY_VARIABLE || entity->kind == ENTITY_PARAMETER)) {
		if (entity->kind == ENTITY_VARIABLE) {
			/* access of a variable from an outer function */
			entity->variable.address_taken = true;
		} else if (entity->kind == ENTITY_PARAMETER) {
			entity->parameter.address_taken = true;
		}
		current_function->need_closure = true;
	}

	check_deprecated(&pos, entity);

	return expression;
}

static bool semantic_cast(expression_t *cast)
{
	expression_t            *expression      = cast->unary.value;
	type_t                  *orig_dest_type  = cast->base.type;
	type_t                  *orig_type_right = expression->base.type;
	type_t            const *dst_type        = skip_typeref(orig_dest_type);
	type_t            const *src_type        = skip_typeref(orig_type_right);
	source_position_t const *pos             = &cast->base.source_position;

	/* §6.5.4 A (void) cast is explicitly permitted, more for documentation than for utility. */
	if (is_type_void(dst_type))
		return true;

	/* only integer and pointer can be casted to pointer */
	if (is_type_pointer(dst_type)  &&
	    !is_type_pointer(src_type) &&
	    !is_type_integer(src_type) &&
	    is_type_valid(src_type)) {
		errorf(pos, "cannot convert type '%T' to a pointer type", orig_type_right);
		return false;
	}

	if (!is_type_scalar(dst_type) && is_type_valid(dst_type)) {
		errorf(pos, "conversion to non-scalar type '%T' requested", orig_dest_type);
		return false;
	}

	if (!is_type_scalar(src_type) && is_type_valid(src_type)) {
		errorf(pos, "conversion from non-scalar type '%T' requested", orig_type_right);
		return false;
	}

	if (is_type_pointer(src_type) && is_type_pointer(dst_type)) {
		type_t *src = skip_typeref(src_type->pointer.points_to);
		type_t *dst = skip_typeref(dst_type->pointer.points_to);
		unsigned missing_qualifiers =
			src->base.qualifiers & ~dst->base.qualifiers;
		if (missing_qualifiers != 0) {
			warningf(WARN_CAST_QUAL, pos, "cast discards qualifiers '%Q' in pointer target type of '%T'", missing_qualifiers, orig_type_right);
		}
	}
	return true;
}

static expression_t *parse_compound_literal(source_position_t const *const pos, type_t *type)
{
	expression_t *expression = allocate_expression_zero(EXPR_COMPOUND_LITERAL);
	expression->base.source_position = *pos;

	parse_initializer_env_t env;
	env.type             = type;
	env.entity           = NULL;
	env.must_be_constant = false;
	initializer_t *initializer = parse_initializer(&env);
	type = env.type;

	expression->compound_literal.initializer = initializer;
	expression->compound_literal.type        = type;
	expression->base.type                    = automatic_type_conversion(type);

	return expression;
}

/**
 * Parse a cast expression.
 */
static expression_t *parse_cast(void)
{
	source_position_t const pos = *HERE;

	eat('(');
	add_anchor_token(')');

	type_t *type = parse_typename();

	rem_anchor_token(')');
	expect(')', end_error);

	if (token.kind == '{') {
		return parse_compound_literal(&pos, type);
	}

	expression_t *cast = allocate_expression_zero(EXPR_UNARY_CAST);
	cast->base.source_position = pos;

	expression_t *value = parse_subexpression(PREC_CAST);
	cast->base.type   = type;
	cast->unary.value = value;

	if (! semantic_cast(cast)) {
		/* TODO: record the error in the AST. else it is impossible to detect it */
	}

	return cast;
end_error:
	return create_error_expression();
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

	/* find last statement and use its type */
	type_t *type = type_void;
	const statement_t *stmt = statement->compound.statements;
	if (stmt != NULL) {
		while (stmt->base.next != NULL)
			stmt = stmt->base.next;

		if (stmt->kind == STATEMENT_EXPRESSION) {
			type = stmt->expression.expression->base.type;
		}
	} else {
		source_position_t const *const pos = &expression->base.source_position;
		warningf(WARN_OTHER, pos, "empty statement expression ({})");
	}
	expression->base.type = type;

	rem_anchor_token(')');
	expect(')', end_error);

end_error:
	return expression;
}

/**
 * Parse a parenthesized expression.
 */
static expression_t *parse_parenthesized_expression(void)
{
	token_t const* const la1 = look_ahead(1);
	switch (la1->kind) {
	case '{':
		/* gcc extension: a statement expression */
		return parse_statement_expression();

	case T_IDENTIFIER:
		if (is_typedef_symbol(la1->identifier.symbol)) {
	DECLARATION_START
			return parse_cast();
		}
	}

	eat('(');
	add_anchor_token(')');
	expression_t *result = parse_expression();
	result->base.parenthesized = true;
	rem_anchor_token(')');
	expect(')', end_error);

end_error:
	return result;
}

static expression_t *parse_function_keyword(void)
{
	/* TODO */

	if (current_function == NULL) {
		errorf(HERE, "'__func__' used outside of a function");
	}

	expression_t *expression  = allocate_expression_zero(EXPR_FUNCNAME);
	expression->base.type     = type_char_ptr;
	expression->funcname.kind = FUNCNAME_FUNCTION;

	next_token();

	return expression;
}

static expression_t *parse_pretty_function_keyword(void)
{
	if (current_function == NULL) {
		errorf(HERE, "'__PRETTY_FUNCTION__' used outside of a function");
	}

	expression_t *expression  = allocate_expression_zero(EXPR_FUNCNAME);
	expression->base.type     = type_char_ptr;
	expression->funcname.kind = FUNCNAME_PRETTY_FUNCTION;

	eat(T___PRETTY_FUNCTION__);

	return expression;
}

static expression_t *parse_funcsig_keyword(void)
{
	if (current_function == NULL) {
		errorf(HERE, "'__FUNCSIG__' used outside of a function");
	}

	expression_t *expression  = allocate_expression_zero(EXPR_FUNCNAME);
	expression->base.type     = type_char_ptr;
	expression->funcname.kind = FUNCNAME_FUNCSIG;

	eat(T___FUNCSIG__);

	return expression;
}

static expression_t *parse_funcdname_keyword(void)
{
	if (current_function == NULL) {
		errorf(HERE, "'__FUNCDNAME__' used outside of a function");
	}

	expression_t *expression  = allocate_expression_zero(EXPR_FUNCNAME);
	expression->base.type     = type_char_ptr;
	expression->funcname.kind = FUNCNAME_FUNCDNAME;

	eat(T___FUNCDNAME__);

	return expression;
}

static designator_t *parse_designator(void)
{
	designator_t *result    = allocate_ast_zero(sizeof(result[0]));
	result->source_position = *HERE;

	if (token.kind != T_IDENTIFIER) {
		parse_error_expected("while parsing member designator",
		                     T_IDENTIFIER, NULL);
		return NULL;
	}
	result->symbol = token.identifier.symbol;
	next_token();

	designator_t *last_designator = result;
	while (true) {
		if (next_if('.')) {
			if (token.kind != T_IDENTIFIER) {
				parse_error_expected("while parsing member designator",
				                     T_IDENTIFIER, NULL);
				return NULL;
			}
			designator_t *designator    = allocate_ast_zero(sizeof(result[0]));
			designator->source_position = *HERE;
			designator->symbol          = token.identifier.symbol;
			next_token();

			last_designator->next = designator;
			last_designator       = designator;
			continue;
		}
		if (next_if('[')) {
			add_anchor_token(']');
			designator_t *designator    = allocate_ast_zero(sizeof(result[0]));
			designator->source_position = *HERE;
			designator->array_index     = parse_expression();
			rem_anchor_token(']');
			expect(']', end_error);
			if (designator->array_index == NULL) {
				return NULL;
			}

			last_designator->next = designator;
			last_designator       = designator;
			continue;
		}
		break;
	}

	return result;
end_error:
	return NULL;
}

/**
 * Parse the __builtin_offsetof() expression.
 */
static expression_t *parse_offsetof(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_OFFSETOF);
	expression->base.type    = type_size_t;

	eat(T___builtin_offsetof);

	expect('(', end_error);
	add_anchor_token(',');
	type_t *type = parse_typename();
	rem_anchor_token(',');
	expect(',', end_error);
	add_anchor_token(')');
	designator_t *designator = parse_designator();
	rem_anchor_token(')');
	expect(')', end_error);

	expression->offsetofe.type       = type;
	expression->offsetofe.designator = designator;

	type_path_t path;
	memset(&path, 0, sizeof(path));
	path.top_type = type;
	path.path     = NEW_ARR_F(type_path_entry_t, 0);

	descend_into_subtype(&path);

	if (!walk_designator(&path, designator, true)) {
		return create_error_expression();
	}

	DEL_ARR_F(path.path);

	return expression;
end_error:
	return create_error_expression();
}

/**
 * Parses a _builtin_va_start() expression.
 */
static expression_t *parse_va_start(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_VA_START);

	eat(T___builtin_va_start);

	expect('(', end_error);
	add_anchor_token(',');
	expression->va_starte.ap = parse_assignment_expression();
	rem_anchor_token(',');
	expect(',', end_error);
	expression_t *const expr = parse_assignment_expression();
	if (expr->kind == EXPR_REFERENCE) {
		entity_t *const entity = expr->reference.entity;
		if (!current_function->base.type->function.variadic) {
			errorf(&expr->base.source_position,
					"'va_start' used in non-variadic function");
		} else if (entity->base.parent_scope != &current_function->parameters ||
				entity->base.next != NULL ||
				entity->kind != ENTITY_PARAMETER) {
			errorf(&expr->base.source_position,
			       "second argument of 'va_start' must be last parameter of the current function");
		} else {
			expression->va_starte.parameter = &entity->variable;
		}
		expect(')', end_error);
		return expression;
	}
	expect(')', end_error);
end_error:
	return create_error_expression();
}

/**
 * Parses a __builtin_va_arg() expression.
 */
static expression_t *parse_va_arg(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_VA_ARG);

	eat(T___builtin_va_arg);

	expect('(', end_error);
	call_argument_t ap;
	ap.expression = parse_assignment_expression();
	expression->va_arge.ap = ap.expression;
	check_call_argument(type_valist, &ap, 1);

	expect(',', end_error);
	expression->base.type = parse_typename();
	expect(')', end_error);

	return expression;
end_error:
	return create_error_expression();
}

/**
 * Parses a __builtin_va_copy() expression.
 */
static expression_t *parse_va_copy(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_VA_COPY);

	eat(T___builtin_va_copy);

	expect('(', end_error);
	expression_t *dst = parse_assignment_expression();
	assign_error_t error = semantic_assign(type_valist, dst);
	report_assign_error(error, type_valist, dst, "call argument 1",
	                    &dst->base.source_position);
	expression->va_copye.dst = dst;

	expect(',', end_error);

	call_argument_t src;
	src.expression = parse_assignment_expression();
	check_call_argument(type_valist, &src, 2);
	expression->va_copye.src = src.expression;
	expect(')', end_error);

	return expression;
end_error:
	return create_error_expression();
}

/**
 * Parses a __builtin_constant_p() expression.
 */
static expression_t *parse_builtin_constant(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_BUILTIN_CONSTANT_P);

	eat(T___builtin_constant_p);

	expect('(', end_error);
	add_anchor_token(')');
	expression->builtin_constant.value = parse_assignment_expression();
	rem_anchor_token(')');
	expect(')', end_error);
	expression->base.type = type_int;

	return expression;
end_error:
	return create_error_expression();
}

/**
 * Parses a __builtin_types_compatible_p() expression.
 */
static expression_t *parse_builtin_types_compatible(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_BUILTIN_TYPES_COMPATIBLE_P);

	eat(T___builtin_types_compatible_p);

	expect('(', end_error);
	add_anchor_token(')');
	add_anchor_token(',');
	expression->builtin_types_compatible.left = parse_typename();
	rem_anchor_token(',');
	expect(',', end_error);
	expression->builtin_types_compatible.right = parse_typename();
	rem_anchor_token(')');
	expect(')', end_error);
	expression->base.type = type_int;

	return expression;
end_error:
	return create_error_expression();
}

/**
 * Parses a __builtin_is_*() compare expression.
 */
static expression_t *parse_compare_builtin(void)
{
	expression_t *expression;

	switch (token.kind) {
	case T___builtin_isgreater:
		expression = allocate_expression_zero(EXPR_BINARY_ISGREATER);
		break;
	case T___builtin_isgreaterequal:
		expression = allocate_expression_zero(EXPR_BINARY_ISGREATEREQUAL);
		break;
	case T___builtin_isless:
		expression = allocate_expression_zero(EXPR_BINARY_ISLESS);
		break;
	case T___builtin_islessequal:
		expression = allocate_expression_zero(EXPR_BINARY_ISLESSEQUAL);
		break;
	case T___builtin_islessgreater:
		expression = allocate_expression_zero(EXPR_BINARY_ISLESSGREATER);
		break;
	case T___builtin_isunordered:
		expression = allocate_expression_zero(EXPR_BINARY_ISUNORDERED);
		break;
	default:
		internal_errorf(HERE, "invalid compare builtin found");
	}
	expression->base.source_position = *HERE;
	next_token();

	expect('(', end_error);
	expression->binary.left = parse_assignment_expression();
	expect(',', end_error);
	expression->binary.right = parse_assignment_expression();
	expect(')', end_error);

	type_t *const orig_type_left  = expression->binary.left->base.type;
	type_t *const orig_type_right = expression->binary.right->base.type;

	type_t *const type_left  = skip_typeref(orig_type_left);
	type_t *const type_right = skip_typeref(orig_type_right);
	if (!is_type_float(type_left) && !is_type_float(type_right)) {
		if (is_type_valid(type_left) && is_type_valid(type_right)) {
			type_error_incompatible("invalid operands in comparison",
				&expression->base.source_position, orig_type_left, orig_type_right);
		}
	} else {
		semantic_comparison(&expression->binary);
	}

	return expression;
end_error:
	return create_error_expression();
}

/**
 * Parses a MS assume() expression.
 */
static expression_t *parse_assume(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_UNARY_ASSUME);

	eat(T__assume);

	expect('(', end_error);
	add_anchor_token(')');
	expression->unary.value = parse_assignment_expression();
	rem_anchor_token(')');
	expect(')', end_error);

	expression->base.type = type_void;
	return expression;
end_error:
	return create_error_expression();
}

/**
 * Return the label for the current symbol or create a new one.
 */
static label_t *get_label(void)
{
	assert(token.kind == T_IDENTIFIER);
	assert(current_function != NULL);

	entity_t *label = get_entity(token.identifier.symbol, NAMESPACE_LABEL);
	/* If we find a local label, we already created the declaration. */
	if (label != NULL && label->kind == ENTITY_LOCAL_LABEL) {
		if (label->base.parent_scope != current_scope) {
			assert(label->base.parent_scope->depth < current_scope->depth);
			current_function->goto_to_outer = true;
		}
	} else if (label == NULL || label->base.parent_scope != &current_function->parameters) {
		/* There is no matching label in the same function, so create a new one. */
		label = allocate_entity_zero(ENTITY_LABEL, NAMESPACE_LABEL, token.identifier.symbol);
		label_push(label);
	}

	eat(T_IDENTIFIER);
	return &label->label;
}

/**
 * Parses a GNU && label address expression.
 */
static expression_t *parse_label_address(void)
{
	source_position_t source_position = token.base.source_position;
	eat(T_ANDAND);
	if (token.kind != T_IDENTIFIER) {
		parse_error_expected("while parsing label address", T_IDENTIFIER, NULL);
		return create_error_expression();
	}

	label_t *const label = get_label();
	label->used          = true;
	label->address_taken = true;

	expression_t *expression = allocate_expression_zero(EXPR_LABEL_ADDRESS);
	expression->base.source_position = source_position;

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
	literal->base.type           = type_int;
	literal->literal.value.begin = "__noop";
	literal->literal.value.size  = 6;

	eat(T___noop);

	if (token.kind == '(') {
		/* parse arguments */
		eat('(');
		add_anchor_token(')');
		add_anchor_token(',');

		if (token.kind != ')') do {
			(void)parse_assignment_expression();
		} while (next_if(','));

		rem_anchor_token(',');
		rem_anchor_token(')');
	}
	expect(')', end_error);

end_error:
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
	case T_INTEGER:
	case T_INTEGER_OCTAL:
	case T_INTEGER_HEXADECIMAL:
	case T_FLOATINGPOINT:
	case T_FLOATINGPOINT_HEXADECIMAL:    return parse_number_literal();
	case T_CHARACTER_CONSTANT:           return parse_character_constant();
	case T_WIDE_CHARACTER_CONSTANT:      return parse_wide_character_constant();
	case T_STRING_LITERAL:
	case T_WIDE_STRING_LITERAL:          return parse_string_literal();
	case T___FUNCTION__:
	case T___func__:                     return parse_function_keyword();
	case T___PRETTY_FUNCTION__:          return parse_pretty_function_keyword();
	case T___FUNCSIG__:                  return parse_funcsig_keyword();
	case T___FUNCDNAME__:                return parse_funcdname_keyword();
	case T___builtin_offsetof:           return parse_offsetof();
	case T___builtin_va_start:           return parse_va_start();
	case T___builtin_va_arg:             return parse_va_arg();
	case T___builtin_va_copy:            return parse_va_copy();
	case T___builtin_isgreater:
	case T___builtin_isgreaterequal:
	case T___builtin_isless:
	case T___builtin_islessequal:
	case T___builtin_islessgreater:
	case T___builtin_isunordered:        return parse_compare_builtin();
	case T___builtin_constant_p:         return parse_builtin_constant();
	case T___builtin_types_compatible_p: return parse_builtin_types_compatible();
	case T__assume:                      return parse_assume();
	case T_ANDAND:
		if (GNU_MODE)
			return parse_label_address();
		break;

	case '(':                            return parse_parenthesized_expression();
	case T___noop:                       return parse_noop_expression();

	/* Gracefully handle type names while parsing expressions. */
	case T_COLONCOLON:
		return parse_reference();
	case T_IDENTIFIER:
		if (!is_typedef_symbol(token.identifier.symbol)) {
			return parse_reference();
		}
		/* FALLTHROUGH */
	DECLARATION_START {
		source_position_t const  pos = *HERE;
		declaration_specifiers_t specifiers;
		parse_declaration_specifiers(&specifiers);
		type_t const *const type = parse_abstract_declarator(specifiers.type);
		errorf(&pos, "encountered type '%T' while parsing expression", type);
		return create_error_expression();
	}
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
			errorf(&idx->base.source_position, "array subscript must have integer type");
		} else if (is_type_atomic(idx_type, ATOMIC_TYPE_CHAR)) {
			source_position_t const *const pos = &idx->base.source_position;
			warningf(WARN_CHAR_SUBSCRIPTS, pos, "array subscript has char type");
		}
	} else {
		if (is_type_valid(type_left) && is_type_valid(type_inside)) {
			errorf(&expr->base.source_position, "invalid types '%T[%T]' for array access", orig_type_left, orig_type_inside);
		}
		res_type = type_error_type;
		ref      = left;
		idx      = inside;
	}

	arr->array_ref = ref;
	arr->index     = idx;
	arr->base.type = res_type;

	rem_anchor_token(']');
	expect(']', end_error);
end_error:
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

	eat(kind == EXPR_SIZEOF ? T_sizeof : T___alignof__);

	type_t       *orig_type;
	expression_t *expression;
	if (token.kind == '(' && is_declaration_specifier(look_ahead(1))) {
		source_position_t const pos = *HERE;
		next_token();
		add_anchor_token(')');
		orig_type = parse_typename();
		rem_anchor_token(')');
		expect(')', end_error);

		if (token.kind == '{') {
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
			errorf(&tp_expression->base.source_position,
				   "operand of %s expression must not be a bitfield", what);
		}

		tp_expression->typeprop.tp_expression = expression;

		orig_type = revert_automatic_type_conversion(expression);
		expression->base.type = orig_type;
	}

	tp_expression->typeprop.type   = orig_type;
	type_t const* const type       = skip_typeref(orig_type);
	char   const*       wrong_type = NULL;
	if (is_type_incomplete(type)) {
		if (!is_type_void(type) || !GNU_MODE)
			wrong_type = "incomplete";
	} else if (type->kind == TYPE_FUNCTION) {
		if (GNU_MODE) {
			/* function types are allowed (and return 1) */
			source_position_t const *const pos  = &tp_expression->base.source_position;
			char              const *const what = kind == EXPR_SIZEOF ? "sizeof" : "alignof";
			warningf(WARN_OTHER, pos, "%s expression with function argument returns invalid result", what);
		} else {
			wrong_type = "function";
		}
	}

	if (wrong_type != NULL) {
		char const* const what = kind == EXPR_SIZEOF ? "sizeof" : "alignof";
		errorf(&tp_expression->base.source_position,
				"operand of %s expression must not be of %s type '%T'",
				what, wrong_type, orig_type);
	}

end_error:
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
	assert(token.kind == '.' || token.kind == T_MINUSGREATER);
	bool select_left_arrow = (token.kind == T_MINUSGREATER);
	source_position_t const pos = *HERE;
	next_token();

	if (token.kind != T_IDENTIFIER) {
		parse_error_expected("while parsing select", T_IDENTIFIER, NULL);
		return create_error_expression();
	}
	symbol_t *symbol = token.identifier.symbol;
	next_token();

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
			errorf(&pos, "left hand side of '->' is not a pointer, but '%T'", orig_type);
			saw_error = true;
		}
		type_left = type;
	}

	if (type_left->kind != TYPE_COMPOUND_STRUCT &&
	    type_left->kind != TYPE_COMPOUND_UNION) {

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
	expression_t      *result     =
		find_create_select(&pos, addr, qualifiers, compound, symbol);

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
		compound_t *union_decl  = expected_type_skip->compound.compound;
		type_t     *best_type   = NULL;
		entity_t   *entry       = union_decl->members.entities;
		for ( ; entry != NULL; entry = entry->base.next) {
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

		if (best_type != NULL) {
			expected_type = best_type;
		}
	}

	error                = semantic_assign(expected_type, arg_expr);
	argument->expression = create_implicit_cast(arg_expr, expected_type);

	if (error != ASSIGN_SUCCESS) {
		/* report exact scope in error messages (like "in argument 3") */
		char buf[64];
		snprintf(buf, sizeof(buf), "call argument %u", pos);
		report_assign_error(error, expected_type, arg_expr, buf,
		                    &arg_expr->base.source_position);
	} else {
		type_t *const promoted_type = get_default_promoted_type(arg_type);
		if (!types_compatible(expected_type_skip, promoted_type) &&
		    !types_compatible(expected_type_skip, type_void_ptr) &&
		    !types_compatible(type_void_ptr,      promoted_type)) {
			/* Deliberately show the skipped types in this warning */
			source_position_t const *const apos = &arg_expr->base.source_position;
			warningf(WARN_TRADITIONAL, apos, "passing call argument %u as '%T' rather than '%T' due to prototype", pos, expected_type_skip, promoted_type);
		}
	}
}

/**
 * Handle the semantic restrictions of builtin calls
 */
static void handle_builtin_argument_restrictions(call_expression_t *call)
{
	entity_t *entity = call->function->reference.entity;
	switch (entity->function.btk) {
	case BUILTIN_FIRM:
		switch (entity->function.b.firm_builtin_kind) {
		case ir_bk_return_address:
		case ir_bk_frame_address: {
			/* argument must be constant */
			call_argument_t *argument = call->arguments;

			if (is_constant_expression(argument->expression) == EXPR_CLASS_VARIABLE) {
				errorf(&call->base.source_position,
					   "argument of '%Y' must be a constant expression",
					   call->function->reference.entity->base.symbol);
			}
			break;
		}
		case ir_bk_prefetch:
			/* second and third argument must be constant if existent */
			if (call->arguments == NULL)
				break;
			call_argument_t *rw = call->arguments->next;
			call_argument_t *locality = NULL;

			if (rw != NULL) {
				if (is_constant_expression(rw->expression) == EXPR_CLASS_VARIABLE) {
					errorf(&call->base.source_position,
						   "second argument of '%Y' must be a constant expression",
						   call->function->reference.entity->base.symbol);
				}
				locality = rw->next;
			}
			if (locality != NULL) {
				if (is_constant_expression(locality->expression) == EXPR_CLASS_VARIABLE) {
					errorf(&call->base.source_position,
						   "third argument of '%Y' must be a constant expression",
						   call->function->reference.entity->base.symbol);
				}
				locality = rw->next;
			}
			break;
		default:
			break;
		}

	case BUILTIN_OBJECT_SIZE:
		if (call->arguments == NULL)
			break;

		call_argument_t *arg = call->arguments->next;
		if (arg != NULL && is_constant_expression(arg->expression) == EXPR_CLASS_VARIABLE) {
			errorf(&call->base.source_position,
				   "second argument of '%Y' must be a constant expression",
				   call->function->reference.entity->base.symbol);
		}
		break;
	default:
		break;
	}
}

/**
 * Parse a call expression, ie. expression '( ... )'.
 *
 * @param expression  the function address
 */
static expression_t *parse_call_expression(expression_t *expression)
{
	expression_t      *result = allocate_expression_zero(EXPR_CALL);
	call_expression_t *call   = &result->call;
	call->function            = expression;

	type_t *const orig_type = expression->base.type;
	type_t *const type      = skip_typeref(orig_type);

	function_type_t *function_type = NULL;
	if (is_type_pointer(type)) {
		type_t *const to_type = skip_typeref(type->pointer.points_to);

		if (is_type_function(to_type)) {
			function_type   = &to_type->function;
			call->base.type = function_type->return_type;
		}
	}

	if (function_type == NULL && is_type_valid(type)) {
		errorf(HERE,
		       "called object '%E' (type '%T') is not a pointer to a function",
		       expression, orig_type);
	}

	/* parse arguments */
	eat('(');
	add_anchor_token(')');
	add_anchor_token(',');

	if (token.kind != ')') {
		call_argument_t **anchor = &call->arguments;
		do {
			call_argument_t *argument = allocate_ast_zero(sizeof(*argument));
			argument->expression = parse_assignment_expression();

			*anchor = argument;
			anchor  = &argument->next;
		} while (next_if(','));
	}
	rem_anchor_token(',');
	rem_anchor_token(')');
	expect(')', end_error);

	if (function_type == NULL)
		return result;

	/* check type and count of call arguments */
	function_parameter_t *parameter = function_type->parameters;
	call_argument_t      *argument  = call->arguments;
	if (!function_type->unspecified_parameters) {
		for (unsigned pos = 0; parameter != NULL && argument != NULL;
				parameter = parameter->next, argument = argument->next) {
			check_call_argument(parameter->type, argument, ++pos);
		}

		if (parameter != NULL) {
			errorf(&expression->base.source_position, "too few arguments to function '%E'", expression);
		} else if (argument != NULL && !function_type->variadic) {
			errorf(&argument->expression->base.source_position, "too many arguments to function '%E'", expression);
		}
	}

	/* do default promotion for other arguments */
	for (; argument != NULL; argument = argument->next) {
		type_t *argument_type = argument->expression->base.type;
		if (!is_type_object(skip_typeref(argument_type))) {
			errorf(&argument->expression->base.source_position,
			       "call argument '%E' must not be void", argument->expression);
		}

		argument_type = get_default_promoted_type(argument_type);

		argument->expression
			= create_implicit_cast(argument->expression, argument_type);
	}

	check_format(call);

	if (is_type_compound(skip_typeref(function_type->return_type))) {
		source_position_t const *const pos = &expression->base.source_position;
		warningf(WARN_AGGREGATE_RETURN, pos, "function call has aggregate value");
	}

	if (expression->kind == EXPR_REFERENCE) {
		reference_expression_t *reference = &expression->reference;
		if (reference->entity->kind == ENTITY_FUNCTION &&
		    reference->entity->function.btk != BUILTIN_NONE)
			handle_builtin_argument_restrictions(call);
	}

end_error:
	return result;
}

static type_t *semantic_arithmetic(type_t *type_left, type_t *type_right);

static bool same_compound_type(const type_t *type1, const type_t *type2)
{
	return
		is_type_compound(type1) &&
		type1->kind == type2->kind &&
		type1->compound.compound == type2->compound.compound;
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
	if (!regular_take_address &&
			expr->reference.entity->kind != ENTITY_FUNCTION) {
		return NULL;
	}

	return expr;
}

static void warn_reference_address_as_bool(expression_t const* expr)
{
	expr = get_reference_address(expr);
	if (expr != NULL) {
		source_position_t const *const pos = &expr->base.source_position;
		entity_t          const *const ent = expr->reference.entity;
		warningf(WARN_ADDRESS, pos, "the address of '%N' will always evaluate as 'true'", ent);
	}
}

static void warn_assignment_in_condition(const expression_t *const expr)
{
	if (expr->base.kind != EXPR_BINARY_ASSIGN)
		return;
	if (expr->base.parenthesized)
		return;
	source_position_t const *const pos = &expr->base.source_position;
	warningf(WARN_PARENTHESES, pos, "suggest parentheses around assignment used as truth value");
}

static void semantic_condition(expression_t const *const expr,
                               char const *const context)
{
	type_t *const type = skip_typeref(expr->base.type);
	if (is_type_scalar(type)) {
		warn_reference_address_as_bool(expr);
		warn_assignment_in_condition(expr);
	} else if (is_type_valid(type)) {
		errorf(&expr->base.source_position,
				"%s must have scalar type", context);
	}
}

/**
 * Parse a conditional expression, ie. 'expression ? ... : ...'.
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
	if (GNU_MODE && token.kind == ':') {
		gnu_cond = true;
	} else {
		true_expression = parse_expression();
	}
	rem_anchor_token(':');
	expect(':', end_error);
end_error:;
	expression_t *false_expression =
		parse_subexpression(c_mode & _CXX ? PREC_ASSIGNMENT : PREC_CONDITIONAL);

	type_t *const orig_true_type  = true_expression->base.type;
	type_t *const orig_false_type = false_expression->base.type;
	type_t *const true_type       = skip_typeref(orig_true_type);
	type_t *const false_type      = skip_typeref(orig_false_type);

	/* 6.5.15.3 */
	source_position_t const *const pos = &conditional->base.source_position;
	type_t                        *result_type;
	if (is_type_void(true_type) || is_type_void(false_type)) {
		/* ISO/IEC 14882:1998(E) §5.16:2 */
		if (true_expression->kind == EXPR_UNARY_THROW) {
			result_type = false_type;
		} else if (false_expression->kind == EXPR_UNARY_THROW) {
			result_type = true_type;
		} else {
			if (!is_type_void(true_type) || !is_type_void(false_type)) {
				warningf(WARN_OTHER, pos, "ISO C forbids conditional expression with only one void side");
			}
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
		if (is_type_pointer(true_type) &&
				(!is_type_pointer(false_type) || is_null_pointer_constant(false_expression))) {
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
			} else if (types_compatible(get_unqualified_type(to1),
			                            get_unqualified_type(to2))) {
				to = to1;
			} else {
				warningf(WARN_OTHER, pos, "pointer types '%T' and '%T' in conditional expression are incompatible", true_type, false_type);
				to = type_void;
			}

			type_t *const type =
				get_qualified_type(to, to1->base.qualifiers | to2->base.qualifiers);
			result_type = make_pointer_type(type, TYPE_QUALIFIER_NONE);
		} else if (is_type_integer(other_type)) {
			warningf(WARN_OTHER, pos, "pointer/integer type mismatch in conditional expression ('%T' and '%T')", true_type, false_type);
			result_type = pointer_type;
		} else {
			goto types_incompatible;
		}
	} else {
types_incompatible:
		if (is_type_valid(true_type) && is_type_valid(false_type)) {
			type_error_incompatible("while parsing conditional", pos, true_type, false_type);
		}
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

	expect('(', end_error);
	add_anchor_token(')');
	expression_t *expression = parse_expression();
	rem_anchor_token(')');
	expect(')', end_error);
	result->classify_type.type_expression = expression;

	return result;
end_error:
	return create_error_expression();
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

	if (next_if('[')) {
		result->kind = EXPR_UNARY_DELETE_ARRAY;
		expect(']', end_error);
end_error:;
	}

	expression_t *const value = parse_subexpression(PREC_CAST);
	result->unary.value = value;

	type_t *const type = skip_typeref(value->base.type);
	if (!is_type_pointer(type)) {
		if (is_type_valid(type)) {
			errorf(&value->base.source_position,
					"operand of delete must have pointer type");
		}
	} else if (is_type_void(skip_typeref(type->pointer.points_to))) {
		source_position_t const *const pos = &value->base.source_position;
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
		EXPRESSION_START {
			value = parse_assignment_expression();
			/* ISO/IEC 14882:1998(E) §15.1:3 */
			type_t *const orig_type = value->base.type;
			type_t *const type      = skip_typeref(orig_type);
			if (is_type_incomplete(type)) {
				errorf(&value->base.source_position,
						"cannot throw object of incomplete type '%T'", orig_type);
			} else if (is_type_pointer(type)) {
				type_t *const points_to = skip_typeref(type->pointer.points_to);
				if (is_type_incomplete(points_to) && !is_type_void(points_to)) {
					errorf(&value->base.source_position,
							"cannot throw pointer to incomplete type '%T'", orig_type);
				}
			}
		}

		default:
			break;
	}
	result->unary.value = value;

	return result;
}

static bool check_pointer_arithmetic(const source_position_t *source_position,
                                     type_t *pointer_type,
                                     type_t *orig_pointer_type)
{
	type_t *points_to = pointer_type->pointer.points_to;
	points_to = skip_typeref(points_to);

	if (is_type_incomplete(points_to)) {
		if (!GNU_MODE || !is_type_void(points_to)) {
			errorf(source_position,
			       "arithmetic with pointer to incomplete type '%T' not allowed",
			       orig_pointer_type);
			return false;
		} else {
			warningf(WARN_POINTER_ARITH, source_position, "pointer of type '%T' used in arithmetic", orig_pointer_type);
		}
	} else if (is_type_function(points_to)) {
		if (!GNU_MODE) {
			errorf(source_position,
			       "arithmetic with pointer to function type '%T' not allowed",
			       orig_pointer_type);
			return false;
		} else {
			warningf(WARN_POINTER_ARITH, source_position, "pointer to a function '%T' used in arithmetic", orig_pointer_type);
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
	case EXPR_UNARY_DEREFERENCE:
		return true;

	default: {
		type_t *type = skip_typeref(expression->base.type);
		return
			/* ISO/IEC 14882:1998(E) §3.10:3 */
			is_type_reference(type) ||
			/* Claim it is an lvalue, if the type is invalid.  There was a parse
			 * error before, which maybe prevented properly recognizing it as
			 * lvalue. */
			!is_type_valid(type);
	}
	}
}

static void semantic_incdec(unary_expression_t *expression)
{
	type_t *const orig_type = expression->value->base.type;
	type_t *const type      = skip_typeref(orig_type);
	if (is_type_pointer(type)) {
		if (!check_pointer_arithmetic(&expression->base.source_position,
		                              type, orig_type)) {
			return;
		}
	} else if (!is_type_real(type) && is_type_valid(type)) {
		/* TODO: improve error message */
		errorf(&expression->base.source_position,
		       "operation needs an arithmetic or pointer type");
		return;
	}
	if (!is_lvalue(expression->value)) {
		/* TODO: improve error message */
		errorf(&expression->base.source_position, "lvalue required as operand");
	}
	expression->base.type = orig_type;
}

static void promote_unary_int_expr(unary_expression_t *const expr, type_t *const type)
{
	type_t *const res_type = promote_integer(type);
	expr->base.type = res_type;
	expr->value     = create_implicit_cast(expr->value, res_type);
}

static void semantic_unexpr_arithmetic(unary_expression_t *expression)
{
	type_t *const orig_type = expression->value->base.type;
	type_t *const type      = skip_typeref(orig_type);
	if (!is_type_arithmetic(type)) {
		if (is_type_valid(type)) {
			/* TODO: improve error message */
			errorf(&expression->base.source_position,
				"operation needs an arithmetic type");
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
	source_position_t const *const pos = &expression->base.source_position;
	warningf(WARN_TRADITIONAL, pos, "traditional C rejects the unary plus operator");
}

static void semantic_not(unary_expression_t *expression)
{
	/* §6.5.3.3:1  The operand [...] of the ! operator, scalar type. */
	semantic_condition(expression->value, "operand of !");
	expression->base.type = c_mode & _CXX ? type_bool : type_int;
}

static void semantic_unexpr_integer(unary_expression_t *expression)
{
	type_t *const orig_type = expression->value->base.type;
	type_t *const type      = skip_typeref(orig_type);
	if (!is_type_integer(type)) {
		if (is_type_valid(type)) {
			errorf(&expression->base.source_position,
			       "operand of ~ must be of integer type");
		}
		return;
	}

	promote_unary_int_expr(expression, type);
}

static void semantic_dereference(unary_expression_t *expression)
{
	type_t *const orig_type = expression->value->base.type;
	type_t *const type      = skip_typeref(orig_type);
	if (!is_type_pointer(type)) {
		if (is_type_valid(type)) {
			errorf(&expression->base.source_position,
			       "Unary '*' needs pointer or array type, but type '%T' given", orig_type);
		}
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
 * @param may_be_register  if true, the expression might be an register
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
		source_position_t const *const pos = &expression->base.source_position;
		errorf(pos, "address of register '%N' requested", entity);
	}

	if (entity->kind == ENTITY_VARIABLE) {
		entity->variable.address_taken = true;
	} else {
		assert(entity->kind == ENTITY_PARAMETER);
		entity->parameter.address_taken = true;
	}
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
	if (!is_lvalue(value)) {
		errorf(&expression->base.source_position, "'&' requires an lvalue");
	}
	if (is_bitfield(value)) {
		errorf(&expression->base.source_position,
		       "'&' not allowed on bitfield");
	}

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
CREATE_UNARY_EXPRESSION_PARSER('~', EXPR_UNARY_BITWISE_NEGATE,
                               semantic_unexpr_integer)
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

static type_t *semantic_arithmetic(type_t *type_left, type_t *type_right)
{
	/* TODO: handle complex + imaginary types */

	type_left  = get_unqualified_type(type_left);
	type_right = get_unqualified_type(type_right);

	/* §6.3.1.8 Usual arithmetic conversions */
	if (type_left == type_long_double || type_right == type_long_double) {
		return type_long_double;
	} else if (type_left == type_double || type_right == type_double) {
		return type_double;
	} else if (type_left == type_float || type_right == type_float) {
		return type_float;
	}

	type_left  = promote_integer(type_left);
	type_right = promote_integer(type_right);

	if (type_left == type_right)
		return type_left;

	bool     const signed_left  = is_type_signed(type_left);
	bool     const signed_right = is_type_signed(type_right);
	unsigned const rank_left    = get_akind_rank(get_akind(type_left));
	unsigned const rank_right   = get_akind_rank(get_akind(type_right));

	if (signed_left == signed_right)
		return rank_left >= rank_right ? type_left : type_right;

	unsigned           s_rank;
	unsigned           u_rank;
	atomic_type_kind_t s_akind;
	atomic_type_kind_t u_akind;
	type_t *s_type;
	type_t *u_type;
	if (signed_left) {
		s_type = type_left;
		u_type = type_right;
	} else {
		s_type = type_right;
		u_type = type_left;
	}
	s_akind = get_akind(s_type);
	u_akind = get_akind(u_type);
	s_rank  = get_akind_rank(s_akind);
	u_rank  = get_akind_rank(u_akind);

	if (u_rank >= s_rank)
		return u_type;

	if (get_atomic_type_size(s_akind) > get_atomic_type_size(u_akind))
		return s_type;

	switch (s_akind) {
	case ATOMIC_TYPE_INT:      return type_unsigned_int;
	case ATOMIC_TYPE_LONG:     return type_unsigned_long;
	case ATOMIC_TYPE_LONGLONG: return type_unsigned_long_long;

	default: panic("invalid atomic type");
	}
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

	if (!is_type_arithmetic(type_left) || !is_type_arithmetic(type_right)) {
		/* TODO: improve error message */
		if (is_type_valid(type_left) && is_type_valid(type_right)) {
			errorf(&expression->base.source_position,
			       "operation needs arithmetic types");
		}
		return;
	}

	type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
	expression->left      = create_implicit_cast(left, arithmetic_type);
	expression->right     = create_implicit_cast(right, arithmetic_type);
	expression->base.type = arithmetic_type;
}

static void semantic_binexpr_integer(binary_expression_t *const expression)
{
	expression_t *const left            = expression->left;
	expression_t *const right           = expression->right;
	type_t       *const orig_type_left  = left->base.type;
	type_t       *const orig_type_right = right->base.type;
	type_t       *const type_left       = skip_typeref(orig_type_left);
	type_t       *const type_right      = skip_typeref(orig_type_right);

	if (!is_type_integer(type_left) || !is_type_integer(type_right)) {
		/* TODO: improve error message */
		if (is_type_valid(type_left) && is_type_valid(type_right)) {
			errorf(&expression->base.source_position,
			       "operation needs integer types");
		}
		return;
	}

	type_t *const result_type = semantic_arithmetic(type_left, type_right);
	expression->left      = create_implicit_cast(left, result_type);
	expression->right     = create_implicit_cast(right, result_type);
	expression->base.type = result_type;
}

static void warn_div_by_zero(binary_expression_t const *const expression)
{
	if (!is_type_integer(expression->base.type))
		return;

	expression_t const *const right = expression->right;
	/* The type of the right operand can be different for /= */
	if (is_type_integer(right->base.type)                    &&
	    is_constant_expression(right) == EXPR_CLASS_CONSTANT &&
	    !fold_constant_to_bool(right)) {
		source_position_t const *const pos = &expression->base.source_position;
		warningf(WARN_DIV_BY_ZERO, pos, "division by zero");
	}
}

/**
 * Check the semantic restrictions for a div/mod expression.
 */
static void semantic_divmod_arithmetic(binary_expression_t *expression)
{
	semantic_binexpr_arithmetic(expression);
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
		default:              return;
	}

	source_position_t const *const pos = &expr->base.source_position;
	warningf(WARN_PARENTHESES, pos, "suggest parentheses around '%c' inside shift", op);
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
		/* TODO: improve error message */
		if (is_type_valid(type_left) && is_type_valid(type_right)) {
			errorf(&expression->base.source_position,
			       "operands of shift operation must have integer types");
		}
		return false;
	}

	type_left = promote_integer(type_left);

	if (is_constant_expression(right) == EXPR_CLASS_CONSTANT) {
		source_position_t const *const pos   = &right->base.source_position;
		long                     const count = fold_constant_to_int(right);
		if (count < 0) {
			warningf(WARN_OTHER, pos, "shift count must be non-negative");
		} else if ((unsigned long)count >=
				get_atomic_type_size(type_left->atomic.akind) * 8) {
			warningf(WARN_OTHER, pos, "shift count must be less than type width");
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
		type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
		expression->left  = create_implicit_cast(left, arithmetic_type);
		expression->right = create_implicit_cast(right, arithmetic_type);
		expression->base.type = arithmetic_type;
	} else if (is_type_pointer(type_left) && is_type_integer(type_right)) {
		check_pointer_arithmetic(&expression->base.source_position,
		                         type_left, orig_type_left);
		expression->base.type = type_left;
	} else if (is_type_pointer(type_right) && is_type_integer(type_left)) {
		check_pointer_arithmetic(&expression->base.source_position,
		                         type_right, orig_type_right);
		expression->base.type = type_right;
	} else if (is_type_valid(type_left) && is_type_valid(type_right)) {
		errorf(&expression->base.source_position,
		       "invalid operands to binary + ('%T', '%T')",
		       orig_type_left, orig_type_right);
	}
}

static void semantic_sub(binary_expression_t *expression)
{
	expression_t            *const left            = expression->left;
	expression_t            *const right           = expression->right;
	type_t                  *const orig_type_left  = left->base.type;
	type_t                  *const orig_type_right = right->base.type;
	type_t                  *const type_left       = skip_typeref(orig_type_left);
	type_t                  *const type_right      = skip_typeref(orig_type_right);
	source_position_t const *const pos             = &expression->base.source_position;

	/* §5.6.5 */
	if (is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
		expression->left        = create_implicit_cast(left, arithmetic_type);
		expression->right       = create_implicit_cast(right, arithmetic_type);
		expression->base.type =  arithmetic_type;
	} else if (is_type_pointer(type_left) && is_type_integer(type_right)) {
		check_pointer_arithmetic(&expression->base.source_position,
		                         type_left, orig_type_left);
		expression->base.type = type_left;
	} else if (is_type_pointer(type_left) && is_type_pointer(type_right)) {
		type_t *const unqual_left  = get_unqualified_type(skip_typeref(type_left->pointer.points_to));
		type_t *const unqual_right = get_unqualified_type(skip_typeref(type_right->pointer.points_to));
		if (!types_compatible(unqual_left, unqual_right)) {
			errorf(pos,
			       "subtracting pointers to incompatible types '%T' and '%T'",
			       orig_type_left, orig_type_right);
		} else if (!is_type_object(unqual_left)) {
			if (!is_type_void(unqual_left)) {
				errorf(pos, "subtracting pointers to non-object types '%T'",
				       orig_type_left);
			} else {
				warningf(WARN_OTHER, pos, "subtracting pointers to void");
			}
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

	if (expr->kind == EXPR_STRING_LITERAL
			|| expr->kind == EXPR_WIDE_STRING_LITERAL) {
		source_position_t const *const pos = &expr->base.source_position;
		warningf(WARN_ADDRESS, pos, "comparison with string literal results in unspecified behaviour");
	}
}

static bool maybe_negative(expression_t const *const expr)
{
	switch (is_constant_expression(expr)) {
		case EXPR_CLASS_ERROR:    return false;
		case EXPR_CLASS_CONSTANT: return constant_is_negative(expr);
		default:                  return true;
	}
}

static void warn_comparison(source_position_t const *const pos, expression_t const *const expr, expression_t const *const other)
{
	warn_string_literal_address(expr);

	expression_t const* const ref = get_reference_address(expr);
	if (ref != NULL && is_null_pointer_constant(other)) {
		entity_t const *const ent = ref->reference.entity;
		warningf(WARN_ADDRESS, pos, "the address of '%N' will never be NULL", ent);
	}

	if (!expr->base.parenthesized) {
		switch (expr->base.kind) {
			case EXPR_BINARY_LESS:
			case EXPR_BINARY_GREATER:
			case EXPR_BINARY_LESSEQUAL:
			case EXPR_BINARY_GREATEREQUAL:
			case EXPR_BINARY_NOTEQUAL:
			case EXPR_BINARY_EQUAL:
				warningf(WARN_PARENTHESES, pos, "comparisons like 'x <= y < z' do not have their mathematical meaning");
				break;
			default:
				break;
		}
	}
}

/**
 * Check the semantics of comparison expressions.
 *
 * @param expression   The expression to check.
 */
static void semantic_comparison(binary_expression_t *expression)
{
	source_position_t const *const pos   = &expression->base.source_position;
	expression_t            *const left  = expression->left;
	expression_t            *const right = expression->right;

	warn_comparison(pos, left, right);
	warn_comparison(pos, right, left);

	type_t *orig_type_left  = left->base.type;
	type_t *orig_type_right = right->base.type;
	type_t *type_left       = skip_typeref(orig_type_left);
	type_t *type_right      = skip_typeref(orig_type_right);

	/* TODO non-arithmetic types */
	if (is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);

		/* test for signed vs unsigned compares */
		if (is_type_integer(arithmetic_type)) {
			bool const signed_left  = is_type_signed(type_left);
			bool const signed_right = is_type_signed(type_right);
			if (signed_left != signed_right) {
				/* FIXME long long needs better const folding magic */
				/* TODO check whether constant value can be represented by other type */
				if ((signed_left  && maybe_negative(left)) ||
						(signed_right && maybe_negative(right))) {
					warningf(WARN_SIGN_COMPARE, pos, "comparison between signed and unsigned");
				}
			}
		}

		expression->left        = create_implicit_cast(left, arithmetic_type);
		expression->right       = create_implicit_cast(right, arithmetic_type);
		expression->base.type   = arithmetic_type;
		if ((expression->base.kind == EXPR_BINARY_EQUAL ||
		     expression->base.kind == EXPR_BINARY_NOTEQUAL) &&
		    is_type_float(arithmetic_type)) {
			warningf(WARN_FLOAT_EQUAL, pos, "comparing floating point with == or != is unsafe");
		}
	} else if (is_type_pointer(type_left) && is_type_pointer(type_right)) {
		/* TODO check compatibility */
	} else if (is_type_pointer(type_left)) {
		expression->right = create_implicit_cast(right, type_left);
	} else if (is_type_pointer(type_right)) {
		expression->left = create_implicit_cast(left, type_right);
	} else if (is_type_valid(type_left) && is_type_valid(type_right)) {
		type_error_incompatible("invalid operands in comparison", pos, type_left, type_right);
	}
	expression->base.type = c_mode & _CXX ? type_bool : type_int;
}

/**
 * Checks if a compound type has constant fields.
 */
static bool has_const_fields(const compound_type_t *type)
{
	compound_t *compound = type->compound;
	entity_t   *entry    = compound->members.entities;

	for (; entry != NULL; entry = entry->base.next) {
		if (!is_declaration(entry))
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
		errorf(&left->base.source_position, "left hand side '%E' of assignment is not an lvalue",
		       left);
		return false;
	}

	if (left->kind == EXPR_REFERENCE
			&& left->reference.entity->kind == ENTITY_FUNCTION) {
		errorf(&left->base.source_position, "cannot assign to function '%E'", left);
		return false;
	}

	if (is_type_array(type_left)) {
		errorf(&left->base.source_position, "cannot assign to array '%E'", left);
		return false;
	}
	if (type_left->base.qualifiers & TYPE_QUALIFIER_CONST) {
		errorf(&left->base.source_position, "assignment to read-only location '%E' (type '%T')", left,
		       orig_type_left);
		return false;
	}
	if (is_type_incomplete(type_left)) {
		errorf(&left->base.source_position, "left-hand side '%E' of assignment has incomplete type '%T'",
		       left, orig_type_left);
		return false;
	}
	if (is_type_compound(type_left) && has_const_fields(&type_left->compound)) {
		errorf(&left->base.source_position, "cannot assign to '%E' because compound type '%T' has read-only fields",
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
		if (is_type_valid(type_left) && is_type_valid(type_right)) {
			errorf(&expression->base.source_position,
			       "operation needs arithmetic types");
		}
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
		check_pointer_arithmetic(&expression->base.source_position,
		                         type_left, orig_type_left);
		expression->base.type = type_left;
	} else if (is_type_valid(type_left) && is_type_valid(type_right)) {
		errorf(&expression->base.source_position,
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
		if (is_type_valid(type_left) && is_type_valid(type_right)) {
			errorf(&expression->base.source_position,
			       "operation needs integer types");
		}
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
	source_position_t const *const pos = &expr->base.source_position;
	warningf(WARN_PARENTHESES, pos, "suggest parentheses around && within ||");
}

/**
 * Check the semantic restrictions of a logical expression.
 */
static void semantic_logical_op(binary_expression_t *expression)
{
	/* §6.5.13:2  Each of the operands shall have scalar type.
	 * §6.5.14:2  Each of the operands shall have scalar type. */
	semantic_condition(expression->left,   "left operand of logical operator");
	semantic_condition(expression->right, "right operand of logical operator");
	if (expression->base.kind == EXPR_BINARY_LOGICAL_OR) {
		warn_logical_and_within_or(expression->left);
		warn_logical_and_within_or(expression->right);
	}
	expression->base.type = c_mode & _CXX ? type_bool : type_int;
}

/**
 * Check the semantic restrictions of a binary assign expression.
 */
static void semantic_binexpr_assign(binary_expression_t *expression)
{
	expression_t *left           = expression->left;
	type_t       *orig_type_left = left->base.type;

	if (!is_valid_assignment_lhs(left))
		return;

	assign_error_t error = semantic_assign(orig_type_left, expression->right);
	report_assign_error(error, orig_type_left, expression->right,
			"assignment", &left->base.source_position);
	expression->right = create_implicit_cast(expression->right, orig_type_left);
	expression->base.type = orig_type_left;
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
		case EXPR_LITERAL_WIDE_CHARACTER:
		case EXPR_LITERAL_INTEGER:
		case EXPR_LITERAL_INTEGER_OCTAL:
		case EXPR_LITERAL_INTEGER_HEXADECIMAL:
		case EXPR_LITERAL_FLOATINGPOINT:
		case EXPR_LITERAL_FLOATINGPOINT_HEXADECIMAL: return false;
		case EXPR_STRING_LITERAL:             return false;
		case EXPR_WIDE_STRING_LITERAL:        return false;

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
			return
				(t == NULL || expression_has_effect(t)) &&
				expression_has_effect(cond->false_expression);
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
		case EXPR_UNARY_BITWISE_NEGATE:       return false;
		case EXPR_UNARY_NOT:                  return false;
		case EXPR_UNARY_DEREFERENCE:          return false;
		case EXPR_UNARY_TAKE_ADDRESS:         return false;
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

	internal_errorf(HERE, "unexpected expression");
}

static void semantic_comma(binary_expression_t *expression)
{
	const expression_t *const left = expression->left;
	if (!expression_has_effect(left)) {
		source_position_t const *const pos = &left->base.source_position;
		warningf(WARN_UNUSED_VALUE, pos, "left-hand operand of comma expression has no effect");
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
CREATE_BINEXPR_PARSER('/',                    EXPR_BINARY_DIV,                PREC_CAST,           semantic_divmod_arithmetic)
CREATE_BINEXPR_PARSER('%',                    EXPR_BINARY_MOD,                PREC_CAST,           semantic_divmod_arithmetic)
CREATE_BINEXPR_PARSER('+',                    EXPR_BINARY_ADD,                PREC_MULTIPLICATIVE, semantic_add)
CREATE_BINEXPR_PARSER('-',                    EXPR_BINARY_SUB,                PREC_MULTIPLICATIVE, semantic_sub)
CREATE_BINEXPR_PARSER(T_LESSLESS,             EXPR_BINARY_SHIFTLEFT,          PREC_ADDITIVE,       semantic_shift_op)
CREATE_BINEXPR_PARSER(T_GREATERGREATER,       EXPR_BINARY_SHIFTRIGHT,         PREC_ADDITIVE,       semantic_shift_op)
CREATE_BINEXPR_PARSER('<',                    EXPR_BINARY_LESS,               PREC_SHIFT,          semantic_comparison)
CREATE_BINEXPR_PARSER('>',                    EXPR_BINARY_GREATER,            PREC_SHIFT,          semantic_comparison)
CREATE_BINEXPR_PARSER(T_LESSEQUAL,            EXPR_BINARY_LESSEQUAL,          PREC_SHIFT,          semantic_comparison)
CREATE_BINEXPR_PARSER(T_GREATEREQUAL,         EXPR_BINARY_GREATEREQUAL,       PREC_SHIFT,          semantic_comparison)
CREATE_BINEXPR_PARSER(T_EXCLAMATIONMARKEQUAL, EXPR_BINARY_NOTEQUAL,           PREC_RELATIONAL,     semantic_comparison)
CREATE_BINEXPR_PARSER(T_EQUALEQUAL,           EXPR_BINARY_EQUAL,              PREC_RELATIONAL,     semantic_comparison)
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
	if (token.kind < 0) {
		return expected_expression_error();
	}

	expression_parser_function_t *parser
		= &expression_parsers[token.kind];
	expression_t                 *left;

	if (parser->parser != NULL) {
		left = parser->parser();
	} else {
		left = parse_primary_expression();
	}
	assert(left != NULL);

	while (true) {
		if (token.kind < 0) {
			return expected_expression_error();
		}

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

	if (entry->parser != NULL) {
		diagnosticf("for token '%k'\n", (token_kind_t)token_kind);
		panic("trying to register multiple expression parsers for a token");
	}
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

	if (entry->infix_parser != NULL) {
		diagnosticf("for token '%k'\n", (token_kind_t)token_kind);
		panic("trying to register multiple infix expression parsers for a "
		      "token");
	}
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
	register_expression_parser(parse_EXPR_UNARY_BITWISE_NEGATE,   '~');
	register_expression_parser(parse_EXPR_UNARY_DEREFERENCE,      '*');
	register_expression_parser(parse_EXPR_UNARY_TAKE_ADDRESS,     '&');
	register_expression_parser(parse_EXPR_UNARY_PREFIX_INCREMENT, T_PLUSPLUS);
	register_expression_parser(parse_EXPR_UNARY_PREFIX_DECREMENT, T_MINUSMINUS);
	register_expression_parser(parse_sizeof,                      T_sizeof);
	register_expression_parser(parse_alignof,                     T___alignof__);
	register_expression_parser(parse_extension,                   T___extension__);
	register_expression_parser(parse_builtin_classify_type,       T___builtin_classify_type);
	register_expression_parser(parse_delete,                      T_delete);
	register_expression_parser(parse_throw,                       T_throw);
}

/**
 * Parse a asm statement arguments specification.
 */
static asm_argument_t *parse_asm_arguments(bool is_out)
{
	asm_argument_t  *result = NULL;
	asm_argument_t **anchor = &result;

	while (token.kind == T_STRING_LITERAL || token.kind == '[') {
		asm_argument_t *argument = allocate_ast_zero(sizeof(argument[0]));
		memset(argument, 0, sizeof(argument[0]));

		if (next_if('[')) {
			if (token.kind != T_IDENTIFIER) {
				parse_error_expected("while parsing asm argument",
				                     T_IDENTIFIER, NULL);
				return NULL;
			}
			argument->symbol = token.identifier.symbol;

			expect(']', end_error);
		}

		argument->constraints = parse_string_literals();
		expect('(', end_error);
		add_anchor_token(')');
		expression_t *expression = parse_expression();
		rem_anchor_token(')');
		if (is_out) {
			/* Ugly GCC stuff: Allow lvalue casts.  Skip casts, when they do not
			 * change size or type representation (e.g. int -> long is ok, but
			 * int -> float is not) */
			if (expression->kind == EXPR_UNARY_CAST) {
				type_t      *const type = expression->base.type;
				type_kind_t  const kind = type->kind;
				if (kind == TYPE_ATOMIC || kind == TYPE_POINTER) {
					unsigned flags;
					unsigned size;
					if (kind == TYPE_ATOMIC) {
						atomic_type_kind_t const akind = type->atomic.akind;
						flags = get_atomic_type_flags(akind) & ~ATOMIC_TYPE_FLAG_SIGNED;
						size  = get_atomic_type_size(akind);
					} else {
						flags = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC;
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
							value_flags = ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_ARITHMETIC;
							value_size  = get_type_size(type_void_ptr);
						} else {
							break;
						}

						if (value_flags != flags || value_size != size)
							break;

						expression = value;
					} while (expression->kind == EXPR_UNARY_CAST);
				}
			}

			if (!is_lvalue(expression)) {
				errorf(&expression->base.source_position,
				       "asm output argument is not an lvalue");
			}

			if (argument->constraints.begin[0] == '=')
				determine_lhs_ent(expression, NULL);
			else
				mark_vars_read(expression, NULL);
		} else {
			mark_vars_read(expression, NULL);
		}
		argument->expression = expression;
		expect(')', end_error);

		set_address_taken(expression, true);

		*anchor = argument;
		anchor  = &argument->next;

		if (!next_if(','))
			break;
	}

	return result;
end_error:
	return NULL;
}

/**
 * Parse a asm statement clobber specification.
 */
static asm_clobber_t *parse_asm_clobbers(void)
{
	asm_clobber_t *result  = NULL;
	asm_clobber_t **anchor = &result;

	while (token.kind == T_STRING_LITERAL) {
		asm_clobber_t *clobber = allocate_ast_zero(sizeof(clobber[0]));
		clobber->clobber       = parse_string_literals();

		*anchor = clobber;
		anchor  = &clobber->next;

		if (!next_if(','))
			break;
	}

	return result;
}

/**
 * Parse an asm statement.
 */
static statement_t *parse_asm_statement(void)
{
	statement_t     *statement     = allocate_statement_zero(STATEMENT_ASM);
	asm_statement_t *asm_statement = &statement->asms;

	eat(T_asm);

	if (next_if(T_volatile))
		asm_statement->is_volatile = true;

	expect('(', end_error);
	add_anchor_token(')');
	if (token.kind != T_STRING_LITERAL) {
		parse_error_expected("after asm(", T_STRING_LITERAL, NULL);
		goto end_of_asm;
	}
	asm_statement->asm_text = parse_string_literals();

	add_anchor_token(':');
	if (!next_if(':')) {
		rem_anchor_token(':');
		goto end_of_asm;
	}

	asm_statement->outputs = parse_asm_arguments(true);
	if (!next_if(':')) {
		rem_anchor_token(':');
		goto end_of_asm;
	}

	asm_statement->inputs = parse_asm_arguments(false);
	if (!next_if(':')) {
		rem_anchor_token(':');
		goto end_of_asm;
	}
	rem_anchor_token(':');

	asm_statement->clobbers = parse_asm_clobbers();

end_of_asm:
	rem_anchor_token(')');
	expect(')', end_error);
	expect(';', end_error);

	if (asm_statement->outputs == NULL) {
		/* GCC: An 'asm' instruction without any output operands will be treated
		 * identically to a volatile 'asm' instruction. */
		asm_statement->is_volatile = true;
	}

	return statement;
end_error:
	return create_error_statement();
}

static statement_t *parse_label_inner_statement(statement_t const *const label, char const *const label_kind)
{
	statement_t *inner_stmt;
	switch (token.kind) {
		case '}':
			errorf(&label->base.source_position, "%s at end of compound statement", label_kind);
			inner_stmt = create_error_statement();
			break;

		case ';':
			if (label->kind == STATEMENT_LABEL) {
				/* Eat an empty statement here, to avoid the warning about an empty
				 * statement after a label.  label:; is commonly used to have a label
				 * before a closing brace. */
				inner_stmt = create_empty_statement();
				next_token();
				break;
			}
			/* FALLTHROUGH */

		default:
			inner_stmt = parse_statement();
			/* ISO/IEC  9899:1999(E) §6.8:1/6.8.2:1  Declarations are no statements */
			/* ISO/IEC 14882:1998(E) §6:1/§6.7       Declarations are statements */
			if (inner_stmt->kind == STATEMENT_DECLARATION && !(c_mode & _CXX)) {
				errorf(&inner_stmt->base.source_position, "declaration after %s", label_kind);
			}
			break;
	}
	return inner_stmt;
}

/**
 * Parse a case statement.
 */
static statement_t *parse_case_statement(void)
{
	statement_t       *const statement = allocate_statement_zero(STATEMENT_CASE_LABEL);
	source_position_t *const pos       = &statement->base.source_position;

	eat(T_case);

	expression_t *expression = parse_expression();
	type_t *expression_type = expression->base.type;
	type_t *skipped         = skip_typeref(expression_type);
	if (!is_type_integer(skipped) && is_type_valid(skipped)) {
		errorf(pos, "case expression '%E' must have integer type but has type '%T'",
		       expression, expression_type);
	}

	type_t *type = expression_type;
	if (current_switch != NULL) {
		type_t *switch_type = current_switch->expression->base.type;
		if (is_type_valid(switch_type)) {
			expression = create_implicit_cast(expression, switch_type);
		}
	}

	statement->case_label.expression = expression;
	expression_classification_t const expr_class = is_constant_expression(expression);
	if (expr_class != EXPR_CLASS_CONSTANT) {
		if (expr_class != EXPR_CLASS_ERROR) {
			errorf(pos, "case label does not reduce to an integer constant");
		}
		statement->case_label.is_bad = true;
	} else {
		long const val = fold_constant_to_int(expression);
		statement->case_label.first_case = val;
		statement->case_label.last_case  = val;
	}

	if (GNU_MODE) {
		if (next_if(T_DOTDOTDOT)) {
			expression_t *end_range = parse_expression();
			expression_type = expression->base.type;
			skipped         = skip_typeref(expression_type);
			if (!is_type_integer(skipped) && is_type_valid(skipped)) {
				errorf(pos, "case expression '%E' must have integer type but has type '%T'",
					   expression, expression_type);
			}

			end_range = create_implicit_cast(end_range, type);
			statement->case_label.end_range = end_range;
			expression_classification_t const end_class = is_constant_expression(end_range);
			if (end_class != EXPR_CLASS_CONSTANT) {
				if (end_class != EXPR_CLASS_ERROR) {
					errorf(pos, "case range does not reduce to an integer constant");
				}
				statement->case_label.is_bad = true;
			} else {
				long const val = fold_constant_to_int(end_range);
				statement->case_label.last_case = val;

				if (val < statement->case_label.first_case) {
					statement->case_label.is_empty_range = true;
					warningf(WARN_OTHER, pos, "empty range specified");
				}
			}
		}
	}

	PUSH_PARENT(statement);

	expect(':', end_error);
end_error:

	if (current_switch != NULL) {
		if (! statement->case_label.is_bad) {
			/* Check for duplicate case values */
			case_label_statement_t *c = &statement->case_label;
			for (case_label_statement_t *l = current_switch->first_case; l != NULL; l = l->next) {
				if (l->is_bad || l->is_empty_range || l->expression == NULL)
					continue;

				if (c->last_case < l->first_case || c->first_case > l->last_case)
					continue;

				errorf(pos, "duplicate case value (previously used %P)",
				       &l->base.source_position);
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

	statement->case_label.statement = parse_label_inner_statement(statement, "case label");

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

	expect(':', end_error);
end_error:

	if (current_switch != NULL) {
		const case_label_statement_t *def_label = current_switch->default_label;
		if (def_label != NULL) {
			errorf(&statement->base.source_position, "multiple default labels in one switch (previous declared %P)", &def_label->base.source_position);
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
		errorf(&statement->base.source_position,
			"'default' label not within a switch statement");
	}

	statement->case_label.statement = parse_label_inner_statement(statement, "default label");

	POP_PARENT();
	return statement;
}

/**
 * Parse a label statement.
 */
static statement_t *parse_label_statement(void)
{
	statement_t *const statement = allocate_statement_zero(STATEMENT_LABEL);
	label_t     *const label     = get_label();
	statement->label.label = label;

	PUSH_PARENT(statement);

	/* if statement is already set then the label is defined twice,
	 * otherwise it was just mentioned in a goto/local label declaration so far
	 */
	source_position_t const* const pos = &statement->base.source_position;
	if (label->statement != NULL) {
		errorf(pos, "duplicate '%N' (declared %P)", (entity_t const*)label, &label->base.source_position);
	} else {
		label->base.source_position = *pos;
		label->statement            = statement;
	}

	eat(':');

	if (token.kind == T___attribute__ && !(c_mode & _CXX)) {
		parse_attributes(NULL); // TODO process attributes
	}

	statement->label.statement = parse_label_inner_statement(statement, "label");

	/* remember the labels in a list for later checking */
	*label_anchor = &statement->label;
	label_anchor  = &statement->label.next;

	POP_PARENT();
	return statement;
}

static statement_t *parse_inner_statement(void)
{
	statement_t *const stmt = parse_statement();
	/* ISO/IEC  9899:1999(E) §6.8:1/6.8.2:1  Declarations are no statements */
	/* ISO/IEC 14882:1998(E) §6:1/§6.7       Declarations are statements */
	if (stmt->kind == STATEMENT_DECLARATION && !(c_mode & _CXX)) {
		errorf(&stmt->base.source_position, "declaration as inner statement, use {}");
	}
	return stmt;
}

/**
 * Parse an if statement.
 */
static statement_t *parse_if(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_IF);

	eat(T_if);

	PUSH_PARENT(statement);

	add_anchor_token('{');

	expect('(', end_error);
	add_anchor_token(')');
	expression_t *const expr = parse_expression();
	statement->ifs.condition = expr;
	/* §6.8.4.1:1  The controlling expression of an if statement shall have
	 *             scalar type. */
	semantic_condition(expr, "condition of 'if'-statment");
	mark_vars_read(expr, NULL);
	rem_anchor_token(')');
	expect(')', end_error);

end_error:
	rem_anchor_token('{');

	add_anchor_token(T_else);
	statement_t *const true_stmt = parse_inner_statement();
	statement->ifs.true_statement = true_stmt;
	rem_anchor_token(T_else);

	if (true_stmt->kind == STATEMENT_EMPTY) {
		warningf(WARN_EMPTY_BODY, HERE,
		        "suggest braces around empty body in an ‘if’ statement");
	}

	if (next_if(T_else)) {
		statement->ifs.false_statement = parse_inner_statement();

		if (statement->ifs.false_statement->kind == STATEMENT_EMPTY) {
			warningf(WARN_EMPTY_BODY, HERE,
					"suggest braces around empty body in an ‘if’ statement");
		}
	} else if (true_stmt->kind == STATEMENT_IF &&
			true_stmt->ifs.false_statement != NULL) {
		source_position_t const *const pos = &true_stmt->base.source_position;
		warningf(WARN_PARENTHESES, pos, "suggest explicit braces to avoid ambiguous 'else'");
	}

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
	const type_t *type = skip_typeref(statement->expression->base.type);
	if (! is_type_enum(type))
		return;
	const enum_type_t *enumt = &type->enumt;

	/* if we have a default, no warnings */
	if (statement->default_label != NULL)
		return;

	/* FIXME: calculation of value should be done while parsing */
	/* TODO: quadratic algorithm here. Change to an n log n one */
	long            last_value = -1;
	const entity_t *entry      = enumt->enume->base.next;
	for (; entry != NULL && entry->kind == ENTITY_ENUM_VALUE;
	     entry = entry->base.next) {
		const expression_t *expression = entry->enum_value.value;
		long                value      = expression != NULL ? fold_constant_to_int(expression) : last_value + 1;
		bool                found      = false;
		for (const case_label_statement_t *l = statement->first_case; l != NULL; l = l->next) {
			if (l->expression == NULL)
				continue;
			if (l->first_case <= value && value <= l->last_case) {
				found = true;
				break;
			}
		}
		if (!found) {
			source_position_t const *const pos = &statement->base.source_position;
			warningf(WARN_SWITCH_ENUM, pos, "'%N' not handled in switch", entry);
		}
		last_value = value;
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

	expect('(', end_error);
	add_anchor_token(')');
	expression_t *const expr = parse_expression();
	mark_vars_read(expr, NULL);
	type_t       *      type = skip_typeref(expr->base.type);
	if (is_type_integer(type)) {
		type = promote_integer(type);
		if (get_akind_rank(get_akind(type)) >= get_akind_rank(ATOMIC_TYPE_LONG)) {
			warningf(WARN_TRADITIONAL, &expr->base.source_position, "'%T' switch expression not converted to '%T' in ISO C", type, type_int);
		}
	} else if (is_type_valid(type)) {
		errorf(&expr->base.source_position,
		       "switch quantity is not an integer, but '%T'", type);
		type = type_error_type;
	}
	statement->switchs.expression = create_implicit_cast(expr, type);
	expect(')', end_error);
	rem_anchor_token(')');

	switch_statement_t *rem = current_switch;
	current_switch          = &statement->switchs;
	statement->switchs.body = parse_inner_statement();
	current_switch          = rem;

	if (statement->switchs.default_label == NULL) {
		warningf(WARN_SWITCH_DEFAULT, &statement->base.source_position, "switch has no default case");
	}
	check_enum_cases(&statement->switchs);

	POP_PARENT();
	return statement;
end_error:
	POP_PARENT();
	return create_error_statement();
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
	statement_t *statement = allocate_statement_zero(STATEMENT_WHILE);

	eat(T_while);

	PUSH_PARENT(statement);

	expect('(', end_error);
	add_anchor_token(')');
	expression_t *const cond = parse_expression();
	statement->whiles.condition = cond;
	/* §6.8.5:2    The controlling expression of an iteration statement shall
	 *             have scalar type. */
	semantic_condition(cond, "condition of 'while'-statement");
	mark_vars_read(cond, NULL);
	rem_anchor_token(')');
	expect(')', end_error);

	statement->whiles.body = parse_loop_body(statement);

	POP_PARENT();
	return statement;
end_error:
	POP_PARENT();
	return create_error_statement();
}

/**
 * Parse a do statement.
 */
static statement_t *parse_do(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_DO_WHILE);

	eat(T_do);

	PUSH_PARENT(statement);

	add_anchor_token(T_while);
	statement->do_while.body = parse_loop_body(statement);
	rem_anchor_token(T_while);

	expect(T_while, end_error);
	expect('(', end_error);
	add_anchor_token(')');
	expression_t *const cond = parse_expression();
	statement->do_while.condition = cond;
	/* §6.8.5:2    The controlling expression of an iteration statement shall
	 *             have scalar type. */
	semantic_condition(cond, "condition of 'do-while'-statement");
	mark_vars_read(cond, NULL);
	rem_anchor_token(')');
	expect(')', end_error);
	expect(';', end_error);

	POP_PARENT();
	return statement;
end_error:
	POP_PARENT();
	return create_error_statement();
}

/**
 * Parse a for statement.
 */
static statement_t *parse_for(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_FOR);

	eat(T_for);

	expect('(', end_error1);
	add_anchor_token(')');

	PUSH_PARENT(statement);
	PUSH_SCOPE(&statement->fors.scope);

	PUSH_EXTENSION();

	if (next_if(';')) {
	} else if (is_declaration_specifier(&token)) {
		parse_declaration(record_entity, DECL_FLAGS_NONE);
	} else {
		add_anchor_token(';');
		expression_t *const init = parse_expression();
		statement->fors.initialisation = init;
		mark_vars_read(init, ENT_ANY);
		if (!expression_has_effect(init)) {
			warningf(WARN_UNUSED_VALUE, &init->base.source_position, "initialisation of 'for'-statement has no effect");
		}
		rem_anchor_token(';');
		expect(';', end_error2);
	}

	POP_EXTENSION();

	if (token.kind != ';') {
		add_anchor_token(';');
		expression_t *const cond = parse_expression();
		statement->fors.condition = cond;
		/* §6.8.5:2    The controlling expression of an iteration statement
		 *             shall have scalar type. */
		semantic_condition(cond, "condition of 'for'-statement");
		mark_vars_read(cond, NULL);
		rem_anchor_token(';');
	}
	expect(';', end_error2);
	if (token.kind != ')') {
		expression_t *const step = parse_expression();
		statement->fors.step = step;
		mark_vars_read(step, ENT_ANY);
		if (!expression_has_effect(step)) {
			warningf(WARN_UNUSED_VALUE, &step->base.source_position, "step of 'for'-statement has no effect");
		}
	}
	expect(')', end_error2);
	rem_anchor_token(')');
	statement->fors.body = parse_loop_body(statement);

	POP_SCOPE();
	POP_PARENT();
	return statement;

end_error2:
	POP_PARENT();
	rem_anchor_token(')');
	POP_SCOPE();
	/* fallthrough */

end_error1:
	return create_error_statement();
}

/**
 * Parse a goto statement.
 */
static statement_t *parse_goto(void)
{
	statement_t *statement;
	if (GNU_MODE && look_ahead(1)->kind == '*') {
		statement = allocate_statement_zero(STATEMENT_COMPUTED_GOTO);
		eat(T_goto);
		eat('*');

		expression_t *expression = parse_expression();
		mark_vars_read(expression, NULL);

		/* Argh: although documentation says the expression must be of type void*,
		 * gcc accepts anything that can be casted into void* without error */
		type_t *type = expression->base.type;

		if (type != type_error_type) {
			if (!is_type_pointer(type) && !is_type_integer(type)) {
				errorf(&expression->base.source_position,
					"cannot convert to a pointer type");
			} else if (type != type_void_ptr) {
				warningf(WARN_OTHER, &expression->base.source_position, "type of computed goto expression should be 'void*' not '%T'", type);
			}
			expression = create_implicit_cast(expression, type_void_ptr);
		}

		statement->computed_goto.expression = expression;
	} else {
		statement = allocate_statement_zero(STATEMENT_GOTO);
		eat(T_goto);
		if (token.kind == T_IDENTIFIER) {
			label_t *const label = get_label();
			label->used            = true;
			statement->gotos.label = label;

			/* remember the goto's in a list for later checking */
			*goto_anchor = &statement->gotos;
			goto_anchor  = &statement->gotos.next;
		} else {
			if (GNU_MODE)
				parse_error_expected("while parsing goto", T_IDENTIFIER, '*', NULL);
			else
				parse_error_expected("while parsing goto", T_IDENTIFIER, NULL);
			eat_until_anchor();
			return create_error_statement();
		}
	}

	expect(';', end_error);

end_error:
	return statement;
}

/**
 * Parse a continue statement.
 */
static statement_t *parse_continue(void)
{
	if (current_loop == NULL) {
		errorf(HERE, "continue statement not within loop");
	}

	statement_t *statement = allocate_statement_zero(STATEMENT_CONTINUE);

	eat(T_continue);
	expect(';', end_error);

end_error:
	return statement;
}

/**
 * Parse a break statement.
 */
static statement_t *parse_break(void)
{
	if (current_switch == NULL && current_loop == NULL) {
		errorf(HERE, "break statement not within loop or switch");
	}

	statement_t *statement = allocate_statement_zero(STATEMENT_BREAK);

	eat(T_break);
	expect(';', end_error);

end_error:
	return statement;
}

/**
 * Parse a __leave statement.
 */
static statement_t *parse_leave_statement(void)
{
	if (current_try == NULL) {
		errorf(HERE, "__leave statement not within __try");
	}

	statement_t *statement = allocate_statement_zero(STATEMENT_LEAVE);

	eat(T___leave);
	expect(';', end_error);

end_error:
	return statement;
}

/**
 * Check if a given entity represents a local variable.
 */
static bool is_local_variable(const entity_t *entity)
{
	if (entity->kind != ENTITY_VARIABLE)
		return false;

	switch ((storage_class_tag_t) entity->declaration.storage_class) {
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
	if (expression->base.kind != EXPR_REFERENCE) {
		return false;
	}
	const entity_t *entity = expression->reference.entity;
	return is_local_variable(entity);
}

/**
 * Check if a given expression represents a local variable and
 * return its declaration then, else return NULL.
 */
entity_t *expression_is_variable(const expression_t *expression)
{
	if (expression->base.kind != EXPR_REFERENCE) {
		return NULL;
	}
	entity_t *entity = expression->reference.entity;
	if (entity->kind != ENTITY_VARIABLE)
		return NULL;

	return entity;
}

static void err_or_warn(source_position_t const *const pos, char const *const msg)
{
	if (c_mode & _CXX || strict_mode) {
		errorf(pos, msg);
	} else {
		warningf(WARN_OTHER, pos, msg);
	}
}

/**
 * Parse a return statement.
 */
static statement_t *parse_return(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_RETURN);
	eat(T_return);

	expression_t *return_value = NULL;
	if (token.kind != ';') {
		return_value = parse_expression();
		mark_vars_read(return_value, NULL);
	}

	const type_t *const func_type = skip_typeref(current_function->base.type);
	assert(is_type_function(func_type));
	type_t *const return_type = skip_typeref(func_type->function.return_type);

	source_position_t const *const pos = &statement->base.source_position;
	if (return_value != NULL) {
		type_t *return_value_type = skip_typeref(return_value->base.type);

		if (is_type_void(return_type)) {
			if (!is_type_void(return_value_type)) {
				/* ISO/IEC 14882:1998(E) §6.6.3:2 */
				/* Only warn in C mode, because GCC does the same */
				err_or_warn(pos, "'return' with a value, in function returning 'void'");
			} else if (!(c_mode & _CXX)) { /* ISO/IEC 14882:1998(E) §6.6.3:3 */
				/* Only warn in C mode, because GCC does the same */
				err_or_warn(pos, "'return' with expression in function returning 'void'");
			}
		} else {
			assign_error_t error = semantic_assign(return_type, return_value);
			report_assign_error(error, return_type, return_value, "'return'",
			                    pos);
		}
		return_value = create_implicit_cast(return_value, return_type);
		/* check for returning address of a local var */
		if (return_value != NULL && return_value->base.kind == EXPR_UNARY_TAKE_ADDRESS) {
			const expression_t *expression = return_value->unary.value;
			if (expression_is_local_variable(expression)) {
				warningf(WARN_OTHER, pos, "function returns address of local variable");
			}
		}
	} else if (!is_type_void(return_type)) {
		/* ISO/IEC 14882:1998(E) §6.6.3:3 */
		err_or_warn(pos, "'return' without value, in function returning non-void");
	}
	statement->returns.value = return_value;

	expect(';', end_error);

end_error:
	return statement;
}

/**
 * Parse a declaration statement.
 */
static statement_t *parse_declaration_statement(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_DECLARATION);

	entity_t *before = current_scope->last_entity;
	if (GNU_MODE) {
		parse_external_declaration();
	} else {
		parse_declaration(record_entity, DECL_FLAGS_NONE);
	}

	declaration_statement_t *const decl  = &statement->declaration;
	entity_t                *const begin =
		before != NULL ? before->base.next : current_scope->entities;
	decl->declarations_begin = begin;
	decl->declarations_end   = begin != NULL ? current_scope->last_entity : NULL;

	return statement;
}

/**
 * Parse an expression statement, ie. expr ';'.
 */
static statement_t *parse_expression_statement(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_EXPRESSION);

	expression_t *const expr         = parse_expression();
	statement->expression.expression = expr;
	mark_vars_read(expr, ENT_ANY);

	expect(';', end_error);

end_error:
	return statement;
}

/**
 * Parse a microsoft __try { } __finally { } or
 * __try{ } __except() { }
 */
static statement_t *parse_ms_try_statment(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_MS_TRY);
	eat(T___try);

	PUSH_PARENT(statement);

	ms_try_statement_t *rem = current_try;
	current_try = &statement->ms_try;
	statement->ms_try.try_statement = parse_compound_statement(false);
	current_try = rem;

	POP_PARENT();

	if (next_if(T___except)) {
		expect('(', end_error);
		add_anchor_token(')');
		expression_t *const expr = parse_expression();
		mark_vars_read(expr, NULL);
		type_t       *      type = skip_typeref(expr->base.type);
		if (is_type_integer(type)) {
			type = promote_integer(type);
		} else if (is_type_valid(type)) {
			errorf(&expr->base.source_position,
			       "__expect expression is not an integer, but '%T'", type);
			type = type_error_type;
		}
		statement->ms_try.except_expression = create_implicit_cast(expr, type);
		rem_anchor_token(')');
		expect(')', end_error);
		statement->ms_try.final_statement = parse_compound_statement(false);
	} else if (next_if(T__finally)) {
		statement->ms_try.final_statement = parse_compound_statement(false);
	} else {
		parse_error_expected("while parsing __try statement", T___except, T___finally, NULL);
		return create_error_statement();
	}
	return statement;
end_error:
	return create_error_statement();
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
	do {
		if (token.kind != T_IDENTIFIER) {
			parse_error_expected("while parsing local label declaration",
				T_IDENTIFIER, NULL);
			goto end_error;
		}
		symbol_t *symbol = token.identifier.symbol;
		entity_t *entity = get_entity(symbol, NAMESPACE_LABEL);
		if (entity != NULL && entity->base.parent_scope == current_scope) {
			source_position_t const *const ppos = &entity->base.source_position;
			errorf(HERE, "multiple definitions of '%N' (previous definition %P)", entity, ppos);
		} else {
			entity = allocate_entity_zero(ENTITY_LOCAL_LABEL, NAMESPACE_LABEL, symbol);
			entity->base.parent_scope    = current_scope;
			entity->base.source_position = token.base.source_position;

			*anchor = entity;
			anchor  = &entity->base.next;
			end     = entity;

			environment_push(entity);
		}
		next_token();
	} while (next_if(','));
	expect(';', end_error);
end_error:
	statement->declaration.declarations_begin = begin;
	statement->declaration.declarations_end   = end;
	return statement;
}

static void parse_namespace_definition(void)
{
	eat(T_namespace);

	entity_t *entity = NULL;
	symbol_t *symbol = NULL;

	if (token.kind == T_IDENTIFIER) {
		symbol = token.identifier.symbol;
		next_token();

		entity = get_entity(symbol, NAMESPACE_NORMAL);
		if (entity != NULL
				&& entity->kind != ENTITY_NAMESPACE
				&& entity->base.parent_scope == current_scope) {
			if (is_entity_valid(entity)) {
				error_redefined_as_different_kind(&token.base.source_position,
						entity, ENTITY_NAMESPACE);
			}
			entity = NULL;
		}
	}

	if (entity == NULL) {
		entity = allocate_entity_zero(ENTITY_NAMESPACE, NAMESPACE_NORMAL, symbol);
		entity->base.source_position = token.base.source_position;
		entity->base.parent_scope    = current_scope;
	}

	if (token.kind == '=') {
		/* TODO: parse namespace alias */
		panic("namespace alias definition not supported yet");
	}

	environment_push(entity);
	append_entity(current_scope, entity);

	PUSH_SCOPE(&entity->namespacee.members);

	entity_t     *old_current_entity = current_entity;
	current_entity = entity;

	expect('{', end_error);
	parse_externals();
	expect('}', end_error);

end_error:
	assert(current_entity == entity);
	current_entity = old_current_entity;
	POP_SCOPE();
}

/**
 * Parse a statement.
 * There's also parse_statement() which additionally checks for
 * "statement has no effect" warnings
 */
static statement_t *intern_parse_statement(void)
{
	statement_t *statement = NULL;

	/* declaration or statement */
	add_anchor_token(';');
	switch (token.kind) {
	case T_IDENTIFIER: {
		token_kind_t la1_type = (token_kind_t)look_ahead(1)->kind;
		if (la1_type == ':') {
			statement = parse_label_statement();
		} else if (is_typedef_symbol(token.identifier.symbol)) {
			statement = parse_declaration_statement();
		} else {
			/* it's an identifier, the grammar says this must be an
			 * expression statement. However it is common that users mistype
			 * declaration types, so we guess a bit here to improve robustness
			 * for incorrect programs */
			switch (la1_type) {
			case '&':
			case '*':
				if (get_entity(token.identifier.symbol, NAMESPACE_NORMAL) != NULL) {
			default:
					statement = parse_expression_statement();
				} else {
			DECLARATION_START
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

	DECLARATION_START
		statement = parse_declaration_statement();
		break;

	case T___label__:
		statement = parse_local_label_declaration();
		break;

	case ';':         statement = parse_empty_statement();         break;
	case '{':         statement = parse_compound_statement(false); break;
	case T___leave:   statement = parse_leave_statement();         break;
	case T___try:     statement = parse_ms_try_statment();         break;
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

	EXPRESSION_START
		statement = parse_expression_statement();
		break;

	default:
		errorf(HERE, "unexpected token %K while parsing statement", &token);
		statement = create_error_statement();
		if (!at_anchor())
			next_token();
		break;
	}
	rem_anchor_token(';');

	assert(statement != NULL
			&& statement->base.source_position.input_name != NULL);

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
		if (!expression_has_effect(expression)) {
			warningf(WARN_UNUSED_VALUE, &expression->base.source_position, "statement has no effect");
		}
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
	add_anchor_token('{');
	add_anchor_token('~');
	add_anchor_token(T_CHARACTER_CONSTANT);
	add_anchor_token(T_COLONCOLON);
	add_anchor_token(T_FLOATINGPOINT);
	add_anchor_token(T_IDENTIFIER);
	add_anchor_token(T_INTEGER);
	add_anchor_token(T_MINUSMINUS);
	add_anchor_token(T_PLUSPLUS);
	add_anchor_token(T_STRING_LITERAL);
	add_anchor_token(T_WIDE_CHARACTER_CONSTANT);
	add_anchor_token(T_WIDE_STRING_LITERAL);
	add_anchor_token(T__Bool);
	add_anchor_token(T__Complex);
	add_anchor_token(T__Imaginary);
	add_anchor_token(T___FUNCTION__);
	add_anchor_token(T___PRETTY_FUNCTION__);
	add_anchor_token(T___alignof__);
	add_anchor_token(T___attribute__);
	add_anchor_token(T___builtin_va_start);
	add_anchor_token(T___extension__);
	add_anchor_token(T___func__);
	add_anchor_token(T___imag__);
	add_anchor_token(T___label__);
	add_anchor_token(T___real__);
	add_anchor_token(T___thread);
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
	while (token.kind != '}') {
		if (token.kind == T_EOF) {
			errorf(&statement->base.source_position,
			       "EOF while parsing compound statement");
			break;
		}
		statement_t *sub_statement = intern_parse_statement();
		if (sub_statement->kind == STATEMENT_ERROR) {
			/* an error occurred. if we are at an anchor, return */
			if (at_anchor())
				goto end_error;
			continue;
		}

		if (sub_statement->kind != STATEMENT_DECLARATION) {
			only_decls_so_far = false;
		} else if (!only_decls_so_far) {
			source_position_t const *const pos = &sub_statement->base.source_position;
			warningf(WARN_DECLARATION_AFTER_STATEMENT, pos, "ISO C90 forbids mixed declarations and code");
		}

		*anchor = sub_statement;

		while (sub_statement->base.next != NULL)
			sub_statement = sub_statement->base.next;

		anchor = &sub_statement->base.next;
	}
	next_token();

	/* look over all statements again to produce no effect warnings */
	if (is_warn_on(WARN_UNUSED_VALUE)) {
		statement_t *sub_statement = statement->compound.statements;
		for (; sub_statement != NULL; sub_statement = sub_statement->base.next) {
			if (sub_statement->kind != STATEMENT_EXPRESSION)
				continue;
			/* don't emit a warning for the last expression in an expression
			 * statement as it has always an effect */
			if (inside_expression_statement && sub_statement->base.next == NULL)
				continue;

			expression_t *expression = sub_statement->expression.expression;
			if (!expression_has_effect(expression)) {
				warningf(WARN_UNUSED_VALUE, &expression->base.source_position, "statement has no effect");
			}
		}
	}

end_error:
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
	rem_anchor_token(T___thread);
	rem_anchor_token(T___real__);
	rem_anchor_token(T___label__);
	rem_anchor_token(T___imag__);
	rem_anchor_token(T___func__);
	rem_anchor_token(T___extension__);
	rem_anchor_token(T___builtin_va_start);
	rem_anchor_token(T___attribute__);
	rem_anchor_token(T___alignof__);
	rem_anchor_token(T___PRETTY_FUNCTION__);
	rem_anchor_token(T___FUNCTION__);
	rem_anchor_token(T__Imaginary);
	rem_anchor_token(T__Complex);
	rem_anchor_token(T__Bool);
	rem_anchor_token(T_WIDE_STRING_LITERAL);
	rem_anchor_token(T_WIDE_CHARACTER_CONSTANT);
	rem_anchor_token(T_STRING_LITERAL);
	rem_anchor_token(T_PLUSPLUS);
	rem_anchor_token(T_MINUSMINUS);
	rem_anchor_token(T_INTEGER);
	rem_anchor_token(T_IDENTIFIER);
	rem_anchor_token(T_FLOATINGPOINT);
	rem_anchor_token(T_COLONCOLON);
	rem_anchor_token(T_CHARACTER_CONSTANT);
	rem_anchor_token('~');
	rem_anchor_token('{');
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

/**
 * Check for unused global static functions and variables
 */
static void check_unused_globals(void)
{
	if (!is_warn_on(WARN_UNUSED_FUNCTION) && !is_warn_on(WARN_UNUSED_VARIABLE))
		return;

	for (const entity_t *entity = file_scope->entities; entity != NULL;
	     entity = entity->base.next) {
		if (!is_declaration(entity))
			continue;

		const declaration_t *declaration = &entity->declaration;
		if (declaration->used                  ||
		    declaration->modifiers & DM_UNUSED ||
		    declaration->modifiers & DM_USED   ||
		    declaration->storage_class != STORAGE_CLASS_STATIC)
			continue;

		warning_t   why;
		char const *s;
		if (entity->kind == ENTITY_FUNCTION) {
			/* inhibit warning for static inline functions */
			if (entity->function.is_inline)
				continue;

			why = WARN_UNUSED_FUNCTION;
			s   = entity->function.statement != NULL ? "defined" : "declared";
		} else {
			why = WARN_UNUSED_VARIABLE;
			s   = "defined";
		}

		warningf(why, &declaration->base.source_position, "'%#N' %s but not used", entity, s);
	}
}

static void parse_global_asm(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_ASM);

	eat(T_asm);
	expect('(', end_error);

	statement->asms.asm_text = parse_string_literals();
	statement->base.next     = unit->global_asm;
	unit->global_asm         = statement;

	expect(')', end_error);
	expect(';', end_error);

end_error:;
}

static void parse_linkage_specification(void)
{
	eat(T_extern);

	source_position_t const pos     = *HERE;
	char const       *const linkage = parse_string_literals().begin;

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

	if (next_if('{')) {
		parse_externals();
		expect('}', end_error);
	} else {
		parse_external();
	}

end_error:
	assert(current_linkage == new_linkage);
	current_linkage = old_linkage;
}

static void parse_external(void)
{
	switch (token.kind) {
		case T_extern:
			if (look_ahead(1)->kind == T_STRING_LITERAL) {
				parse_linkage_specification();
			} else {
		DECLARATION_START_NO_EXTERN
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
			if (!strict_mode) {
				warningf(WARN_STRAY_SEMICOLON, HERE, "stray ';' outside of function");
				next_token();
				return;
			}
			/* FALLTHROUGH */

		default:
			errorf(HERE, "stray %K outside of function", &token);
			if (token.kind == '(' || token.kind == '{' || token.kind == '[')
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
	/* make a copy of the anchor set, so we can check if it is restored after parsing */
	unsigned short token_anchor_copy[T_LAST_TOKEN];
	memcpy(token_anchor_copy, token_anchor_set, sizeof(token_anchor_copy));
#endif

	while (token.kind != T_EOF && token.kind != '}') {
#ifndef NDEBUG
		for (int i = 0; i < T_LAST_TOKEN; ++i) {
			unsigned short count = token_anchor_set[i] - token_anchor_copy[i];
			if (count != 0) {
				/* the anchor set and its copy differs */
				internal_errorf(HERE, "Leaked anchor token %k %d times", i, count);
			}
		}
		if (in_gcc_extension) {
			/* an gcc extension scope was not closed */
			internal_errorf(HERE, "Leaked __extension__");
		}
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

		if (token.kind == T_EOF)
			break;

		errorf(HERE, "stray %K outside of function", &token);
		if (token.kind == '(' || token.kind == '{' || token.kind == '[')
			eat_until_matching_token(token.kind);
		next_token();
	}
}

void set_default_visibility(elf_visibility_tag_t visibility)
{
	default_visibility = visibility;
}

/**
 * Parse the input.
 *
 * @return  the translation unit or NULL if errors occurred.
 */
void start_parsing(void)
{
	environment_stack = NEW_ARR_F(stack_entry_t, 0);
	label_stack       = NEW_ARR_F(stack_entry_t, 0);
	diagnostic_count  = 0;
	error_count       = 0;
	warning_count     = 0;

	print_to_file(stderr);

	assert(unit == NULL);
	unit = allocate_ast_zero(sizeof(unit[0]));

	assert(file_scope == NULL);
	file_scope = &unit->scope;

	assert(current_scope == NULL);
	scope_push(&unit->scope);

	create_gnu_builtins();
	if (c_mode & _MS)
		create_microsoft_intrinsics();
}

translation_unit_t *finish_parsing(void)
{
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

		if (!is_type_incomplete(type))
			continue;

		source_position_t const *const pos = &decl->base.source_position;
		warningf(WARN_OTHER, pos, "array '%#N' assumed to have one element", (entity_t const*)decl);

		type_t *const new_type = duplicate_type(type);
		new_type->array.size_constant     = true;
		new_type->array.has_implicit_size = true;
		new_type->array.size              = 1;

		type_t *const result = identify_new_type(new_type);

		decl->type = result;
	}
}

void prepare_main_collect2(entity_t *entity)
{
	PUSH_SCOPE(&entity->function.statement->compound.scope);

	// create call to __main
	symbol_t *symbol         = symbol_table_insert("__main");
	entity_t *subsubmain_ent
		= create_implicit_function(symbol, &builtin_source_position);

	expression_t *ref         = allocate_expression_zero(EXPR_REFERENCE);
	type_t       *ftype       = subsubmain_ent->declaration.type;
	ref->base.source_position = builtin_source_position;
	ref->base.type            = make_pointer_type(ftype, TYPE_QUALIFIER_NONE);
	ref->reference.entity     = subsubmain_ent;

	expression_t *call = allocate_expression_zero(EXPR_CALL);
	call->base.source_position = builtin_source_position;
	call->base.type            = type_void;
	call->call.function        = ref;

	statement_t *expr_statement = allocate_statement_zero(STATEMENT_EXPRESSION);
	expr_statement->base.source_position  = builtin_source_position;
	expr_statement->expression.expression = call;

	statement_t *statement = entity->function.statement;
	assert(statement->kind == STATEMENT_COMPOUND);
	compound_statement_t *compounds = &statement->compound;

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
	current_linkage   = c_mode & _CXX ? LINKAGE_CXX : LINKAGE_C;
	incomplete_arrays = NEW_ARR_F(declaration_t*, 0);
	parse_translation_unit();
	complete_incomplete_arrays();
	DEL_ARR_F(incomplete_arrays);
	incomplete_arrays = NULL;
}

/**
 * Initialize the parser.
 */
void init_parser(void)
{
	sym_anonymous = symbol_table_insert("<anonymous>");

	memset(token_anchor_set, 0, sizeof(token_anchor_set));

	init_expression_parsers();
	obstack_init(&temp_obst);
}

/**
 * Terminate the parser.
 */
void exit_parser(void)
{
	obstack_free(&temp_obst, NULL);
}
