/*
 * This file is part of cparser.
 * Copyright (C) 2007-2008 Matthias Braun <matze@braunis.de>
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
#include "lang_features.h"
#include "walk_statements.h"
#include "warning.h"
#include "adt/bitfiddle.h"
#include "adt/error.h"
#include "adt/array.h"

//#define PRINT_TOKENS
#define MAX_LOOKAHEAD 2

typedef struct {
	entity_t           *old_entity;
	symbol_t           *symbol;
	entity_namespace_t  namespc;
} stack_entry_t;

typedef struct argument_list_t argument_list_t;
struct argument_list_t {
	long              argument;
	argument_list_t  *next;
};

typedef struct gnu_attribute_t gnu_attribute_t;
struct gnu_attribute_t {
	gnu_attribute_kind_t kind;           /**< The kind of the GNU attribute. */
	gnu_attribute_t     *next;
	bool                 invalid;        /**< Set if this attribute had argument errors, */
	bool                 have_arguments; /**< True, if this attribute has arguments. */
	union {
		size_t              value;
		string_t            string;
		atomic_type_kind_t  akind;
		long                argument;  /**< Single argument. */
		argument_list_t    *arguments; /**< List of argument expressions. */
	} u;
};

typedef struct declaration_specifiers_t  declaration_specifiers_t;
struct declaration_specifiers_t {
	source_position_t  source_position;
	storage_class_t    storage_class;
	unsigned char      alignment;         /**< Alignment, 0 if not set. */
	bool               is_inline    : 1;
	bool               thread_local : 1;  /**< GCC __thread */
	bool               deprecated   : 1;
	decl_modifiers_t   modifiers;         /**< declaration modifiers */
	gnu_attribute_t   *gnu_attributes;    /**< list of GNU attributes */
	const char        *deprecated_string; /**< can be set if declaration was marked deprecated. */
	symbol_t          *get_property_sym;  /**< the name of the get property if set. */
	symbol_t          *put_property_sym;  /**< the name of the put property if set. */
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

/**
 * Capture a MS __base extension.
 */
typedef struct based_spec_t {
	source_position_t  source_position;
	variable_t        *base_variable;
} based_spec_t;

typedef entity_t* (*parsed_declaration_func) (entity_t *declaration, bool is_definition);

/** The current token. */
static token_t              token;
/** The lookahead ring-buffer. */
static token_t              lookahead_buffer[MAX_LOOKAHEAD];
/** Position of the next token in the lookahead buffer. */
static int                  lookahead_bufpos;
static stack_entry_t       *environment_stack = NULL;
static stack_entry_t       *label_stack       = NULL;
static scope_t             *file_scope        = NULL;
static scope_t             *current_scope     = NULL;
/** Point to the current function declaration if inside a function. */
static function_t          *current_function  = NULL;
static entity_t            *current_init_decl = NULL;
static switch_statement_t  *current_switch    = NULL;
static statement_t         *current_loop      = NULL;
static statement_t         *current_parent    = NULL;
static ms_try_statement_t  *current_try       = NULL;
static linkage_kind_t       current_linkage   = LINKAGE_INVALID;
static goto_statement_t    *goto_first        = NULL;
static goto_statement_t   **goto_anchor       = NULL;
static label_statement_t   *label_first       = NULL;
static label_statement_t  **label_anchor      = NULL;
/** current translation unit. */
static translation_unit_t  *unit              = NULL;
/** true if we are in a type property context (evaluation only for type. */
static bool                 in_type_prop      = false;
/** true in we are in a __extension__ context. */
static bool                 in_gcc_extension  = false;
static struct obstack       temp_obst;
static entity_t            *anonymous_entity;
static declaration_t      **incomplete_arrays;


#define PUSH_PARENT(stmt)                          \
	statement_t *const prev_parent = current_parent; \
	((void)(current_parent = (stmt)))
#define POP_PARENT ((void)(current_parent = prev_parent))

/** special symbol used for anonymous entities. */
static const symbol_t *sym_anonymous = NULL;

/* symbols for Microsoft extended-decl-modifier */
static const symbol_t *sym_align      = NULL;
static const symbol_t *sym_allocate   = NULL;
static const symbol_t *sym_dllimport  = NULL;
static const symbol_t *sym_dllexport  = NULL;
static const symbol_t *sym_naked      = NULL;
static const symbol_t *sym_noinline   = NULL;
static const symbol_t *sym_noreturn   = NULL;
static const symbol_t *sym_nothrow    = NULL;
static const symbol_t *sym_novtable   = NULL;
static const symbol_t *sym_property   = NULL;
static const symbol_t *sym_get        = NULL;
static const symbol_t *sym_put        = NULL;
static const symbol_t *sym_selectany  = NULL;
static const symbol_t *sym_thread     = NULL;
static const symbol_t *sym_uuid       = NULL;
static const symbol_t *sym_deprecated = NULL;
static const symbol_t *sym_restrict   = NULL;
static const symbol_t *sym_noalias    = NULL;

/** The token anchor set */
static unsigned char token_anchor_set[T_LAST_TOKEN];

/** The current source position. */
#define HERE (&token.source_position)

/** true if we are in GCC mode. */
#define GNU_MODE ((c_mode & _GNUC) || in_gcc_extension)

static type_t *type_valist;

static statement_t *parse_compound_statement(bool inside_expression_statement);
static statement_t *parse_statement(void);

static expression_t *parse_sub_expression(precedence_t);
static expression_t *parse_expression(void);
static type_t       *parse_typename(void);
static void          parse_externals(void);
static void          parse_external(void);

static void parse_compound_type_entries(compound_t *compound_declaration);

typedef enum declarator_flags_t {
	DECL_FLAGS_NONE             = 0,
	DECL_MAY_BE_ABSTRACT        = 1U << 0,
	DECL_CREATE_COMPOUND_MEMBER = 1U << 1,
	DECL_IS_PARAMETER           = 1U << 2
} declarator_flags_t;

static entity_t *parse_declarator(const declaration_specifiers_t *specifiers,
                                  declarator_flags_t flags);

static entity_t *record_entity(entity_t *entity, bool is_definition);

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

#define TYPENAME_START      \
	TYPE_QUALIFIERS         \
	TYPE_SPECIFIERS

#define EXPRESSION_START           \
	case '!':                        \
	case '&':                        \
	case '(':                        \
	case '*':                        \
	case '+':                        \
	case '-':                        \
	case '~':                        \
	case T_ANDAND:                   \
	case T_CHARACTER_CONSTANT:       \
	case T_FLOATINGPOINT:            \
	case T_INTEGER:                  \
	case T_MINUSMINUS:               \
	case T_PLUSPLUS:                 \
	case T_STRING_LITERAL:           \
	case T_WIDE_CHARACTER_CONSTANT:  \
	case T_WIDE_STRING_LITERAL:      \
	case T___FUNCDNAME__:            \
	case T___FUNCSIG__:              \
	case T___FUNCTION__:             \
	case T___PRETTY_FUNCTION__:      \
	case T___alignof__:              \
	case T___builtin_alloca:         \
	case T___builtin_classify_type:  \
	case T___builtin_constant_p:     \
	case T___builtin_expect:         \
	case T___builtin_huge_val:       \
	case T___builtin_inf:            \
	case T___builtin_inff:           \
	case T___builtin_infl:           \
	case T___builtin_isgreater:      \
	case T___builtin_isgreaterequal: \
	case T___builtin_isless:         \
	case T___builtin_islessequal:    \
	case T___builtin_islessgreater:  \
	case T___builtin_isunordered:    \
	case T___builtin_nan:            \
	case T___builtin_nanf:           \
	case T___builtin_nanl:           \
	case T___builtin_offsetof:       \
	case T___builtin_prefetch:       \
	case T___builtin_va_arg:         \
	case T___builtin_va_end:         \
	case T___builtin_va_start:       \
	case T___func__:                 \
	case T___noop:                   \
	case T__assume:                  \
	case T_delete:                   \
	case T_false:                    \
	case T_sizeof:                   \
	case T_throw:                    \
	case T_true:

/**
 * Allocate an AST node with given size and
 * initialize all fields with zero.
 */
static void *allocate_ast_zero(size_t size)
{
	void *res = allocate_ast(size);
	memset(res, 0, size);
	return res;
}

/**
 * Returns the size of an entity node.
 *
 * @param kind  the entity kind
 */
static size_t get_entity_struct_size(entity_kind_t kind)
{
	static const size_t sizes[] = {
		[ENTITY_VARIABLE]        = sizeof(variable_t),
		[ENTITY_PARAMETER]       = sizeof(parameter_t),
		[ENTITY_COMPOUND_MEMBER] = sizeof(compound_member_t),
		[ENTITY_FUNCTION]        = sizeof(function_t),
		[ENTITY_TYPEDEF]         = sizeof(typedef_t),
		[ENTITY_STRUCT]          = sizeof(compound_t),
		[ENTITY_UNION]           = sizeof(compound_t),
		[ENTITY_ENUM]            = sizeof(enum_t),
		[ENTITY_ENUM_VALUE]      = sizeof(enum_value_t),
		[ENTITY_LABEL]           = sizeof(label_t),
		[ENTITY_LOCAL_LABEL]     = sizeof(label_t),
		[ENTITY_NAMESPACE]       = sizeof(namespace_t)
	};
	assert(kind < sizeof(sizes) / sizeof(sizes[0]));
	assert(sizes[kind] != 0);
	return sizes[kind];
}

/**
 * Allocate an entity of given kind and initialize all
 * fields with zero.
 */
static entity_t *allocate_entity_zero(entity_kind_t kind)
{
	size_t    size   = get_entity_struct_size(kind);
	entity_t *entity = allocate_ast_zero(size);
	entity->kind     = kind;
	return entity;
}

/**
 * Returns the size of a statement node.
 *
 * @param kind  the statement kind
 */
static size_t get_statement_struct_size(statement_kind_t kind)
{
	static const size_t sizes[] = {
		[STATEMENT_INVALID]     = sizeof(invalid_statement_t),
		[STATEMENT_EMPTY]       = sizeof(empty_statement_t),
		[STATEMENT_COMPOUND]    = sizeof(compound_statement_t),
		[STATEMENT_RETURN]      = sizeof(return_statement_t),
		[STATEMENT_DECLARATION] = sizeof(declaration_statement_t),
		[STATEMENT_LOCAL_LABEL] = sizeof(local_label_statement_t),
		[STATEMENT_IF]          = sizeof(if_statement_t),
		[STATEMENT_SWITCH]      = sizeof(switch_statement_t),
		[STATEMENT_EXPRESSION]  = sizeof(expression_statement_t),
		[STATEMENT_CONTINUE]    = sizeof(statement_base_t),
		[STATEMENT_BREAK]       = sizeof(statement_base_t),
		[STATEMENT_GOTO]        = sizeof(goto_statement_t),
		[STATEMENT_LABEL]       = sizeof(label_statement_t),
		[STATEMENT_CASE_LABEL]  = sizeof(case_label_statement_t),
		[STATEMENT_WHILE]       = sizeof(while_statement_t),
		[STATEMENT_DO_WHILE]    = sizeof(do_while_statement_t),
		[STATEMENT_FOR]         = sizeof(for_statement_t),
		[STATEMENT_ASM]         = sizeof(asm_statement_t),
		[STATEMENT_MS_TRY]      = sizeof(ms_try_statement_t),
		[STATEMENT_LEAVE]       = sizeof(leave_statement_t)
	};
	assert(kind < sizeof(sizes) / sizeof(sizes[0]));
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
		[EXPR_INVALID]                 = sizeof(expression_base_t),
		[EXPR_REFERENCE]               = sizeof(reference_expression_t),
		[EXPR_REFERENCE_ENUM_VALUE]    = sizeof(reference_expression_t),
		[EXPR_CONST]                   = sizeof(const_expression_t),
		[EXPR_CHARACTER_CONSTANT]      = sizeof(const_expression_t),
		[EXPR_WIDE_CHARACTER_CONSTANT] = sizeof(const_expression_t),
		[EXPR_STRING_LITERAL]          = sizeof(string_literal_expression_t),
		[EXPR_WIDE_STRING_LITERAL]     = sizeof(wide_string_literal_expression_t),
		[EXPR_COMPOUND_LITERAL]        = sizeof(compound_literal_expression_t),
		[EXPR_CALL]                    = sizeof(call_expression_t),
		[EXPR_UNARY_FIRST]             = sizeof(unary_expression_t),
		[EXPR_BINARY_FIRST]            = sizeof(binary_expression_t),
		[EXPR_CONDITIONAL]             = sizeof(conditional_expression_t),
		[EXPR_SELECT]                  = sizeof(select_expression_t),
		[EXPR_ARRAY_ACCESS]            = sizeof(array_access_expression_t),
		[EXPR_SIZEOF]                  = sizeof(typeprop_expression_t),
		[EXPR_ALIGNOF]                 = sizeof(typeprop_expression_t),
		[EXPR_CLASSIFY_TYPE]           = sizeof(classify_type_expression_t),
		[EXPR_FUNCNAME]                = sizeof(funcname_expression_t),
		[EXPR_BUILTIN_SYMBOL]          = sizeof(builtin_symbol_expression_t),
		[EXPR_BUILTIN_CONSTANT_P]      = sizeof(builtin_constant_expression_t),
		[EXPR_BUILTIN_PREFETCH]        = sizeof(builtin_prefetch_expression_t),
		[EXPR_OFFSETOF]                = sizeof(offsetof_expression_t),
		[EXPR_VA_START]                = sizeof(va_start_expression_t),
		[EXPR_VA_ARG]                  = sizeof(va_arg_expression_t),
		[EXPR_STATEMENT]               = sizeof(statement_expression_t),
		[EXPR_LABEL_ADDRESS]           = sizeof(label_address_expression_t),
	};
	if (kind >= EXPR_UNARY_FIRST && kind <= EXPR_UNARY_LAST) {
		return sizes[EXPR_UNARY_FIRST];
	}
	if (kind >= EXPR_BINARY_FIRST && kind <= EXPR_BINARY_LAST) {
		return sizes[EXPR_BINARY_FIRST];
	}
	assert(kind < sizeof(sizes) / sizeof(sizes[0]));
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
	res->base.source_position = token.source_position;
	return res;
}

/**
 * Allocate an expression node of given kind and initialize all
 * fields with zero.
 */
static expression_t *allocate_expression_zero(expression_kind_t kind)
{
	size_t        size = get_expression_struct_size(kind);
	expression_t *res  = allocate_ast_zero(size);

	res->base.kind            = kind;
	res->base.type            = type_error_type;
	res->base.source_position = token.source_position;
	return res;
}

/**
 * Creates a new invalid expression at the source position
 * of the current token.
 */
static expression_t *create_invalid_expression(void)
{
	return allocate_expression_zero(EXPR_INVALID);
}

/**
 * Creates a new invalid statement.
 */
static statement_t *create_invalid_statement(void)
{
	return allocate_statement_zero(STATEMENT_INVALID);
}

/**
 * Allocate a new empty statement.
 */
static statement_t *create_empty_statement(void)
{
	return allocate_statement_zero(STATEMENT_EMPTY);
}

/**
 * Returns the size of a type node.
 *
 * @param kind  the type kind
 */
static size_t get_type_struct_size(type_kind_t kind)
{
	static const size_t sizes[] = {
		[TYPE_ATOMIC]          = sizeof(atomic_type_t),
		[TYPE_COMPLEX]         = sizeof(complex_type_t),
		[TYPE_IMAGINARY]       = sizeof(imaginary_type_t),
		[TYPE_BITFIELD]        = sizeof(bitfield_type_t),
		[TYPE_COMPOUND_STRUCT] = sizeof(compound_type_t),
		[TYPE_COMPOUND_UNION]  = sizeof(compound_type_t),
		[TYPE_ENUM]            = sizeof(enum_type_t),
		[TYPE_FUNCTION]        = sizeof(function_type_t),
		[TYPE_POINTER]         = sizeof(pointer_type_t),
		[TYPE_ARRAY]           = sizeof(array_type_t),
		[TYPE_BUILTIN]         = sizeof(builtin_type_t),
		[TYPE_TYPEDEF]         = sizeof(typedef_type_t),
		[TYPE_TYPEOF]          = sizeof(typeof_type_t),
	};
	assert(sizeof(sizes) / sizeof(sizes[0]) == (int) TYPE_TYPEOF + 1);
	assert(kind <= TYPE_TYPEOF);
	assert(sizes[kind] != 0);
	return sizes[kind];
}

/**
 * Allocate a type node of given kind and initialize all
 * fields with zero.
 *
 * @param kind             type kind to allocate
 */
static type_t *allocate_type_zero(type_kind_t kind)
{
	size_t  size = get_type_struct_size(kind);
	type_t *res  = obstack_alloc(type_obst, size);
	memset(res, 0, size);
	res->base.kind = kind;

	return res;
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
	assert(kind < sizeof(sizes) / sizeof(*sizes));
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
 * Free a type from the type obstack.
 */
static void free_type(void *type)
{
	obstack_free(type_obst, type);
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

	lookahead_bufpos = (lookahead_bufpos+1) % MAX_LOOKAHEAD;

#ifdef PRINT_TOKENS
	print_token(stderr, &token);
	fprintf(stderr, "\n");
#endif
}

/**
 * Return the next token with a given lookahead.
 */
static inline const token_t *look_ahead(int num)
{
	assert(num > 0 && num <= MAX_LOOKAHEAD);
	int pos = (lookahead_bufpos+num-1) % MAX_LOOKAHEAD;
	return &lookahead_buffer[pos];
}

/**
 * Adds a token type to the token type anchor set (a multi-set).
 */
static void add_anchor_token(int token_type)
{
	assert(0 <= token_type && token_type < T_LAST_TOKEN);
	++token_anchor_set[token_type];
}

/**
 * Set the number of tokens types of the given type
 * to zero and return the old count.
 */
static int save_and_reset_anchor_state(int token_type)
{
	assert(0 <= token_type && token_type < T_LAST_TOKEN);
	int count = token_anchor_set[token_type];
	token_anchor_set[token_type] = 0;
	return count;
}

/**
 * Restore the number of token types to the given count.
 */
static void restore_anchor_state(int token_type, int count)
{
	assert(0 <= token_type && token_type < T_LAST_TOKEN);
	token_anchor_set[token_type] = count;
}

/**
 * Remove a token type from the token type anchor set (a multi-set).
 */
static void rem_anchor_token(int token_type)
{
	assert(0 <= token_type && token_type < T_LAST_TOKEN);
	assert(token_anchor_set[token_type] != 0);
	--token_anchor_set[token_type];
}

/**
 * Return true if the token type of the current token is
 * in the anchor set.
 */
static bool at_anchor(void)
{
	if (token.type < 0)
		return false;
	return token_anchor_set[token.type];
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
	while (token.type        != end_token ||
	       parenthesis_count != 0         ||
	       brace_count       != 0         ||
	       bracket_count     != 0) {
		switch (token.type) {
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
			if (token.type        == end_token &&
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
	while (token_anchor_set[token.type] == 0) {
		if (token.type == '(' || token.type == '{' || token.type == '[')
			eat_until_matching_token(token.type);
		next_token();
	}
}

/**
 * Eat a whole block from input tokens.
 */
static void eat_block(void)
{
	eat_until_matching_token('{');
	if (token.type == '}')
		next_token();
}

#define eat(token_type)  do { assert(token.type == (token_type)); next_token(); } while (0)

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
 * and goto the end_error label.
 */
#define expect(expected)                                  \
	do {                                                  \
		if (UNLIKELY(token.type != (expected))) {         \
			parse_error_expected(NULL, (expected), NULL); \
			add_anchor_token(expected);                   \
			eat_until_anchor();                           \
			if (token.type == expected)                   \
				next_token();                             \
			rem_anchor_token(expected);                   \
			goto end_error;                               \
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
		if (entity->base.namespc == namespc)
			return entity;
	}

	return NULL;
}

/**
 * pushs an entity on the environment stack and links the corresponding symbol
 * it.
 */
static void stack_push(stack_entry_t **stack_ptr, entity_t *entity)
{
	symbol_t           *symbol  = entity->base.symbol;
	entity_namespace_t  namespc = entity->base.namespc;
	assert(namespc != NAMESPACE_INVALID);

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

	ARR_SHRINKLEN(*stack_ptr, (int) new_top);
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

static int get_akind_rank(atomic_type_kind_t akind)
{
	return (int) akind;
}

/**
 * Return the type rank for an atomic type.
 */
static int get_rank(const type_t *type)
{
	assert(!is_typeref(type));
	/* The C-standard allows promoting enums to int or unsigned int (see § 7.2.2
	 * and esp. footnote 108). However we can't fold constants (yet), so we
	 * can't decide whether unsigned int is possible, while int always works.
	 * (unsigned int would be preferable when possible... for stuff like
	 *  struct { enum { ... } bla : 4; } ) */
	if (type->kind == TYPE_ENUM)
		return get_akind_rank(ATOMIC_TYPE_INT);

	assert(type->kind == TYPE_ATOMIC);
	return get_akind_rank(type->atomic.akind);
}

/**
 * Do integer promotion for a given type.
 *
 * @param type  the type to promote
 * @return the promoted type
 */
static type_t *promote_integer(type_t *type)
{
	if (type->kind == TYPE_BITFIELD)
		type = type->bitfield.base_type;

	if (get_rank(type) < get_akind_rank(ATOMIC_TYPE_INT))
		type = type_int;

	return type;
}

/**
 * Create a cast expression.
 *
 * @param expression  the expression to cast
 * @param dest_type   the destination type
 */
static expression_t *create_cast_expression(expression_t *expression,
                                            type_t *dest_type)
{
	expression_t *cast = allocate_expression_zero(EXPR_UNARY_CAST_IMPLICIT);

	cast->unary.value = expression;
	cast->base.type   = dest_type;

	return cast;
}

/**
 * Check if a given expression represents a null pointer constant.
 *
 * @param expression  the expression to check
 */
static bool is_null_pointer_constant(const expression_t *expression)
{
	/* skip void* cast */
	if (expression->kind == EXPR_UNARY_CAST
			|| expression->kind == EXPR_UNARY_CAST_IMPLICIT) {
		expression = expression->unary.value;
	}

	/* TODO: not correct yet, should be any constant integer expression
	 * which evaluates to 0 */
	if (expression->kind != EXPR_CONST)
		return false;

	type_t *const type = skip_typeref(expression->base.type);
	if (!is_type_integer(type))
		return false;

	return expression->conste.v.int_value == 0;
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

	return create_cast_expression(expression, dest_type);
}

typedef enum assign_error_t {
	ASSIGN_SUCCESS,
	ASSIGN_ERROR_INCOMPATIBLE,
	ASSIGN_ERROR_POINTER_QUALIFIER_MISSING,
	ASSIGN_WARNING_POINTER_INCOMPATIBLE,
	ASSIGN_WARNING_POINTER_FROM_INT,
	ASSIGN_WARNING_INT_FROM_POINTER
} assign_error_t;

static void report_assign_error(assign_error_t error, type_t *orig_type_left,
                                const expression_t *const right,
                                const char *context,
                                const source_position_t *source_position)
{
	type_t *const orig_type_right = right->base.type;
	type_t *const type_left       = skip_typeref(orig_type_left);
	type_t *const type_right      = skip_typeref(orig_type_right);

	switch (error) {
	case ASSIGN_SUCCESS:
		return;
	case ASSIGN_ERROR_INCOMPATIBLE:
		errorf(source_position,
		       "destination type '%T' in %s is incompatible with type '%T'",
		       orig_type_left, context, orig_type_right);
		return;

	case ASSIGN_ERROR_POINTER_QUALIFIER_MISSING: {
		if (warning.other) {
			type_t *points_to_left  = skip_typeref(type_left->pointer.points_to);
			type_t *points_to_right = skip_typeref(type_right->pointer.points_to);

			/* the left type has all qualifiers from the right type */
			unsigned missing_qualifiers
				= points_to_right->base.qualifiers & ~points_to_left->base.qualifiers;
			warningf(source_position,
					"destination type '%T' in %s from type '%T' lacks qualifiers '%Q' in pointer target type",
					orig_type_left, context, orig_type_right, missing_qualifiers);
		}
		return;
	}

	case ASSIGN_WARNING_POINTER_INCOMPATIBLE:
		if (warning.other) {
			warningf(source_position,
					"destination type '%T' in %s is incompatible with '%E' of type '%T'",
					orig_type_left, context, right, orig_type_right);
		}
		return;

	case ASSIGN_WARNING_POINTER_FROM_INT:
		if (warning.other) {
			warningf(source_position,
					"%s makes pointer '%T' from integer '%T' without a cast",
					context, orig_type_left, orig_type_right);
		}
		return;

	case ASSIGN_WARNING_INT_FROM_POINTER:
		if (warning.other) {
			warningf(source_position,
					"%s makes integer '%T' from pointer '%T' without a cast",
					context, orig_type_left, orig_type_right);
		}
		return;

	default:
		panic("invalid error value");
	}
}

/** Implements the rules from § 6.5.16.1 */
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

			if (is_type_atomic(points_to_left, ATOMIC_TYPE_VOID))
				return res;

			if (is_type_atomic(points_to_right, ATOMIC_TYPE_VOID)) {
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
	} else if ((is_type_compound(type_left)  && is_type_compound(type_right))
			|| (is_type_builtin(type_left) && is_type_builtin(type_right))) {
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
	expression_t *result = parse_sub_expression(PREC_CONDITIONAL);

	if (!is_constant_expression(result)) {
		errorf(&result->base.source_position,
		       "expression '%E' is not constant", result);
	}

	return result;
}

static expression_t *parse_assignment_expression(void)
{
	return parse_sub_expression(PREC_ASSIGNMENT);
}

static string_t parse_string_literals(void)
{
	assert(token.type == T_STRING_LITERAL);
	string_t result = token.v.string;

	next_token();

	while (token.type == T_STRING_LITERAL) {
		result = concat_strings(&result, &token.v.string);
		next_token();
	}

	return result;
}

static const char *const gnu_attribute_names[GNU_AK_LAST] = {
	[GNU_AK_CONST]                  = "const",
	[GNU_AK_VOLATILE]               = "volatile",
	[GNU_AK_CDECL]                  = "cdecl",
	[GNU_AK_STDCALL]                = "stdcall",
	[GNU_AK_FASTCALL]               = "fastcall",
	[GNU_AK_DEPRECATED]             = "deprecated",
	[GNU_AK_NOINLINE]               = "noinline",
	[GNU_AK_NORETURN]               = "noreturn",
	[GNU_AK_NAKED]                  = "naked",
	[GNU_AK_PURE]                   = "pure",
	[GNU_AK_ALWAYS_INLINE]          = "always_inline",
	[GNU_AK_MALLOC]                 = "malloc",
	[GNU_AK_WEAK]                   = "weak",
	[GNU_AK_CONSTRUCTOR]            = "constructor",
	[GNU_AK_DESTRUCTOR]             = "destructor",
	[GNU_AK_NOTHROW]                = "nothrow",
	[GNU_AK_TRANSPARENT_UNION]      = "transparent_union",
	[GNU_AK_COMMON]                 = "common",
	[GNU_AK_NOCOMMON]               = "nocommon",
	[GNU_AK_PACKED]                 = "packed",
	[GNU_AK_SHARED]                 = "shared",
	[GNU_AK_NOTSHARED]              = "notshared",
	[GNU_AK_USED]                   = "used",
	[GNU_AK_UNUSED]                 = "unused",
	[GNU_AK_NO_INSTRUMENT_FUNCTION] = "no_instrument_function",
	[GNU_AK_WARN_UNUSED_RESULT]     = "warn_unused_result",
	[GNU_AK_LONGCALL]               = "longcall",
	[GNU_AK_SHORTCALL]              = "shortcall",
	[GNU_AK_LONG_CALL]              = "long_call",
	[GNU_AK_SHORT_CALL]             = "short_call",
	[GNU_AK_FUNCTION_VECTOR]        = "function_vector",
	[GNU_AK_INTERRUPT]              = "interrupt",
	[GNU_AK_INTERRUPT_HANDLER]      = "interrupt_handler",
	[GNU_AK_NMI_HANDLER]            = "nmi_handler",
	[GNU_AK_NESTING]                = "nesting",
	[GNU_AK_NEAR]                   = "near",
	[GNU_AK_FAR]                    = "far",
	[GNU_AK_SIGNAL]                 = "signal",
	[GNU_AK_EIGTHBIT_DATA]          = "eightbit_data",
	[GNU_AK_TINY_DATA]              = "tiny_data",
	[GNU_AK_SAVEALL]                = "saveall",
	[GNU_AK_FLATTEN]                = "flatten",
	[GNU_AK_SSEREGPARM]             = "sseregparm",
	[GNU_AK_EXTERNALLY_VISIBLE]     = "externally_visible",
	[GNU_AK_RETURN_TWICE]           = "return_twice",
	[GNU_AK_MAY_ALIAS]              = "may_alias",
	[GNU_AK_MS_STRUCT]              = "ms_struct",
	[GNU_AK_GCC_STRUCT]             = "gcc_struct",
	[GNU_AK_DLLIMPORT]              = "dllimport",
	[GNU_AK_DLLEXPORT]              = "dllexport",
	[GNU_AK_ALIGNED]                = "aligned",
	[GNU_AK_ALIAS]                  = "alias",
	[GNU_AK_SECTION]                = "section",
	[GNU_AK_FORMAT]                 = "format",
	[GNU_AK_FORMAT_ARG]             = "format_arg",
	[GNU_AK_WEAKREF]                = "weakref",
	[GNU_AK_NONNULL]                = "nonnull",
	[GNU_AK_TLS_MODEL]              = "tls_model",
	[GNU_AK_VISIBILITY]             = "visibility",
	[GNU_AK_REGPARM]                = "regparm",
	[GNU_AK_MODE]                   = "mode",
	[GNU_AK_MODEL]                  = "model",
	[GNU_AK_TRAP_EXIT]              = "trap_exit",
	[GNU_AK_SP_SWITCH]              = "sp_switch",
	[GNU_AK_SENTINEL]               = "sentinel"
};

/**
 * compare two string, ignoring double underscores on the second.
 */
static int strcmp_underscore(const char *s1, const char *s2)
{
	if (s2[0] == '_' && s2[1] == '_') {
		size_t len2 = strlen(s2);
		size_t len1 = strlen(s1);
		if (len1 == len2-4 && s2[len2-2] == '_' && s2[len2-1] == '_') {
			return strncmp(s1, s2+2, len2-4);
		}
	}

	return strcmp(s1, s2);
}

/**
 * Allocate a new gnu temporal attribute of given kind.
 */
static gnu_attribute_t *allocate_gnu_attribute(gnu_attribute_kind_t kind)
{
	gnu_attribute_t *attribute = obstack_alloc(&temp_obst, sizeof(*attribute));
	attribute->kind            = kind;
	attribute->next            = NULL;
	attribute->invalid         = false;
	attribute->have_arguments  = false;

	return attribute;
}

/**
 * Parse one constant expression argument of the given attribute.
 */
static void parse_gnu_attribute_const_arg(gnu_attribute_t *attribute)
{
	expression_t *expression;
	add_anchor_token(')');
	expression = parse_constant_expression();
	rem_anchor_token(')');
	expect(')');
	attribute->u.argument = fold_constant(expression);
	return;
end_error:
	attribute->invalid = true;
}

/**
 * Parse a list of constant expressions arguments of the given attribute.
 */
static void parse_gnu_attribute_const_arg_list(gnu_attribute_t *attribute)
{
	argument_list_t **list = &attribute->u.arguments;
	argument_list_t  *entry;
	expression_t     *expression;
	add_anchor_token(')');
	add_anchor_token(',');
	while (true) {
		expression = parse_constant_expression();
		entry = obstack_alloc(&temp_obst, sizeof(entry));
		entry->argument = fold_constant(expression);
		entry->next     = NULL;
		*list = entry;
		list = &entry->next;
		if (token.type != ',')
			break;
		next_token();
	}
	rem_anchor_token(',');
	rem_anchor_token(')');
	expect(')');
	return;
end_error:
	attribute->invalid = true;
}

/**
 * Parse one string literal argument of the given attribute.
 */
static void parse_gnu_attribute_string_arg(gnu_attribute_t *attribute,
                                           string_t *string)
{
	add_anchor_token('(');
	if (token.type != T_STRING_LITERAL) {
		parse_error_expected("while parsing attribute directive",
		                     T_STRING_LITERAL, NULL);
		goto end_error;
	}
	*string = parse_string_literals();
	rem_anchor_token('(');
	expect(')');
	return;
end_error:
	attribute->invalid = true;
}

/**
 * Parse one tls model of the given attribute.
 */
static void parse_gnu_attribute_tls_model_arg(gnu_attribute_t *attribute)
{
	static const char *const tls_models[] = {
		"global-dynamic",
		"local-dynamic",
		"initial-exec",
		"local-exec"
	};
	string_t string = { NULL, 0 };
	parse_gnu_attribute_string_arg(attribute, &string);
	if (string.begin != NULL) {
		for (size_t i = 0; i < 4; ++i) {
			if (strcmp(tls_models[i], string.begin) == 0) {
				attribute->u.value = i;
				return;
			}
		}
		errorf(HERE, "'%s' is an unrecognized tls model", string.begin);
	}
	attribute->invalid = true;
}

/**
 * Parse one tls model of the given attribute.
 */
static void parse_gnu_attribute_visibility_arg(gnu_attribute_t *attribute)
{
	static const char *const visibilities[] = {
		"default",
		"protected",
		"hidden",
		"internal"
	};
	string_t string = { NULL, 0 };
	parse_gnu_attribute_string_arg(attribute, &string);
	if (string.begin != NULL) {
		for (size_t i = 0; i < 4; ++i) {
			if (strcmp(visibilities[i], string.begin) == 0) {
				attribute->u.value = i;
				return;
			}
		}
		errorf(HERE, "'%s' is an unrecognized visibility", string.begin);
	}
	attribute->invalid = true;
}

/**
 * Parse one (code) model of the given attribute.
 */
static void parse_gnu_attribute_model_arg(gnu_attribute_t *attribute)
{
	static const char *const visibilities[] = {
		"small",
		"medium",
		"large"
	};
	string_t string = { NULL, 0 };
	parse_gnu_attribute_string_arg(attribute, &string);
	if (string.begin != NULL) {
		for (int i = 0; i < 3; ++i) {
			if (strcmp(visibilities[i], string.begin) == 0) {
				attribute->u.value = i;
				return;
			}
		}
		errorf(HERE, "'%s' is an unrecognized model", string.begin);
	}
	attribute->invalid = true;
}

/**
 * Parse one mode of the given attribute.
 */
static void parse_gnu_attribute_mode_arg(gnu_attribute_t *attribute)
{
	/* TODO: find out what is allowed here... */

	/* at least: byte, word, pointer, list of machine modes
	 * __XXX___ is interpreted as XXX */
	add_anchor_token(')');

	if (token.type != T_IDENTIFIER) {
		expect(T_IDENTIFIER);
	}

	/* This isn't really correct, the backend should provide a list of machine
	 * specific modes (according to gcc philosophy that is...) */
	const char *symbol_str = token.v.symbol->string;
	if (strcmp_underscore("QI",   symbol_str) == 0 ||
	    strcmp_underscore("byte", symbol_str) == 0) {
		attribute->u.akind = ATOMIC_TYPE_CHAR;
	} else if (strcmp_underscore("HI", symbol_str) == 0) {
		attribute->u.akind = ATOMIC_TYPE_SHORT;
	} else if (strcmp_underscore("SI",      symbol_str) == 0
	        || strcmp_underscore("word",    symbol_str) == 0
	        || strcmp_underscore("pointer", symbol_str) == 0) {
		attribute->u.akind = ATOMIC_TYPE_INT;
	} else if (strcmp_underscore("DI", symbol_str) == 0) {
		attribute->u.akind = ATOMIC_TYPE_LONGLONG;
	} else {
		if (warning.other)
			warningf(HERE, "ignoring unknown mode '%s'", symbol_str);
		attribute->invalid = true;
	}
	next_token();

	rem_anchor_token(')');
	expect(')');
	return;
end_error:
	attribute->invalid = true;
}

/**
 * Parse one interrupt argument of the given attribute.
 */
static void parse_gnu_attribute_interrupt_arg(gnu_attribute_t *attribute)
{
	static const char *const interrupts[] = {
		"IRQ",
		"FIQ",
		"SWI",
		"ABORT",
		"UNDEF"
	};
	string_t string = { NULL, 0 };
	parse_gnu_attribute_string_arg(attribute, &string);
	if (string.begin != NULL) {
		for (size_t i = 0; i < 5; ++i) {
			if (strcmp(interrupts[i], string.begin) == 0) {
				attribute->u.value = i;
				return;
			}
		}
		errorf(HERE, "'%s' is not an interrupt", string.begin);
	}
	attribute->invalid = true;
}

/**
 * Parse ( identifier, const expression, const expression )
 */
static void parse_gnu_attribute_format_args(gnu_attribute_t *attribute)
{
	static const char *const format_names[] = {
		"printf",
		"scanf",
		"strftime",
		"strfmon"
	};
	int i;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("while parsing format attribute directive", T_IDENTIFIER, NULL);
		goto end_error;
	}
	const char *name = token.v.symbol->string;
	for (i = 0; i < 4; ++i) {
		if (strcmp_underscore(format_names[i], name) == 0)
			break;
	}
	if (i >= 4) {
		if (warning.attribute)
			warningf(HERE, "'%s' is an unrecognized format function type", name);
	}
	next_token();

	expect(',');
	add_anchor_token(')');
	add_anchor_token(',');
	parse_constant_expression();
	rem_anchor_token(',');
	rem_anchor_token(')');

	expect(',');
	add_anchor_token(')');
	parse_constant_expression();
	rem_anchor_token(')');
	expect(')');
	return;
end_error:
	attribute->u.value = true;
}

/**
 * Check that a given GNU attribute has no arguments.
 */
static void check_no_argument(gnu_attribute_t *attribute, const char *name)
{
	if (!attribute->have_arguments)
		return;

	/* should have no arguments */
	errorf(HERE, "wrong number of arguments specified for '%s' attribute", name);
	eat_until_matching_token('(');
	/* we have already consumed '(', so we stop before ')', eat it */
	eat(')');
	attribute->invalid = true;
}

/**
 * Parse one GNU attribute.
 *
 * Note that attribute names can be specified WITH or WITHOUT
 * double underscores, ie const or __const__.
 *
 * The following attributes are parsed without arguments
 *  const
 *  volatile
 *  cdecl
 *  stdcall
 *  fastcall
 *  deprecated
 *  noinline
 *  noreturn
 *  naked
 *  pure
 *  always_inline
 *  malloc
 *  weak
 *  constructor
 *  destructor
 *  nothrow
 *  transparent_union
 *  common
 *  nocommon
 *  packed
 *  shared
 *  notshared
 *  used
 *  unused
 *  no_instrument_function
 *  warn_unused_result
 *  longcall
 *  shortcall
 *  long_call
 *  short_call
 *  function_vector
 *  interrupt_handler
 *  nmi_handler
 *  nesting
 *  near
 *  far
 *  signal
 *  eightbit_data
 *  tiny_data
 *  saveall
 *  flatten
 *  sseregparm
 *  externally_visible
 *  return_twice
 *  may_alias
 *  ms_struct
 *  gcc_struct
 *  dllimport
 *  dllexport
 *
 * The following attributes are parsed with arguments
 *  aligned( const expression )
 *  alias( string literal )
 *  section( string literal )
 *  format( identifier, const expression, const expression )
 *  format_arg( const expression )
 *  tls_model( string literal )
 *  visibility( string literal )
 *  regparm( const expression )
 *  model( string leteral )
 *  trap_exit( const expression )
 *  sp_switch( string literal )
 *
 * The following attributes might have arguments
 *  weak_ref( string literal )
 *  non_null( const expression // ',' )
 *  interrupt( string literal )
 *  sentinel( constant expression )
 */
static decl_modifiers_t parse_gnu_attribute(gnu_attribute_t **attributes)
{
	gnu_attribute_t *head      = *attributes;
	gnu_attribute_t *last      = *attributes;
	decl_modifiers_t modifiers = 0;
	gnu_attribute_t *attribute;

	eat(T___attribute__);
	expect('(');
	expect('(');

	if (token.type != ')') {
		/* find the end of the list */
		if (last != NULL) {
			while (last->next != NULL)
				last = last->next;
		}

		/* non-empty attribute list */
		while (true) {
			const char *name;
			if (token.type == T_const) {
				name = "const";
			} else if (token.type == T_volatile) {
				name = "volatile";
			} else if (token.type == T_cdecl) {
				/* __attribute__((cdecl)), WITH ms mode */
				name = "cdecl";
			} else if (token.type == T_IDENTIFIER) {
				const symbol_t *sym = token.v.symbol;
				name = sym->string;
			} else {
				parse_error_expected("while parsing GNU attribute", T_IDENTIFIER, NULL);
				break;
			}

			next_token();

			int i;
			for (i = 0; i < GNU_AK_LAST; ++i) {
				if (strcmp_underscore(gnu_attribute_names[i], name) == 0)
					break;
			}
			gnu_attribute_kind_t kind = (gnu_attribute_kind_t)i;

			attribute = NULL;
			if (kind == GNU_AK_LAST) {
				if (warning.attribute)
					warningf(HERE, "'%s' attribute directive ignored", name);

				/* skip possible arguments */
				if (token.type == '(') {
					eat_until_matching_token(')');
				}
			} else {
				/* check for arguments */
				attribute = allocate_gnu_attribute(kind);
				if (token.type == '(') {
					next_token();
					if (token.type == ')') {
						/* empty args are allowed */
						next_token();
					} else
						attribute->have_arguments = true;
				}

				switch (kind) {
				case GNU_AK_VOLATILE:
				case GNU_AK_NAKED:
				case GNU_AK_MALLOC:
				case GNU_AK_WEAK:
				case GNU_AK_COMMON:
				case GNU_AK_NOCOMMON:
				case GNU_AK_SHARED:
				case GNU_AK_NOTSHARED:
				case GNU_AK_NO_INSTRUMENT_FUNCTION:
				case GNU_AK_WARN_UNUSED_RESULT:
				case GNU_AK_LONGCALL:
				case GNU_AK_SHORTCALL:
				case GNU_AK_LONG_CALL:
				case GNU_AK_SHORT_CALL:
				case GNU_AK_FUNCTION_VECTOR:
				case GNU_AK_INTERRUPT_HANDLER:
				case GNU_AK_NMI_HANDLER:
				case GNU_AK_NESTING:
			 	case GNU_AK_NEAR:
				case GNU_AK_FAR:
				case GNU_AK_SIGNAL:
				case GNU_AK_EIGTHBIT_DATA:
				case GNU_AK_TINY_DATA:
				case GNU_AK_SAVEALL:
				case GNU_AK_FLATTEN:
				case GNU_AK_SSEREGPARM:
				case GNU_AK_EXTERNALLY_VISIBLE:
				case GNU_AK_RETURN_TWICE:
				case GNU_AK_MAY_ALIAS:
				case GNU_AK_MS_STRUCT:
				case GNU_AK_GCC_STRUCT:
					goto no_arg;

				case GNU_AK_CDECL:             modifiers |= DM_CDECL;             goto no_arg;
				case GNU_AK_FASTCALL:          modifiers |= DM_FASTCALL;          goto no_arg;
				case GNU_AK_STDCALL:           modifiers |= DM_STDCALL;           goto no_arg;
				case GNU_AK_UNUSED:            modifiers |= DM_UNUSED;            goto no_arg;
				case GNU_AK_USED:              modifiers |= DM_USED;              goto no_arg;
				case GNU_AK_PURE:              modifiers |= DM_PURE;              goto no_arg;
				case GNU_AK_CONST:             modifiers |= DM_CONST;             goto no_arg;
				case GNU_AK_ALWAYS_INLINE:     modifiers |= DM_FORCEINLINE;       goto no_arg;
				case GNU_AK_DLLIMPORT:         modifiers |= DM_DLLIMPORT;         goto no_arg;
				case GNU_AK_DLLEXPORT:         modifiers |= DM_DLLEXPORT;         goto no_arg;
				case GNU_AK_PACKED:            modifiers |= DM_PACKED;            goto no_arg;
				case GNU_AK_NOINLINE:          modifiers |= DM_NOINLINE;          goto no_arg;
				case GNU_AK_NORETURN:          modifiers |= DM_NORETURN;          goto no_arg;
				case GNU_AK_NOTHROW:           modifiers |= DM_NOTHROW;           goto no_arg;
				case GNU_AK_TRANSPARENT_UNION: modifiers |= DM_TRANSPARENT_UNION; goto no_arg;
				case GNU_AK_CONSTRUCTOR:       modifiers |= DM_CONSTRUCTOR;       goto no_arg;
				case GNU_AK_DESTRUCTOR:        modifiers |= DM_DESTRUCTOR;        goto no_arg;
				case GNU_AK_DEPRECATED:        modifiers |= DM_DEPRECATED;        goto no_arg;

				case GNU_AK_ALIGNED:
					/* __align__ may be used without an argument */
					if (attribute->have_arguments) {
						parse_gnu_attribute_const_arg(attribute);
					}
					break;

				case GNU_AK_FORMAT_ARG:
				case GNU_AK_REGPARM:
				case GNU_AK_TRAP_EXIT:
					if (!attribute->have_arguments) {
						/* should have arguments */
						errorf(HERE, "wrong number of arguments specified for '%s' attribute", name);
						attribute->invalid = true;
					} else
						parse_gnu_attribute_const_arg(attribute);
					break;
				case GNU_AK_ALIAS:
				case GNU_AK_SECTION:
				case GNU_AK_SP_SWITCH:
					if (!attribute->have_arguments) {
						/* should have arguments */
						errorf(HERE, "wrong number of arguments specified for '%s' attribute", name);
						attribute->invalid = true;
					} else
						parse_gnu_attribute_string_arg(attribute, &attribute->u.string);
					break;
				case GNU_AK_FORMAT:
					if (!attribute->have_arguments) {
						/* should have arguments */
						errorf(HERE, "wrong number of arguments specified for '%s' attribute", name);
						attribute->invalid = true;
					} else
						parse_gnu_attribute_format_args(attribute);
					break;
				case GNU_AK_WEAKREF:
					/* may have one string argument */
					if (attribute->have_arguments)
						parse_gnu_attribute_string_arg(attribute, &attribute->u.string);
					break;
				case GNU_AK_NONNULL:
					if (attribute->have_arguments)
						parse_gnu_attribute_const_arg_list(attribute);
					break;
				case GNU_AK_TLS_MODEL:
					if (!attribute->have_arguments) {
						/* should have arguments */
						errorf(HERE, "wrong number of arguments specified for '%s' attribute", name);
					} else
						parse_gnu_attribute_tls_model_arg(attribute);
					break;
				case GNU_AK_VISIBILITY:
					if (!attribute->have_arguments) {
						/* should have arguments */
						errorf(HERE, "wrong number of arguments specified for '%s' attribute", name);
					} else
						parse_gnu_attribute_visibility_arg(attribute);
					break;
				case GNU_AK_MODEL:
					if (!attribute->have_arguments) {
						/* should have arguments */
						errorf(HERE, "wrong number of arguments specified for '%s' attribute", name);
					} else {
						parse_gnu_attribute_model_arg(attribute);
					}
					break;
				case GNU_AK_MODE:
					if (!attribute->have_arguments) {
						/* should have arguments */
						errorf(HERE, "wrong number of arguments specified for '%s' attribute", name);
					} else {
						parse_gnu_attribute_mode_arg(attribute);
					}
					break;
				case GNU_AK_INTERRUPT:
					/* may have one string argument */
					if (attribute->have_arguments)
						parse_gnu_attribute_interrupt_arg(attribute);
					break;
				case GNU_AK_SENTINEL:
					/* may have one string argument */
					if (attribute->have_arguments)
						parse_gnu_attribute_const_arg(attribute);
					break;
				case GNU_AK_LAST:
					/* already handled */
					break;

no_arg:
					check_no_argument(attribute, name);
				}
			}
			if (attribute != NULL) {
				if (last != NULL) {
					last->next = attribute;
					last       = attribute;
				} else {
					head = last = attribute;
				}
			}

			if (token.type != ',')
				break;
			next_token();
		}
	}
	expect(')');
	expect(')');
end_error:
	*attributes = head;

	return modifiers;
}

/**
 * Parse GNU attributes.
 */
static decl_modifiers_t parse_attributes(gnu_attribute_t **attributes)
{
	decl_modifiers_t modifiers = 0;

	while (true) {
		switch (token.type) {
		case T___attribute__:
			modifiers |= parse_gnu_attribute(attributes);
			continue;

		case T_asm:
			next_token();
			expect('(');
			if (token.type != T_STRING_LITERAL) {
				parse_error_expected("while parsing assembler attribute",
				                     T_STRING_LITERAL, NULL);
				eat_until_matching_token('(');
				break;
			} else {
				parse_string_literals();
			}
			expect(')');
			continue;

		case T_cdecl:     modifiers |= DM_CDECL;    break;
		case T__fastcall: modifiers |= DM_FASTCALL; break;
		case T__stdcall:  modifiers |= DM_STDCALL;  break;

		case T___thiscall:
			/* TODO record modifier */
			if (warning.other)
				warningf(HERE, "Ignoring declaration modifier %K", &token);
			break;

end_error:
		default: return modifiers;
		}

		next_token();
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
				mark_vars_read(expr->select.compound, lhs_ent);
			}
			mark_vars_read(expr->array_access.index, lhs_ent);
			return ent;
		}

		case EXPR_SELECT: {
			if (is_type_compound(skip_typeref(expr->base.type))) {
				return determine_lhs_ent(expr->select.compound, lhs_ent);
			} else {
				mark_vars_read(expr->select.compound, lhs_ent);
				return NULL;
			}
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
			expression_t *const ref = expr->array_access.array_ref;
			mark_vars_read(ref, lhs_ent);
			lhs_ent = determine_lhs_ent(ref, lhs_ent);
			mark_vars_read(expr->array_access.index, lhs_ent);
			return;
		}

		case EXPR_VA_ARG:
			mark_vars_read(expr->va_arge.ap, lhs_ent);
			return;

		case EXPR_UNARY_CAST:
			/* Special case: Use void cast to mark a variable as "read" */
			if (is_type_atomic(skip_typeref(expr->base.type), ATOMIC_TYPE_VOID))
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
		case EXPR_UNARY_CAST_IMPLICIT:
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

		case EXPR_UNKNOWN:
		case EXPR_INVALID:
		case EXPR_CONST:
		case EXPR_CHARACTER_CONSTANT:
		case EXPR_WIDE_CHARACTER_CONSTANT:
		case EXPR_STRING_LITERAL:
		case EXPR_WIDE_STRING_LITERAL:
		case EXPR_COMPOUND_LITERAL: // TODO init?
		case EXPR_SIZEOF:
		case EXPR_CLASSIFY_TYPE:
		case EXPR_ALIGNOF:
		case EXPR_FUNCNAME:
		case EXPR_BUILTIN_SYMBOL:
		case EXPR_BUILTIN_CONSTANT_P:
		case EXPR_BUILTIN_PREFETCH:
		case EXPR_OFFSETOF:
		case EXPR_STATEMENT: // TODO
		case EXPR_LABEL_ADDRESS:
		case EXPR_REFERENCE_ENUM_VALUE:
			return;
	}

	panic("unhandled expression");
}

static designator_t *parse_designation(void)
{
	designator_t *result = NULL;
	designator_t *last   = NULL;

	while (true) {
		designator_t *designator;
		switch (token.type) {
		case '[':
			designator = allocate_ast_zero(sizeof(designator[0]));
			designator->source_position = token.source_position;
			next_token();
			add_anchor_token(']');
			designator->array_index = parse_constant_expression();
			rem_anchor_token(']');
			expect(']');
			break;
		case '.':
			designator = allocate_ast_zero(sizeof(designator[0]));
			designator->source_position = token.source_position;
			next_token();
			if (token.type != T_IDENTIFIER) {
				parse_error_expected("while parsing designator",
				                     T_IDENTIFIER, NULL);
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
		if (last != NULL) {
			last->next = designator;
		} else {
			result = designator;
		}
		last = designator;
	}
end_error:
	return NULL;
}

static initializer_t *initializer_from_string(array_type_t *type,
                                              const string_t *const string)
{
	/* TODO: check len vs. size of array type */
	(void) type;

	initializer_t *initializer = allocate_initializer_zero(INITIALIZER_STRING);
	initializer->string.string = *string;

	return initializer;
}

static initializer_t *initializer_from_wide_string(array_type_t *const type,
                                                   wide_string_t *const string)
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

	/* § 6.7.8.14/15 char array may be initialized by string literals */
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
							&expression->string.value);
					}

				case EXPR_WIDE_STRING_LITERAL: {
					type_t *bare_wchar_type = skip_typeref(type_wchar_t);
					if (get_unqualified_type(element_type) == bare_wchar_type) {
						return initializer_from_wide_string(array_type,
							&expression->wide_string.value);
					}
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
#if 0
	if (type->kind == TYPE_BITFIELD) {
		type = type->bitfield.base_type;
	}
#endif
	result->value.value = create_implicit_cast(expression, type);

	return result;
}

/**
 * Checks if a given expression can be used as an constant initializer.
 */
static bool is_initializer_constant(const expression_t *expression)
{
	return is_constant_expression(expression)
		|| is_address_constant(expression);
}

/**
 * Parses an scalar initializer.
 *
 * § 6.7.8.11; eat {} without warning
 */
static initializer_t *parse_scalar_initializer(type_t *type,
                                               bool must_be_constant)
{
	/* there might be extra {} hierarchies */
	int braces = 0;
	if (token.type == '{') {
		if (warning.other)
			warningf(HERE, "extra curly braces around scalar initializer");
		do {
			++braces;
			next_token();
		} while (token.type == '{');
	}

	expression_t *expression = parse_assignment_expression();
	mark_vars_read(expression, NULL);
	if (must_be_constant && !is_initializer_constant(expression)) {
		errorf(&expression->base.source_position,
		       "Initialisation expression '%E' is not constant",
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
		if (token.type == ',') {
			next_token();
		}
		if (token.type != '}') {
			if (!additional_warning_displayed && warning.other) {
				warningf(HERE, "additional elements in scalar initializer");
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
		compound_t *compound  = top_type->compound.compound;
		entity_t   *entry     = compound->members.entities;

		if (entry != NULL) {
			assert(entry->kind == ENTITY_COMPOUND_MEMBER);
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
					goto failed;
				}
				assert(iter->kind == ENTITY_COMPOUND_MEMBER);
				if (used_in_offsetof) {
					type_t *real_type = skip_typeref(iter->declaration.type);
					if (real_type->kind == TYPE_BITFIELD) {
						errorf(&designator->source_position,
						       "offsetof designator '%Y' may not specify bitfield",
						       symbol);
						goto failed;
					}
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
				goto failed;
			}

			long index = fold_constant(array_index);
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

failed:
	return false;
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

		entity_t *next_entity = entry->base.next;
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
 * skip until token is found.
 */
static void skip_until(int type)
{
	while (token.type != type) {
		if (token.type == T_EOF)
			return;
		next_token();
	}
}

/**
 * skip any {...} blocks until a closing bracket is reached.
 */
static void skip_initializers(void)
{
	if (token.type == '{')
		next_token();

	while (token.type != '}') {
		if (token.type == T_EOF)
			return;
		if (token.type == '{') {
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
	if (token.type == '}') {
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
		if (token.type == '.' || token.type == '[') {
			designator = parse_designation();
			goto finish_designator;
		} else if (token.type == T_IDENTIFIER && look_ahead(1)->type == ':') {
			/* GNU-style designator ("identifier: value") */
			designator = allocate_ast_zero(sizeof(designator[0]));
			designator->source_position = token.source_position;
			designator->symbol          = token.v.symbol;
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

		if (token.type == '{') {
			if (type != NULL && is_type_scalar(type)) {
				sub = parse_scalar_initializer(type, env->must_be_constant);
			} else {
				eat('{');
				if (type == NULL) {
					if (env->entity != NULL) {
						errorf(HERE,
						     "extra brace group at end of initializer for '%Y'",
						     env->entity->base.symbol);
					} else {
						errorf(HERE, "extra brace group at end of initializer");
					}
				} else
					descend_into_subtype(path);

				add_anchor_token('}');
				sub = parse_sub_initializer(path, orig_type, top_path_level+1,
				                            env);
				rem_anchor_token('}');

				if (type != NULL) {
					ascend_from_subtype(path);
					expect('}');
				} else {
					expect('}');
					goto error_parse_next;
				}
			}
		} else {
			/* must be an expression */
			expression_t *expression = parse_assignment_expression();

			if (env->must_be_constant && !is_initializer_constant(expression)) {
				errorf(&expression->base.source_position,
				       "Initialisation expression '%E' is not constant",
				       expression);
			}

			if (type == NULL) {
				/* we are already outside, ... */
				type_t *const outer_type_skip = skip_typeref(outer_type);
				if (is_type_compound(outer_type_skip) &&
				    !outer_type_skip->compound.compound->complete) {
					goto error_parse_next;
				}
				goto error_excess;
			}

			/* handle { "string" } special case */
			if ((expression->kind == EXPR_STRING_LITERAL
					|| expression->kind == EXPR_WIDE_STRING_LITERAL)
					&& outer_type != NULL) {
				sub = initializer_from_expression(outer_type, expression);
				if (sub != NULL) {
					if (token.type == ',') {
						next_token();
					}
					if (token.type != '}' && warning.other) {
						warningf(HERE, "excessive elements in initializer for type '%T'",
								 orig_type);
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

		if (type != NULL) {
			/* append to initializers list */
			ARR_APP1(initializer_t*, initializers, sub);
		} else {
error_excess:
			if (warning.other) {
				if (env->entity != NULL) {
					warningf(HERE, "excess elements in struct initializer for '%Y'",
				           env->entity->base.symbol);
				} else {
					warningf(HERE, "excess elements in struct initializer");
				}
			}
		}

error_parse_next:
		if (token.type == '}') {
			break;
		}
		expect(',');
		if (token.type == '}') {
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

/**
 * Parses an initializer. Parsers either a compound literal
 * (env->declaration == NULL) or an initializer of a declaration.
 */
static initializer_t *parse_initializer(parse_initializer_env_t *env)
{
	type_t        *type   = skip_typeref(env->type);
	initializer_t *result = NULL;
	size_t         max_index;

	if (is_type_scalar(type)) {
		result = parse_scalar_initializer(type, env->must_be_constant);
	} else if (token.type == '{') {
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

		expect('}');
	} else {
		/* parse_scalar_initializer() also works in this case: we simply
		 * have an expression without {} around it */
		result = parse_scalar_initializer(type, env->must_be_constant);
	}

	/* § 6.7.8 (22) array initializers for arrays with unknown size determine
	 * the array type size */
	if (is_type_array(type) && type->array.size_expression == NULL
			&& result != NULL) {
		size_t size;
		switch (result->kind) {
		case INITIALIZER_LIST:
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

		expression_t *cnst       = allocate_expression_zero(EXPR_CONST);
		cnst->base.type          = type_size_t;
		cnst->conste.v.int_value = size;

		type_t *new_type = duplicate_type(type);

		new_type->array.size_expression   = cnst;
		new_type->array.size_constant     = true;
		new_type->array.has_implicit_size = true;
		new_type->array.size              = size;
		env->type = new_type;
	}

	return result;
end_error:
	return NULL;
}

static void append_entity(scope_t *scope, entity_t *entity)
{
	if (scope->last_entity != NULL) {
		scope->last_entity->base.next = entity;
	} else {
		scope->entities = entity;
	}
	scope->last_entity = entity;
}


static compound_t *parse_compound_type_specifier(bool is_struct)
{
	gnu_attribute_t  *attributes = NULL;
	decl_modifiers_t  modifiers  = 0;
	if (is_struct) {
		eat(T_struct);
	} else {
		eat(T_union);
	}

	symbol_t   *symbol   = NULL;
	compound_t *compound = NULL;

	if (token.type == T___attribute__) {
		modifiers |= parse_attributes(&attributes);
	}

	if (token.type == T_IDENTIFIER) {
		symbol = token.v.symbol;
		next_token();

		namespace_tag_t const namespc =
			is_struct ? NAMESPACE_STRUCT : NAMESPACE_UNION;
		entity_t *entity = get_entity(symbol, namespc);
		if (entity != NULL) {
			assert(entity->kind == (is_struct ? ENTITY_STRUCT : ENTITY_UNION));
			compound = &entity->compound;
			if (compound->base.parent_scope != current_scope &&
			    (token.type == '{' || token.type == ';')) {
				/* we're in an inner scope and have a definition. Override
				   existing definition in outer scope */
				compound = NULL;
			} else if (compound->complete && token.type == '{') {
				assert(symbol != NULL);
				errorf(HERE, "multiple definitions of '%s %Y' (previous definition %P)",
				       is_struct ? "struct" : "union", symbol,
				       &compound->base.source_position);
				/* clear members in the hope to avoid further errors */
				compound->members.entities = NULL;
			}
		}
	} else if (token.type != '{') {
		if (is_struct) {
			parse_error_expected("while parsing struct type specifier",
			                     T_IDENTIFIER, '{', NULL);
		} else {
			parse_error_expected("while parsing union type specifier",
			                     T_IDENTIFIER, '{', NULL);
		}

		return NULL;
	}

	if (compound == NULL) {
		entity_kind_t  kind   = is_struct ? ENTITY_STRUCT : ENTITY_UNION;
		entity_t      *entity = allocate_entity_zero(kind);
		compound              = &entity->compound;

		compound->base.namespc =
			(is_struct ? NAMESPACE_STRUCT : NAMESPACE_UNION);
		compound->base.source_position = token.source_position;
		compound->base.symbol          = symbol;
		compound->base.parent_scope    = current_scope;
		if (symbol != NULL) {
			environment_push(entity);
		}
		append_entity(current_scope, entity);
	}

	if (token.type == '{') {
		parse_compound_type_entries(compound);
		modifiers |= parse_attributes(&attributes);

		if (symbol == NULL) {
			assert(anonymous_entity == NULL);
			anonymous_entity = (entity_t*)compound;
		}
	}

	compound->modifiers |= modifiers;
	return compound;
}

static void parse_enum_entries(type_t *const enum_type)
{
	eat('{');

	if (token.type == '}') {
		errorf(HERE, "empty enum not allowed");
		next_token();
		return;
	}

	add_anchor_token('}');
	do {
		if (token.type != T_IDENTIFIER) {
			parse_error_expected("while parsing enum entry", T_IDENTIFIER, NULL);
			eat_block();
			rem_anchor_token('}');
			return;
		}

		entity_t *entity             = allocate_entity_zero(ENTITY_ENUM_VALUE);
		entity->enum_value.enum_type = enum_type;
		entity->base.symbol          = token.v.symbol;
		entity->base.source_position = token.source_position;
		next_token();

		if (token.type == '=') {
			next_token();
			expression_t *value = parse_constant_expression();

			value = create_implicit_cast(value, enum_type);
			entity->enum_value.value = value;

			/* TODO semantic */
		}

		record_entity(entity, false);

		if (token.type != ',')
			break;
		next_token();
	} while (token.type != '}');
	rem_anchor_token('}');

	expect('}');

end_error:
	;
}

static type_t *parse_enum_specifier(void)
{
	gnu_attribute_t *attributes = NULL;
	entity_t        *entity;
	symbol_t        *symbol;

	eat(T_enum);
	if (token.type == T_IDENTIFIER) {
		symbol = token.v.symbol;
		next_token();

		entity = get_entity(symbol, NAMESPACE_ENUM);
		assert(entity == NULL || entity->kind == ENTITY_ENUM);
	} else if (token.type != '{') {
		parse_error_expected("while parsing enum type specifier",
		                     T_IDENTIFIER, '{', NULL);
		return NULL;
	} else {
		entity  = NULL;
		symbol  = NULL;
	}

	if (entity == NULL) {
		entity                       = allocate_entity_zero(ENTITY_ENUM);
		entity->base.namespc         = NAMESPACE_ENUM;
		entity->base.source_position = token.source_position;
		entity->base.symbol          = symbol;
		entity->base.parent_scope    = current_scope;
	}

	type_t *const type = allocate_type_zero(TYPE_ENUM);
	type->enumt.enume  = &entity->enume;

	if (token.type == '{') {
		if (entity->enume.complete) {
			errorf(HERE, "multiple definitions of 'enum %Y' (previous definition %P)",
			       symbol, &entity->base.source_position);
		}
		if (symbol != NULL) {
			environment_push(entity);
		}
		append_entity(current_scope, entity);
		entity->enume.complete = true;

		parse_enum_entries(type);
		parse_attributes(&attributes);

		if (symbol == NULL) {
			assert(anonymous_entity == NULL);
			anonymous_entity = entity;
		}
	} else if (!entity->enume.complete && !(c_mode & _GNUC)) {
		errorf(HERE, "'enum %Y' used before definition (incomplete enums are a GNU extension)",
		       symbol);
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

	expect('(');
	add_anchor_token(')');

	expression_t *expression  = NULL;

	bool old_type_prop     = in_type_prop;
	bool old_gcc_extension = in_gcc_extension;
	in_type_prop           = true;

	while (token.type == T___extension__) {
		/* This can be a prefix to a typename or an expression. */
		next_token();
		in_gcc_extension = true;
	}
	switch (token.type) {
	case T_IDENTIFIER:
		if (is_typedef_symbol(token.v.symbol)) {
			type = parse_typename();
		} else {
			expression = parse_expression();
			type       = expression->base.type;
		}
		break;

	TYPENAME_START
		type = parse_typename();
		break;

	default:
		expression = parse_expression();
		type       = expression->base.type;
		break;
	}
	in_type_prop     = old_type_prop;
	in_gcc_extension = old_gcc_extension;

	rem_anchor_token(')');
	expect(')');

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
	SPECIFIER_SHORT     = 1 << 6,
	SPECIFIER_LONG_LONG = 1 << 7,
	SPECIFIER_FLOAT     = 1 << 8,
	SPECIFIER_BOOL      = 1 << 9,
	SPECIFIER_VOID      = 1 << 10,
	SPECIFIER_INT8      = 1 << 11,
	SPECIFIER_INT16     = 1 << 12,
	SPECIFIER_INT32     = 1 << 13,
	SPECIFIER_INT64     = 1 << 14,
	SPECIFIER_INT128    = 1 << 15,
	SPECIFIER_COMPLEX   = 1 << 16,
	SPECIFIER_IMAGINARY = 1 << 17,
} specifiers_t;

static type_t *create_builtin_type(symbol_t *const symbol,
                                   type_t *const real_type)
{
	type_t *type            = allocate_type_zero(TYPE_BUILTIN);
	type->builtin.symbol    = symbol;
	type->builtin.real_type = real_type;

	type_t *result = typehash_insert(type);
	if (type != result) {
		free_type(type);
	}

	return result;
}

static type_t *get_typedef_type(symbol_t *symbol)
{
	entity_t *entity = get_entity(symbol, NAMESPACE_NORMAL);
	if (entity == NULL || entity->kind != ENTITY_TYPEDEF)
		return NULL;

	type_t *type            = allocate_type_zero(TYPE_TYPEDEF);
	type->typedeft.typedefe = &entity->typedefe;

	return type;
}

/**
 * check for the allowed MS alignment values.
 */
static bool check_alignment_value(long long intvalue)
{
	if (intvalue < 1 || intvalue > 8192) {
		errorf(HERE, "illegal alignment value");
		return false;
	}
	unsigned v = (unsigned)intvalue;
	for (unsigned i = 1; i <= 8192; i += i) {
		if (i == v)
			return true;
	}
	errorf(HERE, "alignment must be power of two");
	return false;
}

#define DET_MOD(name, tag) do { \
	if (*modifiers & tag && warning.other) warningf(HERE, #name " used more than once"); \
	*modifiers |= tag; \
} while (0)

static void parse_microsoft_extended_decl_modifier(declaration_specifiers_t *specifiers)
{
	decl_modifiers_t *modifiers = &specifiers->modifiers;

	while (true) {
		if (token.type == T_restrict) {
			next_token();
			DET_MOD(restrict, DM_RESTRICT);
			goto end_loop;
		} else if (token.type != T_IDENTIFIER)
			break;
		symbol_t *symbol = token.v.symbol;
		if (symbol == sym_align) {
			next_token();
			expect('(');
			if (token.type != T_INTEGER)
				goto end_error;
			if (check_alignment_value(token.v.intvalue)) {
				if (specifiers->alignment != 0 && warning.other)
					warningf(HERE, "align used more than once");
				specifiers->alignment = (unsigned char)token.v.intvalue;
			}
			next_token();
			expect(')');
		} else if (symbol == sym_allocate) {
			next_token();
			expect('(');
			if (token.type != T_IDENTIFIER)
				goto end_error;
			(void)token.v.symbol;
			expect(')');
		} else if (symbol == sym_dllimport) {
			next_token();
			DET_MOD(dllimport, DM_DLLIMPORT);
		} else if (symbol == sym_dllexport) {
			next_token();
			DET_MOD(dllexport, DM_DLLEXPORT);
		} else if (symbol == sym_thread) {
			next_token();
			DET_MOD(thread, DM_THREAD);
		} else if (symbol == sym_naked) {
			next_token();
			DET_MOD(naked, DM_NAKED);
		} else if (symbol == sym_noinline) {
			next_token();
			DET_MOD(noinline, DM_NOINLINE);
		} else if (symbol == sym_noreturn) {
			next_token();
			DET_MOD(noreturn, DM_NORETURN);
		} else if (symbol == sym_nothrow) {
			next_token();
			DET_MOD(nothrow, DM_NOTHROW);
		} else if (symbol == sym_novtable) {
			next_token();
			DET_MOD(novtable, DM_NOVTABLE);
		} else if (symbol == sym_property) {
			next_token();
			expect('(');
			for (;;) {
				bool is_get = false;
				if (token.type != T_IDENTIFIER)
					goto end_error;
				if (token.v.symbol == sym_get) {
					is_get = true;
				} else if (token.v.symbol == sym_put) {
				} else {
					errorf(HERE, "Bad property name '%Y'", token.v.symbol);
					goto end_error;
				}
				next_token();
				expect('=');
				if (token.type != T_IDENTIFIER)
					goto end_error;
				if (is_get) {
					if (specifiers->get_property_sym != NULL) {
						errorf(HERE, "get property name already specified");
					} else {
						specifiers->get_property_sym = token.v.symbol;
					}
				} else {
					if (specifiers->put_property_sym != NULL) {
						errorf(HERE, "put property name already specified");
					} else {
						specifiers->put_property_sym = token.v.symbol;
					}
				}
				next_token();
				if (token.type == ',') {
					next_token();
					continue;
				}
				break;
			}
			expect(')');
		} else if (symbol == sym_selectany) {
			next_token();
			DET_MOD(selectany, DM_SELECTANY);
		} else if (symbol == sym_uuid) {
			next_token();
			expect('(');
			if (token.type != T_STRING_LITERAL)
				goto end_error;
			next_token();
			expect(')');
		} else if (symbol == sym_deprecated) {
			next_token();
			if (specifiers->deprecated != 0 && warning.other)
				warningf(HERE, "deprecated used more than once");
			specifiers->deprecated = true;
			if (token.type == '(') {
				next_token();
				if (token.type == T_STRING_LITERAL) {
					specifiers->deprecated_string = token.v.string.begin;
					next_token();
				} else {
					errorf(HERE, "string literal expected");
				}
				expect(')');
			}
		} else if (symbol == sym_noalias) {
			next_token();
			DET_MOD(noalias, DM_NOALIAS);
		} else {
			if (warning.other)
				warningf(HERE, "Unknown modifier '%Y' ignored", token.v.symbol);
			next_token();
			if (token.type == '(')
				skip_until(')');
		}
end_loop:
		if (token.type == ',')
			next_token();
	}
end_error:
	return;
}

static entity_t *create_error_entity(symbol_t *symbol, entity_kind_tag_t kind)
{
	entity_t *entity             = allocate_entity_zero(kind);
	entity->base.source_position = *HERE;
	entity->base.symbol          = symbol;
	if (is_declaration(entity)) {
		entity->declaration.type     = type_error_type;
		entity->declaration.implicit = true;
	} else if (kind == ENTITY_TYPEDEF) {
		entity->typedefe.type    = type_error_type;
		entity->typedefe.builtin = true;
	}
	record_entity(entity, false);
	return entity;
}

static void parse_microsoft_based(based_spec_t *based_spec)
{
	if (token.type != T_IDENTIFIER) {
		parse_error_expected("while parsing __based", T_IDENTIFIER, NULL);
		return;
	}
	symbol_t *symbol = token.v.symbol;
	entity_t *entity = get_entity(symbol, NAMESPACE_NORMAL);

	if (entity == NULL || entity->base.kind != ENTITY_VARIABLE) {
		errorf(HERE, "'%Y' is not a variable name.", symbol);
		entity = create_error_entity(symbol, ENTITY_VARIABLE);
	} else {
		variable_t *variable = &entity->variable;

		if (based_spec->base_variable != NULL) {
			errorf(HERE, "__based type qualifier specified more than once");
		}
		based_spec->source_position = token.source_position;
		based_spec->base_variable   = variable;

		type_t *const type = variable->base.type;

		if (is_type_valid(type)) {
			if (! is_type_pointer(skip_typeref(type))) {
				errorf(HERE, "variable in __based modifier must have pointer type instead of '%T'", type);
			}
			if (variable->base.base.parent_scope != file_scope) {
				errorf(HERE, "a nonstatic local variable may not be used in a __based specification");
			}
		}
	}
	next_token();
}

/**
 * Finish the construction of a struct type by calculating
 * its size, offsets, alignment.
 */
static void finish_struct_type(compound_type_t *type)
{
	assert(type->compound != NULL);

	compound_t *compound = type->compound;
	if (!compound->complete)
		return;

	il_size_t      size           = 0;
	il_size_t      offset;
	il_alignment_t alignment      = 1;
	bool           need_pad       = false;

	entity_t *entry = compound->members.entities;
	for (; entry != NULL; entry = entry->base.next) {
		if (entry->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		type_t *m_type = skip_typeref(entry->declaration.type);
		if (! is_type_valid(m_type)) {
			/* simply ignore errors here */
			continue;
		}
		il_alignment_t m_alignment = m_type->base.alignment;
		if (m_alignment > alignment)
			alignment = m_alignment;

		offset = (size + m_alignment - 1) & -m_alignment;

		if (offset > size)
			need_pad = true;
		entry->compound_member.offset = offset;
		size = offset + m_type->base.size;
	}
	if (type->base.alignment != 0) {
		alignment = type->base.alignment;
	}

	offset = (size + alignment - 1) & -alignment;
	if (offset > size)
		need_pad = true;

	if (need_pad) {
		if (warning.padded) {
			warningf(&compound->base.source_position, "'%T' needs padding", type);
		}
	} else {
		if (compound->modifiers & DM_PACKED && warning.packed) {
			warningf(&compound->base.source_position,
					"superfluous packed attribute on '%T'", type);
		}
	}

	type->base.size      = offset;
	type->base.alignment = alignment;
}

/**
 * Finish the construction of an union type by calculating
 * its size and alignment.
 */
static void finish_union_type(compound_type_t *type)
{
	assert(type->compound != NULL);

	compound_t *compound = type->compound;
	if (! compound->complete)
		return;

	il_size_t      size      = 0;
	il_alignment_t alignment = 1;

	entity_t *entry = compound->members.entities;
	for (; entry != NULL; entry = entry->base.next) {
		if (entry->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		type_t *m_type = skip_typeref(entry->declaration.type);
		if (! is_type_valid(m_type))
			continue;

		entry->compound_member.offset = 0;
		if (m_type->base.size > size)
			size = m_type->base.size;
		if (m_type->base.alignment > alignment)
			alignment = m_type->base.alignment;
	}
	if (type->base.alignment != 0) {
		alignment = type->base.alignment;
	}
	size = (size + alignment - 1) & -alignment;
	type->base.size      = size;
	type->base.alignment = alignment;
}

static void parse_declaration_specifiers(declaration_specifiers_t *specifiers)
{
	type_t            *type              = NULL;
	type_qualifiers_t  qualifiers        = TYPE_QUALIFIER_NONE;
	type_modifiers_t   modifiers         = TYPE_MODIFIER_NONE;
	unsigned           type_specifiers   = 0;
	bool               newtype           = false;
	bool               saw_error         = false;
	bool               old_gcc_extension = in_gcc_extension;

	specifiers->source_position = token.source_position;

	while (true) {
		specifiers->modifiers
			|= parse_attributes(&specifiers->gnu_attributes);
		if (specifiers->modifiers & DM_TRANSPARENT_UNION)
			modifiers |= TYPE_MODIFIER_TRANSPARENT_UNION;

		switch (token.type) {
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
			next_token();
			expect('(');
			add_anchor_token(')');
			parse_microsoft_extended_decl_modifier(specifiers);
			rem_anchor_token(')');
			expect(')');
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
					case STORAGE_CLASS_AUTO:     wrong = "auto";     goto wrong_thread_stoarge_class;
					case STORAGE_CLASS_REGISTER: wrong = "register"; goto wrong_thread_stoarge_class;
					case STORAGE_CLASS_TYPEDEF:  wrong = "typedef";  goto wrong_thread_stoarge_class;
wrong_thread_stoarge_class:
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

		case T___extension__:
			next_token();
			in_gcc_extension = true;
			break;

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

		case T__forceinline:
			/* only in microsoft mode */
			specifiers->modifiers |= DM_FORCEINLINE;
			/* FALLTHROUGH */

		case T_inline:
			next_token();
			specifiers->is_inline = true;
			break;

		case T_long:
			if (type_specifiers & SPECIFIER_LONG_LONG) {
				errorf(HERE, "multiple type specifiers given");
			} else if (type_specifiers & SPECIFIER_LONG) {
				type_specifiers |= SPECIFIER_LONG_LONG;
			} else {
				type_specifiers |= SPECIFIER_LONG;
			}
			next_token();
			break;

		case T_struct: {
			type = allocate_type_zero(TYPE_COMPOUND_STRUCT);

			type->compound.compound = parse_compound_type_specifier(true);
			finish_struct_type(&type->compound);
			break;
		}
		case T_union: {
			type = allocate_type_zero(TYPE_COMPOUND_UNION);
			type->compound.compound = parse_compound_type_specifier(false);
			if (type->compound.compound->modifiers & DM_TRANSPARENT_UNION)
				modifiers |= TYPE_MODIFIER_TRANSPARENT_UNION;
			finish_union_type(&type->compound);
			break;
		}
		case T_enum:
			type = parse_enum_specifier();
			break;
		case T___typeof__:
			type = parse_typeof();
			break;
		case T___builtin_va_list:
			type = duplicate_type(type_valist);
			next_token();
			break;

		case T_IDENTIFIER: {
			/* only parse identifier if we haven't found a type yet */
			if (type != NULL || type_specifiers != 0) {
				/* Be somewhat resilient to typos like 'unsigned lng* f()' in a
				 * declaration, so it doesn't generate errors about expecting '(' or
				 * '{' later on. */
				switch (look_ahead(1)->type) {
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

			type_t *const typedef_type = get_typedef_type(token.v.symbol);
			if (typedef_type == NULL) {
				/* Be somewhat resilient to typos like 'vodi f()' at the beginning of a
				 * declaration, so it doesn't generate 'implicit int' followed by more
				 * errors later on. */
				token_type_t const la1_type = (token_type_t)look_ahead(1)->type;
				switch (la1_type) {
					DECLARATION_START
					case T_IDENTIFIER:
					case '&':
					case '*': {
						errorf(HERE, "%K does not name a type", &token);

						entity_t *entity =
							create_error_entity(token.v.symbol, ENTITY_TYPEDEF);

						type = allocate_type_zero(TYPE_TYPEDEF);
						type->typedeft.typedefe = &entity->typedefe;

						next_token();
						saw_error = true;
						if (la1_type == '&' || la1_type == '*')
							goto finish_specifiers;
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
	in_gcc_extension = old_gcc_extension;

	if (type == NULL || (saw_error && type_specifiers != 0)) {
		atomic_type_kind_t atomic_type;

		/* match valid basic types */
		switch (type_specifiers) {
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
			goto warn_about_long_long;

		case SPECIFIER_UNSIGNED | SPECIFIER_LONG | SPECIFIER_LONG_LONG:
		case SPECIFIER_UNSIGNED | SPECIFIER_LONG | SPECIFIER_LONG_LONG
			| SPECIFIER_INT:
			atomic_type = ATOMIC_TYPE_ULONGLONG;
warn_about_long_long:
			if (warning.long_long) {
				warningf(&specifiers->source_position,
				         "ISO C90 does not support 'long long'");
			}
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
		default:
			/* invalid specifier combination, give an error message */
			if (type_specifiers == 0) {
				if (saw_error)
					goto end_error;

				/* ISO/IEC 14882:1998(E) §C.1.5:4 */
				if (!(c_mode & _CXX) && !strict_mode) {
					if (warning.implicit_int) {
						warningf(HERE, "no type specifiers in declaration, using 'int'");
					}
					atomic_type = ATOMIC_TYPE_INT;
					break;
				} else {
					errorf(HERE, "no type specifiers given in declaration");
				}
			} else if ((type_specifiers & SPECIFIER_SIGNED) &&
			          (type_specifiers & SPECIFIER_UNSIGNED)) {
				errorf(HERE, "signed and unsigned specifiers given");
			} else if (type_specifiers & (SPECIFIER_SIGNED | SPECIFIER_UNSIGNED)) {
				errorf(HERE, "only integer types can be signed or unsigned");
			} else {
				errorf(HERE, "multiple datatypes in declaration");
			}
			goto end_error;
		}

		if (type_specifiers & SPECIFIER_COMPLEX) {
			type                = allocate_type_zero(TYPE_COMPLEX);
			type->complex.akind = atomic_type;
		} else if (type_specifiers & SPECIFIER_IMAGINARY) {
			type                  = allocate_type_zero(TYPE_IMAGINARY);
			type->imaginary.akind = atomic_type;
		} else {
			type               = allocate_type_zero(TYPE_ATOMIC);
			type->atomic.akind = atomic_type;
		}
		newtype = true;
	} else if (type_specifiers != 0) {
		errorf(HERE, "multiple datatypes in declaration");
	}

	/* FIXME: check type qualifiers here */

	type->base.qualifiers = qualifiers;
	type->base.modifiers  = modifiers;

	type_t *result = typehash_insert(type);
	if (newtype && result != type) {
		free_type(type);
	}

	specifiers->type = result;
	return;

end_error:
	specifiers->type = type_error_type;
	return;
}

static type_qualifiers_t parse_type_qualifiers(void)
{
	type_qualifiers_t qualifiers = TYPE_QUALIFIER_NONE;

	while (true) {
		switch (token.type) {
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
	do {
		entity_t *entity = allocate_entity_zero(ENTITY_PARAMETER);
		entity->base.source_position = token.source_position;
		entity->base.namespc         = NAMESPACE_NORMAL;
		entity->base.symbol          = token.v.symbol;
		/* a K&R parameter has no type, yet */
		next_token();

		append_entity(scope, entity);

		if (token.type != ',') {
			break;
		}
		next_token();
	} while (token.type == T_IDENTIFIER);
}

static entity_t *parse_parameter(void)
{
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));

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
		errorf(&entity->base.source_position,
		       "parameter '%Y' has incomplete type '%T'", entity->base.symbol,
		       entity->declaration.type);
	}
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

	if (token.type == T_IDENTIFIER &&
	    !is_typedef_symbol(token.v.symbol)) {
		token_type_t la1_type = (token_type_t)look_ahead(1)->type;
		if (la1_type == ',' || la1_type == ')') {
			type->kr_style_parameters = true;
			parse_identifier_list(scope);
			goto parameters_finished;
		}
	}

	if (token.type == ')') {
		/* ISO/IEC 14882:1998(E) §C.1.6:1 */
		if (!(c_mode & _CXX))
			type->unspecified_parameters = true;
		goto parameters_finished;
	}

	function_parameter_t *parameter;
	function_parameter_t *last_parameter = NULL;

	while (true) {
		switch (token.type) {
		case T_DOTDOTDOT:
			next_token();
			type->variadic = true;
			goto parameters_finished;

		case T_IDENTIFIER:
		case T___extension__:
		DECLARATION_START
		{
			entity_t *entity = parse_parameter();
			if (entity->kind == ENTITY_TYPEDEF) {
				errorf(&entity->base.source_position,
				       "typedef not allowed as function parameter");
				break;
			}
			assert(is_declaration(entity));

			/* func(void) is not a parameter */
			if (last_parameter == NULL
					&& token.type == ')'
					&& entity->base.symbol == NULL
					&& skip_typeref(entity->declaration.type) == type_void) {
				goto parameters_finished;
			}
			semantic_parameter_incomplete(entity);

			parameter = obstack_alloc(type_obst, sizeof(parameter[0]));
			memset(parameter, 0, sizeof(parameter[0]));
			parameter->type = entity->declaration.type;

			if (scope != NULL) {
				append_entity(scope, entity);
			}

			if (last_parameter != NULL) {
				last_parameter->next = parameter;
			} else {
				type->parameters = parameter;
			}
			last_parameter   = parameter;
			break;
		}

		default:
			goto parameters_finished;
		}
		if (token.type != ',') {
			goto parameters_finished;
		}
		next_token();
	}


parameters_finished:
	rem_anchor_token(')');
	expect(')');

end_error:
	restore_anchor_state(',', saved_comma_state);
}

typedef enum construct_type_kind_t {
	CONSTRUCT_INVALID,
	CONSTRUCT_POINTER,
	CONSTRUCT_REFERENCE,
	CONSTRUCT_FUNCTION,
	CONSTRUCT_ARRAY
} construct_type_kind_t;

typedef struct construct_type_t construct_type_t;
struct construct_type_t {
	construct_type_kind_t  kind;
	construct_type_t      *next;
};

typedef struct parsed_pointer_t parsed_pointer_t;
struct parsed_pointer_t {
	construct_type_t  construct_type;
	type_qualifiers_t type_qualifiers;
	variable_t        *base_variable;  /**< MS __based extension. */
};

typedef struct parsed_reference_t parsed_reference_t;
struct parsed_reference_t {
	construct_type_t construct_type;
};

typedef struct construct_function_type_t construct_function_type_t;
struct construct_function_type_t {
	construct_type_t  construct_type;
	type_t           *function_type;
};

typedef struct parsed_array_t parsed_array_t;
struct parsed_array_t {
	construct_type_t  construct_type;
	type_qualifiers_t type_qualifiers;
	bool              is_static;
	bool              is_variable;
	expression_t     *size;
};

typedef struct construct_base_type_t construct_base_type_t;
struct construct_base_type_t {
	construct_type_t  construct_type;
	type_t           *type;
};

static construct_type_t *parse_pointer_declarator(variable_t *base_variable)
{
	eat('*');

	parsed_pointer_t *pointer = obstack_alloc(&temp_obst, sizeof(pointer[0]));
	memset(pointer, 0, sizeof(pointer[0]));
	pointer->construct_type.kind = CONSTRUCT_POINTER;
	pointer->type_qualifiers     = parse_type_qualifiers();
	pointer->base_variable       = base_variable;

	return &pointer->construct_type;
}

static construct_type_t *parse_reference_declarator(void)
{
	eat('&');

	parsed_reference_t *reference = obstack_alloc(&temp_obst, sizeof(reference[0]));
	memset(reference, 0, sizeof(reference[0]));
	reference->construct_type.kind = CONSTRUCT_REFERENCE;

	return (construct_type_t*)reference;
}

static construct_type_t *parse_array_declarator(void)
{
	eat('[');
	add_anchor_token(']');

	parsed_array_t *array = obstack_alloc(&temp_obst, sizeof(array[0]));
	memset(array, 0, sizeof(array[0]));
	array->construct_type.kind = CONSTRUCT_ARRAY;

	if (token.type == T_static) {
		array->is_static = true;
		next_token();
	}

	type_qualifiers_t type_qualifiers = parse_type_qualifiers();
	if (type_qualifiers != 0) {
		if (token.type == T_static) {
			array->is_static = true;
			next_token();
		}
	}
	array->type_qualifiers = type_qualifiers;

	if (token.type == '*' && look_ahead(1)->type == ']') {
		array->is_variable = true;
		next_token();
	} else if (token.type != ']') {
		array->size = parse_assignment_expression();
	}

	rem_anchor_token(']');
	expect(']');

end_error:
	return &array->construct_type;
}

static construct_type_t *parse_function_declarator(scope_t *scope,
                                                   decl_modifiers_t modifiers)
{
	type_t          *type  = allocate_type_zero(TYPE_FUNCTION);
	function_type_t *ftype = &type->function;

	ftype->linkage = current_linkage;

	switch (modifiers & (DM_CDECL | DM_STDCALL | DM_FASTCALL | DM_THISCALL)) {
		case DM_NONE:     break;
		case DM_CDECL:    ftype->calling_convention = CC_CDECL;    break;
		case DM_STDCALL:  ftype->calling_convention = CC_STDCALL;  break;
		case DM_FASTCALL: ftype->calling_convention = CC_FASTCALL; break;
		case DM_THISCALL: ftype->calling_convention = CC_THISCALL; break;

		default:
			errorf(HERE, "multiple calling conventions in declaration");
			break;
	}

	parse_parameters(ftype, scope);

	construct_function_type_t *construct_function_type =
		obstack_alloc(&temp_obst, sizeof(construct_function_type[0]));
	memset(construct_function_type, 0, sizeof(construct_function_type[0]));
	construct_function_type->construct_type.kind = CONSTRUCT_FUNCTION;
	construct_function_type->function_type       = type;

	return &construct_function_type->construct_type;
}

typedef struct parse_declarator_env_t {
	decl_modifiers_t   modifiers;
	symbol_t          *symbol;
	source_position_t  source_position;
	scope_t            parameters;
} parse_declarator_env_t;

static construct_type_t *parse_inner_declarator(parse_declarator_env_t *env,
		bool may_be_abstract)
{
	/* construct a single linked list of construct_type_t's which describe
	 * how to construct the final declarator type */
	construct_type_t *first      = NULL;
	construct_type_t *last       = NULL;
	gnu_attribute_t  *attributes = NULL;

	decl_modifiers_t modifiers = parse_attributes(&attributes);

	/* MS __based extension */
	based_spec_t base_spec;
	base_spec.base_variable = NULL;

	for (;;) {
		construct_type_t *type;
		switch (token.type) {
			case '&':
				if (!(c_mode & _CXX))
					errorf(HERE, "references are only available for C++");
				if (base_spec.base_variable != NULL && warning.other) {
					warningf(&base_spec.source_position,
					         "__based does not precede a pointer operator, ignored");
				}
				type = parse_reference_declarator();
				/* consumed */
				base_spec.base_variable = NULL;
				break;

			case '*':
				type = parse_pointer_declarator(base_spec.base_variable);
				/* consumed */
				base_spec.base_variable = NULL;
				break;

			case T__based:
				next_token();
				expect('(');
				add_anchor_token(')');
				parse_microsoft_based(&base_spec);
				rem_anchor_token(')');
				expect(')');
				continue;

			default:
				goto ptr_operator_end;
		}

		if (last == NULL) {
			first = type;
			last  = type;
		} else {
			last->next = type;
			last       = type;
		}

		/* TODO: find out if this is correct */
		modifiers |= parse_attributes(&attributes);
	}
ptr_operator_end:
	if (base_spec.base_variable != NULL && warning.other) {
		warningf(&base_spec.source_position,
		         "__based does not precede a pointer operator, ignored");
	}

	if (env != NULL) {
		modifiers      |= env->modifiers;
		env->modifiers  = modifiers;
	}

	construct_type_t *inner_types = NULL;

	switch (token.type) {
	case T_IDENTIFIER:
		if (env == NULL) {
			errorf(HERE, "no identifier expected in typename");
		} else {
			env->symbol          = token.v.symbol;
			env->source_position = token.source_position;
		}
		next_token();
		break;
	case '(':
		next_token();
		add_anchor_token(')');
		inner_types = parse_inner_declarator(env, may_be_abstract);
		if (inner_types != NULL) {
			/* All later declarators only modify the return type */
			env = NULL;
		}
		rem_anchor_token(')');
		expect(')');
		break;
	default:
		if (may_be_abstract)
			break;
		parse_error_expected("while parsing declarator", T_IDENTIFIER, '(', NULL);
		eat_until_anchor();
		return NULL;
	}

	construct_type_t *p = last;

	while (true) {
		construct_type_t *type;
		switch (token.type) {
		case '(': {
			scope_t *scope = NULL;
			if (env != NULL)
				scope = &env->parameters;

			type = parse_function_declarator(scope, modifiers);
			break;
		}
		case '[':
			type = parse_array_declarator();
			break;
		default:
			goto declarator_finished;
		}

		/* insert in the middle of the list (behind p) */
		if (p != NULL) {
			type->next = p->next;
			p->next    = type;
		} else {
			type->next = first;
			first      = type;
		}
		if (last == p) {
			last = type;
		}
	}

declarator_finished:
	/* append inner_types at the end of the list, we don't to set last anymore
	 * as it's not needed anymore */
	if (last == NULL) {
		assert(first == NULL);
		first = inner_types;
	} else {
		last->next = inner_types;
	}

	return first;
end_error:
	return NULL;
}

static void parse_declaration_attributes(entity_t *entity)
{
	gnu_attribute_t  *attributes = NULL;
	decl_modifiers_t  modifiers  = parse_attributes(&attributes);

	if (entity == NULL)
		return;

	type_t *type;
	if (entity->kind == ENTITY_TYPEDEF) {
		modifiers |= entity->typedefe.modifiers;
		type       = entity->typedefe.type;
	} else {
		assert(is_declaration(entity));
		modifiers |= entity->declaration.modifiers;
		type       = entity->declaration.type;
	}
	if (type == NULL)
		return;

	/* handle these strange/stupid mode attributes */
	gnu_attribute_t *attribute = attributes;
	for ( ; attribute != NULL; attribute = attribute->next) {
		if (attribute->kind != GNU_AK_MODE || attribute->invalid)
			continue;

		atomic_type_kind_t  akind = attribute->u.akind;
		if (!is_type_signed(type)) {
			switch (akind) {
			case ATOMIC_TYPE_CHAR: akind = ATOMIC_TYPE_UCHAR; break;
			case ATOMIC_TYPE_SHORT: akind = ATOMIC_TYPE_USHORT; break;
			case ATOMIC_TYPE_INT: akind = ATOMIC_TYPE_UINT; break;
			case ATOMIC_TYPE_LONGLONG: akind = ATOMIC_TYPE_ULONGLONG; break;
			default:
				panic("invalid akind in mode attribute");
			}
		} else {
			switch (akind) {
			case ATOMIC_TYPE_CHAR: akind = ATOMIC_TYPE_SCHAR; break;
			case ATOMIC_TYPE_SHORT: akind = ATOMIC_TYPE_SHORT; break;
			case ATOMIC_TYPE_INT: akind = ATOMIC_TYPE_INT; break;
			case ATOMIC_TYPE_LONGLONG: akind = ATOMIC_TYPE_LONGLONG; break;
			default:
				panic("invalid akind in mode attribute");
			}
		}

		type = make_atomic_type(akind, type->base.qualifiers);
	}

	type_modifiers_t type_modifiers = type->base.modifiers;
	if (modifiers & DM_TRANSPARENT_UNION)
		type_modifiers |= TYPE_MODIFIER_TRANSPARENT_UNION;

	if (type->base.modifiers != type_modifiers) {
		type_t *copy = duplicate_type(type);
		copy->base.modifiers = type_modifiers;

		type = typehash_insert(copy);
		if (type != copy) {
			obstack_free(type_obst, copy);
		}
	}

	if (entity->kind == ENTITY_TYPEDEF) {
		entity->typedefe.type      = type;
		entity->typedefe.modifiers = modifiers;
	} else {
		entity->declaration.type      = type;
		entity->declaration.modifiers = modifiers;
	}
}

static type_t *construct_declarator_type(construct_type_t *construct_list, type_t *type)
{
	construct_type_t *iter = construct_list;
	for (; iter != NULL; iter = iter->next) {
		switch (iter->kind) {
		case CONSTRUCT_INVALID:
			internal_errorf(HERE, "invalid type construction found");
		case CONSTRUCT_FUNCTION: {
			construct_function_type_t *construct_function_type
				= (construct_function_type_t*) iter;

			type_t *function_type = construct_function_type->function_type;

			function_type->function.return_type = type;

			type_t *skipped_return_type = skip_typeref(type);
			/* §6.7.5.3(1) */
			if (is_type_function(skipped_return_type)) {
				errorf(HERE, "function returning function is not allowed");
			} else if (is_type_array(skipped_return_type)) {
				errorf(HERE, "function returning array is not allowed");
			} else {
				if (skipped_return_type->base.qualifiers != 0 && warning.other) {
					warningf(HERE,
						"type qualifiers in return type of function type are meaningless");
				}
			}

			type = function_type;
			break;
		}

		case CONSTRUCT_POINTER: {
			if (is_type_reference(skip_typeref(type)))
				errorf(HERE, "cannot declare a pointer to reference");

			parsed_pointer_t *parsed_pointer = (parsed_pointer_t*) iter;
			type = make_based_pointer_type(type, parsed_pointer->type_qualifiers, parsed_pointer->base_variable);
			continue;
		}

		case CONSTRUCT_REFERENCE:
			if (is_type_reference(skip_typeref(type)))
				errorf(HERE, "cannot declare a reference to reference");

			type = make_reference_type(type);
			continue;

		case CONSTRUCT_ARRAY: {
			if (is_type_reference(skip_typeref(type)))
				errorf(HERE, "cannot declare an array of references");

			parsed_array_t *parsed_array  = (parsed_array_t*) iter;
			type_t         *array_type    = allocate_type_zero(TYPE_ARRAY);

			expression_t *size_expression = parsed_array->size;
			if (size_expression != NULL) {
				size_expression
					= create_implicit_cast(size_expression, type_size_t);
			}

			array_type->base.qualifiers       = parsed_array->type_qualifiers;
			array_type->array.element_type    = type;
			array_type->array.is_static       = parsed_array->is_static;
			array_type->array.is_variable     = parsed_array->is_variable;
			array_type->array.size_expression = size_expression;

			if (size_expression != NULL) {
				if (is_constant_expression(size_expression)) {
					array_type->array.size_constant = true;
					array_type->array.size
						= fold_constant(size_expression);
				} else {
					array_type->array.is_vla = true;
				}
			}

			type_t *skipped_type = skip_typeref(type);
			/* §6.7.5.2(1) */
			if (is_type_incomplete(skipped_type)) {
				errorf(HERE, "array of incomplete type '%T' is not allowed", type);
			} else if (is_type_function(skipped_type)) {
				errorf(HERE, "array of functions is not allowed");
			}
			type = array_type;
			break;
		}
		}

		type_t *hashed_type = typehash_insert(type);
		if (hashed_type != type) {
			/* the function type was constructed earlier freeing it here will
			 * destroy other types... */
			if (iter->kind != CONSTRUCT_FUNCTION) {
				free_type(type);
			}
			type = hashed_type;
		}
	}

	return type;
}

static type_t *automatic_type_conversion(type_t *orig_type);

static type_t *semantic_parameter(const source_position_t *pos,
                                  type_t *type,
                                  const declaration_specifiers_t *specifiers,
                                  symbol_t *symbol)
{
	/* §6.7.5.3:7  A declaration of a parameter as ``array of type''
	 *             shall be adjusted to ``qualified pointer to type'',
	 *             [...]
	 * §6.7.5.3:8  A declaration of a parameter as ``function returning
	 *             type'' shall be adjusted to ``pointer to function
	 *             returning type'', as in 6.3.2.1. */
	type = automatic_type_conversion(type);

	if (specifiers->is_inline && is_type_valid(type)) {
		errorf(pos, "parameter '%Y' declared 'inline'", symbol);
	}

	/* §6.9.1:6  The declarations in the declaration list shall contain
	 *           no storage-class specifier other than register and no
	 *           initializations. */
	if (specifiers->thread_local || (
			specifiers->storage_class != STORAGE_CLASS_NONE   &&
			specifiers->storage_class != STORAGE_CLASS_REGISTER)
	   ) {
		errorf(pos, "invalid storage class for parameter '%Y'", symbol);
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
	env.modifiers = specifiers->modifiers;

	construct_type_t *construct_type =
		parse_inner_declarator(&env, (flags & DECL_MAY_BE_ABSTRACT) != 0);
	type_t           *orig_type      =
		construct_declarator_type(construct_type, specifiers->type);
	type_t           *type           = skip_typeref(orig_type);

	if (construct_type != NULL) {
		obstack_free(&temp_obst, construct_type);
	}

	entity_t *entity;
	if (specifiers->storage_class == STORAGE_CLASS_TYPEDEF) {
		entity                       = allocate_entity_zero(ENTITY_TYPEDEF);
		entity->base.symbol          = env.symbol;
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
			entity = allocate_entity_zero(ENTITY_COMPOUND_MEMBER);

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
		} else if (flags & DECL_IS_PARAMETER) {
			orig_type = semantic_parameter(&env.source_position, orig_type,
			                               specifiers, env.symbol);

			entity = allocate_entity_zero(ENTITY_PARAMETER);
		} else if (is_type_function(type)) {
			entity = allocate_entity_zero(ENTITY_FUNCTION);

			entity->function.is_inline  = specifiers->is_inline;
			entity->function.parameters = env.parameters;

			if (specifiers->thread_local || (
					specifiers->storage_class != STORAGE_CLASS_EXTERN &&
					specifiers->storage_class != STORAGE_CLASS_NONE   &&
					specifiers->storage_class != STORAGE_CLASS_STATIC)
			   ) {
				errorf(&env.source_position,
					   "invalid storage class for function '%Y'", env.symbol);
			}
		} else {
			entity = allocate_entity_zero(ENTITY_VARIABLE);

			entity->variable.get_property_sym = specifiers->get_property_sym;
			entity->variable.put_property_sym = specifiers->put_property_sym;
			if (specifiers->alignment != 0) {
				/* TODO: add checks here */
				entity->variable.alignment = specifiers->alignment;
			}

			if (specifiers->is_inline && is_type_valid(type)) {
				errorf(&env.source_position,
					   "variable '%Y' declared 'inline'", env.symbol);
			}

			entity->variable.thread_local = specifiers->thread_local;

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
				errorf(&env.source_position,
						"invalid storage class for variable '%Y'", env.symbol);
			}
		}

		entity->base.source_position          = env.source_position;
		entity->base.symbol                   = env.symbol;
		entity->base.namespc                  = NAMESPACE_NORMAL;
		entity->declaration.type              = orig_type;
		entity->declaration.modifiers         = env.modifiers;
		entity->declaration.deprecated_string = specifiers->deprecated_string;

		storage_class_t storage_class = specifiers->storage_class;
		entity->declaration.declared_storage_class = storage_class;

		if (storage_class == STORAGE_CLASS_NONE	&& current_scope != file_scope)
			storage_class = STORAGE_CLASS_AUTO;
		entity->declaration.storage_class = storage_class;
	}

	parse_declaration_attributes(entity);

	return entity;
}

static type_t *parse_abstract_declarator(type_t *base_type)
{
	construct_type_t *construct_type = parse_inner_declarator(NULL, 1);

	type_t *result = construct_declarator_type(construct_type, base_type);
	if (construct_type != NULL) {
		obstack_free(&temp_obst, construct_type);
	}

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
static void check_type_of_main(const entity_t *entity)
{
	const source_position_t *pos = &entity->base.source_position;
	if (entity->kind != ENTITY_FUNCTION) {
		warningf(pos, "'main' is not a function");
		return;
	}

	if (entity->declaration.storage_class == STORAGE_CLASS_STATIC) {
		warningf(pos, "'main' is normally a non-static function");
	}

	type_t *type = skip_typeref(entity->declaration.type);
	assert(is_type_function(type));

	function_type_t *func_type = &type->function;
	if (!types_compatible(skip_typeref(func_type->return_type), type_int)) {
		warningf(pos, "return type of 'main' should be 'int', but is '%T'",
		         func_type->return_type);
	}
	const function_parameter_t *parm = func_type->parameters;
	if (parm != NULL) {
		type_t *const first_type = parm->type;
		if (!types_compatible(skip_typeref(first_type), type_int)) {
			warningf(pos,
			         "first argument of 'main' should be 'int', but is '%T'",
			         first_type);
		}
		parm = parm->next;
		if (parm != NULL) {
			type_t *const second_type = parm->type;
			if (!types_compatible(skip_typeref(second_type), type_char_ptr_ptr)) {
				warningf(pos, "second argument of 'main' should be 'char**', but is '%T'", second_type);
			}
			parm = parm->next;
			if (parm != NULL) {
				type_t *const third_type = parm->type;
				if (!types_compatible(skip_typeref(third_type), type_char_ptr_ptr)) {
					warningf(pos, "third argument of 'main' should be 'char**', but is '%T'", third_type);
				}
				parm = parm->next;
				if (parm != NULL)
					goto warn_arg_count;
			}
		} else {
warn_arg_count:
			warningf(pos, "'main' takes only zero, two or three arguments");
		}
	}
}

/**
 * Check if a symbol is the equal to "main".
 */
static bool is_sym_main(const symbol_t *const sym)
{
	return strcmp(sym->string, "main") == 0;
}

static void error_redefined_as_different_kind(const source_position_t *pos,
		const entity_t *old, entity_kind_t new_kind)
{
	errorf(pos, "redeclaration of %s '%Y' as %s (declared %P)",
	       get_entity_kind_name(old->kind), old->base.symbol,
	       get_entity_kind_name(new_kind), &old->base.source_position);
}

/**
 * record entities for the NAMESPACE_NORMAL, and produce error messages/warnings
 * for various problems that occur for multiple definitions
 */
static entity_t *record_entity(entity_t *entity, const bool is_definition)
{
	const symbol_t *const    symbol  = entity->base.symbol;
	const namespace_tag_t    namespc = (namespace_tag_t)entity->base.namespc;
	const source_position_t *pos     = &entity->base.source_position;

	/* can happen in error cases */
	if (symbol == NULL)
		return entity;

	entity_t *previous_entity = get_entity(symbol, namespc);
	/* pushing the same entity twice will break the stack structure */
	assert(previous_entity != entity);

	if (entity->kind == ENTITY_FUNCTION) {
		type_t *const orig_type = entity->declaration.type;
		type_t *const type      = skip_typeref(orig_type);

		assert(is_type_function(type));
		if (type->function.unspecified_parameters &&
				warning.strict_prototypes &&
				previous_entity == NULL) {
			warningf(pos, "function declaration '%#T' is not a prototype",
					 orig_type, symbol);
		}

		if (warning.main && current_scope == file_scope
				&& is_sym_main(symbol)) {
			check_type_of_main(entity);
		}
	}

	if (is_declaration(entity) &&
			warning.nested_externs &&
			entity->declaration.storage_class == STORAGE_CLASS_EXTERN &&
			current_scope != file_scope) {
		warningf(pos, "nested extern declaration of '%#T'",
		         entity->declaration.type, symbol);
	}

	if (previous_entity != NULL &&
			previous_entity->base.parent_scope == &current_function->parameters &&
			previous_entity->base.parent_scope->depth + 1 == current_scope->depth) {
		assert(previous_entity->kind == ENTITY_PARAMETER);
		errorf(pos,
		       "declaration '%#T' redeclares the parameter '%#T' (declared %P)",
					 entity->declaration.type, symbol,
					 previous_entity->declaration.type, symbol,
					 &previous_entity->base.source_position);
		goto finish;
	}

	if (previous_entity != NULL &&
			previous_entity->base.parent_scope == current_scope) {
		if (previous_entity->kind != entity->kind) {
			error_redefined_as_different_kind(pos, previous_entity,
			                                  entity->kind);
			goto finish;
		}
		if (previous_entity->kind == ENTITY_ENUM_VALUE) {
			errorf(pos, "redeclaration of enum entry '%Y' (declared %P)",
				   symbol, &previous_entity->base.source_position);
			goto finish;
		}
		if (previous_entity->kind == ENTITY_TYPEDEF) {
			/* TODO: C++ allows this for exactly the same type */
			errorf(pos, "redefinition of typedef '%Y' (declared %P)",
			       symbol, &previous_entity->base.source_position);
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
			prev_decl->deprecated_string      = decl->deprecated_string;
			return previous_entity;
		}

		type_t *const orig_type = decl->type;
		assert(orig_type != NULL);
		type_t *const type      = skip_typeref(orig_type);
		type_t *      prev_type = skip_typeref(prev_decl->type);

		if (!types_compatible(type, prev_type)) {
			errorf(pos,
				   "declaration '%#T' is incompatible with '%#T' (declared %P)",
				   orig_type, symbol, prev_decl->type, symbol,
				   &previous_entity->base.source_position);
		} else {
			unsigned old_storage_class = prev_decl->storage_class;
			if (warning.redundant_decls	          &&
					is_definition                     &&
					!prev_decl->used                  &&
					!(prev_decl->modifiers & DM_USED) &&
					prev_decl->storage_class == STORAGE_CLASS_STATIC) {
				warningf(&previous_entity->base.source_position,
				         "unnecessary static forward declaration for '%#T'",
				         prev_decl->type, symbol);
			}

			unsigned new_storage_class = decl->storage_class;
			if (is_type_incomplete(prev_type)) {
				prev_decl->type = type;
				prev_type       = type;
			}

			/* pretend no storage class means extern for function
			 * declarations (except if the previous declaration is neither
			 * none nor extern) */
			if (entity->kind == ENTITY_FUNCTION) {
				if (prev_type->function.unspecified_parameters) {
					prev_decl->type = type;
					prev_type       = type;
				}

				switch (old_storage_class) {
				case STORAGE_CLASS_NONE:
					old_storage_class = STORAGE_CLASS_EXTERN;
					/* FALLTHROUGH */

				case STORAGE_CLASS_EXTERN:
					if (is_definition) {
						if (warning.missing_prototypes &&
						    prev_type->function.unspecified_parameters &&
						    !is_sym_main(symbol)) {
							warningf(pos, "no previous prototype for '%#T'",
									 orig_type, symbol);
						}
					} else if (new_storage_class == STORAGE_CLASS_NONE) {
						new_storage_class = STORAGE_CLASS_EXTERN;
					}
					break;

				default:
					break;
				}
			}

			if (old_storage_class == STORAGE_CLASS_EXTERN &&
					new_storage_class == STORAGE_CLASS_EXTERN) {
warn_redundant_declaration:
				if (!is_definition           &&
				    warning.redundant_decls  &&
				    is_type_valid(prev_type) &&
				    strcmp(previous_entity->base.source_position.input_name,
				           "<builtin>") != 0) {
					warningf(pos,
					         "redundant declaration for '%Y' (declared %P)",
					         symbol, &previous_entity->base.source_position);
				}
			} else if (current_function == NULL) {
				if (old_storage_class != STORAGE_CLASS_STATIC &&
				    new_storage_class == STORAGE_CLASS_STATIC) {
					errorf(pos,
					       "static declaration of '%Y' follows non-static declaration (declared %P)",
					       symbol, &previous_entity->base.source_position);
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
					errorf(pos, "redeclaration of '%Y' (declared %P)",
					       symbol, &previous_entity->base.source_position);
				} else {
					errorf(pos,
					       "redeclaration of '%Y' with different linkage (declared %P)",
					       symbol, &previous_entity->base.source_position);
				}
			}
		}

		prev_decl->modifiers |= decl->modifiers;
		if (entity->kind == ENTITY_FUNCTION) {
			previous_entity->function.is_inline |= entity->function.is_inline;
		}
		return previous_entity;
	}

	if (entity->kind == ENTITY_FUNCTION) {
		if (is_definition &&
				entity->declaration.storage_class != STORAGE_CLASS_STATIC) {
			if (warning.missing_prototypes && !is_sym_main(symbol)) {
				warningf(pos, "no previous prototype for '%#T'",
				         entity->declaration.type, symbol);
			} else if (warning.missing_declarations && !is_sym_main(symbol)) {
				warningf(pos, "no previous declaration for '%#T'",
				         entity->declaration.type, symbol);
			}
		}
	} else if (warning.missing_declarations &&
			entity->kind == ENTITY_VARIABLE &&
			current_scope == file_scope) {
		declaration_t *declaration = &entity->declaration;
		if (declaration->storage_class == STORAGE_CLASS_NONE) {
			warningf(pos, "no previous declaration for '%#T'",
			         declaration->type, symbol);
		}
	}

finish:
	assert(entity->base.parent_scope == NULL);
	assert(current_scope != NULL);

	entity->base.parent_scope = current_scope;
	entity->base.namespc      = NAMESPACE_NORMAL;
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

static bool is_declaration_specifier(const token_t *token,
                                     bool only_specifiers_qualifiers)
{
	switch (token->type) {
		TYPE_SPECIFIERS
		TYPE_QUALIFIERS
			return true;
		case T_IDENTIFIER:
			return is_typedef_symbol(token->v.symbol);

		case T___extension__:
		STORAGE_CLASSES
			return !only_specifiers_qualifiers;

		default:
			return false;
	}
}

static void parse_init_declarator_rest(entity_t *entity)
{
	assert(is_declaration(entity));
	declaration_t *const declaration = &entity->declaration;

	eat('=');

	type_t *orig_type = declaration->type;
	type_t *type      = skip_typeref(orig_type);

	if (entity->kind == ENTITY_VARIABLE
			&& entity->variable.initializer != NULL) {
		parser_error_multiple_definition(entity, HERE);
	}

	bool must_be_constant = false;
	if (declaration->storage_class == STORAGE_CLASS_STATIC ||
	    entity->base.parent_scope  == file_scope) {
		must_be_constant = true;
	}

	if (is_type_function(type)) {
		errorf(&entity->base.source_position,
		       "function '%#T' is initialized like a variable",
		       orig_type, entity->base.symbol);
		orig_type = type_error_type;
	}

	parse_initializer_env_t env;
	env.type             = orig_type;
	env.must_be_constant = must_be_constant;
	env.entity           = entity;
	current_init_decl    = entity;

	initializer_t *initializer = parse_initializer(&env);
	current_init_decl = NULL;

	if (entity->kind == ENTITY_VARIABLE) {
		/* § 6.7.5 (22)  array initializers for arrays with unknown size
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

	if (warning.other) {
		if (specifiers->storage_class != STORAGE_CLASS_NONE ||
				specifiers->thread_local) {
			warningf(&specifiers->source_position,
			         "useless storage class in empty declaration");
		}

		type_t *type = specifiers->type;
		switch (type->kind) {
			case TYPE_COMPOUND_STRUCT:
			case TYPE_COMPOUND_UNION: {
				if (type->compound.compound->base.symbol == NULL) {
					warningf(&specifiers->source_position,
					         "unnamed struct/union that defines no instances");
				}
				break;
			}

			case TYPE_ENUM:
				break;

			default:
				warningf(&specifiers->source_position, "empty declaration");
				break;
		}
	}
}

static void check_variable_type_complete(entity_t *ent)
{
	if (ent->kind != ENTITY_VARIABLE)
		return;

	/* §6.7:7  If an identifier for an object is declared with no linkage, the
	 *         type for the object shall be complete [...] */
	declaration_t *decl = &ent->declaration;
	if (decl->storage_class != STORAGE_CLASS_NONE)
		return;

	type_t *const orig_type = decl->type;
	type_t *const type      = skip_typeref(orig_type);
	if (!is_type_incomplete(type))
		return;

	/* GCC allows global arrays without size and assigns them a length of one,
	 * if no different declaration follows */
	if (is_type_array(type) &&
			c_mode & _GNUC      &&
			ent->base.parent_scope == file_scope) {
		ARR_APP1(declaration_t*, incomplete_arrays, decl);
		return;
	}

	errorf(&ent->base.source_position, "variable '%#T' has incomplete type",
			orig_type, ent->base.symbol);
}


static void parse_declaration_rest(entity_t *ndeclaration,
		const declaration_specifiers_t *specifiers,
		parsed_declaration_func         finished_declaration,
		declarator_flags_t              flags)
{
	add_anchor_token(';');
	add_anchor_token(',');
	while (true) {
		entity_t *entity = finished_declaration(ndeclaration, token.type == '=');

		if (token.type == '=') {
			parse_init_declarator_rest(entity);
		} else if (entity->kind == ENTITY_VARIABLE) {
			/* ISO/IEC 14882:1998(E) §8.5.3:3  The initializer can be omitted
			 * [...] where the extern specifier is explicitly used. */
			declaration_t *decl = &entity->declaration;
			if (decl->storage_class != STORAGE_CLASS_EXTERN) {
				type_t *type = decl->type;
				if (is_type_reference(skip_typeref(type))) {
					errorf(&entity->base.source_position,
							"reference '%#T' must be initialized",
							type, entity->base.symbol);
				}
			}
		}

		check_variable_type_complete(entity);

		if (token.type != ',')
			break;
		eat(',');

		add_anchor_token('=');
		ndeclaration = parse_declarator(specifiers, flags);
		rem_anchor_token('=');
	}
	expect(';');

end_error:
	anonymous_entity = NULL;
	rem_anchor_token(';');
	rem_anchor_token(',');
}

static entity_t *finished_kr_declaration(entity_t *entity, bool is_definition)
{
	symbol_t *symbol = entity->base.symbol;
	if (symbol == NULL) {
		errorf(HERE, "anonymous declaration not valid as function parameter");
		return entity;
	}

	assert(entity->base.namespc == NAMESPACE_NORMAL);
	entity_t *previous_entity = get_entity(symbol, NAMESPACE_NORMAL);
	if (previous_entity == NULL
			|| previous_entity->base.parent_scope != current_scope) {
		errorf(HERE, "expected declaration of a function parameter, found '%Y'",
		       symbol);
		return entity;
	}

	if (is_definition) {
		errorf(HERE, "parameter '%Y' is initialised", entity->base.symbol);
	}

	return record_entity(entity, false);
}

static void parse_declaration(parsed_declaration_func finished_declaration,
                              declarator_flags_t      flags)
{
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));

	add_anchor_token(';');
	parse_declaration_specifiers(&specifiers);
	rem_anchor_token(';');

	if (token.type == ';') {
		parse_anonymous_declaration_rest(&specifiers);
	} else {
		entity_t *entity = parse_declarator(&specifiers, flags);
		parse_declaration_rest(entity, &specifiers, finished_declaration, flags);
	}
}

static type_t *get_default_promoted_type(type_t *orig_type)
{
	type_t *result = orig_type;

	type_t *type = skip_typeref(orig_type);
	if (is_type_integer(type)) {
		result = promote_integer(type);
	} else if (type == type_float) {
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

	/* push function parameters */
	size_t const  top       = environment_top();
	scope_t      *old_scope = scope_push(&entity->function.parameters);

	entity_t *parameter = entity->function.parameters.entities;
	for ( ; parameter != NULL; parameter = parameter->base.next) {
		assert(parameter->base.parent_scope == NULL);
		parameter->base.parent_scope = current_scope;
		environment_push(parameter);
	}

	/* parse declaration list */
	for (;;) {
		switch (token.type) {
			DECLARATION_START
			case T___extension__:
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

	/* pop function parameters */
	assert(current_scope == &entity->function.parameters);
	scope_pop(old_scope);
	environment_pop_to(top);

	/* update function type */
	type_t *new_type = duplicate_type(type);

	function_parameter_t *parameters     = NULL;
	function_parameter_t *last_parameter = NULL;

	parameter = entity->function.parameters.entities;
	for (; parameter != NULL; parameter = parameter->base.next) {
		type_t *parameter_type = parameter->declaration.type;
		if (parameter_type == NULL) {
			if (strict_mode) {
				errorf(HERE, "no type specified for function parameter '%Y'",
				       parameter->base.symbol);
			} else {
				if (warning.implicit_int) {
					warningf(HERE, "no type specified for function parameter '%Y', using 'int'",
					         parameter->base.symbol);
				}
				parameter_type              = type_int;
				parameter->declaration.type = parameter_type;
			}
		}

		semantic_parameter_incomplete(parameter);
		parameter_type = parameter->declaration.type;

		/*
		 * we need the default promoted types for the function type
		 */
		parameter_type = get_default_promoted_type(parameter_type);

		function_parameter_t *function_parameter
			= obstack_alloc(type_obst, sizeof(function_parameter[0]));
		memset(function_parameter, 0, sizeof(function_parameter[0]));

		function_parameter->type = parameter_type;
		if (last_parameter != NULL) {
			last_parameter->next = function_parameter;
		} else {
			parameters = function_parameter;
		}
		last_parameter = function_parameter;
	}

	/* § 6.9.1.7: A K&R style parameter list does NOT act as a function
	 * prototype */
	new_type->function.parameters             = parameters;
	new_type->function.unspecified_parameters = true;

	type = typehash_insert(new_type);
	if (type != new_type) {
		obstack_free(type_obst, new_type);
	}

	entity->declaration.type = type;

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
		diagnosticf("%s: In function '%Y':\n",
		            current_function->base.base.source_position.input_name,
		            current_function->base.base.symbol);
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
		/* skip computed gotos */
		if (goto_statement->expression != NULL)
			continue;

		label_t *label = goto_statement->label;

		label->used = true;
		if (label->base.source_position.input_name == NULL) {
			print_in_function();
			errorf(&goto_statement->base.source_position,
			       "label '%Y' used but not defined", label->base.symbol);
		 }
	}

	if (warning.unused_label) {
		for (const label_statement_t *label_statement = label_first;
			 label_statement != NULL;
			 label_statement = label_statement->next) {
			label_t *label = label_statement->label;

			if (! label->used) {
				print_in_function();
				warningf(&label_statement->base.source_position,
				         "label '%Y' defined but not used", label->base.symbol);
			}
		}
	}
}

static void warn_unused_entity(entity_t *entity, entity_t *end)
{
	for (; entity != NULL; entity = entity->base.next) {
		if (!is_declaration(entity))
			continue;

		declaration_t *declaration = &entity->declaration;
		if (declaration->implicit)
			continue;

		if (!declaration->used) {
			print_in_function();
			const char *what = get_entity_kind_name(entity->kind);
			warningf(&entity->base.source_position, "%s '%Y' is unused",
			         what, entity->base.symbol);
		} else if (entity->kind == ENTITY_VARIABLE && !entity->variable.read) {
			print_in_function();
			const char *what = get_entity_kind_name(entity->kind);
			warningf(&entity->base.source_position, "%s '%Y' is never read",
			         what, entity->base.symbol);
		}

		if (entity == end)
			break;
	}
}

static void check_unused_variables(statement_t *const stmt, void *const env)
{
	(void)env;

	switch (stmt->kind) {
		case STATEMENT_DECLARATION: {
			declaration_statement_t const *const decls = &stmt->declaration;
			warn_unused_entity(decls->declarations_begin,
			                   decls->declarations_end);
			return;
		}

		case STATEMENT_FOR:
			warn_unused_entity(stmt->fors.scope.entities, NULL);
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
	if (warning.unused_parameter) {
		const scope_t *scope = &current_function->parameters;

		/* do not issue unused warnings for main */
		if (!is_sym_main(current_function->base.base.symbol)) {
			warn_unused_entity(scope->entities, NULL);
		}
	}
	if (warning.unused_variable) {
		walk_statements(current_function->statement, check_unused_variables,
		                NULL);
	}
}

static int determine_truth(expression_t const* const cond)
{
	return
		!is_constant_expression(cond) ? 0 :
		fold_constant(cond) != 0      ? 1 :
		-1;
}

static void check_reachable(statement_t *);

static bool expression_returns(expression_t const *const expr)
{
	switch (expr->kind) {
		case EXPR_CALL: {
			expression_t const *const func = expr->call.function;
			if (func->kind == EXPR_REFERENCE) {
				entity_t *entity = func->reference.entity;
				if (entity->kind == ENTITY_FUNCTION
						&& entity->declaration.modifiers & DM_NORETURN)
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
		case EXPR_REFERENCE_ENUM_VALUE:
		case EXPR_CONST:
		case EXPR_CHARACTER_CONSTANT:
		case EXPR_WIDE_CHARACTER_CONSTANT:
		case EXPR_STRING_LITERAL:
		case EXPR_WIDE_STRING_LITERAL:
		case EXPR_COMPOUND_LITERAL: // TODO descend into initialisers
		case EXPR_LABEL_ADDRESS:
		case EXPR_CLASSIFY_TYPE:
		case EXPR_SIZEOF: // TODO handle obscure VLA case
		case EXPR_ALIGNOF:
		case EXPR_FUNCNAME:
		case EXPR_BUILTIN_SYMBOL:
		case EXPR_BUILTIN_CONSTANT_P:
		case EXPR_BUILTIN_PREFETCH:
		case EXPR_OFFSETOF:
		case EXPR_INVALID:
			return true;

		case EXPR_STATEMENT:
			check_reachable(expr->statement.statement);
			// TODO check if statement can be left
			return true;

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

		EXPR_UNARY_CASES_MANDATORY
			return expression_returns(expr->unary.value);

		case EXPR_UNARY_THROW:
			return false;

		EXPR_BINARY_CASES
			// TODO handle constant lhs of && and ||
			return
				expression_returns(expr->binary.left) &&
				expression_returns(expr->binary.right);

		case EXPR_UNKNOWN:
			break;
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
		case STATEMENT_INVALID:
		case STATEMENT_EMPTY:
		case STATEMENT_LOCAL_LABEL:
		case STATEMENT_ASM:
			next = stmt->base.next;
			break;

		case STATEMENT_DECLARATION: {
			declaration_statement_t const *const decl = &stmt->declaration;
			entity_t                const *      ent  = decl->declarations_begin;
			entity_t                const *const last = decl->declarations_end;
			if (ent != NULL) {
				for (;; ent = ent->base.next) {
					if (ent->kind                 == ENTITY_VARIABLE &&
							ent->variable.initializer != NULL            &&
							!initializer_returns(ent->variable.initializer)) {
						return;
					}
					if (ent == last)
						break;
				}
			}
			next = stmt->base.next;
			break;
		}

		case STATEMENT_COMPOUND:
			next = stmt->compound.statements;
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

			if (is_constant_expression(expr)) {
				long                    const val      = fold_constant(expr);
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

		case STATEMENT_CONTINUE: {
			statement_t *parent = stmt;
			for (;;) {
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
		}

		case STATEMENT_BREAK: {
			statement_t *parent = stmt;
			for (;;) {
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
		}

		case STATEMENT_GOTO:
			if (stmt->gotos.expression) {
				if (!expression_returns(stmt->gotos.expression))
					return;

				statement_t *parent = stmt->base.parent;
				if (parent == NULL) /* top level goto */
					return;
				next = parent;
			} else {
				next = stmt->gotos.label->statement;
				if (next == NULL) /* missing label */
					return;
			}
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
	}

	while (next == NULL) {
		next = last->base.parent;
		if (next == NULL) {
			noreturn_candidate = false;

			type_t *const type = current_function->base.type;
			assert(is_type_function(type));
			type_t *const ret  = skip_typeref(type->function.return_type);
			if (warning.return_type                    &&
			    !is_type_atomic(ret, ATOMIC_TYPE_VOID) &&
			    is_type_valid(ret)                     &&
			    !is_sym_main(current_function->base.base.symbol)) {
				warningf(&stmt->base.source_position,
				         "control reaches end of non-void function");
			}
			return;
		}

		switch (next->kind) {
			case STATEMENT_INVALID:
			case STATEMENT_EMPTY:
			case STATEMENT_DECLARATION:
			case STATEMENT_LOCAL_LABEL:
			case STATEMENT_EXPRESSION:
			case STATEMENT_ASM:
			case STATEMENT_RETURN:
			case STATEMENT_CONTINUE:
			case STATEMENT_BREAK:
			case STATEMENT_GOTO:
			case STATEMENT_LEAVE:
				panic("invalid control flow in function");

			case STATEMENT_COMPOUND:
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
					warningf(&cond->base.source_position,
					         "condition of do-while-loop is unreachable");
				}
			}
			return;

		case STATEMENT_FOR: {
			for_statement_t const* const fors = &stmt->fors;

			// if init and step are unreachable, cond is unreachable, too
			if (!stmt->base.reachable && !fors->step_reachable) {
				warningf(&stmt->base.source_position, "statement is unreachable");
			} else {
				if (!stmt->base.reachable && fors->initialisation != NULL) {
					warningf(&fors->initialisation->base.source_position,
					         "initialisation of for-statement is unreachable");
				}

				if (!fors->condition_reachable && fors->condition != NULL) {
					warningf(&fors->condition->base.source_position,
					         "condition of for-statement is unreachable");
				}

				if (!fors->step_reachable && fors->step != NULL) {
					warningf(&fors->step->base.source_position,
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
			for (;; ent = ent->base.next) {
				if (ent->kind                 == ENTITY_VARIABLE &&
						ent->variable.initializer != NULL) {
					goto warn_unreachable;
				}
				if (ent == last)
					return;
			}
		}

		default:
warn_unreachable:
			if (!stmt->base.reachable)
				warningf(&stmt->base.source_position, "statement is unreachable");
			return;
	}
}

static void parse_external_declaration(void)
{
	/* function-definitions and declarations both start with declaration
	 * specifiers */
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));

	add_anchor_token(';');
	parse_declaration_specifiers(&specifiers);
	rem_anchor_token(';');

	/* must be a declaration */
	if (token.type == ';') {
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
	switch (token.type) {
		case ',':
		case ';':
		case '=':
			parse_declaration_rest(ndeclaration, &specifiers, record_entity,
					DECL_FLAGS_NONE);
			return;
	}

	/* must be a function definition */
	parse_kr_declaration_list(ndeclaration);

	if (token.type != '{') {
		parse_error_expected("while parsing function definition", '{', NULL);
		eat_until_matching_token(';');
		return;
	}

	assert(is_declaration(ndeclaration));
	type_t *type = skip_typeref(ndeclaration->declaration.type);

	if (!is_type_function(type)) {
		if (is_type_valid(type)) {
			errorf(HERE, "declarator '%#T' has a body but is not a function type",
			       type, ndeclaration->base.symbol);
		}
		eat_block();
		return;
	}

	if (warning.aggregate_return &&
	    is_type_compound(skip_typeref(type->function.return_type))) {
		warningf(HERE, "function '%Y' returns an aggregate",
		         ndeclaration->base.symbol);
	}
	if (warning.traditional && !type->function.unspecified_parameters) {
		warningf(HERE, "traditional C rejects ISO C style function definition of function '%Y'",
			ndeclaration->base.symbol);
	}
	if (warning.old_style_definition && type->function.unspecified_parameters) {
		warningf(HERE, "old-style function definition '%Y'",
			ndeclaration->base.symbol);
	}

	/* § 6.7.5.3 (14) a function definition with () means no
	 * parameters (and not unspecified parameters) */
	if (type->function.unspecified_parameters
			&& type->function.parameters == NULL
			&& !type->function.kr_style_parameters) {
		type_t *duplicate = duplicate_type(type);
		duplicate->function.unspecified_parameters = false;

		type = typehash_insert(duplicate);
		if (type != duplicate) {
			obstack_free(type_obst, duplicate);
		}
		ndeclaration->declaration.type = type;
	}

	entity_t *const entity = record_entity(ndeclaration, true);
	assert(entity->kind == ENTITY_FUNCTION);
	assert(ndeclaration->kind == ENTITY_FUNCTION);

	function_t *function = &entity->function;
	if (ndeclaration != entity) {
		function->parameters = ndeclaration->function.parameters;
	}
	assert(is_declaration(entity));
	type = skip_typeref(entity->declaration.type);

	/* push function parameters and switch scope */
	size_t const  top       = environment_top();
	scope_t      *old_scope = scope_push(&function->parameters);

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
		current_function                 = function;
		current_parent                   = NULL;

		goto_first   = NULL;
		goto_anchor  = &goto_first;
		label_first  = NULL;
		label_anchor = &label_first;

		statement_t *const body = parse_compound_statement(false);
		function->statement = body;
		first_err = true;
		check_labels();
		check_declarations();
		if (warning.return_type      ||
		    warning.unreachable_code ||
		    (warning.missing_noreturn
		     && !(function->base.modifiers & DM_NORETURN))) {
			noreturn_candidate = true;
			check_reachable(body);
			if (warning.unreachable_code)
				walk_statements(body, check_unreachable, NULL);
			if (warning.missing_noreturn &&
			    noreturn_candidate       &&
			    !(function->base.modifiers & DM_NORETURN)) {
				warningf(&body->base.source_position,
				         "function '%#T' is candidate for attribute 'noreturn'",
				         type, entity->base.symbol);
			}
		}

		assert(current_parent   == NULL);
		assert(current_function == function);
		current_function = old_current_function;
		label_pop_to(label_stack_top);
	}

	assert(current_scope == &function->parameters);
	scope_pop(old_scope);
	environment_pop_to(top);
}

static type_t *make_bitfield_type(type_t *base_type, expression_t *size,
                                  source_position_t *source_position,
                                  const symbol_t *symbol)
{
	type_t *type = allocate_type_zero(TYPE_BITFIELD);

	type->bitfield.base_type       = base_type;
	type->bitfield.size_expression = size;

	il_size_t bit_size;
	type_t *skipped_type = skip_typeref(base_type);
	if (!is_type_integer(skipped_type)) {
		errorf(HERE, "bitfield base type '%T' is not an integer type",
			base_type);
		bit_size = 0;
	} else {
		bit_size = skipped_type->base.size * 8;
	}

	if (is_constant_expression(size)) {
		long v = fold_constant(size);

		if (v < 0) {
			errorf(source_position, "negative width in bit-field '%Y'", symbol);
		} else if (v == 0) {
			errorf(source_position, "zero width for bit-field '%Y'", symbol);
		} else if (bit_size > 0 && (il_size_t)v > bit_size) {
			errorf(source_position, "width of '%Y' exceeds its type", symbol);
		} else {
			type->bitfield.bit_size = v;
		}
	}

	return type;
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
			type_t *type = skip_typeref(iter->declaration.type);
			if (is_type_compound(type)) {
				entity_t *result
					= find_compound_entry(type->compound.compound, symbol);
				if (result != NULL)
					return result;
			}
			continue;
		}
	}

	return NULL;
}

static void parse_compound_declarators(compound_t *compound,
		const declaration_specifiers_t *specifiers)
{
	while (true) {
		entity_t *entity;

		if (token.type == ':') {
			source_position_t source_position = *HERE;
			next_token();

			type_t *base_type = specifiers->type;
			expression_t *size = parse_constant_expression();

			type_t *type = make_bitfield_type(base_type, size,
					&source_position, sym_anonymous);

			entity = allocate_entity_zero(ENTITY_COMPOUND_MEMBER);
			entity->base.namespc                       = NAMESPACE_NORMAL;
			entity->base.source_position               = source_position;
			entity->declaration.declared_storage_class = STORAGE_CLASS_NONE;
			entity->declaration.storage_class          = STORAGE_CLASS_NONE;
			entity->declaration.modifiers              = specifiers->modifiers;
			entity->declaration.type                   = type;
		} else {
			entity = parse_declarator(specifiers,
					DECL_MAY_BE_ABSTRACT | DECL_CREATE_COMPOUND_MEMBER);
			assert(entity->kind == ENTITY_COMPOUND_MEMBER);

			if (token.type == ':') {
				source_position_t source_position = *HERE;
				next_token();
				expression_t *size = parse_constant_expression();

				type_t *type = entity->declaration.type;
				type_t *bitfield_type = make_bitfield_type(type, size,
						&source_position, entity->base.symbol);
				entity->declaration.type = bitfield_type;
			}
		}

		/* make sure we don't define a symbol multiple times */
		symbol_t *symbol = entity->base.symbol;
		if (symbol != NULL) {
			entity_t *prev = find_compound_entry(compound, symbol);

			if (prev != NULL) {
				errorf(&entity->base.source_position,
				       "multiple declarations of symbol '%Y' (declared %P)",
				       symbol, &prev->base.source_position);
			}
		}

		append_entity(&compound->members, entity);

		type_t *orig_type = entity->declaration.type;
		type_t *type      = skip_typeref(orig_type);
		if (is_type_function(type)) {
			errorf(&entity->base.source_position,
					"compound member '%Y' must not have function type '%T'",
					entity->base.symbol, orig_type);
		} else if (is_type_incomplete(type)) {
			/* §6.7.2.1:16 flexible array member */
			if (is_type_array(type) &&
					token.type == ';'   &&
					look_ahead(1)->type == '}') {
				compound->has_flexible_member = true;
			} else {
				errorf(&entity->base.source_position,
						"compound member '%Y' has incomplete type '%T'",
						entity->base.symbol, orig_type);
			}
		}

		if (token.type != ',')
			break;
		next_token();
	}
	expect(';');

end_error:
	anonymous_entity = NULL;
}

static void parse_compound_type_entries(compound_t *compound)
{
	eat('{');
	add_anchor_token('}');

	while (token.type != '}') {
		if (token.type == T_EOF) {
			errorf(HERE, "EOF while parsing struct");
			break;
		}
		declaration_specifiers_t specifiers;
		memset(&specifiers, 0, sizeof(specifiers));
		parse_declaration_specifiers(&specifiers);

		parse_compound_declarators(compound, &specifiers);
	}
	rem_anchor_token('}');
	next_token();

	/* §6.7.2.1:7 */
	compound->complete = true;
}

static type_t *parse_typename(void)
{
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));
	parse_declaration_specifiers(&specifiers);
	if (specifiers.storage_class != STORAGE_CLASS_NONE ||
			specifiers.thread_local) {
		/* TODO: improve error message, user does probably not know what a
		 * storage class is...
		 */
		errorf(HERE, "typename may not have a storage class");
	}

	type_t *result = parse_abstract_declarator(specifiers.type);

	return result;
}




typedef expression_t* (*parse_expression_function)(void);
typedef expression_t* (*parse_expression_infix_function)(expression_t *left);

typedef struct expression_parser_function_t expression_parser_function_t;
struct expression_parser_function_t {
	parse_expression_function        parser;
	unsigned                         infix_precedence;
	parse_expression_infix_function  infix_parser;
};

expression_parser_function_t expression_parsers[T_LAST_TOKEN];

/**
 * Prints an error message if an expression was expected but not read
 */
static expression_t *expected_expression_error(void)
{
	/* skip the error message if the error token was read */
	if (token.type != T_ERROR) {
		errorf(HERE, "expected expression, got token %K", &token);
	}
	next_token();

	return create_invalid_expression();
}

/**
 * Parse a string constant.
 */
static expression_t *parse_string_const(void)
{
	wide_string_t wres;
	if (token.type == T_STRING_LITERAL) {
		string_t res = token.v.string;
		next_token();
		while (token.type == T_STRING_LITERAL) {
			res = concat_strings(&res, &token.v.string);
			next_token();
		}
		if (token.type != T_WIDE_STRING_LITERAL) {
			expression_t *const cnst = allocate_expression_zero(EXPR_STRING_LITERAL);
			/* note: that we use type_char_ptr here, which is already the
			 * automatic converted type. revert_automatic_type_conversion
			 * will construct the array type */
			cnst->base.type    = warning.write_strings ? type_const_char_ptr : type_char_ptr;
			cnst->string.value = res;
			return cnst;
		}

		wres = concat_string_wide_string(&res, &token.v.wide_string);
	} else {
		wres = token.v.wide_string;
	}
	next_token();

	for (;;) {
		switch (token.type) {
			case T_WIDE_STRING_LITERAL:
				wres = concat_wide_strings(&wres, &token.v.wide_string);
				break;

			case T_STRING_LITERAL:
				wres = concat_wide_string_string(&wres, &token.v.string);
				break;

			default: {
				expression_t *const cnst = allocate_expression_zero(EXPR_WIDE_STRING_LITERAL);
				cnst->base.type         = warning.write_strings ? type_const_wchar_t_ptr : type_wchar_t_ptr;
				cnst->wide_string.value = wres;
				return cnst;
			}
		}
		next_token();
	}
}

/**
 * Parse a boolean constant.
 */
static expression_t *parse_bool_const(bool value)
{
	expression_t *cnst       = allocate_expression_zero(EXPR_CONST);
	cnst->base.type          = type_bool;
	cnst->conste.v.int_value = value;

	next_token();

	return cnst;
}

/**
 * Parse an integer constant.
 */
static expression_t *parse_int_const(void)
{
	expression_t *cnst       = allocate_expression_zero(EXPR_CONST);
	cnst->base.type          = token.datatype;
	cnst->conste.v.int_value = token.v.intvalue;

	next_token();

	return cnst;
}

/**
 * Parse a character constant.
 */
static expression_t *parse_character_constant(void)
{
	expression_t *cnst = allocate_expression_zero(EXPR_CHARACTER_CONSTANT);
	cnst->base.type          = token.datatype;
	cnst->conste.v.character = token.v.string;

	if (cnst->conste.v.character.size != 1) {
		if (!GNU_MODE) {
			errorf(HERE, "more than 1 character in character constant");
		} else if (warning.multichar) {
			warningf(HERE, "multi-character character constant");
		}
	}
	next_token();

	return cnst;
}

/**
 * Parse a wide character constant.
 */
static expression_t *parse_wide_character_constant(void)
{
	expression_t *cnst = allocate_expression_zero(EXPR_WIDE_CHARACTER_CONSTANT);
	cnst->base.type               = token.datatype;
	cnst->conste.v.wide_character = token.v.wide_string;

	if (cnst->conste.v.wide_character.size != 1) {
		if (!GNU_MODE) {
			errorf(HERE, "more than 1 character in character constant");
		} else if (warning.multichar) {
			warningf(HERE, "multi-character character constant");
		}
	}
	next_token();

	return cnst;
}

/**
 * Parse a float constant.
 */
static expression_t *parse_float_const(void)
{
	expression_t *cnst         = allocate_expression_zero(EXPR_CONST);
	cnst->base.type            = token.datatype;
	cnst->conste.v.float_value = token.v.floatvalue;

	next_token();

	return cnst;
}

static entity_t *create_implicit_function(symbol_t *symbol,
		const source_position_t *source_position)
{
	type_t *ntype                          = allocate_type_zero(TYPE_FUNCTION);
	ntype->function.return_type            = type_int;
	ntype->function.unspecified_parameters = true;

	type_t *type = typehash_insert(ntype);
	if (type != ntype) {
		free_type(ntype);
	}

	entity_t *entity = allocate_entity_zero(ENTITY_FUNCTION);
	entity->declaration.storage_class          = STORAGE_CLASS_EXTERN;
	entity->declaration.declared_storage_class = STORAGE_CLASS_EXTERN;
	entity->declaration.type                   = type;
	entity->declaration.implicit               = true;
	entity->base.symbol                        = symbol;
	entity->base.source_position               = *source_position;

	bool strict_prototypes_old = warning.strict_prototypes;
	warning.strict_prototypes  = false;
	record_entity(entity, false);
	warning.strict_prototypes = strict_prototypes_old;

	return entity;
}

/**
 * Creates a return_type (func)(argument_type) function type if not
 * already exists.
 */
static type_t *make_function_2_type(type_t *return_type, type_t *argument_type1,
                                    type_t *argument_type2)
{
	function_parameter_t *parameter2
		= obstack_alloc(type_obst, sizeof(parameter2[0]));
	memset(parameter2, 0, sizeof(parameter2[0]));
	parameter2->type = argument_type2;

	function_parameter_t *parameter1
		= obstack_alloc(type_obst, sizeof(parameter1[0]));
	memset(parameter1, 0, sizeof(parameter1[0]));
	parameter1->type = argument_type1;
	parameter1->next = parameter2;

	type_t *type               = allocate_type_zero(TYPE_FUNCTION);
	type->function.return_type = return_type;
	type->function.parameters  = parameter1;

	type_t *result = typehash_insert(type);
	if (result != type) {
		free_type(type);
	}

	return result;
}

/**
 * Creates a return_type (func)(argument_type) function type if not
 * already exists.
 *
 * @param return_type    the return type
 * @param argument_type  the argument type
 */
static type_t *make_function_1_type(type_t *return_type, type_t *argument_type)
{
	function_parameter_t *parameter
		= obstack_alloc(type_obst, sizeof(parameter[0]));
	memset(parameter, 0, sizeof(parameter[0]));
	parameter->type = argument_type;

	type_t *type               = allocate_type_zero(TYPE_FUNCTION);
	type->function.return_type = return_type;
	type->function.parameters  = parameter;

	type_t *result = typehash_insert(type);
	if (result != type) {
		free_type(type);
	}

	return result;
}

static type_t *make_function_0_type(type_t *return_type)
{
	type_t *type               = allocate_type_zero(TYPE_FUNCTION);
	type->function.return_type = return_type;
	type->function.parameters  = NULL;

	type_t *result = typehash_insert(type);
	if (result != type) {
		free_type(type);
	}

	return result;
}

/**
 * Creates a function type for some function like builtins.
 *
 * @param symbol   the symbol describing the builtin
 */
static type_t *get_builtin_symbol_type(symbol_t *symbol)
{
	switch (symbol->ID) {
	case T___builtin_alloca:
		return make_function_1_type(type_void_ptr, type_size_t);
	case T___builtin_huge_val:
		return make_function_0_type(type_double);
	case T___builtin_inf:
		return make_function_0_type(type_double);
	case T___builtin_inff:
		return make_function_0_type(type_float);
	case T___builtin_infl:
		return make_function_0_type(type_long_double);
	case T___builtin_nan:
		return make_function_1_type(type_double, type_char_ptr);
	case T___builtin_nanf:
		return make_function_1_type(type_float, type_char_ptr);
	case T___builtin_nanl:
		return make_function_1_type(type_long_double, type_char_ptr);
	case T___builtin_va_end:
		return make_function_1_type(type_void, type_valist);
	case T___builtin_expect:
		return make_function_2_type(type_long, type_long, type_long);
	default:
		internal_errorf(HERE, "not implemented builtin identifier found");
	}
}

/**
 * Performs automatic type cast as described in § 6.3.2.1.
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
 * to function-pointer types as defined § 6.3.2.1
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
			return get_qualified_type(type,
			                          expression->base.type->base.qualifiers);
		}

		case EXPR_UNARY_DEREFERENCE: {
			const expression_t *const value = expression->unary.value;
			type_t             *const type  = skip_typeref(value->base.type);
			assert(is_type_pointer(type));
			return type->pointer.points_to;
		}

		case EXPR_BUILTIN_SYMBOL:
			return get_builtin_symbol_type(expression->builtin_symbol.symbol);

		case EXPR_ARRAY_ACCESS: {
			const expression_t *array_ref = expression->array_access.array_ref;
			type_t             *type_left = skip_typeref(array_ref->base.type);
			if (!is_type_valid(type_left))
				return type_left;
			assert(is_type_pointer(type_left));
			return type_left->pointer.points_to;
		}

		case EXPR_STRING_LITERAL: {
			size_t size = expression->string.value.size;
			return make_array_type(type_char, size, TYPE_QUALIFIER_NONE);
		}

		case EXPR_WIDE_STRING_LITERAL: {
			size_t size = expression->wide_string.value.size;
			return make_array_type(type_wchar_t, size, TYPE_QUALIFIER_NONE);
		}

		case EXPR_COMPOUND_LITERAL:
			return expression->compound_literal.type;

		default: break;
	}

	return expression->base.type;
}

static expression_t *parse_reference(void)
{
	symbol_t *const symbol = token.v.symbol;

	entity_t *entity = get_entity(symbol, NAMESPACE_NORMAL);

	if (entity == NULL) {
		if (!strict_mode && look_ahead(1)->type == '(') {
			/* an implicitly declared function */
			if (warning.error_implicit_function_declaration) {
				errorf(HERE, "implicit declaration of function '%Y'", symbol);
			} else if (warning.implicit_function_declaration) {
				warningf(HERE, "implicit declaration of function '%Y'", symbol);
			}

			entity = create_implicit_function(symbol, HERE);
		} else {
			errorf(HERE, "unknown identifier '%Y' found.", symbol);
			entity = create_error_entity(symbol, ENTITY_VARIABLE);
		}
	}

	type_t *orig_type;

	if (is_declaration(entity)) {
		orig_type = entity->declaration.type;
	} else if (entity->kind == ENTITY_ENUM_VALUE) {
		orig_type = entity->enum_value.enum_type;
	} else if (entity->kind == ENTITY_TYPEDEF) {
		errorf(HERE, "encountered typedef name '%Y' while parsing expression",
			symbol);
		next_token();
		return create_invalid_expression();
	} else {
		panic("expected declaration or enum value in reference");
	}

	/* we always do the auto-type conversions; the & and sizeof parser contains
	 * code to revert this! */
	type_t *type = automatic_type_conversion(orig_type);

	expression_kind_t kind = EXPR_REFERENCE;
	if (entity->kind == ENTITY_ENUM_VALUE)
		kind = EXPR_REFERENCE_ENUM_VALUE;

	expression_t *expression     = allocate_expression_zero(kind);
	expression->reference.entity = entity;
	expression->base.type        = type;

	/* this declaration is used */
	if (is_declaration(entity)) {
		entity->declaration.used = true;
	}

	if (entity->base.parent_scope != file_scope
		&& entity->base.parent_scope->depth < current_function->parameters.depth
		&& is_type_valid(orig_type) && !is_type_function(orig_type)) {
		if (entity->kind == ENTITY_VARIABLE) {
			/* access of a variable from an outer function */
			entity->variable.address_taken = true;
		} else if (entity->kind == ENTITY_PARAMETER) {
			entity->parameter.address_taken = true;
		}
		current_function->need_closure = true;
	}

	/* check for deprecated functions */
	if (warning.deprecated_declarations
		&& is_declaration(entity)
		&& entity->declaration.modifiers & DM_DEPRECATED) {
		declaration_t *declaration = &entity->declaration;

		char const *const prefix = entity->kind == ENTITY_FUNCTION ?
			"function" : "variable";

		if (declaration->deprecated_string != NULL) {
			warningf(HERE, "%s '%Y' is deprecated (declared %P): \"%s\"",
			         prefix, entity->base.symbol, &entity->base.source_position,
			         declaration->deprecated_string);
		} else {
			warningf(HERE, "%s '%Y' is deprecated (declared %P)", prefix,
			         entity->base.symbol, &entity->base.source_position);
		}
	}

	if (warning.init_self && entity == current_init_decl && !in_type_prop
	    && entity->kind == ENTITY_VARIABLE) {
		current_init_decl = NULL;
		warningf(HERE, "variable '%#T' is initialized by itself",
		         entity->declaration.type, entity->base.symbol);
	}

	next_token();
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
	if (dst_type == type_void)
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

	if (warning.cast_qual &&
	    is_type_pointer(src_type) &&
	    is_type_pointer(dst_type)) {
		type_t *src = skip_typeref(src_type->pointer.points_to);
		type_t *dst = skip_typeref(dst_type->pointer.points_to);
		unsigned missing_qualifiers =
			src->base.qualifiers & ~dst->base.qualifiers;
		if (missing_qualifiers != 0) {
			warningf(pos,
			         "cast discards qualifiers '%Q' in pointer target type of '%T'",
			         missing_qualifiers, orig_type_right);
		}
	}
	return true;
}

static expression_t *parse_compound_literal(type_t *type)
{
	expression_t *expression = allocate_expression_zero(EXPR_COMPOUND_LITERAL);

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
	add_anchor_token(')');

	source_position_t source_position = token.source_position;

	type_t *type = parse_typename();

	rem_anchor_token(')');
	expect(')');

	if (token.type == '{') {
		return parse_compound_literal(type);
	}

	expression_t *cast = allocate_expression_zero(EXPR_UNARY_CAST);
	cast->base.source_position = source_position;

	expression_t *value = parse_sub_expression(PREC_CAST);
	cast->base.type   = type;
	cast->unary.value = value;

	if (! semantic_cast(cast)) {
		/* TODO: record the error in the AST. else it is impossible to detect it */
	}

	return cast;
end_error:
	return create_invalid_expression();
}

/**
 * Parse a statement expression.
 */
static expression_t *parse_statement_expression(void)
{
	add_anchor_token(')');

	expression_t *expression = allocate_expression_zero(EXPR_STATEMENT);

	statement_t *statement          = parse_compound_statement(true);
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
	} else if (warning.other) {
		warningf(&expression->base.source_position, "empty statement expression ({})");
	}
	expression->base.type = type;

	rem_anchor_token(')');
	expect(')');

end_error:
	return expression;
}

/**
 * Parse a parenthesized expression.
 */
static expression_t *parse_parenthesized_expression(void)
{
	eat('(');

	switch (token.type) {
	case '{':
		/* gcc extension: a statement expression */
		return parse_statement_expression();

	TYPE_QUALIFIERS
	TYPE_SPECIFIERS
		return parse_cast();
	case T_IDENTIFIER:
		if (is_typedef_symbol(token.v.symbol)) {
			return parse_cast();
		}
	}

	add_anchor_token(')');
	expression_t *result = parse_expression();
	rem_anchor_token(')');
	expect(')');

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

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("while parsing member designator",
		                     T_IDENTIFIER, NULL);
		return NULL;
	}
	result->symbol = token.v.symbol;
	next_token();

	designator_t *last_designator = result;
	while (true) {
		if (token.type == '.') {
			next_token();
			if (token.type != T_IDENTIFIER) {
				parse_error_expected("while parsing member designator",
				                     T_IDENTIFIER, NULL);
				return NULL;
			}
			designator_t *designator    = allocate_ast_zero(sizeof(result[0]));
			designator->source_position = *HERE;
			designator->symbol          = token.v.symbol;
			next_token();

			last_designator->next = designator;
			last_designator       = designator;
			continue;
		}
		if (token.type == '[') {
			next_token();
			add_anchor_token(']');
			designator_t *designator    = allocate_ast_zero(sizeof(result[0]));
			designator->source_position = *HERE;
			designator->array_index     = parse_expression();
			rem_anchor_token(']');
			expect(']');
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

	expect('(');
	add_anchor_token(',');
	type_t *type = parse_typename();
	rem_anchor_token(',');
	expect(',');
	add_anchor_token(')');
	designator_t *designator = parse_designator();
	rem_anchor_token(')');
	expect(')');

	expression->offsetofe.type       = type;
	expression->offsetofe.designator = designator;

	type_path_t path;
	memset(&path, 0, sizeof(path));
	path.top_type = type;
	path.path     = NEW_ARR_F(type_path_entry_t, 0);

	descend_into_subtype(&path);

	if (!walk_designator(&path, designator, true)) {
		return create_invalid_expression();
	}

	DEL_ARR_F(path.path);

	return expression;
end_error:
	return create_invalid_expression();
}

/**
 * Parses a _builtin_va_start() expression.
 */
static expression_t *parse_va_start(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_VA_START);

	eat(T___builtin_va_start);

	expect('(');
	add_anchor_token(',');
	expression->va_starte.ap = parse_assignment_expression();
	rem_anchor_token(',');
	expect(',');
	expression_t *const expr = parse_assignment_expression();
	if (expr->kind == EXPR_REFERENCE) {
		entity_t *const entity = expr->reference.entity;
		if (entity->base.parent_scope != &current_function->parameters
				|| entity->base.next != NULL
				|| entity->kind != ENTITY_PARAMETER) {
			errorf(&expr->base.source_position,
			       "second argument of 'va_start' must be last parameter of the current function");
		} else {
			expression->va_starte.parameter = &entity->variable;
		}
		expect(')');
		return expression;
	}
	expect(')');
end_error:
	return create_invalid_expression();
}

/**
 * Parses a _builtin_va_arg() expression.
 */
static expression_t *parse_va_arg(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_VA_ARG);

	eat(T___builtin_va_arg);

	expect('(');
	expression->va_arge.ap = parse_assignment_expression();
	expect(',');
	expression->base.type = parse_typename();
	expect(')');

	return expression;
end_error:
	return create_invalid_expression();
}

static expression_t *parse_builtin_symbol(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_BUILTIN_SYMBOL);

	symbol_t *symbol = token.v.symbol;

	expression->builtin_symbol.symbol = symbol;
	next_token();

	type_t *type = get_builtin_symbol_type(symbol);
	type = automatic_type_conversion(type);

	expression->base.type = type;
	return expression;
}

/**
 * Parses a __builtin_constant() expression.
 */
static expression_t *parse_builtin_constant(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_BUILTIN_CONSTANT_P);

	eat(T___builtin_constant_p);

	expect('(');
	add_anchor_token(')');
	expression->builtin_constant.value = parse_assignment_expression();
	rem_anchor_token(')');
	expect(')');
	expression->base.type = type_int;

	return expression;
end_error:
	return create_invalid_expression();
}

/**
 * Parses a __builtin_prefetch() expression.
 */
static expression_t *parse_builtin_prefetch(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_BUILTIN_PREFETCH);

	eat(T___builtin_prefetch);

	expect('(');
	add_anchor_token(')');
	expression->builtin_prefetch.adr = parse_assignment_expression();
	if (token.type == ',') {
		next_token();
		expression->builtin_prefetch.rw = parse_assignment_expression();
	}
	if (token.type == ',') {
		next_token();
		expression->builtin_prefetch.locality = parse_assignment_expression();
	}
	rem_anchor_token(')');
	expect(')');
	expression->base.type = type_void;

	return expression;
end_error:
	return create_invalid_expression();
}

/**
 * Parses a __builtin_is_*() compare expression.
 */
static expression_t *parse_compare_builtin(void)
{
	expression_t *expression;

	switch (token.type) {
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

	expect('(');
	expression->binary.left = parse_assignment_expression();
	expect(',');
	expression->binary.right = parse_assignment_expression();
	expect(')');

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
	return create_invalid_expression();
}

#if 0
/**
 * Parses a __builtin_expect() expression.
 */
static expression_t *parse_builtin_expect(void)
{
	expression_t *expression
		= allocate_expression_zero(EXPR_BINARY_BUILTIN_EXPECT);

	eat(T___builtin_expect);

	expect('(');
	expression->binary.left = parse_assignment_expression();
	expect(',');
	expression->binary.right = parse_constant_expression();
	expect(')');

	expression->base.type = expression->binary.left->base.type;

	return expression;
end_error:
	return create_invalid_expression();
}
#endif

/**
 * Parses a MS assume() expression.
 */
static expression_t *parse_assume(void)
{
	expression_t *expression = allocate_expression_zero(EXPR_UNARY_ASSUME);

	eat(T__assume);

	expect('(');
	add_anchor_token(')');
	expression->unary.value = parse_assignment_expression();
	rem_anchor_token(')');
	expect(')');

	expression->base.type = type_void;
	return expression;
end_error:
	return create_invalid_expression();
}

/**
 * Return the declaration for a given label symbol or create a new one.
 *
 * @param symbol  the symbol of the label
 */
static label_t *get_label(symbol_t *symbol)
{
	entity_t *label;
	assert(current_function != NULL);

	label = get_entity(symbol, NAMESPACE_LABEL);
	/* if we found a local label, we already created the declaration */
	if (label != NULL && label->kind == ENTITY_LOCAL_LABEL) {
		if (label->base.parent_scope != current_scope) {
			assert(label->base.parent_scope->depth < current_scope->depth);
			current_function->goto_to_outer = true;
		}
		return &label->label;
	}

	label = get_entity(symbol, NAMESPACE_LABEL);
	/* if we found a label in the same function, then we already created the
	 * declaration */
	if (label != NULL
			&& label->base.parent_scope == &current_function->parameters) {
		return &label->label;
	}

	/* otherwise we need to create a new one */
	label               = allocate_entity_zero(ENTITY_LABEL);
	label->base.namespc = NAMESPACE_LABEL;
	label->base.symbol  = symbol;

	label_push(label);

	return &label->label;
}

/**
 * Parses a GNU && label address expression.
 */
static expression_t *parse_label_address(void)
{
	source_position_t source_position = token.source_position;
	eat(T_ANDAND);
	if (token.type != T_IDENTIFIER) {
		parse_error_expected("while parsing label address", T_IDENTIFIER, NULL);
		goto end_error;
	}
	symbol_t *symbol = token.v.symbol;
	next_token();

	label_t *label       = get_label(symbol);
	label->used          = true;
	label->address_taken = true;

	expression_t *expression = allocate_expression_zero(EXPR_LABEL_ADDRESS);
	expression->base.source_position = source_position;

	/* label address is threaten as a void pointer */
	expression->base.type           = type_void_ptr;
	expression->label_address.label = label;
	return expression;
end_error:
	return create_invalid_expression();
}

/**
 * Parse a microsoft __noop expression.
 */
static expression_t *parse_noop_expression(void)
{
	/* the result is a (int)0 */
	expression_t *cnst         = allocate_expression_zero(EXPR_CONST);
	cnst->base.type            = type_int;
	cnst->conste.v.int_value   = 0;
	cnst->conste.is_ms_noop    = true;

	eat(T___noop);

	if (token.type == '(') {
		/* parse arguments */
		eat('(');
		add_anchor_token(')');
		add_anchor_token(',');

		if (token.type != ')') {
			while (true) {
				(void)parse_assignment_expression();
				if (token.type != ',')
					break;
				next_token();
			}
		}
	}
	rem_anchor_token(',');
	rem_anchor_token(')');
	expect(')');

end_error:
	return cnst;
}

/**
 * Parses a primary expression.
 */
static expression_t *parse_primary_expression(void)
{
	switch (token.type) {
		case T_false:                    return parse_bool_const(false);
		case T_true:                     return parse_bool_const(true);
		case T_INTEGER:                  return parse_int_const();
		case T_CHARACTER_CONSTANT:       return parse_character_constant();
		case T_WIDE_CHARACTER_CONSTANT:  return parse_wide_character_constant();
		case T_FLOATINGPOINT:            return parse_float_const();
		case T_STRING_LITERAL:
		case T_WIDE_STRING_LITERAL:      return parse_string_const();
		case T_IDENTIFIER:               return parse_reference();
		case T___FUNCTION__:
		case T___func__:                 return parse_function_keyword();
		case T___PRETTY_FUNCTION__:      return parse_pretty_function_keyword();
		case T___FUNCSIG__:              return parse_funcsig_keyword();
		case T___FUNCDNAME__:            return parse_funcdname_keyword();
		case T___builtin_offsetof:       return parse_offsetof();
		case T___builtin_va_start:       return parse_va_start();
		case T___builtin_va_arg:         return parse_va_arg();
		case T___builtin_expect:
		case T___builtin_alloca:
		case T___builtin_inf:
		case T___builtin_inff:
		case T___builtin_infl:
		case T___builtin_nan:
		case T___builtin_nanf:
		case T___builtin_nanl:
		case T___builtin_huge_val:
		case T___builtin_va_end:         return parse_builtin_symbol();
		case T___builtin_isgreater:
		case T___builtin_isgreaterequal:
		case T___builtin_isless:
		case T___builtin_islessequal:
		case T___builtin_islessgreater:
		case T___builtin_isunordered:    return parse_compare_builtin();
		case T___builtin_constant_p:     return parse_builtin_constant();
		case T___builtin_prefetch:       return parse_builtin_prefetch();
		case T__assume:                  return parse_assume();
		case T_ANDAND:
			if (GNU_MODE)
				return parse_label_address();
			break;

		case '(':                        return parse_parenthesized_expression();
		case T___noop:                   return parse_noop_expression();
	}

	errorf(HERE, "unexpected token %K, expected an expression", &token);
	return create_invalid_expression();
}

/**
 * Check if the expression has the character type and issue a warning then.
 */
static void check_for_char_index_type(const expression_t *expression)
{
	type_t       *const type      = expression->base.type;
	const type_t *const base_type = skip_typeref(type);

	if (is_type_atomic(base_type, ATOMIC_TYPE_CHAR) &&
			warning.char_subscripts) {
		warningf(&expression->base.source_position,
		         "array subscript has type '%T'", type);
	}
}

static expression_t *parse_array_expression(expression_t *left)
{
	expression_t *expression = allocate_expression_zero(EXPR_ARRAY_ACCESS);

	eat('[');
	add_anchor_token(']');

	expression_t *inside = parse_expression();

	type_t *const orig_type_left   = left->base.type;
	type_t *const orig_type_inside = inside->base.type;

	type_t *const type_left   = skip_typeref(orig_type_left);
	type_t *const type_inside = skip_typeref(orig_type_inside);

	type_t                    *return_type;
	array_access_expression_t *array_access = &expression->array_access;
	if (is_type_pointer(type_left)) {
		return_type             = type_left->pointer.points_to;
		array_access->array_ref = left;
		array_access->index     = inside;
		check_for_char_index_type(inside);
	} else if (is_type_pointer(type_inside)) {
		return_type             = type_inside->pointer.points_to;
		array_access->array_ref = inside;
		array_access->index     = left;
		array_access->flipped   = true;
		check_for_char_index_type(left);
	} else {
		if (is_type_valid(type_left) && is_type_valid(type_inside)) {
			errorf(HERE,
				"array access on object with non-pointer types '%T', '%T'",
				orig_type_left, orig_type_inside);
		}
		return_type             = type_error_type;
		array_access->array_ref = left;
		array_access->index     = inside;
	}

	expression->base.type = automatic_type_conversion(return_type);

	rem_anchor_token(']');
	expect(']');
end_error:
	return expression;
}

static expression_t *parse_typeprop(expression_kind_t const kind)
{
	expression_t  *tp_expression = allocate_expression_zero(kind);
	tp_expression->base.type     = type_size_t;

	eat(kind == EXPR_SIZEOF ? T_sizeof : T___alignof__);

	/* we only refer to a type property, mark this case */
	bool old     = in_type_prop;
	in_type_prop = true;

	type_t       *orig_type;
	expression_t *expression;
	if (token.type == '(' && is_declaration_specifier(look_ahead(1), true)) {
		next_token();
		add_anchor_token(')');
		orig_type = parse_typename();
		rem_anchor_token(')');
		expect(')');

		if (token.type == '{') {
			/* It was not sizeof(type) after all.  It is sizeof of an expression
			 * starting with a compound literal */
			expression = parse_compound_literal(orig_type);
			goto typeprop_expression;
		}
	} else {
		expression = parse_sub_expression(PREC_UNARY);

typeprop_expression:
		tp_expression->typeprop.tp_expression = expression;

		orig_type = revert_automatic_type_conversion(expression);
		expression->base.type = orig_type;
	}

	tp_expression->typeprop.type   = orig_type;
	type_t const* const type       = skip_typeref(orig_type);
	char   const* const wrong_type =
		is_type_incomplete(type)    ? "incomplete"          :
		type->kind == TYPE_FUNCTION ? "function designator" :
		type->kind == TYPE_BITFIELD ? "bitfield"            :
		NULL;
	if (wrong_type != NULL) {
		char const* const what = kind == EXPR_SIZEOF ? "sizeof" : "alignof";
		errorf(&tp_expression->base.source_position,
				"operand of %s expression must not be of %s type '%T'",
				what, wrong_type, orig_type);
	}

end_error:
	in_type_prop = old;
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

static expression_t *parse_select_expression(expression_t *compound)
{
	expression_t *select    = allocate_expression_zero(EXPR_SELECT);
	select->select.compound = compound;

	assert(token.type == '.' || token.type == T_MINUSGREATER);
	bool is_pointer = (token.type == T_MINUSGREATER);
	next_token();

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("while parsing select", T_IDENTIFIER, NULL);
		return select;
	}
	symbol_t *symbol = token.v.symbol;
	next_token();

	type_t *const orig_type = compound->base.type;
	type_t *const type      = skip_typeref(orig_type);

	type_t *type_left;
	bool    saw_error = false;
	if (is_type_pointer(type)) {
		if (!is_pointer) {
			errorf(HERE,
			       "request for member '%Y' in something not a struct or union, but '%T'",
			       symbol, orig_type);
			saw_error = true;
		}
		type_left = skip_typeref(type->pointer.points_to);
	} else {
		if (is_pointer && is_type_valid(type)) {
			errorf(HERE, "left hand side of '->' is not a pointer, but '%T'", orig_type);
			saw_error = true;
		}
		type_left = type;
	}

	entity_t *entry;
	if (type_left->kind == TYPE_COMPOUND_STRUCT ||
	    type_left->kind == TYPE_COMPOUND_UNION) {
		compound_t *compound = type_left->compound.compound;

		if (!compound->complete) {
			errorf(HERE, "request for member '%Y' of incomplete type '%T'",
			       symbol, type_left);
			goto create_error_entry;
		}

		entry = find_compound_entry(compound, symbol);
		if (entry == NULL) {
			errorf(HERE, "'%T' has no member named '%Y'", orig_type, symbol);
			goto create_error_entry;
		}
	} else {
		if (is_type_valid(type_left) && !saw_error) {
			errorf(HERE,
			       "request for member '%Y' in something not a struct or union, but '%T'",
			       symbol, type_left);
		}
create_error_entry:
		entry = create_error_entity(symbol, ENTITY_COMPOUND_MEMBER);
	}

	assert(is_declaration(entry));
	select->select.compound_entry = entry;

	type_t *entry_type = entry->declaration.type;
	type_t *res_type
		= get_qualified_type(entry_type, type_left->base.qualifiers);

	/* we always do the auto-type conversions; the & and sizeof parser contains
	 * code to revert this! */
	select->base.type = automatic_type_conversion(res_type);

	type_t *skipped = skip_typeref(res_type);
	if (skipped->kind == TYPE_BITFIELD) {
		select->base.type = skipped->bitfield.base_type;
	}

	return select;
}

static void check_call_argument(const function_parameter_t *parameter,
                                call_argument_t *argument, unsigned pos)
{
	type_t         *expected_type      = parameter->type;
	type_t         *expected_type_skip = skip_typeref(expected_type);
	assign_error_t  error              = ASSIGN_ERROR_INCOMPATIBLE;
	expression_t   *arg_expr           = argument->expression;
	type_t         *arg_type           = skip_typeref(arg_expr->base.type);

	/* handle transparent union gnu extension */
	if (is_type_union(expected_type_skip)
			&& (expected_type_skip->base.modifiers
				& TYPE_MODIFIER_TRANSPARENT_UNION)) {
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
	argument->expression = create_implicit_cast(argument->expression,
	                                            expected_type);

	if (error != ASSIGN_SUCCESS) {
		/* report exact scope in error messages (like "in argument 3") */
		char buf[64];
		snprintf(buf, sizeof(buf), "call argument %u", pos);
		report_assign_error(error, expected_type, arg_expr,	buf,
							&arg_expr->base.source_position);
	} else if (warning.traditional || warning.conversion) {
		type_t *const promoted_type = get_default_promoted_type(arg_type);
		if (!types_compatible(expected_type_skip, promoted_type) &&
		    !types_compatible(expected_type_skip, type_void_ptr) &&
		    !types_compatible(type_void_ptr,      promoted_type)) {
			/* Deliberately show the skipped types in this warning */
			warningf(&arg_expr->base.source_position,
				"passing call argument %u as '%T' rather than '%T' due to prototype",
				pos, expected_type_skip, promoted_type);
		}
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
		errorf(HERE, "called object '%E' (type '%T') is not a pointer to a function", expression, orig_type);
	}

	/* parse arguments */
	eat('(');
	add_anchor_token(')');
	add_anchor_token(',');

	if (token.type != ')') {
		call_argument_t *last_argument = NULL;

		while (true) {
			call_argument_t *argument = allocate_ast_zero(sizeof(argument[0]));

			argument->expression = parse_assignment_expression();
			if (last_argument == NULL) {
				call->arguments = argument;
			} else {
				last_argument->next = argument;
			}
			last_argument = argument;

			if (token.type != ',')
				break;
			next_token();
		}
	}
	rem_anchor_token(',');
	rem_anchor_token(')');
	expect(')');

	if (function_type == NULL)
		return result;

	function_parameter_t *parameter = function_type->parameters;
	call_argument_t      *argument  = call->arguments;
	if (!function_type->unspecified_parameters) {
		for (unsigned pos = 0; parameter != NULL && argument != NULL;
				parameter = parameter->next, argument = argument->next) {
			check_call_argument(parameter, argument, ++pos);
		}

		if (parameter != NULL) {
			errorf(HERE, "too few arguments to function '%E'", expression);
		} else if (argument != NULL && !function_type->variadic) {
			errorf(HERE, "too many arguments to function '%E'", expression);
		}
	}

	/* do default promotion */
	for (; argument != NULL; argument = argument->next) {
		type_t *type = argument->expression->base.type;

		type = get_default_promoted_type(type);

		argument->expression
			= create_implicit_cast(argument->expression, type);
	}

	check_format(&result->call);

	if (warning.aggregate_return &&
	    is_type_compound(skip_typeref(function_type->return_type))) {
		warningf(&result->base.source_position,
		         "function call has aggregate value");
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
	if (!warning.address)
		return;

	expr = get_reference_address(expr);
	if (expr != NULL) {
		warningf(&expr->base.source_position,
		         "the address of '%Y' will always evaluate as 'true'",
		         expr->reference.entity->base.symbol);
	}
}

static void semantic_condition(expression_t const *const expr,
                               char const *const context)
{
	type_t *const type = skip_typeref(expr->base.type);
	if (is_type_scalar(type)) {
		warn_reference_address_as_bool(expr);
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
	if (GNU_MODE && token.type == ':') {
		gnu_cond = true;
	} else {
		true_expression = parse_expression();
	}
	rem_anchor_token(':');
	expect(':');
	expression_t *false_expression =
		parse_sub_expression(c_mode & _CXX ? PREC_ASSIGNMENT : PREC_CONDITIONAL);

	type_t *const orig_true_type  = true_expression->base.type;
	type_t *const orig_false_type = false_expression->base.type;
	type_t *const true_type       = skip_typeref(orig_true_type);
	type_t *const false_type      = skip_typeref(orig_false_type);

	/* 6.5.15.3 */
	type_t *result_type;
	if (is_type_atomic(true_type,  ATOMIC_TYPE_VOID) ||
			is_type_atomic(false_type, ATOMIC_TYPE_VOID)) {
		/* ISO/IEC 14882:1998(E) §5.16:2 */
		if (true_expression->kind == EXPR_UNARY_THROW) {
			result_type = false_type;
		} else if (false_expression->kind == EXPR_UNARY_THROW) {
			result_type = true_type;
		} else {
			if (warning.other && (
						!is_type_atomic(true_type,  ATOMIC_TYPE_VOID) ||
						!is_type_atomic(false_type, ATOMIC_TYPE_VOID)
					)) {
				warningf(&conditional->base.source_position,
						"ISO C forbids conditional expression with only one void side");
			}
			result_type = type_void;
		}
	} else if (is_type_arithmetic(true_type)
	           && is_type_arithmetic(false_type)) {
		result_type = semantic_arithmetic(true_type, false_type);

		true_expression  = create_implicit_cast(true_expression, result_type);
		false_expression = create_implicit_cast(false_expression, result_type);

		conditional->true_expression  = true_expression;
		conditional->false_expression = false_expression;
		conditional->base.type        = result_type;
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
			if (is_type_atomic(to1, ATOMIC_TYPE_VOID) ||
			    is_type_atomic(to2, ATOMIC_TYPE_VOID)) {
				to = type_void;
			} else if (types_compatible(get_unqualified_type(to1),
			                            get_unqualified_type(to2))) {
				to = to1;
			} else {
				if (warning.other) {
					warningf(&conditional->base.source_position,
							"pointer types '%T' and '%T' in conditional expression are incompatible",
							true_type, false_type);
				}
				to = type_void;
			}

			type_t *const type =
				get_qualified_type(to, to1->base.qualifiers | to2->base.qualifiers);
			result_type = make_pointer_type(type, TYPE_QUALIFIER_NONE);
		} else if (is_type_integer(other_type)) {
			if (warning.other) {
				warningf(&conditional->base.source_position,
						"pointer/integer type mismatch in conditional expression ('%T' and '%T')", true_type, false_type);
			}
			result_type = pointer_type;
		} else {
			if (is_type_valid(other_type)) {
			 	type_error_incompatible("while parsing conditional",
						&expression->base.source_position, true_type, false_type);
			}
			result_type = type_error_type;
		}
	} else {
		if (is_type_valid(true_type) && is_type_valid(false_type)) {
			type_error_incompatible("while parsing conditional",
			                        &conditional->base.source_position, true_type,
			                        false_type);
		}
		result_type = type_error_type;
	}

	conditional->true_expression
		= gnu_cond ? NULL : create_implicit_cast(true_expression, result_type);
	conditional->false_expression
		= create_implicit_cast(false_expression, result_type);
	conditional->base.type = result_type;
	return result;
end_error:
	return create_invalid_expression();
}

/**
 * Parse an extension expression.
 */
static expression_t *parse_extension(void)
{
	eat(T___extension__);

	bool old_gcc_extension   = in_gcc_extension;
	in_gcc_extension         = true;
	expression_t *expression = parse_sub_expression(PREC_UNARY);
	in_gcc_extension         = old_gcc_extension;
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

	expect('(');
	add_anchor_token(')');
	expression_t *expression = parse_expression();
	rem_anchor_token(')');
	expect(')');
	result->classify_type.type_expression = expression;

	return result;
end_error:
	return create_invalid_expression();
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

	if (token.type == '[') {
		next_token();
		result->kind = EXPR_UNARY_DELETE_ARRAY;
		expect(']');
end_error:;
	}

	expression_t *const value = parse_sub_expression(PREC_CAST);
	result->unary.value = value;

	type_t *const type = skip_typeref(value->base.type);
	if (!is_type_pointer(type)) {
		errorf(&value->base.source_position,
				"operand of delete must have pointer type");
	} else if (warning.other &&
			is_type_atomic(skip_typeref(type->pointer.points_to), ATOMIC_TYPE_VOID)) {
		warningf(&value->base.source_position,
				"deleting 'void*' is undefined");
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
	switch (token.type) {
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
				if (is_type_incomplete(points_to) &&
						!is_type_atomic(points_to, ATOMIC_TYPE_VOID)) {
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
		if (!GNU_MODE || !is_type_atomic(points_to, ATOMIC_TYPE_VOID)) {
			errorf(source_position,
			       "arithmetic with pointer to incomplete type '%T' not allowed",
			       orig_pointer_type);
			return false;
		} else if (warning.pointer_arith) {
			warningf(source_position,
			         "pointer of type '%T' used in arithmetic",
			         orig_pointer_type);
		}
	} else if (is_type_function(points_to)) {
		if (!GNU_MODE) {
			errorf(source_position,
			       "arithmetic with pointer to function type '%T' not allowed",
			       orig_pointer_type);
			return false;
		} else if (warning.pointer_arith) {
			warningf(source_position,
			         "pointer to a function '%T' used in arithmetic",
			         orig_pointer_type);
		}
	}
	return true;
}

static bool is_lvalue(const expression_t *expression)
{
	/* TODO: doesn't seem to be consistent with §6.3.2.1 (1) */
	switch (expression->kind) {
	case EXPR_REFERENCE:
	case EXPR_ARRAY_ACCESS:
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
	}

	expression->base.type = orig_type;
}

static void semantic_unexpr_plus(unary_expression_t *expression)
{
	semantic_unexpr_arithmetic(expression);
	if (warning.traditional)
		warningf(&expression->base.source_position,
			"traditional C rejects the unary plus operator");
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

	expression->base.type = orig_type;
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
		errorf(&expression->base.source_position,
				"address of register %s '%Y' requested",
				get_entity_kind_name(entity->kind),	entity->base.symbol);
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
	if (type->kind == TYPE_BITFIELD) {
		errorf(&expression->base.source_position,
		       "'&' not allowed on object with bitfield type '%T'",
		       type);
	}

	set_address_taken(value, false);

	expression->base.type = make_pointer_type(orig_type, TYPE_QUALIFIER_NONE);
}

#define CREATE_UNARY_EXPRESSION_PARSER(token_type, unexpression_type, sfunc) \
static expression_t *parse_##unexpression_type(void)                         \
{                                                                            \
	expression_t *unary_expression                                           \
		= allocate_expression_zero(unexpression_type);                       \
	eat(token_type);                                                         \
	unary_expression->unary.value = parse_sub_expression(PREC_UNARY);        \
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

#define CREATE_UNARY_POSTFIX_EXPRESSION_PARSER(token_type, unexpression_type, \
                                               sfunc)                         \
static expression_t *parse_##unexpression_type(expression_t *left)            \
{                                                                             \
	expression_t *unary_expression                                            \
		= allocate_expression_zero(unexpression_type);                        \
	eat(token_type);                                                          \
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

	/* § 6.3.1.8 Usual arithmetic conversions */
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

	bool const signed_left  = is_type_signed(type_left);
	bool const signed_right = is_type_signed(type_right);
	int const  rank_left    = get_rank(type_left);
	int const  rank_right   = get_rank(type_right);

	if (signed_left == signed_right)
		return rank_left >= rank_right ? type_left : type_right;

	int     s_rank;
	int     u_rank;
	type_t *s_type;
	type_t *u_type;
	if (signed_left) {
		s_rank = rank_left;
		s_type = type_left;
		u_rank = rank_right;
		u_type = type_right;
	} else {
		s_rank = rank_right;
		s_type = type_right;
		u_rank = rank_left;
		u_type = type_left;
	}

	if (u_rank >= s_rank)
		return u_type;

	/* casting rank to atomic_type_kind is a bit hacky, but makes things
	 * easier here... */
	if (get_atomic_type_size((atomic_type_kind_t) s_rank)
			> get_atomic_type_size((atomic_type_kind_t) u_rank))
		return s_type;

	switch (s_rank) {
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

static void warn_div_by_zero(binary_expression_t const *const expression)
{
	if (!warning.div_by_zero ||
	    !is_type_integer(expression->base.type))
		return;

	expression_t const *const right = expression->right;
	/* The type of the right operand can be different for /= */
	if (is_type_integer(right->base.type) &&
	    is_constant_expression(right)     &&
	    fold_constant(right) == 0) {
		warningf(&expression->base.source_position, "division by zero");
	}
}

/**
 * Check the semantic restrictions for a div/mod expression.
 */
static void semantic_divmod_arithmetic(binary_expression_t *expression) {
	semantic_binexpr_arithmetic(expression);
	warn_div_by_zero(expression);
}

static void semantic_shift_op(binary_expression_t *expression)
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
		return;
	}

	type_left  = promote_integer(type_left);
	type_right = promote_integer(type_right);

	expression->left      = create_implicit_cast(left, type_left);
	expression->right     = create_implicit_cast(right, type_right);
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

	/* § 6.5.6 */
	if (is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
		expression->left  = create_implicit_cast(left, arithmetic_type);
		expression->right = create_implicit_cast(right, arithmetic_type);
		expression->base.type = arithmetic_type;
		return;
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

	/* § 5.6.5 */
	if (is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
		expression->left        = create_implicit_cast(left, arithmetic_type);
		expression->right       = create_implicit_cast(right, arithmetic_type);
		expression->base.type =  arithmetic_type;
		return;
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
			if (!is_type_atomic(unqual_left, ATOMIC_TYPE_VOID)) {
				errorf(pos, "subtracting pointers to non-object types '%T'",
				       orig_type_left);
			} else if (warning.other) {
				warningf(pos, "subtracting pointers to void");
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

	if (expr->kind == EXPR_STRING_LITERAL ||
	    expr->kind == EXPR_WIDE_STRING_LITERAL) {
		warningf(&expr->base.source_position,
			"comparison with string literal results in unspecified behaviour");
	}
}

/**
 * Check the semantics of comparison expressions.
 *
 * @param expression   The expression to check.
 */
static void semantic_comparison(binary_expression_t *expression)
{
	expression_t *left  = expression->left;
	expression_t *right = expression->right;

	if (warning.address) {
		warn_string_literal_address(left);
		warn_string_literal_address(right);

		expression_t const* const func_left = get_reference_address(left);
		if (func_left != NULL && is_null_pointer_constant(right)) {
			warningf(&expression->base.source_position,
			         "the address of '%Y' will never be NULL",
			         func_left->reference.entity->base.symbol);
		}

		expression_t const* const func_right = get_reference_address(right);
		if (func_right != NULL && is_null_pointer_constant(right)) {
			warningf(&expression->base.source_position,
			         "the address of '%Y' will never be NULL",
			         func_right->reference.entity->base.symbol);
		}
	}

	type_t *orig_type_left  = left->base.type;
	type_t *orig_type_right = right->base.type;
	type_t *type_left       = skip_typeref(orig_type_left);
	type_t *type_right      = skip_typeref(orig_type_right);

	/* TODO non-arithmetic types */
	if (is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		/* test for signed vs unsigned compares */
		if (warning.sign_compare &&
		    (expression->base.kind != EXPR_BINARY_EQUAL &&
		     expression->base.kind != EXPR_BINARY_NOTEQUAL) &&
		    (is_type_signed(type_left) != is_type_signed(type_right))) {

			/* check if 1 of the operands is a constant, in this case we just
			 * check wether we can safely represent the resulting constant in
			 * the type of the other operand. */
			expression_t *const_expr = NULL;
			expression_t *other_expr = NULL;

			if (is_constant_expression(left)) {
				const_expr = left;
				other_expr = right;
			} else if (is_constant_expression(right)) {
				const_expr = right;
				other_expr = left;
			}

			if (const_expr != NULL) {
				type_t *other_type = skip_typeref(other_expr->base.type);
				long    val        = fold_constant(const_expr);
				/* TODO: check if val can be represented by other_type */
				(void) other_type;
				(void) val;
			}
			warningf(&expression->base.source_position,
			         "comparison between signed and unsigned");
		}
		type_t *arithmetic_type = semantic_arithmetic(type_left, type_right);
		expression->left        = create_implicit_cast(left, arithmetic_type);
		expression->right       = create_implicit_cast(right, arithmetic_type);
		expression->base.type   = arithmetic_type;
		if (warning.float_equal &&
		    (expression->base.kind == EXPR_BINARY_EQUAL ||
		     expression->base.kind == EXPR_BINARY_NOTEQUAL) &&
		    is_type_float(arithmetic_type)) {
			warningf(&expression->base.source_position,
			         "comparing floating point with == or != is unsafe");
		}
	} else if (is_type_pointer(type_left) && is_type_pointer(type_right)) {
		/* TODO check compatibility */
	} else if (is_type_pointer(type_left)) {
		expression->right = create_implicit_cast(right, type_left);
	} else if (is_type_pointer(type_right)) {
		expression->left = create_implicit_cast(left, type_right);
	} else if (is_type_valid(type_left) && is_type_valid(type_right)) {
		type_error_incompatible("invalid operands in comparison",
		                        &expression->base.source_position,
		                        type_left, type_right);
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
		errorf(HERE, "left hand side '%E' of assignment is not an lvalue",
		       left);
		return false;
	}

	if (left->kind == EXPR_REFERENCE
			&& left->reference.entity->kind == ENTITY_FUNCTION) {
		errorf(HERE, "cannot assign to function '%E'", left);
		return false;
	}

	if (is_type_array(type_left)) {
		errorf(HERE, "cannot assign to array '%E'", left);
		return false;
	}
	if (type_left->base.qualifiers & TYPE_QUALIFIER_CONST) {
		errorf(HERE, "assignment to readonly location '%E' (type '%T')", left,
		       orig_type_left);
		return false;
	}
	if (is_type_incomplete(type_left)) {
		errorf(HERE, "left-hand side '%E' of assignment has incomplete type '%T'",
		       left, orig_type_left);
		return false;
	}
	if (is_type_compound(type_left) && has_const_fields(&type_left->compound)) {
		errorf(HERE, "cannot assign to '%E' because compound type '%T' has readonly fields",
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

/**
 * Check the semantic restrictions of a logical expression.
 */
static void semantic_logical_op(binary_expression_t *expression)
{
	/* §6.5.13:2  Each of the operands shall have scalar type.
	 * §6.5.14:2  Each of the operands shall have scalar type. */
	semantic_condition(expression->left,   "left operand of logical operator");
	semantic_condition(expression->right, "right operand of logical operator");
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
		case EXPR_UNKNOWN:                   break;
		case EXPR_INVALID:                   return true; /* do NOT warn */
		case EXPR_REFERENCE:                 return false;
		case EXPR_REFERENCE_ENUM_VALUE:      return false;
		/* suppress the warning for microsoft __noop operations */
		case EXPR_CONST:                     return expr->conste.is_ms_noop;
		case EXPR_CHARACTER_CONSTANT:        return false;
		case EXPR_WIDE_CHARACTER_CONSTANT:   return false;
		case EXPR_STRING_LITERAL:            return false;
		case EXPR_WIDE_STRING_LITERAL:       return false;
		case EXPR_LABEL_ADDRESS:             return false;

		case EXPR_CALL: {
			const call_expression_t *const call = &expr->call;
			if (call->function->kind != EXPR_BUILTIN_SYMBOL)
				return true;

			switch (call->function->builtin_symbol.symbol->ID) {
				case T___builtin_va_end: return true;
				default:                 return false;
			}
		}

		/* Generate the warning if either the left or right hand side of a
		 * conditional expression has no effect */
		case EXPR_CONDITIONAL: {
			const conditional_expression_t *const cond = &expr->conditional;
			return
				expression_has_effect(cond->true_expression) &&
				expression_has_effect(cond->false_expression);
		}

		case EXPR_SELECT:                    return false;
		case EXPR_ARRAY_ACCESS:              return false;
		case EXPR_SIZEOF:                    return false;
		case EXPR_CLASSIFY_TYPE:             return false;
		case EXPR_ALIGNOF:                   return false;

		case EXPR_FUNCNAME:                  return false;
		case EXPR_BUILTIN_SYMBOL:            break; /* handled in EXPR_CALL */
		case EXPR_BUILTIN_CONSTANT_P:        return false;
		case EXPR_BUILTIN_PREFETCH:          return true;
		case EXPR_OFFSETOF:                  return false;
		case EXPR_VA_START:                  return true;
		case EXPR_VA_ARG:                    return true;
		case EXPR_STATEMENT:                 return true; // TODO
		case EXPR_COMPOUND_LITERAL:          return false;

		case EXPR_UNARY_NEGATE:              return false;
		case EXPR_UNARY_PLUS:                return false;
		case EXPR_UNARY_BITWISE_NEGATE:      return false;
		case EXPR_UNARY_NOT:                 return false;
		case EXPR_UNARY_DEREFERENCE:         return false;
		case EXPR_UNARY_TAKE_ADDRESS:        return false;
		case EXPR_UNARY_POSTFIX_INCREMENT:   return true;
		case EXPR_UNARY_POSTFIX_DECREMENT:   return true;
		case EXPR_UNARY_PREFIX_INCREMENT:    return true;
		case EXPR_UNARY_PREFIX_DECREMENT:    return true;

		/* Treat void casts as if they have an effect in order to being able to
		 * suppress the warning */
		case EXPR_UNARY_CAST: {
			type_t *const type = skip_typeref(expr->base.type);
			return is_type_atomic(type, ATOMIC_TYPE_VOID);
		}

		case EXPR_UNARY_CAST_IMPLICIT:       return true;
		case EXPR_UNARY_ASSUME:              return true;
		case EXPR_UNARY_DELETE:              return true;
		case EXPR_UNARY_DELETE_ARRAY:        return true;
		case EXPR_UNARY_THROW:               return true;

		case EXPR_BINARY_ADD:                return false;
		case EXPR_BINARY_SUB:                return false;
		case EXPR_BINARY_MUL:                return false;
		case EXPR_BINARY_DIV:                return false;
		case EXPR_BINARY_MOD:                return false;
		case EXPR_BINARY_EQUAL:              return false;
		case EXPR_BINARY_NOTEQUAL:           return false;
		case EXPR_BINARY_LESS:               return false;
		case EXPR_BINARY_LESSEQUAL:          return false;
		case EXPR_BINARY_GREATER:            return false;
		case EXPR_BINARY_GREATEREQUAL:       return false;
		case EXPR_BINARY_BITWISE_AND:        return false;
		case EXPR_BINARY_BITWISE_OR:         return false;
		case EXPR_BINARY_BITWISE_XOR:        return false;
		case EXPR_BINARY_SHIFTLEFT:          return false;
		case EXPR_BINARY_SHIFTRIGHT:         return false;
		case EXPR_BINARY_ASSIGN:             return true;
		case EXPR_BINARY_MUL_ASSIGN:         return true;
		case EXPR_BINARY_DIV_ASSIGN:         return true;
		case EXPR_BINARY_MOD_ASSIGN:         return true;
		case EXPR_BINARY_ADD_ASSIGN:         return true;
		case EXPR_BINARY_SUB_ASSIGN:         return true;
		case EXPR_BINARY_SHIFTLEFT_ASSIGN:   return true;
		case EXPR_BINARY_SHIFTRIGHT_ASSIGN:  return true;
		case EXPR_BINARY_BITWISE_AND_ASSIGN: return true;
		case EXPR_BINARY_BITWISE_XOR_ASSIGN: return true;
		case EXPR_BINARY_BITWISE_OR_ASSIGN:  return true;

		/* Only examine the right hand side of && and ||, because the left hand
		 * side already has the effect of controlling the execution of the right
		 * hand side */
		case EXPR_BINARY_LOGICAL_AND:
		case EXPR_BINARY_LOGICAL_OR:
		/* Only examine the right hand side of a comma expression, because the left
		 * hand side has a separate warning */
		case EXPR_BINARY_COMMA:
			return expression_has_effect(expr->binary.right);

		case EXPR_BINARY_ISGREATER:          return false;
		case EXPR_BINARY_ISGREATEREQUAL:     return false;
		case EXPR_BINARY_ISLESS:             return false;
		case EXPR_BINARY_ISLESSEQUAL:        return false;
		case EXPR_BINARY_ISLESSGREATER:      return false;
		case EXPR_BINARY_ISUNORDERED:        return false;
	}

	internal_errorf(HERE, "unexpected expression");
}

static void semantic_comma(binary_expression_t *expression)
{
	if (warning.unused_value) {
		const expression_t *const left = expression->left;
		if (!expression_has_effect(left)) {
			warningf(&left->base.source_position,
			         "left-hand operand of comma expression has no effect");
		}
	}
	expression->base.type = expression->right->base.type;
}

/**
 * @param prec_r precedence of the right operand
 */
#define CREATE_BINEXPR_PARSER(token_type, binexpression_type, prec_r, sfunc) \
static expression_t *parse_##binexpression_type(expression_t *left)          \
{                                                                            \
	expression_t *binexpr = allocate_expression_zero(binexpression_type);    \
	binexpr->binary.left  = left;                                            \
	eat(token_type);                                                         \
                                                                             \
	expression_t *right = parse_sub_expression(prec_r);                      \
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
CREATE_BINEXPR_PARSER('&',                    EXPR_BINARY_BITWISE_AND,        PREC_EQUALITY,       semantic_binexpr_arithmetic)
CREATE_BINEXPR_PARSER('^',                    EXPR_BINARY_BITWISE_XOR,        PREC_AND,            semantic_binexpr_arithmetic)
CREATE_BINEXPR_PARSER('|',                    EXPR_BINARY_BITWISE_OR,         PREC_XOR,            semantic_binexpr_arithmetic)
CREATE_BINEXPR_PARSER(T_ANDAND,               EXPR_BINARY_LOGICAL_AND,        PREC_OR,             semantic_logical_op)
CREATE_BINEXPR_PARSER(T_PIPEPIPE,             EXPR_BINARY_LOGICAL_OR,         PREC_LOGICAL_AND,    semantic_logical_op)
CREATE_BINEXPR_PARSER('=',                    EXPR_BINARY_ASSIGN,             PREC_ASSIGNMENT,     semantic_binexpr_assign)
CREATE_BINEXPR_PARSER(T_PLUSEQUAL,            EXPR_BINARY_ADD_ASSIGN,         PREC_ASSIGNMENT,     semantic_arithmetic_addsubb_assign)
CREATE_BINEXPR_PARSER(T_MINUSEQUAL,           EXPR_BINARY_SUB_ASSIGN,         PREC_ASSIGNMENT,     semantic_arithmetic_addsubb_assign)
CREATE_BINEXPR_PARSER(T_ASTERISKEQUAL,        EXPR_BINARY_MUL_ASSIGN,         PREC_ASSIGNMENT,     semantic_arithmetic_assign)
CREATE_BINEXPR_PARSER(T_SLASHEQUAL,           EXPR_BINARY_DIV_ASSIGN,         PREC_ASSIGNMENT,     semantic_divmod_assign)
CREATE_BINEXPR_PARSER(T_PERCENTEQUAL,         EXPR_BINARY_MOD_ASSIGN,         PREC_ASSIGNMENT,     semantic_divmod_assign)
CREATE_BINEXPR_PARSER(T_LESSLESSEQUAL,        EXPR_BINARY_SHIFTLEFT_ASSIGN,   PREC_ASSIGNMENT,     semantic_arithmetic_assign)
CREATE_BINEXPR_PARSER(T_GREATERGREATEREQUAL,  EXPR_BINARY_SHIFTRIGHT_ASSIGN,  PREC_ASSIGNMENT,     semantic_arithmetic_assign)
CREATE_BINEXPR_PARSER(T_ANDEQUAL,             EXPR_BINARY_BITWISE_AND_ASSIGN, PREC_ASSIGNMENT,     semantic_arithmetic_assign)
CREATE_BINEXPR_PARSER(T_PIPEEQUAL,            EXPR_BINARY_BITWISE_OR_ASSIGN,  PREC_ASSIGNMENT,     semantic_arithmetic_assign)
CREATE_BINEXPR_PARSER(T_CARETEQUAL,           EXPR_BINARY_BITWISE_XOR_ASSIGN, PREC_ASSIGNMENT,     semantic_arithmetic_assign)
CREATE_BINEXPR_PARSER(',',                    EXPR_BINARY_COMMA,              PREC_ASSIGNMENT,     semantic_comma)


static expression_t *parse_sub_expression(precedence_t precedence)
{
	if (token.type < 0) {
		return expected_expression_error();
	}

	expression_parser_function_t *parser
		= &expression_parsers[token.type];
	source_position_t             source_position = token.source_position;
	expression_t                 *left;

	if (parser->parser != NULL) {
		left = parser->parser();
	} else {
		left = parse_primary_expression();
	}
	assert(left != NULL);
	left->base.source_position = source_position;

	while (true) {
		if (token.type < 0) {
			return expected_expression_error();
		}

		parser = &expression_parsers[token.type];
		if (parser->infix_parser == NULL)
			break;
		if (parser->infix_precedence < precedence)
			break;

		left = parser->infix_parser(left);

		assert(left != NULL);
		assert(left->kind != EXPR_UNKNOWN);
		left->base.source_position = source_position;
	}

	return left;
}

/**
 * Parse an expression.
 */
static expression_t *parse_expression(void)
{
	return parse_sub_expression(PREC_EXPRESSION);
}

/**
 * Register a parser for a prefix-like operator.
 *
 * @param parser      the parser function
 * @param token_type  the token type of the prefix token
 */
static void register_expression_parser(parse_expression_function parser,
                                       int token_type)
{
	expression_parser_function_t *entry = &expression_parsers[token_type];

	if (entry->parser != NULL) {
		diagnosticf("for token '%k'\n", (token_type_t)token_type);
		panic("trying to register multiple expression parsers for a token");
	}
	entry->parser = parser;
}

/**
 * Register a parser for an infix operator with given precedence.
 *
 * @param parser      the parser function
 * @param token_type  the token type of the infix operator
 * @param precedence  the precedence of the operator
 */
static void register_infix_parser(parse_expression_infix_function parser,
		int token_type,	unsigned precedence)
{
	expression_parser_function_t *entry = &expression_parsers[token_type];

	if (entry->infix_parser != NULL) {
		diagnosticf("for token '%k'\n", (token_type_t)token_type);
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

	while (token.type == T_STRING_LITERAL || token.type == '[') {
		asm_argument_t *argument = allocate_ast_zero(sizeof(argument[0]));
		memset(argument, 0, sizeof(argument[0]));

		if (token.type == '[') {
			eat('[');
			if (token.type != T_IDENTIFIER) {
				parse_error_expected("while parsing asm argument",
				                     T_IDENTIFIER, NULL);
				return NULL;
			}
			argument->symbol = token.v.symbol;

			expect(']');
		}

		argument->constraints = parse_string_literals();
		expect('(');
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
						size  = get_atomic_type_size(get_intptr_kind());
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
							value_size  = get_atomic_type_size(get_intptr_kind());
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

			if (argument->constraints.begin[0] == '+')
				mark_vars_read(expression, NULL);
		} else {
			mark_vars_read(expression, NULL);
		}
		argument->expression = expression;
		expect(')');

		set_address_taken(expression, true);

		*anchor = argument;
		anchor  = &argument->next;

		if (token.type != ',')
			break;
		eat(',');
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
	asm_clobber_t *result = NULL;
	asm_clobber_t *last   = NULL;

	while (token.type == T_STRING_LITERAL) {
		asm_clobber_t *clobber = allocate_ast_zero(sizeof(clobber[0]));
		clobber->clobber       = parse_string_literals();

		if (last != NULL) {
			last->next = clobber;
		} else {
			result = clobber;
		}
		last = clobber;

		if (token.type != ',')
			break;
		eat(',');
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

	if (token.type == T_volatile) {
		next_token();
		asm_statement->is_volatile = true;
	}

	expect('(');
	add_anchor_token(')');
	add_anchor_token(':');
	asm_statement->asm_text = parse_string_literals();

	if (token.type != ':') {
		rem_anchor_token(':');
		goto end_of_asm;
	}
	eat(':');

	asm_statement->outputs = parse_asm_arguments(true);
	if (token.type != ':') {
		rem_anchor_token(':');
		goto end_of_asm;
	}
	eat(':');

	asm_statement->inputs = parse_asm_arguments(false);
	if (token.type != ':') {
		rem_anchor_token(':');
		goto end_of_asm;
	}
	rem_anchor_token(':');
	eat(':');

	asm_statement->clobbers = parse_asm_clobbers();

end_of_asm:
	rem_anchor_token(')');
	expect(')');
	expect(';');

	if (asm_statement->outputs == NULL) {
		/* GCC: An 'asm' instruction without any output operands will be treated
		 * identically to a volatile 'asm' instruction. */
		asm_statement->is_volatile = true;
	}

	return statement;
end_error:
	return create_invalid_statement();
}

/**
 * Parse a case statement.
 */
static statement_t *parse_case_statement(void)
{
	statement_t       *const statement = allocate_statement_zero(STATEMENT_CASE_LABEL);
	source_position_t *const pos       = &statement->base.source_position;

	eat(T_case);

	expression_t *const expression   = parse_expression();
	statement->case_label.expression = expression;
	if (!is_constant_expression(expression)) {
		/* This check does not prevent the error message in all cases of an
		 * prior error while parsing the expression.  At least it catches the
		 * common case of a mistyped enum entry. */
		if (is_type_valid(skip_typeref(expression->base.type))) {
			errorf(pos, "case label does not reduce to an integer constant");
		}
		statement->case_label.is_bad = true;
	} else {
		long const val = fold_constant(expression);
		statement->case_label.first_case = val;
		statement->case_label.last_case  = val;
	}

	if (GNU_MODE) {
		if (token.type == T_DOTDOTDOT) {
			next_token();
			expression_t *const end_range   = parse_expression();
			statement->case_label.end_range = end_range;
			if (!is_constant_expression(end_range)) {
				/* This check does not prevent the error message in all cases of an
				 * prior error while parsing the expression.  At least it catches the
				 * common case of a mistyped enum entry. */
				if (is_type_valid(skip_typeref(end_range->base.type))) {
					errorf(pos, "case range does not reduce to an integer constant");
				}
				statement->case_label.is_bad = true;
			} else {
				long const val = fold_constant(end_range);
				statement->case_label.last_case = val;

				if (warning.other && val < statement->case_label.first_case) {
					statement->case_label.is_empty_range = true;
					warningf(pos, "empty range specified");
				}
			}
		}
	}

	PUSH_PARENT(statement);

	expect(':');
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

	statement_t *const inner_stmt = parse_statement();
	statement->case_label.statement = inner_stmt;
	if (inner_stmt->kind == STATEMENT_DECLARATION) {
		errorf(&inner_stmt->base.source_position, "declaration after case label");
	}

	POP_PARENT;
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
			errorf(HERE, "multiple default labels in one switch (previous declared %P)",
			       &def_label->base.source_position);
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

	statement_t *const inner_stmt = parse_statement();
	statement->case_label.statement = inner_stmt;
	if (inner_stmt->kind == STATEMENT_DECLARATION) {
		errorf(&inner_stmt->base.source_position, "declaration after default label");
	}

	POP_PARENT;
	return statement;
end_error:
	POP_PARENT;
	return create_invalid_statement();
}

/**
 * Parse a label statement.
 */
static statement_t *parse_label_statement(void)
{
	assert(token.type == T_IDENTIFIER);
	symbol_t *symbol = token.v.symbol;
	label_t  *label  = get_label(symbol);

	statement_t *const statement = allocate_statement_zero(STATEMENT_LABEL);
	statement->label.label       = label;

	next_token();

	PUSH_PARENT(statement);

	/* if statement is already set then the label is defined twice,
	 * otherwise it was just mentioned in a goto/local label declaration so far
	 */
	if (label->statement != NULL) {
		errorf(HERE, "duplicate label '%Y' (declared %P)",
		       symbol, &label->base.source_position);
	} else {
		label->base.source_position = token.source_position;
		label->statement            = statement;
	}

	eat(':');

	if (token.type == '}') {
		/* TODO only warn? */
		if (warning.other && false) {
			warningf(HERE, "label at end of compound statement");
			statement->label.statement = create_empty_statement();
		} else {
			errorf(HERE, "label at end of compound statement");
			statement->label.statement = create_invalid_statement();
		}
	} else if (token.type == ';') {
		/* Eat an empty statement here, to avoid the warning about an empty
		 * statement after a label.  label:; is commonly used to have a label
		 * before a closing brace. */
		statement->label.statement = create_empty_statement();
		next_token();
	} else {
		statement_t *const inner_stmt = parse_statement();
		statement->label.statement = inner_stmt;
		if (inner_stmt->kind == STATEMENT_DECLARATION) {
			errorf(&inner_stmt->base.source_position, "declaration after label");
		}
	}

	/* remember the labels in a list for later checking */
	*label_anchor = &statement->label;
	label_anchor  = &statement->label.next;

	POP_PARENT;
	return statement;
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

	expect('(');
	add_anchor_token(')');
	expression_t *const expr = parse_expression();
	statement->ifs.condition = expr;
	/* §6.8.4.1:1  The controlling expression of an if statement shall have
	 *             scalar type. */
	semantic_condition(expr, "condition of 'if'-statment");
	mark_vars_read(expr, NULL);
	rem_anchor_token(')');
	expect(')');

end_error:
	rem_anchor_token('{');

	add_anchor_token(T_else);
	statement->ifs.true_statement = parse_statement();
	rem_anchor_token(T_else);

	if (token.type == T_else) {
		next_token();
		statement->ifs.false_statement = parse_statement();
	}

	POP_PARENT;
	return statement;
}

/**
 * Check that all enums are handled in a switch.
 *
 * @param statement  the switch statement to check
 */
static void check_enum_cases(const switch_statement_t *statement) {
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
		long                value      = expression != NULL ? fold_constant(expression) : last_value + 1;
		bool                found      = false;
		for (const case_label_statement_t *l = statement->first_case; l != NULL; l = l->next) {
			if (l->expression == NULL)
				continue;
			if (l->first_case <= value && value <= l->last_case) {
				found = true;
				break;
			}
		}
		if (! found) {
			warningf(&statement->base.source_position,
			         "enumeration value '%Y' not handled in switch",
			         entry->base.symbol);
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

	expect('(');
	add_anchor_token(')');
	expression_t *const expr = parse_expression();
	mark_vars_read(expr, NULL);
	type_t       *      type = skip_typeref(expr->base.type);
	if (is_type_integer(type)) {
		type = promote_integer(type);
		if (warning.traditional) {
			if (get_rank(type) >= get_akind_rank(ATOMIC_TYPE_LONG)) {
				warningf(&expr->base.source_position,
					"'%T' switch expression not converted to '%T' in ISO C",
					type, type_int);
			}
		}
	} else if (is_type_valid(type)) {
		errorf(&expr->base.source_position,
		       "switch quantity is not an integer, but '%T'", type);
		type = type_error_type;
	}
	statement->switchs.expression = create_implicit_cast(expr, type);
	expect(')');
	rem_anchor_token(')');

	switch_statement_t *rem = current_switch;
	current_switch          = &statement->switchs;
	statement->switchs.body = parse_statement();
	current_switch          = rem;

	if (warning.switch_default &&
	    statement->switchs.default_label == NULL) {
		warningf(&statement->base.source_position, "switch has no default case");
	}
	if (warning.switch_enum)
		check_enum_cases(&statement->switchs);

	POP_PARENT;
	return statement;
end_error:
	POP_PARENT;
	return create_invalid_statement();
}

static statement_t *parse_loop_body(statement_t *const loop)
{
	statement_t *const rem = current_loop;
	current_loop = loop;

	statement_t *const body = parse_statement();

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

	expect('(');
	add_anchor_token(')');
	expression_t *const cond = parse_expression();
	statement->whiles.condition = cond;
	/* §6.8.5:2    The controlling expression of an iteration statement shall
	 *             have scalar type. */
	semantic_condition(cond, "condition of 'while'-statement");
	mark_vars_read(cond, NULL);
	rem_anchor_token(')');
	expect(')');

	statement->whiles.body = parse_loop_body(statement);

	POP_PARENT;
	return statement;
end_error:
	POP_PARENT;
	return create_invalid_statement();
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

	expect(T_while);
	expect('(');
	add_anchor_token(')');
	expression_t *const cond = parse_expression();
	statement->do_while.condition = cond;
	/* §6.8.5:2    The controlling expression of an iteration statement shall
	 *             have scalar type. */
	semantic_condition(cond, "condition of 'do-while'-statement");
	mark_vars_read(cond, NULL);
	rem_anchor_token(')');
	expect(')');
	expect(';');

	POP_PARENT;
	return statement;
end_error:
	POP_PARENT;
	return create_invalid_statement();
}

/**
 * Parse a for statement.
 */
static statement_t *parse_for(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_FOR);

	eat(T_for);

	PUSH_PARENT(statement);

	size_t const  top       = environment_top();
	scope_t      *old_scope = scope_push(&statement->fors.scope);

	expect('(');
	add_anchor_token(')');

	if (token.type == ';') {
		next_token();
	} else if (is_declaration_specifier(&token, false)) {
		parse_declaration(record_entity, DECL_FLAGS_NONE);
	} else {
		add_anchor_token(';');
		expression_t *const init = parse_expression();
		statement->fors.initialisation = init;
		mark_vars_read(init, ENT_ANY);
		if (warning.unused_value && !expression_has_effect(init)) {
			warningf(&init->base.source_position,
					"initialisation of 'for'-statement has no effect");
		}
		rem_anchor_token(';');
		expect(';');
	}

	if (token.type != ';') {
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
	if (token.type != ')') {
		expression_t *const step = parse_expression();
		statement->fors.step = step;
		mark_vars_read(step, ENT_ANY);
		if (warning.unused_value && !expression_has_effect(step)) {
			warningf(&step->base.source_position,
			         "step of 'for'-statement has no effect");
		}
	}
	expect(')');
	rem_anchor_token(')');
	statement->fors.body = parse_loop_body(statement);

	assert(current_scope == &statement->fors.scope);
	scope_pop(old_scope);
	environment_pop_to(top);

	POP_PARENT;
	return statement;

end_error:
	POP_PARENT;
	rem_anchor_token(')');
	assert(current_scope == &statement->fors.scope);
	scope_pop(old_scope);
	environment_pop_to(top);

	return create_invalid_statement();
}

/**
 * Parse a goto statement.
 */
static statement_t *parse_goto(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_GOTO);
	eat(T_goto);

	if (GNU_MODE && token.type == '*') {
		next_token();
		expression_t *expression = parse_expression();
		mark_vars_read(expression, NULL);

		/* Argh: although documentation says the expression must be of type void*,
		 * gcc accepts anything that can be casted into void* without error */
		type_t *type = expression->base.type;

		if (type != type_error_type) {
			if (!is_type_pointer(type) && !is_type_integer(type)) {
				errorf(&expression->base.source_position,
					"cannot convert to a pointer type");
			} else if (warning.other && type != type_void_ptr) {
				warningf(&expression->base.source_position,
					"type of computed goto expression should be 'void*' not '%T'", type);
			}
			expression = create_implicit_cast(expression, type_void_ptr);
		}

		statement->gotos.expression = expression;
	} else {
		if (token.type != T_IDENTIFIER) {
			if (GNU_MODE)
				parse_error_expected("while parsing goto", T_IDENTIFIER, '*', NULL);
			else
				parse_error_expected("while parsing goto", T_IDENTIFIER, NULL);
			eat_until_anchor();
			goto end_error;
		}
		symbol_t *symbol = token.v.symbol;
		next_token();

		statement->gotos.label = get_label(symbol);
	}

	/* remember the goto's in a list for later checking */
	*goto_anchor = &statement->gotos;
	goto_anchor  = &statement->gotos.next;

	expect(';');

	return statement;
end_error:
	return create_invalid_statement();
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
	expect(';');

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
	expect(';');

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
	expect(';');

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

/**
 * Parse a return statement.
 */
static statement_t *parse_return(void)
{
	eat(T_return);

	statement_t *statement = allocate_statement_zero(STATEMENT_RETURN);

	expression_t *return_value = NULL;
	if (token.type != ';') {
		return_value = parse_expression();
		mark_vars_read(return_value, NULL);
	}

	const type_t *const func_type = skip_typeref(current_function->base.type);
	assert(is_type_function(func_type));
	type_t *const return_type = skip_typeref(func_type->function.return_type);

	if (return_value != NULL) {
		type_t *return_value_type = skip_typeref(return_value->base.type);

		if (is_type_atomic(return_type,        ATOMIC_TYPE_VOID) &&
				!is_type_atomic(return_value_type, ATOMIC_TYPE_VOID)) {
			if (warning.other) {
				warningf(&statement->base.source_position,
						"'return' with a value, in function returning void");
			}
			return_value = NULL;
		} else {
			assign_error_t error = semantic_assign(return_type, return_value);
			report_assign_error(error, return_type,	return_value, "'return'",
			                    &statement->base.source_position);
			return_value = create_implicit_cast(return_value, return_type);
		}
		/* check for returning address of a local var */
		if (warning.other && return_value != NULL
				&& return_value->base.kind == EXPR_UNARY_TAKE_ADDRESS) {
			const expression_t *expression = return_value->unary.value;
			if (expression_is_local_variable(expression)) {
				warningf(&statement->base.source_position,
				         "function returns address of local variable");
			}
		}
	} else if (warning.other && !is_type_atomic(return_type, ATOMIC_TYPE_VOID)) {
		warningf(&statement->base.source_position,
				"'return' without value, in function returning non-void");
	}
	statement->returns.value = return_value;

	expect(';');

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

	if (before == NULL) {
		statement->declaration.declarations_begin = current_scope->entities;
	} else {
		statement->declaration.declarations_begin = before->base.next;
	}
	statement->declaration.declarations_end = current_scope->last_entity;

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

	expect(';');

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

	POP_PARENT;

	if (token.type == T___except) {
		eat(T___except);
		expect('(');
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
		expect(')');
		statement->ms_try.final_statement = parse_compound_statement(false);
	} else if (token.type == T__finally) {
		eat(T___finally);
		statement->ms_try.final_statement = parse_compound_statement(false);
	} else {
		parse_error_expected("while parsing __try statement", T___except, T___finally, NULL);
		return create_invalid_statement();
	}
	return statement;
end_error:
	return create_invalid_statement();
}

static statement_t *parse_empty_statement(void)
{
	if (warning.empty_statement) {
		warningf(HERE, "statement is empty");
	}
	statement_t *const statement = create_empty_statement();
	eat(';');
	return statement;
}

static statement_t *parse_local_label_declaration(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_DECLARATION);

	eat(T___label__);

	entity_t *begin = NULL, *end = NULL;

	while (true) {
		if (token.type != T_IDENTIFIER) {
			parse_error_expected("while parsing local label declaration",
				T_IDENTIFIER, NULL);
			goto end_error;
		}
		symbol_t *symbol = token.v.symbol;
		entity_t *entity = get_entity(symbol, NAMESPACE_LABEL);
		if (entity != NULL && entity->base.parent_scope == current_scope) {
			errorf(HERE, "multiple definitions of '__label__ %Y' (previous definition %P)",
			       symbol, &entity->base.source_position);
		} else {
			entity = allocate_entity_zero(ENTITY_LOCAL_LABEL);

			entity->base.parent_scope    = current_scope;
			entity->base.namespc         = NAMESPACE_LABEL;
			entity->base.source_position = token.source_position;
			entity->base.symbol          = symbol;

			if (end != NULL)
				end->base.next = entity;
			end = entity;
			if (begin == NULL)
				begin = entity;

			environment_push(entity);
		}
		next_token();

		if (token.type != ',')
			break;
		next_token();
	}
	eat(';');
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

	if (token.type == T_IDENTIFIER) {
		symbol = token.v.symbol;
		next_token();

		entity = get_entity(symbol, NAMESPACE_NORMAL);
		if (entity != NULL && entity->kind != ENTITY_NAMESPACE
				&& entity->base.parent_scope == current_scope) {
			error_redefined_as_different_kind(&token.source_position,
			                                  entity, ENTITY_NAMESPACE);
			entity = NULL;
		}
	}

	if (entity == NULL) {
		entity                       = allocate_entity_zero(ENTITY_NAMESPACE);
		entity->base.symbol          = symbol;
		entity->base.source_position = token.source_position;
		entity->base.namespc         = NAMESPACE_NORMAL;
		entity->base.parent_scope    = current_scope;
	}

	if (token.type == '=') {
		/* TODO: parse namespace alias */
		panic("namespace alias definition not supported yet");
	}

	environment_push(entity);
	append_entity(current_scope, entity);

	size_t const  top       = environment_top();
	scope_t      *old_scope = scope_push(&entity->namespacee.members);

	expect('{');
	parse_externals();
	expect('}');

end_error:
	assert(current_scope == &entity->namespacee.members);
	scope_pop(old_scope);
	environment_pop_to(top);
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
	switch (token.type) {
	case T_IDENTIFIER: {
		token_type_t la1_type = (token_type_t)look_ahead(1)->type;
		if (la1_type == ':') {
			statement = parse_label_statement();
		} else if (is_typedef_symbol(token.v.symbol)) {
			statement = parse_declaration_statement();
		} else {
			/* it's an identifier, the grammar says this must be an
			 * expression statement. However it is common that users mistype
			 * declaration types, so we guess a bit here to improve robustness
			 * for incorrect programs */
			switch (la1_type) {
			case '&':
			case '*':
				if (get_entity(token.v.symbol, NAMESPACE_NORMAL) != NULL)
					goto expression_statment;
				/* FALLTHROUGH */

			DECLARATION_START
			case T_IDENTIFIER:
				statement = parse_declaration_statement();
				break;

			default:
expression_statment:
				statement = parse_expression_statement();
				break;
			}
		}
		break;
	}

	case T___extension__:
		/* This can be a prefix to a declaration or an expression statement.
		 * We simply eat it now and parse the rest with tail recursion. */
		do {
			next_token();
		} while (token.type == T___extension__);
		bool old_gcc_extension = in_gcc_extension;
		in_gcc_extension       = true;
		statement = intern_parse_statement();
		in_gcc_extension = old_gcc_extension;
		break;

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
		statement = create_invalid_statement();
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

	if (statement->kind == STATEMENT_EXPRESSION && warning.unused_value) {
		expression_t *expression = statement->expression.expression;
		if (!expression_has_effect(expression)) {
			warningf(&expression->base.source_position,
					"statement has no effect");
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

	size_t const  top       = environment_top();
	scope_t      *old_scope = scope_push(&statement->compound.scope);

	statement_t **anchor            = &statement->compound.statements;
	bool          only_decls_so_far = true;
	while (token.type != '}') {
		if (token.type == T_EOF) {
			errorf(&statement->base.source_position,
			       "EOF while parsing compound statement");
			break;
		}
		statement_t *sub_statement = intern_parse_statement();
		if (is_invalid_statement(sub_statement)) {
			/* an error occurred. if we are at an anchor, return */
			if (at_anchor())
				goto end_error;
			continue;
		}

		if (warning.declaration_after_statement) {
			if (sub_statement->kind != STATEMENT_DECLARATION) {
				only_decls_so_far = false;
			} else if (!only_decls_so_far) {
				warningf(&sub_statement->base.source_position,
				         "ISO C90 forbids mixed declarations and code");
			}
		}

		*anchor = sub_statement;

		while (sub_statement->base.next != NULL)
			sub_statement = sub_statement->base.next;

		anchor = &sub_statement->base.next;
	}
	next_token();

	/* look over all statements again to produce no effect warnings */
	if (warning.unused_value) {
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
				warningf(&expression->base.source_position,
				         "statement has no effect");
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
	assert(current_scope == &statement->compound.scope);
	scope_pop(old_scope);
	environment_pop_to(top);

	POP_PARENT;
	return statement;
}

/**
 * Check for unused global static functions and variables
 */
static void check_unused_globals(void)
{
	if (!warning.unused_function && !warning.unused_variable)
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

		type_t *const type = declaration->type;
		const char *s;
		if (entity->kind == ENTITY_FUNCTION) {
			/* inhibit warning for static inline functions */
			if (entity->function.is_inline)
				continue;

			s = entity->function.statement != NULL ? "defined" : "declared";
		} else {
			s = "defined";
		}

		warningf(&declaration->base.source_position, "'%#T' %s but not used",
			type, declaration->base.symbol, s);
	}
}

static void parse_global_asm(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_ASM);

	eat(T_asm);
	expect('(');

	statement->asms.asm_text = parse_string_literals();
	statement->base.next     = unit->global_asm;
	unit->global_asm         = statement;

	expect(')');
	expect(';');

end_error:;
}

static void parse_linkage_specification(void)
{
	eat(T_extern);
	assert(token.type == T_STRING_LITERAL);

	const char *linkage = parse_string_literals().begin;

	linkage_kind_t old_linkage = current_linkage;
	linkage_kind_t new_linkage;
	if (strcmp(linkage, "C") == 0) {
		new_linkage = LINKAGE_C;
	} else if (strcmp(linkage, "C++") == 0) {
		new_linkage = LINKAGE_CXX;
	} else {
		errorf(HERE, "linkage string \"%s\" not recognized", linkage);
		new_linkage = LINKAGE_INVALID;
	}
	current_linkage = new_linkage;

	if (token.type == '{') {
		next_token();
		parse_externals();
		expect('}');
	} else {
		parse_external();
	}

end_error:
	assert(current_linkage == new_linkage);
	current_linkage = old_linkage;
}

static void parse_external(void)
{
	switch (token.type) {
		DECLARATION_START_NO_EXTERN
		case T_IDENTIFIER:
		case T___extension__:
		/* tokens below are for implicit int */
		case '&': /* & x; -> int& x; (and error later, because C++ has no
		             implicit int) */
		case '*': /* * x; -> int* x; */
		case '(': /* (x); -> int (x); */
			parse_external_declaration();
			return;

		case T_extern:
			if (look_ahead(1)->type == T_STRING_LITERAL) {
				parse_linkage_specification();
			} else {
				parse_external_declaration();
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
				if (warning.other)
					warningf(HERE, "stray ';' outside of function");
				next_token();
				return;
			}
			/* FALLTHROUGH */

		default:
			errorf(HERE, "stray %K outside of function", &token);
			if (token.type == '(' || token.type == '{' || token.type == '[')
				eat_until_matching_token(token.type);
			next_token();
			return;
	}
}

static void parse_externals(void)
{
	add_anchor_token('}');
	add_anchor_token(T_EOF);

#ifndef NDEBUG
	unsigned char token_anchor_copy[T_LAST_TOKEN];
	memcpy(token_anchor_copy, token_anchor_set, sizeof(token_anchor_copy));
#endif

	while (token.type != T_EOF && token.type != '}') {
#ifndef NDEBUG
		bool anchor_leak = false;
		for (int i = 0; i != T_LAST_TOKEN; ++i) {
			unsigned char count = token_anchor_set[i] - token_anchor_copy[i];
			if (count != 0) {
				errorf(HERE, "Leaked anchor token %k %d times", i, count);
				anchor_leak = true;
			}
		}
		if (in_gcc_extension) {
			errorf(HERE, "Leaked __extension__");
			anchor_leak = true;
		}

		if (anchor_leak)
			abort();
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

		if (token.type == T_EOF)
			break;

		errorf(HERE, "stray %K outside of function", &token);
		if (token.type == '(' || token.type == '{' || token.type == '[')
			eat_until_matching_token(token.type);
		next_token();
	}
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

	type_set_output(stderr);
	ast_set_output(stderr);

	assert(unit == NULL);
	unit = allocate_ast_zero(sizeof(unit[0]));

	assert(file_scope == NULL);
	file_scope = &unit->scope;

	assert(current_scope == NULL);
	scope_push(&unit->scope);
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

/* GCC allows global arrays without size and assigns them a length of one,
 * if no different declaration follows */
static void complete_incomplete_arrays(void)
{
	size_t n = ARR_LEN(incomplete_arrays);
	for (size_t i = 0; i != n; ++i) {
		declaration_t *const decl      = incomplete_arrays[i];
		type_t        *const orig_type = decl->type;
		type_t        *const type      = skip_typeref(orig_type);

		if (!is_type_incomplete(type))
			continue;

		if (warning.other) {
			warningf(&decl->base.source_position,
					"array '%#T' assumed to have one element",
					orig_type, decl->base.symbol);
		}

		type_t *const new_type = duplicate_type(type);
		new_type->array.size_constant     = true;
		new_type->array.has_implicit_size = true;
		new_type->array.size              = 1;

		type_t *const result = typehash_insert(new_type);
		if (type != result)
			free_type(type);

		decl->type = result;
	}
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

	if (c_mode & _MS) {
		/* add predefined symbols for extended-decl-modifier */
		sym_align      = symbol_table_insert("align");
		sym_allocate   = symbol_table_insert("allocate");
		sym_dllimport  = symbol_table_insert("dllimport");
		sym_dllexport  = symbol_table_insert("dllexport");
		sym_naked      = symbol_table_insert("naked");
		sym_noinline   = symbol_table_insert("noinline");
		sym_noreturn   = symbol_table_insert("noreturn");
		sym_nothrow    = symbol_table_insert("nothrow");
		sym_novtable   = symbol_table_insert("novtable");
		sym_property   = symbol_table_insert("property");
		sym_get        = symbol_table_insert("get");
		sym_put        = symbol_table_insert("put");
		sym_selectany  = symbol_table_insert("selectany");
		sym_thread     = symbol_table_insert("thread");
		sym_uuid       = symbol_table_insert("uuid");
		sym_deprecated = symbol_table_insert("deprecated");
		sym_restrict   = symbol_table_insert("restrict");
		sym_noalias    = symbol_table_insert("noalias");
	}
	memset(token_anchor_set, 0, sizeof(token_anchor_set));

	init_expression_parsers();
	obstack_init(&temp_obst);

	symbol_t *const va_list_sym = symbol_table_insert("__builtin_va_list");
	type_valist = create_builtin_type(va_list_sym, type_void_ptr);
}

/**
 * Terminate the parser.
 */
void exit_parser(void)
{
	obstack_free(&temp_obst, NULL);
}
