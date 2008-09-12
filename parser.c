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
#include "lang_features.h"
#include "warning.h"
#include "adt/bitfiddle.h"
#include "adt/error.h"
#include "adt/array.h"

/** if wchar_t is equal to unsigned short. */
bool opt_short_wchar_t =
#ifdef _WIN32
	true;
#else
	false;
#endif

//#define PRINT_TOKENS
#define MAX_LOOKAHEAD 2

typedef struct {
	declaration_t *old_declaration;
	symbol_t      *symbol;
	unsigned short namespc;
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
	unsigned char      declared_storage_class;
	unsigned char      alignment;         /**< Alignment, 0 if not set. */
	unsigned int       is_inline : 1;
	unsigned int       deprecated : 1;
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
	type_t        *type;        /**< the type of the initializer. In case of an
	                                 array type with unspecified size this gets
	                                 adjusted to the actual size. */
	declaration_t *declaration; /**< the declaration that is initialized if any */
	bool           must_be_constant;
} parse_initializer_env_t;

typedef declaration_t* (*parsed_declaration_func) (declaration_t *declaration);

static token_t             token;
static token_t             lookahead_buffer[MAX_LOOKAHEAD];
static int                 lookahead_bufpos;
static stack_entry_t      *environment_stack = NULL;
static stack_entry_t      *label_stack       = NULL;
static scope_t            *global_scope      = NULL;
static scope_t            *scope             = NULL;
static declaration_t      *last_declaration  = NULL;
static declaration_t      *current_function  = NULL;
static declaration_t      *current_init_decl = NULL;
static switch_statement_t *current_switch    = NULL;
static statement_t        *current_loop      = NULL;
static statement_t        *current_parent    = NULL;
static ms_try_statement_t *current_try       = NULL;
static goto_statement_t   *goto_first        = NULL;
static goto_statement_t   *goto_last         = NULL;
static label_statement_t  *label_first       = NULL;
static label_statement_t  *label_last        = NULL;
static translation_unit_t *unit              = NULL;
static struct obstack      temp_obst;

#define PUSH_PARENT(stmt)                          \
	statement_t *const prev_parent = current_parent; \
	current_parent = (stmt);
#define POP_PARENT ((void)(current_parent = prev_parent))

static source_position_t null_position = { NULL, 0 };

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

static type_t *type_valist;

static statement_t *parse_compound_statement(bool inside_expression_statement);
static statement_t *parse_statement(void);

static expression_t *parse_sub_expression(unsigned precedence);
static expression_t *parse_expression(void);
static type_t       *parse_typename(void);

static void parse_compound_type_entries(declaration_t *compound_declaration);
static declaration_t *parse_declarator(
		const declaration_specifiers_t *specifiers, bool may_be_abstract);
static declaration_t *record_declaration(declaration_t *declaration);

static void semantic_comparison(binary_expression_t *expression);

#define STORAGE_CLASSES     \
	case T_typedef:         \
	case T_extern:          \
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

#ifdef PROVIDE_COMPLEX
#define COMPLEX_SPECIFIERS  \
	case T__Complex:
#define IMAGINARY_SPECIFIERS \
	case T__Imaginary:
#else
#define COMPLEX_SPECIFIERS
#define IMAGINARY_SPECIFIERS
#endif

#define TYPE_SPECIFIERS       \
	case T_void:              \
	case T_char:              \
	case T_short:             \
	case T_int:               \
	case T_long:              \
	case T_float:             \
	case T_double:            \
	case T_signed:            \
	case T_unsigned:          \
	case T__Bool:             \
	case T_struct:            \
	case T_union:             \
	case T_enum:              \
	case T___typeof__:        \
	case T___builtin_va_list: \
	case T__declspec:         \
	COMPLEX_SPECIFIERS        \
	IMAGINARY_SPECIFIERS

#define DECLARATION_START   \
	STORAGE_CLASSES         \
	TYPE_QUALIFIERS         \
	TYPE_SPECIFIERS

#define TYPENAME_START      \
	TYPE_QUALIFIERS         \
	TYPE_SPECIFIERS

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

static declaration_t *allocate_declaration_zero(void)
{
	declaration_t *declaration = allocate_ast_zero(sizeof(declaration_t));
	declaration->type      = type_error_type;
	declaration->alignment = 0;
	return declaration;
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
	assert(kind <= sizeof(sizes) / sizeof(sizes[0]));
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
	};
	if (kind >= EXPR_UNARY_FIRST && kind <= EXPR_UNARY_LAST) {
		return sizes[EXPR_UNARY_FIRST];
	}
	if (kind >= EXPR_BINARY_FIRST && kind <= EXPR_BINARY_LAST) {
		return sizes[EXPR_BINARY_FIRST];
	}
	assert(kind <= sizeof(sizes) / sizeof(sizes[0]));
	assert(sizes[kind] != 0);
	return sizes[kind];
}

/**
 * Allocate a statement node of given kind and initialize all
 * fields with zero.
 */
static statement_t *allocate_statement_zero(statement_kind_t kind)
{
	size_t       size = get_statement_struct_size(kind);
	statement_t *res  = allocate_ast_zero(size);

	res->base.kind   = kind;
	res->base.parent = current_parent;
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

	res->base.kind = kind;
	res->base.type = type_error_type;
	return res;
}

/**
 * Creates a new invalid expression.
 */
static expression_t *create_invalid_expression(void)
{
	expression_t *expression         = allocate_expression_zero(EXPR_INVALID);
	expression->base.source_position = token.source_position;
	return expression;
}

/**
 * Creates a new invalid statement.
 */
static statement_t *create_invalid_statement(void)
{
	statement_t *statement          = allocate_statement_zero(STATEMENT_INVALID);
	statement->base.source_position = token.source_position;
	return statement;
}

/**
 * Allocate a new empty statement.
 */
static statement_t *create_empty_statement(void)
{
	statement_t *statement          = allocate_statement_zero(STATEMENT_EMPTY);
	statement->base.source_position = token.source_position;
	return statement;
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
 * @param source_position  the source position of the type definition
 */
static type_t *allocate_type_zero(type_kind_t kind, const source_position_t *source_position)
{
	size_t  size = get_type_struct_size(kind);
	type_t *res  = obstack_alloc(type_obst, size);
	memset(res, 0, size);

	res->base.kind            = kind;
	res->base.source_position = *source_position;
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
 * Returns the index of the top element of the label stack.
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
 * Adds a token to the token anchor set (a multi-set).
 */
static void add_anchor_token(int token_type)
{
	assert(0 <= token_type && token_type < T_LAST_TOKEN);
	++token_anchor_set[token_type];
}

static int save_and_reset_anchor_state(int token_type)
{
	assert(0 <= token_type && token_type < T_LAST_TOKEN);
	int count = token_anchor_set[token_type];
	token_anchor_set[token_type] = 0;
	return count;
}

static void restore_anchor_state(int token_type, int count)
{
	assert(0 <= token_type && token_type < T_LAST_TOKEN);
	token_anchor_set[token_type] = count;
}

/**
 * Remove a token from the token anchor set (a multi-set).
 */
static void rem_anchor_token(int token_type)
{
	assert(0 <= token_type && token_type < T_LAST_TOKEN);
	--token_anchor_set[token_type];
}

static bool at_anchor(void)
{
	if (token.type < 0)
		return false;
	return token_anchor_set[token.type];
}

/**
 * Eat tokens until a matching token is found.
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
	if (token.type == T_EOF)
		return;
	while (token_anchor_set[token.type] == 0) {
		if (token.type == '(' || token.type == '{' || token.type == '[')
			eat_until_matching_token(token.type);
		if (token.type == T_EOF)
			break;
		next_token();
	}
}

static void eat_block(void)
{
	eat_until_matching_token('{');
	if (token.type == '}')
		next_token();
}

/**
 * eat all token until a ';' is reached or a stop token is found.
 */
static void eat_statement(void)
{
	eat_until_matching_token(';');
	if (token.type == ';')
		next_token();
}

#define eat(token_type)  do { assert(token.type == token_type); next_token(); } while (0)

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
 * Report a type error.
 */
static void type_error(const char *msg, const source_position_t *source_position,
                       type_t *type)
{
	errorf(source_position, "%s, but found type '%T'", msg, type);
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
 * Expect the the current token is the expected token.
 * If not, generate an error, eat the current statement,
 * and goto the end_error label.
 */
#define expect(expected)                              \
	do {                                              \
    if (UNLIKELY(token.type != (expected))) {          \
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

static void set_scope(scope_t *new_scope)
{
	if (scope != NULL) {
		scope->last_declaration = last_declaration;
	}
	scope = new_scope;

	last_declaration = new_scope->last_declaration;
}

/**
 * Search a symbol in a given namespace and returns its declaration or
 * NULL if this symbol was not found.
 */
static declaration_t *get_declaration(const symbol_t *const symbol,
                                      const namespace_t namespc)
{
	declaration_t *declaration = symbol->declaration;
	for( ; declaration != NULL; declaration = declaration->symbol_next) {
		if (declaration->namespc == namespc)
			return declaration;
	}

	return NULL;
}

/**
 * pushs an environment_entry on the environment stack and links the
 * corresponding symbol to the new entry
 */
static void stack_push(stack_entry_t **stack_ptr, declaration_t *declaration)
{
	symbol_t    *symbol  = declaration->symbol;
	namespace_t  namespc = (namespace_t) declaration->namespc;

	/* replace/add declaration into declaration list of the symbol */
	declaration_t *iter = symbol->declaration;
	if (iter == NULL) {
		symbol->declaration = declaration;
	} else {
		declaration_t *iter_last = NULL;
		for( ; iter != NULL; iter_last = iter, iter = iter->symbol_next) {
			/* replace an entry? */
			if (iter->namespc == namespc) {
				if (iter_last == NULL) {
					symbol->declaration = declaration;
				} else {
					iter_last->symbol_next = declaration;
				}
				declaration->symbol_next = iter->symbol_next;
				break;
			}
		}
		if (iter == NULL) {
			assert(iter_last->symbol_next == NULL);
			iter_last->symbol_next = declaration;
		}
	}

	/* remember old declaration */
	stack_entry_t entry;
	entry.symbol          = symbol;
	entry.old_declaration = iter;
	entry.namespc         = (unsigned short) namespc;
	ARR_APP1(stack_entry_t, *stack_ptr, entry);
}

static void environment_push(declaration_t *declaration)
{
	assert(declaration->source_position.input_name != NULL);
	assert(declaration->parent_scope != NULL);
	stack_push(&environment_stack, declaration);
}

/**
 * Push a declaration of the label stack.
 *
 * @param declaration  the declaration
 */
static void label_push(declaration_t *declaration)
{
	declaration->parent_scope = &current_function->scope;
	stack_push(&label_stack, declaration);
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

	for(i = top; i > new_top; --i) {
		stack_entry_t *entry = &stack[i - 1];

		declaration_t *old_declaration = entry->old_declaration;
		symbol_t      *symbol          = entry->symbol;
		namespace_t    namespc         = (namespace_t)entry->namespc;

		/* replace/remove declaration */
		declaration_t *declaration = symbol->declaration;
		assert(declaration != NULL);
		if (declaration->namespc == namespc) {
			if (old_declaration == NULL) {
				symbol->declaration = declaration->symbol_next;
			} else {
				symbol->declaration = old_declaration;
			}
		} else {
			declaration_t *iter_last = declaration;
			declaration_t *iter      = declaration->symbol_next;
			for( ; iter != NULL; iter_last = iter, iter = iter->symbol_next) {
				/* replace an entry? */
				if (iter->namespc == namespc) {
					assert(iter_last != NULL);
					iter_last->symbol_next = old_declaration;
					if (old_declaration != NULL) {
						old_declaration->symbol_next = iter->symbol_next;
					}
					break;
				}
			}
			assert(iter != NULL);
		}
	}

	ARR_SHRINKLEN(*stack_ptr, (int) new_top);
}

static void environment_pop_to(size_t new_top)
{
	stack_pop_to(&environment_stack, new_top);
}

/**
 * Pop all entries on the label stack until the new_top
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
 * Check if a given expression represents the 0 pointer constant.
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
		type_t *points_to_left
			= skip_typeref(type_left->pointer.points_to);
		type_t *points_to_right
			= skip_typeref(type_right->pointer.points_to);

		/* the left type has all qualifiers from the right type */
		unsigned missing_qualifiers
			= points_to_right->base.qualifiers & ~points_to_left->base.qualifiers;
		warningf(source_position,
		         "destination type '%T' in %s from type '%T' lacks qualifiers '%Q' in pointer target type",
		         orig_type_left, context, orig_type_right, missing_qualifiers);
		return;
	}

	case ASSIGN_WARNING_POINTER_INCOMPATIBLE:
		warningf(source_position,
		         "destination type '%T' in %s is incompatible with '%E' of type '%T'",
			 	 orig_type_left, context, right, orig_type_right);
		return;

	case ASSIGN_WARNING_POINTER_FROM_INT:
		warningf(source_position,
		         "%s makes integer '%T' from pointer '%T' without a cast",
				 context, orig_type_left, orig_type_right);
		return;

	case ASSIGN_WARNING_INT_FROM_POINTER:
		warningf(source_position,
				"%s makes integer '%T' from pointer '%T' without a cast",
				context, orig_type_left, orig_type_right);
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

			if (is_type_atomic(points_to_left, ATOMIC_TYPE_VOID) ||
					is_type_atomic(points_to_right, ATOMIC_TYPE_VOID)) {
				return res;
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
	/* start parsing at precedence 7 (conditional expression) */
	expression_t *result = parse_sub_expression(7);

	if (!is_constant_expression(result)) {
		errorf(&result->base.source_position,
		       "expression '%E' is not constant\n", result);
	}

	return result;
}

static expression_t *parse_assignment_expression(void)
{
	/* start parsing at precedence 2 (assignment expression) */
	return parse_sub_expression(2);
}

static type_t *make_global_typedef(const char *name, type_t *type)
{
	symbol_t *const symbol       = symbol_table_insert(name);

	declaration_t *const declaration = allocate_declaration_zero();
	declaration->namespc                = NAMESPACE_NORMAL;
	declaration->storage_class          = STORAGE_CLASS_TYPEDEF;
	declaration->declared_storage_class = STORAGE_CLASS_TYPEDEF;
	declaration->type                   = type;
	declaration->symbol                 = symbol;
	declaration->source_position        = builtin_source_position;
	declaration->implicit               = true;

	record_declaration(declaration);

	type_t *typedef_type               = allocate_type_zero(TYPE_TYPEDEF, &builtin_source_position);
	typedef_type->typedeft.declaration = declaration;

	return typedef_type;
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
 * Allocate a new gnu temporal attribute.
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
 * parse one constant expression argument.
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
 * parse a list of constant expressions arguments.
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
 * parse one string literal argument.
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
 * parse one tls model.
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
		for(size_t i = 0; i < 4; ++i) {
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
 * parse one tls model.
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
		for(size_t i = 0; i < 4; ++i) {
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
 * parse one (code) model.
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
		for(int i = 0; i < 3; ++i) {
			if (strcmp(visibilities[i], string.begin) == 0) {
				attribute->u.value = i;
				return;
			}
		}
		errorf(HERE, "'%s' is an unrecognized model", string.begin);
	}
	attribute->invalid = true;
}

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
 * parse one interrupt argument.
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
		for(size_t i = 0; i < 5; ++i) {
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
 * parse ( identifier, const expression, const expression )
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
	for(i = 0; i < 4; ++i) {
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
	rem_anchor_token('(');

	expect(',');
	add_anchor_token(')');
	parse_constant_expression();
	rem_anchor_token('(');
	expect(')');
	return;
end_error:
	attribute->u.value = true;
}

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
			for(i = 0; i < GNU_AK_LAST; ++i) {
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

				switch(kind) {
				case GNU_AK_CONST:
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
		switch(token.type) {
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
			warningf(HERE, "Ignoring declaration modifier %K", &token);
			break;

end_error:
		default: return modifiers;
		}

		next_token();
	}
}

static designator_t *parse_designation(void)
{
	designator_t *result = NULL;
	designator_t *last   = NULL;

	while (true) {
		designator_t *designator;
		switch(token.type) {
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
		warningf(HERE, "extra curly braces around scalar initializer");
		do {
			++braces;
			next_token();
		} while (token.type == '{');
	}

	expression_t *expression = parse_assignment_expression();
	if (must_be_constant && !is_initializer_constant(expression)) {
		errorf(&expression->base.source_position,
		       "Initialisation expression '%E' is not constant\n",
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
			if (!additional_warning_displayed) {
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

	for(size_t i = 0; i < len; ++i) {
		const type_path_entry_t *entry = & path->path[i];

		type_t *type = skip_typeref(entry->type);
		if (is_type_compound(type)) {
			/* in gcc mode structs can have no members */
			if (entry->v.compound_entry == NULL) {
				assert(i == len-1);
				continue;
			}
			fprintf(stderr, ".%s", entry->v.compound_entry->symbol->string);
		} else if (is_type_array(type)) {
			fprintf(stderr, "[%zu]", entry->v.index);
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
 * Descending into a sub-type. Enter the scope of the current
 * top_type.
 */
static void descend_into_subtype(type_path_t *path)
{
	type_t *orig_top_type = path->top_type;
	type_t *top_type      = skip_typeref(orig_top_type);

	assert(is_type_compound(top_type) || is_type_array(top_type));

	type_path_entry_t *top = append_to_type_path(path);
	top->type              = top_type;

	if (is_type_compound(top_type)) {
		declaration_t *declaration = top_type->compound.declaration;
		declaration_t *entry       = declaration->scope.declarations;
		top->v.compound_entry      = entry;

		if (entry != NULL) {
			path->top_type         = entry->type;
		} else {
			path->top_type         = NULL;
		}
	} else {
		assert(is_type_array(top_type));

		top->v.index   = 0;
		path->top_type = top_type->array.element_type;
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
	for( ; designator != NULL; designator = designator->next) {
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
				goto failed;
			}

			declaration_t *declaration = type->compound.declaration;
			declaration_t *iter        = declaration->scope.declarations;
			for( ; iter != NULL; iter = iter->next) {
				if (iter->symbol == symbol) {
					break;
				}
			}
			if (iter == NULL) {
				errorf(&designator->source_position,
				       "'%T' has no member named '%Y'", orig_type, symbol);
				goto failed;
			}
			if (used_in_offsetof) {
				type_t *real_type = skip_typeref(iter->type);
				if (real_type->kind == TYPE_BITFIELD) {
					errorf(&designator->source_position,
					       "offsetof designator '%Y' may not specify bitfield",
					       symbol);
					goto failed;
				}
			}

			top->type             = orig_type;
			top->v.compound_entry = iter;
			orig_type             = iter->type;
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
			if (!is_type_valid(array_index->base.type)) {
				goto failed;
			}

			long index = fold_constant(array_index);
			if (!used_in_offsetof) {
				if (index < 0) {
					errorf(&designator->source_position,
					       "array index [%E] must be positive", array_index);
					goto failed;
				}
				if (type->array.size_constant == true) {
					long array_size = type->array.size;
					if (index >= array_size) {
						errorf(&designator->source_position,
		 				       "designator [%E] (%d) exceeds array size %d",
			 			       array_index, index, array_size);
						goto failed;
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

		entry                 = entry->next;
		top->v.compound_entry = entry;
		if (entry != NULL) {
			path->top_type = entry->type;
			return;
		}
	} else {
		assert(is_type_array(type));

		top->v.index++;

		if (!type->array.size_constant || top->v.index < type->array.size) {
			return;
		}
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

		/* we can't do usefull stuff if we didn't even parse the type. Skip the
		 * initializers in this case. */
		if (!is_type_valid(type)) {
			skip_initializers();
			return create_empty_initializer();
		}
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
					if (env->declaration != NULL) {
						errorf(HERE, "extra brace group at end of initializer for '%Y'",
						       env->declaration->symbol);
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
				       "Initialisation expression '%E' is not constant\n",
				       expression);
			}

			if (type == NULL) {
				/* we are already outside, ... */
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
					if (token.type != '}') {
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
			if (env->declaration != NULL)
				warningf(HERE, "excess elements in struct initializer for '%Y'",
			         env->declaration->symbol);
			else
				warningf(HERE, "excess elements in struct initializer");
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

	/* § 6.7.5 (22)  array initializers for arrays with unknown size determine
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

		new_type->array.size_expression = cnst;
		new_type->array.size_constant   = true;
		new_type->array.size            = size;
		env->type = new_type;
	}

	return result;
end_error:
	return NULL;
}

static declaration_t *append_declaration(declaration_t *declaration);

static declaration_t *parse_compound_type_specifier(bool is_struct)
{
	gnu_attribute_t  *attributes = NULL;
	decl_modifiers_t  modifiers  = 0;
	if (is_struct) {
		eat(T_struct);
	} else {
		eat(T_union);
	}

	symbol_t      *symbol      = NULL;
	declaration_t *declaration = NULL;

	if (token.type == T___attribute__) {
		modifiers |= parse_attributes(&attributes);
	}

	if (token.type == T_IDENTIFIER) {
		symbol = token.v.symbol;
		next_token();

		namespace_t const namespc =
			is_struct ? NAMESPACE_STRUCT : NAMESPACE_UNION;
		declaration = get_declaration(symbol, namespc);
		if (declaration != NULL) {
			if (declaration->parent_scope != scope &&
			    (token.type == '{' || token.type == ';')) {
				declaration = NULL;
			} else if (declaration->init.complete &&
			           token.type == '{') {
				assert(symbol != NULL);
				errorf(HERE, "multiple definitions of '%s %Y' (previous definition at %P)",
				       is_struct ? "struct" : "union", symbol,
				       &declaration->source_position);
				declaration->scope.declarations = NULL;
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

	if (declaration == NULL) {
		declaration = allocate_declaration_zero();
		declaration->namespc         =
			(is_struct ? NAMESPACE_STRUCT : NAMESPACE_UNION);
		declaration->source_position = token.source_position;
		declaration->symbol          = symbol;
		declaration->parent_scope    = scope;
		if (symbol != NULL) {
			environment_push(declaration);
		}
		append_declaration(declaration);
	}

	if (token.type == '{') {
		declaration->init.complete = true;

		parse_compound_type_entries(declaration);
		modifiers |= parse_attributes(&attributes);
	}

	declaration->modifiers |= modifiers;
	return declaration;
}

static void parse_enum_entries(type_t *const enum_type)
{
	eat('{');

	if (token.type == '}') {
		next_token();
		errorf(HERE, "empty enum not allowed");
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

		declaration_t *const entry = allocate_declaration_zero();
		entry->storage_class   = STORAGE_CLASS_ENUM_ENTRY;
		entry->type            = enum_type;
		entry->symbol          = token.v.symbol;
		entry->source_position = token.source_position;
		next_token();

		if (token.type == '=') {
			next_token();
			expression_t *value = parse_constant_expression();

			value = create_implicit_cast(value, enum_type);
			entry->init.enum_value = value;

			/* TODO semantic */
		}

		record_declaration(entry);

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
	declaration_t   *declaration;
	symbol_t        *symbol;

	eat(T_enum);
	if (token.type == T_IDENTIFIER) {
		symbol = token.v.symbol;
		next_token();

		declaration = get_declaration(symbol, NAMESPACE_ENUM);
	} else if (token.type != '{') {
		parse_error_expected("while parsing enum type specifier",
		                     T_IDENTIFIER, '{', NULL);
		return NULL;
	} else {
		declaration = NULL;
		symbol      = NULL;
	}

	if (declaration == NULL) {
		declaration = allocate_declaration_zero();
		declaration->namespc         = NAMESPACE_ENUM;
		declaration->source_position = token.source_position;
		declaration->symbol          = symbol;
		declaration->parent_scope  = scope;
	}

	type_t *const type      = allocate_type_zero(TYPE_ENUM, &declaration->source_position);
	type->enumt.declaration = declaration;

	if (token.type == '{') {
		if (declaration->init.complete) {
			errorf(HERE, "multiple definitions of enum %Y", symbol);
		}
		if (symbol != NULL) {
			environment_push(declaration);
		}
		append_declaration(declaration);
		declaration->init.complete = true;

		parse_enum_entries(type);
		parse_attributes(&attributes);
	}

	return type;
}

/**
 * if a symbol is a typedef to another type, return true
 */
static bool is_typedef_symbol(symbol_t *symbol)
{
	const declaration_t *const declaration =
		get_declaration(symbol, NAMESPACE_NORMAL);
	return
		declaration != NULL &&
		declaration->storage_class == STORAGE_CLASS_TYPEDEF;
}

static type_t *parse_typeof(void)
{
	eat(T___typeof__);

	type_t *type;

	expect('(');
	add_anchor_token(')');

	expression_t *expression  = NULL;

restart:
	switch(token.type) {
	case T___extension__:
		/* This can be a prefix to a typename or an expression.  We simply eat
		 * it now. */
		do {
			next_token();
		} while (token.type == T___extension__);
		goto restart;

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

	rem_anchor_token(')');
	expect(')');

	type_t *typeof_type              = allocate_type_zero(TYPE_TYPEOF, &expression->base.source_position);
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
	type_t *type            = allocate_type_zero(TYPE_BUILTIN, &builtin_source_position);
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
	declaration_t *declaration = get_declaration(symbol, NAMESPACE_NORMAL);
	if (declaration == NULL ||
	   declaration->storage_class != STORAGE_CLASS_TYPEDEF)
		return NULL;

	type_t *type               = allocate_type_zero(TYPE_TYPEDEF, &declaration->source_position);
	type->typedeft.declaration = declaration;

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
	for(unsigned i = 1; i <= 8192; i += i) {
		if (i == v)
			return true;
	}
	errorf(HERE, "alignment must be power of two");
	return false;
}

#define DET_MOD(name, tag) do { \
	if (*modifiers & tag) warningf(HERE, #name " used more than once"); \
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
				if (specifiers->alignment != 0)
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
			for(;;) {
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
			if (specifiers->deprecated != 0)
				warningf(HERE, "deprecated used more than once");
			specifiers->deprecated = 1;
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
			warningf(HERE, "Unknown modifier %Y ignored", token.v.symbol);
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

static declaration_t *create_error_declaration(symbol_t *symbol, storage_class_tag_t storage_class)
{
	declaration_t *const decl    = allocate_declaration_zero();
	decl->source_position        = *HERE;
	decl->declared_storage_class = storage_class;
	decl->storage_class          =
		storage_class != STORAGE_CLASS_NONE || scope == global_scope ?
			storage_class : STORAGE_CLASS_AUTO;
	decl->symbol                 = symbol;
	decl->implicit               = true;
	record_declaration(decl);
	return decl;
}

static void parse_declaration_specifiers(declaration_specifiers_t *specifiers)
{
	type_t            *type            = NULL;
	type_qualifiers_t  qualifiers      = TYPE_QUALIFIER_NONE;
	type_modifiers_t   modifiers       = TYPE_MODIFIER_NONE;
	unsigned           type_specifiers = 0;
	bool               newtype         = false;
	bool               saw_error       = false;

	specifiers->source_position = token.source_position;

	while (true) {
		specifiers->modifiers
			|= parse_attributes(&specifiers->gnu_attributes);
		if (specifiers->modifiers & DM_TRANSPARENT_UNION)
			modifiers |= TYPE_MODIFIER_TRANSPARENT_UNION;

		switch(token.type) {

		/* storage class */
#define MATCH_STORAGE_CLASS(token, class)                                  \
		case token:                                                        \
			if (specifiers->declared_storage_class != STORAGE_CLASS_NONE) { \
				errorf(HERE, "multiple storage classes in declaration specifiers"); \
			}                                                              \
			specifiers->declared_storage_class = class;                    \
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
			switch (specifiers->declared_storage_class) {
			case STORAGE_CLASS_NONE:
				specifiers->declared_storage_class = STORAGE_CLASS_THREAD;
				break;

			case STORAGE_CLASS_EXTERN:
				specifiers->declared_storage_class = STORAGE_CLASS_THREAD_EXTERN;
				break;

			case STORAGE_CLASS_STATIC:
				specifiers->declared_storage_class = STORAGE_CLASS_THREAD_STATIC;
				break;

			default:
				errorf(HERE, "multiple storage classes in declaration specifiers");
				break;
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
			/* TODO */
			next_token();
			break;

		/* type specifiers */
#define MATCH_SPECIFIER(token, specifier, name)                         \
		case token:                                                     \
			next_token();                                               \
			if (type_specifiers & specifier) {                           \
				errorf(HERE, "multiple " name " type specifiers given"); \
			} else {                                                    \
				type_specifiers |= specifier;                           \
			}                                                           \
			break

		MATCH_SPECIFIER(T_void,       SPECIFIER_VOID,      "void");
		MATCH_SPECIFIER(T_char,       SPECIFIER_CHAR,      "char");
		MATCH_SPECIFIER(T_short,      SPECIFIER_SHORT,     "short");
		MATCH_SPECIFIER(T_int,        SPECIFIER_INT,       "int");
		MATCH_SPECIFIER(T_float,      SPECIFIER_FLOAT,     "float");
		MATCH_SPECIFIER(T_double,     SPECIFIER_DOUBLE,    "double");
		MATCH_SPECIFIER(T_signed,     SPECIFIER_SIGNED,    "signed");
		MATCH_SPECIFIER(T_unsigned,   SPECIFIER_UNSIGNED,  "unsigned");
		MATCH_SPECIFIER(T__Bool,      SPECIFIER_BOOL,      "_Bool");
		MATCH_SPECIFIER(T__int8,      SPECIFIER_INT8,      "_int8");
		MATCH_SPECIFIER(T__int16,     SPECIFIER_INT16,     "_int16");
		MATCH_SPECIFIER(T__int32,     SPECIFIER_INT32,     "_int32");
		MATCH_SPECIFIER(T__int64,     SPECIFIER_INT64,     "_int64");
		MATCH_SPECIFIER(T__int128,    SPECIFIER_INT128,    "_int128");
		MATCH_SPECIFIER(T__Complex,   SPECIFIER_COMPLEX,   "_Complex");
		MATCH_SPECIFIER(T__Imaginary, SPECIFIER_IMAGINARY, "_Imaginary");

		case T__forceinline:
			/* only in microsoft mode */
			specifiers->modifiers |= DM_FORCEINLINE;
			/* FALLTHROUGH */

		case T_inline:
			next_token();
			specifiers->is_inline = true;
			break;

		case T_long:
			next_token();
			if (type_specifiers & SPECIFIER_LONG_LONG) {
				errorf(HERE, "multiple type specifiers given");
			} else if (type_specifiers & SPECIFIER_LONG) {
				type_specifiers |= SPECIFIER_LONG_LONG;
			} else {
				type_specifiers |= SPECIFIER_LONG;
			}
			break;

		case T_struct: {
			type = allocate_type_zero(TYPE_COMPOUND_STRUCT, HERE);

			type->compound.declaration = parse_compound_type_specifier(true);
			break;
		}
		case T_union: {
			type = allocate_type_zero(TYPE_COMPOUND_UNION, HERE);
			type->compound.declaration = parse_compound_type_specifier(false);
			if (type->compound.declaration->modifiers & DM_TRANSPARENT_UNION)
				modifiers |= TYPE_MODIFIER_TRANSPARENT_UNION;
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
					case '*': {
						errorf(HERE, "%K does not name a type", &token);

						declaration_t *const decl =
							create_error_declaration(token.v.symbol, STORAGE_CLASS_TYPEDEF);

						type = allocate_type_zero(TYPE_TYPEDEF, HERE);
						type->typedeft.declaration = decl;

						next_token();
						saw_error = true;
						if (la1_type == '*')
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
	if (type == NULL || (saw_error && type_specifiers != 0)) {
		atomic_type_kind_t atomic_type;

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
				if (saw_error) {
					specifiers->type = type_error_type;
					return;
				}

				if (!strict_mode) {
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
			atomic_type = ATOMIC_TYPE_INVALID;
		}

		if (type_specifiers & SPECIFIER_COMPLEX &&
		   atomic_type != ATOMIC_TYPE_INVALID) {
			type                = allocate_type_zero(TYPE_COMPLEX, &builtin_source_position);
			type->complex.akind = atomic_type;
		} else if (type_specifiers & SPECIFIER_IMAGINARY &&
		          atomic_type != ATOMIC_TYPE_INVALID) {
			type                  = allocate_type_zero(TYPE_IMAGINARY, &builtin_source_position);
			type->imaginary.akind = atomic_type;
		} else {
			type               = allocate_type_zero(TYPE_ATOMIC, &builtin_source_position);
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
end_error:
	return;
}

static type_qualifiers_t parse_type_qualifiers(void)
{
	type_qualifiers_t qualifiers = TYPE_QUALIFIER_NONE;

	while (true) {
		switch(token.type) {
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

static declaration_t *parse_identifier_list(void)
{
	declaration_t *declarations     = NULL;
	declaration_t *last_declaration = NULL;
	do {
		declaration_t *const declaration = allocate_declaration_zero();
		declaration->type            = NULL; /* a K&R parameter list has no types, yet */
		declaration->source_position = token.source_position;
		declaration->symbol          = token.v.symbol;
		next_token();

		if (last_declaration != NULL) {
			last_declaration->next = declaration;
		} else {
			declarations = declaration;
		}
		last_declaration = declaration;

		if (token.type != ',') {
			break;
		}
		next_token();
	} while (token.type == T_IDENTIFIER);

	return declarations;
}

static type_t *automatic_type_conversion(type_t *orig_type);

static void semantic_parameter(declaration_t *declaration)
{
	/* TODO: improve error messages */
	source_position_t const* const pos = &declaration->source_position;

	switch (declaration->declared_storage_class) {
		case STORAGE_CLASS_TYPEDEF:
			errorf(pos, "typedef not allowed in parameter list");
			break;

		/* Allowed storage classes */
		case STORAGE_CLASS_NONE:
		case STORAGE_CLASS_REGISTER:
			break;

		default:
			errorf(pos, "parameter may only have none or register storage class");
			break;
	}

	type_t *const orig_type = declaration->type;
	/* §6.7.5.3(7): Array as last part of a parameter type is just syntactic
	 * sugar.  Turn it into a pointer.
	 * §6.7.5.3(8): A declaration of a parameter as ``function returning type''
	 * shall be adjusted to ``pointer to function returning type'', as in 6.3.2.1.
	 */
	type_t *const type = automatic_type_conversion(orig_type);
	declaration->type = type;

	if (is_type_incomplete(skip_typeref(type))) {
		errorf(pos, "incomplete type '%T' not allowed for parameter '%Y'",
		       orig_type, declaration->symbol);
	}
}

static declaration_t *parse_parameter(void)
{
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));

	parse_declaration_specifiers(&specifiers);

	declaration_t *declaration = parse_declarator(&specifiers, /*may_be_abstract=*/true);

	return declaration;
}

static declaration_t *parse_parameters(function_type_t *type)
{
	declaration_t *declarations = NULL;

	eat('(');
	add_anchor_token(')');
	int saved_comma_state = save_and_reset_anchor_state(',');

	if (token.type == T_IDENTIFIER &&
	    !is_typedef_symbol(token.v.symbol)) {
		token_type_t la1_type = look_ahead(1)->type;
		if (la1_type == ',' || la1_type == ')') {
			type->kr_style_parameters = true;
			declarations = parse_identifier_list();
			goto parameters_finished;
		}
	}

	if (token.type == ')') {
		type->unspecified_parameters = 1;
		goto parameters_finished;
	}

	declaration_t        *declaration;
	declaration_t        *last_declaration = NULL;
	function_parameter_t *parameter;
	function_parameter_t *last_parameter = NULL;

	while (true) {
		switch(token.type) {
		case T_DOTDOTDOT:
			next_token();
			type->variadic = 1;
			goto parameters_finished;

		case T_IDENTIFIER:
		case T___extension__:
		DECLARATION_START
			declaration = parse_parameter();

			/* func(void) is not a parameter */
			if (last_parameter == NULL
					&& token.type == ')'
					&& declaration->symbol == NULL
					&& skip_typeref(declaration->type) == type_void) {
				goto parameters_finished;
			}
			semantic_parameter(declaration);

			parameter       = obstack_alloc(type_obst, sizeof(parameter[0]));
			memset(parameter, 0, sizeof(parameter[0]));
			parameter->type = declaration->type;

			if (last_parameter != NULL) {
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

	restore_anchor_state(',', saved_comma_state);
	return declarations;

end_error:
	restore_anchor_state(',', saved_comma_state);
	return NULL;
}

typedef enum construct_type_kind_t {
	CONSTRUCT_INVALID,
	CONSTRUCT_POINTER,
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

static construct_type_t *parse_pointer_declarator(void)
{
	eat('*');

	parsed_pointer_t *pointer = obstack_alloc(&temp_obst, sizeof(pointer[0]));
	memset(pointer, 0, sizeof(pointer[0]));
	pointer->construct_type.kind = CONSTRUCT_POINTER;
	pointer->type_qualifiers     = parse_type_qualifiers();

	return (construct_type_t*) pointer;
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

	return (construct_type_t*) array;
end_error:
	return NULL;
}

static construct_type_t *parse_function_declarator(declaration_t *declaration)
{
	type_t *type;
	if (declaration != NULL) {
		type = allocate_type_zero(TYPE_FUNCTION, &declaration->source_position);

		unsigned mask = declaration->modifiers & (DM_CDECL|DM_STDCALL|DM_FASTCALL|DM_THISCALL);

		if (mask & (mask-1)) {
			const char *first = NULL, *second = NULL;

			/* more than one calling convention set */
			if (declaration->modifiers & DM_CDECL) {
				if (first == NULL)       first = "cdecl";
				else if (second == NULL) second = "cdecl";
			}
			if (declaration->modifiers & DM_STDCALL) {
				if (first == NULL)       first = "stdcall";
				else if (second == NULL) second = "stdcall";
			}
			if (declaration->modifiers & DM_FASTCALL) {
				if (first == NULL)       first = "fastcall";
				else if (second == NULL) second = "fastcall";
			}
			if (declaration->modifiers & DM_THISCALL) {
				if (first == NULL)       first = "thiscall";
				else if (second == NULL) second = "thiscall";
			}
			errorf(&declaration->source_position, "%s and %s attributes are not compatible", first, second);
		}

		if (declaration->modifiers & DM_CDECL)
			type->function.calling_convention = CC_CDECL;
		else if (declaration->modifiers & DM_STDCALL)
			type->function.calling_convention = CC_STDCALL;
		else if (declaration->modifiers & DM_FASTCALL)
			type->function.calling_convention = CC_FASTCALL;
		else if (declaration->modifiers & DM_THISCALL)
			type->function.calling_convention = CC_THISCALL;
	} else {
		type = allocate_type_zero(TYPE_FUNCTION, HERE);
	}

	declaration_t *parameters = parse_parameters(&type->function);
	if (declaration != NULL) {
		declaration->scope.declarations = parameters;
	}

	construct_function_type_t *construct_function_type =
		obstack_alloc(&temp_obst, sizeof(construct_function_type[0]));
	memset(construct_function_type, 0, sizeof(construct_function_type[0]));
	construct_function_type->construct_type.kind = CONSTRUCT_FUNCTION;
	construct_function_type->function_type       = type;

	return &construct_function_type->construct_type;
}

static void fix_declaration_type(declaration_t *declaration)
{
	decl_modifiers_t declaration_modifiers = declaration->modifiers;
	type_modifiers_t type_modifiers        = declaration->type->base.modifiers;

	if (declaration_modifiers & DM_TRANSPARENT_UNION)
		type_modifiers |= TYPE_MODIFIER_TRANSPARENT_UNION;

	if (declaration->type->base.modifiers == type_modifiers)
		return;

	type_t *copy = duplicate_type(declaration->type);
	copy->base.modifiers = type_modifiers;

	type_t *result = typehash_insert(copy);
	if (result != copy) {
		obstack_free(type_obst, copy);
	}

	declaration->type = result;
}

static construct_type_t *parse_inner_declarator(declaration_t *declaration,
		bool may_be_abstract)
{
	/* construct a single linked list of construct_type_t's which describe
	 * how to construct the final declarator type */
	construct_type_t *first = NULL;
	construct_type_t *last  = NULL;
	gnu_attribute_t  *attributes = NULL;

	decl_modifiers_t modifiers = parse_attributes(&attributes);

	/* pointers */
	while (token.type == '*') {
		construct_type_t *type = parse_pointer_declarator();

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

	if (declaration != NULL)
		declaration->modifiers |= modifiers;

	construct_type_t *inner_types = NULL;

	switch(token.type) {
	case T_IDENTIFIER:
		if (declaration == NULL) {
			errorf(HERE, "no identifier expected in typename");
		} else {
			declaration->symbol          = token.v.symbol;
			declaration->source_position = token.source_position;
		}
		next_token();
		break;
	case '(':
		next_token();
		add_anchor_token(')');
		inner_types = parse_inner_declarator(declaration, may_be_abstract);
		/* All later declarators only modify the return type, not declaration */
		declaration = NULL;
		rem_anchor_token(')');
		expect(')');
		break;
	default:
		if (may_be_abstract)
			break;
		parse_error_expected("while parsing declarator", T_IDENTIFIER, '(', NULL);
		/* avoid a loop in the outermost scope, because eat_statement doesn't
		 * eat '}' */
		if (token.type == '}' && current_function == NULL) {
			next_token();
		} else {
			eat_statement();
		}
		return NULL;
	}

	construct_type_t *p = last;

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

static void parse_declaration_attributes(declaration_t *declaration)
{
	gnu_attribute_t  *attributes = NULL;
	decl_modifiers_t  modifiers  = parse_attributes(&attributes);

	if (declaration == NULL)
		return;

	declaration->modifiers |= modifiers;
	/* check if we have these stupid mode attributes... */
	type_t *old_type = declaration->type;
	if (old_type == NULL)
		return;

	gnu_attribute_t *attribute = attributes;
	for ( ; attribute != NULL; attribute = attribute->next) {
		if (attribute->kind != GNU_AK_MODE || attribute->invalid)
			continue;

		atomic_type_kind_t  akind = attribute->u.akind;
		if (!is_type_signed(old_type)) {
			switch(akind) {
			case ATOMIC_TYPE_CHAR: akind = ATOMIC_TYPE_UCHAR; break;
			case ATOMIC_TYPE_SHORT: akind = ATOMIC_TYPE_USHORT; break;
			case ATOMIC_TYPE_INT: akind = ATOMIC_TYPE_UINT; break;
			case ATOMIC_TYPE_LONGLONG: akind = ATOMIC_TYPE_ULONGLONG; break;
			default:
				panic("invalid akind in mode attribute");
			}
		}
		declaration->type
			= make_atomic_type(akind, old_type->base.qualifiers);
	}
}

static type_t *construct_declarator_type(construct_type_t *construct_list,
                                         type_t *type)
{
	construct_type_t *iter = construct_list;
	for( ; iter != NULL; iter = iter->next) {
		switch(iter->kind) {
		case CONSTRUCT_INVALID:
			internal_errorf(HERE, "invalid type construction found");
		case CONSTRUCT_FUNCTION: {
			construct_function_type_t *construct_function_type
				= (construct_function_type_t*) iter;

			type_t *function_type = construct_function_type->function_type;

			function_type->function.return_type = type;

			type_t *skipped_return_type = skip_typeref(type);
			if (is_type_function(skipped_return_type)) {
				errorf(HERE, "function returning function is not allowed");
				type = type_error_type;
			} else if (is_type_array(skipped_return_type)) {
				errorf(HERE, "function returning array is not allowed");
				type = type_error_type;
			} else {
				type = function_type;
			}
			break;
		}

		case CONSTRUCT_POINTER: {
			parsed_pointer_t *parsed_pointer = (parsed_pointer_t*) iter;
			type_t           *pointer_type   = allocate_type_zero(TYPE_POINTER, &null_position);
			pointer_type->pointer.points_to  = type;
			pointer_type->base.qualifiers    = parsed_pointer->type_qualifiers;

			type = pointer_type;
			break;
		}

		case CONSTRUCT_ARRAY: {
			parsed_array_t *parsed_array  = (parsed_array_t*) iter;
			type_t         *array_type    = allocate_type_zero(TYPE_ARRAY, &null_position);

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
			if (is_type_atomic(skipped_type, ATOMIC_TYPE_VOID)) {
				errorf(HERE, "array of void is not allowed");
				type = type_error_type;
			} else {
				type = array_type;
			}
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

static declaration_t *parse_declarator(
		const declaration_specifiers_t *specifiers, bool may_be_abstract)
{
	declaration_t *const declaration    = allocate_declaration_zero();
	declaration->source_position        = specifiers->source_position;
	declaration->declared_storage_class = specifiers->declared_storage_class;
	declaration->modifiers              = specifiers->modifiers;
	declaration->deprecated_string      = specifiers->deprecated_string;
	declaration->get_property_sym       = specifiers->get_property_sym;
	declaration->put_property_sym       = specifiers->put_property_sym;
	declaration->is_inline              = specifiers->is_inline;

	declaration->storage_class          = specifiers->declared_storage_class;
	if (declaration->storage_class == STORAGE_CLASS_NONE
			&& scope != global_scope) {
		declaration->storage_class = STORAGE_CLASS_AUTO;
	}

	if (specifiers->alignment != 0) {
		/* TODO: add checks here */
		declaration->alignment = specifiers->alignment;
	}

	construct_type_t *construct_type
		= parse_inner_declarator(declaration, may_be_abstract);
	type_t *const type = specifiers->type;
	declaration->type = construct_declarator_type(construct_type, type);

	parse_declaration_attributes(declaration);

	fix_declaration_type(declaration);

	if (construct_type != NULL) {
		obstack_free(&temp_obst, construct_type);
	}

	return declaration;
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

static declaration_t *append_declaration(declaration_t* const declaration)
{
	if (last_declaration != NULL) {
		last_declaration->next = declaration;
	} else {
		scope->declarations = declaration;
	}
	last_declaration = declaration;
	return declaration;
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
static void check_type_of_main(const declaration_t *const decl, const function_type_t *const func_type)
{
	if (decl->storage_class == STORAGE_CLASS_STATIC) {
		warningf(&decl->source_position,
		         "'main' is normally a non-static function");
	}
	if (skip_typeref(func_type->return_type) != type_int) {
		warningf(&decl->source_position,
		         "return type of 'main' should be 'int', but is '%T'",
		         func_type->return_type);
	}
	const function_parameter_t *parm = func_type->parameters;
	if (parm != NULL) {
		type_t *const first_type = parm->type;
		if (!types_compatible(skip_typeref(first_type), type_int)) {
			warningf(&decl->source_position,
			         "first argument of 'main' should be 'int', but is '%T'", first_type);
		}
		parm = parm->next;
		if (parm != NULL) {
			type_t *const second_type = parm->type;
			if (!types_compatible(skip_typeref(second_type), type_char_ptr_ptr)) {
				warningf(&decl->source_position,
				         "second argument of 'main' should be 'char**', but is '%T'", second_type);
			}
			parm = parm->next;
			if (parm != NULL) {
				type_t *const third_type = parm->type;
				if (!types_compatible(skip_typeref(third_type), type_char_ptr_ptr)) {
					warningf(&decl->source_position,
					         "third argument of 'main' should be 'char**', but is '%T'", third_type);
				}
				parm = parm->next;
				if (parm != NULL)
					goto warn_arg_count;
			}
		} else {
warn_arg_count:
			warningf(&decl->source_position, "'main' takes only zero, two or three arguments");
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

static declaration_t *internal_record_declaration(
	declaration_t *const declaration,
	const bool is_definition)
{
	const symbol_t *const symbol  = declaration->symbol;
	const namespace_t     namespc = (namespace_t)declaration->namespc;

	assert(symbol != NULL);
	declaration_t *previous_declaration = get_declaration(symbol, namespc);

	type_t *const orig_type = declaration->type;
	type_t *const type      = skip_typeref(orig_type);
	if (is_type_function(type) &&
			type->function.unspecified_parameters &&
			warning.strict_prototypes &&
			previous_declaration == NULL) {
		warningf(&declaration->source_position,
		         "function declaration '%#T' is not a prototype",
		         orig_type, declaration->symbol);
	}

	if (warning.main && is_type_function(type) && is_sym_main(symbol)) {
		check_type_of_main(declaration, &type->function);
	}

	if (warning.nested_externs                             &&
	    declaration->storage_class == STORAGE_CLASS_EXTERN &&
	    scope                      != global_scope) {
		warningf(&declaration->source_position,
		         "nested extern declaration of '%#T'", declaration->type, symbol);
	}

	assert(declaration != previous_declaration);
	if (previous_declaration != NULL
			&& previous_declaration->parent_scope == scope) {
		/* can happen for K&R style declarations */
		if (previous_declaration->type == NULL) {
			previous_declaration->type = declaration->type;
		}

		const type_t *prev_type = skip_typeref(previous_declaration->type);
		if (!types_compatible(type, prev_type)) {
			errorf(&declaration->source_position,
				   "declaration '%#T' is incompatible with '%#T' (declared %P)",
				   orig_type, symbol, previous_declaration->type, symbol,
				   &previous_declaration->source_position);
		} else {
			unsigned old_storage_class = previous_declaration->storage_class;
			if (old_storage_class == STORAGE_CLASS_ENUM_ENTRY) {
				errorf(&declaration->source_position,
					   "redeclaration of enum entry '%Y' (declared %P)",
					   symbol, &previous_declaration->source_position);
				return previous_declaration;
			}

			if (warning.redundant_decls                                     &&
			    is_definition                                               &&
			    previous_declaration->storage_class == STORAGE_CLASS_STATIC &&
			    !(previous_declaration->modifiers & DM_USED)                &&
			    !previous_declaration->used) {
				warningf(&previous_declaration->source_position,
				         "unnecessary static forward declaration for '%#T'",
				         previous_declaration->type, symbol);
			}

			unsigned new_storage_class = declaration->storage_class;

			if (is_type_incomplete(prev_type)) {
				previous_declaration->type = type;
				prev_type                  = type;
			}

			/* pretend no storage class means extern for function
			 * declarations (except if the previous declaration is neither
			 * none nor extern) */
			if (is_type_function(type)) {
				if (prev_type->function.unspecified_parameters) {
					previous_declaration->type = type;
					prev_type                  = type;
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
							warningf(&declaration->source_position,
									 "no previous prototype for '%#T'",
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
				if (!is_definition          &&
				    warning.redundant_decls &&
				    strcmp(previous_declaration->source_position.input_name, "<builtin>") != 0) {
					warningf(&declaration->source_position,
					         "redundant declaration for '%Y' (declared %P)",
					         symbol, &previous_declaration->source_position);
				}
			} else if (current_function == NULL) {
				if (old_storage_class != STORAGE_CLASS_STATIC &&
				    new_storage_class == STORAGE_CLASS_STATIC) {
					errorf(&declaration->source_position,
					       "static declaration of '%Y' follows non-static declaration (declared %P)",
					       symbol, &previous_declaration->source_position);
				} else if (old_storage_class == STORAGE_CLASS_EXTERN) {
					previous_declaration->storage_class          = STORAGE_CLASS_NONE;
					previous_declaration->declared_storage_class = STORAGE_CLASS_NONE;
				} else {
					goto warn_redundant_declaration;
				}
			} else if (old_storage_class == new_storage_class) {
				errorf(&declaration->source_position,
				       "redeclaration of '%Y' (declared %P)",
				       symbol, &previous_declaration->source_position);
			} else {
				errorf(&declaration->source_position,
				       "redeclaration of '%Y' with different linkage (declared %P)",
				       symbol, &previous_declaration->source_position);
			}
		}

		previous_declaration->modifiers |= declaration->modifiers;
		previous_declaration->is_inline |= declaration->is_inline;
		return previous_declaration;
	} else if (is_type_function(type)) {
		if (is_definition &&
		    declaration->storage_class != STORAGE_CLASS_STATIC) {
			if (warning.missing_prototypes && !is_sym_main(symbol)) {
				warningf(&declaration->source_position,
				         "no previous prototype for '%#T'", orig_type, symbol);
			} else if (warning.missing_declarations && !is_sym_main(symbol)) {
				warningf(&declaration->source_position,
				         "no previous declaration for '%#T'", orig_type,
				         symbol);
			}
		}
	} else {
		if (warning.missing_declarations &&
		    scope == global_scope && (
		      declaration->storage_class == STORAGE_CLASS_NONE ||
		      declaration->storage_class == STORAGE_CLASS_THREAD
		    )) {
			warningf(&declaration->source_position,
			         "no previous declaration for '%#T'", orig_type, symbol);
		}
	}

	assert(declaration->parent_scope == NULL);
	assert(scope != NULL);

	declaration->parent_scope = scope;

	environment_push(declaration);
	return append_declaration(declaration);
}

static declaration_t *record_declaration(declaration_t *declaration)
{
	return internal_record_declaration(declaration, false);
}

static declaration_t *record_definition(declaration_t *declaration)
{
	return internal_record_declaration(declaration, true);
}

static void parser_error_multiple_definition(declaration_t *declaration,
		const source_position_t *source_position)
{
	errorf(source_position, "multiple definition of symbol '%Y' (declared %P)",
	       declaration->symbol, &declaration->source_position);
}

static bool is_declaration_specifier(const token_t *token,
                                     bool only_specifiers_qualifiers)
{
	switch(token->type) {
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

static void parse_init_declarator_rest(declaration_t *declaration)
{
	eat('=');

	type_t *orig_type = declaration->type;
	type_t *type      = skip_typeref(orig_type);

	if (declaration->init.initializer != NULL) {
		parser_error_multiple_definition(declaration, HERE);
	}

	bool must_be_constant = false;
	if (declaration->storage_class == STORAGE_CLASS_STATIC
			|| declaration->storage_class == STORAGE_CLASS_THREAD_STATIC
			|| declaration->parent_scope == global_scope) {
		must_be_constant = true;
	}

	parse_initializer_env_t env;
	env.type             = orig_type;
	env.must_be_constant = must_be_constant;
	env.declaration      = current_init_decl = declaration;

	initializer_t *initializer = parse_initializer(&env);
	current_init_decl = NULL;

	if (env.type != orig_type) {
		orig_type         = env.type;
		type              = skip_typeref(orig_type);
		declaration->type = env.type;
	}

	if (is_type_function(type)) {
		errorf(&declaration->source_position,
		       "initializers not allowed for function types at declarator '%Y' (type '%T')",
		       declaration->symbol, orig_type);
	} else {
		declaration->init.initializer = initializer;
	}
}

/* parse rest of a declaration without any declarator */
static void parse_anonymous_declaration_rest(
		const declaration_specifiers_t *specifiers,
		parsed_declaration_func finished_declaration)
{
	eat(';');

	declaration_t *const declaration    = allocate_declaration_zero();
	declaration->type                   = specifiers->type;
	declaration->declared_storage_class = specifiers->declared_storage_class;
	declaration->source_position        = specifiers->source_position;
	declaration->modifiers              = specifiers->modifiers;

	if (declaration->declared_storage_class != STORAGE_CLASS_NONE) {
		warningf(&declaration->source_position,
		         "useless storage class in empty declaration");
	}
	declaration->storage_class = STORAGE_CLASS_NONE;

	type_t *type = declaration->type;
	switch (type->kind) {
		case TYPE_COMPOUND_STRUCT:
		case TYPE_COMPOUND_UNION: {
			if (type->compound.declaration->symbol == NULL) {
				warningf(&declaration->source_position,
				         "unnamed struct/union that defines no instances");
			}
			break;
		}

		case TYPE_ENUM:
			break;

		default:
			warningf(&declaration->source_position, "empty declaration");
			break;
	}

	finished_declaration(declaration);
}

static void parse_declaration_rest(declaration_t *ndeclaration,
		const declaration_specifiers_t *specifiers,
		parsed_declaration_func finished_declaration)
{
	add_anchor_token(';');
	add_anchor_token('=');
	add_anchor_token(',');
	while(true) {
		declaration_t *declaration = finished_declaration(ndeclaration);

		type_t *orig_type = declaration->type;
		type_t *type      = skip_typeref(orig_type);

		if (type->kind != TYPE_FUNCTION &&
		    declaration->is_inline &&
		    is_type_valid(type)) {
			warningf(&declaration->source_position,
			         "variable '%Y' declared 'inline'\n", declaration->symbol);
		}

		if (token.type == '=') {
			parse_init_declarator_rest(declaration);
		}

		if (token.type != ',')
			break;
		eat(',');

		ndeclaration = parse_declarator(specifiers, /*may_be_abstract=*/false);
	}
	expect(';');

end_error:
	rem_anchor_token(';');
	rem_anchor_token('=');
	rem_anchor_token(',');
}

static declaration_t *finished_kr_declaration(declaration_t *declaration)
{
	symbol_t *symbol  = declaration->symbol;
	if (symbol == NULL) {
		errorf(HERE, "anonymous declaration not valid as function parameter");
		return declaration;
	}
	namespace_t namespc = (namespace_t) declaration->namespc;
	if (namespc != NAMESPACE_NORMAL) {
		return record_declaration(declaration);
	}

	declaration_t *previous_declaration = get_declaration(symbol, namespc);
	if (previous_declaration == NULL ||
			previous_declaration->parent_scope != scope) {
		errorf(HERE, "expected declaration of a function parameter, found '%Y'",
		       symbol);
		return declaration;
	}

	if (previous_declaration->type == NULL) {
		previous_declaration->type          = declaration->type;
		previous_declaration->declared_storage_class = declaration->declared_storage_class;
		previous_declaration->storage_class = declaration->storage_class;
		previous_declaration->parent_scope  = scope;
		return previous_declaration;
	} else {
		return record_declaration(declaration);
	}
}

static void parse_declaration(parsed_declaration_func finished_declaration)
{
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));
	parse_declaration_specifiers(&specifiers);

	if (token.type == ';') {
		parse_anonymous_declaration_rest(&specifiers, append_declaration);
	} else {
		declaration_t *declaration = parse_declarator(&specifiers, /*may_be_abstract=*/false);
		parse_declaration_rest(declaration, &specifiers, finished_declaration);
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

static void parse_kr_declaration_list(declaration_t *declaration)
{
	type_t *type = skip_typeref(declaration->type);
	if (!is_type_function(type))
		return;

	if (!type->function.kr_style_parameters)
		return;

	/* push function parameters */
	int       top        = environment_top();
	scope_t  *last_scope = scope;
	set_scope(&declaration->scope);

	declaration_t *parameter = declaration->scope.declarations;
	for ( ; parameter != NULL; parameter = parameter->next) {
		assert(parameter->parent_scope == NULL);
		parameter->parent_scope = scope;
		environment_push(parameter);
	}

	/* parse declaration list */
	while (is_declaration_specifier(&token, false)) {
		parse_declaration(finished_kr_declaration);
	}

	/* pop function parameters */
	assert(scope == &declaration->scope);
	set_scope(last_scope);
	environment_pop_to(top);

	/* update function type */
	type_t *new_type = duplicate_type(type);

	function_parameter_t *parameters     = NULL;
	function_parameter_t *last_parameter = NULL;

	declaration_t *parameter_declaration = declaration->scope.declarations;
	for( ; parameter_declaration != NULL;
			parameter_declaration = parameter_declaration->next) {
		type_t *parameter_type = parameter_declaration->type;
		if (parameter_type == NULL) {
			if (strict_mode) {
				errorf(HERE, "no type specified for function parameter '%Y'",
				       parameter_declaration->symbol);
			} else {
				if (warning.implicit_int) {
					warningf(HERE, "no type specified for function parameter '%Y', using 'int'",
						parameter_declaration->symbol);
				}
				parameter_type              = type_int;
				parameter_declaration->type = parameter_type;
			}
		}

		semantic_parameter(parameter_declaration);
		parameter_type = parameter_declaration->type;

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

	declaration->type = type;
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
			current_function->source_position.input_name,
			current_function->symbol);
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
		declaration_t *label = goto_statement->label;

		label->used = true;
		if (label->source_position.input_name == NULL) {
			print_in_function();
			errorf(&goto_statement->base.source_position,
			       "label '%Y' used but not defined", label->symbol);
		 }
	}
	goto_first = goto_last = NULL;

	if (warning.unused_label) {
		for (const label_statement_t *label_statement = label_first;
			 label_statement != NULL;
			 label_statement = label_statement->next) {
			const declaration_t *label = label_statement->label;

			if (! label->used) {
				print_in_function();
				warningf(&label_statement->base.source_position,
					"label '%Y' defined but not used", label->symbol);
			}
		}
	}
	label_first = label_last = NULL;
}

/**
 * Check declarations of current_function for unused entities.
 */
static void check_declarations(void)
{
	if (warning.unused_parameter) {
		const scope_t *scope = &current_function->scope;

		if (is_sym_main(current_function->symbol)) {
			/* do not issue unused warnings for main */
			return;
		}
		const declaration_t *parameter = scope->declarations;
		for (; parameter != NULL; parameter = parameter->next) {
			if (! parameter->used) {
				print_in_function();
				warningf(&parameter->source_position,
				         "unused parameter '%Y'", parameter->symbol);
			}
		}
	}
	if (warning.unused_variable) {
	}
}

static int determine_truth(expression_t const* const cond)
{
	return
		!is_constant_expression(cond) ? 0 :
		fold_constant(cond) != 0      ? 1 :
		-1;
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
		case STATEMENT_DECLARATION:
		case STATEMENT_ASM:
			next = stmt->base.next;
			break;

		case STATEMENT_COMPOUND:
			next = stmt->compound.statements;
			break;

		case STATEMENT_RETURN:
			noreturn_candidate = false;
			return;

		case STATEMENT_IF: {
			if_statement_t const* const ifs = &stmt->ifs;
			int            const        val = determine_truth(ifs->condition);

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
			if (expr->kind == EXPR_CALL) {
				expression_t const *const func = expr->call.function;
				if (func->kind == EXPR_REFERENCE) {
					declaration_t const *const decl = func->reference.declaration;
					if (decl != NULL && decl->modifiers & DM_NORETURN) {
						return;
					}
				}
			}

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
			next = stmt->gotos.label->init.statement;
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
			int                      const val    = determine_truth(whiles->condition);

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
			int          const        val  =
				cond == NULL ? 1 : determine_truth(cond);

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

			type_t *const type = current_function->type;
			assert(is_type_function(type));
			type_t *const ret  = skip_typeref(type->function.return_type);
			if (warning.return_type                    &&
			    !is_type_atomic(ret, ATOMIC_TYPE_VOID) &&
			    is_type_valid(ret)                     &&
			    !is_sym_main(current_function->symbol)) {
				warningf(&stmt->base.source_position,
				         "control reaches end of non-void function");
			}
			return;
		}

		switch (next->kind) {
			case STATEMENT_INVALID:
			case STATEMENT_EMPTY:
			case STATEMENT_DECLARATION:
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
				int                      const val    = determine_truth(whiles->condition);

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

				do_while_statement_t const *const dw  = &next->do_while;
				int                  const        val = determine_truth(dw->condition);

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
				int          const        val  =
					cond == NULL ? 1 : determine_truth(cond);

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

	if (next == NULL) {
		next = stmt->base.parent;
		if (next == NULL) {
			warningf(&stmt->base.source_position,
			         "control reaches end of non-void function");
		}
	}

	check_reachable(next);
}

static void check_unreachable(statement_t const* const stmt)
{
	if (!stmt->base.reachable            &&
	    stmt->kind != STATEMENT_DO_WHILE &&
	    stmt->kind != STATEMENT_FOR      &&
	    (stmt->kind != STATEMENT_COMPOUND || stmt->compound.statements == NULL)) {
		warningf(&stmt->base.source_position, "statement is unreachable");
	}

	switch (stmt->kind) {
		case STATEMENT_INVALID:
		case STATEMENT_EMPTY:
		case STATEMENT_RETURN:
		case STATEMENT_DECLARATION:
		case STATEMENT_EXPRESSION:
		case STATEMENT_CONTINUE:
		case STATEMENT_BREAK:
		case STATEMENT_GOTO:
		case STATEMENT_ASM:
		case STATEMENT_LEAVE:
			break;

		case STATEMENT_COMPOUND:
			if (stmt->compound.statements)
				check_unreachable(stmt->compound.statements);
			break;

		case STATEMENT_IF:
			check_unreachable(stmt->ifs.true_statement);
			if (stmt->ifs.false_statement != NULL)
				check_unreachable(stmt->ifs.false_statement);
			break;

		case STATEMENT_SWITCH:
			check_unreachable(stmt->switchs.body);
			break;

		case STATEMENT_LABEL:
			check_unreachable(stmt->label.statement);
			break;

		case STATEMENT_CASE_LABEL:
			check_unreachable(stmt->case_label.statement);
			break;

		case STATEMENT_WHILE:
			check_unreachable(stmt->whiles.body);
			break;

		case STATEMENT_DO_WHILE:
			check_unreachable(stmt->do_while.body);
			if (!stmt->base.reachable) {
				expression_t const *const cond = stmt->do_while.condition;
				if (determine_truth(cond) >= 0) {
					warningf(&cond->base.source_position,
					         "condition of do-while-loop is unreachable");
				}
			}
			break;

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

			check_unreachable(fors->body);
			break;
		}

		case STATEMENT_MS_TRY: {
			ms_try_statement_t const *const ms_try = &stmt->ms_try;
			check_unreachable(ms_try->try_statement);
			check_unreachable(ms_try->final_statement);
		}
	}

	if (stmt->base.next)
		check_unreachable(stmt->base.next);
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
		parse_anonymous_declaration_rest(&specifiers, append_declaration);
		return;
	}

	add_anchor_token(',');
	add_anchor_token('=');
	rem_anchor_token(';');

	/* declarator is common to both function-definitions and declarations */
	declaration_t *ndeclaration = parse_declarator(&specifiers, /*may_be_abstract=*/false);

	rem_anchor_token(',');
	rem_anchor_token('=');
	rem_anchor_token(';');

	/* must be a declaration */
	switch (token.type) {
		case ',':
		case ';':
			parse_declaration_rest(ndeclaration, &specifiers, record_declaration);
			return;

		case '=':
			parse_declaration_rest(ndeclaration, &specifiers, record_definition);
			return;
	}

	/* must be a function definition */
	parse_kr_declaration_list(ndeclaration);

	if (token.type != '{') {
		parse_error_expected("while parsing function definition", '{', NULL);
		eat_until_matching_token(';');
		return;
	}

	type_t *type = ndeclaration->type;

	/* note that we don't skip typerefs: the standard doesn't allow them here
	 * (so we can't use is_type_function here) */
	if (type->kind != TYPE_FUNCTION) {
		if (is_type_valid(type)) {
			errorf(HERE, "declarator '%#T' has a body but is not a function type",
			       type, ndeclaration->symbol);
		}
		eat_block();
		return;
	}

	if (warning.aggregate_return &&
	    is_type_compound(skip_typeref(type->function.return_type))) {
		warningf(HERE, "function '%Y' returns an aggregate",
		         ndeclaration->symbol);
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
		ndeclaration->type = type;
	}

	declaration_t *const declaration = record_definition(ndeclaration);
	if (ndeclaration != declaration) {
		declaration->scope = ndeclaration->scope;
	}
	type = skip_typeref(declaration->type);

	/* push function parameters and switch scope */
	int       top        = environment_top();
	scope_t  *last_scope = scope;
	set_scope(&declaration->scope);

	declaration_t *parameter = declaration->scope.declarations;
	for( ; parameter != NULL; parameter = parameter->next) {
		if (parameter->parent_scope == &ndeclaration->scope) {
			parameter->parent_scope = scope;
		}
		assert(parameter->parent_scope == NULL
				|| parameter->parent_scope == scope);
		parameter->parent_scope = scope;
		if (parameter->symbol == NULL) {
			errorf(&parameter->source_position, "parameter name omitted");
			continue;
		}
		environment_push(parameter);
	}

	if (declaration->init.statement != NULL) {
		parser_error_multiple_definition(declaration, HERE);
		eat_block();
	} else {
		/* parse function body */
		int            label_stack_top      = label_top();
		declaration_t *old_current_function = current_function;
		current_function                    = declaration;
		current_parent                      = NULL;

		statement_t *const body = parse_compound_statement(false);
		declaration->init.statement = body;
		first_err = true;
		check_labels();
		check_declarations();
		if (warning.return_type      ||
		    warning.unreachable_code ||
		    (warning.missing_noreturn && !(declaration->modifiers & DM_NORETURN))) {
			noreturn_candidate = true;
			check_reachable(body);
			if (warning.unreachable_code)
				check_unreachable(body);
			if (warning.missing_noreturn &&
			    noreturn_candidate       &&
			    !(declaration->modifiers & DM_NORETURN)) {
				warningf(&body->base.source_position,
				         "function '%#T' is candidate for attribute 'noreturn'",
				         type, declaration->symbol);
			}
		}

		assert(current_parent   == NULL);
		assert(current_function == declaration);
		current_function = old_current_function;
		label_pop_to(label_stack_top);
	}

	assert(scope == &declaration->scope);
	set_scope(last_scope);
	environment_pop_to(top);
}

static type_t *make_bitfield_type(type_t *base_type, expression_t *size,
                                  source_position_t *source_position)
{
	type_t *type = allocate_type_zero(TYPE_BITFIELD, source_position);

	type->bitfield.base_type = base_type;
	type->bitfield.size      = size;

	return type;
}

static declaration_t *find_compound_entry(declaration_t *compound_declaration,
                                          symbol_t *symbol)
{
	declaration_t *iter = compound_declaration->scope.declarations;
	for( ; iter != NULL; iter = iter->next) {
		if (iter->namespc != NAMESPACE_NORMAL)
			continue;

		if (iter->symbol == NULL) {
			type_t *type = skip_typeref(iter->type);
			if (is_type_compound(type)) {
				declaration_t *result
					= find_compound_entry(type->compound.declaration, symbol);
				if (result != NULL)
					return result;
			}
			continue;
		}

		if (iter->symbol == symbol) {
			return iter;
		}
	}

	return NULL;
}

static void parse_compound_declarators(declaration_t *struct_declaration,
		const declaration_specifiers_t *specifiers)
{
	declaration_t *last_declaration = struct_declaration->scope.declarations;
	if (last_declaration != NULL) {
		while(last_declaration->next != NULL) {
			last_declaration = last_declaration->next;
		}
	}

	while(1) {
		declaration_t *declaration;

		if (token.type == ':') {
			source_position_t source_position = *HERE;
			next_token();

			type_t *base_type = specifiers->type;
			expression_t *size = parse_constant_expression();

			if (!is_type_integer(skip_typeref(base_type))) {
				errorf(HERE, "bitfield base type '%T' is not an integer type",
				       base_type);
			}

			type_t *type = make_bitfield_type(base_type, size, &source_position);

			declaration                         = allocate_declaration_zero();
			declaration->namespc                = NAMESPACE_NORMAL;
			declaration->declared_storage_class = STORAGE_CLASS_NONE;
			declaration->storage_class          = STORAGE_CLASS_NONE;
			declaration->source_position        = source_position;
			declaration->modifiers              = specifiers->modifiers;
			declaration->type                   = type;
		} else {
			declaration = parse_declarator(specifiers,/*may_be_abstract=*/true);

			type_t *orig_type = declaration->type;
			type_t *type      = skip_typeref(orig_type);

			if (token.type == ':') {
				source_position_t source_position = *HERE;
				next_token();
				expression_t *size = parse_constant_expression();

				if (!is_type_integer(type)) {
					errorf(HERE, "bitfield base type '%T' is not an integer type",
					       orig_type);
				}

				type_t *bitfield_type = make_bitfield_type(orig_type, size, &source_position);
				declaration->type = bitfield_type;
			} else {
				/* TODO we ignore arrays for now... what is missing is a check
				 * that they're at the end of the struct */
				if (is_type_incomplete(type) && !is_type_array(type)) {
					errorf(HERE,
					       "compound member '%Y' has incomplete type '%T'",
					       declaration->symbol, orig_type);
				} else if (is_type_function(type)) {
					errorf(HERE, "compound member '%Y' must not have function type '%T'",
					       declaration->symbol, orig_type);
				}
			}
		}

		/* make sure we don't define a symbol multiple times */
		symbol_t *symbol = declaration->symbol;
		if (symbol != NULL) {
			declaration_t *prev_decl
				= find_compound_entry(struct_declaration, symbol);

			if (prev_decl != NULL) {
				assert(prev_decl->symbol == symbol);
				errorf(&declaration->source_position,
				       "multiple declarations of symbol '%Y' (declared %P)",
				       symbol, &prev_decl->source_position);
			}
		}

		/* append declaration */
		if (last_declaration != NULL) {
			last_declaration->next = declaration;
		} else {
			struct_declaration->scope.declarations = declaration;
		}
		last_declaration = declaration;

		if (token.type != ',')
			break;
		next_token();
	}
	expect(';');

end_error:
	;
}

static void parse_compound_type_entries(declaration_t *compound_declaration)
{
	eat('{');
	add_anchor_token('}');

	while(token.type != '}' && token.type != T_EOF) {
		declaration_specifiers_t specifiers;
		memset(&specifiers, 0, sizeof(specifiers));
		parse_declaration_specifiers(&specifiers);

		parse_compound_declarators(compound_declaration, &specifiers);
	}
	rem_anchor_token('}');

	if (token.type == T_EOF) {
		errorf(HERE, "EOF while parsing struct");
	}
	next_token();
}

static type_t *parse_typename(void)
{
	declaration_specifiers_t specifiers;
	memset(&specifiers, 0, sizeof(specifiers));
	parse_declaration_specifiers(&specifiers);
	if (specifiers.declared_storage_class != STORAGE_CLASS_NONE) {
		/* TODO: improve error message, user does probably not know what a
		 * storage class is...
		 */
		errorf(HERE, "typename may not have a storage class");
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

/**
 * Prints an error message if an expression was expected but not read
 */
static expression_t *expected_expression_error(void)
{
	/* skip the error message if the error token was read */
	if (token.type != T_ERROR) {
		errorf(HERE, "expected expression, got token '%K'", &token);
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
 * Parse an integer constant.
 */
static expression_t *parse_int_const(void)
{
	expression_t *cnst         = allocate_expression_zero(EXPR_CONST);
	cnst->base.source_position = *HERE;
	cnst->base.type            = token.datatype;
	cnst->conste.v.int_value   = token.v.intvalue;

	next_token();

	return cnst;
}

/**
 * Parse a character constant.
 */
static expression_t *parse_character_constant(void)
{
	expression_t *cnst = allocate_expression_zero(EXPR_CHARACTER_CONSTANT);

	cnst->base.source_position = *HERE;
	cnst->base.type            = token.datatype;
	cnst->conste.v.character   = token.v.string;

	if (cnst->conste.v.character.size != 1) {
		if (warning.multichar && (c_mode & _GNUC)) {
			/* TODO */
			warningf(HERE, "multi-character character constant");
		} else {
			errorf(HERE, "more than 1 characters in character constant");
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

	cnst->base.source_position    = *HERE;
	cnst->base.type               = token.datatype;
	cnst->conste.v.wide_character = token.v.wide_string;

	if (cnst->conste.v.wide_character.size != 1) {
		if (warning.multichar && (c_mode & _GNUC)) {
			/* TODO */
			warningf(HERE, "multi-character character constant");
		} else {
			errorf(HERE, "more than 1 characters in character constant");
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

static declaration_t *create_implicit_function(symbol_t *symbol,
		const source_position_t *source_position)
{
	type_t *ntype                          = allocate_type_zero(TYPE_FUNCTION, source_position);
	ntype->function.return_type            = type_int;
	ntype->function.unspecified_parameters = true;

	type_t *type = typehash_insert(ntype);
	if (type != ntype) {
		free_type(ntype);
	}

	declaration_t *const declaration    = allocate_declaration_zero();
	declaration->storage_class          = STORAGE_CLASS_EXTERN;
	declaration->declared_storage_class = STORAGE_CLASS_EXTERN;
	declaration->type                   = type;
	declaration->symbol                 = symbol;
	declaration->source_position        = *source_position;
	declaration->implicit               = true;

	bool strict_prototypes_old = warning.strict_prototypes;
	warning.strict_prototypes  = false;
	record_declaration(declaration);
	warning.strict_prototypes = strict_prototypes_old;

	return declaration;
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

	type_t *type               = allocate_type_zero(TYPE_FUNCTION, &builtin_source_position);
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

	type_t *type               = allocate_type_zero(TYPE_FUNCTION, &builtin_source_position);
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
	type_t *type               = allocate_type_zero(TYPE_FUNCTION, &builtin_source_position);
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
	switch(symbol->ID) {
	case T___builtin_alloca:
		return make_function_1_type(type_void_ptr, type_size_t);
	case T___builtin_huge_val:
		return make_function_0_type(type_double);
	case T___builtin_nan:
		return make_function_1_type(type_double, type_char_ptr);
	case T___builtin_nanf:
		return make_function_1_type(type_float, type_char_ptr);
	case T___builtin_nand:
		return make_function_1_type(type_long_double, type_char_ptr);
	case T___builtin_va_end:
		return make_function_1_type(type_void, type_valist);
	case T___builtin_expect:
		return make_function_2_type(type_long, type_long, type_long);
	default:
		internal_errorf(HERE, "not implemented builtin symbol found");
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
		case EXPR_REFERENCE: return expression->reference.declaration->type;
		case EXPR_SELECT:    return expression->select.compound_entry->type;

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
	expression_t *expression = allocate_expression_zero(EXPR_REFERENCE);

	reference_expression_t *ref = &expression->reference;
	symbol_t *const symbol = token.v.symbol;

	declaration_t *declaration = get_declaration(symbol, NAMESPACE_NORMAL);

	source_position_t source_position = token.source_position;
	next_token();

	if (declaration == NULL) {
		if (token.type == '(') {
			/* an implicitly declared function */
			if (strict_mode) {
				errorf(HERE, "unknown symbol '%Y' found.", symbol);
			} else if (warning.implicit_function_declaration) {
				warningf(HERE, "implicit declaration of function '%Y'",
					symbol);
			}

			declaration = create_implicit_function(symbol,
			                                       &source_position);
		} else {
			errorf(HERE, "unknown symbol '%Y' found.", symbol);
			declaration = create_error_declaration(symbol, STORAGE_CLASS_NONE);
		}
	}

	type_t *type = declaration->type;

	/* we always do the auto-type conversions; the & and sizeof parser contains
	 * code to revert this! */
	type = automatic_type_conversion(type);

	ref->declaration = declaration;
	ref->base.type   = type;

	/* this declaration is used */
	declaration->used = true;

	/* check for deprecated functions */
	if (warning.deprecated_declarations &&
	    declaration->modifiers & DM_DEPRECATED) {
		char const *const prefix = is_type_function(declaration->type) ?
			"function" : "variable";

		if (declaration->deprecated_string != NULL) {
			warningf(&source_position,
				"%s '%Y' is deprecated (declared %P): \"%s\"", prefix,
				declaration->symbol, &declaration->source_position,
				declaration->deprecated_string);
		} else {
			warningf(&source_position,
				"%s '%Y' is deprecated (declared %P)", prefix,
				declaration->symbol, &declaration->source_position);
		}
	}
	if (warning.init_self && declaration == current_init_decl) {
		current_init_decl = NULL;
		warningf(&source_position,
			"variable '%#T' is initialized by itself",
			declaration->type, declaration->symbol);
	}

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
	env.declaration      = NULL;
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
	source_position_t source_position = token.source_position;

	type_t *type  = parse_typename();

	/* matching add_anchor_token() is at call site */
	rem_anchor_token(')');
	expect(')');

	if (token.type == '{') {
		return parse_compound_literal(type);
	}

	expression_t *cast = allocate_expression_zero(EXPR_UNARY_CAST);
	cast->base.source_position = source_position;

	expression_t *value = parse_sub_expression(20);
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
	expression_t *expression = allocate_expression_zero(EXPR_STATEMENT);

	statement_t *statement           = parse_compound_statement(true);
	expression->statement.statement  = statement;
	expression->base.source_position = statement->base.source_position;

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
		warningf(&expression->base.source_position, "empty statement expression ({})");
	}
	expression->base.type = type;

	expect(')');

	return expression;
end_error:
	return create_invalid_expression();
}

/**
 * Parse a parenthesized expression.
 */
static expression_t *parse_parenthesized_expression(void)
{
	eat('(');
	add_anchor_token(')');

	switch(token.type) {
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

	expression_t *result = parse_expression();
	rem_anchor_token(')');
	expect(')');

	return result;
end_error:
	return create_invalid_expression();
}

static expression_t *parse_function_keyword(void)
{
	next_token();
	/* TODO */

	if (current_function == NULL) {
		errorf(HERE, "'__func__' used outside of a function");
	}

	expression_t *expression  = allocate_expression_zero(EXPR_FUNCNAME);
	expression->base.type     = type_char_ptr;
	expression->funcname.kind = FUNCNAME_FUNCTION;

	return expression;
}

static expression_t *parse_pretty_function_keyword(void)
{
	eat(T___PRETTY_FUNCTION__);

	if (current_function == NULL) {
		errorf(HERE, "'__PRETTY_FUNCTION__' used outside of a function");
	}

	expression_t *expression  = allocate_expression_zero(EXPR_FUNCNAME);
	expression->base.type     = type_char_ptr;
	expression->funcname.kind = FUNCNAME_PRETTY_FUNCTION;

	return expression;
}

static expression_t *parse_funcsig_keyword(void)
{
	eat(T___FUNCSIG__);

	if (current_function == NULL) {
		errorf(HERE, "'__FUNCSIG__' used outside of a function");
	}

	expression_t *expression  = allocate_expression_zero(EXPR_FUNCNAME);
	expression->base.type     = type_char_ptr;
	expression->funcname.kind = FUNCNAME_FUNCSIG;

	return expression;
}

static expression_t *parse_funcdname_keyword(void)
{
	eat(T___FUNCDNAME__);

	if (current_function == NULL) {
		errorf(HERE, "'__FUNCDNAME__' used outside of a function");
	}

	expression_t *expression  = allocate_expression_zero(EXPR_FUNCNAME);
	expression->base.type     = type_char_ptr;
	expression->funcname.kind = FUNCNAME_FUNCDNAME;

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
	while(true) {
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
	eat(T___builtin_offsetof);

	expression_t *expression = allocate_expression_zero(EXPR_OFFSETOF);
	expression->base.type    = type_size_t;

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
	eat(T___builtin_va_start);

	expression_t *expression = allocate_expression_zero(EXPR_VA_START);

	expect('(');
	add_anchor_token(',');
	expression->va_starte.ap = parse_assignment_expression();
	rem_anchor_token(',');
	expect(',');
	expression_t *const expr = parse_assignment_expression();
	if (expr->kind == EXPR_REFERENCE) {
		declaration_t *const decl = expr->reference.declaration;
		if (decl == NULL)
			return create_invalid_expression();
		if (decl->parent_scope == &current_function->scope &&
		    decl->next == NULL) {
			expression->va_starte.parameter = decl;
			expect(')');
			return expression;
		}
	}
	errorf(&expr->base.source_position,
	       "second argument of 'va_start' must be last parameter of the current function");
end_error:
	return create_invalid_expression();
}

/**
 * Parses a _builtin_va_arg() expression.
 */
static expression_t *parse_va_arg(void)
{
	eat(T___builtin_va_arg);

	expression_t *expression = allocate_expression_zero(EXPR_VA_ARG);

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
	eat(T___builtin_constant_p);

	expression_t *expression = allocate_expression_zero(EXPR_BUILTIN_CONSTANT_P);

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
	eat(T___builtin_prefetch);

	expression_t *expression = allocate_expression_zero(EXPR_BUILTIN_PREFETCH);

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

	switch(token.type) {
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
		break;
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
	eat(T___builtin_expect);

	expression_t *expression
		= allocate_expression_zero(EXPR_BINARY_BUILTIN_EXPECT);

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
	eat(T__assume);

	expression_t *expression
		= allocate_expression_zero(EXPR_UNARY_ASSUME);

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
 * Parse a microsoft __noop expression.
 */
static expression_t *parse_noop_expression(void)
{
	source_position_t source_position = *HERE;
	eat(T___noop);

	if (token.type == '(') {
		/* parse arguments */
		eat('(');
		add_anchor_token(')');
		add_anchor_token(',');

		if (token.type != ')') {
			while(true) {
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

	/* the result is a (int)0 */
	expression_t *cnst         = allocate_expression_zero(EXPR_CONST);
	cnst->base.source_position = source_position;
	cnst->base.type            = type_int;
	cnst->conste.v.int_value   = 0;
	cnst->conste.is_ms_noop    = true;

	return cnst;

end_error:
	return create_invalid_expression();
}

/**
 * Parses a primary expression.
 */
static expression_t *parse_primary_expression(void)
{
	switch (token.type) {
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
		case T___builtin_nan:
		case T___builtin_nand:
		case T___builtin_nanf:
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

static expression_t *parse_array_expression(unsigned precedence,
                                            expression_t *left)
{
	(void) precedence;

	eat('[');
	add_anchor_token(']');

	expression_t *inside = parse_expression();

	expression_t *expression = allocate_expression_zero(EXPR_ARRAY_ACCESS);

	array_access_expression_t *array_access = &expression->array_access;

	type_t *const orig_type_left   = left->base.type;
	type_t *const orig_type_inside = inside->base.type;

	type_t *const type_left   = skip_typeref(orig_type_left);
	type_t *const type_inside = skip_typeref(orig_type_inside);

	type_t *return_type;
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
		array_access->array_ref = create_invalid_expression();
	}

	rem_anchor_token(']');
	if (token.type != ']') {
		parse_error_expected("Problem while parsing array access", ']', NULL);
		return expression;
	}
	next_token();

	return_type           = automatic_type_conversion(return_type);
	expression->base.type = return_type;

	return expression;
}

static expression_t *parse_typeprop(expression_kind_t const kind,
                                    source_position_t const pos,
                                    unsigned const precedence)
{
	expression_t *tp_expression = allocate_expression_zero(kind);
	tp_expression->base.type            = type_size_t;
	tp_expression->base.source_position = pos;

	char const* const what = kind == EXPR_SIZEOF ? "sizeof" : "alignof";

	if (token.type == '(' && is_declaration_specifier(look_ahead(1), true)) {
		next_token();
		add_anchor_token(')');
		type_t* const orig_type = parse_typename();
		tp_expression->typeprop.type = orig_type;

		type_t const* const type = skip_typeref(orig_type);
		char const* const wrong_type =
			is_type_incomplete(type)    ? "incomplete"          :
			type->kind == TYPE_FUNCTION ? "function designator" :
			type->kind == TYPE_BITFIELD ? "bitfield"            :
			NULL;
		if (wrong_type != NULL) {
			errorf(&pos, "operand of %s expression must not be %s type '%T'",
			       what, wrong_type, type);
		}

		rem_anchor_token(')');
		expect(')');
	} else {
		expression_t *expression = parse_sub_expression(precedence);

		type_t* const orig_type = revert_automatic_type_conversion(expression);
		expression->base.type = orig_type;

		type_t const* const type = skip_typeref(orig_type);
		char const* const wrong_type =
			is_type_incomplete(type)    ? "incomplete"          :
			type->kind == TYPE_FUNCTION ? "function designator" :
			type->kind == TYPE_BITFIELD ? "bitfield"            :
			NULL;
		if (wrong_type != NULL) {
			errorf(&pos, "operand of %s expression must not be expression of %s type '%T'", what, wrong_type, type);
		}

		tp_expression->typeprop.type          = expression->base.type;
		tp_expression->typeprop.tp_expression = expression;
	}

	return tp_expression;
end_error:
	return create_invalid_expression();
}

static expression_t *parse_sizeof(unsigned precedence)
{
	source_position_t pos = *HERE;
	eat(T_sizeof);
	return parse_typeprop(EXPR_SIZEOF, pos, precedence);
}

static expression_t *parse_alignof(unsigned precedence)
{
	source_position_t pos = *HERE;
	eat(T___alignof__);
	return parse_typeprop(EXPR_ALIGNOF, pos, precedence);
}

static expression_t *parse_select_expression(unsigned precedence,
                                             expression_t *compound)
{
	(void) precedence;
	assert(token.type == '.' || token.type == T_MINUSGREATER);

	bool is_pointer = (token.type == T_MINUSGREATER);
	next_token();

	expression_t *select    = allocate_expression_zero(EXPR_SELECT);
	select->select.compound = compound;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("while parsing select", T_IDENTIFIER, NULL);
		return select;
	}
	symbol_t *symbol = token.v.symbol;
	next_token();

	type_t *const orig_type = compound->base.type;
	type_t *const type      = skip_typeref(orig_type);

	type_t *type_left = type;
	bool    saw_error = false;
	if (is_type_pointer(type)) {
		if (!is_pointer) {
			errorf(HERE,
			       "request for member '%Y' in something not a struct or union, but '%T'",
			       symbol, type_left);
			saw_error = true;
		}
		type_left = type->pointer.points_to;
	} else if (is_pointer) {
		if (is_type_valid(type)) {
			errorf(HERE, "left hand side of '->' is not a pointer, but '%T'", orig_type);
			saw_error = true;
		}
		type_left = type;
	}
	type_left = skip_typeref(type_left);

	declaration_t *entry;
	if (type_left->kind == TYPE_COMPOUND_STRUCT ||
	    type_left->kind == TYPE_COMPOUND_UNION) {
		declaration_t *const declaration = type_left->compound.declaration;

		if (!declaration->init.complete) {
			errorf(HERE, "request for member '%Y' of incomplete type '%T'",
			       symbol, type_left);
			return create_invalid_expression();
		}

		entry = find_compound_entry(declaration, symbol);
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
		entry         = allocate_declaration_zero();
		entry->symbol = symbol;
	}

	select->select.compound_entry = entry;
	/* we always do the auto-type conversions; the & and sizeof parser contains
	 * code to revert this! */
	select->base.type = automatic_type_conversion(entry->type);

	type_t *skipped = skip_typeref(entry->type);
	if (skipped->kind == TYPE_BITFIELD) {
		select->base.type = skipped->bitfield.base_type;
	}

	return select;
}

static void check_call_argument(const function_parameter_t *parameter,
                                call_argument_t *argument)
{
	type_t         *expected_type      = parameter->type;
	type_t         *expected_type_skip = skip_typeref(expected_type);
	assign_error_t  error              = ASSIGN_ERROR_INCOMPATIBLE;
	expression_t   *arg_expr           = argument->expression;

	/* handle transparent union gnu extension */
	if (is_type_union(expected_type_skip)
			&& (expected_type_skip->base.modifiers
				& TYPE_MODIFIER_TRANSPARENT_UNION)) {
		declaration_t  *union_decl = expected_type_skip->compound.declaration;

		declaration_t *declaration = union_decl->scope.declarations;
		type_t        *best_type   = NULL;
		for ( ; declaration != NULL; declaration = declaration->next) {
			type_t *decl_type = declaration->type;
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

	/* TODO report exact scope in error messages (like "in 3rd parameter") */
	report_assign_error(error, expected_type, arg_expr,	"function call",
	                    &arg_expr->base.source_position);
}

/**
 * Parse a call expression, ie. expression '( ... )'.
 *
 * @param expression  the function address
 */
static expression_t *parse_call_expression(unsigned precedence,
                                           expression_t *expression)
{
	(void) precedence;
	expression_t *result = allocate_expression_zero(EXPR_CALL);
	result->base.source_position = expression->base.source_position;

	call_expression_t *call = &result->call;
	call->function          = expression;

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

		while(true) {
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
		for( ; parameter != NULL && argument != NULL;
				parameter = parameter->next, argument = argument->next) {
			check_call_argument(parameter, argument);
		}

		if (parameter != NULL) {
			errorf(HERE, "too few arguments to function '%E'", expression);
		} else if (argument != NULL && !function_type->variadic) {
			errorf(HERE, "too many arguments to function '%E'", expression);
		}
	}

	/* do default promotion */
	for( ; argument != NULL; argument = argument->next) {
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

	return result;
end_error:
	return create_invalid_expression();
}

static type_t *semantic_arithmetic(type_t *type_left, type_t *type_right);

static bool same_compound_type(const type_t *type1, const type_t *type2)
{
	return
		is_type_compound(type1) &&
		type1->kind == type2->kind &&
		type1->compound.declaration == type2->compound.declaration;
}

/**
 * Parse a conditional expression, ie. 'expression ? ... : ...'.
 *
 * @param expression  the conditional expression
 */
static expression_t *parse_conditional_expression(unsigned precedence,
                                                  expression_t *expression)
{
	expression_t *result = allocate_expression_zero(EXPR_CONDITIONAL);

	conditional_expression_t *conditional = &result->conditional;
	conditional->base.source_position = *HERE;
	conditional->condition            = expression;

	eat('?');
	add_anchor_token(':');

	/* 6.5.15.2 */
	type_t *const condition_type_orig = expression->base.type;
	type_t *const condition_type      = skip_typeref(condition_type_orig);
	if (!is_type_scalar(condition_type) && is_type_valid(condition_type)) {
		type_error("expected a scalar type in conditional condition",
		           &expression->base.source_position, condition_type_orig);
	}

	expression_t *true_expression = expression;
	bool          gnu_cond = false;
	if ((c_mode & _GNUC) && token.type == ':') {
		gnu_cond = true;
	} else
		true_expression = parse_expression();
	rem_anchor_token(':');
	expect(':');
	expression_t *false_expression = parse_sub_expression(precedence);

	type_t *const orig_true_type  = true_expression->base.type;
	type_t *const orig_false_type = false_expression->base.type;
	type_t *const true_type       = skip_typeref(orig_true_type);
	type_t *const false_type      = skip_typeref(orig_false_type);

	/* 6.5.15.3 */
	type_t *result_type;
	if (is_type_atomic(true_type, ATOMIC_TYPE_VOID) ||
		is_type_atomic(false_type, ATOMIC_TYPE_VOID)) {
		if (!is_type_atomic(true_type, ATOMIC_TYPE_VOID)
		    || !is_type_atomic(false_type, ATOMIC_TYPE_VOID)) {
			warningf(&conditional->base.source_position,
					"ISO C forbids conditional expression with only one void side");
		}
		result_type = type_void;
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
				warningf(&conditional->base.source_position,
					"pointer types '%T' and '%T' in conditional expression are incompatible",
					true_type, false_type);
				to = type_void;
			}

			type_t *const copy = duplicate_type(to);
			copy->base.qualifiers = to1->base.qualifiers | to2->base.qualifiers;

			type_t *const type = typehash_insert(copy);
			if (type != copy)
				free_type(copy);

			result_type = make_pointer_type(type, TYPE_QUALIFIER_NONE);
		} else if (is_type_integer(other_type)) {
			warningf(&conditional->base.source_position,
					"pointer/integer type mismatch in conditional expression ('%T' and '%T')", true_type, false_type);
			result_type = pointer_type;
		} else {
			type_error_incompatible("while parsing conditional",
					&expression->base.source_position, true_type, false_type);
			result_type = type_error_type;
		}
	} else {
		/* TODO: one pointer to void*, other some pointer */

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
static expression_t *parse_extension(unsigned precedence)
{
	eat(T___extension__);

	/* TODO enable extensions */
	expression_t *expression = parse_sub_expression(precedence);
	/* TODO disable extensions */
	return expression;
}

/**
 * Parse a __builtin_classify_type() expression.
 */
static expression_t *parse_builtin_classify_type(const unsigned precedence)
{
	eat(T___builtin_classify_type);

	expression_t *result = allocate_expression_zero(EXPR_CLASSIFY_TYPE);
	result->base.type    = type_int;

	expect('(');
	add_anchor_token(')');
	expression_t *expression = parse_sub_expression(precedence);
	rem_anchor_token(')');
	expect(')');
	result->classify_type.type_expression = expression;

	return result;
end_error:
	return create_invalid_expression();
}

static bool check_pointer_arithmetic(const source_position_t *source_position,
                                     type_t *pointer_type,
                                     type_t *orig_pointer_type)
{
	type_t *points_to = pointer_type->pointer.points_to;
	points_to = skip_typeref(points_to);

	if (is_type_incomplete(points_to)) {
		if (!(c_mode & _GNUC) || !is_type_atomic(points_to, ATOMIC_TYPE_VOID)) {
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
		if (!(c_mode && _GNUC)) {
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
	switch (expression->kind) {
	case EXPR_REFERENCE:
	case EXPR_ARRAY_ACCESS:
	case EXPR_SELECT:
	case EXPR_UNARY_DEREFERENCE:
		return true;

	default:
		return false;
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

static void semantic_not(unary_expression_t *expression)
{
	type_t *const orig_type = expression->value->base.type;
	type_t *const type      = skip_typeref(orig_type);
	if (!is_type_scalar(type) && is_type_valid(type)) {
		errorf(&expression->base.source_position,
		       "operand of ! must be of scalar type");
	}

	expression->base.type = type_int;
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
			       "Unary '*' needs pointer or arrray type, but type '%T' given", orig_type);
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

	declaration_t *const declaration = expression->reference.declaration;
	/* happens for parse errors */
	if (declaration == NULL)
		return;

	if (declaration->storage_class == STORAGE_CLASS_REGISTER && !may_be_register) {
		errorf(&expression->base.source_position,
				"address of register variable '%Y' requested",
				declaration->symbol);
	} else {
		declaration->address_taken = 1;
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
	if (!is_type_valid(orig_type))
		return;

	set_address_taken(value, false);

	expression->base.type = make_pointer_type(orig_type, TYPE_QUALIFIER_NONE);
}

#define CREATE_UNARY_EXPRESSION_PARSER(token_type, unexpression_type, sfunc)   \
static expression_t *parse_##unexpression_type(unsigned precedence)            \
{                                                                              \
	expression_t *unary_expression                                             \
		= allocate_expression_zero(unexpression_type);                         \
	unary_expression->base.source_position = *HERE;                            \
	eat(token_type);                                                           \
	unary_expression->unary.value = parse_sub_expression(precedence);          \
	                                                                           \
	sfunc(&unary_expression->unary);                                           \
	                                                                           \
	return unary_expression;                                                   \
}

CREATE_UNARY_EXPRESSION_PARSER('-', EXPR_UNARY_NEGATE,
                               semantic_unexpr_arithmetic)
CREATE_UNARY_EXPRESSION_PARSER('+', EXPR_UNARY_PLUS,
                               semantic_unexpr_arithmetic)
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
static expression_t *parse_##unexpression_type(unsigned precedence,           \
                                               expression_t *left)            \
{                                                                             \
	(void) precedence;                                                        \
                                                                              \
	expression_t *unary_expression                                            \
		= allocate_expression_zero(unexpression_type);                        \
	unary_expression->base.source_position = *HERE;                           \
	eat(token_type);                                                          \
	unary_expression->unary.value          = left;                            \
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
	if (warning.div_by_zero                       &&
	    is_type_integer(expression->base.type)    &&
	    is_constant_expression(expression->right) &&
	    fold_constant(expression->right) == 0) {
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
			if (is_type_atomic(unqual_left, ATOMIC_TYPE_VOID)) {
				warningf(pos, "subtracting pointers to void");
			} else {
				errorf(pos, "subtracting pointers to non-object types '%T'",
				       orig_type_left);
			}
		}
		expression->base.type = type_ptrdiff_t;
	} else if (is_type_valid(type_left) && is_type_valid(type_right)) {
		errorf(pos, "invalid operands of types '%T' and '%T' to binary '-'",
		       orig_type_left, orig_type_right);
	}
}

/**
 * Check the semantics of comparison expressions.
 *
 * @param expression   The expression to check.
 */
static void semantic_comparison(binary_expression_t *expression)
{
	expression_t *left            = expression->left;
	expression_t *right           = expression->right;
	type_t       *orig_type_left  = left->base.type;
	type_t       *orig_type_right = right->base.type;

	type_t *type_left  = skip_typeref(orig_type_left);
	type_t *type_right = skip_typeref(orig_type_right);

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
	expression->base.type = type_int;
}

/**
 * Checks if a compound type has constant fields.
 */
static bool has_const_fields(const compound_type_t *type)
{
	const scope_t       *scope       = &type->declaration->scope;
	const declaration_t *declaration = scope->declarations;

	for (; declaration != NULL; declaration = declaration->next) {
		if (declaration->namespc != NAMESPACE_NORMAL)
			continue;

		const type_t *decl_type = skip_typeref(declaration->type);
		if (decl_type->base.qualifiers & TYPE_QUALIFIER_CONST)
			return true;
	}
	/* TODO */
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

	if (is_type_array(type_left)) {
		errorf(HERE, "cannot assign to arrays ('%E')", left);
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
	expression_t *const left            = expression->left;
	expression_t *const right           = expression->right;
	type_t       *const orig_type_left  = left->base.type;
	type_t       *const orig_type_right = right->base.type;
	type_t       *const type_left       = skip_typeref(orig_type_left);
	type_t       *const type_right      = skip_typeref(orig_type_right);

	if (!is_type_scalar(type_left) || !is_type_scalar(type_right)) {
		/* TODO: improve error message */
		if (is_type_valid(type_left) && is_type_valid(type_right)) {
			errorf(&expression->base.source_position,
			       "operation needs scalar types");
		}
		return;
	}

	expression->base.type = type_int;
}

/**
 * Check the semantic restrictions of a binary assign expression.
 */
static void semantic_binexpr_assign(binary_expression_t *expression)
{
	expression_t *left           = expression->left;
	type_t       *orig_type_left = left->base.type;

	type_t *type_left = revert_automatic_type_conversion(left);
	type_left         = skip_typeref(orig_type_left);

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
		/* suppress the warning for microsoft __noop operations */
		case EXPR_CONST:                     return expr->conste.is_ms_noop;
		case EXPR_CHARACTER_CONSTANT:        return false;
		case EXPR_WIDE_CHARACTER_CONSTANT:   return false;
		case EXPR_STRING_LITERAL:            return false;
		case EXPR_WIDE_STRING_LITERAL:       return false;

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

		case EXPR_BINARY_BUILTIN_EXPECT:     return true;
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

#define CREATE_BINEXPR_PARSER(token_type, binexpression_type, sfunc, lr)  \
static expression_t *parse_##binexpression_type(unsigned precedence,      \
                                                expression_t *left)       \
{                                                                         \
	expression_t *binexpr = allocate_expression_zero(binexpression_type); \
	binexpr->base.source_position = *HERE;                                \
	binexpr->binary.left          = left;                                 \
	eat(token_type);                                                      \
                                                                          \
	expression_t *right = parse_sub_expression(precedence + lr);          \
                                                                          \
	binexpr->binary.right = right;                                        \
	sfunc(&binexpr->binary);                                              \
                                                                          \
	return binexpr;                                                       \
}

CREATE_BINEXPR_PARSER(',', EXPR_BINARY_COMMA,    semantic_comma, 1)
CREATE_BINEXPR_PARSER('*', EXPR_BINARY_MUL,      semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER('/', EXPR_BINARY_DIV,      semantic_divmod_arithmetic, 1)
CREATE_BINEXPR_PARSER('%', EXPR_BINARY_MOD,      semantic_divmod_arithmetic, 1)
CREATE_BINEXPR_PARSER('+', EXPR_BINARY_ADD,      semantic_add, 1)
CREATE_BINEXPR_PARSER('-', EXPR_BINARY_SUB,      semantic_sub, 1)
CREATE_BINEXPR_PARSER('<', EXPR_BINARY_LESS,     semantic_comparison, 1)
CREATE_BINEXPR_PARSER('>', EXPR_BINARY_GREATER,  semantic_comparison, 1)
CREATE_BINEXPR_PARSER('=', EXPR_BINARY_ASSIGN,   semantic_binexpr_assign, 0)

CREATE_BINEXPR_PARSER(T_EQUALEQUAL,           EXPR_BINARY_EQUAL,
                      semantic_comparison, 1)
CREATE_BINEXPR_PARSER(T_EXCLAMATIONMARKEQUAL, EXPR_BINARY_NOTEQUAL,
                      semantic_comparison, 1)
CREATE_BINEXPR_PARSER(T_LESSEQUAL,            EXPR_BINARY_LESSEQUAL,
                      semantic_comparison, 1)
CREATE_BINEXPR_PARSER(T_GREATEREQUAL,         EXPR_BINARY_GREATEREQUAL,
                      semantic_comparison, 1)

CREATE_BINEXPR_PARSER('&', EXPR_BINARY_BITWISE_AND,
                      semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER('|', EXPR_BINARY_BITWISE_OR,
                      semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER('^', EXPR_BINARY_BITWISE_XOR,
                      semantic_binexpr_arithmetic, 1)
CREATE_BINEXPR_PARSER(T_ANDAND, EXPR_BINARY_LOGICAL_AND,
                      semantic_logical_op, 1)
CREATE_BINEXPR_PARSER(T_PIPEPIPE, EXPR_BINARY_LOGICAL_OR,
                      semantic_logical_op, 1)
CREATE_BINEXPR_PARSER(T_LESSLESS, EXPR_BINARY_SHIFTLEFT,
                      semantic_shift_op, 1)
CREATE_BINEXPR_PARSER(T_GREATERGREATER, EXPR_BINARY_SHIFTRIGHT,
                      semantic_shift_op, 1)
CREATE_BINEXPR_PARSER(T_PLUSEQUAL, EXPR_BINARY_ADD_ASSIGN,
                      semantic_arithmetic_addsubb_assign, 0)
CREATE_BINEXPR_PARSER(T_MINUSEQUAL, EXPR_BINARY_SUB_ASSIGN,
                      semantic_arithmetic_addsubb_assign, 0)
CREATE_BINEXPR_PARSER(T_ASTERISKEQUAL, EXPR_BINARY_MUL_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_SLASHEQUAL, EXPR_BINARY_DIV_ASSIGN,
                      semantic_divmod_assign, 0)
CREATE_BINEXPR_PARSER(T_PERCENTEQUAL, EXPR_BINARY_MOD_ASSIGN,
                      semantic_divmod_assign, 0)
CREATE_BINEXPR_PARSER(T_LESSLESSEQUAL, EXPR_BINARY_SHIFTLEFT_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_GREATERGREATEREQUAL, EXPR_BINARY_SHIFTRIGHT_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_ANDEQUAL, EXPR_BINARY_BITWISE_AND_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_PIPEEQUAL, EXPR_BINARY_BITWISE_OR_ASSIGN,
                      semantic_arithmetic_assign, 0)
CREATE_BINEXPR_PARSER(T_CARETEQUAL, EXPR_BINARY_BITWISE_XOR_ASSIGN,
                      semantic_arithmetic_assign, 0)

static expression_t *parse_sub_expression(unsigned precedence)
{
	if (token.type < 0) {
		return expected_expression_error();
	}

	expression_parser_function_t *parser
		= &expression_parsers[token.type];
	source_position_t             source_position = token.source_position;
	expression_t                 *left;

	if (parser->parser != NULL) {
		left = parser->parser(parser->precedence);
	} else {
		left = parse_primary_expression();
	}
	assert(left != NULL);
	left->base.source_position = source_position;

	while(true) {
		if (token.type < 0) {
			return expected_expression_error();
		}

		parser = &expression_parsers[token.type];
		if (parser->infix_parser == NULL)
			break;
		if (parser->infix_precedence < precedence)
			break;

		left = parser->infix_parser(parser->infix_precedence, left);

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
	return parse_sub_expression(1);
}

/**
 * Register a parser for a prefix-like operator with given precedence.
 *
 * @param parser      the parser function
 * @param token_type  the token type of the prefix token
 * @param precedence  the precedence of the operator
 */
static void register_expression_parser(parse_expression_function parser,
                                       int token_type, unsigned precedence)
{
	expression_parser_function_t *entry = &expression_parsers[token_type];

	if (entry->parser != NULL) {
		diagnosticf("for token '%k'\n", (token_type_t)token_type);
		panic("trying to register multiple expression parsers for a token");
	}
	entry->parser     = parser;
	entry->precedence = precedence;
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

	register_infix_parser(parse_array_expression,         '[',              30);
	register_infix_parser(parse_call_expression,          '(',              30);
	register_infix_parser(parse_select_expression,        '.',              30);
	register_infix_parser(parse_select_expression,        T_MINUSGREATER,   30);
	register_infix_parser(parse_EXPR_UNARY_POSTFIX_INCREMENT,
	                                                      T_PLUSPLUS,       30);
	register_infix_parser(parse_EXPR_UNARY_POSTFIX_DECREMENT,
	                                                      T_MINUSMINUS,     30);

	register_infix_parser(parse_EXPR_BINARY_MUL,          '*',              17);
	register_infix_parser(parse_EXPR_BINARY_DIV,          '/',              17);
	register_infix_parser(parse_EXPR_BINARY_MOD,          '%',              17);
	register_infix_parser(parse_EXPR_BINARY_ADD,          '+',              16);
	register_infix_parser(parse_EXPR_BINARY_SUB,          '-',              16);
	register_infix_parser(parse_EXPR_BINARY_SHIFTLEFT,    T_LESSLESS,       15);
	register_infix_parser(parse_EXPR_BINARY_SHIFTRIGHT,   T_GREATERGREATER, 15);
	register_infix_parser(parse_EXPR_BINARY_LESS,         '<',              14);
	register_infix_parser(parse_EXPR_BINARY_GREATER,      '>',              14);
	register_infix_parser(parse_EXPR_BINARY_LESSEQUAL,    T_LESSEQUAL,      14);
	register_infix_parser(parse_EXPR_BINARY_GREATEREQUAL, T_GREATEREQUAL,   14);
	register_infix_parser(parse_EXPR_BINARY_EQUAL,        T_EQUALEQUAL,     13);
	register_infix_parser(parse_EXPR_BINARY_NOTEQUAL,
                                                    T_EXCLAMATIONMARKEQUAL, 13);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_AND,  '&',              12);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_XOR,  '^',              11);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_OR,   '|',              10);
	register_infix_parser(parse_EXPR_BINARY_LOGICAL_AND,  T_ANDAND,          9);
	register_infix_parser(parse_EXPR_BINARY_LOGICAL_OR,   T_PIPEPIPE,        8);
	register_infix_parser(parse_conditional_expression,   '?',               7);
	register_infix_parser(parse_EXPR_BINARY_ASSIGN,       '=',               2);
	register_infix_parser(parse_EXPR_BINARY_ADD_ASSIGN,   T_PLUSEQUAL,       2);
	register_infix_parser(parse_EXPR_BINARY_SUB_ASSIGN,   T_MINUSEQUAL,      2);
	register_infix_parser(parse_EXPR_BINARY_MUL_ASSIGN,   T_ASTERISKEQUAL,   2);
	register_infix_parser(parse_EXPR_BINARY_DIV_ASSIGN,   T_SLASHEQUAL,      2);
	register_infix_parser(parse_EXPR_BINARY_MOD_ASSIGN,   T_PERCENTEQUAL,    2);
	register_infix_parser(parse_EXPR_BINARY_SHIFTLEFT_ASSIGN,
	                                                        T_LESSLESSEQUAL, 2);
	register_infix_parser(parse_EXPR_BINARY_SHIFTRIGHT_ASSIGN,
	                                                  T_GREATERGREATEREQUAL, 2);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_AND_ASSIGN,
	                                                             T_ANDEQUAL, 2);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_OR_ASSIGN,
	                                                            T_PIPEEQUAL, 2);
	register_infix_parser(parse_EXPR_BINARY_BITWISE_XOR_ASSIGN,
	                                                           T_CARETEQUAL, 2);

	register_infix_parser(parse_EXPR_BINARY_COMMA,        ',',               1);

	register_expression_parser(parse_EXPR_UNARY_NEGATE,           '-',      25);
	register_expression_parser(parse_EXPR_UNARY_PLUS,             '+',      25);
	register_expression_parser(parse_EXPR_UNARY_NOT,              '!',      25);
	register_expression_parser(parse_EXPR_UNARY_BITWISE_NEGATE,   '~',      25);
	register_expression_parser(parse_EXPR_UNARY_DEREFERENCE,      '*',      25);
	register_expression_parser(parse_EXPR_UNARY_TAKE_ADDRESS,     '&',      25);
	register_expression_parser(parse_EXPR_UNARY_PREFIX_INCREMENT,
	                                                          T_PLUSPLUS,   25);
	register_expression_parser(parse_EXPR_UNARY_PREFIX_DECREMENT,
	                                                          T_MINUSMINUS, 25);
	register_expression_parser(parse_sizeof,                      T_sizeof, 25);
	register_expression_parser(parse_alignof,                T___alignof__, 25);
	register_expression_parser(parse_extension,            T___extension__, 25);
	register_expression_parser(parse_builtin_classify_type,
	                                             T___builtin_classify_type, 25);
}

/**
 * Parse a asm statement arguments specification.
 */
static asm_argument_t *parse_asm_arguments(bool is_out)
{
	asm_argument_t *result = NULL;
	asm_argument_t *last   = NULL;

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
		}
		argument->expression = expression;
		expect(')');

		set_address_taken(expression, true);

		if (last != NULL) {
			last->next = argument;
		} else {
			result = argument;
		}
		last = argument;

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

	while(token.type == T_STRING_LITERAL) {
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
	eat(T_asm);

	statement_t *statement          = allocate_statement_zero(STATEMENT_ASM);
	statement->base.source_position = token.source_position;

	asm_statement_t *asm_statement = &statement->asms;

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
	eat(T_case);

	statement_t       *const statement = allocate_statement_zero(STATEMENT_CASE_LABEL);
	source_position_t *const pos       = &statement->base.source_position;

	*pos                             = token.source_position;
	statement->case_label.expression = parse_expression();
	if (! is_constant_expression(statement->case_label.expression)) {
		errorf(pos, "case label does not reduce to an integer constant");
		statement->case_label.is_bad = true;
	} else {
		long const val = fold_constant(statement->case_label.expression);
		statement->case_label.first_case = val;
		statement->case_label.last_case  = val;
	}

	if (c_mode & _GNUC) {
		if (token.type == T_DOTDOTDOT) {
			next_token();
			statement->case_label.end_range = parse_expression();
			if (! is_constant_expression(statement->case_label.end_range)) {
				errorf(pos, "case range does not reduce to an integer constant");
				statement->case_label.is_bad = true;
			} else {
				long const val = fold_constant(statement->case_label.end_range);
				statement->case_label.last_case = val;

				if (val < statement->case_label.first_case) {
					statement->case_label.is_empty = true;
					warningf(pos, "empty range specified");
				}
			}
		}
	}

	PUSH_PARENT(statement);

	expect(':');

	if (current_switch != NULL) {
		if (! statement->case_label.is_bad) {
			/* Check for duplicate case values */
			case_label_statement_t *c = &statement->case_label;
			for (case_label_statement_t *l = current_switch->first_case; l != NULL; l = l->next) {
				if (l->is_bad || l->is_empty || l->expression == NULL)
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
end_error:
	POP_PARENT;
	return create_invalid_statement();
}

/**
 * Parse a default statement.
 */
static statement_t *parse_default_statement(void)
{
	eat(T_default);

	statement_t *statement = allocate_statement_zero(STATEMENT_CASE_LABEL);
	statement->base.source_position = token.source_position;

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
 * Return the declaration for a given label symbol or create a new one.
 *
 * @param symbol  the symbol of the label
 */
static declaration_t *get_label(symbol_t *symbol)
{
	declaration_t *candidate = get_declaration(symbol, NAMESPACE_LABEL);
	assert(current_function != NULL);
	/* if we found a label in the same function, then we already created the
	 * declaration */
	if (candidate != NULL
			&& candidate->parent_scope == &current_function->scope) {
		return candidate;
	}

	/* otherwise we need to create a new one */
	declaration_t *const declaration = allocate_declaration_zero();
	declaration->namespc       = NAMESPACE_LABEL;
	declaration->symbol        = symbol;

	label_push(declaration);

	return declaration;
}

/**
 * Parse a label statement.
 */
static statement_t *parse_label_statement(void)
{
	assert(token.type == T_IDENTIFIER);
	symbol_t *symbol = token.v.symbol;
	next_token();

	declaration_t *label = get_label(symbol);

	statement_t *const statement = allocate_statement_zero(STATEMENT_LABEL);
	statement->base.source_position = token.source_position;
	statement->label.label          = label;

	PUSH_PARENT(statement);

	/* if source position is already set then the label is defined twice,
	 * otherwise it was just mentioned in a goto so far */
	if (label->source_position.input_name != NULL) {
		errorf(HERE, "duplicate label '%Y' (declared %P)",
		       symbol, &label->source_position);
	} else {
		label->source_position = token.source_position;
		label->init.statement  = statement;
	}

	eat(':');

	if (token.type == '}') {
		/* TODO only warn? */
		if (false) {
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
	if (label_last == NULL) {
		label_first = &statement->label;
	} else {
		label_last->next = &statement->label;
	}
	label_last = &statement->label;

	POP_PARENT;
	return statement;
}

/**
 * Parse an if statement.
 */
static statement_t *parse_if(void)
{
	eat(T_if);

	statement_t *statement          = allocate_statement_zero(STATEMENT_IF);
	statement->base.source_position = token.source_position;

	PUSH_PARENT(statement);

	expect('(');
	add_anchor_token(')');
	statement->ifs.condition = parse_expression();
	rem_anchor_token(')');
	expect(')');

	add_anchor_token(T_else);
	statement->ifs.true_statement = parse_statement();
	rem_anchor_token(T_else);

	if (token.type == T_else) {
		next_token();
		statement->ifs.false_statement = parse_statement();
	}

	POP_PARENT;
	return statement;
end_error:
	POP_PARENT;
	return create_invalid_statement();
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
	const declaration_t *declaration;
	long last_value = -1;
	for (declaration = enumt->declaration->next;
	     declaration != NULL && declaration->storage_class == STORAGE_CLASS_ENUM_ENTRY;
		 declaration = declaration->next) {
		const expression_t *expression = declaration->init.enum_value;
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
				"enumeration value '%Y' not handled in switch", declaration->symbol);
		}
		last_value = value;
	}
}

/**
 * Parse a switch statement.
 */
static statement_t *parse_switch(void)
{
	eat(T_switch);

	statement_t *statement          = allocate_statement_zero(STATEMENT_SWITCH);
	statement->base.source_position = token.source_position;

	PUSH_PARENT(statement);

	expect('(');
	add_anchor_token(')');
	expression_t *const expr = parse_expression();
	type_t       *      type = skip_typeref(expr->base.type);
	if (is_type_integer(type)) {
		type = promote_integer(type);
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
	eat(T_while);

	statement_t *statement          = allocate_statement_zero(STATEMENT_WHILE);
	statement->base.source_position = token.source_position;

	PUSH_PARENT(statement);

	expect('(');
	add_anchor_token(')');
	statement->whiles.condition = parse_expression();
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
	eat(T_do);

	statement_t *statement = allocate_statement_zero(STATEMENT_DO_WHILE);
	statement->base.source_position = token.source_position;

	PUSH_PARENT(statement)

	add_anchor_token(T_while);
	statement->do_while.body = parse_loop_body(statement);
	rem_anchor_token(T_while);

	expect(T_while);
	expect('(');
	add_anchor_token(')');
	statement->do_while.condition = parse_expression();
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
	eat(T_for);

	statement_t *statement          = allocate_statement_zero(STATEMENT_FOR);
	statement->base.source_position = token.source_position;

	PUSH_PARENT(statement);

	int      top        = environment_top();
	scope_t *last_scope = scope;
	set_scope(&statement->fors.scope);

	expect('(');
	add_anchor_token(')');

	if (token.type != ';') {
		if (is_declaration_specifier(&token, false)) {
			parse_declaration(record_declaration);
		} else {
			add_anchor_token(';');
			expression_t *const init = parse_expression();
			statement->fors.initialisation = init;
			if (warning.unused_value && !expression_has_effect(init)) {
				warningf(&init->base.source_position,
				         "initialisation of 'for'-statement has no effect");
			}
			rem_anchor_token(';');
			expect(';');
		}
	} else {
		expect(';');
	}

	if (token.type != ';') {
		add_anchor_token(';');
		statement->fors.condition = parse_expression();
		rem_anchor_token(';');
	}
	expect(';');
	if (token.type != ')') {
		expression_t *const step = parse_expression();
		statement->fors.step = step;
		if (warning.unused_value && !expression_has_effect(step)) {
			warningf(&step->base.source_position,
			         "step of 'for'-statement has no effect");
		}
	}
	rem_anchor_token(')');
	expect(')');
	statement->fors.body = parse_loop_body(statement);

	assert(scope == &statement->fors.scope);
	set_scope(last_scope);
	environment_pop_to(top);

	POP_PARENT;
	return statement;

end_error:
	POP_PARENT;
	rem_anchor_token(')');
	assert(scope == &statement->fors.scope);
	set_scope(last_scope);
	environment_pop_to(top);

	return create_invalid_statement();
}

/**
 * Parse a goto statement.
 */
static statement_t *parse_goto(void)
{
	eat(T_goto);

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("while parsing goto", T_IDENTIFIER, NULL);
		eat_statement();
		goto end_error;
	}
	symbol_t *symbol = token.v.symbol;
	next_token();

	declaration_t *label = get_label(symbol);

	statement_t *statement          = allocate_statement_zero(STATEMENT_GOTO);
	statement->base.source_position = token.source_position;

	statement->gotos.label = label;

	/* remember the goto's in a list for later checking */
	if (goto_last == NULL) {
		goto_first = &statement->gotos;
	} else {
		goto_last->next = &statement->gotos;
	}
	goto_last = &statement->gotos;

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
	statement->base.source_position = token.source_position;

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
	statement->base.source_position = token.source_position;

	eat(T_break);
	expect(';');

end_error:
	return statement;
}

/**
 * Parse a __leave statement.
 */
static statement_t *parse_leave(void)
{
	if (current_try == NULL) {
		errorf(HERE, "__leave statement not within __try");
	}

	statement_t *statement = allocate_statement_zero(STATEMENT_LEAVE);
	statement->base.source_position = token.source_position;

	eat(T___leave);
	expect(';');

end_error:
	return statement;
}

/**
 * Check if a given declaration represents a local variable.
 */
static bool is_local_var_declaration(const declaration_t *declaration)
{
	switch ((storage_class_tag_t) declaration->storage_class) {
	case STORAGE_CLASS_AUTO:
	case STORAGE_CLASS_REGISTER: {
		const type_t *type = skip_typeref(declaration->type);
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
 * Check if a given declaration represents a variable.
 */
static bool is_var_declaration(const declaration_t *declaration)
{
	if (declaration->storage_class == STORAGE_CLASS_TYPEDEF)
		return false;

	const type_t *type = skip_typeref(declaration->type);
	return !is_type_function(type);
}

/**
 * Check if a given expression represents a local variable.
 */
static bool is_local_variable(const expression_t *expression)
{
	if (expression->base.kind != EXPR_REFERENCE) {
		return false;
	}
	const declaration_t *declaration = expression->reference.declaration;
	return is_local_var_declaration(declaration);
}

/**
 * Check if a given expression represents a local variable and
 * return its declaration then, else return NULL.
 */
declaration_t *expr_is_variable(const expression_t *expression)
{
	if (expression->base.kind != EXPR_REFERENCE) {
		return NULL;
	}
	declaration_t *declaration = expression->reference.declaration;
	if (is_var_declaration(declaration))
		return declaration;
	return NULL;
}

/**
 * Parse a return statement.
 */
static statement_t *parse_return(void)
{
	statement_t *statement          = allocate_statement_zero(STATEMENT_RETURN);
	statement->base.source_position = token.source_position;

	eat(T_return);

	expression_t *return_value = NULL;
	if (token.type != ';') {
		return_value = parse_expression();
	}

	const type_t *const func_type = current_function->type;
	assert(is_type_function(func_type));
	type_t *const return_type = skip_typeref(func_type->function.return_type);

	if (return_value != NULL) {
		type_t *return_value_type = skip_typeref(return_value->base.type);

		if (is_type_atomic(return_type, ATOMIC_TYPE_VOID)
				&& !is_type_atomic(return_value_type, ATOMIC_TYPE_VOID)) {
			warningf(&statement->base.source_position,
			         "'return' with a value, in function returning void");
			return_value = NULL;
		} else {
			assign_error_t error = semantic_assign(return_type, return_value);
			report_assign_error(error, return_type,	return_value, "'return'",
			                    &statement->base.source_position);
			return_value = create_implicit_cast(return_value, return_type);
		}
		/* check for returning address of a local var */
		if (return_value != NULL &&
				return_value->base.kind == EXPR_UNARY_TAKE_ADDRESS) {
			const expression_t *expression = return_value->unary.value;
			if (is_local_variable(expression)) {
				warningf(&statement->base.source_position,
				         "function returns address of local variable");
			}
		}
	} else {
		if (!is_type_atomic(return_type, ATOMIC_TYPE_VOID)) {
			warningf(&statement->base.source_position,
			         "'return' without value, in function returning non-void");
		}
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

	statement->base.source_position = token.source_position;

	declaration_t *before = last_declaration;
	parse_declaration(record_declaration);

	if (before == NULL) {
		statement->declaration.declarations_begin = scope->declarations;
	} else {
		statement->declaration.declarations_begin = before->next;
	}
	statement->declaration.declarations_end = last_declaration;

	return statement;
}

/**
 * Parse an expression statement, ie. expr ';'.
 */
static statement_t *parse_expression_statement(void)
{
	statement_t *statement = allocate_statement_zero(STATEMENT_EXPRESSION);

	statement->base.source_position  = token.source_position;
	expression_t *const expr         = parse_expression();
	statement->expression.expression = expr;

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
	statement->base.source_position  = token.source_position;
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
		} else switch (la1_type) {
			DECLARATION_START
			case T_IDENTIFIER:
			case '*':
				statement = parse_declaration_statement();
				break;

			default:
				statement = parse_expression_statement();
				break;
		}
		break;
	}

	case T___extension__:
		/* This can be a prefix to a declaration or an expression statement.
		 * We simply eat it now and parse the rest with tail recursion. */
		do {
			next_token();
		} while (token.type == T___extension__);
		statement = parse_statement();
		break;

	DECLARATION_START
		statement = parse_declaration_statement();
		break;

	case ';':        statement = parse_empty_statement();         break;
	case '{':        statement = parse_compound_statement(false); break;
	case T___leave:  statement = parse_leave();                   break;
	case T___try:    statement = parse_ms_try_statment();         break;
	case T_asm:      statement = parse_asm_statement();           break;
	case T_break:    statement = parse_break();                   break;
	case T_case:     statement = parse_case_statement();          break;
	case T_continue: statement = parse_continue();                break;
	case T_default:  statement = parse_default_statement();       break;
	case T_do:       statement = parse_do();                      break;
	case T_for:      statement = parse_for();                     break;
	case T_goto:     statement = parse_goto();                    break;
	case T_if:       statement = parse_if ();                     break;
	case T_return:   statement = parse_return();                  break;
	case T_switch:   statement = parse_switch();                  break;
	case T_while:    statement = parse_while();                   break;
	default:         statement = parse_expression_statement();    break;
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
	statement->base.source_position = token.source_position;

	PUSH_PARENT(statement);

	eat('{');
	add_anchor_token('}');

	int      top        = environment_top();
	scope_t *last_scope = scope;
	set_scope(&statement->compound.scope);

	statement_t **anchor            = &statement->compound.statements;
	bool          only_decls_so_far = true;
	while (token.type != '}' && token.type != T_EOF) {
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

	if (token.type == '}') {
		next_token();
	} else {
		errorf(&statement->base.source_position,
		       "end of file while looking for closing '}'");
	}

	/* look over all statements again to produce no effect warnings */
	if (warning.unused_value) {
		statement_t *sub_statement = statement->compound.statements;
		for( ; sub_statement != NULL; sub_statement = sub_statement->base.next) {
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
	rem_anchor_token('}');
	assert(scope == &statement->compound.scope);
	set_scope(last_scope);
	environment_pop_to(top);

	POP_PARENT;
	return statement;
}

/**
 * Initialize builtin types.
 */
static void initialize_builtin_types(void)
{
	type_intmax_t    = make_global_typedef("__intmax_t__",      type_long_long);
	type_size_t      = make_global_typedef("__SIZE_TYPE__",     type_unsigned_long);
	type_ssize_t     = make_global_typedef("__SSIZE_TYPE__",    type_long);
	type_ptrdiff_t   = make_global_typedef("__PTRDIFF_TYPE__",  type_long);
	type_uintmax_t   = make_global_typedef("__uintmax_t__",     type_unsigned_long_long);
	type_uptrdiff_t  = make_global_typedef("__UPTRDIFF_TYPE__", type_unsigned_long);
	type_wchar_t     = make_global_typedef("__WCHAR_TYPE__",    opt_short_wchar_t ? type_unsigned_short : type_int);
	type_wint_t      = make_global_typedef("__WINT_TYPE__",     type_int);

	type_intmax_t_ptr  = make_pointer_type(type_intmax_t,  TYPE_QUALIFIER_NONE);
	type_ptrdiff_t_ptr = make_pointer_type(type_ptrdiff_t, TYPE_QUALIFIER_NONE);
	type_ssize_t_ptr   = make_pointer_type(type_ssize_t,   TYPE_QUALIFIER_NONE);
	type_wchar_t_ptr   = make_pointer_type(type_wchar_t,   TYPE_QUALIFIER_NONE);

	/* const version of wchar_t */
	type_const_wchar_t = allocate_type_zero(TYPE_TYPEDEF, &builtin_source_position);
	type_const_wchar_t->typedeft.declaration  = type_wchar_t->typedeft.declaration;
	type_const_wchar_t->base.qualifiers      |= TYPE_QUALIFIER_CONST;

	type_const_wchar_t_ptr = make_pointer_type(type_const_wchar_t, TYPE_QUALIFIER_NONE);
}

/**
 * Check for unused global static functions and variables
 */
static void check_unused_globals(void)
{
	if (!warning.unused_function && !warning.unused_variable)
		return;

	for (const declaration_t *decl = global_scope->declarations; decl != NULL; decl = decl->next) {
		if (decl->used                  ||
		    decl->modifiers & DM_UNUSED ||
		    decl->modifiers & DM_USED   ||
		    decl->storage_class != STORAGE_CLASS_STATIC)
			continue;

		type_t *const type = decl->type;
		const char *s;
		if (is_type_function(skip_typeref(type))) {
			if (!warning.unused_function || decl->is_inline)
				continue;

			s = (decl->init.statement != NULL ? "defined" : "declared");
		} else {
			if (!warning.unused_variable)
				continue;

			s = "defined";
		}

		warningf(&decl->source_position, "'%#T' %s but not used",
			type, decl->symbol, s);
	}
}

static void parse_global_asm(void)
{
	eat(T_asm);
	expect('(');

	statement_t *statement          = allocate_statement_zero(STATEMENT_ASM);
	statement->base.source_position = token.source_position;
	statement->asms.asm_text        = parse_string_literals();
	statement->base.next            = unit->global_asm;
	unit->global_asm                = statement;

	expect(')');
	expect(';');

end_error:;
}

/**
 * Parse a translation unit.
 */
static void parse_translation_unit(void)
{
	for (;;) switch (token.type) {
		DECLARATION_START
		case T_IDENTIFIER:
		case T___extension__:
			parse_external_declaration();
			break;

		case T_asm:
			parse_global_asm();
			break;

		case T_EOF:
			return;

		case ';':
			/* TODO error in strict mode */
			warningf(HERE, "stray ';' outside of function");
			next_token();
			break;

		default:
			errorf(HERE, "stray %K outside of function", &token);
			if (token.type == '(' || token.type == '{' || token.type == '[')
				eat_until_matching_token(token.type);
			next_token();
			break;
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

	assert(global_scope == NULL);
	global_scope = &unit->scope;

	assert(scope == NULL);
	set_scope(&unit->scope);

	initialize_builtin_types();
}

translation_unit_t *finish_parsing(void)
{
	assert(scope == &unit->scope);
	scope          = NULL;
	last_declaration = NULL;

	assert(global_scope == &unit->scope);
	check_unused_globals();
	global_scope = NULL;

	DEL_ARR_F(environment_stack);
	DEL_ARR_F(label_stack);

	translation_unit_t *result = unit;
	unit = NULL;
	return result;
}

void parse(void)
{
	lookahead_bufpos = 0;
	for(int i = 0; i < MAX_LOOKAHEAD + 2; ++i) {
		next_token();
	}
	parse_translation_unit();
}

/**
 * Initialize the parser.
 */
void init_parser(void)
{
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
