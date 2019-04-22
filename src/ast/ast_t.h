/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef AST_T_H
#define AST_T_H

#include <assert.h>
#include <libfirm/firm_types.h>

#include "adt/obst.h"
#include "ast.h"
#include "entity_t.h"
#include "symbol.h"
#include "type.h"

/** The AST obstack contains all data that must stay in the AST. */
extern struct obstack ast_obstack;

/**
 * Operator precedence classes
 */
typedef enum precedence_t {
	PREC_BOTTOM,
	PREC_EXPRESSION,     /* ,                                  left to right */
	PREC_ASSIGNMENT,     /* = += -= *= /= %= <<= >>= &= ^= |=  right to left */
	PREC_CONDITIONAL,    /* ?:                                 right to left */
	PREC_LOGICAL_OR,     /* ||                                 left to right */
	PREC_LOGICAL_AND,    /* &&                                 left to right */
	PREC_OR,             /* |                                  left to right */
	PREC_XOR,            /* ^                                  left to right */
	PREC_AND,            /* &                                  left to right */
	PREC_EQUALITY,       /* == !=                              left to right */
	PREC_RELATIONAL,     /* < <= > >=                          left to right */
	PREC_SHIFT,          /* << >>                              left to right */
	PREC_ADDITIVE,       /* + -                                left to right */
	PREC_MULTIPLICATIVE, /* * / %                              left to right */
	PREC_CAST,           /* (type)                             right to left */
	PREC_UNARY,          /* ! ~ ++ -- + - * & sizeof           right to left */
	PREC_POSTFIX,        /* () [] -> .                         left to right */
	PREC_PRIMARY,
	PREC_TOP
} precedence_t;

/**
 * Expression kinds.
 */
typedef enum expression_kind_t {
	EXPR_ERROR = 1,
	EXPR_REFERENCE,
	EXPR_ENUM_CONSTANT,
	EXPR_LITERAL_BOOLEAN,
	EXPR_LITERAL_INTEGER,
	EXPR_LITERAL_FLOATINGPOINT,
	EXPR_LITERAL_CHARACTER,
	EXPR_LITERAL_MS_NOOP, /**< MS __noop extension */
	EXPR_STRING_LITERAL,
	EXPR_COMPOUND_LITERAL,
	EXPR_CALL,
	EXPR_CONDITIONAL,
	EXPR_SELECT,
	EXPR_ARRAY_ACCESS,
	EXPR_SIZEOF,
	EXPR_CLASSIFY_TYPE,
	EXPR_ALIGNOF,
	EXPR_FUNCNAME,
	EXPR_BUILTIN_CONSTANT_P,
	EXPR_BUILTIN_TYPES_COMPATIBLE_P,
	EXPR_OFFSETOF,
	EXPR_VA_START,
	EXPR_VA_ARG,
	EXPR_VA_COPY,
	EXPR_STATEMENT,
	EXPR_LABEL_ADDRESS, /**< GCC extension &&label operator */

	EXPR_UNARY_FIRST,
	EXPR_UNARY_NEGATE = EXPR_UNARY_FIRST,
	EXPR_UNARY_PLUS,
	EXPR_UNARY_COMPLEMENT,
	EXPR_UNARY_NOT,
	EXPR_UNARY_DEREFERENCE,
	EXPR_UNARY_TAKE_ADDRESS,
	EXPR_UNARY_POSTFIX_INCREMENT,
	EXPR_UNARY_POSTFIX_DECREMENT,
	EXPR_UNARY_PREFIX_INCREMENT,
	EXPR_UNARY_PREFIX_DECREMENT,
	EXPR_UNARY_CAST,
	EXPR_UNARY_ASSUME,        /**< MS __assume() */
	EXPR_UNARY_DELETE,
	EXPR_UNARY_DELETE_ARRAY,
	EXPR_UNARY_THROW,
	EXPR_UNARY_REAL,
	EXPR_UNARY_IMAG,
	EXPR_UNARY_LAST = EXPR_UNARY_IMAG,

	EXPR_BINARY_FIRST,
	EXPR_BINARY_ADD = EXPR_BINARY_FIRST,
	EXPR_BINARY_SUB,
	EXPR_BINARY_MUL,
	EXPR_BINARY_DIV,
	EXPR_BINARY_MOD,
	EXPR_BINARY_EQUAL,
	EXPR_BINARY_NOTEQUAL,
	EXPR_BINARY_LESS,
	EXPR_BINARY_LESSEQUAL,
	EXPR_BINARY_GREATER,
	EXPR_BINARY_GREATEREQUAL,
	EXPR_BINARY_BITWISE_AND,
	EXPR_BINARY_BITWISE_OR,
	EXPR_BINARY_BITWISE_XOR,
	EXPR_BINARY_LOGICAL_AND,
	EXPR_BINARY_LOGICAL_OR,
	EXPR_BINARY_SHIFTLEFT,
	EXPR_BINARY_SHIFTRIGHT,
	EXPR_BINARY_ASSIGN,
	EXPR_BINARY_MUL_ASSIGN,
	EXPR_BINARY_DIV_ASSIGN,
	EXPR_BINARY_MOD_ASSIGN,
	EXPR_BINARY_ADD_ASSIGN,
	EXPR_BINARY_SUB_ASSIGN,
	EXPR_BINARY_SHIFTLEFT_ASSIGN,
	EXPR_BINARY_SHIFTRIGHT_ASSIGN,
	EXPR_BINARY_BITWISE_AND_ASSIGN,
	EXPR_BINARY_BITWISE_XOR_ASSIGN,
	EXPR_BINARY_BITWISE_OR_ASSIGN,
	EXPR_BINARY_COMMA,
	EXPR_BINARY_ISGREATER,
	EXPR_BINARY_ISGREATEREQUAL,
	EXPR_BINARY_ISLESS,
	EXPR_BINARY_ISLESSEQUAL,
	EXPR_BINARY_ISLESSGREATER,
	EXPR_BINARY_ISUNORDERED,
	EXPR_BINARY_LAST = EXPR_BINARY_ISUNORDERED,
} expression_kind_t;

typedef enum funcname_kind_t {
	FUNCNAME_FUNCTION,         /**< C99 __func__, older __FUNCTION__ */
	FUNCNAME_PRETTY_FUNCTION,  /**< GNUC __PRETTY_FUNCTION__ */
	FUNCNAME_FUNCSIG,          /**< MS __FUNCSIG__ */
	FUNCNAME_FUNCDNAME         /**< MS __FUNCDNAME__ */
} funcname_kind_t;

/* convenience macros */
#define EXPR_BINARY_CASES                \
	     EXPR_BINARY_ADD:                \
	case EXPR_BINARY_SUB:                \
	case EXPR_BINARY_MUL:                \
	case EXPR_BINARY_DIV:                \
	case EXPR_BINARY_MOD:                \
	case EXPR_BINARY_EQUAL:              \
	case EXPR_BINARY_NOTEQUAL:           \
	case EXPR_BINARY_LESS:               \
	case EXPR_BINARY_LESSEQUAL:          \
	case EXPR_BINARY_GREATER:            \
	case EXPR_BINARY_GREATEREQUAL:       \
	case EXPR_BINARY_BITWISE_AND:        \
	case EXPR_BINARY_BITWISE_OR:         \
	case EXPR_BINARY_BITWISE_XOR:        \
	case EXPR_BINARY_LOGICAL_AND:        \
	case EXPR_BINARY_LOGICAL_OR:         \
	case EXPR_BINARY_SHIFTLEFT:          \
	case EXPR_BINARY_SHIFTRIGHT:         \
	case EXPR_BINARY_ASSIGN:             \
	case EXPR_BINARY_MUL_ASSIGN:         \
	case EXPR_BINARY_DIV_ASSIGN:         \
	case EXPR_BINARY_MOD_ASSIGN:         \
	case EXPR_BINARY_ADD_ASSIGN:         \
	case EXPR_BINARY_SUB_ASSIGN:         \
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:   \
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:  \
	case EXPR_BINARY_BITWISE_AND_ASSIGN: \
	case EXPR_BINARY_BITWISE_XOR_ASSIGN: \
	case EXPR_BINARY_BITWISE_OR_ASSIGN:  \
	case EXPR_BINARY_COMMA:              \
	case EXPR_BINARY_ISGREATER:          \
	case EXPR_BINARY_ISGREATEREQUAL:     \
	case EXPR_BINARY_ISLESS:             \
	case EXPR_BINARY_ISLESSEQUAL:        \
	case EXPR_BINARY_ISLESSGREATER:      \
	case EXPR_BINARY_ISUNORDERED

/**
 * unary expression with mandatory operand
 */
#define EXPR_UNARY_CASES_MANDATORY     \
	     EXPR_UNARY_NEGATE:            \
	case EXPR_UNARY_PLUS:              \
	case EXPR_UNARY_COMPLEMENT:        \
	case EXPR_UNARY_NOT:               \
	case EXPR_UNARY_DEREFERENCE:       \
	case EXPR_UNARY_TAKE_ADDRESS:      \
	case EXPR_UNARY_POSTFIX_INCREMENT: \
	case EXPR_UNARY_POSTFIX_DECREMENT: \
	case EXPR_UNARY_PREFIX_INCREMENT:  \
	case EXPR_UNARY_PREFIX_DECREMENT:  \
	case EXPR_UNARY_CAST:              \
	case EXPR_UNARY_ASSUME:            \
	case EXPR_UNARY_DELETE:            \
	case EXPR_UNARY_DELETE_ARRAY:      \
	case EXPR_UNARY_IMAG:              \
	case EXPR_UNARY_REAL

/**
 * unary expression with optional operand
 */
#define EXPR_UNARY_CASES_OPTIONAL \
	EXPR_UNARY_THROW

#define EXPR_UNARY_CASES             \
	     EXPR_UNARY_CASES_MANDATORY: \
	case EXPR_UNARY_CASES_OPTIONAL

#define EXPR_LITERAL_CASES                       \
	     EXPR_LITERAL_BOOLEAN:                   \
	case EXPR_LITERAL_INTEGER:                   \
	case EXPR_LITERAL_FLOATINGPOINT:             \
	case EXPR_LITERAL_MS_NOOP

/**
 * The base class of every expression.
 */
struct expression_base_t {
	expression_kind_t kind;     /**< The expression kind. */
	type_t           *type;     /**< The type of the expression. */
	position_t        pos;      /**< The source position of this expression. */
	bool              parenthesized : 1;
	/** compiler generated expression. Examples: select into anonymous structs,
	 * implicit casts */
	bool              implicit      : 1;
#ifndef NDEBUG
	/** Set if this expression was already transformed in ast2firm. */
	bool              transformed   : 1;
#endif
};

/**
 * integer, float and boolean constants
 */
struct literal_expression_t {
	expression_base_t base;
	const string_t   *value;
	char const       *suffix; /**< Start of the suffix in value. */

	/* ast2firm data */
	ir_tarval        *target_value;
};

/**
 * string and character literals
 */
struct string_literal_expression_t {
	expression_base_t base;
	string_t         *value;
};

struct funcname_expression_t {
	expression_base_t  base;
	funcname_kind_t    kind;
};

struct compound_literal_expression_t {
	expression_base_t  base;
	type_t            *type;
	initializer_t     *initializer;
	bool               global_scope;
};

struct builtin_constant_expression_t {
	expression_base_t  base;
	expression_t      *value;
};

struct builtin_types_compatible_expression_t {
	expression_base_t  base;
	type_t            *left;
	type_t            *right;
};

struct reference_expression_t {
	expression_base_t  base;
	entity_t          *entity;
};

/**
 * An argument of a call.
 */
struct call_argument_t {
	expression_t    *expression; /**< The expression which value is transmitted */
	call_argument_t *next;       /**< Links to the next argument of this call. */
};


struct call_expression_t {
	expression_base_t  base;
	expression_t      *function;  /**< The address of the function to call. */
	call_argument_t   *arguments; /**< List of arguments of this call. */
	function_type_t   *concrete_type; /**< concrete type in case of a generic
	                                       builtin function */
};


struct unary_expression_t {
	expression_base_t  base;
	expression_t      *value;     /**< The unary operand. */
};

struct binary_expression_t {
	expression_base_t  base;
	expression_t      *left;
	expression_t      *right;
};

struct select_expression_t {
	expression_base_t  base;
	expression_t      *compound;
	entity_t          *compound_entry;
};

struct array_access_expression_t {
	expression_base_t  base;
	expression_t      *array_ref;   /**< the referenced array */
	expression_t      *index;       /**< the index used */
	/** True if access was written in a 5[a] way */
	bool               flipped : 1;
};

struct typeprop_expression_t {
	expression_base_t  base;
	type_t            *type;
	expression_t      *tp_expression;
};

struct designator_t {
	position_t    pos;
	symbol_t     *symbol;      /**< the symbol if any */
	expression_t *array_index; /**< the array index if any */
	expression_t *range_last;  /**< last index of a range initializer, if any */
	designator_t *next;
};

struct offsetof_expression_t {
	expression_base_t  base;
	type_t            *type;
	designator_t      *designator;
};

struct va_start_expression_t {
	expression_base_t  base;
	expression_t      *ap;
	expression_t      *parameter;
};

struct va_arg_expression_t {
	expression_base_t  base;
	expression_t      *ap;
};

struct va_copy_expression_t {
	expression_base_t  base;
	expression_t      *dst;    /**< destination argument */
	expression_t      *src;    /**< source argument */
};

struct conditional_expression_t {
	expression_base_t  base;
	expression_t      *condition;
	expression_t      *true_expression;
	expression_t      *false_expression;
};

struct statement_expression_t {
	expression_base_t  base;
	statement_t       *statement;
};

struct classify_type_expression_t {
	expression_base_t  base;
	expression_t      *type_expression;
};

struct label_address_expression_t {
	expression_base_t  base;
	label_t           *label;
};

union expression_t {
	expression_kind_t                     kind;
	expression_base_t                     base;
	literal_expression_t                  literal;
	string_literal_expression_t           string_literal;
	funcname_expression_t                 funcname;
	compound_literal_expression_t         compound_literal;
	builtin_constant_expression_t         builtin_constant;
	builtin_types_compatible_expression_t builtin_types_compatible;
	reference_expression_t                reference;
	call_expression_t                     call;
	unary_expression_t                    unary;
	binary_expression_t                   binary;
	select_expression_t                   select;
	array_access_expression_t             array_access;
	typeprop_expression_t                 typeprop;
	offsetof_expression_t                 offsetofe;
	va_start_expression_t                 va_starte;
	va_arg_expression_t                   va_arge;
	va_copy_expression_t                  va_copye;
	conditional_expression_t              conditional;
	statement_expression_t                statement;
	classify_type_expression_t            classify_type;
	label_address_expression_t            label_address;
};

typedef enum initializer_kind_t {
	INITIALIZER_VALUE,
	INITIALIZER_LIST,
	INITIALIZER_STRING,
	INITIALIZER_DESIGNATOR
} initializer_kind_t;

struct initializer_base_t {
	initializer_kind_t kind;
	position_t         pos;
};

struct initializer_value_t {
	initializer_base_t  base;
	expression_t       *value;
};

struct initializer_list_t {
	initializer_base_t  base;
	size_t              len;
	initializer_t      *initializers[];
};

struct initializer_designator_t {
	initializer_base_t  base;
	designator_t       *designator;
};

union initializer_t {
	initializer_kind_t       kind;
	initializer_base_t       base;
	initializer_value_t      value;
	initializer_list_t       list;
	initializer_designator_t designator;
};

static inline string_literal_expression_t const *get_init_string(initializer_t const *const init)
{
	assert(init->kind == INITIALIZER_STRING);
	assert(init->value.value->kind == EXPR_STRING_LITERAL);
	return &init->value.value->string_literal;
}

/**
 * The statement kinds.
 */
typedef enum statement_kind_t {
	STATEMENT_ERROR = 1,
	STATEMENT_ASM,
	STATEMENT_BREAK,
	STATEMENT_CASE_LABEL,
	STATEMENT_COMPOUND,
	STATEMENT_COMPUTED_GOTO,
	STATEMENT_CONTINUE,
	STATEMENT_DECLARATION,
	STATEMENT_DO_WHILE,
	STATEMENT_EMPTY,
	STATEMENT_EXPRESSION,
	STATEMENT_FOR,
	STATEMENT_GOTO,
	STATEMENT_IF,
	STATEMENT_LABEL,
	STATEMENT_RETURN,
	STATEMENT_SWITCH,
} statement_kind_t;

/**
 * The base class of every statement.
 */
struct statement_base_t {
	ENUMBF(statement_kind_t) kind        : 8;
	bool                     reachable   : 1;
#ifndef NDEBUG
	bool                     transformed : 1;
#endif
	/** Pointer to next statement in a compound statement. */
	statement_t             *next;
	position_t               pos;
	/** The Parent statement that controls the execution. */
	statement_t             *parent;
};

struct return_statement_t {
	statement_base_t  base;
	expression_t     *value;    /**< The return value if any. */
};

struct compound_statement_t {
	statement_base_t  base;
	statement_t      *statements;
	scope_t           scope;
	/** True if this compound statement is a statement expression. */
	bool              stmt_expr : 1;
};

struct declaration_statement_t {
	statement_base_t  base;
	entity_t         *declarations_begin;
	entity_t         *declarations_end;
};

struct if_statement_t {
	statement_base_t  base;
	scope_t           scope;
	expression_t     *condition;
	statement_t      *true_statement;
	statement_t      *false_statement;
};

struct switch_statement_t {
	statement_base_t        base;
	scope_t                 scope;
	expression_t           *expression;
	statement_t            *body;
	/** List of all cases, including default. */
	case_label_statement_t *first_case;
	case_label_statement_t *last_case;
	case_label_statement_t *default_label; /**< Default label if existent. */
};

struct goto_statement_t {
	statement_base_t  base;
	label_t          *label;  /**< The destination label. */
};

struct computed_goto_statement_t {
	statement_base_t  base;
	expression_t     *expression; /**< The expression for the computed goto. */
};

struct case_label_statement_t {
	statement_base_t        base;
	/** The case label expression, NULL for default label. */
	expression_t           *expression;
	/** For GNUC case a .. b: the end range expression, NULL else. */
	expression_t           *end_range;
	/** link to the next case label in switch */
	case_label_statement_t *next;
	statement_t            *statement;
	ir_tarval              *first_case;
	ir_tarval              *last_case;
	unsigned                pn;
	/** If set marked as bad to suppress warnings. */
	bool                   is_bad         : 1;
	/** If set marked this as an empty range. */
	bool                   is_empty_range : 1;
};

struct label_statement_t {
	statement_base_t  base;
	label_t          *label;
	statement_t      *statement;
};

struct expression_statement_t {
	statement_base_t  base;
	expression_t     *expression;
};

struct do_while_statement_t {
	statement_base_t  base;
	scope_t           scope;
	expression_t     *condition;
	statement_t      *body;
};

struct for_statement_t {
	statement_base_t  base;
	scope_t           scope;
	expression_t     *initialisation;
	expression_t     *condition;
	expression_t     *step;
	statement_t      *body;
	bool              condition_reachable : 1;
	bool              step_reachable      : 1;
};

struct asm_clobber_t {
	string_t      *clobber;
	asm_clobber_t *next;
};

struct asm_statement_t {
	statement_base_t base;
	position_t       textpos;
	string_t        *asm_text;
	string_t        *normalized_text; /**< asm_text with %[symbol] references
	                                       replaced by %XX number references */
	entity_t        *inputs;  /**< list of asm_argument_t entities */
	entity_t        *outputs; /**< list of asm_argument_t entities */
	asm_clobber_t   *clobbers;
	entity_t        *labels;
	bool             has_arguments : 1;
	bool             is_volatile   : 1;
};

union statement_t {
	ENUMBF(statement_kind_t)  kind : 8;
	statement_base_t          base;
	return_statement_t        returns;
	compound_statement_t      compound;
	declaration_statement_t   declaration;
	if_statement_t            ifs;
	switch_statement_t        switchs;
	computed_goto_statement_t computed_goto;
	goto_statement_t          gotos;
	case_label_statement_t    case_label;
	label_statement_t         label;
	expression_statement_t    expression;
	do_while_statement_t      do_while;
	for_statement_t           fors;
	asm_statement_t           asms;
};

struct translation_unit_t {
	scope_t      scope;
	statement_t *global_asm;
};

/**
 * Allocate an AST node with given size and
 * initialize all fields with zero.
 */
static inline void *allocate_ast_zero(size_t size)
{
	assert(obstack_object_size(&ast_obstack) == 0);
	return memset(obstack_alloc(&ast_obstack, size), 0, size);
}

/** If set, implicit casts are printed. */
extern bool print_implicit_casts;
/** If set parenthesis are printed to indicate operator precedence. */
extern bool print_parenthesis;

#endif
