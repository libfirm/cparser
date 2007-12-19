#ifndef AST_T_H
#define AST_T_H

#include <libfirm/firm_types.h>
#include <assert.h>

#include "ast.h"
#include "symbol.h"
#include "token_t.h"
#include "type.h"
#include "adt/obst.h"

extern struct obstack ast_obstack;

typedef enum {
	EXPR_UNKNOWN = 0,
	EXPR_INVALID,
	EXPR_REFERENCE,
	EXPR_CONST,
	EXPR_STRING_LITERAL,
	EXPR_WIDE_STRING_LITERAL,
	EXPR_CALL,
	EXPR_CONDITIONAL,
	EXPR_SELECT,
	EXPR_ARRAY_ACCESS,
	EXPR_SIZEOF,
	EXPR_CLASSIFY_TYPE,
	EXPR_ALIGNOF,

	EXPR_FUNCTION,
	EXPR_PRETTY_FUNCTION,
	EXPR_BUILTIN_SYMBOL,
	EXPR_BUILTIN_CONSTANT_P,
	EXPR_BUILTIN_PREFETCH,
	EXPR_OFFSETOF,
	EXPR_VA_START,
	EXPR_VA_ARG,
	EXPR_STATEMENT,

	EXPR_UNARY_FIRST,
	EXPR_UNARY_NEGATE = EXPR_UNARY_FIRST,
	EXPR_UNARY_PLUS,
	EXPR_UNARY_BITWISE_NEGATE,
	EXPR_UNARY_NOT,
	EXPR_UNARY_DEREFERENCE,
	EXPR_UNARY_TAKE_ADDRESS,
	EXPR_UNARY_POSTFIX_INCREMENT,
	EXPR_UNARY_POSTFIX_DECREMENT,
	EXPR_UNARY_PREFIX_INCREMENT,
	EXPR_UNARY_PREFIX_DECREMENT,
	EXPR_UNARY_CAST,
	EXPR_UNARY_CAST_IMPLICIT, /**< compiler generated cast */
	EXPR_UNARY_ASSUME,        /**< MS __assume() */
	EXPR_UNARY_BITFIELD_EXTRACT,
	EXPR_UNARY_LAST = EXPR_UNARY_BITFIELD_EXTRACT,

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

	EXPR_BINARY_BUILTIN_EXPECT,
	EXPR_BINARY_ISGREATER,
	EXPR_BINARY_ISGREATEREQUAL,
	EXPR_BINARY_ISLESS,
	EXPR_BINARY_ISLESSEQUAL,
	EXPR_BINARY_ISLESSGREATER,
	EXPR_BINARY_ISUNORDERED,
	EXPR_BINARY_LAST = EXPR_BINARY_ISUNORDERED,
} expression_kind_t;

/* convenience macros */
#define EXPR_BINARY_CASES                  \
	case EXPR_BINARY_ADD:                  \
	case EXPR_BINARY_SUB:                  \
	case EXPR_BINARY_MUL:                  \
	case EXPR_BINARY_DIV:                  \
	case EXPR_BINARY_MOD:                  \
	case EXPR_BINARY_EQUAL:                \
	case EXPR_BINARY_NOTEQUAL:             \
	case EXPR_BINARY_LESS:                 \
	case EXPR_BINARY_LESSEQUAL:            \
	case EXPR_BINARY_GREATER:              \
	case EXPR_BINARY_GREATEREQUAL:         \
	case EXPR_BINARY_BITWISE_AND:          \
	case EXPR_BINARY_BITWISE_OR:           \
	case EXPR_BINARY_BITWISE_XOR:          \
	case EXPR_BINARY_LOGICAL_AND:          \
	case EXPR_BINARY_LOGICAL_OR:           \
	case EXPR_BINARY_SHIFTLEFT:            \
	case EXPR_BINARY_SHIFTRIGHT:           \
	case EXPR_BINARY_ASSIGN:               \
	case EXPR_BINARY_MUL_ASSIGN:           \
	case EXPR_BINARY_DIV_ASSIGN:           \
	case EXPR_BINARY_MOD_ASSIGN:           \
	case EXPR_BINARY_ADD_ASSIGN:           \
	case EXPR_BINARY_SUB_ASSIGN:           \
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:     \
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:    \
	case EXPR_BINARY_BITWISE_AND_ASSIGN:   \
	case EXPR_BINARY_BITWISE_XOR_ASSIGN:   \
	case EXPR_BINARY_BITWISE_OR_ASSIGN:    \
	case EXPR_BINARY_COMMA:                \
	case EXPR_BINARY_BUILTIN_EXPECT:       \
	case EXPR_BINARY_ISGREATER:            \
	case EXPR_BINARY_ISGREATEREQUAL:       \
	case EXPR_BINARY_ISLESS:               \
	case EXPR_BINARY_ISLESSEQUAL:          \
	case EXPR_BINARY_ISLESSGREATER:        \
	case EXPR_BINARY_ISUNORDERED:

#define EXPR_UNARY_CASES                   \
	case EXPR_UNARY_NEGATE:                \
	case EXPR_UNARY_PLUS:                  \
	case EXPR_UNARY_BITWISE_NEGATE:        \
	case EXPR_UNARY_NOT:                   \
	case EXPR_UNARY_DEREFERENCE:           \
	case EXPR_UNARY_TAKE_ADDRESS:          \
	case EXPR_UNARY_POSTFIX_INCREMENT:     \
	case EXPR_UNARY_POSTFIX_DECREMENT:     \
	case EXPR_UNARY_PREFIX_INCREMENT:      \
	case EXPR_UNARY_PREFIX_DECREMENT:      \
	case EXPR_UNARY_CAST:                  \
	case EXPR_UNARY_CAST_IMPLICIT:         \
	case EXPR_UNARY_ASSUME:                \
	case EXPR_UNARY_BITFIELD_EXTRACT:

struct scope_t {
	declaration_t *declarations;  /**< List of declarations in this scope. */
};

struct expression_base_t {
	expression_kind_t   kind;
	type_t             *type;
	source_position_t   source_position;
};

struct const_expression_t {
	expression_base_t  base;
	union {
		long long   int_value;
		long double float_value;
	} v;
};

struct string_literal_expression_t {
	expression_base_t  base;
	string_t           value;
};

struct wide_string_literal_expression_t {
	expression_base_t  base;
	wide_string_t      value;
};

struct builtin_symbol_expression_t {
	expression_base_t  base;
	symbol_t          *symbol;
};

struct builtin_constant_expression_t {
	expression_base_t  base;
	expression_t      *value;
};

struct builtin_prefetch_expression_t {
	expression_base_t  base;
	expression_t      *adr;
	expression_t      *rw;
	expression_t      *locality;
};

struct reference_expression_t {
	expression_base_t  base;
	symbol_t          *symbol;
	declaration_t     *declaration;
};

struct call_argument_t {
	expression_t    *expression;
	call_argument_t *next;
};

struct call_expression_t {
	expression_base_t  base;
	expression_t      *function;
	call_argument_t   *arguments;
};

struct unary_expression_t {
	expression_base_t  base;
	expression_t      *value;
};

struct binary_expression_t {
	expression_base_t  base;
	expression_t      *left;
	expression_t      *right;
};

struct select_expression_t {
	expression_base_t  base;
	expression_t      *compound;
	symbol_t          *symbol;

	declaration_t     *compound_entry;
};

struct array_access_expression_t {
	expression_base_t  base;
	expression_t      *array_ref;
	expression_t      *index;
	bool               flipped; /* index/ref was written in a 5[a] way */
};

struct typeprop_expression_t {
	expression_base_t  base;
	type_t            *type;
	expression_t      *tp_expression;
};

struct designator_t {
	symbol_t     *symbol;
	expression_t *array_access;
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
	declaration_t     *parameter;
};

struct va_arg_expression_t {
	expression_base_t  base;
	expression_t      *ap;
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

union expression_t {
	expression_kind_t                kind;
	expression_base_t                base;
	const_expression_t               conste;
	string_literal_expression_t      string;
	wide_string_literal_expression_t wide_string;
	builtin_symbol_expression_t      builtin_symbol;
	builtin_constant_expression_t    builtin_constant;
	builtin_prefetch_expression_t    builtin_prefetch;
	reference_expression_t           reference;
	call_expression_t                call;
	unary_expression_t               unary;
	binary_expression_t              binary;
	select_expression_t              select;
	array_access_expression_t        array_access;
	typeprop_expression_t            typeprop;
	offsetof_expression_t            offsetofe;
	va_start_expression_t            va_starte;
	va_arg_expression_t              va_arge;
	conditional_expression_t         conditional;
	statement_expression_t           statement;
	classify_type_expression_t       classify_type;
};

typedef enum {
	STORAGE_CLASS_NONE,
	STORAGE_CLASS_TYPEDEF,
	STORAGE_CLASS_EXTERN,
	STORAGE_CLASS_STATIC,
	STORAGE_CLASS_AUTO,
	STORAGE_CLASS_REGISTER,
	STORAGE_CLASS_ENUM_ENTRY,
	STORAGE_CLASS_THREAD,
	STORAGE_CLASS_THREAD_EXTERN,
	STORAGE_CLASS_THREAD_STATIC
} storage_class_tag_t;

typedef enum {
	NAMESPACE_NORMAL,
	NAMESPACE_STRUCT,
	NAMESPACE_UNION,
	NAMESPACE_ENUM,
	NAMESPACE_LABEL,
} namespace_t;

typedef enum {
	INITIALIZER_VALUE,
	INITIALIZER_LIST,
	INITIALIZER_STRING,
	INITIALIZER_WIDE_STRING
} initializer_kind_t;

struct initializer_base_t {
	initializer_kind_t kind;
};

struct initializer_value_t {
	initializer_base_t  initializer;
	expression_t       *value;
};

struct initializer_list_t {
	initializer_base_t  initializer;
	size_t              len;
	initializer_t      *initializers[];
};

struct initializer_string_t {
	initializer_base_t initializer;
	string_t           string;
};

struct initializer_wide_string_t {
	initializer_base_t  initializer;
	wide_string_t       string;
};

union initializer_t {
	initializer_kind_t        kind;
	initializer_base_t        base;
	initializer_value_t       value;
	initializer_list_t        list;
	initializer_string_t      string;
	initializer_wide_string_t wide_string;
};

typedef enum {
	DM_DLLIMPORT   = (1 << 0),
	DM_DLLEXPORT   = (1 << 1),
	DM_THREAD      = (1 << 2),
	DM_NAKED       = (1 << 3),
	DM_FORCEINLINE = (1 << 4),
	DM_NOTHROW     = (1 << 5),
	DM_NORETURN    = (1 << 6),
	DM_NOINLINE    = (1 << 7)
} decl_modifier_t;

typedef unsigned short decl_modifiers_t;

struct declaration_t {
	unsigned char       namespc;
	unsigned char       storage_class;
	decl_modifiers_t    modifiers;
	unsigned int        address_taken : 1;
	unsigned int        is_inline     : 1;
	unsigned int        used          : 1;  /**< Set if the declaration is used. */
	type_t             *type;
	symbol_t           *symbol;
	source_position_t   source_position;
	union {
		bool            is_defined;
		statement_t    *statement;
		initializer_t  *initializer;
		expression_t   *enum_value;
	} init;
	scope_t             scope;
	scope_t            *parent_scope;

	/** next declaration in a scope */
	declaration_t      *next;
	/** next declaration with same symbol */
	declaration_t      *symbol_next;

	/* the following fields are used in ast2firm module */
	unsigned char       declaration_kind;
	union {
		unsigned int    value_number;
		ir_entity      *entity;
		ir_node        *block;
		tarval         *enum_val;
	} v;
};

typedef enum {
	STATEMENT_INVALID,
	STATEMENT_COMPOUND,
	STATEMENT_RETURN,
	STATEMENT_DECLARATION,
	STATEMENT_IF,
	STATEMENT_SWITCH,
	STATEMENT_EXPRESSION,
	STATEMENT_CONTINUE,
	STATEMENT_BREAK,
	STATEMENT_GOTO,
	STATEMENT_LABEL,
	STATEMENT_CASE_LABEL,
	STATEMENT_WHILE,
	STATEMENT_DO_WHILE,
	STATEMENT_FOR,
	STATEMENT_ASM
} statement_kind_t;

struct statement_base_t {
	statement_kind_t   kind;
	statement_t       *next;
	source_position_t  source_position;
};

struct return_statement_t {
	statement_base_t  base;
	expression_t     *value;
};

struct compound_statement_t {
	statement_base_t  base;
	statement_t      *statements;
	scope_t           scope;
};

struct declaration_statement_t {
	statement_base_t  base;
	declaration_t    *declarations_begin;
	declaration_t    *declarations_end;
};

struct if_statement_t {
	statement_base_t  base;
	expression_t     *condition;
	statement_t      *true_statement;
	statement_t      *false_statement;
};

struct switch_statement_t {
	statement_base_t       base;
	expression_t           *expression;
	statement_t            *body;
	case_label_statement_t *first_case, *last_case;
};

struct goto_statement_t {
	statement_base_t  base;
	declaration_t    *label;     /**< The destination label. */
	goto_statement_t *next;      /**< links all goto statements of a function */
};

struct case_label_statement_t {
	statement_base_t        base;
	expression_t           *expression;  /**< The case label expression, NULL for default label. */
	expression_t           *end_range;   /**< For GNUC case a .. b: the end range expression, NULL else. */
	statement_t            *statement;
	case_label_statement_t *next; /**< link to the next case label in switch */
};

struct label_statement_t {
	statement_base_t   base;
	declaration_t     *label;
	statement_t       *statement;
	label_statement_t *next;    /**< links all label statements of a function */
};

struct expression_statement_t {
	statement_base_t  base;
	expression_t     *expression;
};

struct while_statement_t {
	statement_base_t  base;
	expression_t     *condition;
	statement_t      *body;
};

struct do_while_statement_t {
	statement_base_t  base;
	expression_t     *condition;
	statement_t      *body;
};

struct for_statement_t {
	statement_base_t  base;
	expression_t     *initialisation;
	expression_t     *condition;
	expression_t     *step;
	statement_t      *body;
	scope_t           scope;
};

struct asm_constraint_t {
	string_t          constraints;
	expression_t     *expression;
	symbol_t         *symbol;
	asm_constraint_t *next;
};

struct asm_clobber_t {
	string_t       clobber;
	asm_clobber_t *next;
};

struct asm_statement_t {
	statement_base_t  base;
	string_t          asm_text;
	asm_constraint_t *inputs;
	asm_constraint_t *outputs;
	asm_clobber_t    *clobbers;
	bool              is_volatile;
};

union statement_t {
	statement_kind_t         kind;
	statement_base_t         base;
	return_statement_t       returns;
	compound_statement_t     compound;
	declaration_statement_t  declaration;
	if_statement_t           ifs;
	switch_statement_t       switchs;
	goto_statement_t         gotos;
	case_label_statement_t   case_label;
	label_statement_t        label;
	expression_statement_t   expression;
	while_statement_t        whiles;
	do_while_statement_t     do_while;
	for_statement_t          fors;
	asm_statement_t          asms;
};

struct translation_unit_t {
	scope_t scope;
};

static inline
void *_allocate_ast(size_t size)
{
	return obstack_alloc(&ast_obstack, size);
}

#define allocate_ast(size)                 _allocate_ast(size)

#endif
