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
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_CONDITIONAL,
	EXPR_SELECT,
	EXPR_ARRAY_ACCESS,
	EXPR_SIZEOF,
	EXPR_CLASSIFY_TYPE,

	EXPR_FUNCTION,
	EXPR_PRETTY_FUNCTION,
	EXPR_BUILTIN_SYMBOL,
	EXPR_OFFSETOF,
	EXPR_VA_ARG,
	EXPR_STATEMENT
} expression_type_t;

struct context_t {
	declaration_t   *declarations;
};

struct expression_base_t {
	expression_type_t   type;
	type_t             *datatype;
	source_position_t   source_position;
};

struct const_expression_t {
	expression_base_t  expression;
	union {
		long long   int_value;
		long double float_value;
	} v;
};

struct string_literal_expression_t {
	expression_base_t  expression;
	const char        *value;
};

struct wide_string_literal_expression_t {
	expression_base_t  expression;
	wide_string_t      value;
};

struct builtin_symbol_expression_t {
	expression_base_t  expression;
	symbol_t          *symbol;
};

struct reference_expression_t {
	expression_base_t  expression;
	symbol_t          *symbol;
	declaration_t     *declaration;
};

struct call_argument_t {
	expression_t    *expression;
	call_argument_t *next;
};

struct call_expression_t {
	expression_base_t  expression;
	expression_t      *function;
	call_argument_t   *arguments;
};

typedef enum {
	UNEXPR_INVALID = 0,
	UNEXPR_NEGATE,
	UNEXPR_PLUS,
	UNEXPR_BITWISE_NEGATE,
	UNEXPR_NOT,
	UNEXPR_DEREFERENCE,
	UNEXPR_TAKE_ADDRESS,
	UNEXPR_POSTFIX_INCREMENT,
	UNEXPR_POSTFIX_DECREMENT,
	UNEXPR_PREFIX_INCREMENT,
	UNEXPR_PREFIX_DECREMENT,
	UNEXPR_CAST,
	UNEXPR_CAST_IMPLICIT /* compiler generated cast */
} unary_expression_type_t;

struct unary_expression_t {
	expression_base_t        expression;
	unary_expression_type_t  type;
	expression_t            *value;
};

typedef enum {
	BINEXPR_INVALID = 0,
	BINEXPR_ADD,
	BINEXPR_SUB,
	BINEXPR_MUL,
	BINEXPR_DIV,
	BINEXPR_MOD,
	BINEXPR_EQUAL,
	BINEXPR_NOTEQUAL,
	BINEXPR_LESS,
	BINEXPR_LESSEQUAL,
	BINEXPR_GREATER,
	BINEXPR_GREATEREQUAL,
	BINEXPR_BITWISE_AND,
	BINEXPR_BITWISE_OR,
	BINEXPR_BITWISE_XOR,
	BINEXPR_LOGICAL_AND,
	BINEXPR_LOGICAL_OR,
	BINEXPR_SHIFTLEFT,
	BINEXPR_SHIFTRIGHT,
	BINEXPR_ASSIGN,
	BINEXPR_MUL_ASSIGN,
	BINEXPR_DIV_ASSIGN,
	BINEXPR_MOD_ASSIGN,
	BINEXPR_ADD_ASSIGN,
	BINEXPR_SUB_ASSIGN,
	BINEXPR_SHIFTLEFT_ASSIGN,
	BINEXPR_SHIFTRIGHT_ASSIGN,
	BINEXPR_BITWISE_AND_ASSIGN,
	BINEXPR_BITWISE_XOR_ASSIGN,
	BINEXPR_BITWISE_OR_ASSIGN,
	BINEXPR_COMMA
} binary_expression_type_t;

struct binary_expression_t {
	expression_base_t         expression;
	binary_expression_type_t  type;
	expression_t             *left;
	expression_t             *right;
};

struct select_expression_t {
	expression_base_t  expression;
	expression_t      *compound;
	symbol_t          *symbol;

	declaration_t     *compound_entry;
};

struct array_access_expression_t {
	expression_base_t  expression;
	expression_t      *array_ref;
	expression_t      *index;
	bool               flipped; /* index/ref was written in a 5[a] way */
};

struct sizeof_expression_t {
	expression_base_t  expression;
	type_t            *type;
	expression_t      *size_expression;
};

struct designator_t {
	symbol_t     *symbol;
	expression_t *array_access;
	designator_t *next;
};

struct offsetof_expression_t {
	expression_base_t  expression;
	type_t            *type;
	designator_t      *designator;
};

struct va_arg_expression_t {
	expression_base_t  expression;
	expression_t      *arg;
	type_t            *type;
};

struct conditional_expression_t {
	expression_base_t  expression;
	expression_t      *condition;
	expression_t      *true_expression;
	expression_t      *false_expression;
};

struct statement_expression_t {
	expression_base_t  expression;
	statement_t       *statement;
};

struct classify_type_expression_t {
	expression_base_t  expression;
	expression_t      *type_expression;
};

union expression_t {
	expression_type_t                type;
	expression_base_t                base;
	const_expression_t               conste;
	string_literal_expression_t      string;
	wide_string_literal_expression_t wide_string;
	builtin_symbol_expression_t      builtin_symbol;
	reference_expression_t           reference;
	call_expression_t                call;
	unary_expression_t               unary;
	binary_expression_t              binary;
	select_expression_t              select;
	array_access_expression_t        array_access;
	sizeof_expression_t              sizeofe;
	offsetof_expression_t            offsetofe;
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
} initializer_type_t;

struct initializer_base_t {
	initializer_type_t type;
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
	initializer_base_t  initializer;
	const char         *string;
};

struct initializer_wide_string_t {
	initializer_base_t  initializer;
	wide_string_t       string;
};

union initializer_t {
	initializer_type_t        type;
	initializer_base_t        base;
	initializer_value_t       value;
	initializer_list_t        list;
	initializer_string_t      string;
	initializer_wide_string_t wide_string;
};

struct declaration_t {
	unsigned char       namespc;
	unsigned char       storage_class;
	unsigned int        address_taken : 1;
	unsigned int        is_inline     : 1;
	type_t             *type;
	symbol_t           *symbol;
	source_position_t   source_position;
	union {
		bool            is_defined;
		statement_t    *statement;
		initializer_t  *initializer;
		expression_t   *enum_value;
	} init;
	context_t           context;
	context_t          *parent_context;

	/** next declaration in a context */
	declaration_t      *next;
	/** next declaration with same symbol */
	declaration_t      *symbol_next;

	unsigned char       declaration_type; /* used in ast2firm module */
	union {
		unsigned int    value_number;     /* used in ast2firm module */
		ir_entity      *entity;           /* used in ast2firm module */
		ir_node        *block;            /* used in ast2firm module */
		tarval         *enum_val;         /* used in ast2firm module */
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
} statement_type_t;

struct statement_base_t {
	statement_type_t   type;
	statement_t       *next;
	source_position_t  source_position;
};

struct return_statement_t {
	statement_base_t  statement;
	expression_t     *return_value;
};

struct compound_statement_t {
	statement_base_t  statement;
	statement_t      *statements;
	context_t         context;
};

struct declaration_statement_t {
	statement_base_t  statement;
	declaration_t    *declarations_begin;
	declaration_t    *declarations_end;
};

struct if_statement_t {
	statement_base_t  statement;
	expression_t     *condition;
	statement_t      *true_statement;
	statement_t      *false_statement;
};

struct switch_statement_t {
	statement_base_t  statement;
	expression_t     *expression;
	statement_t      *body;
};

struct goto_statement_t {
	statement_base_t  statement;
	declaration_t    *label;
};

struct case_label_statement_t {
	statement_base_t  statement;
	expression_t     *expression;
	statement_t      *label_statement;
};

struct label_statement_t {
	statement_base_t  statement;
	declaration_t    *label;
	statement_t      *label_statement;
};

struct expression_statement_t {
	statement_base_t  statement;
	expression_t     *expression;
};

struct while_statement_t {
	statement_base_t  statement;
	expression_t     *condition;
	statement_t      *body;
};

struct do_while_statement_t {
	statement_base_t  statement;
	expression_t     *condition;
	statement_t      *body;
};

struct for_statement_t {
	statement_base_t  statement;
	expression_t     *initialisation;
	expression_t     *condition;
	expression_t     *step;
	statement_t      *body;
	context_t         context;
};

struct asm_constraint_t {
	const char       *constraints;
	expression_t     *expression;
	symbol_t         *symbol;
	asm_constraint_t *next;
};

struct asm_clobber_t {
	const char    *clobber;
	asm_clobber_t *next;
};

struct asm_statement_t {
	statement_base_t  statement;
	const char       *asm_text;
	asm_constraint_t *inputs;
	asm_constraint_t *outputs;
	asm_clobber_t    *clobbers;
	bool              is_volatile;
};

union statement_t {
	statement_type_t         type;
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
	context_t context;
};

static inline
void *_allocate_ast(size_t size)
{
	return obstack_alloc(&ast_obstack, size);
}

#define allocate_ast(size)                 _allocate_ast(size)

#endif
