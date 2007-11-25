#ifndef AST_T_H
#define AST_T_H

#include <libfirm/firm_types.h>

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
} expresion_type_t;

struct context_t {
	declaration_t   *declarations;
};

struct expression_t {
	expresion_type_t   type;
	type_t            *datatype;
	source_position_t  source_position;
};

struct const_t {
	expression_t  expression;
	union {
		long long   int_value;
		long double float_value;
	} v;
};

struct string_literal_t {
	expression_t  expression;
	const char   *value;
};

struct builtin_symbol_expression_t {
	expression_t  expression;
	symbol_t     *symbol;
};

struct reference_expression_t {
	expression_t   expression;
	symbol_t      *symbol;
	declaration_t *declaration;
};

struct call_argument_t {
	expression_t    *expression;
	call_argument_t *next;
};

struct call_expression_t {
	expression_t     expression;
	expression_t    *function;
	call_argument_t *arguments;
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
	UNEXPR_CAST
} unary_expression_type_t;

struct unary_expression_t {
	expression_t             expression;
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
	expression_t              expression;
	binary_expression_type_t  type;
	expression_t             *left;
	expression_t             *right;
};

struct select_expression_t {
	expression_t   expression;
	expression_t  *compound;
	symbol_t      *symbol;

	declaration_t *compound_entry;
};

struct array_access_expression_t {
	expression_t  expression;
	expression_t *array_ref;
	expression_t *index;
};

struct sizeof_expression_t {
	expression_t  expression;
	type_t       *type;
	expression_t *size_expression;
};

struct designator_t {
	symbol_t     *symbol;
	expression_t *array_access;
	designator_t *next;
};

struct offsetof_expression_t {
	expression_t  expression;
	type_t       *type;
	designator_t *designator;
};

struct va_arg_expression_t {
	expression_t  expression;
	expression_t *arg;
	type_t       *type;
};

struct conditional_expression_t {
	expression_t  expression;
	expression_t *condition;
	expression_t *true_expression;
	expression_t *false_expression;
};

struct statement_expression_t {
	expression_t  expression;
	statement_t  *statement;
};

struct classify_type_expression_t {
	expression_t  expression;
	expression_t *type_expression;
};

typedef enum {
	STORAGE_CLASS_NONE,
	STORAGE_CLASS_TYPEDEF,
	STORAGE_CLASS_EXTERN,
	STORAGE_CLASS_STATIC,
	STORAGE_CLASS_AUTO,
	STORAGE_CLASS_REGISTER,
	STORAGE_CLASS_ENUM_ENTRY
} storage_class_t;

typedef enum {
	NAMESPACE_NORMAL,
	NAMESPACE_STRUCT,
	NAMESPACE_UNION,
	NAMESPACE_ENUM,
	NAMESPACE_LABEL
} namespace_t;

typedef enum {
	INITIALIZER_VALUE,
	INITIALIZER_LIST,
	INITIALIZER_STRING
} initializer_type_t;

struct initializer_t {
	initializer_type_t  type;
	union {
		/* if type == INITIALIZER_VALUE */
		expression_t *value;
		/* if type == INITIALIZER_LIST */
		struct {
			size_t         len;
			initializer_t *initializers[];
		} list;
		/* if type == INITIALIZER_STRING */
		const char    *string;
	} v;
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
	STATEMENT_FOR
} statement_type_t;

struct statement_t {
	statement_type_t   type;
	statement_t       *next;
	source_position_t  source_position;
	union {
		/* if type == STATEMENT_COMPOUND */
		struct {
			statement_t *statements;
			context_t    context;
		} compound_stmt;
		/* if type == STATEMENT_RETURN */
		expression_t *return_value;
		/* if type == STATEMENT_DECLARATION */
		struct {
			declaration_t *begin;
			declaration_t *end;
		} declaration_stmt;
		/* if type == STATEMENT_IF */
		struct {
			expression_t *condition;
			statement_t  *true_statement;
			statement_t  *false_statement;
		} if_stmt;
		/* if type == STATEMENT_SWITCH */
		struct {
			expression_t *expression;
			statement_t  *body;
		} switch_stmt;
		/* if type == STATEMENT_EXPRESSION */
		expression_t *expression;
		/* if type == STATEMENT_GOTO */
		declaration_t *goto_label;
		/* if type == STATEMENT_LABEL */
		struct {
			declaration_t *label;
			statement_t   *label_statement;
		} label_stmt;
		/* if type == STATEMENT_CASE_LABEL */
		struct {
			expression_t *expression;
			statement_t  *label_statement;
		} case_label_stmt;
		/* if type == STATEMENT_WHILE or STATEMENT_DO_WHILE */
		struct {
			expression_t *condition;
			statement_t  *body;
		} while_stmt;
		/* if type == STATEMENT_FOR */
		struct {
			expression_t  *initialisation;
			expression_t  *condition;
			expression_t  *step;
			statement_t   *body;
			context_t      context;
		} for_stmt;
	} v;
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
