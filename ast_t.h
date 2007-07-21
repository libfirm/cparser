#ifndef AST_T_H
#define AST_T_H

#include "ast.h"
#include "symbol.h"
#include "token_t.h"
#include "type.h"
#include "adt/obst.h"

extern struct obstack ast_obstack;

typedef enum {
	EXPR_INVALID = 0,
	EXPR_REFERENCE,
	EXPR_CONST,
	EXPR_STRING_LITERAL,
	EXPR_CALL,
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_SELECT,
	EXPR_ARRAY_ACCESS,
	EXPR_SIZEOF,
} expresion_type_t;

struct expression_t {
	expresion_type_t   type;
	type_t            *datatype;
	source_position_t  source_position;
};

struct const_t {
	expression_t  expression;
	int           value;
};

struct string_literal_t {
	expression_t  expression;
	const char   *value;
};

struct reference_expression_t {
	expression_t    expression;
	symbol_t       *symbol;
	declaration_t  *declaration;
};

struct call_argument_t {
	expression_t    *expression;
	call_argument_t *next;
};

struct call_expression_t {
	expression_t     expression;
	expression_t    *method;
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
	BINEXPR_BITWISE_OR_ASSIGN
} binary_expression_type_t;

struct binary_expression_t {
	expression_t              expression;
	binary_expression_type_t  type;
	expression_t             *left;
	expression_t             *right;
};

struct select_expression_t {
	expression_t      expression;
	expression_t     *compound;
	symbol_t         *symbol;

	compound_entry_t *compound_entry;
};

struct array_access_expression_t {
	expression_t  expression;
	expression_t *array_ref;
	expression_t *index;
};

struct sizeof_expression_t {
	expression_t  expression;
	type_t       *type;
};

struct conditional_expression_t {
	expression_t  expression;
	expression_t *condition;
	expression_t *true_expression;
	expression_t *false_expression;
};

struct expression_list_element_t {
	expression_t *expression;
	expression_t *next;
};

struct comma_expression_t {
	expression_t               expression;
	expression_list_element_t *expressions;
};

typedef enum {
	STORAGE_CLASS_NONE,
	STORAGE_CLASS_TYPEDEF,
	STORAGE_CLASS_EXTERN,
	STORAGE_CLASS_STATIC,
	STORAGE_CLASS_AUTO,
	STORAGE_CLASS_REGISTER
} storage_class_t;

struct method_parameter_t {
	method_parameter_t *next;
	symbol_t           *symbol;
	type_t             *type;
	int                 num;
};

struct declaration_t {
	storage_class_t     storage_class;
	type_t             *type;
	symbol_t           *symbol;
	method_parameter_t *parameters;
	statement_t        *statement;
	source_position_t   source_position;
};

typedef enum {
	STATEMENT_INVALID,
	STATEMENT_COMPOUND,
	STATEMENT_RETURN,
	STATEMENT_DECLARATION,
	STATEMENT_IF,
	STATEMENT_EXPRESSION,
	STATEMENT_GOTO,
	STATEMENT_LABEL
} statement_type_t;

struct statement_t {
	statement_type_t   type;
	statement_t       *next;
	source_position_t  source_position;
};

struct return_statement_t {
	statement_t   statement;
	expression_t *return_value;
};

struct compound_statement_t {
	statement_t  statement;
	statement_t *first_statement;
};

struct declaration_statement_t {
	statement_t    statement;
	declaration_t  declaration;

	int            value_number; /**< filled in by semantic phase */
	int            refs;
};

struct if_statement_t {
	statement_t   statement;
	expression_t *condition;
	statement_t  *true_statement;
	statement_t  *false_statement;
};

struct goto_statement_t {
	statement_t        statement;
	symbol_t          *label_symbol;
	label_statement_t *label;
};

struct label_statement_t {
	statement_t        statement;
	symbol_t          *symbol;
};

struct expression_statement_t {
	statement_t   statement;
	expression_t *expression;
};

struct unit_entry_t {
	declaration_t  declaration;
	unit_entry_t  *next;
};

struct translation_unit_t {
	unit_entry_t *entries;
};

static inline
void *_allocate_ast(size_t size)
{
	return obstack_alloc(&ast_obstack, size);
}

#define allocate_ast(size)                 _allocate_ast(size)

#endif
