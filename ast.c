#include <config.h>

#include "ast_t.h"
#include "type_t.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/error.h"

struct obstack ast_obstack;

static
void print_const(FILE *out, const const_t *cnst)
{
	fprintf(out, "%d", cnst->value);
}

static
void print_string_literal(FILE *out, const string_literal_t *string_literal)
{
	/* TODO escape " and non-printable chars */
	fprintf(out, "\"%s\"", string_literal->value);
}

static
void print_call_expression(FILE *out, const call_expression_t *call)
{
	print_expression(out, call->method);
	fprintf(out, "(");
	call_argument_t *argument = call->arguments;
	int              first    = 1;
	while(argument != NULL) {
		if(!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}
		print_expression(out, argument->expression);

		argument = argument->next;
	}
	fprintf(out, ")");
}

static
void print_binary_expression(FILE *out, const binary_expression_t *binexpr)
{
	fprintf(out, "(");
	print_expression(out, binexpr->left);
	fprintf(out, " ");
	switch(binexpr->type) {
	case BINEXPR_INVALID:
		fprintf(out, "INVOP");
		break;
	case BINEXPR_ASSIGN:
		fprintf(out, "<-");
		break;
	case BINEXPR_ADD:
		fprintf(out, "+");
		break;
	case BINEXPR_SUB:
		fprintf(out, "-");
		break;
	case BINEXPR_NOTEQUAL:
		fprintf(out, "/=");
		break;
	case BINEXPR_EQUAL:
		fprintf(out, "=");
		break;
	case BINEXPR_LESS:
		fprintf(out, "<");
		break;
	case BINEXPR_LESSEQUAL:
		fprintf(out, "<=");
		break;
	case BINEXPR_GREATER:
		fprintf(out, ">");
		break;
	case BINEXPR_GREATEREQUAL:
		fprintf(out, ">=");
		break;
	default:
		/* TODO: add missing ops */
		fprintf(out, "op%d", binexpr->type);
		break;
	}
	fprintf(out, " ");
	print_expression(out, binexpr->right);
	fprintf(out, ")");
}

void print_expression(FILE *out, const expression_t *expression)
{
	switch(expression->type) {
	case EXPR_INVALID:
		fprintf(out, "*invalid expression*");
		break;
	case EXPR_CONST:
		print_const(out, (const const_t*) expression);
		break;
	case EXPR_STRING_LITERAL:
		print_string_literal(out, (const string_literal_t*) expression);
		break;
	case EXPR_CALL:
		print_call_expression(out, (const call_expression_t*) expression);
		break;
	case EXPR_BINARY:
		print_binary_expression(out, (const binary_expression_t*) expression);
		break;
	case EXPR_REFERENCE:
	case EXPR_UNARY:
	case EXPR_SELECT:
	case EXPR_ARRAY_ACCESS:
	case EXPR_SIZEOF:
		/* TODO */
		fprintf(out, "some expression of type %d", expression->type);
		break;
	}
}

static
void print_block_statement(FILE *out, int indent,
                           const block_statement_t *block)
{
	statement_t *statement = block->first_statement;
	while(statement != NULL) {
		print_statement(out, indent + 1, statement);

		statement = statement->next;
	}
}

static
void print_return_statement(FILE *out, const return_statement_t *statement)
{
	fprintf(out, "return ");
	if(statement->return_value != NULL)
		print_expression(out, statement->return_value);
}

static
void print_expression_statement(FILE *out,
                                const expression_statement_t *statement)
{
	print_expression(out, statement->expression);
}

static
void print_goto_statement(FILE *out, const goto_statement_t *statement)
{
	fprintf(out, "goto ");
	if(statement->label != NULL) {
		fprintf(out, "%s", statement->label->symbol->string);
	} else {
		fprintf(out, "?%s", statement->label_symbol->string);
	}
}

static
void print_label_statement(FILE *out, const label_statement_t *statement)
{
	fprintf(out, ":%s", statement->symbol->string);
}

static
void print_if_statement(FILE *out, int indent, const if_statement_t *statement)
{
	fprintf(out, "if ");
	print_expression(out, statement->condition);
	fprintf(out, ":\n");
	if(statement->true_statement != NULL) {
		print_statement(out, indent, statement->true_statement);
	}

	if(statement->false_statement != NULL) {
		fprintf(out, "else:\n");
		print_statement(out, indent, statement->false_statement);
	}
}

static
void print_variable_declaration_statement(FILE *out,
                     const variable_declaration_statement_t *statement)
{
	fprintf(out, "var");
	if(statement->type != NULL) {
		fprintf(out, "<");
		print_type(out, statement->type);
		fprintf(out, ">");
	}
	fprintf(out, " %s", statement->symbol->string);
}

void print_statement(FILE *out, int indent, const statement_t *statement)
{
	for(int i = 0; i < indent; ++i)
		fprintf(out, "\t");

	switch(statement->type) {
	case STATEMENT_BLOCK:
		print_block_statement(out, indent,
		                      (const block_statement_t*) statement);
		break;
	case STATEMENT_RETURN:
		print_return_statement(out, (const return_statement_t*) statement);
		break;
	case STATEMENT_EXPRESSION:
		print_expression_statement(out,
		                           (const expression_statement_t*) statement);
		break;
	case STATEMENT_LABEL:
		print_label_statement(out, (const label_statement_t*) statement);
		break;
	case STATEMENT_GOTO:
		print_goto_statement(out, (const goto_statement_t*) statement);
		break;
	case STATEMENT_IF:
		print_if_statement(out, indent, (const if_statement_t*) statement);
		break;
	case STATEMENT_VARIABLE_DECLARATION:
		print_variable_declaration_statement(out,
		        (const variable_declaration_statement_t*) statement);
		break;
	case STATEMENT_INVALID:
	default:
		fprintf(out, "*invalid statement*");
		break;

	}
	fprintf(out, "\n");
}

static
void print_method_parameters(FILE *out, const method_parameter_t *parameters,
                             const method_type_t *method_type)
{
	fprintf(out, "(");

	int                            first          = 1;
	const method_parameter_t      *parameter      = parameters;
	const method_parameter_type_t *parameter_type
		= method_type->parameter_types;
	while(parameter != NULL && parameter_type != NULL) {
		if(!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}

		print_type(out, parameter_type->type);
		fprintf(out, " %s", parameter->symbol->string);

		parameter      = parameter->next;
		parameter_type = parameter_type->next;
	}
	assert(parameter == NULL && parameter_type == NULL);

	fprintf(out, ")");
}

static
void print_method(FILE *out, const method_t *method)
{
	method_type_t *type = method->type;

	fprintf(out, "func ");
	print_type(out, type->result_type);
	fprintf(out, " %s", method->symbol->string);

	print_method_parameters(out, method->parameters, type);

	if(method->statement != NULL) {
		fprintf(out, ":\n");
		print_statement(out, 0, method->statement);
	} else {
		fprintf(out, "\n");
	}
}

static
void print_namespace_entry(FILE *out, const namespace_entry_t *entry)
{
	switch(entry->type) {
	case NAMESPACE_ENTRY_METHOD:
		print_method(out, (const method_t*) entry);
		break;
	case NAMESPACE_ENTRY_VARIABLE:
		/* TODO */
		fprintf(out, "some namespace entry of type %d\n\n", entry->type);
		break;
	case NAMESPACE_ENTRY_INVALID:
	default:
		fprintf(out, "invalid namespace entry (%d)\n", entry->type);
		break;
	}
}

void print_ast(FILE *out, const namespace_t *namespace)
{
	namespace_entry_t *entry = namespace->entries;

	while(entry != NULL) {
		print_namespace_entry(out, entry);

		entry = entry->next;
	}
}

void init_ast_module(void)
{
	obstack_init(&ast_obstack);
}

void exit_ast_module(void)
{
	obstack_free(&ast_obstack, NULL);
}

void* (allocate_ast) (size_t size)
{
	return _allocate_ast(size);
}
