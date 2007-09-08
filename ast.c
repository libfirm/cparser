#include <config.h>

#include "ast_t.h"
#include "type_t.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/error.h"

struct obstack ast_obstack;

static FILE *out;
static int   indent;

static void print_expression(const expression_t *expression);
static void print_statement(const statement_t *statement);

static
void print_const(const const_t *cnst)
{
	fprintf(out, "%d", cnst->value);
}

static
void print_string_literal(const string_literal_t *string_literal)
{
	/* TODO escape " and non-printable chars */
	fprintf(out, "\"%s\"", string_literal->value);
}

static
void print_call_expression(const call_expression_t *call)
{
	print_expression(call->method);
	fprintf(out, "(");
	call_argument_t *argument = call->arguments;
	int              first    = 1;
	while(argument != NULL) {
		if(!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}
		print_expression(argument->expression);

		argument = argument->next;
	}
	fprintf(out, ")");
}

static
void print_binary_expression(const binary_expression_t *binexpr)
{
	fprintf(out, "(");
	print_expression(binexpr->left);
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
	print_expression(binexpr->right);
	fprintf(out, ")");
}

void print_expression(const expression_t *expression)
{
	switch(expression->type) {
	case EXPR_INVALID:
		fprintf(out, "*invalid expression*");
		break;
	case EXPR_CONST:
		print_const((const const_t*) expression);
		break;
	case EXPR_STRING_LITERAL:
		print_string_literal((const string_literal_t*) expression);
		break;
	case EXPR_CALL:
		print_call_expression((const call_expression_t*) expression);
		break;
	case EXPR_BINARY:
		print_binary_expression((const binary_expression_t*) expression);
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
void print_compound_statement(const compound_statement_t *block)
{
	fputs("{\n", out);
	indent++;

	statement_t *statement = block->statements;
	while(statement != NULL) {
		print_statement(statement);

		statement = statement->next;
	}
	indent--;
	fputs("}\n", out);
}

static
void print_return_statement(const return_statement_t *statement)
{
	fprintf(out, "return ");
	if(statement->return_value != NULL)
		print_expression(statement->return_value);
}

static
void print_expression_statement(const expression_statement_t *statement)
{
	print_expression(statement->expression);
}

static
void print_goto_statement(const goto_statement_t *statement)
{
	fprintf(out, "goto ");
	if(statement->label != NULL) {
		fprintf(out, "%s", statement->label->symbol->string);
	} else {
		fprintf(out, "?%s", statement->label_symbol->string);
	}
}

static
void print_label_statement(const label_statement_t *statement)
{
	fprintf(out, ":%s", statement->symbol->string);
}

static
void print_if_statement(const if_statement_t *statement)
{
	fprintf(out, "if ");
	print_expression(statement->condition);
	fprintf(out, ":\n");
	if(statement->true_statement != NULL) {
		print_statement(statement->true_statement);
	}

	if(statement->false_statement != NULL) {
		fprintf(out, "else:\n");
		print_statement(statement->false_statement);
	}
}

static
void print_declaration_statement(const declaration_statement_t *statement)
{
	(void) statement;
	fprintf(out, "*declaration statement*");
}

void print_statement(const statement_t *statement)
{
	for(int i = 0; i < indent; ++i)
		fprintf(out, "\t");

	switch(statement->type) {
	case STATEMENT_COMPOUND:
		print_compound_statement((const compound_statement_t*) statement);
		break;
	case STATEMENT_RETURN:
		print_return_statement((const return_statement_t*) statement);
		break;
	case STATEMENT_EXPRESSION:
		print_expression_statement((const expression_statement_t*) statement);
		break;
	case STATEMENT_LABEL:
		print_label_statement((const label_statement_t*) statement);
		break;
	case STATEMENT_GOTO:
		print_goto_statement((const goto_statement_t*) statement);
		break;
	case STATEMENT_IF:
		print_if_statement((const if_statement_t*) statement);
		break;
	case STATEMENT_DECLARATION:
		print_declaration_statement((const declaration_statement_t*) statement);
		break;
	case STATEMENT_INVALID:
	default:
		fprintf(out, "*invalid statement*");
		break;

	}
	fprintf(out, "\n");
}

#if 0
static
void print_method_parameters(const method_parameter_t *parameters,
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

		print_type(parameter_type->type);
		fprintf(out, " %s", parameter->symbol->string);

		parameter      = parameter->next;
		parameter_type = parameter_type->next;
	}
	assert(parameter == NULL && parameter_type == NULL);

	fprintf(out, ")");
}
#endif

static
void print_declaration(const declaration_t *declaration)
{
	print_type(declaration->type, declaration->symbol);
	fprintf(out, "\n");
	if(declaration->statement != NULL) {
		print_statement(declaration->statement);
	}
}

void print_ast(const translation_unit_t *unit)
{
	declaration_t *declaration = unit->context.declarations;
	while(declaration != NULL) {
		print_declaration(declaration);

		declaration = declaration->next;
	}
}

void init_ast(void)
{
	obstack_init(&ast_obstack);
}

void exit_ast(void)
{
	obstack_free(&ast_obstack, NULL);
}

void ast_set_output(FILE *stream)
{
	out = stream;
	type_set_output(stream);
}

void* (allocate_ast) (size_t size)
{
	return _allocate_ast(size);
}
