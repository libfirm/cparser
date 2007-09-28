#include <config.h>

#include "ast_t.h"
#include "type_t.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "adt/error.h"

struct obstack ast_obstack;

static FILE *out;
static int   indent;

static void print_statement(const statement_t *statement);
static void print_declaration(const declaration_t *declaration);

static void print_indent(void)
{
	for(int i = 0; i < indent; ++i)
		fprintf(out, "\t");
}

static void print_const(const const_t *cnst)
{
	if(cnst->expression.datatype == NULL)
		return;

	if(is_type_integer(cnst->expression.datatype)) {
		fprintf(out, "%d", cnst->v.int_value);
	} else if(is_type_floating(cnst->expression.datatype)) {
		fprintf(out, "%Lf", cnst->v.float_value);
	}
}

static void print_string_literal(const string_literal_t *string_literal)
{
	fputc('"', out);
	for(const char *c = string_literal->value; *c != '\0'; ++c) {
		switch(*c) {
		case '\"':  fputs("\\\"", out); break;
		case '\\':  fputs("\\\\", out); break;
		case '\a':  fputs("\\a", out); break;
		case '\b':  fputs("\\b", out); break;
		case '\f':  fputs("\\f", out); break;
		case '\n':  fputs("\\n", out); break;
		case '\r':  fputs("\\r", out); break;
		case '\t':  fputs("\\t", out); break;
		case '\v':  fputs("\\v", out); break;
		case '\?':  fputs("\\?", out); break;
		default:
			if(!isprint(*c)) {
				fprintf(out, "\\x%x", *c);
				break;
			}
			fputc(*c, out);
			break;
		}
	}
	fputc('"', out);
}

static void print_call_expression(const call_expression_t *call)
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

static void print_binary_expression(const binary_expression_t *binexpr)
{
	fprintf(out, "(");
	print_expression(binexpr->left);
	fprintf(out, " ");
	switch(binexpr->type) {
	case BINEXPR_INVALID:	         fputs("INVOP", out); break;
	case BINEXPR_COMMA:              fputs(",", out);     break;
	case BINEXPR_ASSIGN:             fputs("=", out);     break;
	case BINEXPR_ADD:                fputs("+", out);     break;
	case BINEXPR_SUB:                fputs("-", out);     break;
	case BINEXPR_MUL:                fputs("*", out);     break;
	case BINEXPR_MOD:                fputs("%", out);     break;
	case BINEXPR_DIV:                fputs("/", out);     break;
	case BINEXPR_BITWISE_OR:         fputs("|", out);     break;
	case BINEXPR_BITWISE_AND:        fputs("&", out);     break;
	case BINEXPR_BITWISE_XOR:        fputs("^", out);     break;
	case BINEXPR_LOGICAL_OR:         fputs("||", out);    break;
	case BINEXPR_LOGICAL_AND:        fputs("&&", out);    break;
	case BINEXPR_NOTEQUAL:           fputs("!=", out);    break;
	case BINEXPR_EQUAL:              fputs("==", out);    break;
	case BINEXPR_LESS:               fputs("<", out);     break;
	case BINEXPR_LESSEQUAL:          fputs("<=", out);    break;
	case BINEXPR_GREATER:            fputs(">", out);     break;
	case BINEXPR_GREATEREQUAL:       fputs(">=", out);    break;
	case BINEXPR_SHIFTLEFT:          fputs("<<", out);    break;
	case BINEXPR_SHIFTRIGHT:         fputs(">>", out);    break;

	case BINEXPR_ADD_ASSIGN:         fputs("+=", out);    break;
	case BINEXPR_SUB_ASSIGN:         fputs("-=", out);    break;
	case BINEXPR_MUL_ASSIGN:         fputs("*=", out);    break;
	case BINEXPR_MOD_ASSIGN:         fputs("%=", out);    break;
	case BINEXPR_DIV_ASSIGN:         fputs("/=", out);    break;
	case BINEXPR_BITWISE_OR_ASSIGN:  fputs("|=", out);    break;
	case BINEXPR_BITWISE_AND_ASSIGN: fputs("&=", out);    break;
	case BINEXPR_BITWISE_XOR_ASSIGN: fputs("^=", out);    break;
	case BINEXPR_SHIFTLEFT_ASSIGN:   fputs("<<=", out);   break;
	case BINEXPR_SHIFTRIGHT_ASSIGN:  fputs(">>=", out);   break;
	}
	fprintf(out, " ");
	print_expression(binexpr->right);
	fprintf(out, ")");
}

static void print_unary_expression(const unary_expression_t *unexpr)
{
	switch(unexpr->type) {
	case UNEXPR_NEGATE:           fputs("-", out);  break;
	case UNEXPR_PLUS:             fputs("+", out);  break;
	case UNEXPR_NOT:              fputs("!", out);  break;
	case UNEXPR_BITWISE_NEGATE:   fputs("~", out);  break;
	case UNEXPR_PREFIX_INCREMENT: fputs("++", out); break;
	case UNEXPR_PREFIX_DECREMENT: fputs("--", out); break;
	case UNEXPR_DEREFERENCE:      fputs("*", out);  break;
	case UNEXPR_TAKE_ADDRESS:     fputs("&", out);  break;

	case UNEXPR_POSTFIX_INCREMENT:
		fputs("(", out);
		print_expression(unexpr->value);
		fputs(")", out);
		fputs("++", out);
		return;
	case UNEXPR_POSTFIX_DECREMENT:
		fputs("(", out);
		print_expression(unexpr->value);
		fputs(")", out);
		fputs("--", out);
		return;
	case UNEXPR_CAST:
		fputs("(", out);
		print_type(unexpr->expression.datatype);
		fputs(")", out);
		break;
	case UNEXPR_INVALID:
		fprintf(out, "unop%d", unexpr->type);
		break;
	}
	fputs("(", out);
	print_expression(unexpr->value);
	fputs(")", out);
}

static void print_reference_expression(const reference_expression_t *ref)
{
	fprintf(out, "%s", ref->declaration->symbol->string);
}

static void print_array_expression(const array_access_expression_t *expression)
{
	fputs("(", out);
	print_expression(expression->array_ref);
	fputs(")[", out);
	print_expression(expression->index);
	fputs("]", out);
}

static void print_sizeof_expression(const sizeof_expression_t *expression)
{
	fputs("sizeof", out);
	if(expression->size_expression != NULL) {
		fputc('(', out);
		print_expression(expression->size_expression);
		fputc(')', out);
	} else {
		fputc('(', out);
		print_type(expression->type);
		fputc(')', out);
	}
}

static void print_builtin_symbol(const builtin_symbol_expression_t *expression)
{
	fputs(expression->symbol->string, out);
}

static void print_conditional(const conditional_expression_t *expression)
{
	fputs("(", out);
	print_expression(expression->condition);
	fputs(" ? ", out);
	print_expression(expression->true_expression);
	fputs(" : ", out);
	print_expression(expression->false_expression);
	fputs(")", out);
}

static void print_va_arg(const va_arg_expression_t *expression)
{
	fputs("__builtin_va_arg(", out);
	print_expression(expression->arg);
	fputs(", ", out);
	print_type(expression->expression.datatype);
	fputs(")", out);
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
	case EXPR_FUNCTION:
	case EXPR_PRETTY_FUNCTION:
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
		print_reference_expression((const reference_expression_t*) expression);
		break;
	case EXPR_ARRAY_ACCESS:
		print_array_expression((const array_access_expression_t*) expression);
		break;
	case EXPR_UNARY:
		print_unary_expression((const unary_expression_t*) expression);
		break;
	case EXPR_SIZEOF:
		print_sizeof_expression((const sizeof_expression_t*) expression);
		break;
	case EXPR_BUILTIN_SYMBOL:
		print_builtin_symbol((const builtin_symbol_expression_t*) expression);
		break;
	case EXPR_CONDITIONAL:
		print_conditional((const conditional_expression_t*) expression);
		break;
	case EXPR_VA_ARG:
		print_va_arg((const va_arg_expression_t*) expression);
		break;

	case EXPR_OFFSETOF:
	case EXPR_STATEMENT:
	case EXPR_SELECT:
		/* TODO */
		fprintf(out, "some expression of type %d", expression->type);
		break;
	}
}

static void print_compound_statement(const compound_statement_t *block)
{
	fputs("{\n", out);
	indent++;

	statement_t *statement = block->statements;
	while(statement != NULL) {
		print_indent();
		print_statement(statement);

		statement = statement->next;
	}
	indent--;
	print_indent();
	fputs("}\n", out);
}

static void print_return_statement(const return_statement_t *statement)
{
	fprintf(out, "return ");
	if(statement->return_value != NULL)
		print_expression(statement->return_value);
	fputs(";\n", out);
}

static void print_expression_statement(const expression_statement_t *statement)
{
	print_expression(statement->expression);
	fputs(";\n", out);
}

static void print_goto_statement(const goto_statement_t *statement)
{
	fprintf(out, "goto ");
	if(statement->label != NULL) {
		fprintf(out, "%s", statement->label->symbol->string);
	} else {
		fprintf(out, "?%s", statement->label_symbol->string);
	}
	fputs(";\n", out);
}

static void print_label_statement(const label_statement_t *statement)
{
	fprintf(out, "%s:\n", statement->symbol->string);
}

static void print_if_statement(const if_statement_t *statement)
{
	fputs("if(", out);
	print_expression(statement->condition);
	fputs(") ", out);
	if(statement->true_statement != NULL) {
		print_statement(statement->true_statement);
	}

	if(statement->false_statement != NULL) {
		print_indent();
		fputs("else ", out);
		print_statement(statement->false_statement);
	}
}

static void print_switch_statement(const switch_statement_t *statement)
{
	fputs("switch(", out);
	print_expression(statement->expression);
	fputs(") ", out);
	print_statement(statement->body);
}

static void print_case_label(const case_label_statement_t *statement)
{
	if(statement->expression == NULL) {
		fputs("default:\n", out);
	} else {
		fputs("case ", out);
		print_expression(statement->expression);
		fputs(":\n", out);
	}
}

static void print_declaration_statement(
		const declaration_statement_t *statement)
{
	declaration_t *declaration = statement->declarations_begin;
	for( ; declaration != statement->declarations_end->next;
	       declaration = declaration->next) {
		print_declaration(declaration);
	}
}

static void print_while_statement(const while_statement_t *statement)
{
	fputs("while(", out);
	print_expression(statement->condition);
	fputs(") ", out);
	print_statement(statement->body);
}

static void print_do_while_statement(const do_while_statement_t *statement)
{
	fputs("do ", out);
	print_statement(statement->body);
	print_indent();
	fputs("while(", out);
	print_expression(statement->condition);
	fputs(");\n", out);
}

static void print_for_statement(const for_statement_t *statement)
{
	fputs("for(", out);
	if(statement->context.declarations != NULL) {
		assert(statement->initialisation == NULL);
		print_declaration(statement->context.declarations);
		if(statement->context.declarations->next != NULL) {
			panic("multiple declarations in for statement not supported yet");
		}
	} else if(statement->initialisation) {
		print_expression(statement->initialisation);
	}
	fputs("; ", out);
	if(statement->condition != NULL) {
		print_expression(statement->condition);
	}
	fputs("; ", out);
	if(statement->step != NULL) {
		print_expression(statement->step);
	}
	fputs(")", out);
	print_statement(statement->body);
}

void print_statement(const statement_t *statement)
{
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
	case STATEMENT_CONTINUE:
		fputs("continue;\n", out);
		break;
	case STATEMENT_BREAK:
		fputs("break;\n", out);
		break;
	case STATEMENT_IF:
		print_if_statement((const if_statement_t*) statement);
		break;
	case STATEMENT_SWITCH:
		print_switch_statement((const switch_statement_t*) statement);
		break;
	case STATEMENT_CASE_LABEL:
		print_case_label((const case_label_statement_t*) statement);
		break;
	case STATEMENT_DECLARATION:
		print_declaration_statement((const declaration_statement_t*) statement);
		break;
	case STATEMENT_WHILE:
		print_while_statement((const while_statement_t*) statement);
		break;
	case STATEMENT_DO_WHILE:
		print_do_while_statement((const do_while_statement_t*) statement);
		break;
	case STATEMENT_FOR:
		print_for_statement((const for_statement_t*) statement);
		break;
	case STATEMENT_INVALID:
		fprintf(out, "*invalid statement*");
		break;
	}
}

static void print_storage_class(storage_class_t storage_class)
{
	switch(storage_class) {
	case STORAGE_CLASS_ENUM_ENTRY:
	case STORAGE_CLASS_NONE:
		break;
	case STORAGE_CLASS_TYPEDEF:  fputs("typedef ", out); break;
	case STORAGE_CLASS_EXTERN:   fputs("extern ", out); break;
	case STORAGE_CLASS_STATIC:   fputs("static ", out); break;
	case STORAGE_CLASS_AUTO:     fputs("auto ", out); break;
	case STORAGE_CLASS_REGISTER: fputs("register ", out); break;
	}
}

void print_initializer(const initializer_t *initializer)
{
	if(initializer->type == INITIALIZER_VALUE) {
		print_expression(initializer->v.value);
		return;
	}

	assert(initializer->type == INITIALIZER_LIST);
	fputs("{ ", out);
	initializer_t *iter = initializer->v.list;
	for( ; iter != NULL; iter = iter->next) {
		print_initializer(iter);
		if(iter->next != NULL) {
			fputs(", ", out);
		}
	}
	fputs("}", out);
}

static void print_declaration(const declaration_t *declaration)
{
	print_storage_class(declaration->storage_class);
	print_type_ext(declaration->type, declaration->symbol,
	               &declaration->context);
	if(declaration->statement != NULL) {
		fputs("\n", out);
		print_statement(declaration->statement);
	} else if(declaration->initializer != NULL) {
		fputs(" = ", out);
		print_initializer(declaration->initializer);
		fprintf(out, ";\n");
	} else {
		fprintf(out, ";\n");
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
