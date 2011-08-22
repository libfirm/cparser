#include "config.h"

#include "ast_grep.h"
#include "walk.h"
#include "input.h"
#include "lexer.h"
#include "parser_t.h"
#include "ast_t.h"
#include "type_t.h"
#include "entity_t.h"
#include "symbol_t.h"
#include "types.h"
#include "diagnostic.h"
#include "adt/strutil.h"

#define HERE (&token.base.source_position)

typedef enum {
	EXPR_MATCH_TYPE = EXPR_LAST + 1,
	EXPR_MATCH_ANY,
	EXPR_MATCH_OR,
} match_kind_t;

typedef struct match_type_expression_t {
	expression_base_t  base;
	type_t            *pattern;
} match_type_expression_t;

typedef struct match_or_expression_t {
	expression_base_t  base;
	expression_t      *expr1;
	expression_t      *expr2;
} match_or_expression_t;

static expression_t *parse_match_expression(void)
{
	assert(token.kind == '@');
	parser_next_token();

	if (token.kind != T_IDENTIFIER) {
		errorf(HERE, "expected identifier after '@'");
		return NULL;
	}
	const char *name = token.identifier.symbol->string;
	parser_next_token();

	if (streq(name, "type")) {
		assert(token.kind == '(');
		parser_next_token();

		match_type_expression_t *expr
			= (match_type_expression_t*) allocate_ast_zero(sizeof(*expr));
		memset(expr, 0, sizeof(*expr));
		expr->base.kind = EXPR_MATCH_TYPE;
		expr->base.type = type_error_type;
		expr->pattern   = parse_typename();

		assert(token.kind == ')');
		parser_next_token();
		return (expression_t*)expr;
	} else if (streq(name, "any")) {
		expression_t *expr = allocate_ast_zero(sizeof(*expr));
		memset(expr, 0, sizeof(*expr));
		expr->base.kind = EXPR_MATCH_ANY;
		expr->base.type = type_error_type;
		return expr;
	} else if (streq(name, "or")) {
		assert(token.kind == '(');
		parser_next_token();

		match_or_expression_t *expr
			= (match_or_expression_t*) allocate_ast_zero(sizeof(*expr));
		memset(expr, 0, sizeof(*expr));
		expr->base.kind = EXPR_MATCH_OR;
		expr->base.type = type_error_type;
		expr->expr1 = parse_assignment_expression();

		assert(token.kind == ',');
		parser_next_token();

		expr->expr2 = parse_assignment_expression();

		assert(token.kind == ')');
		parser_next_token();
		return (expression_t*)expr;
	}

	errorf(HERE, "unknown matcher '%s'", name);
	return NULL;
}

expression_t *parse_grep_expression(translation_unit_t *context,
                                    const char *string)
{
	input_t *input = input_from_string(string, "utf-8");
	lexer_switch_input(input, "<commandline>");
	start_parsing_into(context);
	register_expression_parser(parse_match_expression, '@');
	expression_t *result = parse_expression();
	input_free(input);

	return result;
}

static bool match_type(type_t *type, type_t *pattern)
{
	/* shortcut */
	if (type == pattern)
		return true;

	if (type->kind != pattern->kind)
		return false;

	/* we ignore type qualifiers for now not sure if this is a good or bad */

	switch (type->kind) {
	case TYPE_ATOMIC:
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
		return type->atomic.akind == pattern->atomic.akind;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return type->compound.compound == pattern->compound.compound;
	case TYPE_POINTER:
		return match_type(type->pointer.points_to, pattern->pointer.points_to);
	case TYPE_TYPEDEF:
		return type->typedeft.typedefe == pattern->typedeft.typedefe;
	default:
		/* TODO */
		return false;
	}
}

static expression_t *skip_implicit_cast(expression_t *expression)
{
	while (expression->kind == EXPR_UNARY_CAST && expression->base.implicit)
		expression = expression->unary.value;
	return expression;
}

static bool match(expression_t *expression, expression_t *pattern)
{
	/* skip implicit casts */
	expression = skip_implicit_cast(expression);
	pattern = skip_implicit_cast(pattern);

	switch ((match_kind_t)pattern->kind) {
	case EXPR_MATCH_TYPE: {
		match_type_expression_t *matche = (match_type_expression_t*) pattern;
		return match_type(expression->base.type, matche->pattern);
	}
	case EXPR_MATCH_ANY:
		return true;
	case EXPR_MATCH_OR: {
		match_or_expression_t *matcho = (match_or_expression_t*) pattern;
		if (match(expression, matcho->expr1))
			return true;
		return match(expression, matcho->expr2);
	}
	}

	if (pattern->kind != expression->kind)
		return false;

	switch (expression->kind) {
	EXPR_LITERAL_CASES
		/* ignore suffix for now... not sure if this is a good or bad thing */
		return strcmp(expression->literal.value.begin,
		              pattern->literal.value.begin) == 0;

	EXPR_UNARY_CASES
		return match(expression->unary.value, pattern->unary.value);

	EXPR_BINARY_CASES
		return match(expression->binary.left, pattern->binary.left)
		    && match(expression->binary.right, pattern->binary.right);

	case EXPR_REFERENCE:
		return expression->reference.entity == pattern->reference.entity;

	case EXPR_CALL: {
		if (!match(expression->call.function, pattern->call.function))
			return false;
		call_argument_t *arg1 = expression->call.arguments;
		call_argument_t *arg2 = pattern->call.arguments;
		for ( ; arg1 != NULL && arg2 != NULL;
		     arg1 = arg1->next, arg2 = arg2->next) {
		    if (!match(arg1->expression, arg2->expression))
				return false;
		}
		if (arg1 != arg2)
			return false;
		return true;
	}

	default:
		/* TODO */
		return false;
	}
}

static void match_expression(expression_t *expression, void *env)
{
	expression_t *pattern = (expression_t*) env;
	if (match(expression, pattern)) {
		fprintf(stderr, "%s:%u:%u: ",
				expression->base.source_position.input_name,
		        expression->base.source_position.lineno,
				expression->base.source_position.colno);
		print_expression(expression);
		fputs("\n", stderr);
	}
}

void ast_grep(translation_unit_t *unit, expression_t *pattern)
{
	walk_translation_unit(unit, NULL, NULL, match_expression, pattern);
}
