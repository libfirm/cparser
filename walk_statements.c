#include <config.h>

#include "adt/error.h"
#include "ast_t.h"
#include "entity_t.h"
#include "walk_statements.h"


static void walk_expression(expression_t const *const expr,
                            statement_callback const callback, void *const env)
{
	switch (expr->base.kind) {
	case EXPR_STATEMENT:
		walk_statements(expr->statement.statement, callback, env);
		return;

	EXPR_BINARY_CASES
		walk_expression(expr->binary.left,  callback, env);
		walk_expression(expr->binary.right, callback, env);
		return;

	EXPR_UNARY_CASES_OPTIONAL
		if (expr->unary.value == NULL)
			return;
		/* FALLTHROUGH */
	EXPR_UNARY_CASES_MANDATORY
		walk_expression(expr->unary.value, callback, env);
		return;

	case EXPR_CALL:
		for (call_argument_t *arg = expr->call.arguments; arg != NULL;
		     arg = arg->next) {
			walk_expression(arg->expression, callback, env);
		}
		return;

	case EXPR_UNKNOWN:
		panic("unexpected expr kind");

	case EXPR_COMPOUND_LITERAL:
		/* TODO... */
		break;

	case EXPR_CONDITIONAL:
		walk_expression(expr->conditional.condition,        callback, env);
		/* may be NULL because of gnu extension */
		if (expr->conditional.true_expression != NULL)
			walk_expression(expr->conditional.true_expression,  callback, env);
		walk_expression(expr->conditional.false_expression, callback, env);
		return;

	case EXPR_BUILTIN_PREFETCH:
		walk_expression(expr->builtin_prefetch.adr,      callback, env);
		walk_expression(expr->builtin_prefetch.rw,       callback, env);
		walk_expression(expr->builtin_prefetch.locality, callback, env);
		return;

	case EXPR_BUILTIN_CONSTANT_P:
		walk_expression(expr->builtin_constant.value, callback, env);
		return;

	case EXPR_SELECT:
		walk_expression(expr->select.compound, callback, env);
		return;

	case EXPR_ARRAY_ACCESS:
		walk_expression(expr->array_access.array_ref, callback, env);
		walk_expression(expr->array_access.index,     callback, env);
		return;

	case EXPR_CLASSIFY_TYPE:
		walk_expression(expr->classify_type.type_expression, callback, env);
		return;

	case EXPR_SIZEOF:
	case EXPR_ALIGNOF: {
		expression_t *tp_expression = expr->typeprop.tp_expression;
		if (tp_expression != NULL) {
			walk_expression(tp_expression, callback, env);
		}
		return;
	}

	case EXPR_INVALID:
	case EXPR_OFFSETOF:
	case EXPR_REFERENCE:
	case EXPR_REFERENCE_ENUM_VALUE:
	case EXPR_CONST:
	case EXPR_CHARACTER_CONSTANT:
	case EXPR_WIDE_CHARACTER_CONSTANT:
	case EXPR_STRING_LITERAL:
	case EXPR_WIDE_STRING_LITERAL:
	case EXPR_FUNCNAME:
	case EXPR_BUILTIN_SYMBOL:
	case EXPR_VA_START:
	case EXPR_VA_ARG:
	case EXPR_LABEL_ADDRESS:
		break;
	}

	/* TODO FIXME: implement all the missing expressions here */
}

static void walk_initializer(const initializer_t  *initializer,
                             statement_callback    callback,
							 void                 *env)
{
	switch(initializer->kind) {
	case INITIALIZER_VALUE:
		walk_expression(initializer->value.value, callback, env);
		return;
	default:
		/* FIXME: should walk initializer hierarchies... */
		break;
	}
}

static void walk_declarations(const entity_t*            entity,
                              const entity_t*      const end,
                              statement_callback   const callback,
                              void                *const env)
{
	for (; entity != end; entity = entity->base.next) {
		/* we only look at variables */
		if (entity->kind != ENTITY_VARIABLE)
			continue;

		const variable_t    *variable    = &entity->variable;
		const initializer_t *initializer = variable->initializer;
		if (initializer != NULL) {
			walk_initializer(initializer, callback, env);
		}
	}
}


void walk_statements(statement_t *const stmt, statement_callback const callback, void *const env)
{
	callback(stmt, env);

	switch (stmt->kind) {
		case STATEMENT_COMPOUND:
			for (statement_t *s = stmt->compound.statements; s != NULL; s = s->base.next) {
				walk_statements(s, callback, env);
			}
			return;

		case STATEMENT_FOR:
			walk_declarations(stmt->fors.scope.entities, NULL, callback, env);
			if (stmt->fors.initialisation != NULL)
				walk_expression(stmt->fors.initialisation, callback, env);
			if (stmt->fors.condition != NULL)
				walk_expression(stmt->fors.condition, callback, env);
			if (stmt->fors.step != NULL)
				walk_expression(stmt->fors.step, callback, env);
			walk_statements(stmt->fors.body,           callback, env);
			return;

		case STATEMENT_IF:
			walk_expression(stmt->ifs.condition,      callback, env);
			walk_statements(stmt->ifs.true_statement, callback, env);
			if (stmt->ifs.false_statement != NULL)
				walk_statements(stmt->ifs.false_statement, callback, env);
			return;

		case STATEMENT_SWITCH:
			walk_expression(stmt->switchs.expression, callback, env);
			walk_statements(stmt->switchs.body,       callback, env);
			return;

		case STATEMENT_LABEL:
			walk_statements(stmt->label.statement, callback, env);
			return;

		case STATEMENT_CASE_LABEL:
			walk_statements(stmt->case_label.statement, callback, env);
			return;

		case STATEMENT_WHILE:
			walk_expression(stmt->whiles.condition, callback, env);
			walk_statements(stmt->whiles.body,      callback, env);
			return;

		case STATEMENT_DO_WHILE:
			walk_statements(stmt->do_while.body,      callback, env);
			walk_expression(stmt->do_while.condition, callback, env);
			return;

		case STATEMENT_EXPRESSION:
			walk_expression(stmt->expression.expression, callback, env);
			return;

		case STATEMENT_RETURN:
			if (stmt->returns.value != NULL)
				walk_expression(stmt->returns.value, callback, env);
			return;

		case STATEMENT_DECLARATION:
			walk_declarations(stmt->declaration.declarations_begin,
			                  stmt->declaration.declarations_end->base.next,
			                  callback, env);
			return;

		case STATEMENT_MS_TRY:
			walk_statements(stmt->ms_try.try_statement,   callback, env);
			walk_statements(stmt->ms_try.final_statement, callback, env);
			return;

		case STATEMENT_LOCAL_LABEL:
		case STATEMENT_INVALID:
		case STATEMENT_EMPTY:
		case STATEMENT_CONTINUE:
		case STATEMENT_BREAK:
		case STATEMENT_GOTO:
		case STATEMENT_ASM:
		case STATEMENT_LEAVE:
			return;
	}

	panic("unhandled statement");
}
