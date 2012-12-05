/*
 * This file is part of cparser.
 * Copyright (C) 2007-2009 Matthias Braun <matze@braunis.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
#include <config.h>

#include "adt/error.h"
#include "ast_t.h"
#include "entity_t.h"
#include "type_t.h"
#include "walk.h"
#include <libfirm/adt/pset.h>

typedef struct walk_env_t {
	pset *visited_types;
	declaration_callback declaration_func;
	statement_callback   statement_func;
	expression_callback  expression_func;
	void *env;
} walk_env_t;

static void walk_expression(expression_t *expr, const walk_env_t *const env);
static void walk_statement(statement_t *stmt, const walk_env_t *env);
static void walk_entity(entity_t *entity, const walk_env_t *env);
static void walk_designator(const designator_t *, const walk_env_t *);
static void walk_initializer(const initializer_t  *, const walk_env_t *);
static void walk_scope(const scope_t *const scope, const walk_env_t *const env);

static void walk_type(type_t *const type, const walk_env_t *const env)
{
	if (pset_find_ptr(env->visited_types, type) != NULL)
		return;
	pset_insert_ptr(env->visited_types, type);

	switch (type->kind) {
	case TYPE_ATOMIC:
	case TYPE_COMPLEX:
	case TYPE_IMAGINARY:
	case TYPE_REFERENCE:
	case TYPE_ERROR:
		return;
	case TYPE_POINTER:
		walk_type(type->pointer.points_to, env);
		return;
	case TYPE_ARRAY:
		walk_type(type->array.element_type, env);
		if (type->array.size_expression != NULL)
			walk_expression(type->array.size_expression, env);
		return;
	case TYPE_FUNCTION:
		for (function_parameter_t *parameter = type->function.parameters;
		     parameter != NULL; parameter = parameter->next) {
			walk_type(parameter->type, env);
		}
		walk_type(type->function.return_type, env);
		return;
	case TYPE_TYPEOF:
		walk_expression(type->typeoft.expression, env);
		return;
	case TYPE_TYPEDEF:
		walk_entity((entity_t*)type->typedeft.typedefe, env);
		return;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		walk_entity((entity_t*)type->compound.compound, env);
		return;
	case TYPE_ENUM:
		walk_entity((entity_t*)type->enumt.enume, env);
		return;
	}
	panic("invalid type");
}

static void walk_expression(expression_t *const expr,
                            const walk_env_t *const env)
{
	env->expression_func(expr, env->env);

	switch (expr->base.kind) {
	case EXPR_STATEMENT:
		walk_statement(expr->statement.statement, env);
		return;

	case EXPR_BINARY_CASES:
		walk_expression(expr->binary.left, env);
		walk_expression(expr->binary.right, env);
		return;

	case EXPR_UNARY_CASES_OPTIONAL:
		if (expr->unary.value == NULL)
			return;
		/* FALLTHROUGH */
	case EXPR_UNARY_CASES_MANDATORY:
		walk_expression(expr->unary.value, env);
		return;

	case EXPR_CALL:
		for (call_argument_t *arg = expr->call.arguments; arg != NULL;
		     arg = arg->next) {
			walk_expression(arg->expression, env);
		}
		return;

	case EXPR_COMPOUND_LITERAL:
		walk_initializer(expr->compound_literal.initializer, env);
		return;

	case EXPR_CONDITIONAL:
		walk_expression(expr->conditional.condition, env);
		/* may be NULL because of gnu extension */
		if (expr->conditional.true_expression != NULL)
			walk_expression(expr->conditional.true_expression, env);
		walk_expression(expr->conditional.false_expression, env);
		return;

	case EXPR_BUILTIN_CONSTANT_P:
		walk_expression(expr->builtin_constant.value, env);
		return;

	case EXPR_BUILTIN_TYPES_COMPATIBLE_P:
		walk_type(expr->builtin_types_compatible.left, env);
		walk_type(expr->builtin_types_compatible.right, env);
		return;

	case EXPR_SELECT:
		walk_expression(expr->select.compound, env);
		return;

	case EXPR_ARRAY_ACCESS:
		walk_expression(expr->array_access.array_ref, env);
		walk_expression(expr->array_access.index, env);
		return;

	case EXPR_CLASSIFY_TYPE:
		walk_expression(expr->classify_type.type_expression, env);
		return;

	case EXPR_SIZEOF:
	case EXPR_ALIGNOF: {
		expression_t *tp_expression = expr->typeprop.tp_expression;
		if (tp_expression != NULL) {
			walk_expression(tp_expression, env);
		}
		return;
	}

	case EXPR_VA_START:
		walk_expression(expr->va_starte.ap, env);
		return;

	case EXPR_VA_ARG:
		walk_expression(expr->va_arge.ap, env);
		return;

	case EXPR_VA_COPY:
		walk_expression(expr->va_copye.src, env);
		walk_expression(expr->va_copye.dst, env);
		return;

	case EXPR_OFFSETOF:
		walk_designator(expr->offsetofe.designator, env);
		return;

	case EXPR_LITERAL_CASES:
	case EXPR_LITERAL_CHARACTER:
	case EXPR_REFERENCE:
	case EXPR_ENUM_CONSTANT:
	case EXPR_STRING_LITERAL:
	case EXPR_FUNCNAME:
	case EXPR_LABEL_ADDRESS:
	case EXPR_ERROR:
		return;
	}
	panic("invalid expr kind");
}

static void walk_designator(const designator_t *designator,
                            const walk_env_t *const env)
{
	for ( ; designator != NULL; designator = designator->next) {
		if (designator->array_index != NULL)
			walk_expression(designator->array_index, env);
	}
}

static void walk_initializer(const initializer_t  *initializer,
                             const walk_env_t *const env)
{
	switch (initializer->kind) {
	case INITIALIZER_VALUE:
		walk_expression(initializer->value.value, env);
		return;
	case INITIALIZER_LIST: {
		for (size_t i = 0; i < initializer->list.len; ++i) {
			walk_initializer(initializer->list.initializers[i], env);
		}
		return;
	}
	case INITIALIZER_DESIGNATOR:
		walk_designator(initializer->designator.designator, env);
		return;

	case INITIALIZER_STRING:
		return;
	}
}

static void walk_entity(entity_t *entity, const walk_env_t *const env)
{
	env->declaration_func(entity, env->env);

	switch (entity->kind) {
	case ENTITY_VARIABLE: {
		const variable_t    *variable    = &entity->variable;
		const initializer_t *initializer = variable->initializer;
		walk_type(entity->declaration.type, env);
		if (initializer != NULL) {
			walk_initializer(initializer, env);
		}
		return;
	}
	case ENTITY_ENUM_VALUE:
		if (entity->enum_value.value != NULL)
			walk_expression(entity->enum_value.value, env);
		return;
	case ENTITY_TYPEDEF:
		walk_type(entity->typedefe.type, env);
		return;
	case ENTITY_FUNCTION:
		walk_type(entity->declaration.type, env);
		if (entity->function.body != NULL)
			walk_statement(entity->function.body, env);
		return;
	case ENTITY_COMPOUND_MEMBER:
	case ENTITY_PARAMETER:
		walk_type(entity->declaration.type, env);
		return;
	case ENTITY_CLASS:
	case ENTITY_STRUCT:
	case ENTITY_UNION:
		walk_scope(&entity->compound.members, env);
		return;
	case ENTITY_NAMESPACE:
		walk_scope(&entity->namespacee.members, env);
		return;
	case ENTITY_ENUM:
		for (entity = entity->base.next;
		     entity != NULL && entity->kind == ENTITY_ENUM_VALUE;
			 entity = entity->base.next) {
			walk_entity(entity, env);
		}
		return;
	case ENTITY_LABEL:
	case ENTITY_LOCAL_LABEL:
		return;
	}
	panic("invalid entity");
}

static void walk_declarations(entity_t*            entity,
                              entity_t*      const last,
							  const walk_env_t    *const env)
{
	entity_t const *const end = last != NULL ? last->base.next : NULL;
	for (; entity != end; entity = entity->base.next) {
		walk_entity(entity, env);
	}
}

static void walk_scope(const scope_t *const scope, const walk_env_t *const env)
{
	walk_declarations(scope->entities, NULL, env);
}

static void walk_statement(statement_t *const stmt, const walk_env_t *const env)
{
	env->statement_func(stmt, env->env);

	switch (stmt->kind) {
	case STATEMENT_COMPOUND:
		for (statement_t *s = stmt->compound.statements; s != NULL;
		     s = s->base.next) {
			walk_statement(s, env);
		}
		return;

	case STATEMENT_FOR:
		walk_declarations(stmt->fors.scope.entities, NULL, env);
		if (stmt->fors.initialisation != NULL)
			walk_expression(stmt->fors.initialisation, env);
		if (stmt->fors.condition != NULL)
			walk_expression(stmt->fors.condition, env);
		if (stmt->fors.step != NULL)
			walk_expression(stmt->fors.step, env);
		walk_statement(stmt->fors.body, env);
		return;

	case STATEMENT_IF:
		walk_expression(stmt->ifs.condition, env);
		walk_statement(stmt->ifs.true_statement, env);
		if (stmt->ifs.false_statement != NULL)
			walk_statement(stmt->ifs.false_statement, env);
		return;

	case STATEMENT_SWITCH:
		walk_expression(stmt->switchs.expression, env);
		walk_statement(stmt->switchs.body, env);
		return;

	case STATEMENT_LABEL:
		walk_statement(stmt->label.statement, env);
		return;

	case STATEMENT_CASE_LABEL:
		if (stmt->case_label.expression) {
			walk_expression(stmt->case_label.expression, env);
			if (stmt->case_label.end_range)
				walk_expression(stmt->case_label.end_range, env);
		}
		walk_statement(stmt->case_label.statement, env);
		return;

	case STATEMENT_DO_WHILE:
		walk_statement(stmt->do_while.body, env);
		walk_expression(stmt->do_while.condition, env);
		return;

	case STATEMENT_EXPRESSION:
		walk_expression(stmt->expression.expression, env);
		return;

	case STATEMENT_RETURN:
		if (stmt->returns.value != NULL)
			walk_expression(stmt->returns.value, env);
		return;

	case STATEMENT_DECLARATION:
		walk_declarations(stmt->declaration.declarations_begin,
				stmt->declaration.declarations_end, env);
		return;

	case STATEMENT_MS_TRY:
		walk_statement(stmt->ms_try.try_statement, env);
		walk_statement(stmt->ms_try.final_statement, env);
		return;

	case STATEMENT_COMPUTED_GOTO:
		walk_expression(stmt->computed_goto.expression, env);
		return;

	case STATEMENT_ERROR:
	case STATEMENT_EMPTY:
	case STATEMENT_CONTINUE:
	case STATEMENT_BREAK:
	case STATEMENT_ASM:
	case STATEMENT_GOTO:
	case STATEMENT_LEAVE:
		return;
	}

	panic("unhandled statement");
}

static void null_declaration_func(entity_t *entity, void *env)
{
	(void) entity;
	(void) env;
}

static void null_statement_func(statement_t *statement, void *env)
{
	(void) statement;
	(void) env;
}

static void null_expression_func(expression_t *expression, void *env)
{
	(void) expression;
	(void) env;
}

void walk_translation_unit(translation_unit_t *unit,
                           declaration_callback declaration_func,
						   statement_callback statement_func,
						   expression_callback expression_func, void *env)
{
	walk_env_t walk_env = {
		pset_new_ptr_default(),
		declaration_func != NULL ? declaration_func : null_declaration_func,
		statement_func != NULL ? statement_func : null_statement_func,
		expression_func != NULL ? expression_func : null_expression_func,
		env
	};
	walk_scope(&unit->scope, &walk_env);
	del_pset(walk_env.visited_types);
}

void walk_statements(statement_t *statement, statement_callback func, void *env)
{
	walk_env_t walk_env
		= { pset_new_ptr_default(),
		    null_declaration_func, func, null_expression_func, env };
	walk_statement(statement, &walk_env);
	del_pset(walk_env.visited_types);
}
