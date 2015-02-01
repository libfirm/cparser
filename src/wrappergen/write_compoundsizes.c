/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "ast/ast_t.h"
#include "ast/attribute_t.h"
#include "ast/entity_t.h"
#include "ast/type_t.h"
#include "write_compoundsizes.h"

void write_compoundsizes(FILE *output, const translation_unit_t *unit)
{
	for (const entity_t *entity = unit->scope.entities;
		 entity != NULL; entity = entity->base.next) {
		if (entity->kind != ENTITY_TYPEDEF)
			continue;

		type_t *type = skip_typeref(entity->declaration.type);
		if (!is_type_compound(type))
			continue;

		/* see if we have the required attributes */
		const compound_t *compound = type->compound.compound;
		bool        had_dllexport = false;
		const char *string        = NULL;
		for (const attribute_t *attrib = compound->attributes;
		     attrib != NULL; attrib = attrib->next) {
		    if (attrib->kind == ATTRIBUTE_GNU_DLLEXPORT)
				had_dllexport = true;
			if (attrib->kind == ATTRIBUTE_GNU_DEPRECATED) {
				const attribute_argument_t *argument = attrib->a.arguments;
				assert(argument != NULL && argument->kind == ATTRIBUTE_ARGUMENT_EXPRESSION);
				const expression_t *expr = argument->v.expression;
				assert(expr->kind == EXPR_STRING_LITERAL);
				string = expr->string_literal.value->begin;
			}
		}

		if (had_dllexport && string != NULL) {
			fprintf(output, "%s %u\n", string, get_type_size(type));
		}
	}
}
