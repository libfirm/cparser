/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include <config.h>

#include <assert.h>

#include "entity_t.h"
#include "ast_t.h"
#include "adt/error.h"
#include "adt/util.h"
#include "adt/strutil.h"

const char *get_entity_kind_name(entity_kind_t kind)
{
	switch ((entity_kind_tag_t) kind) {
	case ENTITY_FUNCTION:        return "function";
	case ENTITY_VARIABLE:        return "variable";
	case ENTITY_PARAMETER:       return "parameter";
	case ENTITY_COMPOUND_MEMBER: return "compound member";
	case ENTITY_CLASS:           return "class";
	case ENTITY_STRUCT:          return "struct";
	case ENTITY_UNION:           return "union";
	case ENTITY_ENUM:            return "enum";
	case ENTITY_ENUM_VALUE:      return "enum value";
	case ENTITY_LABEL:           return "label";
	case ENTITY_LOCAL_LABEL:     return "local label";
	case ENTITY_TYPEDEF:         return "typedef";
	case ENTITY_NAMESPACE:       return "namespace";
	case ENTITY_ASM_ARGUMENT:    return "asm argument";
	}

	panic("invalid entity kind");
}

/**
 * Returns the size of an entity node.
 *
 * @param kind  the entity kind
 */
static size_t get_entity_struct_size(entity_kind_t kind)
{
	static const size_t sizes[] = {
		[ENTITY_VARIABLE]        = sizeof(variable_t),
		[ENTITY_PARAMETER]       = sizeof(variable_t),
		[ENTITY_COMPOUND_MEMBER] = sizeof(compound_member_t),
		[ENTITY_FUNCTION]        = sizeof(function_t),
		[ENTITY_TYPEDEF]         = sizeof(typedef_t),
		[ENTITY_STRUCT]          = sizeof(compound_t),
		[ENTITY_UNION]           = sizeof(compound_t),
		[ENTITY_ENUM]            = sizeof(enum_t),
		[ENTITY_ENUM_VALUE]      = sizeof(enum_value_t),
		[ENTITY_LABEL]           = sizeof(label_t),
		[ENTITY_LOCAL_LABEL]     = sizeof(label_t),
		[ENTITY_NAMESPACE]       = sizeof(namespace_t),
		[ENTITY_ASM_ARGUMENT]    = sizeof(asm_argument_t),
	};
	assert(kind < ARRAY_SIZE(sizes));
	assert(sizes[kind] != 0);
	return sizes[kind];
}

/**
 * Allocate an entity of given kind and initialize all
 * fields with zero.
 *
 * @param kind   the kind of the entity to allocate
 */
entity_t *allocate_entity_zero(entity_kind_t const kind, entity_namespace_t const namespc, symbol_t *const symbol, position_t const *const pos)
{
	size_t    const size   = get_entity_struct_size(kind);
	entity_t *const entity = allocate_ast_zero(size);
	entity->kind           = kind;
	entity->base.namespc   = namespc;
	entity->base.symbol    = symbol;
	entity->base.pos       = *pos;
	return entity;
}

elf_visibility_tag_t get_elf_visibility_from_string(const char *string)
{
	if (streq(string, "default")) {
		return ELF_VISIBILITY_DEFAULT;
	} else if (streq(string, "hidden")) {
		return ELF_VISIBILITY_HIDDEN;
	} else if (streq(string, "internal")) {
		return ELF_VISIBILITY_INTERNAL;
	} else if (streq(string, "protected")) {
		return ELF_VISIBILITY_PROTECTED;
	} else {
		return ELF_VISIBILITY_ERROR;
	}
}

entity_t *skip_unnamed_bitfields(entity_t *entry)
{
	for (; entry != NULL; entry = entry->base.next) {
		assert(entry->kind == ENTITY_COMPOUND_MEMBER);
		if (!entry->compound_member.bitfield || entry->base.symbol != NULL)
			break;
	}
	return entry;
}
