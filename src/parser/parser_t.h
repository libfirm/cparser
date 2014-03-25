/*
 * This file is part of cparser.
 * Copyright (C) 2014 Matthias Braun <matze@braunis.de>
 */
#ifndef PARSER_T_H
#define PARSER_T_H

#include "parser.h"

/**
 * record entities for the NAMESPACE_NORMAL, and produce error messages/warnings
 * for various problems that occur for multiple definitions
 */
entity_t *record_entity(entity_t *entity, bool is_definition);

/**
 * Merge two declarations/a definition and a declaration.
 */
void merge_into_decl(entity_t *decl, const entity_t *other);

/**
 * Search an entity by its symbol in a given namespace.
 */
entity_t *get_entity(const symbol_t *const symbol, entity_namespace_t namespc);

#endif
