/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef ATTRIBUTE_H
#define ATTRIBUTE_H

#include "entity.h"
#include "string_rep.h"
#include "type.h"

typedef struct attribute_t attribute_t;

string_t const *get_deprecated_string(attribute_t const *attribute);

type_t *handle_type_attributes(const attribute_t *attributes, type_t *type);
void handle_entity_attributes(const attribute_t *attributes, entity_t *entity);
type_t *handle_attribute_mode(const attribute_t *attribute, type_t *orig_type);

/** array of entities using attribute((alias())) */
extern entity_t **alias_entities;

#endif
