#ifndef ATTRIBUTE_H
#define ATTRIBUTE_H

#include "entity.h"
#include "type.h"

typedef struct attribute_t attribute_t;

const char *get_deprecated_string(const attribute_t *attribute);

type_t *handle_type_attributes(const attribute_t *attributes, type_t *type);
void handle_entity_attributes(const attribute_t *attributes, entity_t *entity);
type_t *handle_attribute_mode(const attribute_t *attribute, type_t *orig_type);

#endif
