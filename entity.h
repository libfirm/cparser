/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef ENTITY_H
#define ENTITY_H

typedef struct scope_t                      scope_t;

typedef struct entity_base_t                entity_base_t;
typedef struct compound_t                   compound_t;
typedef struct enum_t                       enum_t;
typedef struct enum_value_t                 enum_value_t;
typedef struct label_t                      label_t;
typedef struct namespace_t                  namespace_t;
typedef struct declaration_t                declaration_t;
typedef struct typedef_t                    typedef_t;
typedef struct variable_t                   variable_t;
typedef struct function_t                   function_t;
typedef struct compound_member_t            compound_member_t;
typedef union  entity_t                     entity_t;

typedef unsigned                            decl_modifiers_t;

#endif
