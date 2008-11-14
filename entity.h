/*
 * This file is part of cparser.
 * Copyright (C) 2007-2008 Matthias Braun <matze@braunis.de>
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

#endif
