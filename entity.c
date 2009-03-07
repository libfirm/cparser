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
#include <config.h>

#include "entity_t.h"
#include "adt/error.h"

const char *get_entity_kind_name(entity_kind_t kind)
{
	switch ((entity_kind_tag_t) kind) {
	case ENTITY_FUNCTION:        return "function";
	case ENTITY_VARIABLE:        return "variable";
	case ENTITY_PARAMETER:       return "parameter";
	case ENTITY_COMPOUND_MEMBER: return "compound member";
	case ENTITY_STRUCT:          return "struct";
	case ENTITY_UNION:           return "union";
	case ENTITY_ENUM:            return "enum";
	case ENTITY_ENUM_VALUE:      return "enum value";
	case ENTITY_LABEL:           return "label";
	case ENTITY_LOCAL_LABEL:     return "local label";
	case ENTITY_TYPEDEF:         return "typedef";
	case ENTITY_NAMESPACE:       return "namespace";
	case ENTITY_INVALID:         break;
	}

	panic("Invalid entity kind encountered in get_entity_kind_name");
}
