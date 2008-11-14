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
#ifndef ENTITY_T_H
#define ENTITY_T_H

#include "lexer.h"
#include "symbol.h"
#include "entity.h"

typedef enum {
	ENTITY_INVALID,
	ENTITY_VARIABLE,
	ENTITY_COMPOUND_MEMBER,
	ENTITY_FUNCTION,
	ENTITY_TYPEDEF,
	ENTITY_STRUCT,
	ENTITY_UNION,
	ENTITY_ENUM,
	ENTITY_ENUM_VALUE,
	ENTITY_LABEL,
	ENTITY_LOCAL_LABEL,
	ENTITY_NAMESPACE
} entity_kind_tag_t;
typedef unsigned char entity_kind_t;

typedef enum namespace_tag_t {
	NAMESPACE_INVALID,
	NAMESPACE_NORMAL,
	NAMESPACE_STRUCT,
	NAMESPACE_UNION,
	NAMESPACE_ENUM,
	NAMESPACE_LABEL
} namespace_tag_t;
typedef unsigned char entity_namespace_t;

typedef enum storage_class_tag_t {
	STORAGE_CLASS_NONE,
	STORAGE_CLASS_EXTERN,
	STORAGE_CLASS_STATIC,
	STORAGE_CLASS_TYPEDEF,
	STORAGE_CLASS_AUTO,
	STORAGE_CLASS_REGISTER,
	STORAGE_CLASS_THREAD,
	STORAGE_CLASS_THREAD_EXTERN,
	STORAGE_CLASS_THREAD_STATIC,
} storage_class_tag_t;
typedef unsigned char storage_class_t;

typedef enum decl_modifier_t {
	DM_DLLIMPORT         = 1 <<  0,
	DM_DLLEXPORT         = 1 <<  1,
	DM_THREAD            = 1 <<  2,
	DM_NAKED             = 1 <<  3,
	DM_MICROSOFT_INLINE  = 1 <<  4,
	DM_FORCEINLINE       = 1 <<  5,
	DM_SELECTANY         = 1 <<  6,
	DM_NOTHROW           = 1 <<  7,
	DM_NOVTABLE          = 1 <<  8,
	DM_NORETURN          = 1 <<  9,
	DM_NOINLINE          = 1 << 10,
	DM_RESTRICT          = 1 << 11,
	DM_NOALIAS           = 1 << 12,
	DM_PACKED            = 1 << 13,
	DM_TRANSPARENT_UNION = 1 << 14,
	DM_CONST             = 1 << 15,
	DM_PURE              = 1 << 16,
	DM_CONSTRUCTOR       = 1 << 17,
	DM_DESTRUCTOR        = 1 << 18,
	DM_UNUSED            = 1 << 19,
	DM_USED              = 1 << 20,
	DM_CDECL             = 1 << 21,
	DM_FASTCALL          = 1 << 22,
	DM_STDCALL           = 1 << 23,
	DM_THISCALL          = 1 << 24,
	DM_DEPRECATED        = 1 << 25
} decl_modifier_t;

typedef unsigned decl_modifiers_t;

/**
 * A scope containing entities.
 */
struct scope_t {
	entity_t *entities;
	entity_t *last_entity;
	scope_t  *parent;       /**< points to the parent scope. */
	unsigned  depth;        /**< while parsing, the depth of this scope in the
	                             scope stack. */
};

/**
 * a named entity is something which can be referenced by its name
 * (a symbol)
 */
struct entity_base_t {
	entity_kind_t       kind;
	entity_namespace_t  namespc;
	symbol_t           *symbol;
	source_position_t   source_position;
	scope_t            *parent_scope;       /**< The parent scope where this declaration lives. */

	/** next declaration in a scope */
	entity_t           *next;
	/** next declaration with same symbol */
	entity_t           *symbol_next;
};

struct compound_t {
	entity_base_t     base;
	scope_t           members;
	decl_modifiers_t  modifiers;
	bool              complete            : 1;
	bool              has_flexible_member : 1;

	/* ast2firm info */
	ir_type          *irtype;
	bool              irtype_complete : 1;
};

struct enum_t {
	entity_base_t  base;
	bool           complete : 1;

	/* ast2firm info */
	ir_type       *irtype;
};

struct enum_value_t {
	entity_base_t  base;
	expression_t  *value;
	type_t        *enum_type;

	/* ast2firm info */
	tarval        *tv;
};

struct label_t {
	entity_base_t  base;
	bool           used : 1;
	bool           address_taken : 1;
	statement_t   *statement;

	/* ast2firm info */
	ir_node       *block;
};

struct namespace_t {
	entity_base_t  base;
	scope_t        members;
};

struct typedef_t {
	entity_base_t     base;
	decl_modifiers_t  modifiers;
	type_t           *type;
	bool              builtin : 1;
};

struct declaration_t {
	entity_base_t     base;
	storage_class_t   declared_storage_class;
	storage_class_t   storage_class;
	decl_modifiers_t  modifiers;
	const char       *deprecated_string;  /**< MS deprecated string if any. */
	bool              used          : 1;  /**< Set if the declaration is used. */
	bool              implicit      : 1;  /**< Set for implicit (not found in source code) declarations. */
	type_t           *type;

	/* ast2firm info */
	unsigned char     kind;
};

struct compound_member_t {
	declaration_t  base;
	bool           read : 1;

	/* ast2firm info */
	ir_entity *entity;
	il_size_t  offset;  /**< The offset of this member inside a compound. */
};

struct variable_t {
	declaration_t  base;
	bool           address_taken : 1;  /**< Set if the address of this declaration was taken. */
	bool           read          : 1;
	unsigned char  alignment;          /**< Alignment of the declaration, 0 for default. */
	symbol_t      *get_property_sym;   /**< MS get property. */
	symbol_t      *put_property_sym;   /**< MS put property. */

	initializer_t *initializer;

	/* ast2firm info */
	union {
		unsigned int  value_number;
		ir_entity    *entity;
		ir_node      *vla_base;
	} v;
};

struct function_t {
	declaration_t  base;
	bool           is_inline     : 1;
	bool           need_closure  : 1;  /**< Inner function needs closure. */
	bool           goto_to_outer : 1;  /**< Inner function has goto to outer function. */

	scope_t        parameters;
	statement_t   *statement;

	/* ast2firm info */
	ir_entity     *entity;
};

union entity_t {
	entity_kind_t      kind;
	entity_base_t      base;
	compound_t         structe;
	compound_t         unione;
	compound_t         compound;
	enum_t             enume;
	enum_value_t       enum_value;
	label_t            label;
	namespace_t        namespacee;
	typedef_t          typedefe;
	declaration_t      declaration;
	variable_t         variable;
	function_t         function;
	compound_member_t  compound_member;
};

static inline bool is_declaration(const entity_t *entity)
{
	return entity->kind == ENTITY_FUNCTION || entity->kind == ENTITY_VARIABLE
		|| entity->kind == ENTITY_COMPOUND_MEMBER;
}

#endif
