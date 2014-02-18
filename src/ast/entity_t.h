/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef ENTITY_T_H
#define ENTITY_T_H

#include <libfirm/firm_types.h>

#include "adt/util.h"
#include "ast/ast.h"
#include "ast/attribute.h"
#include "ast/entity.h"
#include "ast/symbol.h"
#include "firm/jump_target.h"
#include "position.h"

typedef enum {
	ENTITY_VARIABLE = 1,
	ENTITY_COMPOUND_MEMBER,
	ENTITY_PARAMETER,
	ENTITY_FUNCTION,
	ENTITY_TYPEDEF,
	ENTITY_CLASS,
	ENTITY_STRUCT,
	ENTITY_UNION,
	ENTITY_ENUM,
	ENTITY_ENUM_VALUE,
	ENTITY_LABEL,
	ENTITY_LOCAL_LABEL,
	ENTITY_NAMESPACE,
	ENTITY_ASM_ARGUMENT,
} entity_kind_t;

typedef enum entity_namespace_t {
	NAMESPACE_NORMAL = 1,
	NAMESPACE_TAG,
	NAMESPACE_LABEL,
	NAMESPACE_ASM_ARGUMENT,
} entity_namespace_t;

typedef enum storage_class_t {
	STORAGE_CLASS_NONE,
	STORAGE_CLASS_EXTERN,
	STORAGE_CLASS_STATIC,
	STORAGE_CLASS_TYPEDEF,
	STORAGE_CLASS_AUTO,
	STORAGE_CLASS_REGISTER,
} storage_class_t;

typedef enum decl_modifier_t {
	DM_NONE              = 0,
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
	DM_TRANSPARENT_UNION = 1 << 13,
	DM_CONST             = 1 << 14,
	DM_PURE              = 1 << 15,
	DM_CONSTRUCTOR       = 1 << 16,
	DM_DESTRUCTOR        = 1 << 17,
	DM_UNUSED            = 1 << 18,
	DM_USED              = 1 << 19,
	DM_CDECL             = 1 << 20,
	DM_FASTCALL          = 1 << 21,
	DM_STDCALL           = 1 << 22,
	DM_THISCALL          = 1 << 23,
	DM_DEPRECATED        = 1 << 24,
	DM_RETURNS_TWICE     = 1 << 25,
	DM_MALLOC            = 1 << 26,
	DM_WEAK              = 1 << 27,
	DM_LEAF              = 1 << 28,
} decl_modifier_t;

typedef enum elf_visibility_t {
	ELF_VISIBILITY_DEFAULT,
	ELF_VISIBILITY_HIDDEN,
	ELF_VISIBILITY_INTERNAL,
	ELF_VISIBILITY_PROTECTED,
	ELF_VISIBILITY_ERROR
} elf_visibility_t;

typedef enum {
	BUILTIN_NONE,
	BUILTIN_ALLOCA,
	BUILTIN_INF,
	BUILTIN_NAN,
	BUILTIN_EXPECT,
	BUILTIN_VA_END,
	BUILTIN_OBJECT_SIZE,
	BUILTIN_ROTL,
	BUILTIN_ROTR,
	BUILTIN_LIBC,
	BUILTIN_LIBC_CHECK,
	BUILTIN_FIRM,
} builtin_kind_t;

/**
 * A scope containing entities.
 */
struct scope_t {
	entity_t *entities;
	entity_t *last_entity; /**< pointer to last entity (so appending is fast) */
	unsigned  depth;       /**< while parsing, the depth of this scope in the
	                            scope stack. */
};

/**
 * a named entity is something which can be referenced by its name
 * (a symbol)
 */
struct entity_base_t {
	ENUMBF(entity_kind_t)      kind    : 8;
	ENUMBF(entity_namespace_t) namespc : 8;
	symbol_t                  *symbol;
	position_t                 pos;
	scope_t                   *parent_scope;  /**< The scope where this entity
	                                               is contained in */
	entity_t                  *parent_entity;

	/** next declaration in a scope */
	entity_t           *next;
	/** next declaration with same symbol */
	entity_t           *symbol_next;
};

struct compound_t {
	entity_base_t     base;
	entity_t         *alias; /* used for name mangling of anonymous types */
	scope_t           members;
	decl_modifiers_t  modifiers;
	attribute_t      *attributes;
	bool              complete          : 1;
	bool              transparent_union : 1;
	bool              packed            : 1;

	il_alignment_t    alignment;
	il_size_t         size;
};

struct enum_t {
	entity_base_t  base;
	entity_t      *alias; /* used for name mangling of anonymous types */
	bool           complete : 1;
};

struct enum_value_t {
	entity_base_t  base;
	expression_t  *value;
	type_t        *enum_type;

	/* ast2firm info */
	ir_tarval     *tv;
};

struct label_t {
	entity_base_t  base;
	bool           used : 1;
	bool           address_taken : 1;
	unsigned       n_users; /* Reference counter to mature the label block as early as possible. */
	statement_t   *statement;

	/* ast2firm info */
	jump_target    target;
	ir_node       *indirect_block;
};

struct namespace_t {
	entity_base_t  base;
	scope_t        members;
};

struct typedef_t {
	entity_base_t     base;
	decl_modifiers_t  modifiers;
	type_t           *type;
	il_alignment_t    alignment;
	bool              builtin : 1;
};

struct declaration_t {
	entity_base_t     base;
	type_t           *type;
	ENUMBF(storage_class_t) declared_storage_class : 8;
	ENUMBF(storage_class_t) storage_class          : 8;
	decl_modifiers_t  modifiers;
	il_alignment_t    alignment;
	attribute_t      *attributes;
	bool              used     : 1;  /**< Set if the declaration is used. */
	bool              implicit : 1;  /**< Set for implicit (not found in source code) declarations. */

	/* ast2firm info */
	unsigned char     kind;
};

struct compound_member_t {
	declaration_t  base;
	il_size_t      offset;     /**< the offset of this member in the compound */
	unsigned char  bit_offset; /**< extra bit offset for bitfield members */
	unsigned char  bit_size;   /**< bitsize for bitfield members */
	bool           bitfield : 1;  /**< member is (part of) a bitfield */

	/* ast2firm info */
	ir_entity *entity;
};

struct variable_t {
	declaration_t  base;
	bool           thread_local   : 1;
	bool           address_taken  : 1;  /**< Set if the address of this
	                                         declaration was taken. */
	bool           read           : 1;
	unsigned       elf_visibility : 2;
	initializer_t *initializer;
	union {
		symbol_t  *symbol;
		entity_t  *entity;
	} alias;                            /**< value from attribute((alias())) */

	/* ast2firm info */
	union {
		unsigned int  value_number;
		ir_entity    *entity;
		ir_node      *vla_base;
	} v;
};

struct function_t {
	declaration_t  base;
	bool           is_inline      : 1;
	bool           need_closure   : 1;  /**< Inner function needs closure. */
	bool           goto_to_outer  : 1;  /**< Inner function has goto to outer
	                                         function. */
	unsigned       elf_visibility : 2;
	builtin_kind_t btk;
	scope_t        parameters;
	statement_t   *body;
	symbol_t      *actual_name;        /**< gnu extension __REDIRECT */
	union {
		symbol_t  *symbol;
		entity_t  *entity;
	} alias;                           /**< value from attribute((alias())) */

	/* ast2firm info */
	union {
		ir_builtin_kind firm_builtin_kind;
		unsigned        chk_arg_pos;
	} b;
	ir_entity      *irentity;
	ir_node        *static_link;        /**< if need_closure is set, the node
	                                         representing the static link. */
};

struct asm_argument_t {
	entity_base_t base;
	string_t      constraints;
	expression_t *expression;
	unsigned      pos;
	bool          direct_read:1;    /**< argument value is read */
	bool          direct_write:1;   /**< argument is lvalue and written to */
	bool          indirect_read:1;  /**< argument is address which is read */
	bool          indirect_write:1; /**< argument is address which is written */
};

union entity_t {
	ENUMBF(entity_kind_t) kind : 8;
	entity_base_t         base;
	compound_t            compound;
	enum_t                enume;
	enum_value_t          enum_value;
	label_t               label;
	namespace_t           namespacee;
	typedef_t             typedefe;
	declaration_t         declaration;
	variable_t            variable;
	function_t            function;
	compound_member_t     compound_member;
	asm_argument_t        asm_argument;
};

#define DECLARATION_KIND_CASES \
	     ENTITY_FUNCTION:        \
	case ENTITY_VARIABLE:        \
	case ENTITY_PARAMETER:       \
	case ENTITY_COMPOUND_MEMBER

static inline bool is_declaration(const entity_t *entity)
{
	switch (entity->kind) {
	case DECLARATION_KIND_CASES:
		return true;
	default:
		return false;
	}
}

const char *get_entity_kind_name(entity_kind_t kind);

entity_t *allocate_entity_zero(entity_kind_t, entity_namespace_t, symbol_t*, position_t const*);

elf_visibility_t get_elf_visibility_from_string(const char *string);

entity_t *skip_unnamed_bitfields(entity_t*);

#endif
