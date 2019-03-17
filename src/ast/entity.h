/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef ENTITY_H
#define ENTITY_H

typedef struct asm_argument_t    asm_argument_t;
typedef struct asm_label_t       asm_label_t;
typedef struct asm_operand_t     asm_operand_t;
typedef struct compound_member_t compound_member_t;
typedef struct compound_t        compound_t;
typedef struct declaration_t     declaration_t;
typedef struct entity_base_t     entity_base_t;
typedef struct enum_t            enum_t;
typedef struct enum_value_t      enum_value_t;
typedef struct function_t        function_t;
typedef struct label_t           label_t;
typedef struct namespace_t       namespace_t;
typedef struct scope_t           scope_t;
typedef struct typedef_t         typedef_t;
typedef struct variable_t        variable_t;
typedef union  entity_t          entity_t;

typedef enum decl_modifiers_t {
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
	DM_GNU_INLINE        = 1 << 29,
	DM_PACKED            = 1 << 30,
} decl_modifiers_t;

unsigned get_declaration_alignment(const declaration_t *declaration);

#endif
