/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef ATTRIBUTE_T_H
#define ATTRIBUTE_T_H

#include "ast.h"
#include "attribute.h"
#include "symbol.h"

/**
 * GNU attributes.
 */
typedef enum attribute_kind_t {
	ATTRIBUTE_ERROR,
	ATTRIBUTE_UNKNOWN,
	ATTRIBUTE_GNU_FIRST,
	ATTRIBUTE_CPARSER_CUSTOM_FORMAT = ATTRIBUTE_GNU_FIRST,
	ATTRIBUTE_GNU_ALIAS,
	ATTRIBUTE_GNU_ALIGNED,
	ATTRIBUTE_GNU_ALLOC_SIZE,
	ATTRIBUTE_GNU_ALWAYS_INLINE,
	ATTRIBUTE_GNU_ASM,
	ATTRIBUTE_GNU_CDECL,
	ATTRIBUTE_GNU_COMMON,
	ATTRIBUTE_GNU_CONST,
	ATTRIBUTE_GNU_CONSTRUCTOR,
	ATTRIBUTE_GNU_DEPRECATED,
	ATTRIBUTE_GNU_DESTRUCTOR,
	ATTRIBUTE_GNU_DLLEXPORT,
	ATTRIBUTE_GNU_DLLIMPORT,
	ATTRIBUTE_GNU_EIGTHBIT_DATA,
	ATTRIBUTE_GNU_EXTERNALLY_VISIBLE,
	ATTRIBUTE_GNU_FAR,
	ATTRIBUTE_GNU_FASTCALL,
	ATTRIBUTE_GNU_FLATTEN,
	ATTRIBUTE_GNU_FORMAT,
	ATTRIBUTE_GNU_FORMAT_ARG,
	ATTRIBUTE_GNU_FUNCTION_VECTOR,
	ATTRIBUTE_GNU_GCC_STRUCT,
	ATTRIBUTE_GNU_GNU_INLINE,
	ATTRIBUTE_GNU_INTERRUPT,
	ATTRIBUTE_GNU_INTERRUPT_HANDLER,
	ATTRIBUTE_GNU_LEAF,
	ATTRIBUTE_GNU_LONGCALL,
	ATTRIBUTE_GNU_LONG_CALL,
	ATTRIBUTE_GNU_MALLOC,
	ATTRIBUTE_GNU_MAY_ALIAS,
	ATTRIBUTE_GNU_MODE,
	ATTRIBUTE_GNU_MODEL,
	ATTRIBUTE_GNU_MS_STRUCT,
	ATTRIBUTE_GNU_NAKED,
	ATTRIBUTE_GNU_NEAR,
	ATTRIBUTE_GNU_NESTING,
	ATTRIBUTE_GNU_NMI_HANDLER,
	ATTRIBUTE_GNU_NOCOMMON,
	ATTRIBUTE_GNU_NOINLINE,
	ATTRIBUTE_GNU_NO_INSTRUMENT_FUNCTION,
	ATTRIBUTE_GNU_NONNULL,
	ATTRIBUTE_GNU_NORETURN,
	ATTRIBUTE_GNU_NOTHROW,
	ATTRIBUTE_GNU_NOTSHARED,
	ATTRIBUTE_GNU_PACKED,
	ATTRIBUTE_GNU_PURE,
	ATTRIBUTE_GNU_REGPARM,
	ATTRIBUTE_GNU_RETURNS_TWICE,
	ATTRIBUTE_GNU_RETURN_TWICE,
	ATTRIBUTE_GNU_SAVEALL,
	ATTRIBUTE_GNU_SECTION,
	ATTRIBUTE_GNU_SENTINEL,
	ATTRIBUTE_GNU_SHARED,
	ATTRIBUTE_GNU_SHORTCALL,
	ATTRIBUTE_GNU_SHORT_CALL,
	ATTRIBUTE_GNU_SIGNAL,
	ATTRIBUTE_GNU_SP_SWITCH,
	ATTRIBUTE_GNU_SSEREGPARM,
	ATTRIBUTE_GNU_STDCALL,
	ATTRIBUTE_GNU_TINY_DATA,
	ATTRIBUTE_GNU_TLS_MODEL,
	ATTRIBUTE_GNU_TRANSPARENT_UNION,
	ATTRIBUTE_GNU_TRAP_EXIT,
	ATTRIBUTE_GNU_UNUSED,
	ATTRIBUTE_GNU_USED,
	ATTRIBUTE_GNU_VISIBILITY,
	ATTRIBUTE_GNU_VOLATILE,
	ATTRIBUTE_GNU_WARN_UNUSED_RESULT,
	ATTRIBUTE_GNU_WEAK,
	ATTRIBUTE_GNU_WEAKREF,
	ATTRIBUTE_GNU_WEAK_IMPORT,
	ATTRIBUTE_GNU_LAST = ATTRIBUTE_GNU_WEAK_IMPORT,
	ATTRIBUTE_ICORE_FIRST,
	ATTRIBUTE_ICORE_SPECIAL_INSTRUCTION = ATTRIBUTE_ICORE_FIRST,
	ATTRIBUTE_ICORE_LAST = ATTRIBUTE_ICORE_SPECIAL_INSTRUCTION,
	ATTRIBUTE_MS_FIRST,
	ATTRIBUTE_MS_ALIGN = ATTRIBUTE_MS_FIRST,
	ATTRIBUTE_MS_ALLOCATE,
	ATTRIBUTE_MS_BASED,
	ATTRIBUTE_MS_CDECL,
	ATTRIBUTE_MS_DEPRECATED,
	ATTRIBUTE_MS_DLLEXPORT,
	ATTRIBUTE_MS_DLLIMPORT,
	ATTRIBUTE_MS_FASTCALL,
	ATTRIBUTE_MS_FORCEINLINE,
	ATTRIBUTE_MS_NAKED,
	ATTRIBUTE_MS_NOALIAS,
	ATTRIBUTE_MS_NOINLINE,
	ATTRIBUTE_MS_NORETURN,
	ATTRIBUTE_MS_NOTHROW,
	ATTRIBUTE_MS_NOVTABLE,
	ATTRIBUTE_MS_PROPERTY,
	ATTRIBUTE_MS_RESTRICT,
	ATTRIBUTE_MS_RETURNS_TWICE,
	ATTRIBUTE_MS_SELECTANY,
	ATTRIBUTE_MS_STDCALL,
	ATTRIBUTE_MS_THISCALL,
	ATTRIBUTE_MS_THREAD,
	ATTRIBUTE_MS_UUID,
	ATTRIBUTE_MS_LAST = ATTRIBUTE_MS_UUID,
	ATTRIBUTE_LAST = ATTRIBUTE_MS_LAST
} attribute_kind_t;

typedef enum attribute_argument_kind_t {
	ATTRIBUTE_ARGUMENT_SYMBOL,
	ATTRIBUTE_ARGUMENT_EXPRESSION
} attribute_argument_kind_t;

/** this argument type should be fine for 99% of the arguments */
typedef struct attribute_argument_t attribute_argument_t;
struct attribute_argument_t {
	attribute_argument_t      *next;
	attribute_argument_kind_t  kind;
	union {
		symbol_t     *symbol;
		expression_t *expression;
	} v;
};

typedef struct attribute_property_argument_t attribute_property_argument_t;
struct attribute_property_argument_t {
	symbol_t *put_symbol;
	symbol_t *get_symbol;
};

typedef struct attribute_format_specifier_t attribute_format_specifier_t;
struct attribute_format_specifier_t {
	attribute_format_specifier_t *next;
	string_t                     *specifier;
	type_t                       *type;
};

typedef struct attribute_format_argument_t {
	expression_t                 *fmt_idx;
	string_t                     *flags;
	attribute_format_specifier_t *specifiers;
} attribute_format_argument_t;

struct attribute_t {
	position_t        pos;
	attribute_t      *next;
	attribute_kind_t  kind;         /**< The kind of the GNU attribute. */
	union {
		attribute_argument_t          *arguments;
		attribute_property_argument_t *property;
		attribute_format_argument_t   *format;
	} a;
};

const char *get_attribute_name(attribute_kind_t kind);

bool attributes_equal(const attribute_t *attr1, const attribute_t *attr2);

attribute_t const *get_attribute(attribute_t const *attributes, attribute_kind_t kind);

#endif
