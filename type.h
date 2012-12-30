/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef TYPE_H
#define TYPE_H

#include <stdio.h>
#include <stdbool.h>

#include "entity.h"
#include "symbol.h"

/** Type used to express sizes. */
typedef unsigned long il_size_t;
typedef unsigned char il_alignment_t;

/* note that the constant values represent the rank of the types as defined
 * in § 6.3.1 */
typedef enum atomic_type_kind_t {
	ATOMIC_TYPE_BOOL = 1,
	ATOMIC_TYPE_WCHAR_T, /* only used in C++, in C code wchar_t is a pp-macro */
	ATOMIC_TYPE_CHAR,
	ATOMIC_TYPE_SCHAR,
	ATOMIC_TYPE_UCHAR,
	ATOMIC_TYPE_SHORT,
	ATOMIC_TYPE_USHORT,
	ATOMIC_TYPE_INT,
	ATOMIC_TYPE_UINT,
	ATOMIC_TYPE_LONG,
	ATOMIC_TYPE_ULONG,
	ATOMIC_TYPE_LONGLONG,
	ATOMIC_TYPE_ULONGLONG,
	ATOMIC_TYPE_FLOAT,
	ATOMIC_TYPE_DOUBLE,
	ATOMIC_TYPE_LONG_DOUBLE,

	ATOMIC_TYPE_LAST = ATOMIC_TYPE_LONG_DOUBLE
} atomic_type_kind_t;

typedef enum atomic_type_flag_t {
	ATOMIC_TYPE_FLAG_NONE       = 0,
	ATOMIC_TYPE_FLAG_SIGNED     = 1 << 0,
	ATOMIC_TYPE_FLAG_INTEGER    = 1 << 1,
	ATOMIC_TYPE_FLAG_FLOAT      = 1 << 2,
	ATOMIC_TYPE_FLAG_ARITHMETIC = 1 << 3,
} atomic_type_flag_t;

typedef enum type_qualifier_t {
	TYPE_QUALIFIER_NONE     = 0,
	TYPE_QUALIFIER_CONST    = 1 << 0,
	TYPE_QUALIFIER_RESTRICT = 1 << 1,
	TYPE_QUALIFIER_VOLATILE = 1 << 2,
	/* microsoft extended qualifiers */
	TYPE_QUALIFIER_W64      = 1 << 3,
	TYPE_QUALIFIER_PTR32    = 1 << 4,
	TYPE_QUALIFIER_PTR64    = 1 << 5,
	TYPE_QUALIFIER_SPTR     = 1 << 6,
	TYPE_QUALIFIER_UPTR     = 1 << 7,
} type_qualifier_t;
typedef unsigned short type_qualifiers_t;

typedef struct type_base_t           type_base_t;
typedef struct atomic_type_t         atomic_type_t;
typedef struct pointer_type_t        pointer_type_t;
typedef struct reference_type_t      reference_type_t;
typedef struct function_parameter_t  function_parameter_t;
typedef struct function_type_t       function_type_t;
typedef struct compound_type_t       compound_type_t;
typedef struct enum_type_t           enum_type_t;
typedef struct builtin_type_t        builtin_type_t;
typedef struct array_type_t          array_type_t;
typedef struct typedef_type_t        typedef_type_t;
typedef struct bitfield_type_t       bitfield_type_t;
typedef struct typeof_type_t         typeof_type_t;
typedef union  type_t                type_t;

/**
 * Initializes the type system. Attempts to set some defaults on the atomic
 * types based on a given machine size.
 * These type properties are not final but you should adapt them to your system
 * as your architecture and operating systems application binary interface (ABI)
 * requires.
 */
void init_types(unsigned machine_size);
void exit_types(void);

/**
 * Prints a type.
 *
 * @param type   The type.
 */
void print_type(const type_t *type);

/**
 * prints a human readable form of @p type. prints an abstract typename
 * if symbol is NULL
 */
void print_type_ext(const type_t *type, const symbol_t *symbol,
                    const scope_t *parameters);

typedef enum QualifierSeparators {
	QUAL_SEP_NONE  = 0,
	QUAL_SEP_START = 1U << 0,
	QUAL_SEP_END   = 1U << 1
} QualifierSeparators;

void print_type_qualifiers(type_qualifiers_t qualifiers, QualifierSeparators);

/**
 * Prints an enum definition.
 *
 * @param declaration  The enum's type declaration.
 */
void print_enum_definition(const enum_t *enume);

/**
 * Print the compound part of a compound type.
 */
void print_compound_definition(const compound_t *compound);

void inc_type_visited(void);

/**
 * Returns true if the given type is an integer type.
 *
 * @param type  The type to check.
 * @return True if type is an integer type.
 */
bool is_type_integer(const type_t *type);

/**
 * Returns true if the given type is an enum type.
 *
 * @param type  The type to check.
 * @return True if type is an enum type.
 */
bool is_type_enum(const type_t *type);

/**
 * Returns true if the given type is a signed type.
 *
 * @param type  The type to check.
 * @return True if type is a signed type.
 */
bool is_type_signed(const type_t *type);

/**
 * Returns true if the given type is a floating point type.
 *
 * @param type  The type to check.
 * @return True if type is a floating point type.
 */
bool is_type_float(const type_t *type);

/**
 * Returns true if the given type is a complex type.
 *
 * @param type  The type to check.
 * @return True if type is a complex type.
 */
bool is_type_complex(const type_t *type);

/**
 * Returns true if the given type is an integer or float type.
 *
 * @param type  The type to check.
 * @return True if type is an integer or float type.
 */
bool is_type_real(const type_t *type);

/**
 * Returns true if the type is an arithmetic type (§6.2.5 clause 18)
 *
 * @param type  The type to check.
 * @return True if type represents an arithmetic type.
 */
bool is_type_arithmetic(const type_t *type);

/**
 * Returns true if the type is a scalar type (§6.2.5 clause 21)
 *
 * @param type  The type to check.
 * @return True if type represents a scalar type.
 */
bool is_type_scalar(const type_t *type);

/**
 * Check if a given type is incomplete.
 *
 * @param type  The type to check.
 * @return True if the given type is incomplete (i.e. just forward).
 */
bool is_type_incomplete(const type_t *type);

bool is_type_object(const type_t *type);

/**
 * Check if two types are compatible.
 */
bool types_compatible(const type_t *type1, const type_t *type2);

/**
 * Returns the unqualified type of a given type.
 *
 * @param type  The type.
 * @returns The unqualified type.
 */
type_t *get_unqualified_type(type_t *type);

type_t *get_qualified_type(type_t*, type_qualifiers_t);
type_t *skip_typeref(type_t *type);

/**
 * Return the type qualifier set of a type. If skip_array_type
 * is true, skip all array types.
 */
type_qualifiers_t get_type_qualifier(const type_t *type, bool skip_array_type);

/**
 * returns size of an atomic type kind in bytes
 */
unsigned get_atomic_type_size(atomic_type_kind_t kind);

/**
 * returns alignment of an atomic type kind in bytes
 */
unsigned get_atomic_type_alignment(atomic_type_kind_t kind);

unsigned get_type_alignment(type_t const *type);

/**
 * Get alignment of a type when used inside a compound.
 * Some ABIs are broken and alignment inside a compound is different from
 * recommended alignment of a type.
 */
unsigned get_type_alignment_compound(type_t const *type);

unsigned get_type_size(type_t const *type);

decl_modifiers_t get_type_modifiers(const type_t *type);

/**
 * returns flags of an atomic type kind
 */
unsigned get_atomic_type_flags(atomic_type_kind_t kind);

/**
 * Find the atomic type kind representing a given size (signed).
 */
atomic_type_kind_t find_signed_int_atomic_type_kind_for_size(unsigned size);

/**
 * Find the atomic type kind representing a given size (unsigned).
 */
atomic_type_kind_t find_unsigned_int_atomic_type_kind_for_size(unsigned size);

const char *get_atomic_kind_name(atomic_type_kind_t kind);

/**
 * Finish the construction of a compound by calculating its size, offsets and
 * alignment.
 */
void layout_compound(compound_t *compound);

#endif
