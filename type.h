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
#ifndef TYPE_H
#define TYPE_H

#include <stdio.h>
#include <stdbool.h>
#include "ast.h"
#include "symbol.h"

/** Type used to express sizes. */
typedef unsigned long il_size_t;
typedef unsigned char il_alignment_t;

/* note that the constant values represent the rank of the types as defined
 * in § 6.3.1 */
typedef enum atomic_type_kind_t {
	ATOMIC_TYPE_INVALID = 0,
	ATOMIC_TYPE_VOID,
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
	ATOMIC_TYPE_BOOL,

	ATOMIC_TYPE_LAST = ATOMIC_TYPE_BOOL
} atomic_type_kind_t;

typedef enum atomic_type_flag_t {
	ATOMIC_TYPE_FLAG_NONE       = 0,
	ATOMIC_TYPE_FLAG_SIGNED     = 1 << 0,
	ATOMIC_TYPE_FLAG_INTEGER    = 1 << 1,
	ATOMIC_TYPE_FLAG_FLOAT      = 1 << 2,
	ATOMIC_TYPE_FLAG_ARITHMETIC = 1 << 3,
	ATOMIC_TYPE_FLAG_COMPLEX    = 1 << 4,
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
typedef struct complex_type_t        complex_type_t;
typedef struct imaginary_type_t      imaginary_type_t;
typedef struct pointer_type_t        pointer_type_t;
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

void init_types(void);
void exit_types(void);

void print_type(const type_t *type);

/**
 * prints a human readable form of @p type. prints an abstract typename
 * if symbol is NULL
 */
void print_type_ext(const type_t *type, const symbol_t *symbol,
                    const scope_t *parameters);

void print_type_qualifiers(type_qualifiers_t qualifiers);

void print_enum_definition(const enum_t *enume);
void print_compound_definition(const compound_t *compound);

/**
 * set output stream for the type printer
 */
void type_set_output(FILE *out);

void inc_type_visited(void);

/**
 * returns true if type contains integer numbers
 */
bool is_type_integer(const type_t *type);

/**
 * Returns true if the given type is an enum type.
 */
bool is_type_enum(const type_t *type);

/**
 * return true if type contains signed numbers
 */
bool is_type_signed(const type_t *type);

/**
 * returns true if type contains floating point numbers
 */
bool is_type_float(const type_t *type);

/**
 * returns true if type contains complex numbers
 */
bool is_type_complex(const type_t *type);

bool is_type_real(const type_t *type);

/**
 * returns true if the type is valid. A type is valid if it contains no
 * unresolved references anymore and is not of TYPE_INVALID.
 */
bool type_valid(const type_t *type);

/**
 * returns true if the type is an arithmetic type (§6.2.5 clause 18)
 */
bool is_type_arithmetic(const type_t *type);

/**
 * returns true if the type is a scalar type (§6.2.5 clause 21)
 */
bool is_type_scalar(const type_t *type);

bool is_type_incomplete(const type_t *type);

bool is_type_object(const type_t *type);

bool types_compatible(const type_t *type1, const type_t *type2);

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

/**
 * returns flags of an atomic type kind
 */
unsigned get_atomic_type_flags(atomic_type_kind_t kind);

atomic_type_kind_t get_intptr_kind(void);
atomic_type_kind_t get_uintptr_kind(void);

/**
 * Find the atomic type kind representing a given size (signed).
 */
atomic_type_kind_t find_signed_int_atomic_type_kind_for_size(unsigned size);

/**
 * Find the atomic type kind representing a given size (unsigned).
 */
atomic_type_kind_t find_unsigned_int_atomic_type_kind_for_size(unsigned size);

#endif
