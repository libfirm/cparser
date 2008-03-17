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

/* note that the constant values represent the rank of the types as defined
 * in § 6.3.1 */
typedef enum {
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
	/* microsoft extensions */
	ATOMIC_TYPE_INT8,
	ATOMIC_TYPE_INT16,
	ATOMIC_TYPE_INT32,
	ATOMIC_TYPE_INT64,
	ATOMIC_TYPE_INT128,
	ATOMIC_TYPE_UINT8,
	ATOMIC_TYPE_UINT16,
	ATOMIC_TYPE_UINT32,
	ATOMIC_TYPE_UINT64,
	ATOMIC_TYPE_UINT128,

	ATOMIC_TYPE_FLOAT_COMPLEX,
	ATOMIC_TYPE_DOUBLE_COMPLEX,
	ATOMIC_TYPE_LONG_DOUBLE_COMPLEX,
	ATOMIC_TYPE_FLOAT_IMAGINARY,
	ATOMIC_TYPE_DOUBLE_IMAGINARY,
	ATOMIC_TYPE_LONG_DOUBLE_IMAGINARY,

	ATOMIC_TYPE_LAST = ATOMIC_TYPE_LONG_DOUBLE_IMAGINARY
} atomic_type_kind_t;

typedef struct type_base_t           type_base_t;
typedef struct atomic_type_t         atomic_type_t;
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
                    const scope_t *scope);

void print_type_qualifiers(unsigned qualifiers);

void print_enum_definition(const declaration_t *declaration);
void print_compound_definition(const declaration_t *declaration);

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
 * return true if type contains signed numbers
 */
bool is_type_signed(const type_t *type);

/**
 * returns true if type contains floating point numbers
 */
bool is_type_float(const type_t *type);

/**
 * returns true if the type is valid. A type is valid if it contains no
 * unresolved references anymore and is not of TYPE_INVALID.
 */
bool type_valid(const type_t *type);

/**
 * returns true if the type is an arithmetic type (6.2.18)
 */
bool is_type_arithmetic(const type_t *type);

/**
 * returns true if the type is a scalar type (6.2.21)
 */
bool is_type_scalar(const type_t *type);

bool is_type_incomplete(const type_t *type);

bool types_compatible(const type_t *type1, const type_t *type2);

bool pointers_compatible(const type_t *type1, const type_t *type2);

type_t *get_unqualified_type(type_t *type);
type_t *skip_typeref(type_t *type);

/**
 * returns size of an atomic type kind in bytes
 */
unsigned get_atomic_type_size(atomic_type_kind_t kind);

/**
 * returns alignment of an atomic type kind in bytes
 */
unsigned get_atomic_type_align(atomic_type_kind_t kind);

#endif
