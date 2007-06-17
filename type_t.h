#ifndef TYPE_T_H
#define TYPE_T_H

#include "type.h"
#include "symbol.h"
#include "lexer_t.h"
#include "adt/obst.h"

struct obstack *type_obst;

typedef enum {
	TYPE_INVALID,
	TYPE_ATOMIC,
	TYPE_COMPOUND_STRUCT,
	TYPE_COMPOUND_UNION,
	TYPE_ENUM,
	TYPE_METHOD,
	TYPE_POINTER
} type_type_t;

typedef enum {
	ATOMIC_TYPE_INVALID,
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
#ifdef PROVIDE_COMPLEX
	ATOMIC_TYPE_FLOAT_COMPLEX,
	ATOMIC_TYPE_DOUBLE_COMPLEX,
	ATOMIC_TYPE_LONG_DOUBLE_COMPLEX,
#endif
#ifdef PROVIDE_IMAGINARY
	ATOMIC_TYPE_FLOAT_IMAGINARY,
	ATOMIC_TYPE_DOUBLE_IMAGINARY,
	ATOMIC_TYPE_LONG_DOUBLE_IMAGINARY,
#endif
} atomic_type_type_t;

typedef enum {
	TYPE_QUALIFIER_CONST    = 1 << 0,
	TYPE_QUALIFIER_RESTRICT = 1 << 1,
	TYPE_QUALIFIER_VOLATILE = 1 << 2,
	TYPE_QUALIFIER_INLINE   = 1 << 3,
} type_qualifier_t;

struct type_t {
	type_type_t  type;
	unsigned     qualifiers;
};

struct atomic_type_t {
	type_t              type;
	atomic_type_type_t  atype;
};

struct pointer_type_t {
	type_t   type;
	type_t  *points_to;
};

struct method_parameter_type_t {
	type_t                  *type;
	method_parameter_type_t *next;
};

struct method_type_t {
	type_t                   type;
	type_t                  *result_type;
	method_parameter_type_t *parameter_types;
	const char              *abi_style;
};

struct compound_entry_t {
	type_t            *type;
	symbol_t          *symbol;
	compound_entry_t  *next;
	source_position_t  source_position;
};

struct compound_type_t {
	type_t             type;
	compound_entry_t  *entries;
	symbol_t          *symbol;
	source_position_t  source_position;
};

struct enum_type_t {
	/* todo */
	symbol_t          *symbol;
	source_position_t  source_position;
};

#endif
