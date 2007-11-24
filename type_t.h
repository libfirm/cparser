#ifndef TYPE_T_H
#define TYPE_T_H

#include <stdbool.h>

#include <libfirm/firm_types.h>

#include "type.h"
#include "symbol.h"
#include "token_t.h"
#include "ast_t.h"
#include "adt/obst.h"

struct obstack *type_obst;

typedef enum {
	TYPE_INVALID,
	TYPE_ATOMIC,
	TYPE_COMPOUND_STRUCT,
	TYPE_COMPOUND_UNION,
	TYPE_ENUM,
	TYPE_FUNCTION,
	TYPE_POINTER,
	TYPE_ARRAY,
	TYPE_BUILTIN,
	TYPE_TYPEDEF,
	TYPE_TYPEOF
} type_type_t;

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
#ifdef PROVIDE_COMPLEX
	ATOMIC_TYPE_FLOAT_COMPLEX,
	ATOMIC_TYPE_DOUBLE_COMPLEX,
	ATOMIC_TYPE_LONG_DOUBLE_COMPLEX,
	ATOMIC_TYPE_FLOAT_IMAGINARY,
	ATOMIC_TYPE_DOUBLE_IMAGINARY,
	ATOMIC_TYPE_LONG_DOUBLE_IMAGINARY,
#endif
} atomic_type_type_t;

typedef enum {
	TYPE_QUALIFIER_NONE     = 0,
	TYPE_QUALIFIER_CONST    = 1 << 0,
	TYPE_QUALIFIER_RESTRICT = 1 << 1,
	TYPE_QUALIFIER_VOLATILE = 1 << 2,
} type_qualifier_t;

struct type_t {
	type_type_t  type;
	unsigned     qualifiers;

	ir_type     *firm_type;
};

struct atomic_type_t {
	type_t              type;
	atomic_type_type_t  atype;
};

struct builtin_type_t {
	type_t    type;
	symbol_t *symbol;
	type_t   *real_type;
};

struct pointer_type_t {
	type_t   type;
	type_t  *points_to;
};

struct array_type_t {
	type_t        type;
	type_t       *element_type;
	bool          is_static;
	bool          is_variable;
	expression_t *size;
};

struct function_parameter_t {
	type_t               *type;
	function_parameter_t *next;
};

struct function_type_t {
	type_t                type;
	type_t               *result_type;
	function_parameter_t *parameters;
	bool                  variadic;
	bool                  unspecified_parameters;
};

struct compound_type_t {
	type_t         type;
	/** the declaration of the compound type, its context field
	 * contains the compound entries. */
	declaration_t *declaration;
};

struct enum_type_t {
	type_t         type;
	/** the declaration of the enum type. You can find the enum entries by
	 * walking the declaration->next list until you don't find
	 * STORAGE_CLASS_ENUM_ENTRY declarations anymore */
	declaration_t *declaration;
};

struct typedef_type_t {
	type_t         type;
	declaration_t *declaration;
	type_t        *resolved_type;
};

struct typeof_type_t {
	type_t        type;
	expression_t *expression;
	type_t       *typeof_type;
	type_t       *resolved_type;
};

type_t *make_atomic_type(atomic_type_type_t type, type_qualifier_t qualifiers);
type_t *make_pointer_type(type_t *points_to, type_qualifier_t qualifiers);

#endif
