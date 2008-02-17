#ifndef TYPE_T_H
#define TYPE_T_H

#include <stdbool.h>
#include <assert.h>

#include <libfirm/firm_types.h>

#include "type.h"
#include "symbol.h"
#include "token_t.h"
#include "ast_t.h"
#include "adt/obst.h"

extern struct obstack *type_obst;

typedef enum {
	TYPE_INVALID,
	TYPE_ERROR,
	TYPE_ATOMIC,
	TYPE_COMPOUND_STRUCT,
	TYPE_COMPOUND_UNION,
	TYPE_ENUM,
	TYPE_FUNCTION,
	TYPE_POINTER,
	TYPE_ARRAY,
	TYPE_BITFIELD,
	TYPE_BUILTIN,
	TYPE_TYPEDEF,
	TYPE_TYPEOF,
} type_kind_t;

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
	ATOMIC_TYPE_LAST
} atomic_type_kind_t;

typedef enum {
	TYPE_QUALIFIER_NONE     = 0,
	TYPE_QUALIFIER_CONST    = 1 << 0,
	TYPE_QUALIFIER_RESTRICT = 1 << 1,
	TYPE_QUALIFIER_VOLATILE = 1 << 2,
} type_qualifier_t;

typedef unsigned int type_qualifiers_t;

struct type_base_t {
	type_kind_t       kind;
	type_qualifiers_t qualifiers;
	source_position_t source_position;

	ir_type          *firm_type;
};

struct atomic_type_t {
	type_base_t         type;
	atomic_type_kind_t  akind;
};

struct builtin_type_t {
	type_base_t  type;
	symbol_t    *symbol;
	type_t      *real_type;
};

struct pointer_type_t {
	type_base_t  type;
	type_t      *points_to;
};

struct array_type_t {
	type_base_t   type;
	type_t       *element_type;
	expression_t *size_expression;
	size_t        size;

	ir_node      *size_node; /**< used by ast2firm phase */

	unsigned      is_static         : 1; /**< a [static] type */
	unsigned      is_variable       : 1; /**< a [*] type */
	unsigned      has_implicit_size : 1;
	unsigned      size_constant     : 1; /**< size expression is constant */
	unsigned      is_vla            : 1; /**< it's a variable length array */
};

struct function_parameter_t {
	type_t               *type;
	function_parameter_t *next;
};

struct function_type_t {
	type_base_t           type;
	type_t               *return_type;
	function_parameter_t *parameters;
	unsigned              variadic : 1;
	unsigned              unspecified_parameters : 1;
	unsigned              kr_style_parameters : 1;
};

struct compound_type_t {
	type_base_t    type;
	/** the declaration of the compound type, the scope of the declaration
	 *  contains the compound entries. */
	declaration_t *declaration;
};

struct enum_type_t {
	type_base_t    type;
	/** the declaration of the enum type. You can find the enum entries by
	 *  walking the declaration->next list until you don't find
	 *  STORAGE_CLASS_ENUM_ENTRY declarations anymore */
	declaration_t *declaration;
};

struct typedef_type_t {
	type_base_t    type;
	declaration_t *declaration;
	type_t        *resolved_type;
};

struct typeof_type_t {
	type_base_t   type;
	expression_t *expression;
	type_t       *typeof_type;
	type_t       *resolved_type;
};

struct bitfield_type_t {
	type_base_t   type;
	type_t       *base;
	expression_t *size;
};

union type_t {
	type_kind_t      kind;
	type_base_t      base;
	atomic_type_t    atomic;
	builtin_type_t   builtin;
	pointer_type_t   pointer;
	array_type_t     array;
	function_type_t  function;
	compound_type_t  compound;
	enum_type_t      enumt;
	typedef_type_t   typedeft;
	bitfield_type_t  bitfield;
	typeof_type_t    typeoft;
};

type_t *make_atomic_type(atomic_type_kind_t type, type_qualifiers_t qualifiers);
type_t *make_pointer_type(type_t *points_to, type_qualifiers_t qualifiers);
type_t *make_array_type(type_t *element_type, size_t size,
                        type_qualifiers_t qualifiers);

type_t *duplicate_type(const type_t *type);

static inline bool is_typeref(const type_t *type)
{
	return type->kind == TYPE_TYPEDEF || type->kind == TYPE_TYPEOF;
}

static inline bool is_type_atomic(const type_t *type, atomic_type_kind_t atype)
{
	assert(!is_typeref(type));

	if(type->kind != TYPE_ATOMIC)
		return false;
	const atomic_type_t *atomic_type = &type->atomic;

	return atomic_type->akind == atype;
}

static inline bool is_type_pointer(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_POINTER;
}

static inline bool is_type_array(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_ARRAY;
}

static inline bool is_type_function(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_FUNCTION;
}

static inline bool is_type_union(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_COMPOUND_UNION;
}

static inline bool is_type_struct(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_COMPOUND_STRUCT;
}

static inline bool is_type_builtin(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_BUILTIN;
}

static inline bool is_type_compound(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_COMPOUND_STRUCT
		|| type->kind == TYPE_COMPOUND_UNION;
}

static inline bool is_type_valid(const type_t *type)
{
	return type->kind != TYPE_ERROR;
}

#endif
