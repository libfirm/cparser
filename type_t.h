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

typedef unsigned int type_qualifiers_t;

struct function_parameter_t {
	type_t               *type;
	function_parameter_t *next;
};

struct type_t {
	type_type_t       type;
	type_qualifiers_t qualifiers;

	union {
		/* if type == TYPE_ATOMIC */
		struct {
			atomic_type_type_t  atype;
		} atomic_type;
		/* if type == TYPE_COMPOUND_STRUCT or type == TYPE_COMPOUND_UNION */
		struct {
			/** the declaration of the compound type, its context field
			 * contains the compound entries. */
			declaration_t *declaration;
		} compound_type;
		/* if type == TYPE_ENUM */
		struct {
			/** the declaration of the enum type. You can find the enum entries by
			 * walking the declaration->next list until you don't find
			 * STORAGE_CLASS_ENUM_ENTRY declarations anymore */
			declaration_t *declaration;
		} enum_type;
		/* if type == TYPE_FUNCTION */
		struct  {
			type_t               *result_type;
			function_parameter_t *parameters;
			bool                  variadic;
			bool                  unspecified_parameters;
		} function_type;
		/* if type == TYPE_POINTER */
		struct {
			type_t  *points_to;
		} pointer_type;
		/* if type == TYPE_ARRAY */
		struct {
			type_t       *element_type;
			bool          is_static;
			bool          is_variable;
			expression_t *size;
		} array_type;
		/* if type == TYPE_BUILTIN */
		struct {
			symbol_t *symbol;
			type_t   *real_type;
		} builtin_type;
		/* if type == TYPE_TYPEDEF */
		struct {
			declaration_t *declaration;
			type_t        *resolved_type;
		} typedef_type;
		/* if type == TYPE_TYPEOF */
		struct {
			expression_t *expression;
			type_t       *typeof_type;
			type_t       *resolved_type;
		} typeof_type;
	} v;

	ir_type          *firm_type;
};

type_t *make_atomic_type(atomic_type_type_t type, type_qualifiers_t qualifiers);
type_t *make_pointer_type(type_t *points_to, type_qualifiers_t qualifiers);

#endif
