/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef TYPE_T_H
#define TYPE_T_H

#include <assert.h>
#include <libfirm/firm_types.h>
#include <stdbool.h>

#include "adt/util.h"
#include "ast.h"
#include "type.h"

typedef enum type_kind_t {
	TYPE_ERROR = 1,
	TYPE_ATOMIC,
	TYPE_COMPLEX,
	TYPE_IMAGINARY,
	TYPE_COMPOUND_STRUCT,
	TYPE_COMPOUND_UNION,
	TYPE_ENUM,
	TYPE_FUNCTION,
	TYPE_POINTER,
	TYPE_REFERENCE,
	TYPE_ARRAY,
	TYPE_TYPEDEF,
	TYPE_TYPEOF,
	TYPE_VOID,
	/** type used for some type-generic builtins */
	TYPE_BUILTIN_TEMPLATE,
} type_kind_t;

struct type_base_t {
	ENUMBF(type_kind_t)       kind       : 8;
	ENUMBF(type_qualifiers_t) qualifiers : 8;

	/* cached ast2firm infos */
	ir_type *firm_type;
};

/**
 * used for atomic types, complex and imaginary and as base for enum
 */
struct atomic_type_t {
	type_base_t        base;
	atomic_type_kind_t akind;
};

struct pointer_type_t {
	type_base_t  base;
	type_t      *points_to;
};

struct reference_type_t {
	type_base_t  base;
	type_t      *refers_to;
};

struct array_type_t {
	type_base_t   base;
	type_t       *element_type;
	expression_t *size_expression;
	size_t        size;

	ir_node      *size_node; /**< used by ast2firm phase */

	bool          is_static         : 1; /**< a [static] type */
	bool          is_variable       : 1; /**< a [*] type */
	bool          has_implicit_size : 1;
	bool          size_constant     : 1; /**< size expression is constant */
	bool          is_vla            : 1; /**< it's a variable length array */
};

/**
 * An entry in the parameter list of a function type.
 */
struct function_parameter_t {
	type_t               *type; /**< The parameter type. */
	function_parameter_t *next; /**< Points to next type in parameter list.*/
};

/** Linkage specifications. */
typedef enum linkage_kind_t {
	LINKAGE_C = 1,   /**< C linkage. */
	LINKAGE_CXX      /**< C++ linkage. */
} linkage_kind_t;

/** Calling conventions. */
typedef enum cc_kind_t {
	CC_DEFAULT,      /**< default calling convention. */
	CC_CDECL,        /**< cdecl calling convention. */
	CC_STDCALL,      /**< stdcall calling convention. */
	CC_FASTCALL,     /**< fastcall calling convention. */
	CC_THISCALL      /**< thiscall calling convention. */
} cc_kind_t;

/**
 * A function type.
 */
struct function_type_t {
	type_base_t            base;
	type_t                *return_type;        /**< The return type. */
	function_parameter_t  *parameters;         /**< List of parameter types. */
	decl_modifiers_t       modifiers;
	bool                   variadic               : 1;
	bool                   unspecified_parameters : 1;
	bool                   kr_style_parameters    : 1;
	bool                   typegeneric            : 1;
	ENUMBF(linkage_kind_t) linkage                : 2;
	ENUMBF(cc_kind_t)      calling_convention     : 3;
};

struct compound_type_t {
	type_base_t  base;
	/** the declaration of the compound type, the scope of the declaration
	 *  contains the compound entries. */
	compound_t  *compound;
};

struct enum_type_t {
	atomic_type_t base;
	/** the enum entity. You can find the enum entries by walking the
	 *  enum->base.next list until you don't find ENTITY_ENUM_VALUE entities
	 *  anymore */
	enum_t       *enume;
};

struct typedef_type_t {
	type_base_t    base;
	declaration_t *typedefe;
	type_t        *resolved_type;
};

struct typeof_type_t {
	type_base_t   base;
	expression_t *expression;
	type_t       *typeof_type;
	type_t       *resolved_type;
};

union type_t {
	ENUMBF(type_kind_t) kind : 8;
	type_base_t         base;
	atomic_type_t       atomic;
	pointer_type_t      pointer;
	reference_type_t    reference;
	array_type_t        array;
	function_type_t     function;
	compound_type_t     compound;
	enum_type_t         enumt;
	typedef_type_t      typedeft;
	typeof_type_t       typeoft;
};

typedef struct atomic_type_properties_t atomic_type_properties_t;
struct atomic_type_properties_t {
	unsigned size;              /**< type size in bytes */
	unsigned alignment;         /**< type alignment in bytes */
	/** some ABIs are broken and require an alignment different from the
	 * recommended/best alignment inside structs. Fixing ABIs is difficult
	 * so people rather stick with the wrong values for compatibility.
	 * (double type on x86 System V ABI)
	 */
	unsigned            struct_alignment;
	atomic_type_flags_t flags;
	unsigned            rank;   /**< integer conversion rank */
};

extern atomic_type_properties_t atomic_type_properties[ATOMIC_TYPE_LAST+1];
extern atomic_type_properties_t pointer_properties;

/** The default calling convention for functions. */
extern cc_kind_t default_calling_convention;

type_t *make_atomic_type(atomic_type_kind_t type, type_qualifiers_t qualifiers);
type_t *make_complex_type(atomic_type_kind_t type, type_qualifiers_t qualifiers);
type_t *make_imaginary_type(atomic_type_kind_t type, type_qualifiers_t qualifiers);
type_t *make_pointer_type(type_t *points_to, type_qualifiers_t qualifiers);
type_t *make_reference_type(type_t *refers_to);
type_t *make_array_type(type_t *element_type, size_t size,
                        type_qualifiers_t qualifiers);

/**
 * Creates a new void type.
 *
 * @param qualifiers  Type qualifiers for the new type.
 */
type_t *make_void_type(type_qualifiers_t qualifiers);

function_parameter_t *allocate_parameter(type_t*);

/**
 * Duplicates a type.
 *
 * @param type  The type to copy.
 * @return A copy of the type.
 *
 * @note This does not produce a deep copy!
 */
type_t *duplicate_type(const type_t *type);

type_t *identify_new_type(type_t *type);

static inline bool is_typeref(const type_t *type)
{
	return type->kind == TYPE_TYPEDEF || type->kind == TYPE_TYPEOF;
}

static inline bool is_type_atomic(const type_t *type, atomic_type_kind_t atype)
{
	assert(!is_typeref(type));

	if (type->kind != TYPE_ATOMIC)
		return false;
	const atomic_type_t *atomic_type = &type->atomic;

	return atomic_type->akind == atype;
}

static inline bool is_type_void(type_t const *const type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_VOID;
}

static inline bool is_type_pointer(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_POINTER;
}

static inline bool is_type_reference(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_REFERENCE;
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

static inline bool is_type_compound(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind == TYPE_COMPOUND_STRUCT
		|| type->kind == TYPE_COMPOUND_UNION;
}

static inline bool is_type_valid(const type_t *type)
{
	assert(!is_typeref(type));
	return type->kind != TYPE_ERROR;
}

/**
 * return integer conversion rank of an atomic type kind
 */
static inline unsigned get_akind_rank(atomic_type_kind_t akind)
{
	return atomic_type_properties[akind].rank;
}

static inline bool is_akind_signed(atomic_type_kind_t akind)
{
	return atomic_type_properties[akind].flags & ATOMIC_TYPE_FLAG_SIGNED;
}

static inline atomic_type_kind_t get_arithmetic_akind(const type_t *type)
{
	assert(type->kind == TYPE_ATOMIC || type->kind == TYPE_COMPLEX
	       || type->kind == TYPE_IMAGINARY || type->kind == TYPE_ENUM);
	/* note that atomic, complex and enum share atomic_type_t base */
	return type->atomic.akind;
}

/**
 * Allocate a type node of given kind and initialize all
 * fields with zero.
 *
 * @param kind             type kind to allocate
 */
type_t *allocate_type_zero(type_kind_t kind);

/**
 * Creates a return_type (func)(void) function type if not
 * already exists.
 *
 * @param return_type    the return type
 */
type_t *make_function_0_type(type_t *return_type,
                             decl_modifiers_t modifiers);

/**
 * Creates a return_type (func)(argument_type) function type if not
 * already exists.
 *
 * @param return_type    the return type
 * @param argument_type  the argument type
 */
type_t *make_function_1_type(type_t *return_type, type_t *argument_type1,
                             decl_modifiers_t modifiers);


/**
 * Creates a return_type (func)(argument_type1,argument_type2) function type
 * if not already exists.
 */
type_t *make_function_2_type(type_t *return_type, type_t *argument_type1,
                             type_t *argument_type2,
                             decl_modifiers_t modifiers);

/**
 * Creates a return_type (func)(argument_type, ...) function type if not
 * already exists.
 *
 * @param return_type    the return type
 * @param argument_type  the argument type
 */
type_t *make_function_1_type_variadic(type_t *return_type,
                                      type_t *argument_type,
                                      decl_modifiers_t modifiers);

/**
 * Create a function type with n parameters
 */
type_t *make_function_type(type_t *return_type, int n_types,
                           type_t *const *argument_types,
						   decl_modifiers_t modifiers);

#endif
