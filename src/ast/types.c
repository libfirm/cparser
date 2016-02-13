/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "types.h"

#include <libfirm/be.h>

#include "ast/entity_t.h"
#include "ast/position.h"
#include "ast/symbol.h"
#include "dialect.h"
#include "parser/parser.h"
#include "type_t.h"

/** The error type. */
type_t *type_error_type;

type_t *type_bool;
type_t *type_char;
type_t *type_const_char;
type_t *type_double;
type_t *type_float;
type_t *type_int;
type_t *type_long_double;
type_t *type_long_long;
type_t *type_long;
type_t *type_short;
type_t *type_unsigned_short;
type_t *type_signed_char;
type_t *type_unsigned_char;
type_t *type_unsigned_int;
type_t *type_unsigned_long_long;
type_t *type_unsigned_long;
type_t *type_void;
type_t *type_builtin_template;

type_t *type_char_ptr;
type_t *type_char_ptr_restrict;
type_t *type_const_char_ptr;
type_t *type_const_char_ptr_restrict;
type_t *type_double_ptr;
type_t *type_float_ptr;
type_t *type_int_ptr;
type_t *type_long_double_ptr;
type_t *type_long_long_ptr;
type_t *type_long_ptr;
type_t *type_unsigned_char_ptr;
type_t *type_unsigned_short_ptr;
type_t *type_unsigned_int_ptr;
type_t *type_unsigned_long_ptr;
type_t *type_unsigned_long_long_ptr;
type_t *type_short_ptr;
type_t *type_signed_char_ptr;
type_t *type_void_ptr;
type_t *type_const_void;
type_t *type_const_void_ptr;
type_t *type_void_ptr_restrict;
type_t *type_const_void_ptr_restrict;
type_t *type_builtin_template_ptr;

type_t *type_char_ptr_ptr;

type_t *type_char16_t;
type_t *type_char32_t;
type_t *type_char16_t_const;
type_t *type_char32_t_const;
type_t *type_intmax_t;
type_t *type_ptrdiff_t;
type_t *type_size_t;
type_t *type_ssize_t;
type_t *type_uintmax_t;
type_t *type_uptrdiff_t;
type_t *type_wchar_t;
type_t *type_const_wchar_t;
type_t *type_wint_t;
type_t *type_int32_t;
type_t *type_int64_t;

type_t *type_char16_t_ptr;
type_t *type_char32_t_ptr;
type_t *type_char16_t_const_ptr;
type_t *type_char32_t_const_ptr;
type_t *type_intmax_t_ptr;
type_t *type_uintmax_t_ptr;
type_t *type_ptrdiff_t_ptr;
type_t *type_uptrdiff_t_ptr;
type_t *type_ssize_t_ptr;
type_t *type_size_t_ptr;
type_t *type_wchar_t_ptr;
type_t *type_const_wchar_t_ptr;

type_t *type_valist;
type_t *type_valist_arg;

/* microsoft types */
atomic_type_kind_t int8_type_kind;
atomic_type_kind_t int16_type_kind;
atomic_type_kind_t int32_type_kind;
atomic_type_kind_t int64_type_kind;
atomic_type_kind_t unsigned_int8_type_kind;
atomic_type_kind_t unsigned_int16_type_kind;
atomic_type_kind_t unsigned_int32_type_kind;
atomic_type_kind_t unsigned_int64_type_kind;

type_t *type_int8;
type_t *type_int16;
type_t *type_int32;
type_t *type_int64;
type_t *type_unsigned_int8;
type_t *type_unsigned_int16;
type_t *type_unsigned_int32;
type_t *type_unsigned_int64;
type_t *type_int32_ptr;
type_t *type_int64_ptr;
type_t *type_unsigned_int32_ptr;
type_t *type_unsigned_int64_ptr;

static void init_ms_types(void)
{
	type_int8                = make_atomic_type(int8_type_kind, TYPE_QUALIFIER_NONE);
	type_int16               = make_atomic_type(int16_type_kind, TYPE_QUALIFIER_NONE);
	type_int32               = make_atomic_type(int32_type_kind, TYPE_QUALIFIER_NONE);
	type_int64               = make_atomic_type(int64_type_kind, TYPE_QUALIFIER_NONE);
	unsigned_int8_type_kind  = find_unsigned_int_atomic_type_kind_for_size(1);
	type_unsigned_int8       = make_atomic_type(unsigned_int8_type_kind, TYPE_QUALIFIER_NONE);
	unsigned_int16_type_kind = find_unsigned_int_atomic_type_kind_for_size(2);
	type_unsigned_int16      = make_atomic_type(unsigned_int16_type_kind, TYPE_QUALIFIER_NONE);
	unsigned_int32_type_kind = find_unsigned_int_atomic_type_kind_for_size(4);
	type_unsigned_int32      = make_atomic_type(unsigned_int32_type_kind, TYPE_QUALIFIER_NONE);
	unsigned_int64_type_kind = find_unsigned_int_atomic_type_kind_for_size(8);
	type_unsigned_int64      = make_atomic_type(unsigned_int64_type_kind, TYPE_QUALIFIER_NONE);

	/* pointer types */
	type_int32_ptr           = make_pointer_type(type_int32,              TYPE_QUALIFIER_NONE);
	type_int64_ptr           = make_pointer_type(type_int64,              TYPE_QUALIFIER_NONE);
	type_unsigned_int32_ptr  = make_pointer_type(type_unsigned_int32,     TYPE_QUALIFIER_NONE);
	type_unsigned_int64_ptr  = make_pointer_type(type_unsigned_int64,     TYPE_QUALIFIER_NONE);
}

void init_predefined_types(void)
{
	static const type_base_t error = { TYPE_ERROR, TYPE_QUALIFIER_NONE, NULL };

	type_error_type         = (type_t*)&error;
	type_bool               = make_atomic_type(ATOMIC_TYPE_BOOL,        TYPE_QUALIFIER_NONE);
	type_signed_char        = make_atomic_type(ATOMIC_TYPE_SCHAR,       TYPE_QUALIFIER_NONE);
	type_unsigned_char      = make_atomic_type(ATOMIC_TYPE_UCHAR,       TYPE_QUALIFIER_NONE);
	type_short              = make_atomic_type(ATOMIC_TYPE_SHORT,       TYPE_QUALIFIER_NONE);
	type_unsigned_short     = make_atomic_type(ATOMIC_TYPE_USHORT,      TYPE_QUALIFIER_NONE);
	type_int                = make_atomic_type(ATOMIC_TYPE_INT,         TYPE_QUALIFIER_NONE);
	type_unsigned_int       = make_atomic_type(ATOMIC_TYPE_UINT,        TYPE_QUALIFIER_NONE);
	type_long               = make_atomic_type(ATOMIC_TYPE_LONG,        TYPE_QUALIFIER_NONE);
	type_unsigned_long      = make_atomic_type(ATOMIC_TYPE_ULONG,       TYPE_QUALIFIER_NONE);
	type_long_long          = make_atomic_type(ATOMIC_TYPE_LONGLONG,    TYPE_QUALIFIER_NONE);
	type_unsigned_long_long = make_atomic_type(ATOMIC_TYPE_ULONGLONG,   TYPE_QUALIFIER_NONE);
	type_long_double        = make_atomic_type(ATOMIC_TYPE_LONG_DOUBLE, TYPE_QUALIFIER_NONE);
	type_double             = make_atomic_type(ATOMIC_TYPE_DOUBLE,      TYPE_QUALIFIER_NONE);
	type_float              = make_atomic_type(ATOMIC_TYPE_FLOAT,       TYPE_QUALIFIER_NONE);
	type_char               = make_atomic_type(ATOMIC_TYPE_CHAR,        TYPE_QUALIFIER_NONE);

	type_void       = make_void_type(TYPE_QUALIFIER_NONE);
	type_const_void = make_void_type(TYPE_QUALIFIER_CONST);

	type_builtin_template = allocate_type_zero(TYPE_BUILTIN_TEMPLATE);
	type_builtin_template = identify_new_type(type_builtin_template);

	int8_type_kind  = find_signed_int_atomic_type_kind_for_size(1);
	int16_type_kind = find_signed_int_atomic_type_kind_for_size(2);
	int32_type_kind = find_signed_int_atomic_type_kind_for_size(4);
	int64_type_kind = find_signed_int_atomic_type_kind_for_size(8);

	type_int32_t = make_atomic_type(int32_type_kind, TYPE_QUALIFIER_NONE);
	type_int64_t = make_atomic_type(int64_type_kind, TYPE_QUALIFIER_NONE);

	/* pointer types */
	type_void_ptr                = make_pointer_type(type_void,              TYPE_QUALIFIER_NONE);
	type_const_void_ptr          = make_pointer_type(type_const_void,        TYPE_QUALIFIER_NONE);
	type_void_ptr_restrict       = make_pointer_type(type_void,              TYPE_QUALIFIER_RESTRICT);
	type_const_void_ptr_restrict = make_pointer_type(type_const_void,        TYPE_QUALIFIER_RESTRICT);
	type_char_ptr                = make_pointer_type(type_char,              TYPE_QUALIFIER_NONE);
	type_char_ptr_restrict       = make_pointer_type(type_char,              TYPE_QUALIFIER_RESTRICT);
	type_signed_char_ptr         = make_pointer_type(type_signed_char,       TYPE_QUALIFIER_NONE);
	type_short_ptr               = make_pointer_type(type_short,             TYPE_QUALIFIER_NONE);
	type_int_ptr                 = make_pointer_type(type_int,               TYPE_QUALIFIER_NONE);
	type_long_ptr                = make_pointer_type(type_long,              TYPE_QUALIFIER_NONE);
	type_unsigned_char_ptr       = make_pointer_type(type_unsigned_char,     TYPE_QUALIFIER_NONE);
	type_unsigned_short_ptr      = make_pointer_type(type_unsigned_short,    TYPE_QUALIFIER_NONE);
	type_unsigned_int_ptr        = make_pointer_type(type_unsigned_int,      TYPE_QUALIFIER_NONE);
	type_unsigned_long_ptr       = make_pointer_type(type_unsigned_long,     TYPE_QUALIFIER_NONE);
	type_unsigned_long_long_ptr  = make_pointer_type(type_unsigned_long,     TYPE_QUALIFIER_NONE);
	type_long_long_ptr           = make_pointer_type(type_long_long,         TYPE_QUALIFIER_NONE);
	type_long_double_ptr         = make_pointer_type(type_long_double,       TYPE_QUALIFIER_NONE);
	type_double_ptr              = make_pointer_type(type_double,            TYPE_QUALIFIER_NONE);
	type_float_ptr               = make_pointer_type(type_float,             TYPE_QUALIFIER_NONE);

	type_char_ptr_ptr            = make_pointer_type(type_char_ptr,          TYPE_QUALIFIER_NONE);

	type_builtin_template_ptr    = make_pointer_type(type_builtin_template,  TYPE_QUALIFIER_NONE);

	backend_params const *const be_params = be_get_backend_param();
	ir_type *be_va_list_type = be_params->vararg.va_list_type;
	if (!be_va_list_type) {
		/* Backend has no vararg support. Just hope the the program will not be
		 * using any. If it does, the parse_va_* functions will complain. */
		type_valist     = type_error_type;
		type_valist_arg = type_error_type;
	} else if (is_Pointer_type(be_va_list_type)) {
		type_valist     = type_void_ptr;
		type_valist_arg = type_void_ptr;
	} else if (is_Struct_type(be_va_list_type)) {
		entity_t *ent = allocate_entity_zero(ENTITY_STRUCT, NAMESPACE_NORMAL, sym_anonymous, &builtin_position);
		ent->compound.alignment = get_type_alignment(be_va_list_type);
		ent->compound.size      = get_type_size(be_va_list_type);
		ent->compound.complete  = true;
		ent->compound.members   = (scope_t){
			.first_entity = NULL,
			.last_entity  = NULL,
			.depth = 0
		};

		type_t *type_valist_struct = allocate_type_zero(TYPE_COMPOUND_STRUCT);
		type_valist_struct->base.firm_type = be_va_list_type;
		type_valist_struct->compound.compound = &ent->compound;

		type_valist     = make_array_type(type_valist_struct, 1, TYPE_QUALIFIER_NONE);
		type_valist_arg = automatic_type_conversion(type_valist);
	}

	/* const character types */
	type_const_char         = make_atomic_type(ATOMIC_TYPE_CHAR,        TYPE_QUALIFIER_CONST);
	type_const_char_ptr     = make_pointer_type(type_const_char,        TYPE_QUALIFIER_NONE);
	type_const_char_ptr_restrict = make_pointer_type(type_const_char,        TYPE_QUALIFIER_RESTRICT);

	atomic_type_kind_t pointer_sized_int  = dialect.pointer_sized_int;
	atomic_type_kind_t pointer_sized_uint = dialect.pointer_sized_uint;
	type_size_t     = make_atomic_type(pointer_sized_uint, TYPE_QUALIFIER_NONE);
	type_ssize_t    = make_atomic_type(pointer_sized_int, TYPE_QUALIFIER_NONE);
	type_uptrdiff_t = type_size_t;
	type_ptrdiff_t  = type_ssize_t;

	type_intmax_t  = type_long_long;
	type_uintmax_t = type_unsigned_long_long;
	type_wint_t    = type_unsigned_int;
	type_intmax_t_ptr   = make_pointer_type(type_intmax_t,   TYPE_QUALIFIER_NONE);
	type_uintmax_t_ptr  = make_pointer_type(type_uintmax_t,  TYPE_QUALIFIER_NONE);
	type_ptrdiff_t_ptr  = make_pointer_type(type_ptrdiff_t,  TYPE_QUALIFIER_NONE);
	type_uptrdiff_t_ptr = make_pointer_type(type_uptrdiff_t, TYPE_QUALIFIER_NONE);
	type_ssize_t_ptr    = make_pointer_type(type_ssize_t,    TYPE_QUALIFIER_NONE);
	type_size_t_ptr     = make_pointer_type(type_size_t,     TYPE_QUALIFIER_NONE);

	atomic_type_kind_t akind
		= dialect.cpp ? ATOMIC_TYPE_WCHAR_T : dialect.wchar_atomic_kind;
	type_wchar_t       = make_atomic_type(akind, TYPE_QUALIFIER_NONE);
	type_const_wchar_t = make_atomic_type(akind, TYPE_QUALIFIER_CONST);
	type_wchar_t_ptr   = make_pointer_type(type_wchar_t, TYPE_QUALIFIER_NONE);
	type_const_wchar_t_ptr
		= make_pointer_type(type_const_wchar_t, TYPE_QUALIFIER_NONE);

	atomic_type_kind_t const u2 = find_unsigned_int_atomic_type_kind_for_size(2);
	type_char16_t           = make_atomic_type(u2, TYPE_QUALIFIER_NONE);
	type_char16_t_const     = make_atomic_type(u2, TYPE_QUALIFIER_CONST);
	type_char16_t_ptr       = make_pointer_type(type_char16_t,       TYPE_QUALIFIER_NONE);
	type_char16_t_const_ptr = make_pointer_type(type_char16_t_const, TYPE_QUALIFIER_NONE);

	atomic_type_kind_t const u4 = find_unsigned_int_atomic_type_kind_for_size(4);
	type_char32_t           = make_atomic_type(u4, TYPE_QUALIFIER_NONE);
	type_char32_t_const     = make_atomic_type(u4, TYPE_QUALIFIER_CONST);
	type_char32_t_ptr       = make_pointer_type(type_char32_t,       TYPE_QUALIFIER_NONE);
	type_char32_t_const_ptr = make_pointer_type(type_char32_t_const, TYPE_QUALIFIER_NONE);

	if (dialect.ms)
		init_ms_types();
}
