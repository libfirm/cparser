/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include <config.h>

#include "type_t.h"
#include "types.h"
#include "lang_features.h"

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

type_t *type_char_ptr;
type_t *type_char_ptr_restrict;
type_t *type_const_char_ptr;
type_t *type_const_char_ptr_restrict;
type_t *type_int_ptr;
type_t *type_long_long_ptr;
type_t *type_long_ptr;
type_t *type_unsigned_long_ptr;
type_t *type_short_ptr;
type_t *type_signed_char_ptr;
type_t *type_void_ptr;
type_t *type_const_void;
type_t *type_const_void_ptr;
type_t *type_void_ptr_restrict;
type_t *type_const_void_ptr_restrict;

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
type_t *type_ptrdiff_t_ptr;
type_t *type_ssize_t_ptr;
type_t *type_wchar_t_ptr;
type_t *type_const_wchar_t_ptr;

type_t *type_valist;

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
type_t *type_int64_ptr;


void init_basic_types(void)
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

	int8_type_kind  = find_signed_int_atomic_type_kind_for_size(1);
	int16_type_kind = find_signed_int_atomic_type_kind_for_size(2);
	int32_type_kind = find_signed_int_atomic_type_kind_for_size(4);
	int64_type_kind = find_signed_int_atomic_type_kind_for_size(8);

	type_int32_t = make_atomic_type(int32_type_kind, TYPE_QUALIFIER_NONE);
	type_int64_t = make_atomic_type(int64_type_kind, TYPE_QUALIFIER_NONE);

	/* microsoft types */
	if (c_mode & _MS) {
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
		type_int64_ptr           = make_pointer_type(type_int64,              TYPE_QUALIFIER_NONE);
	}

	/* pointer types */
	type_void_ptr           = make_pointer_type(type_void,              TYPE_QUALIFIER_NONE);
	type_const_void_ptr     = make_pointer_type(type_const_void,        TYPE_QUALIFIER_NONE);
	type_void_ptr_restrict  = make_pointer_type(type_void,              TYPE_QUALIFIER_RESTRICT);
	type_const_void_ptr_restrict
	                        = make_pointer_type(type_const_void,        TYPE_QUALIFIER_RESTRICT);
	type_char_ptr           = make_pointer_type(type_char,              TYPE_QUALIFIER_NONE);
	type_char_ptr_restrict  = make_pointer_type(type_char,              TYPE_QUALIFIER_RESTRICT);
	type_signed_char_ptr    = make_pointer_type(type_signed_char,       TYPE_QUALIFIER_NONE);
	type_short_ptr          = make_pointer_type(type_short,             TYPE_QUALIFIER_NONE);
	type_int_ptr            = make_pointer_type(type_int,               TYPE_QUALIFIER_NONE);
	type_long_ptr           = make_pointer_type(type_long,              TYPE_QUALIFIER_NONE);
	type_unsigned_long_ptr  = make_pointer_type(type_unsigned_long,     TYPE_QUALIFIER_NONE);
	type_long_long_ptr      = make_pointer_type(type_long_long,         TYPE_QUALIFIER_NONE);

	type_char_ptr_ptr       = make_pointer_type(type_char_ptr,          TYPE_QUALIFIER_NONE);
	type_valist             = type_void_ptr;

	/* const character types */
	type_const_char         = make_atomic_type(ATOMIC_TYPE_CHAR,        TYPE_QUALIFIER_CONST);
	type_const_char_ptr     = make_pointer_type(type_const_char,        TYPE_QUALIFIER_NONE);
	type_const_char_ptr_restrict = make_pointer_type(type_const_char,        TYPE_QUALIFIER_RESTRICT);

	/* other types */
	type_intmax_t    = type_long_long;
	type_size_t      = type_unsigned_long;
	type_ssize_t     = type_long;
	type_ptrdiff_t   = type_long;
	type_uintmax_t   = type_unsigned_long_long;
	type_uptrdiff_t  = type_unsigned_long;
	type_wint_t      = type_unsigned_int;

	type_intmax_t_ptr  = make_pointer_type(type_intmax_t,  TYPE_QUALIFIER_NONE);
	type_ptrdiff_t_ptr = make_pointer_type(type_ptrdiff_t, TYPE_QUALIFIER_NONE);
	type_ssize_t_ptr   = make_pointer_type(type_ssize_t,   TYPE_QUALIFIER_NONE);
}

void init_wchar_types(atomic_type_kind_t akind)
{
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
}
