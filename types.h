/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef TYPES_H
#define TYPES_H

#include "type.h"

extern type_t *type_error_type;

extern type_t *type_bool;
extern type_t *type_char;
extern type_t *type_const_char;
extern type_t *type_double;
extern type_t *type_float;
extern type_t *type_int;
extern type_t *type_long_double;
extern type_t *type_long_long;
extern type_t *type_long;
extern type_t *type_short;
extern type_t *type_unsigned_short;
extern type_t *type_signed_char;
extern type_t *type_unsigned_char;
extern type_t *type_unsigned_int;
extern type_t *type_unsigned_long_long;
extern type_t *type_unsigned_long;
extern type_t *type_void;

extern type_t *type_char_ptr;
extern type_t *type_char_ptr_restrict;
extern type_t *type_const_char_ptr;
extern type_t *type_const_char_ptr_restrict;
extern type_t *type_int_ptr;
extern type_t *type_long_long_ptr;
extern type_t *type_long_ptr;
extern type_t *type_unsigned_long_ptr;
extern type_t *type_short_ptr;
extern type_t *type_signed_char_ptr;
extern type_t *type_void_ptr;
extern type_t *type_const_void;
extern type_t *type_const_void_ptr;
extern type_t *type_void_ptr_restrict;
extern type_t *type_const_void_ptr_restrict;

extern type_t *type_char_ptr_ptr;

extern type_t *type_char16_t;
extern type_t *type_char32_t;
extern type_t *type_char16_t_const;
extern type_t *type_char32_t_const;
extern type_t *type_intmax_t;
extern type_t *type_ptrdiff_t;
extern type_t *type_size_t;
extern type_t *type_ssize_t;
extern type_t *type_uintmax_t;
extern type_t *type_uptrdiff_t;
extern type_t *type_wchar_t;
extern type_t *type_const_wchar_t;
extern type_t *type_wchar_ptr_t;
extern type_t *type_wint_t;
extern type_t *type_int32_t;
extern type_t *type_int64_t;

extern type_t *type_char16_t_ptr;
extern type_t *type_char32_t_ptr;
extern type_t *type_char16_t_const_ptr;
extern type_t *type_char32_t_const_ptr;
extern type_t *type_intmax_t_ptr;
extern type_t *type_ptrdiff_t_ptr;
extern type_t *type_ssize_t_ptr;
extern type_t *type_wchar_t_ptr;
extern type_t *type_const_wchar_t_ptr;

extern type_t *type_valist;

/* microsoft types */
extern atomic_type_kind_t int8_type_kind;
extern atomic_type_kind_t int16_type_kind;
extern atomic_type_kind_t int32_type_kind;
extern atomic_type_kind_t int64_type_kind;
extern atomic_type_kind_t unsigned_int8_type_kind;
extern atomic_type_kind_t unsigned_int16_type_kind;
extern atomic_type_kind_t unsigned_int32_type_kind;
extern atomic_type_kind_t unsigned_int64_type_kind;

extern type_t *type_int8;
extern type_t *type_int16;
extern type_t *type_int32;
extern type_t *type_int64;
extern type_t *type_int64_ptr;
extern type_t *type_unsigned_int8;
extern type_t *type_unsigned_int16;
extern type_t *type_unsigned_int32;
extern type_t *type_unsigned_int64;

void init_basic_types(void);
void init_wchar_types(atomic_type_kind_t wchar_akind);

#endif
