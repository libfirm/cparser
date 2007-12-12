#ifndef TYPES_H
#define TYPES_H

#include "type.h"

extern type_t *type_error_type;

extern type_t *type_char;
extern type_t *type_double;
extern type_t *type_float;
extern type_t *type_int;
extern type_t *type_long_double;
extern type_t *type_long_long;
extern type_t *type_long;
extern type_t *type_short;
extern type_t *type_signed_char;
extern type_t *type_string;
extern type_t *type_unsigned_int;
extern type_t *type_unsigned_long_long;
extern type_t *type_unsigned_long;
extern type_t *type_void;

extern type_t *type_int_ptr;
extern type_t *type_long_long_ptr;
extern type_t *type_long_ptr;
extern type_t *type_short_ptr;
extern type_t *type_signed_char_ptr;
extern type_t *type_void_ptr;

extern type_t *type_intmax_t;
extern type_t *type_ptrdiff_t;
extern type_t *type_size_t;
extern type_t *type_ssize_t;
extern type_t *type_uintmax_t;
extern type_t *type_uptrdiff_t;
extern type_t *type_wchar_t;
extern type_t *type_wint_t;

extern type_t *type_intmax_t_ptr;
extern type_t *type_ptrdiff_t_ptr;
extern type_t *type_ssize_t_ptr;
extern type_t *type_wchar_t_ptr;

void init_basic_types(void);

#define is_type_valid(type) ((type)->kind != TYPE_ERROR)

#endif
