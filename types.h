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
extern type_t *type_unsigned_int;
extern type_t *type_unsigned_long_long;
extern type_t *type_unsigned_long;
extern type_t *type_void;

extern type_t *type_char_ptr;
extern type_t *type_int_ptr;
extern type_t *type_long_long_ptr;
extern type_t *type_long_ptr;
extern type_t *type_short_ptr;
extern type_t *type_signed_char_ptr;
extern type_t *type_void_ptr;

extern type_t *type_char_ptr_ptr;

extern type_t *type_intmax_t;
extern type_t *type_ptrdiff_t;
extern type_t *type_size_t;
extern type_t *type_ssize_t;
extern type_t *type_uintmax_t;
extern type_t *type_uptrdiff_t;
extern type_t *type_wchar_t;
extern type_t *type_wchar_ptr_t;
extern type_t *type_wint_t;

extern type_t *type_intmax_t_ptr;
extern type_t *type_ptrdiff_t_ptr;
extern type_t *type_ssize_t_ptr;
extern type_t *type_wchar_t_ptr;

void init_basic_types(void);

#endif
