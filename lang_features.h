/*
 * This file is part of cparser.
 * Copyright (C) 2007-2009 Matthias Braun <matze@braunis.de>
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
#ifndef LANG_FEATURES_H
#define LANG_FEATURES_H

#include "type.h"

#define BITS_PER_BYTE    8

typedef enum lang_features_t {
	_C89  = 1U << 0,
	_C99  = 1U << 1,
	_C11  = 1U << 2,
	_CXX  = 1U << 3,
	_GNUC = 1U << 4,
	_MS   = 1U << 5,
	_ALL  = 0xFF
} lang_features_t;

/** the current C mode/dialect */
extern unsigned int c_mode;

/**
 * whether architecture shift instructions usually perform modulo bit_size
 * on the shift amount, if yes this equals to the machine_size.
 */
extern unsigned int architecture_modulo_shift;

/** byte-order: true = big-endian, false = little-endian */
extern bool byte_order_big_endian;

/** true for strict language checking. */
extern bool strict_mode;

/** a hack that adds a call to __main into the main function, necessary on
 * mingw */
extern bool enable_main_collect2_hack;

extern bool freestanding;

#endif
