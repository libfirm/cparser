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
#ifndef LANG_FEATURES_H
#define LANG_FEATURES_H

typedef enum lang_features_t {
	_C89  = 1U << 0,
	_ANSI = 1U << 1,
	_C99  = 1U << 2,
	_CXX  = 1U << 3,
	_GNUC = 1U << 4,
	_MS   = 1U << 5,
	_ALL  = 0xFF
} lang_features_t;

/* the current C mode/dialect */
extern lang_features_t c_mode;

/* the 'machine size', 16, 32 or 64 bit */
extern unsigned int machine_size;

/* true if the char type is signed */
extern bool char_is_signed;

/* true for strict language checking. */
extern bool strict_mode;

/* true if wchar_t is equal to unsigned short. */
extern bool opt_short_wchar_t;

#endif
