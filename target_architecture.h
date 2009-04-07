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
#ifndef TARGET_ARCHITECTURE_H
#define TARGET_ARCHITECTURE_H

#include <limits.h>

#define TARGET_INT_MIN     INT_MIN
#define TARGET_INT_MAX     INT_MAX
#define TARGET_UINT_MAX    UINT_MAX

#define TARGET_LONG_MIN    LONG_MIN
#define TARGET_LONG_MAX    LONG_MAX
#define TARGET_ULONG_MAX   ULONG_MAX

#define TARGET_LONGLONG_MIN   LLONG_MIN
#define TARGET_LONGLONG_MAX   LLONG_MAX
#define TARGET_ULONGLONG_MAX  ULLONG_MAX

#endif
