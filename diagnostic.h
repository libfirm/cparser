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
#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include "token_t.h"

void diagnosticf(const char *fmt, ...);
void errorf(source_position_t pos, const char *fmt, ...);
void warningf(source_position_t pos, const char *fmt, ...);

extern unsigned diagnostic_count;
extern unsigned error_count;
extern unsigned warning_count;

/* true if warnings should be inhibited */
extern bool inhibit_all_warnings;

#endif
