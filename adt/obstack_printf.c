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
#include <config.h>

#include <stdarg.h>
#include <stdio.h>
#include "obstack.h"

int obstack_vprintf(struct obstack *obst, const char *fmt, va_list ap)
{
	/* currently a poor implementation ... */
  	char buf[1024];
	int len;

	len = vsnprintf(buf, sizeof(buf), fmt, ap);
	obstack_grow(obst, buf, len);

  	return len;
}

int obstack_printf(struct obstack *obst, const char *fmt, ...)
{
	va_list ap;
	int     len;

	va_start(ap, fmt);
	len = obstack_vprintf(obst, fmt, ap);
	va_end(ap);

	return len;
}
