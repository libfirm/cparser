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

/**
 * @file
 * @brief  Abstracts away printing strings to a stream so we can reuse our
 *         printing routines for printing to files or into memory
 */
#ifndef PRINTER_H
#define PRINTER_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "adt/obst.h"
#include "string_rep.h"

/** print a string into current output */
extern void (*print_string)(const char *str);
extern void (*print_vformat)(const char *format, va_list ap);
/** print a single unicode character to current output (encoded as UTF-8) */
extern void (*print_char)(wchar_rep_t c);

/** print a printf style format string to current output */
static inline void __attribute__((format(printf,1,2))) print_format(const char *format, ...)
{
	va_list ap;
	va_start(ap, format);
	print_vformat(format, ap);
	va_end(ap);
}

/** Set current output to be a FILE* stream */
void print_to_file(FILE *out);

/** Set current output to an obstack (grows an object on the obstack) */
void print_to_obstack(struct obstack *obst);

/** Set current output to be a buffer with limited size */
void print_to_buffer(char *buffer, size_t buffer_size);

/** Assures that the string in the buffer is 0 terminated */
void finish_print_to_buffer(void);

/** push current printer output to the (printer output) stack */
void printer_push(void);

/** pop a printer output from the stack */
void printer_pop(void);

#endif
