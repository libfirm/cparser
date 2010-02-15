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
#include "config.h"

#include "printer.h"

#include <stdio.h>
#include <stdarg.h>

static FILE* out;

static void print_string_file(const char *str)
{
	fputs(str, out);
}

static void print_vformat_file(const char *format, va_list ap)
{
	vfprintf(out, format, ap);
}

static void print_char_file(wchar_rep_t c)
{
	const unsigned tc = (unsigned) c;
	if (tc < 0x80) {
		fputc(tc, out);
	} else if (tc < 0x800) {
		fputc(0xC0 | (tc >> 6),   out);
		fputc(0x80 | (tc & 0x3F), out);
	} else if (tc < 0x10000) {
		fputc(0xE0 | ( tc >> 12),         out);
		fputc(0x80 | ((tc >>  6) & 0x3F), out);
		fputc(0x80 | ( tc        & 0x3F), out);
	} else {
		fputc(0xF0 | ( tc >> 18),         out);
		fputc(0x80 | ((tc >> 12) & 0x3F), out);
		fputc(0x80 | ((tc >>  6) & 0x3F), out);
		fputc(0x80 | ( tc        & 0x3F), out);
	}
}

void print_to_file(FILE *new_out)
{
	out = new_out;
	print_string  = print_string_file;
	print_vformat = print_vformat_file;
	print_char    = print_char_file;
}



static struct obstack *obst;

static void print_string_obstack(const char *str)
{
	size_t len = strlen(str);
	obstack_grow(obst, str, len);
}

static void print_vformat_obstack(const char *format, va_list ap)
{
	obstack_vprintf(obst, format, ap);
}

static void print_char_obstack(wchar_rep_t c)
{
	const unsigned tc = (unsigned) c;
	if (tc < 0x80) {
		obstack_1grow(obst, tc);
	} else if (tc < 0x800) {
		obstack_1grow(obst, 0xC0 | (tc >> 6));
		obstack_1grow(obst, 0x80 | (tc & 0x3F));
	} else if (tc < 0x10000) {
		obstack_1grow(obst, 0xE0 | ( tc >> 12));
		obstack_1grow(obst, 0x80 | ((tc >>  6) & 0x3F));
		obstack_1grow(obst, 0x80 | ( tc        & 0x3F));
	} else {
		obstack_1grow(obst, 0xF0 | ( tc >> 18));
		obstack_1grow(obst, 0x80 | ((tc >> 12) & 0x3F));
		obstack_1grow(obst, 0x80 | ((tc >>  6) & 0x3F));
		obstack_1grow(obst, 0x80 | ( tc        & 0x3F));
	}
}

void print_to_obstack(struct obstack *new_obst)
{
	obst = new_obst;
	print_string  = print_string_obstack;
	print_vformat = print_vformat_obstack;
	print_char    = print_char_obstack;
}



static char *buffer_pos;
static char *buffer_end;

static inline void buffer_add_char(int c)
{
	if (buffer_pos == buffer_end)
		return;
	*buffer_pos++ = c;
}

static void print_string_buffer(const char *str)
{
	for (const char *c = str; *c != '\0'; ++c) {
		buffer_add_char(*c);
	}
}

static void print_vformat_buffer(const char *format, va_list ap)
{
	size_t size    = buffer_end - buffer_pos;
	size_t written = (size_t) vsnprintf(buffer_pos, size, format, ap);
	buffer_pos    += written < size ? written : size;
}

static void print_char_buffer(wchar_rep_t c)
{
	const unsigned tc = (unsigned) c;
	if (tc < 0x80) {
		buffer_add_char(tc);
	} else if (tc < 0x800) {
		buffer_add_char(0xC0 | (tc >> 6));
		buffer_add_char(0x80 | (tc & 0x3F));
	} else if (tc < 0x10000) {
		buffer_add_char(0xE0 | ( tc >> 12));
		buffer_add_char(0x80 | ((tc >>  6) & 0x3F));
		buffer_add_char(0x80 | ( tc        & 0x3F));
	} else {
		buffer_add_char(0xF0 | ( tc >> 18));
		buffer_add_char(0x80 | ((tc >> 12) & 0x3F));
		buffer_add_char(0x80 | ((tc >>  6) & 0x3F));
		buffer_add_char(0x80 | ( tc        & 0x3F));
	}
}

void print_to_buffer(char *buffer, size_t buffer_size)
{
	buffer_pos = buffer;
	buffer_end = buffer + buffer_size - 2;

	print_string  = print_string_buffer;
	print_vformat = print_vformat_buffer;
	print_char    = print_char_buffer;
}

void finish_print_to_buffer(void)
{
	*buffer_pos = '\0';
	buffer_pos = NULL;
	buffer_end = NULL;
}


void (*print_string)(const char *str) = print_string_file;
void (*print_vformat)(const char *format, va_list ap) = print_vformat_file;
void (*print_char)(wchar_rep_t c) = print_char_file;

void printer_push(void)
{
	/* TODO */
}

void printer_pop(void)
{
	/* TODO */
}
