/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "adt/util.h"
#include "printer.h"

#include <stdio.h>
#include <stdarg.h>

static FILE* out;

static void print_char_file(const char c)
{
	fputc(c, out);
}

static void print_string_file(const char *str)
{
	fputs(str, out);
}

static void print_vformat_file(const char *format, va_list ap)
{
	vfprintf(out, format, ap);
}

void print_to_file(FILE *new_out)
{
	out = new_out;
	print_string  = print_string_file;
	print_vformat = print_vformat_file;
	print_char    = print_char_file;
}



static struct obstack *obst;

static void print_char_obstack(const char c)
{
	obstack_1grow(obst, c);
}

static void print_string_obstack(const char *str)
{
	size_t len = strlen(str);
	obstack_grow(obst, str, len);
}

static void print_vformat_obstack(const char *format, va_list ap)
{
	obstack_vprintf(obst, format, ap);
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

static void print_char_buffer(const char c)
{
	if (buffer_pos == buffer_end)
		return;
	*buffer_pos++ = c;
}

static void print_string_buffer(const char *str)
{
	for (const char *c = str; *c != '\0'; ++c) {
		print_char_buffer(*c);
	}
}

static void print_vformat_buffer(const char *format, va_list ap)
{
	size_t size    = buffer_end - buffer_pos;
	size_t written = (size_t) vsnprintf(buffer_pos, size, format, ap);
	buffer_pos    += MIN(written, size);
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
void (*print_char)(const char c) = print_char_file;

void printer_push(void)
{
	/* TODO */
}

void printer_pop(void)
{
	/* TODO */
}
