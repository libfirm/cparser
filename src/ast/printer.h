/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */

/**
 * @file
 * @brief  Abstracts away printing strings to a stream so we can reuse our
 *         printing routines for printing to files or into memory
 */
#ifndef PRINTER_H
#define PRINTER_H

#include <stdarg.h>
#include <stdio.h>

#include "adt/obst.h"

/** print a string into current output */
extern void (*print_string)(const char *str);
extern void (*print_vformat)(const char *format, va_list ap);
extern void (*print_char)(const char c);

/** print a printf style format string to current output */
static inline void __attribute__((format(printf,1,2)))
print_format(const char *format, ...)
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
