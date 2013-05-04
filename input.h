/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef INPUT_H
#define INPUT_H

#include <stdio.h>
#include "unicode.h"

typedef struct input_t input_t;
typedef size_t (input_decoder_t)(input_t *input, utf32 *buffer, size_t buffer_size);

input_decoder_t *input_get_decoder(const char *encoding);

input_t *input_from_stream(FILE *stream, input_decoder_t *decoder);
input_t *input_from_string(const char *string, input_decoder_t *decoder);

input_decoder_t input_decode_utf8;

/** return underlying FILE* of an input if available, else NULL */
FILE *input_get_file(const input_t *input);

/** Type for a function being called on an input (or encoding) errors. */
typedef void (*input_error_callback_func)(unsigned delta_lines,
                                          unsigned delta_cols,
                                          const char *message);

void set_input_error_callback(input_error_callback_func func);

size_t decode(input_t *input, utf32 *buffer, size_t buffer_size);

void input_free(input_t *input);

#endif
