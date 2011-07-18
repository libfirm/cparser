#ifndef INPUT_H
#define INPUT_H

#include <stdio.h>
#include "unicode.h"

typedef struct input_t input_t;

input_t *input_from_stream(FILE *stream, const char *encoding);

/** Type for a function being called on an input (or encoding) errors. */
typedef void (*input_error_callback_func)(unsigned delta_lines,
                                          unsigned delta_cols,
                                          const char *message);

void set_input_error_callback(input_error_callback_func func);

size_t decode(input_t *input, utf32 *buffer, size_t buffer_size);

void input_free(input_t *input);

#endif
