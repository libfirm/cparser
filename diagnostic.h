#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include "token_t.h"


void parser_print_prefix_pos(source_position_t);
void parser_print_warning_prefix_pos(source_position_t);
void parse_warning_pos(source_position_t, const char *message);
void parse_warning_posf(source_position_t, const char *fmt, ...);

#endif
