#include <stdarg.h>
#include <stdio.h>

#include "diagnostic.h"


void parser_print_prefix_pos(const source_position_t pos)
{
	fprintf(stderr, "%s:%u: ", pos.input_name, pos.linenr);
}

void parser_print_warning_prefix_pos(const source_position_t pos)
{
	parser_print_prefix_pos(pos);
	fputs("warning: ", stderr);
}

void parse_warning_pos(const source_position_t pos, const char *const message)
{
	parser_print_prefix_pos(pos);
	fprintf(stderr, "warning: %s\n", message);
}

void parse_warning_posf(const source_position_t pos, const char *const fmt, ...)
{
	parser_print_prefix_pos(pos);
	fputs("warning: ", stderr);
	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fputc('\n', stderr);
}
