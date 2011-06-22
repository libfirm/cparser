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
#include <stdarg.h>
#include <stdio.h>

#include "diagnostic.h"
#include "adt/error.h"
#include "entity_t.h"
#include "symbol_t.h"
#include "token_t.h"
#include "ast.h"
#include "type.h"
#include "warning.h"

/** Number of occurred diagnostics. */
unsigned diagnostic_count        = 0;
/** Number of occurred errors. */
unsigned error_count             = 0;
/** Number of occurred warnings. */
unsigned warning_count           = 0;
bool     show_column             = true;
bool     diagnostics_show_option = true;

static const source_position_t *curr_pos = NULL;

/**
 * prints an additional source position
 */
static void print_source_position(FILE *out, const source_position_t *pos)
{
	fprintf(out, "at line %u", pos->lineno);
	if (show_column)
		fprintf(out, ":%u", pos->colno);
	if (curr_pos == NULL || curr_pos->input_name != pos->input_name)
		fprintf(out, " of \"%s\"", pos->input_name);
}

/**
 * Issue a diagnostic message.
 */
static void diagnosticvf(const char *const fmt, va_list ap)
{
	for (const char* f = fmt; *f != '\0'; ++f) {
		if (*f == '%') {
			++f;

			bool extended = false;
			if (*f == '#') {
				extended = true;
				++f;
			}

			switch (*f) {
				case '%':
					fputc(*f, stderr);
					break;

				case 'c': {
					const unsigned char val = (unsigned char) va_arg(ap, int);
					fputc(val, stderr);
					break;
				}

				case 'd': {
					const int val = va_arg(ap, int);
					fprintf(stderr, "%d", val);
					break;
				}

				case 's': {
					const char* const str = va_arg(ap, const char*);
					fputs(str, stderr);
					break;
				}

				case 'S': {
					const string_t *str = va_arg(ap, const string_t*);
					for (size_t i = 0; i < str->size; ++i) {
						fputc(str->begin[i], stderr);
					}
					break;
				}

				case 'u': {
					const unsigned int val = va_arg(ap, unsigned int);
					fprintf(stderr, "%u", val);
					break;
				}

				case 'Y': {
					const symbol_t *const symbol = va_arg(ap, const symbol_t*);
					if (symbol == NULL)
						fputs("(null)", stderr);
					else
						fputs(symbol->string, stderr);
					break;
				}

				case 'E': {
					const expression_t* const expr = va_arg(ap, const expression_t*);
					print_expression(expr);
					break;
				}

				case 'Q': {
					const unsigned qualifiers = va_arg(ap, unsigned);
					print_type_qualifiers(qualifiers, QUAL_SEP_NONE);
					break;
				}

				case 'T': {
					const type_t* const type = va_arg(ap, const type_t*);
					const symbol_t*     sym  = NULL;
					if (extended) {
						sym = va_arg(ap, const symbol_t*);
					}
					print_type_ext(type, sym, NULL);
					break;
				}

				case 't': {
					const token_t *const token = va_arg(ap, const token_t*);
					print_pp_token(stderr, token);
					break;
				}

				case 'K': {
					const token_t* const token = va_arg(ap, const token_t*);
					print_token(stderr, token);
					break;
				}

				case 'k': {
					if (extended) {
						bool              first     = true;
						va_list*          toks      = va_arg(ap, va_list*);
						const char* const delimiter = va_arg(ap, const char*);
						for (;;) {
							const token_type_t tok = va_arg(*toks, token_type_t);
							if (tok == 0)
								break;
							if (first) {
								first = false;
							} else {
								fputs(delimiter, stderr);
							}
							print_token_type(stderr, tok);
						}
					} else {
						const token_type_t token = va_arg(ap, token_type_t);
						print_token_type(stderr, token);
					}
					break;
				}

				case 'N': {
					entity_t const *const ent = va_arg(ap, entity_t const*);
					if (extended && is_declaration(ent)) {
						print_type_ext(ent->declaration.type, ent->base.symbol, NULL);
					} else {
						char     const *const kind = get_entity_kind_name(ent->kind);
						symbol_t const *const sym  = ent->base.symbol;
						if (sym) {
							fprintf(stderr, "%s %s", kind, sym->string);
						} else {
							fprintf(stderr, "anonymous %s", kind);
						}
					}
					break;
				}

				case 'P': {
					const source_position_t *pos = va_arg(ap, const source_position_t *);
					print_source_position(stderr, pos);
					break;
				}

				default:
					panic("unknown format specifier");
			}
		} else {
			fputc(*f, stderr);
		}
	}
}

void diagnosticf(const char *const fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	++diagnostic_count;
	curr_pos = NULL;
	diagnosticvf(fmt, ap);
	va_end(ap);
}

static void diagnosticposvf(source_position_t const *const pos, char const *const kind, char const *const fmt, va_list ap)
{
	FILE *const out = stderr;
	fprintf(out, "%s:%u:", pos->input_name, pos->lineno);
	if (show_column)
		fprintf(out, "%u:", pos->colno);
	fprintf(out, " %s: ", kind);
	curr_pos = pos;
	diagnosticvf(fmt, ap);
}

static void errorvf(const source_position_t *pos,
                    const char *const fmt, va_list ap)
{
	curr_pos = pos;
	++error_count;
	diagnosticposvf(pos, "error", fmt, ap);
	fputc('\n', stderr);
	if (is_warn_on(WARN_FATAL_ERRORS))
		exit(EXIT_FAILURE);
}

void errorf(const source_position_t *pos, const char *const fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	errorvf(pos, fmt, ap);
	va_end(ap);
}

void warningf(warning_t const warn, source_position_t const* pos, char const *const fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	warning_switch_t const *const s = get_warn_switch(warn);
	switch (s->state) {
			char const* kind;
		case WARN_STATE_ON:
			if (is_warn_on(WARN_ERROR)) {
		case WARN_STATE_ON | WARN_STATE_ERROR:
				++error_count;
				kind = "error";
			} else {
		case WARN_STATE_ON | WARN_STATE_NO_ERROR:
				++warning_count;
				kind = "warning";
			}
			diagnosticposvf(pos, kind, fmt, ap);
			if (diagnostics_show_option)
				fprintf(stderr, " [-W%s]\n", s->name);
			break;

		default:
			break;
	}
	va_end(ap);
}

static void internal_errorvf(const source_position_t *pos,
                    const char *const fmt, va_list ap)
{
	diagnosticposvf(pos, "internal error", fmt, ap);
	fputc('\n', stderr);
}

void internal_errorf(const source_position_t *pos, const char *const fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	curr_pos = pos;
	internal_errorvf(pos, fmt, ap);
	va_end(ap);
	abort();
}
