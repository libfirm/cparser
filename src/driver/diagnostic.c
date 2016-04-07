/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "diagnostic.h"

#include <stdarg.h>
#include <stdio.h>

#include "adt/panic.h"
#include "adt/separator_t.h"
#include "adt/strutil.h"
#include "adt/unicode.h"
#include "ast/ast.h"
#include "ast/entity_t.h"
#include "ast/symbol_t.h"
#include "ast/type.h"

typedef struct colorscheme_t {
	const char *fatal;
	const char *error;
	const char *warning;
	const char *note;
	const char *highlight;
	const char *reset_highlight;
	const char *reset_all;
} colorscheme_t;

static const colorscheme_t no_colors = { "", "", "", "", "", "", "" };
static const colorscheme_t colors_8  = {
	.fatal           = "\033[31m",
	.error           = "\033[31m",
	.warning         = "\033[33m",
	.note            = "\033[30m",
	.highlight       = "\033[1m",
	.reset_highlight = "\033[22m",
	.reset_all       = "\033[0m",
};
static const colorscheme_t colors_256 = {
	.fatal           = "\033[38;5;124m",
	.error           = "\033[38;5;160m",
	.warning         = "\033[38;5;139m",
	.note            = "\033[38;5;008m",
	.highlight       = "\033[1m",
	.reset_highlight = "\033[22m",
	.reset_all       = "\033[0m",
};
static colorscheme_t colors = { "", "", "", "", "", "", "" };

/** Number of occurred errors. */
unsigned        error_count   = 0;
/** Number of occurred warnings. */
static unsigned warning_count = 0;

bool     show_column             = true;
bool     diagnostics_show_option = true;

static void fpututf32(utf32 const c, FILE *const out)
{
	if (c < 0x80U) {
		fputc(c, out);
	} else if (c < 0x800) {
		fputc(0xC0 | (c >> 6), out);
		fputc(0x80 | (c & 0x3F), out);
	} else if (c < 0x10000) {
		fputc(0xE0 | ( c >> 12), out);
		fputc(0x80 | ((c >>  6) & 0x3F), out);
		fputc(0x80 | ( c        & 0x3F), out);
	} else {
		fputc(0xF0 | ( c >> 18), out);
		fputc(0x80 | ((c >> 12) & 0x3F), out);
		fputc(0x80 | ((c >>  6) & 0x3F), out);
		fputc(0x80 | ( c        & 0x3F), out);
	}
}

/**
 * Issue a diagnostic message.
 */
static void diagnosticvf(position_t const *const pos,
                         char const *const kind_color, char const *const kind,
                         char const *fmt, va_list ap)
{
	FILE *const out = stderr;

	fputs(colors.highlight, out);
	if (pos) {
		if (pos->colno != 0 && show_column) {
			fprintf(out, "%s:%u:%u: ", pos->input_name, pos->lineno, (unsigned)pos->colno);
		} else if (pos->lineno != 0) {
			fprintf(out, "%s:%u: ", pos->input_name, pos->lineno);
		} else {
			fprintf(out, "%s: ", pos->input_name);
		}
	}

	fprintf(out, "%s%s:%s ", kind_color, kind, colors.reset_all);

	for (char const *f; (f = strchr(fmt, '%')); fmt = f) {
		fwrite(fmt, sizeof(*fmt), f - fmt, out); // Print till '%'.
		++f; // Skip '%'.

		bool extended  = false;
		bool flag_zero = false;
		bool flag_long = false;
		bool flag_high = false;
		for (;; ++f) {
			switch (*f) {
			case '#': extended  = true; break;
			case '0': flag_zero = true; break;
			case 'l': flag_long = true; break;
			case 'h': flag_high = true; break;
			default:  goto done_flags;
			}
		}
done_flags:;

		int field_width = 0;
		if (*f == '*') {
			++f;
			field_width = va_arg(ap, int);
		}

		int               precision = -1;
		char const *const rest      = strstart(f, ".*");
		if (rest) {
			f         = rest;
			precision = va_arg(ap, int);
		}

		/* Automatic highlight for some formats. */
		if (!flag_high)
			flag_high = strchr("EKNQTYk", *f);

		if (flag_high)
			fputs(colors.highlight, out);
		switch (*f++) {
		case '%':
			fputc('%', out);
			break;

		case 'c': {
			if (flag_long) {
				const utf32 val = va_arg(ap, utf32);
				fpututf32(val, out);
			} else {
				const unsigned char val = (unsigned char) va_arg(ap, int);
				fputc(val, out);
			}
			break;
		}

		case 'd': {
			if (flag_long) {
				const long val = va_arg(ap, long);
				fprintf(out, "%ld", val);
			} else {
				const int val = va_arg(ap, int);
				fprintf(out, "%d", val);
			}
			break;
		}

		case 's': {
			const char* const str = va_arg(ap, const char*);
			fprintf(out, "%.*s", precision, str);
			break;
		}

		case 'S': {
			const string_t *str = va_arg(ap, const string_t*);
			fwrite(str->begin, 1, str->size, out);
			break;
		}

		case 'u': {
			const unsigned int val = va_arg(ap, unsigned int);
			fprintf(out, "%u", val);
			break;
		}

		case 'X': {
			unsigned int const val  = va_arg(ap, unsigned int);
			char  const *const xfmt = flag_zero ? "%0*X" : "%*X";
			fprintf(out, xfmt, field_width, val);
			break;
		}

		case 'Y': {
			const symbol_t *const symbol = va_arg(ap, const symbol_t*);
			fputs(symbol ? symbol->string : "(null)", out);
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
			print_type_ext(type, NULL, NULL);
			break;
		}

		case 'K': {
			const token_t* const token = va_arg(ap, const token_t*);
			print_token(out, token, extended);
			break;
		}

		case 'k': {
			if (flag_long) {
				va_list* const toks = va_arg(ap, va_list*);
				separator_t    sep  = { "", ", " };
				for (;;) {
					const token_kind_t tok = (token_kind_t)va_arg(*toks, int);
					if (tok == 0)
						break;
					fputs(sep_next(&sep), out);
					fputc('\'', out);
					print_token_kind(out, tok);
					fputc('\'', out);
				}
			} else {
				const token_kind_t token = (token_kind_t)va_arg(ap, int);
				fputc('\'', out);
				print_token_kind(out, token);
				fputc('\'', out);
			}
			break;
		}

		case 'N': {
			entity_t const *const ent = va_arg(ap, entity_t const*);
			if (extended && is_declaration(ent)) {
				print_type_ext(ent->declaration.type, ent->base.symbol, NULL);
			} else {
				char     const *const ent_kind = get_entity_kind_name(ent->kind);
				symbol_t const *const sym      = ent->base.symbol;
				if (sym) {
					fprintf(out, "%s '%s'", ent_kind, sym->string);
				} else {
					fprintf(out, "anonymous '%s'", ent_kind);
				}
			}
			break;
		}

		default:
			panic("unknown format specifier");
		}
		if (flag_high)
			fputs(colors.reset_highlight, out);
	}
	fputs(fmt, out); // Print rest.
}

static void errorvf(const position_t *pos,
                    const char *const fmt, va_list ap)
{
	++error_count;
	diagnosticvf(pos, colors.error, "error", fmt, ap);
	fputc('\n', stderr);
	if (is_warn_on(WARN_FATAL_ERRORS))
		exit(EXIT_FAILURE);
}

void errorf(const position_t *pos, const char *const fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	errorvf(pos, fmt, ap);
	va_end(ap);
}

void notef(position_t const *const pos, char const *const fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	diagnosticvf(pos, colors.note, "note", fmt, ap);
	fputc('\n', stderr);
	va_end(ap);
}

bool warningf(warning_t const warn, position_t const* pos, char const *const fmt, ...)
{
	if (pos != NULL && pos->is_system_header && !is_warn_on(WARN_SYSTEM_HEADERS))
		return false;

	warning_switch_t const *const s = get_warn_switch(warn);
	switch ((unsigned) s->state) {
		char const* kind;
		char const* kind_color;
		case WARN_STATE_ON:
			if (is_warn_on(WARN_ERROR)) {
		case WARN_STATE_ON | WARN_STATE_ERROR:
				++error_count;
				kind       = "error";
				kind_color = colors.error;
			} else {
		case WARN_STATE_ON | WARN_STATE_NO_ERROR:
				++warning_count;
				kind       = "warning";
				kind_color = colors.warning;
			}
			va_list ap;
			va_start(ap, fmt);
			diagnosticvf(pos, kind_color, kind, fmt, ap);
			va_end(ap);
			if (diagnostics_show_option) {
				char const *const err = s->state & WARN_STATE_ERROR ? "error=" : "";
				fprintf(stderr, " [-W%s%s]", err, s->name);
			}
			fputc('\n', stderr);
			return true;

		default:
			return false;
	}
}

void diagnostic_enable_color(int n_cols)
{
	switch (n_cols) {
	case 8:   colors = colors_8;   break;
	case 256: colors = colors_256; break;
	default:  colors = no_colors;  break;
	}
}

void print_diagnostic_summary(void)
{
	if (error_count > 0) {
		fprintf(stderr, "%u error(s), %u warning(s)\n", error_count, warning_count);
	} else if (warning_count > 0) {
		fprintf(stderr, "%u warning(s)\n", warning_count);
	}
}
