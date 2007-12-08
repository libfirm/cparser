#include <stdarg.h>
#include <stdio.h>

#include "adt/error.h"
#include "ast.h"
#include "diagnostic.h"
#include "token_t.h"
#include "type.h"


//#define ABORT_ON_ERROR

/** Number of occurred diagnostics. */
unsigned diagnostic_count = 0;
/** Number of occurred errors. */
unsigned error_count      = 0;
/** Number of occurred warnings. */
unsigned warning_count    = 0;

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

				case 'C': {
					const wint_t val = va_arg(ap, wint_t);
					fputwc(val, stderr);
					break;
				}

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

				case 'Y': {
					const symbol_t *const symbol = va_arg(ap, const symbol_t*);
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
					print_type_qualifiers(qualifiers);
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

				case 'K': {
					const token_t* const token = va_arg(ap, const token_t*);
					print_token(stderr, token);
					break;
				}

				case 'k': {
					if (extended) {
						bool              first     = false;
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
	diagnosticvf(fmt, ap);
	va_end(ap);
}

void errorf(const source_position_t pos, const char *const fmt, ...)
{
	++error_count;
	fprintf(stderr, "%s:%u: error: ", pos.input_name, pos.linenr);
	va_list ap;
	va_start(ap, fmt);
	diagnosticvf(fmt, ap);
	va_end(ap);
	fputc('\n', stderr);

#ifdef ABORT_ON_ERROR
	abort();
#endif
}

void warningf(const source_position_t pos, const char *const fmt, ...)
{
	++warning_count;
	fprintf(stderr, "%s:%u: warning: ", pos.input_name, pos.linenr);
	va_list ap;
	va_start(ap, fmt);
	diagnosticvf(fmt, ap);
	va_end(ap);
	fputc('\n', stderr);
}
