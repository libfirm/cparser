#include "panic.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

void (panic)(char const *const file, int const line, char const *const func, char const *const msg, ...)
{
	FILE *const out = stderr;
	fprintf(out, "%s:%d: panic in %s: ", file, line, func);
	va_list ap;
	va_start(ap, msg);
	vfprintf(out, msg, ap);
	va_end(ap);
	fputc('\n', out);
	abort();
}
