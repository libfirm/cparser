#include "adt/error.h"
#include "string_rep.h"

static inline size_t wstrlen(const string_t *string)
{
	size_t      result = 0;
	const char *p      = string->begin;
	const char *end    = p + string->size;
	while (p < end) {
		read_utf8_char(&p);
		++result;
	}
	return result;
}

size_t get_string_len(string_t const *const str)
{
	switch (str->encoding) {
	case STRING_ENCODING_CHAR: return str->size;
	case STRING_ENCODING_WIDE: return wstrlen(str);
	}
	panic("invalid string encoding");
}
