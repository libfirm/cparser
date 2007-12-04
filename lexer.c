#include <config.h>

#include "lexer.h"
#include "token_t.h"
#include "symbol_table_t.h"
#include "adt/error.h"
#include "adt/strset.h"
#include "adt/util.h"
#include "type_t.h"
#include "target_architecture.h"
#include "parser.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

//#define DEBUG_CHARS
#define MAX_PUTBACK 3

#ifdef _WIN32
/* No strtold on windows and no replacement yet */
#define strtold(s, e) strtod(s, e)
#endif

#if defined HAS_SIGNED_CHAR
typedef signed char char_type;
#elif defined HAS_UNSIGNED_CHAR
typedef unsigned char char_type;
#else
#	error signedness of char not determined
#endif

static int         c;
token_t            lexer_token;
symbol_t          *symbol_L;
static FILE       *input;
static char        buf[1024 + MAX_PUTBACK];
static const char *bufend;
static const char *bufpos;
static strset_t    stringset;

static type_t     *type_int        = NULL;
static type_t     *type_uint       = NULL;
static type_t     *type_long       = NULL;
static type_t     *type_ulong      = NULL;
static type_t     *type_longlong   = NULL;
static type_t     *type_ulonglong  = NULL;
static type_t     *type_float      = NULL;
static type_t     *type_double     = NULL;
static type_t     *type_longdouble = NULL;

static void error_prefix_at(const char *input_name, unsigned linenr)
{
	fprintf(stderr, "%s:%u: Error: ", input_name, linenr);
}

static void error_prefix(void)
{
	error_prefix_at(lexer_token.source_position.input_name,
	                lexer_token.source_position.linenr);
}

static void parse_error(const char *msg)
{
	error_prefix();
	fprintf(stderr, "%s\n", msg);
}

static inline void next_real_char(void)
{
	bufpos++;
	if(bufpos >= bufend) {
		size_t s = fread(buf + MAX_PUTBACK, 1, sizeof(buf) - MAX_PUTBACK,
		                 input);
		if(s == 0) {
			c = EOF;
			return;
		}
		bufpos = buf + MAX_PUTBACK;
		bufend = buf + MAX_PUTBACK + s;
	}
	c = *(bufpos);
}

static inline void put_back(int pc)
{
	assert(bufpos >= buf);
	//assert(bufpos < buf+MAX_PUTBACK || *bufpos == pc);

	char *p = buf + (bufpos - buf);
	*p = (char) pc;

	/* going backwards in the buffer is legal as long as it's not more often
	 * than MAX_PUTBACK */
	bufpos--;

#ifdef DEBUG_CHARS
	printf("putback '%c'\n", pc);
#endif
}

static inline void next_char(void);

#define MATCH_NEWLINE(code)                   \
	case '\r':                                \
		next_char();                          \
		if(c == '\n') {                       \
			next_char();                      \
		}                                     \
		lexer_token.source_position.linenr++; \
		code;                                 \
	case '\n':                                \
		next_char();                          \
		lexer_token.source_position.linenr++; \
		code;

#define eat(c_type)  do { assert(c == c_type); next_char(); } while(0)

static void maybe_concat_lines(void)
{
	eat('\\');

	switch(c) {
	MATCH_NEWLINE(return;)

	default:
		break;
	}

	put_back(c);
	c = '\\';
}

static inline void next_char(void)
{
	next_real_char();

	/* filter trigraphs */
	if(UNLIKELY(c == '\\')) {
		maybe_concat_lines();
		goto end_of_next_char;
	}

	if(LIKELY(c != '?'))
		goto end_of_next_char;

	next_real_char();
	if(LIKELY(c != '?')) {
		put_back(c);
		c = '?';
		goto end_of_next_char;
	}

	next_real_char();
	switch(c) {
	case '=': c = '#'; break;
	case '(': c = '['; break;
	case '/': c = '\\'; maybe_concat_lines(); break;
	case ')': c = ']'; break;
	case '\'': c = '^'; break;
	case '<': c = '{'; break;
	case '!': c = '|'; break;
	case '>': c = '}'; break;
	case '-': c = '~'; break;
	default:
		put_back('?');
		put_back(c);
		c = '?';
		break;
	}

end_of_next_char:;
#ifdef DEBUG_CHARS
	printf("nchar '%c'\n", c);
#endif
}

#define SYMBOL_CHARS  \
	case 'a':         \
	case 'b':         \
	case 'c':         \
	case 'd':         \
	case 'e':         \
	case 'f':         \
	case 'g':         \
	case 'h':         \
	case 'i':         \
	case 'j':         \
	case 'k':         \
	case 'l':         \
	case 'm':         \
	case 'n':         \
	case 'o':         \
	case 'p':         \
	case 'q':         \
	case 'r':         \
	case 's':         \
	case 't':         \
	case 'u':         \
	case 'v':         \
	case 'w':         \
	case 'x':         \
	case 'y':         \
	case 'z':         \
	case 'A':         \
	case 'B':         \
	case 'C':         \
	case 'D':         \
	case 'E':         \
	case 'F':         \
	case 'G':         \
	case 'H':         \
	case 'I':         \
	case 'J':         \
	case 'K':         \
	case 'L':         \
	case 'M':         \
	case 'N':         \
	case 'O':         \
	case 'P':         \
	case 'Q':         \
	case 'R':         \
	case 'S':         \
	case 'T':         \
	case 'U':         \
	case 'V':         \
	case 'W':         \
	case 'X':         \
	case 'Y':         \
	case 'Z':         \
	case '_':

#define DIGITS        \
	case '0':         \
	case '1':         \
	case '2':         \
	case '3':         \
	case '4':         \
	case '5':         \
	case '6':         \
	case '7':         \
	case '8':         \
	case '9':

static void parse_symbol(void)
{
	symbol_t *symbol;
	char     *string;

	obstack_1grow(&symbol_obstack, (char) c);
	next_char();

	while(1) {
		switch(c) {
		DIGITS
		SYMBOL_CHARS
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
			break;

		default:
			goto end_symbol;
		}
	}

end_symbol:
	obstack_1grow(&symbol_obstack, '\0');

	string = obstack_finish(&symbol_obstack);
	symbol = symbol_table_insert(string);

	lexer_token.type     = symbol->ID;
	lexer_token.v.symbol = symbol;

	if(symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}
}

static void parse_integer_suffix(bool is_oct_hex)
{
	bool is_unsigned  = false;
	bool min_long     = false;
	bool min_longlong = false;

	if(c == 'U' || c == 'u') {
		is_unsigned = true;
		next_char();
		if(c == 'L' || c == 'l') {
			min_long = true;
			next_char();
			if(c == 'L' || c == 'l') {
				min_longlong = true;
				next_char();
			}
		}
	} else if(c == 'l' || c == 'L') {
		min_long = true;
		next_char();
		if(c == 'l' || c == 'L') {
			min_longlong = true;
			next_char();
			if(c == 'u' || c == 'U') {
				is_unsigned = true;
				next_char();
			}
		} else if(c == 'u' || c == 'U') {
			is_unsigned = true;
			next_char();
			lexer_token.datatype = type_ulong;
		}
	}

	if(!is_unsigned) {
		long long v = lexer_token.v.intvalue;
		if(!min_long) {
			if(v >= TARGET_INT_MIN && v <= TARGET_INT_MAX) {
				lexer_token.datatype = type_int;
				return;
			} else if(is_oct_hex && v >= 0 && v <= TARGET_UINT_MAX) {
				lexer_token.datatype = type_uint;
				return;
			}
		}
		if(!min_longlong) {
			if(v >= TARGET_LONG_MIN && v <= TARGET_LONG_MAX) {
				lexer_token.datatype = type_long;
				return;
			} else if(is_oct_hex && v >= 0 && v <= TARGET_ULONG_MAX) {
				lexer_token.datatype = type_ulong;
				return;
			}
		}
		unsigned long long uv = (unsigned long long) v;
		if(is_oct_hex && uv > (unsigned long long) TARGET_LONGLONG_MAX) {
			lexer_token.datatype = type_ulonglong;
			return;
		}

		lexer_token.datatype = type_longlong;
	} else {
		unsigned long long v = (unsigned long long) lexer_token.v.intvalue;
		if(!min_long && v <= TARGET_UINT_MAX) {
			lexer_token.datatype = type_uint;
			return;
		}
		if(!min_longlong && v <= TARGET_ULONG_MAX) {
			lexer_token.datatype = type_ulong;
			return;
		}
		lexer_token.datatype = type_ulonglong;
	}
}

static void parse_floating_suffix(void)
{
	switch(c) {
	/* TODO: do something usefull with the suffixes... */
	case 'f':
	case 'F':
		next_char();
		lexer_token.datatype = type_float;
		break;
	case 'l':
	case 'L':
		next_char();
		lexer_token.datatype = type_longdouble;
		break;
	default:
		lexer_token.datatype = type_double;
		break;
	}
}

/**
 * A replacement for strtoull. Only those parts needed for
 * our parser are implemented.
 */
static unsigned long long parse_int_string(const char *s, const char **endptr, int base) {
	unsigned long long v = 0;

	switch (base) {
	case 16:
		for (;; ++s) {
			/* check for overrun */
			if (v >= 0x1000000000000000ULL)
				break;
			switch (tolower(*s)) {
			case '0': v <<= 4; break;
			case '1': v <<= 4; v |= 0x1; break;
			case '2': v <<= 4; v |= 0x2; break;
			case '3': v <<= 4; v |= 0x3; break;
			case '4': v <<= 4; v |= 0x4; break;
			case '5': v <<= 4; v |= 0x5; break;
			case '6': v <<= 4; v |= 0x6; break;
			case '7': v <<= 4; v |= 0x7; break;
			case '8': v <<= 4; v |= 0x8; break;
			case '9': v <<= 4; v |= 0x9; break;
			case 'a': v <<= 4; v |= 0xa; break;
			case 'b': v <<= 4; v |= 0xb; break;
			case 'c': v <<= 4; v |= 0xc; break;
			case 'd': v <<= 4; v |= 0xd; break;
			case 'e': v <<= 4; v |= 0xe; break;
			case 'f': v <<= 4; v |= 0xf; break;
			default:
				goto end;
			}
		}
		break;
	case 8:
		for (;; ++s) {
			/* check for overrun */
			if (v >= 0x2000000000000000ULL)
				break;
			switch (tolower(*s)) {
			case '0': v <<= 3; break;
			case '1': v <<= 3; v |= 1; break;
			case '2': v <<= 3; v |= 2; break;
			case '3': v <<= 3; v |= 3; break;
			case '4': v <<= 3; v |= 4; break;
			case '5': v <<= 3; v |= 5; break;
			case '6': v <<= 3; v |= 6; break;
			case '7': v <<= 3; v |= 7; break;
			default:
				goto end;
			}
		}
		break;
	case 10:
		for (;; ++s) {
			/* check for overrun */
			if (v > 0x1999999999999999ULL)
				break;
			switch (tolower(*s)) {
			case '0': v *= 10; break;
			case '1': v *= 10; v += 1; break;
			case '2': v *= 10; v += 2; break;
			case '3': v *= 10; v += 3; break;
			case '4': v *= 10; v += 4; break;
			case '5': v *= 10; v += 5; break;
			case '6': v *= 10; v += 6; break;
			case '7': v *= 10; v += 7; break;
			case '8': v *= 10; v += 8; break;
			case '9': v *= 10; v += 9; break;
			default:
				goto end;
			}
		}
		break;
	default:
		assert(0);
		break;
	}
end:
	*endptr = s;
	return v;
}

static void parse_number_hex(void)
{
	assert(c == 'x' || c == 'X');
	next_char();

	while(isxdigit(c)) {
		obstack_1grow(&symbol_obstack, (char) c);
		next_char();
	}
	obstack_1grow(&symbol_obstack, '\0');
	char *string = obstack_finish(&symbol_obstack);

	if(c == '.' || c == 'p' || c == 'P') {
		next_char();
		panic("Hex floating point numbers not implemented yet");
	}
	if(*string == '\0') {
		parse_error("invalid hex number");
		lexer_token.type = T_ERROR;
	}

	const char *endptr;
	lexer_token.type       = T_INTEGER;
	lexer_token.v.intvalue = parse_int_string(string, &endptr, 16);
	if(*endptr != '\0') {
		parse_error("hex number literal too long");
	}

	obstack_free(&symbol_obstack, string);
	parse_integer_suffix(true);
}

static inline bool is_octal_digit(int chr)
{
	return '0' <= chr && chr <= '7';
}

static void parse_number_oct(void)
{
	while(is_octal_digit(c)) {
		obstack_1grow(&symbol_obstack, (char) c);
		next_char();
	}
	obstack_1grow(&symbol_obstack, '\0');
	char *string = obstack_finish(&symbol_obstack);

	const char *endptr;
	lexer_token.type       = T_INTEGER;
	lexer_token.v.intvalue = parse_int_string(string, &endptr, 8);
	if(*endptr != '\0') {
		parse_error("octal number literal too long");
	}

	obstack_free(&symbol_obstack, string);
	parse_integer_suffix(true);
}

static void parse_number_dec(void)
{
	bool is_float = false;
	while(isdigit(c)) {
		obstack_1grow(&symbol_obstack, (char) c);
		next_char();
	}

	if(c == '.') {
		obstack_1grow(&symbol_obstack, '.');
		next_char();

		while(isdigit(c)) {
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}
		is_float = true;
	}
	if(c == 'e' || c == 'E') {
		obstack_1grow(&symbol_obstack, 'e');
		next_char();

		if(c == '-' || c == '+') {
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}

		while(isdigit(c)) {
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
		}
		is_float = true;
	}

	obstack_1grow(&symbol_obstack, '\0');
	char *string = obstack_finish(&symbol_obstack);

	if(is_float) {
		char *endptr;
		lexer_token.type         = T_FLOATINGPOINT;
		lexer_token.v.floatvalue = strtold(string, &endptr);

		if(*endptr != '\0') {
			parse_error("invalid number literal");
		}

		parse_floating_suffix();
	} else {
		const char *endptr;
		lexer_token.type       = T_INTEGER;
		lexer_token.v.intvalue = parse_int_string(string, &endptr, 10);

		if(*endptr != '\0') {
			parse_error("invalid number literal");
		}

		parse_integer_suffix(false);
	}
	obstack_free(&symbol_obstack, string);
}

static void parse_number(void)
{
	if (c == '0') {
		next_char();
		switch (c) {
			case 'X':
			case 'x':
				parse_number_hex();
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
				parse_number_oct();
				break;
			case '8':
			case '9':
				next_char();
				parse_error("invalid octal number");
				lexer_token.type = T_ERROR;
				return;
			case '.':
			case 'e':
			case 'E':
			default:
				obstack_1grow(&symbol_obstack, '0');
				parse_number_dec();
				return;
		}
	} else {
		parse_number_dec();
	}
}

static int parse_octal_sequence(const int first_digit)
{
	assert(is_octal_digit(first_digit));
	int value = first_digit - '0';
	if (!is_octal_digit(c)) return value;
	value = 8 * value + c - '0';
	next_char();
	if (!is_octal_digit(c)) return value;
	value = 8 * value + c - '0';
	next_char();
	return (char_type)value;
}

static int parse_hex_sequence(void)
{
	int value = 0;
	while(1) {
		if (c >= '0' && c <= '9') {
			value = 16 * value + c - '0';
		} else if ('A' <= c && c <= 'F') {
			value = 16 * value + c - 'A' + 10;
		} else if ('a' <= c && c <= 'f') {
			value = 16 * value + c - 'a' + 10;
		} else {
			break;
		}
		next_char();
	}

	return (char_type)value;
}

static int parse_escape_sequence(void)
{
	eat('\\');

	int ec = c;
	next_char();

	switch(ec) {
	case '"':  return '"';
	case '\'': return '\'';
	case '\\': return '\\';
	case '?': return '\?';
	case 'a': return '\a';
	case 'b': return '\b';
	case 'f': return '\f';
	case 'n': return '\n';
	case 'r': return '\r';
	case 't': return '\t';
	case 'v': return '\v';
	case 'x':
		return parse_hex_sequence();
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
		return parse_octal_sequence(ec);
	case EOF:
		parse_error("reached end of file while parsing escape sequence");
		return EOF;
	default:
		parse_error("unknown escape sequence");
		return EOF;
	}
}

const char *concat_strings(const char *s1, const char *s2)
{
	size_t  len1   = strlen(s1);
	size_t  len2   = strlen(s2);

	char   *concat = obstack_alloc(&symbol_obstack, len1 + len2 + 1);
	memcpy(concat, s1, len1);
	memcpy(concat + len1, s2, len2 + 1);

	const char *result = strset_insert(&stringset, concat);
	if(result != concat) {
		obstack_free(&symbol_obstack, concat);
	}

	return result;
}

static void parse_string_literal(void)
{
	unsigned    start_linenr = lexer_token.source_position.linenr;
	char       *string;
	const char *result;

	assert(c == '"');
	next_char();

	int tc;
	while(1) {
		switch(c) {
		case '\\':
			tc = parse_escape_sequence();
			obstack_1grow(&symbol_obstack, (char) tc);
			break;

		case EOF:
			error_prefix_at(lexer_token.source_position.input_name,
			                start_linenr);
			fprintf(stderr, "string has no end\n");
			lexer_token.type = T_ERROR;
			return;

		case '"':
			next_char();
			goto end_of_string;

		default:
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
			break;
		}
	}

end_of_string:

	/* TODO: concatenate multiple strings separated by whitespace... */

	/* add finishing 0 to the string */
	obstack_1grow(&symbol_obstack, '\0');
	string = obstack_finish(&symbol_obstack);

	/* check if there is already a copy of the string */
	result = strset_insert(&stringset, string);
	if(result != string) {
		obstack_free(&symbol_obstack, string);
	}

	lexer_token.type     = T_STRING_LITERAL;
	lexer_token.v.string = result;
}

static void parse_wide_character_constant(void)
{
	eat('\'');

	int found_char = 0;
	while(1) {
		switch(c) {
		case '\\':
			found_char = parse_escape_sequence();
			break;

		MATCH_NEWLINE(
			parse_error("newline while parsing character constant");
			break;
		)

		case '\'':
			next_char();
			goto end_of_wide_char_constant;

		case EOF:
			parse_error("EOF while parsing character constant");
			lexer_token.type = T_ERROR;
			return;

		default:
			if(found_char != 0) {
				parse_error("more than 1 characters in character "
				            "constant");
				goto end_of_wide_char_constant;
			} else {
				found_char = c;
				next_char();
			}
			break;
		}
	}

end_of_wide_char_constant:
	lexer_token.type       = T_INTEGER;
	lexer_token.v.intvalue = found_char;
	lexer_token.datatype   = type_wchar_t;
}

static void parse_wide_string_literal(void)
{
	const unsigned start_linenr = lexer_token.source_position.linenr;

	assert(c == '"');
	next_char();

	while(1) {
		switch(c) {
			case '\\': {
				wchar_rep_t tc = parse_escape_sequence();
				obstack_grow(&symbol_obstack, &tc, sizeof(tc));
				break;
			}

			case EOF:
				error_prefix_at(lexer_token.source_position.input_name,
				                start_linenr);
				fprintf(stderr, "string has no end\n");
				lexer_token.type = T_ERROR;
				return;

			case '"':
				next_char();
				goto end_of_string;

			default: {
				wchar_rep_t tc = c;
				obstack_grow(&symbol_obstack, &tc, sizeof(tc));
				next_char();
				break;
			}
		}
	}

end_of_string:;

	/* TODO: concatenate multiple strings separated by whitespace... */

	/* add finishing 0 to the string */
	wchar_rep_t nul = L'\0';
	obstack_grow(&symbol_obstack, &nul, sizeof(nul));
	const size_t             size   = (size_t)obstack_object_size(&symbol_obstack) / sizeof(wchar_rep_t);
	const wchar_rep_t *const string = obstack_finish(&symbol_obstack);

#if 0 /* TODO hash */
	/* check if there is already a copy of the string */
	const wchar_rep_t *const result = strset_insert(&stringset, string);
	if(result != string) {
		obstack_free(&symbol_obstack, string);
	}
#else
	const wchar_rep_t *const result = string;
#endif

	lexer_token.type                = T_WIDE_STRING_LITERAL;
	lexer_token.v.wide_string.begin = result;
	lexer_token.v.wide_string.size  = size;
}

static void parse_character_constant(void)
{
	eat('\'');

	int found_char = 0;
	while(1) {
		switch(c) {
		case '\\':
			found_char = parse_escape_sequence();
			break;

		MATCH_NEWLINE(
			parse_error("newline while parsing character constant");
			break;
		)

		case '\'':
			next_char();
			goto end_of_char_constant;

		case EOF:
			parse_error("EOF while parsing character constant");
			lexer_token.type = T_ERROR;
			return;

		default:
			if(found_char != 0) {
				parse_error("more than 1 characters in character "
				            "constant");
				goto end_of_char_constant;
			} else {
				found_char = c;
				next_char();
			}
			break;
		}
	}

end_of_char_constant:
	lexer_token.type       = T_INTEGER;
	lexer_token.v.intvalue = found_char;
	lexer_token.datatype   = type_int;
}

static void skip_multiline_comment(void)
{
	unsigned start_linenr = lexer_token.source_position.linenr;

	while(1) {
		switch(c) {
		case '*':
			next_char();
			if(c == '/') {
				next_char();
				return;
			}
			break;

		MATCH_NEWLINE(break;)

		case EOF:
			error_prefix_at(lexer_token.source_position.input_name,
			                start_linenr);
			fprintf(stderr, "at end of file while looking for comment end\n");
			return;

		default:
			next_char();
			break;
		}
	}
}

static void skip_line_comment(void)
{
	while(1) {
		switch(c) {
		case EOF:
			return;

		case '\n':
		case '\r':
			return;

		default:
			next_char();
			break;
		}
	}
}

static token_t pp_token;

static inline void next_pp_token(void)
{
	lexer_next_preprocessing_token();
	pp_token = lexer_token;
}

static void eat_until_newline(void)
{
	while(pp_token.type != '\n' && pp_token.type != T_EOF) {
		next_pp_token();
	}
}

static void error_directive(void)
{
	error_prefix();
	fprintf(stderr, "#error directive: \n");

	/* parse pp-tokens until new-line */
}

static void define_directive(void)
{
	lexer_next_preprocessing_token();
	if(lexer_token.type != T_IDENTIFIER) {
		parse_error("expected identifier after #define\n");
		eat_until_newline();
	}
}

static void ifdef_directive(int is_ifndef)
{
	(void) is_ifndef;
	lexer_next_preprocessing_token();
	//expect_identifier();
	//extect_newline();
}

static void endif_directive(void)
{
	//expect_newline();
}

static void parse_line_directive(void)
{
	if(pp_token.type != T_INTEGER) {
		parse_error("expected integer");
	} else {
		lexer_token.source_position.linenr = (unsigned int)(pp_token.v.intvalue - 1);
		next_pp_token();
	}
	if(pp_token.type == T_STRING_LITERAL) {
		lexer_token.source_position.input_name = pp_token.v.string;
		next_pp_token();
	}

	eat_until_newline();
}

static void parse_preprocessor_identifier(void)
{
	assert(pp_token.type == T_IDENTIFIER);
	symbol_t *symbol = pp_token.v.symbol;

	switch(symbol->pp_ID) {
	case TP_include:
		printf("include - enable header name parsing!\n");
		break;
	case TP_define:
		define_directive();
		break;
	case TP_ifdef:
		ifdef_directive(0);
		break;
	case TP_ifndef:
		ifdef_directive(1);
		break;
	case TP_endif:
		endif_directive();
		break;
	case TP_line:
		next_pp_token();
		parse_line_directive();
		break;
	case TP_if:
	case TP_else:
	case TP_elif:
	case TP_undef:
	case TP_error:
		error_directive();
		break;
	case TP_pragma:
		break;
	}
}

static void parse_preprocessor_directive(void)
{
	next_pp_token();

	switch(pp_token.type) {
	case T_IDENTIFIER:
		parse_preprocessor_identifier();
		break;
	case T_INTEGER:
		parse_line_directive();
		break;
	default:
		parse_error("invalid preprocessor directive");
		eat_until_newline();
		break;
	}
}

#define MAYBE_PROLOG                                       \
			next_char();                                   \
			while(1) {                                     \
				switch(c) {

#define MAYBE(ch, set_type)                                \
				case ch:                                   \
					next_char();                           \
					lexer_token.type = set_type;           \
					return;

#define ELSE_CODE(code)                                    \
				default:                                   \
					code;                                  \
				}                                          \
			} /* end of while(1) */                        \
			break;

#define ELSE(set_type)                                     \
		ELSE_CODE(                                         \
			lexer_token.type = set_type;                   \
			return;                                        \
		)

void lexer_next_preprocessing_token(void)
{
	while(1) {
		switch(c) {
		case ' ':
		case '\t':
			next_char();
			break;

		MATCH_NEWLINE(
			lexer_token.type = '\n';
			return;
		)

		SYMBOL_CHARS
			parse_symbol();
			/* might be a wide string ( L"string" ) */
			if(lexer_token.type == T_IDENTIFIER &&
			    lexer_token.v.symbol == symbol_L) {
			    if(c == '"') {
					parse_wide_string_literal();
				} else if(c == '\'') {
					parse_wide_character_constant();
				}
			}
			return;

		DIGITS
			parse_number();
			return;

		case '"':
			parse_string_literal();
			return;

		case '\'':
			parse_character_constant();
			return;

		case '.':
			MAYBE_PROLOG
				case '.':
					MAYBE_PROLOG
					MAYBE('.', T_DOTDOTDOT)
					ELSE_CODE(
						put_back(c);
						c = '.';
						lexer_token.type = '.';
						return;
					)
			ELSE('.')
		case '&':
			MAYBE_PROLOG
			MAYBE('&', T_ANDAND)
			MAYBE('=', T_ANDEQUAL)
			ELSE('&')
		case '*':
			MAYBE_PROLOG
			MAYBE('=', T_ASTERISKEQUAL)
			ELSE('*')
		case '+':
			MAYBE_PROLOG
			MAYBE('+', T_PLUSPLUS)
			MAYBE('=', T_PLUSEQUAL)
			ELSE('+')
		case '-':
			MAYBE_PROLOG
			MAYBE('>', T_MINUSGREATER)
			MAYBE('-', T_MINUSMINUS)
			MAYBE('=', T_MINUSEQUAL)
			ELSE('-')
		case '!':
			MAYBE_PROLOG
			MAYBE('=', T_EXCLAMATIONMARKEQUAL)
			ELSE('!')
		case '/':
			MAYBE_PROLOG
			MAYBE('=', T_SLASHEQUAL)
				case '*':
					next_char();
					skip_multiline_comment();
					lexer_next_preprocessing_token();
					return;
				case '/':
					next_char();
					skip_line_comment();
					lexer_next_preprocessing_token();
					return;
			ELSE('/')
		case '%':
			MAYBE_PROLOG
			MAYBE('>', T_PERCENTGREATER)
			MAYBE('=', T_PERCENTEQUAL)
				case ':':
					MAYBE_PROLOG
						case '%':
							MAYBE_PROLOG
							MAYBE(':', T_PERCENTCOLONPERCENTCOLON)
							ELSE_CODE(
								put_back(c);
								c = '%';
								lexer_token.type = T_PERCENTCOLON;
								return;
							)
					ELSE(T_PERCENTCOLON)
			ELSE('%')
		case '<':
			MAYBE_PROLOG
			MAYBE(':', T_LESSCOLON)
			MAYBE('%', T_LESSPERCENT)
			MAYBE('=', T_LESSEQUAL)
				case '<':
					MAYBE_PROLOG
					MAYBE('=', T_LESSLESSEQUAL)
					ELSE(T_LESSLESS)
			ELSE('<')
		case '>':
			MAYBE_PROLOG
			MAYBE('=', T_GREATEREQUAL)
				case '>':
					MAYBE_PROLOG
					MAYBE('=', T_GREATERGREATEREQUAL)
					ELSE(T_GREATERGREATER)
			ELSE('>')
		case '^':
			MAYBE_PROLOG
			MAYBE('=', T_CARETEQUAL)
			ELSE('^')
		case '|':
			MAYBE_PROLOG
			MAYBE('=', T_PIPEEQUAL)
			MAYBE('|', T_PIPEPIPE)
			ELSE('|')
		case ':':
			MAYBE_PROLOG
			MAYBE('>', T_COLONGREATER)
			ELSE(':')
		case '=':
			MAYBE_PROLOG
			MAYBE('=', T_EQUALEQUAL)
			ELSE('=')
		case '#':
			MAYBE_PROLOG
			MAYBE('#', T_HASHHASH)
			ELSE('#')

		case '?':
		case '[':
		case ']':
		case '(':
		case ')':
		case '{':
		case '}':
		case '~':
		case ';':
		case ',':
		case '\\':
			lexer_token.type = c;
			next_char();
			return;

		case EOF:
			lexer_token.type = T_EOF;
			return;

		default:
			next_char();
			error_prefix();
			fprintf(stderr, "unknown character '%c' found\n", c);
			lexer_token.type = T_ERROR;
			return;
		}
	}
}

void lexer_next_token(void)
{
	lexer_next_preprocessing_token();
	if(lexer_token.type != '\n')
		return;

newline_found:
	do {
		lexer_next_preprocessing_token();
	} while(lexer_token.type == '\n');

	if(lexer_token.type == '#') {
		parse_preprocessor_directive();
		goto newline_found;
	}
}

void init_lexer(void)
{
	strset_init(&stringset);

	type_int       = make_atomic_type(ATOMIC_TYPE_INT, TYPE_QUALIFIER_NONE);
	type_uint      = make_atomic_type(ATOMIC_TYPE_UINT, TYPE_QUALIFIER_NONE);
	type_long      = make_atomic_type(ATOMIC_TYPE_LONG, TYPE_QUALIFIER_NONE);
	type_ulong     = make_atomic_type(ATOMIC_TYPE_ULONG, TYPE_QUALIFIER_NONE);
	type_longlong  = make_atomic_type(ATOMIC_TYPE_LONGLONG,
	                                  TYPE_QUALIFIER_NONE);
	type_ulonglong = make_atomic_type(ATOMIC_TYPE_ULONGLONG,
	                                  TYPE_QUALIFIER_NONE);

	type_float      = make_atomic_type(ATOMIC_TYPE_FLOAT, TYPE_QUALIFIER_CONST);
	type_double     = make_atomic_type(ATOMIC_TYPE_DOUBLE,
	                                   TYPE_QUALIFIER_CONST);
	type_longdouble = make_atomic_type(ATOMIC_TYPE_LONG_DOUBLE,
	                                   TYPE_QUALIFIER_CONST);
}

void lexer_open_stream(FILE *stream, const char *input_name)
{
	input                                  = stream;
	lexer_token.source_position.linenr     = 0;
	lexer_token.source_position.input_name = input_name;

	symbol_L = symbol_table_insert("L");

	/* place a virtual \n at the beginning so the lexer knows that we're
	 * at the beginning of a line */
	c = '\n';
}

void exit_lexer(void)
{
	strset_destroy(&stringset);
}

static __attribute__((unused))
void dbg_pos(const source_position_t source_position)
{
	fprintf(stdout, "%s:%u\n", source_position.input_name,
	        source_position.linenr);
	fflush(stdout);
}
