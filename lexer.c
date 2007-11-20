#include <config.h>

#include "lexer.h"
#include "token_t.h"
#include "symbol_table_t.h"
#include "adt/error.h"
#include "adt/strset.h"
#include "adt/util.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

//#define DEBUG_CHARS
#define MAX_PUTBACK 3

static int         c;
token_t            lexer_token;
symbol_t          *symbol_L;
static FILE       *input;
static char        buf[1024 + MAX_PUTBACK];
static const char *bufend;
static const char *bufpos;
static strset_t    stringset;

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
	assert(bufpos < buf+MAX_PUTBACK || *bufpos == pc);

	char *p = buf + (bufpos - buf);
	*p = pc;

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

#if 0
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
#endif
	(void) maybe_concat_lines;
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

	obstack_1grow(&symbol_obstack, c);
	next_char();

	while(1) {
		switch(c) {
		DIGITS
		SYMBOL_CHARS
			obstack_1grow(&symbol_obstack, c);
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

static void parse_integer_suffix(void)
{
	if(c == 'U' || c == 'U') {
		/* TODO do something with the suffixes... */
		next_char();
		if(c == 'L' || c == 'l') {
			next_char();
			if(c == 'L' || c == 'l') {
				next_char();
			}
		}
	} else if(c == 'l' || c == 'L') {
		next_char();
		if(c == 'l' || c == 'L') {
			next_char();
			if(c == 'u' || c == 'U') {
				next_char();
			}
		} else if(c == 'u' || c == 'U') {
			next_char();
		}
	}
}

static void parse_floating_suffix(void)
{
	switch(c) {
	/* TODO: do something usefull with the suffixes... */
	case 'f':
	case 'F':
	case 'l':
	case 'L':
		next_char();
		break;
	default:
		break;
	}
}

static void parse_number_hex(void)
{
	assert(c == 'x' || c == 'X');
	next_char();

	if (!isdigit(c) &&
		!('A' <= c && c <= 'F') &&
		!('a' <= c && c <= 'f')) {
		parse_error("premature end of hex number literal");
		lexer_token.type = T_ERROR;
		return;
	}

	int value = 0;
	while(1) {
		if (isdigit(c)) {
			value = 16 * value + c - '0';
		} else if ('A' <= c && c <= 'F') {
			value = 16 * value + c - 'A' + 10;
		} else if ('a' <= c && c <= 'f') {
			value = 16 * value + c - 'a' + 10;
		} else {
			parse_integer_suffix();

			lexer_token.type       = T_INTEGER;
			lexer_token.v.intvalue = value;
			return;
		}
		next_char();
	}

	if(c == '.' || c == 'p' || c == 'P') {
		next_char();
		panic("Hex floating point numbers not implemented yet");
	}
}

static void parse_number_oct(void)
{
	int value = 0;
	while(c >= '0' && c <= '7') {
		value = 8 * value + c - '0';
		next_char();
	}
	if (c == '8' || c == '9') {
		parse_error("invalid octal number");
		lexer_token.type = T_ERROR;
		return;
	}

	lexer_token.type       = T_INTEGER;
	lexer_token.v.intvalue = value;

	parse_integer_suffix();
}

static void parse_floatingpoint_exponent(long double value)
{
	unsigned int expo = 0;
	long double  factor = 10.;

	if(c == '-') {
		next_char();
		factor = 0.1;
	} else if(c == '+') {
		next_char();
	}

	while(c >= '0' && c <= '9') {
		expo = 10 * expo + (c - '0');
		next_char();
	}

	while(1) {
		if(expo & 1)
			value *= factor;
		expo >>= 1;
		if(expo == 0)
			break;
		factor *= factor;
	}

	lexer_token.type         = T_FLOATINGPOINT;
	lexer_token.v.floatvalue = value;

	parse_floating_suffix();
}

static void parse_floatingpoint_fract(int integer_part)
{
	long double value  = integer_part;
	long double factor = 1.;

	while(c >= '0' && c <= '9') {
		factor *= 0.1;
		value  += (c - '0') * factor;
		next_char();
	}

	if(c == 'e' || c == 'E') {
		next_char();
		parse_floatingpoint_exponent(value);
		return;
	}

	lexer_token.type         = T_FLOATINGPOINT;
	lexer_token.v.floatvalue = value;

	parse_floating_suffix();
}

static void parse_number_dec(void)
{
	int value = 0;

	while(isdigit(c)) {
		value = 10 * value + c - '0';
		next_char();
	}

	if(c == '.') {
		next_char();
		parse_floatingpoint_fract(value);
		return;
	}
	if(c == 'e' || c == 'E') {
		next_char();
		parse_floatingpoint_exponent(value);
		return;
	}
	parse_integer_suffix();

	lexer_token.type       = T_INTEGER;
	lexer_token.v.intvalue = value;
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
			case '.':
				next_char();
				parse_floatingpoint_fract(0);
				break;
			case 'e':
			case 'E':
				parse_floatingpoint_exponent(0);
				break;
			case '8':
			case '9':
				next_char();
				parse_error("invalid octal number");
				lexer_token.type = T_ERROR;
				return;
			default:
				put_back(c);
				c = '0';
				parse_number_dec();
				return;
		}
	} else {
		parse_number_dec();
	}
}

static inline int is_octal_digit(int chr)
{
	return '0' <= chr && chr <= '7';
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
	return value;
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

	return value;
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
			obstack_1grow(&symbol_obstack, tc);
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
			obstack_1grow(&symbol_obstack, c);
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
		lexer_token.source_position.linenr = pp_token.v.intvalue - 1;
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
			if(c == '"' && (lexer_token.type == T_IDENTIFIER &&
			   lexer_token.v.symbol == symbol_L)) {
			   	parse_string_literal();
			   	return;
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
	fprintf(stdout, "%s:%d\n", source_position.input_name,
	        source_position.linenr);
	fflush(stdout);
}
