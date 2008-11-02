#include <config.h>

#include "token_t.h"
#include "symbol_t.h"
#include "adt/util.h"
#include "adt/error.h"
#include "lang_features.h"
#include "diagnostic.h"
#include "string_rep.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

//#define DEBUG_CHARS
#define MAX_PUTBACK 3

struct pp_definition_t {
	symbol_t          *symbol;
	source_position_t  source_position;
	pp_definition_t   *parent_expansion;
	size_t             expand_pos;
	bool               is_variadic  : 1;
	bool               is_expanding : 1;
	size_t             argument_count;
	token_t           *arguments;
	size_t             list_len;
	token_t           *replacement_list;
};

static int                c;
token_t                   pp_token;
static FILE              *input;
static char               buf[1024 + MAX_PUTBACK];
static const char        *bufend;
static const char        *bufpos;
static bool               resolve_escape_sequences = false;
static bool               print_spaces             = true;
static FILE              *out;
static struct obstack     pp_obstack;
static unsigned           counted_newlines;
static unsigned           counted_spaces;
static source_position_t  input_position;
static const char        *printed_input_name = NULL;
static pp_definition_t   *current_expansion  = NULL;
static bool               do_expansions;

static void next_preprocessing_token(void);

/**
 * Prints a parse error message at the current token.
 *
 * @param msg   the error message
 */
static void parse_error(const char *msg)
{
	errorf(&pp_token.source_position,  "%s", msg);
}

static inline void next_real_char(void)
{
	assert(bufpos <= bufend);
	if (bufpos >= bufend) {
		size_t s = fread(buf + MAX_PUTBACK, 1, sizeof(buf) - MAX_PUTBACK,
		                 input);
		if(s == 0) {
			c = EOF;
			return;
		}
		bufpos = buf + MAX_PUTBACK;
		bufend = buf + MAX_PUTBACK + s;
	}
	c = *bufpos++;
}

/**
 * Put a character back into the buffer.
 *
 * @param pc  the character to put back
 */
static inline void put_back(int pc)
{
	assert(bufpos > buf);
	*(--bufpos - buf + buf) = (char) pc;

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
		++input_position.linenr;              \
		code                                  \
	case '\n':                                \
		next_char();                          \
		++input_position.linenr;              \
		code

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

/**
 * Set c to the next input character, ie.
 * after expanding trigraphs.
 */
static inline void next_char(void)
{
	next_real_char();

	/* filter trigraphs and concatenated lines */
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
		put_back(c);
		put_back('?');
		c = '?';
		break;
	}

end_of_next_char:;
#ifdef DEBUG_CHARS
	printf("nchar '%c'\n", c);
#endif
}



/**
 * Returns true if the given char is a octal digit.
 *
 * @param char  the character to check
 */
static inline bool is_octal_digit(int chr)
{
	switch(chr) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
		return true;
	default:
		return false;
	}
}

/**
 * Returns the value of a digit.
 * The only portable way to do it ...
 */
static int digit_value(int digit) {
	switch (digit) {
	case '0': return 0;
	case '1': return 1;
	case '2': return 2;
	case '3': return 3;
	case '4': return 4;
	case '5': return 5;
	case '6': return 6;
	case '7': return 7;
	case '8': return 8;
	case '9': return 9;
	case 'a':
	case 'A': return 10;
	case 'b':
	case 'B': return 11;
	case 'c':
	case 'C': return 12;
	case 'd':
	case 'D': return 13;
	case 'e':
	case 'E': return 14;
	case 'f':
	case 'F': return 15;
	default:
		panic("wrong character given");
	}
}

/**
 * Parses an octal character sequence.
 *
 * @param first_digit  the already read first digit
 */
static int parse_octal_sequence(const int first_digit)
{
	assert(is_octal_digit(first_digit));
	int value = digit_value(first_digit);
	if (!is_octal_digit(c)) return value;
	value = 8 * value + digit_value(c);
	next_char();
	if (!is_octal_digit(c)) return value;
	value = 8 * value + digit_value(c);
	next_char();

	if(char_is_signed) {
		return (signed char) value;
	} else {
		return (unsigned char) value;
	}
}

/**
 * Parses a hex character sequence.
 */
static int parse_hex_sequence(void)
{
	int value = 0;
	while(isxdigit(c)) {
		value = 16 * value + digit_value(c);
		next_char();
	}

	if(char_is_signed) {
		return (signed char) value;
	} else {
		return (unsigned char) value;
	}
}

/**
 * Parse an escape sequence.
 */
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

static void parse_string_literal(void)
{
	const unsigned start_linenr = input_position.linenr;

	eat('"');

	int tc;
	while(1) {
		switch(c) {
		case '\\':
			if(resolve_escape_sequences) {
				tc = parse_escape_sequence();
				obstack_1grow(&symbol_obstack, (char) tc);
			} else {
				obstack_1grow(&symbol_obstack, (char) c);
				next_char();
				obstack_1grow(&symbol_obstack, (char) c);
				next_char();
			}
			break;

		case EOF: {
			source_position_t source_position;
			source_position.input_name = pp_token.source_position.input_name;
			source_position.linenr     = start_linenr;
			errorf(&source_position, "string has no end");
			pp_token.type = TP_ERROR;
			return;
		}

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
	/* add finishing 0 to the string */
	obstack_1grow(&symbol_obstack, '\0');
	const size_t      size   = (size_t)obstack_object_size(&symbol_obstack);
	const char *const string = obstack_finish(&symbol_obstack);

#if 0 /* TODO hash */
	/* check if there is already a copy of the string */
	result = strset_insert(&stringset, string);
	if(result != string) {
		obstack_free(&symbol_obstack, string);
	}
#else
	const char *const result = string;
#endif

	pp_token.type           = TP_STRING_LITERAL;
	pp_token.v.string.begin = result;
	pp_token.v.string.size  = size;
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
			pp_token.type = TP_ERROR;
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
	pp_token.type       = TP_WIDE_CHARACTER_CONSTANT;
	/* TODO... */
}

static void parse_wide_string_literal(void)
{
	const unsigned start_linenr = input_position.linenr;

	assert(c == '"');
	next_char();

	while(1) {
		switch(c) {
		case '\\': {
			wchar_rep_t tc = parse_escape_sequence();
			obstack_grow(&symbol_obstack, &tc, sizeof(tc));
			break;
		}

		case EOF: {
			source_position_t source_position;
			source_position.input_name = pp_token.source_position.input_name;
			source_position.linenr     = start_linenr;
			errorf(&source_position, "string has no end");
			pp_token.type = TP_ERROR;
			return;
		}

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
	/* add finishing 0 to the string */
	static const wchar_rep_t nul = L'\0';
	obstack_grow(&symbol_obstack, &nul, sizeof(nul));

	const size_t size
		= (size_t)obstack_object_size(&symbol_obstack) / sizeof(wchar_rep_t);
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

	pp_token.type                = TP_WIDE_STRING_LITERAL;
	pp_token.v.wide_string.begin = result;
	pp_token.v.wide_string.size  = size;
}

static void parse_character_constant(void)
{
	const unsigned start_linenr = input_position.linenr;

	eat('\'');

	int tc;
	while(1) {
		switch(c) {
		case '\\':
			tc = parse_escape_sequence();
			obstack_1grow(&symbol_obstack, (char) tc);
			break;

		MATCH_NEWLINE(
			parse_error("newline while parsing character constant");
			break;
		)

		case EOF: {
			source_position_t source_position;
			source_position.input_name = pp_token.source_position.input_name;
			source_position.linenr     = start_linenr;
			errorf(&source_position, "EOF while parsing character constant");
			pp_token.type = TP_ERROR;
			return;
		}

		case '\'':
			next_char();
			goto end_of_char_constant;

		default:
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
			break;

		}
	}

end_of_char_constant:;
	const size_t      size   = (size_t)obstack_object_size(&symbol_obstack);
	const char *const string = obstack_finish(&symbol_obstack);

	pp_token.type           = TP_CHARACTER_CONSTANT;
	pp_token.v.string.begin = string;
	pp_token.v.string.size  = size;
}

#define SYMBOL_CHARS_WITHOUT_E_P \
	case 'a': \
	case 'b': \
	case 'c': \
	case 'd': \
	case 'f': \
	case 'g': \
	case 'h': \
	case 'i': \
	case 'j': \
	case 'k': \
	case 'l': \
	case 'm': \
	case 'n': \
	case 'o': \
	case 'q': \
	case 'r': \
	case 's': \
	case 't': \
	case 'u': \
	case 'v': \
	case 'w': \
	case 'x': \
	case 'y': \
	case 'z': \
	case 'A': \
	case 'B': \
	case 'C': \
	case 'D': \
	case 'F': \
	case 'G': \
	case 'H': \
	case 'I': \
	case 'J': \
	case 'K': \
	case 'L': \
	case 'M': \
	case 'N': \
	case 'O': \
	case 'Q': \
	case 'R': \
	case 'S': \
	case 'T': \
	case 'U': \
	case 'V': \
	case 'W': \
	case 'X': \
	case 'Y': \
	case 'Z': \
	case '_':

#define SYMBOL_CHARS \
	SYMBOL_CHARS_WITHOUT_E_P \
	case 'e': \
	case 'p': \
	case 'E': \
	case 'P':

#define DIGITS \
	case '0':  \
	case '1':  \
	case '2':  \
	case '3':  \
	case '4':  \
	case '5':  \
	case '6':  \
	case '7':  \
	case '8':  \
	case '9':

/**
 * returns next final token from a preprocessor macro expansion
 */
static void expand_next(void)
{
	assert(current_expansion != NULL);

	pp_definition_t *definition = current_expansion;

restart:
	if(definition->list_len == 0
			|| definition->expand_pos >= definition->list_len) {
		/* we're finished with the current macro, move up 1 level in the
		 * expansion stack */
		pp_definition_t *parent = definition->parent_expansion;
		definition->parent_expansion = NULL;
		definition->is_expanding     = false;
		if(parent == NULL) {
			current_expansion = NULL;
			next_preprocessing_token();
			return;
		}
		definition        = parent;
		current_expansion = definition;
		goto restart;
	}
	pp_token = definition->replacement_list[definition->expand_pos];
	++definition->expand_pos;

	if(pp_token.type != TP_IDENTIFIER)
		return;

	pp_definition_t *symbol_definition = pp_token.v.symbol->pp_definition;
	if(symbol_definition != NULL && !symbol_definition->is_expanding) {
		symbol_definition->parent_expansion = definition;
		symbol_definition->expand_pos       = 0;
		symbol_definition->is_expanding     = true;
		definition                          = symbol_definition;
		current_expansion                   = definition;
		goto restart;
	}
}

static void parse_symbol(void)
{
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
	char *string = obstack_finish(&symbol_obstack);

	/* might be a wide string or character constant ( L"string"/L'c' ) */
	if(c == '"' && string[0] == 'L' && string[1] == '\0') {
		obstack_free(&symbol_obstack, string);
		parse_wide_string_literal();
		return;
	} else if(c == '\'' && string[0] == 'L' && string[1] == '\0') {
		obstack_free(&symbol_obstack, string);
		parse_wide_character_constant();
		return;
	}

	symbol_t *symbol = symbol_table_insert(string);

	pp_token.type     = symbol->pp_ID;
	pp_token.v.symbol = symbol;

	/* we can free the memory from symbol obstack if we already had an entry in
	 * the symbol table */
	if(symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}

	pp_definition_t *pp_definition = symbol->pp_definition;
	if(do_expansions && pp_definition != NULL) {
		pp_definition->expand_pos   = 0;
		pp_definition->is_expanding = true,
		current_expansion           = pp_definition;
		expand_next();
	}
}

static void parse_number(void)
{
	obstack_1grow(&symbol_obstack, (char) c);
	next_char();

	while(1) {
		switch(c) {
		case '.':
		DIGITS
		SYMBOL_CHARS_WITHOUT_E_P
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
			break;

		case 'e':
		case 'p':
		case 'E':
		case 'P':
			obstack_1grow(&symbol_obstack, (char) c);
			next_char();
			if(c == '+' || c == '-') {
				obstack_1grow(&symbol_obstack, (char) c);
				next_char();
			}
			break;

		default:
			goto end_number;
		}
	}

end_number:
	obstack_1grow(&symbol_obstack, '\0');
	size_t  size   = obstack_object_size(&symbol_obstack);
	char   *string = obstack_finish(&symbol_obstack);

	pp_token.type           = TP_NUMBER;
	pp_token.v.string.begin = string;
	pp_token.v.string.size  = size;
}

static void skip_multiline_comment(void)
{
	unsigned start_linenr = input_position.linenr;

	while(1) {
		switch(c) {
		case '/':
			next_char();
			if (c == '*') {
				/* TODO: nested comment, warn here */
			}
			break;
		case '*':
			next_char();
			if(c == '/') {
				next_char();
				return;
			}
			break;

		MATCH_NEWLINE(
			if(print_spaces) {
				counted_newlines++;
				counted_spaces = 0;
			}
			break;
		)

		case EOF: {
			source_position_t source_position;
			source_position.input_name = pp_token.source_position.input_name;
			source_position.linenr     = start_linenr;
			errorf(&source_position, "at end of file while looking for comment end");
			return;
		}

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



#define MAYBE_PROLOG                                       \
			next_char();                                   \
			while(1) {                                     \
				switch(c) {

#define MAYBE(ch, set_type)                                \
				case ch:                                   \
					next_char();                           \
					pp_token.type = set_type;              \
					return;

#define ELSE_CODE(code)                                    \
				default:                                   \
					code;                                  \
				}                                          \
			} /* end of while(1) */                        \
			break;

#define ELSE(set_type)                                     \
		ELSE_CODE(                                         \
			pp_token.type = set_type;                      \
			return;                                        \
		)

static void next_preprocessing_token(void)
{
	if(current_expansion != NULL) {
		expand_next();
		return;
	}

	pp_token.source_position = input_position;

restart:
	switch(c) {
	case ' ':
	case '\t':
 		if(print_spaces)
			counted_spaces++;
 		next_char();
		goto restart;

	MATCH_NEWLINE(
		counted_newlines++;
		counted_spaces = 0;
		pp_token.type = '\n';
		return;
	)

	SYMBOL_CHARS
		parse_symbol();
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
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				put_back(c);
				c = '.';
				parse_number();
				return;

			case '.':
				MAYBE_PROLOG
				MAYBE('.', TP_DOTDOTDOT)
				ELSE_CODE(
					put_back(c);
					c = '.';
					pp_token.type = '.';
					return;
				)
		ELSE('.')
	case '&':
		MAYBE_PROLOG
		MAYBE('&', TP_ANDAND)
		MAYBE('=', TP_ANDEQUAL)
		ELSE('&')
	case '*':
		MAYBE_PROLOG
		MAYBE('=', TP_ASTERISKEQUAL)
		ELSE('*')
	case '+':
		MAYBE_PROLOG
		MAYBE('+', TP_PLUSPLUS)
		MAYBE('=', TP_PLUSEQUAL)
		ELSE('+')
	case '-':
		MAYBE_PROLOG
		MAYBE('>', TP_MINUSGREATER)
		MAYBE('-', TP_MINUSMINUS)
		MAYBE('=', TP_MINUSEQUAL)
		ELSE('-')
	case '!':
		MAYBE_PROLOG
		MAYBE('=', TP_EXCLAMATIONMARKEQUAL)
		ELSE('!')
	case '/':
		MAYBE_PROLOG
		MAYBE('=', TP_SLASHEQUAL)
			case '*':
				next_char();
				skip_multiline_comment();
				if(print_spaces)
					counted_spaces++;
				goto restart;
			case '/':
				next_char();
				skip_line_comment();
				if(print_spaces)
					counted_spaces++;
				goto restart;
		ELSE('/')
	case '%':
		MAYBE_PROLOG
		MAYBE('>', '}')
		MAYBE('=', TP_PERCENTEQUAL)
			case ':':
				MAYBE_PROLOG
					case '%':
						MAYBE_PROLOG
						MAYBE(':', TP_HASHHASH)
						ELSE_CODE(
							put_back(c);
							c = '%';
							pp_token.type = '#';
							return;
						)
				ELSE('#')
		ELSE('%')
	case '<':
		MAYBE_PROLOG
		MAYBE(':', '[')
		MAYBE('%', '{')
		MAYBE('=', TP_LESSEQUAL)
			case '<':
				MAYBE_PROLOG
				MAYBE('=', TP_LESSLESSEQUAL)
				ELSE(TP_LESSLESS)
		ELSE('<')
	case '>':
		MAYBE_PROLOG
		MAYBE('=', TP_GREATEREQUAL)
			case '>':
				MAYBE_PROLOG
				MAYBE('=', TP_GREATERGREATEREQUAL)
				ELSE(TP_GREATERGREATER)
		ELSE('>')
	case '^':
		MAYBE_PROLOG
		MAYBE('=', TP_CARETEQUAL)
		ELSE('^')
	case '|':
		MAYBE_PROLOG
		MAYBE('=', TP_PIPEEQUAL)
		MAYBE('|', TP_PIPEPIPE)
		ELSE('|')
	case ':':
		MAYBE_PROLOG
		MAYBE('>', ']')
		ELSE(':')
	case '=':
		MAYBE_PROLOG
		MAYBE('=', TP_EQUALEQUAL)
		ELSE('=')
	case '#':
		MAYBE_PROLOG
		MAYBE('#', TP_HASHHASH)
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
		pp_token.type = c;
		next_char();
		return;

	case EOF:
		pp_token.type = TP_EOF;
		return;

	default:
		next_char();
		errorf(&pp_token.source_position, "unknown character '%c' found\n", c);
		pp_token.type = TP_ERROR;
		return;
	}
}

static void print_quoted_string(const char *const string)
{
	fputc('"', out);
	for (const char *c = string; *c != 0; ++c) {
		switch(*c) {
		case '"': fputs("\\\"", out); break;
		case '\\':  fputs("\\\\", out); break;
		case '\a':  fputs("\\a", out); break;
		case '\b':  fputs("\\b", out); break;
		case '\f':  fputs("\\f", out); break;
		case '\n':  fputs("\\n", out); break;
		case '\r':  fputs("\\r", out); break;
		case '\t':  fputs("\\t", out); break;
		case '\v':  fputs("\\v", out); break;
		case '\?':  fputs("\\?", out); break;
		default:
			if(!isprint(*c)) {
				fprintf(out, "\\%03o", *c);
				break;
			}
			fputc(*c, out);
			break;
		}
	}
	fputc('"', out);
}

static void print_line_directive(const source_position_t *pos)
{
	fprintf(out, "# %d ", pos->linenr);
	print_quoted_string(pos->input_name);
	fputc('\n', out);

	printed_input_name = pos->input_name;
}

static bool had_non_space = false;

static void emit_pp_token(void)
{
	if (printed_input_name != pp_token.source_position.input_name) {
		print_line_directive(&pp_token.source_position);
	} else if (pp_token.type != '\n') {
		if (counted_newlines >= 9) {
			if (had_non_space) {
				fputc('\n', out);
			}
			print_line_directive(&pp_token.source_position);
			counted_newlines = 0;
		} else {
			for (unsigned i = 0; i < counted_newlines; ++i)
				fputc('\n', out);
			counted_newlines = 0;
		}
		for (unsigned i = 0; i < counted_spaces; ++i)
			fputc(' ', out);
		counted_spaces = 0;
		had_non_space  = true;
	}

	switch(pp_token.type) {
	case TP_IDENTIFIER:
		fputs(pp_token.v.symbol->string, out);
		break;
	case TP_NUMBER:
		fputs(pp_token.v.string.begin, out);
		break;
	case TP_STRING_LITERAL:
		fputc('"', out);
		fputs(pp_token.v.string.begin, out);
		fputc('"', out);
		break;
	case '\n':
		break;
	default:
		print_pp_token_type(out, pp_token.type);
		break;
	}
}

static void eat_pp(preprocessor_token_type_t type)
{
	(void) type;
	assert(pp_token.type == type);
	next_preprocessing_token();
}

static void eat_pp_directive(void)
{
	while(pp_token.type != '\n' && pp_token.type != TP_EOF) {
		next_preprocessing_token();
	}
}

static bool strings_equal(const string_t *string1, const string_t *string2)
{
	size_t size = string1->size;
	if(size != string2->size)
		return false;

	const char *c1 = string1->begin;
	const char *c2 = string2->begin;
	for(size_t i = 0; i < size; ++i, ++c1, ++c2) {
		if(*c1 != *c2)
			return false;
	}
	return true;
}

static bool wide_strings_equal(const wide_string_t *string1,
                               const wide_string_t *string2)
{
	size_t size = string1->size;
	if(size != string2->size)
		return false;

	const wchar_rep_t *c1 = string1->begin;
	const wchar_rep_t *c2 = string2->begin;
	for(size_t i = 0; i < size; ++i, ++c1, ++c2) {
		if(*c1 != *c2)
			return false;
	}
	return true;
}

static bool pp_tokens_equal(const token_t *token1, const token_t *token2)
{
	if(token1->type != token2->type)
		return false;

	switch(token1->type) {
	case TP_HEADERNAME:
		/* TODO */
		return false;
	case TP_IDENTIFIER:
		return token1->v.symbol == token2->v.symbol;
	case TP_NUMBER:
	case TP_CHARACTER_CONSTANT:
	case TP_STRING_LITERAL:
		return strings_equal(&token1->v.string, &token2->v.string);

	case TP_WIDE_CHARACTER_CONSTANT:
	case TP_WIDE_STRING_LITERAL:
		return wide_strings_equal(&token1->v.wide_string,
		                          &token2->v.wide_string);
	default:
		return true;
	}
}

static bool pp_definitions_equal(const pp_definition_t *definition1,
                                 const pp_definition_t *definition2)
{
	if(definition1->list_len != definition2->list_len)
		return false;

	size_t         len = definition1->list_len;
	const token_t *t1  = definition1->replacement_list;
	const token_t *t2  = definition2->replacement_list;
	for(size_t i = 0; i < len; ++i, ++t1, ++t2) {
		if(!pp_tokens_equal(t1, t2))
			return false;
	}
	return true;
}

static void parse_define_directive(void)
{
	eat_pp(TP_define);

	if(pp_token.type != TP_IDENTIFIER) {
		errorf(&pp_token.source_position,
		       "expected identifier after #define, got '%T'", &pp_token);
		eat_pp_directive();
		return;
	}
	symbol_t *symbol = pp_token.v.symbol;

	pp_definition_t *new_definition
		= obstack_alloc(&pp_obstack, sizeof(new_definition[0]));
	memset(new_definition, 0, sizeof(new_definition[0]));
	new_definition->source_position = input_position;

	/* this is probably the only place where spaces are significant in the
	 * lexer (except for the fact that they separate tokens). #define b(x)
	 * is something else than #define b (x) */
	//token_t *arguments = NULL;
	if(c == '(') {
		next_preprocessing_token();
		while(pp_token.type != ')') {
			if(pp_token.type == TP_DOTDOTDOT) {
				new_definition->is_variadic = true;
				next_preprocessing_token();
				if(pp_token.type != ')') {
					errorf(&input_position,
							"'...' not at end of macro argument list");
					continue;
				}
			} else if(pp_token.type != TP_IDENTIFIER) {
				next_preprocessing_token();
			}
		}
	} else {
		next_preprocessing_token();
	}

	/* construct a new pp_definition on the obstack */
	assert(obstack_object_size(&pp_obstack) == 0);
	size_t list_len = 0;
	while(pp_token.type != '\n' && pp_token.type != TP_EOF) {
		obstack_grow(&pp_obstack, &pp_token, sizeof(pp_token));
		++list_len;
		next_preprocessing_token();
	}

	new_definition->list_len         = list_len;
	new_definition->replacement_list = obstack_finish(&pp_obstack);

	pp_definition_t *old_definition = symbol->pp_definition;
	if(old_definition != NULL) {
		if(!pp_definitions_equal(old_definition, new_definition)) {
			warningf(&input_position, "multiple definition of macro '%Y' (first defined %P)",
	   		         symbol, &old_definition->source_position);
		} else {
			/* reuse the old definition */
			obstack_free(&pp_obstack, new_definition);
			new_definition = old_definition;
		}
	}

	symbol->pp_definition = new_definition;
}

static void parse_undef_directive(void)
{
	eat_pp(TP_undef);

	if(pp_token.type != TP_IDENTIFIER) {
		errorf(&input_position,
		       "expected identifier after #undef, got '%T'", &pp_token);
		eat_pp_directive();
		return;
	}

	symbol_t *symbol = pp_token.v.symbol;
	symbol->pp_definition = NULL;
	next_preprocessing_token();

	if(pp_token.type != '\n') {
		warningf(&input_position, "extra tokens at end of #undef directive");
	}
	/* eat until '\n' */
	eat_pp_directive();
}

static void parse_preprocessing_directive(void)
{
	print_spaces  = false;
	do_expansions = false;
	eat_pp('#');

	switch(pp_token.type) {
	case TP_define:
		parse_define_directive();
		break;
	case TP_undef:
		parse_undef_directive();
		break;
	default:
		errorf(&pp_token.source_position,
		       "invalid preprocessing directive #%T", &pp_token);
		eat_pp_directive();
		break;
	}

	print_spaces  = true;
	do_expansions = true;

	/* eat '\n' */
	assert(pp_token.type == '\n' || pp_token.type == TP_EOF);
	next_preprocessing_token();
}

int pptest_main(int argc, char **argv);

#define GCC_COMPAT_MODE

int pptest_main(int argc, char **argv)
{
	init_symbol_table();
	init_tokens();

	obstack_init(&pp_obstack);

	const char *infname = "t.c";
	if (argc > 1)
		infname = argv[1];

	input = fopen(infname, "r");
	assert(input != NULL);
	input_position.input_name = infname;
	input_position.linenr     = 1;

	bufpos       = NULL;
	bufend       = NULL;
	counted_newlines = 0;
	counted_spaces   = 0;

	out = stdout;

#ifdef GCC_COMPAT_MODE
	/* this is here so we can directly compare "gcc -E" output and our output */
	fprintf(out, "# 1 \"%s\"\n", input_position.input_name);
	fputs("# 1 \"<built-in>\"\n", out);
	fputs("# 1 \"<command-line>\"\n", out);
#endif

	next_char();

	next_preprocessing_token();

	while(true) {
		/* we're at a line begin */
		if(pp_token.type == '#') {
			parse_preprocessing_directive();
		} else {
			/* parse+emit a line */
			while(pp_token.type != '\n') {
				if(pp_token.type == TP_EOF)
					goto end_of_main_loop;
				emit_pp_token();
				next_preprocessing_token();
			}
			emit_pp_token();
			next_preprocessing_token();
		}
	}
end_of_main_loop:

	if (counted_newlines > 0) {
		fputc('\n', out);
	}

	obstack_free(&pp_obstack, NULL);

	exit_tokens();
	exit_symbol_table();

	return 0;
}
