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
#define INCLUDE_LIMIT 199  /* 199 is for gcc "compatibility" */

struct pp_argument_t {
	size_t   list_len;
	token_t *token_list;
};

struct pp_definition_t {
	symbol_t          *symbol;
	source_position_t  source_position;
	pp_definition_t   *parent_expansion;
	size_t             expand_pos;
	bool               is_variadic    : 1;
	bool               is_expanding   : 1;
	bool               has_parameters : 1;
	size_t             n_parameters;
	symbol_t          *parameters;

	/* replacement */
	size_t             list_len;
	token_t           *token_list;

};

typedef struct pp_conditional_t pp_conditional_t;
struct pp_conditional_t {
	source_position_t  source_position;
	bool               condition;
	bool               in_else;
	bool               skip; /**< conditional in skip mode (then+else gets skipped) */
	pp_conditional_t  *parent;
};

typedef struct pp_input_t pp_input_t;
struct pp_input_t {
	FILE              *file;
	int                c;
	char               buf[1024+MAX_PUTBACK];
	const char        *bufend;
	const char        *bufpos;
	source_position_t  position;
	bool               had_non_space;
	pp_input_t        *parent;
};

pp_input_t input;
#define CC input.c

static pp_input_t     *input_stack;
static unsigned        n_inputs;
static struct obstack  input_obstack;

static pp_conditional_t *conditional_stack;

token_t                   pp_token;
static bool               resolve_escape_sequences = false;
static bool               do_print_spaces          = true;
static bool               do_expansions;
static bool               skip_mode;
static FILE              *out;
static struct obstack     pp_obstack;
static unsigned           counted_newlines;
static unsigned           counted_spaces;
static const char        *printed_input_name = NULL;
static pp_definition_t   *current_expansion  = NULL;

static inline void next_char(void);
static void next_preprocessing_token(void);
static void print_line_directive(const source_position_t *pos, const char *add);
static void print_spaces(void);

static bool open_input(const char *filename)
{
	FILE *file = fopen(filename, "r");
	if (file == NULL)
		return false;

	input.file                = file;
	input.bufend              = NULL;
	input.bufpos              = NULL;
	input.had_non_space       = false;
	input.position.input_name = filename;
	input.position.linenr     = 1;

	/* indicate that we're at a new input */
	print_line_directive(&input.position, input_stack != NULL ? "1" : NULL);

	counted_newlines = 0;
	counted_spaces   = 0;

	/* read first char and first token */
	next_char();
	next_preprocessing_token();

	return true;
}

static void close_input(void)
{
	/* ensure we have a newline at EOF */
	if (input.had_non_space) {
		fputc('\n', out);
	}

	assert(input.file != NULL);

	fclose(input.file);
	input.file   = NULL;
	input.bufend = NULL;
	input.bufpos = NULL;
	input.c      = EOF;
}

static void push_input(void)
{
	pp_input_t *saved_input
		= obstack_alloc(&input_obstack, sizeof(*saved_input));

	memcpy(saved_input, &input, sizeof(*saved_input));

	/* adjust buffer positions */
	if (input.bufpos != NULL)
		saved_input->bufpos = saved_input->buf + (input.bufpos - input.buf);
	if (input.bufend != NULL)
		saved_input->bufend = saved_input->buf + (input.bufend - input.buf);

	saved_input->parent = input_stack;
	input_stack         = saved_input;
	++n_inputs;
}

static void pop_restore_input(void)
{
	assert(n_inputs > 0);
	assert(input_stack != NULL);

	pp_input_t *saved_input = input_stack;

	memcpy(&input, saved_input, sizeof(input));
	input.parent = NULL;

	/* adjust buffer positions */
	if (saved_input->bufpos != NULL)
		input.bufpos = input.buf + (saved_input->bufpos - saved_input->buf);
	if (saved_input->bufend != NULL)
		input.bufend = input.buf + (saved_input->bufend - saved_input->buf);

	input_stack = saved_input->parent;
	obstack_free(&input_obstack, saved_input);
	--n_inputs;
}

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
	assert(input.bufpos <= input.bufend);
	if (input.bufpos >= input.bufend) {
		size_t s = fread(input.buf + MAX_PUTBACK, 1,
		                 sizeof(input.buf) - MAX_PUTBACK, input.file);
		if (s == 0) {
			CC = EOF;
			return;
		}
		input.bufpos = input.buf + MAX_PUTBACK;
		input.bufend = input.buf + MAX_PUTBACK + s;
	}
	CC = *input.bufpos++;
}

/**
 * Put a character back into the buffer.
 *
 * @param pc  the character to put back
 */
static inline void put_back(int pc)
{
	assert(input.bufpos > input.buf);
	*(--input.bufpos - input.buf + input.buf) = (char) pc;

#ifdef DEBUG_CHARS
	printf("putback '%c'\n", pc);
#endif
}

#define MATCH_NEWLINE(code)                   \
	case '\r':                                \
		next_char();                          \
		if(CC == '\n') {                      \
			next_char();                      \
		}                                     \
		++input.position.linenr;              \
		code                                  \
	case '\n':                                \
		next_char();                          \
		++input.position.linenr;              \
		code

#define eat(c_type)  do { assert(CC == c_type); next_char(); } while(0)

static void maybe_concat_lines(void)
{
	eat('\\');

	switch(CC) {
	MATCH_NEWLINE(return;)

	default:
		break;
	}

	put_back(CC);
	CC = '\\';
}

/**
 * Set c to the next input character, ie.
 * after expanding trigraphs.
 */
static inline void next_char(void)
{
	next_real_char();

	/* filter trigraphs and concatenated lines */
	if(UNLIKELY(CC == '\\')) {
		maybe_concat_lines();
		goto end_of_next_char;
	}

	if(LIKELY(CC != '?'))
		goto end_of_next_char;

	next_real_char();
	if(LIKELY(CC != '?')) {
		put_back(CC);
		CC = '?';
		goto end_of_next_char;
	}

	next_real_char();
	switch(CC) {
	case '=': CC = '#'; break;
	case '(': CC = '['; break;
	case '/': CC = '\\'; maybe_concat_lines(); break;
	case ')': CC = ']'; break;
	case '\'': CC = '^'; break;
	case '<': CC = '{'; break;
	case '!': CC = '|'; break;
	case '>': CC = '}'; break;
	case '-': CC = '~'; break;
	default:
		put_back(CC);
		put_back('?');
		CC = '?';
		break;
	}

end_of_next_char:;
#ifdef DEBUG_CHARS
	printf("nchar '%c'\n", CC);
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
	if (!is_octal_digit(CC)) return value;
	value = 8 * value + digit_value(CC);
	next_char();
	if (!is_octal_digit(CC)) return value;
	value = 8 * value + digit_value(CC);
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
	while(isxdigit(CC)) {
		value = 16 * value + digit_value(CC);
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

	int ec = CC;
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
	const unsigned start_linenr = input.position.linenr;

	eat('"');

	int tc;
	while(1) {
		switch(CC) {
		case '\\':
			if(resolve_escape_sequences) {
				tc = parse_escape_sequence();
				obstack_1grow(&symbol_obstack, (char) tc);
			} else {
				obstack_1grow(&symbol_obstack, (char) CC);
				next_char();
				obstack_1grow(&symbol_obstack, (char) CC);
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
			obstack_1grow(&symbol_obstack, (char) CC);
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
		switch(CC) {
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
				found_char = CC;
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
	const unsigned start_linenr = input.position.linenr;

	assert(CC == '"');
	next_char();

	while(1) {
		switch(CC) {
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
			wchar_rep_t tc = CC;
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
	const unsigned start_linenr = input.position.linenr;

	eat('\'');

	int tc;
	while(1) {
		switch(CC) {
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
			obstack_1grow(&symbol_obstack, (char) CC);
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

		/* it was the outermost expansion, parse normal pptoken */
		if(parent == NULL) {
			current_expansion = NULL;
			next_preprocessing_token();
			return;
		}
		definition        = parent;
		current_expansion = definition;
		goto restart;
	}
	pp_token = definition->token_list[definition->expand_pos];
	++definition->expand_pos;

	if(pp_token.type != TP_IDENTIFIER)
		return;

	/* if it was an identifier then we might need to expand again */
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

static void skip_line_comment(void)
{
	if(do_print_spaces)
		counted_spaces++;

	while(1) {
		switch(CC) {
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

static void skip_multiline_comment(void)
{
	if(do_print_spaces)
		counted_spaces++;

	unsigned start_linenr = input.position.linenr;
	while(1) {
		switch(CC) {
		case '/':
			next_char();
			if (CC == '*') {
				/* TODO: nested comment, warn here */
			}
			break;
		case '*':
			next_char();
			if(CC == '/') {
				next_char();
				return;
			}
			break;

		MATCH_NEWLINE(
			if(do_print_spaces) {
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

/* skip spaces advancing at the start of the next preprocessing token */
static void skip_spaces(bool skip_newline)
{
	while (true) {
		switch (CC) {
		case ' ':
		case '\t':
	 		if(do_print_spaces)
				counted_spaces++;
	 		next_char();
			continue;
		case '/':
			next_char();
			if (CC == '/') {
				next_char();
				skip_line_comment();
				continue;
			} else if (CC == '*') {
				next_char();
				skip_multiline_comment();
				continue;
			} else {
				put_back(CC);
				CC = '/';
			}
			return;

		case '\r':
			if (!skip_newline)
				return;

			next_char();
			if(CC == '\n') {
				next_char();
			}
			++input.position.linenr;
			if (do_print_spaces)
				++counted_newlines;
			continue;

		case '\n':
			if (!skip_newline)
				return;

			next_char();
			++input.position.linenr;
			if (do_print_spaces)
				++counted_newlines;
			continue;

		default:
			return;
		}
	}
}

static void eat_pp(int type)
{
	(void) type;
	assert(pp_token.type == type);
	next_preprocessing_token();
}

static void parse_symbol(void)
{
	obstack_1grow(&symbol_obstack, (char) CC);
	next_char();

	while(1) {
		switch(CC) {
		DIGITS
		SYMBOL_CHARS
			obstack_1grow(&symbol_obstack, (char) CC);
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
	if (CC == '"' && string[0] == 'L' && string[1] == '\0') {
		obstack_free(&symbol_obstack, string);
		parse_wide_string_literal();
		return;
	} else if (CC == '\'' && string[0] == 'L' && string[1] == '\0') {
		obstack_free(&symbol_obstack, string);
		parse_wide_character_constant();
		return;
	}

	symbol_t *symbol = symbol_table_insert(string);

	pp_token.type     = symbol->pp_ID;
	pp_token.v.symbol = symbol;

	/* we can free the memory from symbol obstack if we already had an entry in
	 * the symbol table */
	if (symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}
	if (!do_expansions)
		return;

	pp_definition_t *pp_definition = symbol->pp_definition;
	if (pp_definition == NULL)
		return;

	if (pp_definition->has_parameters) {
		skip_spaces(true);
		/* no opening brace -> no expansion */
		if (CC != '(')
			return;
		next_preprocessing_token();
		eat_pp('(');

		/* parse arguments (TODO) */
		while (pp_token.type != TP_EOF && pp_token.type != ')')
			next_preprocessing_token();
		next_preprocessing_token();
	}

	pp_definition->expand_pos   = 0;
	pp_definition->is_expanding = true,
	current_expansion           = pp_definition;
	expand_next();
}

static void parse_number(void)
{
	obstack_1grow(&symbol_obstack, (char) CC);
	next_char();

	while(1) {
		switch(CC) {
		case '.':
		DIGITS
		SYMBOL_CHARS_WITHOUT_E_P
			obstack_1grow(&symbol_obstack, (char) CC);
			next_char();
			break;

		case 'e':
		case 'p':
		case 'E':
		case 'P':
			obstack_1grow(&symbol_obstack, (char) CC);
			next_char();
			if(CC == '+' || CC == '-') {
				obstack_1grow(&symbol_obstack, (char) CC);
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



#define MAYBE_PROLOG                                       \
			next_char();                                   \
			while(1) {                                     \
				switch(CC) {

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

	pp_token.source_position = input.position;

restart:
	switch(CC) {
	case ' ':
	case '\t':
 		if(do_print_spaces)
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
				put_back(CC);
				CC = '.';
				parse_number();
				return;

			case '.':
				MAYBE_PROLOG
				MAYBE('.', TP_DOTDOTDOT)
				ELSE_CODE(
					put_back(CC);
					CC = '.';
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
				goto restart;
			case '/':
				next_char();
				skip_line_comment();
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
							put_back(CC);
							CC = '%';
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
		pp_token.type = CC;
		next_char();
		return;

	case EOF:
		if (input_stack != NULL) {
			close_input();
			pop_restore_input();
			counted_newlines = 0;
			counted_spaces   = 0;
			/* hack to output correct line number */
			print_line_directive(&input.position, "2");
			next_preprocessing_token();
		} else {
			pp_token.type = TP_EOF;
		}
		return;

	default:
		next_char();
		errorf(&pp_token.source_position, "unknown character '%c' found\n", CC);
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

static void print_line_directive(const source_position_t *pos, const char *add)
{
	fprintf(out, "# %d ", pos->linenr);
	print_quoted_string(pos->input_name);
	if (add != NULL) {
		fputc(' ', out);
		fputs(add, out);
	}
	fputc('\n', out);

	printed_input_name = pos->input_name;
}

static void print_spaces(void)
{
	if (counted_newlines >= 9) {
		if (input.had_non_space) {
			fputc('\n', out);
		}
		print_line_directive(&pp_token.source_position, NULL);
		counted_newlines = 0;
	} else {
		for (unsigned i = 0; i < counted_newlines; ++i)
			fputc('\n', out);
		counted_newlines = 0;
	}
	for (unsigned i = 0; i < counted_spaces; ++i)
		fputc(' ', out);
	counted_spaces = 0;
}

static void emit_pp_token(void)
{
	if (skip_mode)
		return;

	if (pp_token.type != '\n') {
		print_spaces();
		input.had_non_space = true;
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
	const token_t *t1  = definition1->token_list;
	const token_t *t2  = definition2->token_list;
	for(size_t i = 0; i < len; ++i, ++t1, ++t2) {
		if(!pp_tokens_equal(t1, t2))
			return false;
	}
	return true;
}

static void parse_define_directive(void)
{
	eat_pp(TP_define);
	assert(obstack_object_size(&pp_obstack) == 0);

	if (pp_token.type != TP_IDENTIFIER) {
		errorf(&pp_token.source_position,
		       "expected identifier after #define, got '%t'", &pp_token);
		goto error_out;
	}
	symbol_t *symbol = pp_token.v.symbol;

	pp_definition_t *new_definition
		= obstack_alloc(&pp_obstack, sizeof(new_definition[0]));
	memset(new_definition, 0, sizeof(new_definition[0]));
	new_definition->source_position = input.position;

	/* this is probably the only place where spaces are significant in the
	 * lexer (except for the fact that they separate tokens). #define b(x)
	 * is something else than #define b (x) */
	if (CC == '(') {
		/* eat the '(' */
		next_preprocessing_token();
		/* get next token after '(' */
		next_preprocessing_token();

		while (true) {
			switch (pp_token.type) {
			case TP_DOTDOTDOT:
				new_definition->is_variadic = true;
				next_preprocessing_token();
				if (pp_token.type != ')') {
					errorf(&input.position,
							"'...' not at end of macro argument list");
					goto error_out;
				}
				break;
			case TP_IDENTIFIER:
				obstack_ptr_grow(&pp_obstack, pp_token.v.symbol);
				next_preprocessing_token();

				if (pp_token.type == ',') {
					next_preprocessing_token();
					break;
				}

				if (pp_token.type != ')') {
					errorf(&pp_token.source_position,
					       "expected ',' or ')' after identifier, got '%t'",
					       &pp_token);
					goto error_out;
				}
				break;
			case ')':
				next_preprocessing_token();
				goto finish_argument_list;
			default:
				errorf(&pp_token.source_position,
				       "expected identifier, '...' or ')' in #define argument list, got '%t'",
				       &pp_token);
				goto error_out;
			}
		}

	finish_argument_list:
		new_definition->has_parameters = true;
		new_definition->n_parameters
			= obstack_object_size(&pp_obstack) / sizeof(new_definition->parameters[0]);
		new_definition->parameters = obstack_finish(&pp_obstack);
	} else {
		next_preprocessing_token();
	}

	/* construct a new pp_definition on the obstack */
	assert(obstack_object_size(&pp_obstack) == 0);
	size_t list_len = 0;
	while (pp_token.type != '\n' && pp_token.type != TP_EOF) {
		obstack_grow(&pp_obstack, &pp_token, sizeof(pp_token));
		++list_len;
		next_preprocessing_token();
	}

	new_definition->list_len   = list_len;
	new_definition->token_list = obstack_finish(&pp_obstack);

	pp_definition_t *old_definition = symbol->pp_definition;
	if (old_definition != NULL) {
		if (!pp_definitions_equal(old_definition, new_definition)) {
			warningf(&input.position, "multiple definition of macro '%Y' (first defined %P)",
	   		         symbol, &old_definition->source_position);
		} else {
			/* reuse the old definition */
			obstack_free(&pp_obstack, new_definition);
			new_definition = old_definition;
		}
	}

	symbol->pp_definition = new_definition;
	return;

error_out:
	if (obstack_object_size(&pp_obstack) > 0) {
		char *ptr = obstack_finish(&pp_obstack);
		obstack_free(&pp_obstack, ptr);
	}
	eat_pp_directive();
}

static void parse_undef_directive(void)
{
	eat_pp(TP_undef);

	if(pp_token.type != TP_IDENTIFIER) {
		errorf(&input.position,
		       "expected identifier after #undef, got '%t'", &pp_token);
		eat_pp_directive();
		return;
	}

	symbol_t *symbol = pp_token.v.symbol;
	symbol->pp_definition = NULL;
	next_preprocessing_token();

	if(pp_token.type != '\n') {
		warningf(&input.position, "extra tokens at end of #undef directive");
	}
	/* eat until '\n' */
	eat_pp_directive();
}

static const char *parse_headername(void)
{
	/* behind an #include we can have the special headername lexems.
	 * They're only allowed behind an #include so they're not recognized
	 * by the normal next_preprocessing_token. We handle them as a special
	 * exception here */

	/* skip spaces so we reach start of next preprocessing token */
	skip_spaces(false);

	assert(obstack_object_size(&input_obstack) == 0);

	/* check wether we have a "... or <... headername */
	switch (CC) {
	case '<':
		/* for now until we have proper searchpath handling */
		obstack_1grow(&input_obstack, '.');
		obstack_1grow(&input_obstack, '/');

		next_char();
		while (true) {
			switch (CC) {
			case EOF:
				/* fallthrough */
			MATCH_NEWLINE(
				parse_error("header name without closing '>'");
				return NULL;
			)
			case '>':
				next_char();
				goto finished_headername;
			}
			obstack_1grow(&input_obstack, (char) CC);
			next_char();
		}
		/* we should never be here */

	case '"':
		/* for now until we have proper searchpath handling */
		obstack_1grow(&input_obstack, '.');
		obstack_1grow(&input_obstack, '/');

		next_char();
		while (true) {
			switch (CC) {
			case EOF:
				/* fallthrough */
			MATCH_NEWLINE(
				parse_error("header name without closing '>'");
				return NULL;
			)
			case '"':
				next_char();
				goto finished_headername;
			}
			obstack_1grow(&input_obstack, (char) CC);
			next_char();
		}
		/* we should never be here */

	default:
		/* TODO: do normale pp_token parsing and concatenate results */
		panic("pp_token concat include not implemented yet");
	}

finished_headername:
	obstack_1grow(&input_obstack, '\0');
	char *headername = obstack_finish(&input_obstack);

	/* TODO: iterate search-path to find the file */

	next_preprocessing_token();

	return headername;
}

static bool parse_include_directive(void)
{
	/* don't eat the TP_include here!
	 * we need an alternative parsing for the next token */

	print_spaces();

	const char *headername = parse_headername();
	if (headername == NULL) {
		eat_pp_directive();
		return false;
	}

	if (pp_token.type != '\n' && pp_token.type != TP_EOF) {
		warningf(&pp_token.source_position,
		         "extra tokens at end of #include directive");
		eat_pp_directive();
	}

	if (n_inputs > INCLUDE_LIMIT) {
		errorf(&pp_token.source_position, "#include nested too deeply");
		/* eat \n or EOF */
		next_preprocessing_token();
		return false;
	}

	/* we have to reenable space counting and macro expansion here,
	 * because it is still disabled in directive parsing,
	 * but we will trigger a preprocessing token reading of the new file
	 * now and need expansions/space counting */
	do_print_spaces = true;
	do_expansions   = true;

	/* switch inputs */
	push_input();
	bool res = open_input(headername);
	if (!res) {
		errorf(&pp_token.source_position,
		       "failed including '%s': %s", headername, strerror(errno));
		pop_restore_input();
		return false;
	}

	return true;
}

static pp_conditional_t *push_conditional(void)
{
	pp_conditional_t *conditional
		= obstack_alloc(&pp_obstack, sizeof(*conditional));
	memset(conditional, 0, sizeof(*conditional));

	conditional->parent = conditional_stack;
	conditional_stack   = conditional;

	return conditional;
}

static void pop_conditional(void)
{
	assert(conditional_stack != NULL);
	conditional_stack = conditional_stack->parent;
}

static void check_unclosed_conditionals(void)
{
	while (conditional_stack != NULL) {
		pp_conditional_t *conditional = conditional_stack;

		if (conditional->in_else) {
			errorf(&conditional->source_position, "unterminated #else");
		} else {
			errorf(&conditional->source_position, "unterminated condition");
		}
		pop_conditional();
	}
}

static void parse_ifdef_ifndef_directive(void)
{
	bool is_ifndef = (pp_token.type == TP_ifndef);
	bool condition;
	next_preprocessing_token();

	if (skip_mode) {
		eat_pp_directive();
		pp_conditional_t *conditional = push_conditional();
		conditional->source_position  = pp_token.source_position;
		conditional->skip             = true;
		return;
	}

	if (pp_token.type != TP_IDENTIFIER) {
		errorf(&pp_token.source_position,
		       "expected identifier after #%s, got '%t'",
		       is_ifndef ? "ifndef" : "ifdef", &pp_token);
		eat_pp_directive();

		/* just take the true case in the hope to avoid further errors */
		condition = true;
	} else {
		symbol_t        *symbol        = pp_token.v.symbol;
		pp_definition_t *pp_definition = symbol->pp_definition;
		next_preprocessing_token();

		if (pp_token.type != '\n') {
			errorf(&pp_token.source_position,
			       "extra tokens at end of #%s",
			       is_ifndef ? "ifndef" : "ifdef");
			eat_pp_directive();
		}

		/* evaluate wether we are in true or false case */
		condition = is_ifndef ? pp_definition == NULL : pp_definition != NULL;
	}

	pp_conditional_t *conditional = push_conditional();
	conditional->source_position  = pp_token.source_position;
	conditional->condition        = condition;

	if (!condition) {
		skip_mode = true;
	}
}

static void parse_else_directive(void)
{
	eat_pp(TP_else);

	if (pp_token.type != '\n') {
		if (!skip_mode) {
			warningf(&pp_token.source_position, "extra tokens at end of #else");
		}
		eat_pp_directive();
	}

	pp_conditional_t *conditional = conditional_stack;
	if (conditional == NULL) {
		errorf(&pp_token.source_position, "#else without prior #if");
		return;
	}

	if (conditional->in_else) {
		errorf(&pp_token.source_position,
		       "#else after #else (condition started %P)",
		       conditional->source_position);
		skip_mode = true;
		return;
	}

	conditional->in_else = true;
	if (!conditional->skip) {
		skip_mode = conditional->condition;
	}
	conditional->source_position = pp_token.source_position;
}

static void parse_endif_directive(void)
{
	eat_pp(TP_endif);

	if (pp_token.type != '\n') {
		if (!skip_mode) {
			warningf(&pp_token.source_position,
			         "extra tokens at end of #endif");
		}
		eat_pp_directive();
	}

	pp_conditional_t *conditional = conditional_stack;
	if (conditional == NULL) {
		errorf(&pp_token.source_position, "#endif without prior #if");
		return;
	}

	if (!conditional->skip) {
		skip_mode = false;
	}
	pop_conditional();
}

static void parse_preprocessing_directive(void)
{
	do_print_spaces = false;
	do_expansions   = false;
	eat_pp('#');

	if (skip_mode) {
		switch(pp_token.type) {
		case TP_ifdef:
		case TP_ifndef:
			parse_ifdef_ifndef_directive();
			break;
		case TP_else:
			parse_else_directive();
			break;
		case TP_endif:
			parse_endif_directive();
			break;
		default:
			eat_pp_directive();
			break;
		}
	} else {
		switch(pp_token.type) {
		case TP_define:
			parse_define_directive();
			break;
		case TP_undef:
			parse_undef_directive();
			break;
		case TP_ifdef:
		case TP_ifndef:
			parse_ifdef_ifndef_directive();
			break;
		case TP_else:
			parse_else_directive();
			break;
		case TP_endif:
			parse_endif_directive();
			break;
		case TP_include: {
			bool in_new_source = parse_include_directive();
			/* no need to do anything if source file switched */
			if (in_new_source)
				return;
			break;
		}
		case '\n':
			/* the nop directive */
			break;
		default:
			errorf(&pp_token.source_position,
				   "invalid preprocessing directive #%t", &pp_token);
			eat_pp_directive();
			break;
		}
	}

	do_print_spaces = true;
	do_expansions   = true;

	/* eat '\n' */
	assert(pp_token.type == '\n' || pp_token.type == TP_EOF);
	next_preprocessing_token();
}

#define GCC_COMPAT_MODE

int pptest_main(int argc, char **argv);
int pptest_main(int argc, char **argv)
{
	init_symbol_table();
	init_tokens();

	obstack_init(&pp_obstack);
	obstack_init(&input_obstack);

	const char *filename = "t.c";
	if (argc > 1)
		filename = argv[1];

	out = stdout;

#ifdef GCC_COMPAT_MODE
	/* this is here so we can directly compare "gcc -E" output and our output */
	fprintf(out, "# 1 \"%s\"\n", filename);
	fputs("# 1 \"<built-in>\"\n", out);
	fputs("# 1 \"<command-line>\"\n", out);
#endif

	bool ok = open_input(filename);
	assert(ok);

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

	check_unclosed_conditionals();
	close_input();

	obstack_free(&input_obstack, NULL);
	obstack_free(&pp_obstack, NULL);

	exit_tokens();
	exit_symbol_table();

	return 0;
}
