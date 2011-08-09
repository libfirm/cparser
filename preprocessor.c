#include <config.h>

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#include "token_t.h"
#include "symbol_t.h"
#include "adt/util.h"
#include "adt/error.h"
#include "adt/strutil.h"
#include "adt/strset.h"
#include "lang_features.h"
#include "diagnostic.h"
#include "string_rep.h"
#include "input.h"

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
	input_t           *input;
	utf32              c;
	utf32              buf[1024+MAX_PUTBACK];
	const utf32       *bufend;
	const utf32       *bufpos;
	source_position_t  position;
	pp_input_t        *parent;
	unsigned           output_line;
};

/** additional info about the current token */
typedef struct add_token_info_t {
	/** whitespace from beginning of line to the token */
	unsigned whitespace;
	/** there has been any whitespace before the token */
	bool     had_whitespace;
	/** the token is at the beginning of the line */
	bool     at_line_begin;
} add_token_info_t;

typedef struct searchpath_entry_t searchpath_entry_t;
struct searchpath_entry_t {
	const char         *path;
	searchpath_entry_t *next;
};

static pp_input_t      input;

static pp_input_t     *input_stack;
static unsigned        n_inputs;
static struct obstack  input_obstack;

static pp_conditional_t *conditional_stack;

static token_t           pp_token;
static bool              resolve_escape_sequences = false;
static bool              ignore_unknown_chars     = true;
static bool              in_pp_directive;
static bool              skip_mode;
static FILE             *out;
static struct obstack    pp_obstack;
static struct obstack    config_obstack;
static const char       *printed_input_name = NULL;
static source_position_t expansion_pos;
static pp_definition_t  *current_expansion  = NULL;
static strset_t          stringset;
static preprocessor_token_kind_t last_token = TP_ERROR;

static searchpath_entry_t *searchpath;

static add_token_info_t  info;

static inline void next_char(void);
static void next_preprocessing_token(void);
static void print_line_directive(const source_position_t *pos, const char *add);

static void switch_input(FILE *file, const char *filename)
{
	input.file                = file;
	input.input               = input_from_stream(file, NULL);
	input.bufend              = NULL;
	input.bufpos              = NULL;
	input.output_line         = 0;
	input.position.input_name = filename;
	input.position.lineno     = 1;

	/* indicate that we're at a new input */
	print_line_directive(&input.position, input_stack != NULL ? "1" : NULL);

	/* place a virtual '\n' so we realize we're at line begin */
	input.position.lineno = 0;
	input.c               = '\n';
	next_preprocessing_token();
}

static void close_input(void)
{
	input_free(input.input);
	assert(input.file != NULL);

	fclose(input.file);
	input.input  = NULL;
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
	errorf(&pp_token.base.source_position,  "%s", msg);
}

static inline void next_real_char(void)
{
	assert(input.bufpos <= input.bufend);
	if (input.bufpos >= input.bufend) {
		size_t n = decode(input.input, input.buf + MAX_PUTBACK,
		                  sizeof(input.buf)/sizeof(input.buf[0]) - MAX_PUTBACK);
		if (n == 0) {
			input.c = EOF;
			return;
		}
		input.bufpos = input.buf + MAX_PUTBACK;
		input.bufend = input.bufpos + n;
	}
	input.c = *input.bufpos++;
	++input.position.colno;
}

/**
 * Put a character back into the buffer.
 *
 * @param pc  the character to put back
 */
static inline void put_back(utf32 const pc)
{
	assert(input.bufpos > input.buf);
	*(--input.bufpos - input.buf + input.buf) = (char) pc;
	--input.position.colno;
}

#define MATCH_NEWLINE(code)                   \
	case '\r':                                \
		next_char();                          \
		if (input.c == '\n') {                \
	case '\n':                                \
			next_char();                      \
		}                                     \
		info.whitespace = 0;                  \
		++input.position.lineno;              \
		input.position.colno = 1;             \
		code

#define eat(c_type) (assert(input.c == c_type), next_char())

static void maybe_concat_lines(void)
{
	eat('\\');

	switch (input.c) {
	MATCH_NEWLINE(
		return;
	)

	default:
		break;
	}

	put_back(input.c);
	input.c = '\\';
}

/**
 * Set c to the next input character, ie.
 * after expanding trigraphs.
 */
static inline void next_char(void)
{
	next_real_char();

	/* filter trigraphs and concatenated lines */
	if (UNLIKELY(input.c == '\\')) {
		maybe_concat_lines();
		goto end_of_next_char;
	}

	if (LIKELY(input.c != '?'))
		goto end_of_next_char;

	next_real_char();
	if (LIKELY(input.c != '?')) {
		put_back(input.c);
		input.c = '?';
		goto end_of_next_char;
	}

	next_real_char();
	switch (input.c) {
	case '=': input.c = '#'; break;
	case '(': input.c = '['; break;
	case '/': input.c = '\\'; maybe_concat_lines(); break;
	case ')': input.c = ']'; break;
	case '\'': input.c = '^'; break;
	case '<': input.c = '{'; break;
	case '!': input.c = '|'; break;
	case '>': input.c = '}'; break;
	case '-': input.c = '~'; break;
	default:
		put_back(input.c);
		put_back('?');
		input.c = '?';
		break;
	}

end_of_next_char:;
#ifdef DEBUG_CHARS
	printf("nchar '%c'\n", input.c);
#endif
}



/**
 * Returns true if the given char is a octal digit.
 *
 * @param char  the character to check
 */
static inline bool is_octal_digit(int chr)
{
	switch (chr) {
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
static int digit_value(int digit)
{
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
static utf32 parse_octal_sequence(const utf32 first_digit)
{
	assert(is_octal_digit(first_digit));
	utf32 value = digit_value(first_digit);
	if (!is_octal_digit(input.c)) return value;
	value = 8 * value + digit_value(input.c);
	next_char();
	if (!is_octal_digit(input.c)) return value;
	value = 8 * value + digit_value(input.c);
	next_char();
	return value;

}

/**
 * Parses a hex character sequence.
 */
static utf32 parse_hex_sequence(void)
{
	utf32 value = 0;
	while (isxdigit(input.c)) {
		value = 16 * value + digit_value(input.c);
		next_char();
	}
	return value;
}

/**
 * Parse an escape sequence.
 */
static utf32 parse_escape_sequence(void)
{
	eat('\\');

	utf32 const ec = input.c;
	next_char();

	switch (ec) {
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
	/* \E is not documented, but handled, by GCC.  It is acceptable according
	 * to §6.11.4, whereas \e is not. */
	case 'E':
	case 'e':
		if (c_mode & _GNUC)
			return 27;   /* hopefully 27 is ALWAYS the code for ESCAPE */
		break;
	case 'u':
	case 'U':
		parse_error("universal character parsing not implemented yet");
		return EOF;
	default:
		break;
	}
	/* §6.4.4.4:8 footnote 64 */
	parse_error("unknown escape sequence");
	return EOF;
}

static const char *identify_string(char *string)
{
	const char *result = strset_insert(&stringset, string);
	if (result != string) {
		obstack_free(&symbol_obstack, string);
	}
	return result;
}

static string_t make_string(char *string, size_t len)
{
	const char *result = identify_string(string);
	return (string_t) {result, len};
}

static void parse_string_literal(void)
{
	const unsigned start_linenr = input.position.lineno;

	eat('"');

	while (true) {
		switch (input.c) {
		case '\\': {
			utf32 tc;
			if (resolve_escape_sequences) {
				tc = parse_escape_sequence();
				obstack_1grow(&symbol_obstack, (char) tc);
			} else {
				obstack_1grow(&symbol_obstack, (char) input.c);
				next_char();
				obstack_1grow(&symbol_obstack, (char) input.c);
				next_char();
			}
			break;
		}

		case EOF: {
			source_position_t source_position;
			source_position.input_name = pp_token.base.source_position.input_name;
			source_position.lineno     = start_linenr;
			errorf(&source_position, "string has no end");
			pp_token.kind = TP_ERROR;
			return;
		}

		case '"':
			next_char();
			goto end_of_string;

		default:
			obstack_grow_symbol(&symbol_obstack, input.c);
			next_char();
			break;
		}
	}

end_of_string:
	/* add finishing 0 to the string */
	obstack_1grow(&symbol_obstack, '\0');
	const size_t size   = (size_t)obstack_object_size(&symbol_obstack);
	char *const  string = obstack_finish(&symbol_obstack);

	pp_token.kind          = TP_STRING_LITERAL;
	pp_token.string.string = make_string(string, size);
}

/**
 * Parse a wide string literal and set lexer_token.
 */
static void parse_wide_string_literal(void)
{
	parse_string_literal();
	if (pp_token.kind == TP_STRING_LITERAL)
		pp_token.kind = TP_WIDE_STRING_LITERAL;
}

static void parse_wide_character_constant(void)
{
	eat('\'');

	while (true) {
		switch (input.c) {
		case '\\': {
			const utf32 tc = parse_escape_sequence();
			obstack_grow_symbol(&symbol_obstack, tc);
			break;
		}

		MATCH_NEWLINE(
			parse_error("newline while parsing character constant");
			break;
		)

		case '\'':
			next_char();
			goto end_of_wide_char_constant;

		case EOF:
			parse_error("EOF while parsing character constant");
			pp_token.kind = TP_ERROR;
			return;

		default:
			obstack_grow_symbol(&symbol_obstack, input.c);
			next_char();
			break;
		}
	}

end_of_wide_char_constant:
	obstack_1grow(&symbol_obstack, '\0');
	size_t  size = (size_t) obstack_object_size(&symbol_obstack)-1;
	char   *string = obstack_finish(&symbol_obstack);
	pp_token.kind          = TP_WIDE_CHARACTER_CONSTANT;
	pp_token.string.string = make_string(string, size);

	if (size == 0) {
		parse_error("empty character constant");
	}
}

static void parse_character_constant(void)
{
	const unsigned start_linenr = input.position.lineno;

	eat('\'');

	int tc;
	while (true) {
		switch (input.c) {
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
			source_position.input_name = pp_token.base.source_position.input_name;
			source_position.lineno     = start_linenr;
			errorf(&source_position, "EOF while parsing character constant");
			pp_token.kind = TP_ERROR;
			return;
		}

		case '\'':
			next_char();
			goto end_of_char_constant;

		default:
			obstack_1grow(&symbol_obstack, (char) input.c);
			next_char();
			break;

		}
	}

end_of_char_constant:;
	obstack_1grow(&symbol_obstack, '\0');
	const size_t size   = (size_t)obstack_object_size(&symbol_obstack);
	char *const  string = obstack_finish(&symbol_obstack);

	pp_token.kind          = TP_CHARACTER_CONSTANT;
	pp_token.string.string = make_string(string, size);

	if (size == 0) {
		parse_error("empty character constant");
	}
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
	if (definition->list_len == 0
			|| definition->expand_pos >= definition->list_len) {
		/* we're finished with the current macro, move up 1 level in the
		 * expansion stack */
		pp_definition_t *parent = definition->parent_expansion;
		definition->parent_expansion = NULL;
		definition->is_expanding     = false;

		/* it was the outermost expansion, parse normal pptoken */
		if (parent == NULL) {
			current_expansion = NULL;
			next_preprocessing_token();
			return;
		}
		definition        = parent;
		current_expansion = definition;
		goto restart;
	}
	pp_token = definition->token_list[definition->expand_pos];
	pp_token.base.source_position = expansion_pos;
	++definition->expand_pos;

	if (pp_token.kind != TP_IDENTIFIER)
		return;

	/* if it was an identifier then we might need to expand again */
	pp_definition_t *symbol_definition = pp_token.identifier.symbol->pp_definition;
	if (symbol_definition != NULL && !symbol_definition->is_expanding) {
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
	while (true) {
		switch (input.c) {
		case EOF:
			return;

		case '\r':
		case '\n':
			return;

		default:
			next_char();
			break;
		}
	}
}

static void skip_multiline_comment(void)
{
	unsigned start_linenr = input.position.lineno;
	while (true) {
		switch (input.c) {
		case '/':
			next_char();
			if (input.c == '*') {
				/* TODO: nested comment, warn here */
			}
			break;
		case '*':
			next_char();
			if (input.c == '/') {
				next_char();
				info.whitespace += input.position.colno-1;
				return;
			}
			break;

		MATCH_NEWLINE(
			info.at_line_begin |= !in_pp_directive;
			break;
		)

		case EOF: {
			source_position_t source_position;
			source_position.input_name = pp_token.base.source_position.input_name;
			source_position.lineno     = start_linenr;
			errorf(&source_position, "at end of file while looking for comment end");
			return;
		}

		default:
			next_char();
			break;
		}
	}
}

static void skip_whitespace(void)
{
	while (true) {
		switch (input.c) {
		case ' ':
		case '\t':
			next_char();
			continue;

		MATCH_NEWLINE(
			info.at_line_begin = true;
			return;
		)

		case '/':
			next_char();
			if (input.c == '/') {
				next_char();
				skip_line_comment();
				continue;
			} else if (input.c == '*') {
				next_char();
				skip_multiline_comment();
				continue;
			} else {
				put_back(input.c);
				input.c = '/';
			}
			return;
		default:
			return;
		}
	}
}

static void eat_pp(int type)
{
	(void) type;
	assert(pp_token.kind == type);
	next_preprocessing_token();
}

static void parse_symbol(void)
{
	obstack_1grow(&symbol_obstack, (char) input.c);
	next_char();

	while (true) {
		switch (input.c) {
		DIGITS
		SYMBOL_CHARS
			obstack_1grow(&symbol_obstack, (char) input.c);
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
	if (input.c == '"' && string[0] == 'L' && string[1] == '\0') {
		obstack_free(&symbol_obstack, string);
		parse_wide_string_literal();
		return;
	} else if (input.c == '\'' && string[0] == 'L' && string[1] == '\0') {
		obstack_free(&symbol_obstack, string);
		parse_wide_character_constant();
		return;
	}

	symbol_t *symbol = symbol_table_insert(string);

	pp_token.kind              = symbol->pp_ID;
	pp_token.identifier.symbol = symbol;

	/* we can free the memory from symbol obstack if we already had an entry in
	 * the symbol table */
	if (symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}
}

static void parse_number(void)
{
	obstack_1grow(&symbol_obstack, (char) input.c);
	next_char();

	while (true) {
		switch (input.c) {
		case '.':
		DIGITS
		SYMBOL_CHARS_WITHOUT_E_P
			obstack_1grow(&symbol_obstack, (char) input.c);
			next_char();
			break;

		case 'e':
		case 'p':
		case 'E':
		case 'P':
			obstack_1grow(&symbol_obstack, (char) input.c);
			next_char();
			if (input.c == '+' || input.c == '-') {
				obstack_1grow(&symbol_obstack, (char) input.c);
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

	pp_token.kind          = TP_NUMBER;
	pp_token.number.number = make_string(string, size);
}


#define MAYBE_PROLOG                                       \
			next_char();                                   \
			while (true) {                                 \
				switch (input.c) {

#define MAYBE(ch, set_type)                                \
				case ch:                                   \
					next_char();                           \
					pp_token.kind = set_type;              \
					return;

#define ELSE_CODE(code)                                    \
				default:                                   \
					code                                   \
					return;                                \
				}                                          \
			}

#define ELSE(set_type)                                     \
		ELSE_CODE(                                         \
			pp_token.kind = set_type;                      \
		)

static void next_preprocessing_token(void)
{
	if (current_expansion != NULL) {
		expand_next();
		return;
	}

	info.at_line_begin  = false;
	info.had_whitespace = false;
restart:
	pp_token.base.source_position = input.position;
	switch (input.c) {
	case ' ':
	case '\t':
		++info.whitespace;
		info.had_whitespace = true;
		next_char();
		goto restart;

	MATCH_NEWLINE(
		info.at_line_begin = true;
		info.had_whitespace = true;
		goto restart;
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
				put_back(input.c);
				input.c = '.';
				parse_number();
				return;

			case '.':
				MAYBE_PROLOG
				MAYBE('.', TP_DOTDOTDOT)
				ELSE_CODE(
					put_back(input.c);
					input.c = '.';
					pp_token.kind = '.';
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
				info.had_whitespace = true;
				skip_multiline_comment();
				goto restart;
			case '/':
				next_char();
				info.had_whitespace = true;
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
							put_back(input.c);
							input.c = '%';
							pp_token.kind = '#';
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
		ELSE_CODE(
			pp_token.kind = '#';
		)

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
		pp_token.kind = input.c;
		next_char();
		return;

	case EOF:
		if (input_stack != NULL) {
			close_input();
			pop_restore_input();
			fputc('\n', out);
			print_line_directive(&input.position, "2");
			goto restart;
		} else {
			pp_token.base.source_position.lineno++;
			info.at_line_begin = true;
			pp_token.kind = TP_EOF;
		}
		return;

	default:
		next_char();
		if (!ignore_unknown_chars) {
			errorf(&pp_token.base.source_position,
			       "unknown character '%c' found\n", input.c);
			pp_token.kind = TP_ERROR;
		} else {
			pp_token.kind = input.c;
		}
		return;
	}
}

static void print_quoted_string(const char *const string)
{
	fputc('"', out);
	for (const char *c = string; *c != 0; ++c) {
		switch (*c) {
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
			if (!isprint(*c)) {
				fprintf(out, "\\%03o", (unsigned)*c);
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
	fprintf(out, "# %u ", pos->lineno);
	print_quoted_string(pos->input_name);
	if (add != NULL) {
		fputc(' ', out);
		fputs(add, out);
	}

	printed_input_name = pos->input_name;
	input.output_line  = pos->lineno-1;
}

static void emit_newlines(void)
{
	unsigned delta = pp_token.base.source_position.lineno - input.output_line;

	if (delta >= 9) {
		fputc('\n', out);
		print_line_directive(&pp_token.base.source_position, NULL);
		fputc('\n', out);
	} else {
		for (unsigned i = 0; i < delta; ++i) {
			fputc('\n', out);
		}
	}
	input.output_line = pp_token.base.source_position.lineno;
}

static void emit_pp_token(void)
{
	if (skip_mode)
		return;

	if (info.at_line_begin) {
		emit_newlines();

		for (unsigned i = 0; i < info.whitespace; ++i)
			fputc(' ', out);

	} else if (info.had_whitespace ||
			   tokens_would_paste(last_token, pp_token.kind)) {
		fputc(' ', out);
	}

	switch (pp_token.kind) {
	case TP_IDENTIFIER:
		fputs(pp_token.identifier.symbol->string, out);
		break;
	case TP_NUMBER:
		fputs(pp_token.number.number.begin, out);
		break;
	case TP_WIDE_STRING_LITERAL:
		fputc('L', out);
	case TP_STRING_LITERAL:
		fputc('"', out);
		fputs(pp_token.string.string.begin, out);
		fputc('"', out);
		break;
	case TP_WIDE_CHARACTER_CONSTANT:
		fputc('L', out);
	case TP_CHARACTER_CONSTANT:
		fputc('\'', out);
		fputs(pp_token.string.string.begin, out);
		fputc('\'', out);
		break;
	default:
		print_pp_token_kind(out, pp_token.kind);
		break;
	}
	last_token = pp_token.kind;
}

static void eat_pp_directive(void)
{
	while (!info.at_line_begin) {
		next_preprocessing_token();
	}
}

static bool strings_equal(const string_t *string1, const string_t *string2)
{
	size_t size = string1->size;
	if (size != string2->size)
		return false;

	const char *c1 = string1->begin;
	const char *c2 = string2->begin;
	for (size_t i = 0; i < size; ++i, ++c1, ++c2) {
		if (*c1 != *c2)
			return false;
	}
	return true;
}

static bool pp_tokens_equal(const token_t *token1, const token_t *token2)
{
	if (token1->kind != token2->kind)
		return false;

	switch (token1->kind) {
	case TP_IDENTIFIER:
		return token1->identifier.symbol == token2->identifier.symbol;
	case TP_NUMBER:
	case TP_CHARACTER_CONSTANT:
	case TP_STRING_LITERAL:
		return strings_equal(&token1->string.string, &token2->string.string);

	default:
		return true;
	}
}

static bool pp_definitions_equal(const pp_definition_t *definition1,
                                 const pp_definition_t *definition2)
{
	if (definition1->list_len != definition2->list_len)
		return false;

	size_t         len = definition1->list_len;
	const token_t *t1  = definition1->token_list;
	const token_t *t2  = definition2->token_list;
	for (size_t i = 0; i < len; ++i, ++t1, ++t2) {
		if (!pp_tokens_equal(t1, t2))
			return false;
	}
	return true;
}

static void parse_define_directive(void)
{
	eat_pp(TP_define);
	assert(obstack_object_size(&pp_obstack) == 0);

	if (pp_token.kind != TP_IDENTIFIER || info.at_line_begin) {
		errorf(&pp_token.base.source_position,
		       "expected identifier after #define, got '%t'", &pp_token);
		goto error_out;
	}
	symbol_t *symbol = pp_token.identifier.symbol;

	pp_definition_t *new_definition
		= obstack_alloc(&pp_obstack, sizeof(new_definition[0]));
	memset(new_definition, 0, sizeof(new_definition[0]));
	new_definition->source_position = input.position;

	/* this is probably the only place where spaces are significant in the
	 * lexer (except for the fact that they separate tokens). #define b(x)
	 * is something else than #define b (x) */
	if (input.c == '(') {
		/* eat the '(' */
		next_preprocessing_token();
		/* get next token after '(' */
		next_preprocessing_token();

		while (true) {
			switch (pp_token.kind) {
			case TP_DOTDOTDOT:
				new_definition->is_variadic = true;
				next_preprocessing_token();
				if (pp_token.kind != ')') {
					errorf(&input.position,
							"'...' not at end of macro argument list");
					goto error_out;
				}
				break;
			case TP_IDENTIFIER:
				obstack_ptr_grow(&pp_obstack, pp_token.identifier.symbol);
				next_preprocessing_token();

				if (pp_token.kind == ',') {
					next_preprocessing_token();
					break;
				}

				if (pp_token.kind != ')') {
					errorf(&pp_token.base.source_position,
					       "expected ',' or ')' after identifier, got '%t'",
					       &pp_token);
					goto error_out;
				}
				break;
			case ')':
				next_preprocessing_token();
				goto finish_argument_list;
			default:
				errorf(&pp_token.base.source_position,
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
	while (!info.at_line_begin) {
		obstack_grow(&pp_obstack, &pp_token, sizeof(pp_token));
		++list_len;
		next_preprocessing_token();
	}

	new_definition->list_len   = list_len;
	new_definition->token_list = obstack_finish(&pp_obstack);

	pp_definition_t *old_definition = symbol->pp_definition;
	if (old_definition != NULL) {
		if (!pp_definitions_equal(old_definition, new_definition)) {
			warningf(WARN_OTHER, &input.position, "multiple definition of macro '%Y' (first defined %P)", symbol, &old_definition->source_position);
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

	if (pp_token.kind != TP_IDENTIFIER) {
		errorf(&input.position,
		       "expected identifier after #undef, got '%t'", &pp_token);
		eat_pp_directive();
		return;
	}

	symbol_t *symbol = pp_token.identifier.symbol;
	symbol->pp_definition = NULL;
	next_preprocessing_token();

	if (!info.at_line_begin) {
		warningf(WARN_OTHER, &input.position, "extra tokens at end of #undef directive");
	}
	eat_pp_directive();
}

static const char *parse_headername(void)
{
	/* behind an #include we can have the special headername lexems.
	 * They're only allowed behind an #include so they're not recognized
	 * by the normal next_preprocessing_token. We handle them as a special
	 * exception here */
	if (info.at_line_begin) {
		parse_error("expected headername after #include");
		return NULL;
	}

	assert(obstack_object_size(&symbol_obstack) == 0);

	/* check wether we have a "... or <... headername */
	switch (input.c) {
	case '<':
		next_char();
		while (true) {
			switch (input.c) {
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
			obstack_1grow(&symbol_obstack, (char) input.c);
			next_char();
		}
		/* we should never be here */

	case '"':
		next_char();
		while (true) {
			switch (input.c) {
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
			obstack_1grow(&symbol_obstack, (char) input.c);
			next_char();
		}
		/* we should never be here */

	default:
		/* TODO: do normal pp_token parsing and concatenate results */
		panic("pp_token concat include not implemented yet");
	}

finished_headername:
	obstack_1grow(&symbol_obstack, '\0');
	char *headername = obstack_finish(&symbol_obstack);

	/* TODO: iterate search-path to find the file */

	skip_whitespace();

	return identify_string(headername);
}

static bool do_include(bool system_include, const char *headername)
{
	if (!system_include) {
		/* for "bla" includes first try current dir
		 * TODO: this isn't correct, should be the directory of the source file
		 */
		FILE *file = fopen(headername, "r");
		if (file != NULL) {
			switch_input(file, headername);
			return true;
		}
	}

	size_t headername_len = strlen(headername);
	assert(obstack_object_size(&pp_obstack) == 0);
	/* check searchpath */
	for (searchpath_entry_t *entry = searchpath; entry != NULL;
	     entry = entry->next) {
	    const char *path = entry->path;
	    size_t      len  = strlen(path);
		obstack_grow(&pp_obstack, path, len);
		if (path[len-1] != '/')
			obstack_1grow(&pp_obstack, '/');
		obstack_grow(&pp_obstack, headername, headername_len+1);

		char *complete_path = obstack_finish(&pp_obstack);
		FILE *file          = fopen(complete_path, "r");
		if (file != NULL) {
			const char *filename = identify_string(complete_path);
			switch_input(file, filename);
			return true;
		}
		obstack_free(&pp_obstack, complete_path);
	}

	return false;
}

static bool parse_include_directive(void)
{
	/* don't eat the TP_include here!
	 * we need an alternative parsing for the next token */
	skip_whitespace();
	bool system_include = input.c == '<';
	const char *headername = parse_headername();
	if (headername == NULL) {
		eat_pp_directive();
		return false;
	}

	if (!info.at_line_begin) {
		warningf(WARN_OTHER, &pp_token.base.source_position,
		         "extra tokens at end of #include directive");
		eat_pp_directive();
	}

	if (n_inputs > INCLUDE_LIMIT) {
		errorf(&pp_token.base.source_position, "#include nested too deeply");
		/* eat \n or EOF */
		next_preprocessing_token();
		return false;
	}

	/* we have to reenable space counting and macro expansion here,
	 * because it is still disabled in directive parsing,
	 * but we will trigger a preprocessing token reading of the new file
	 * now and need expansions/space counting */
	in_pp_directive = false;

	/* switch inputs */
	emit_newlines();
	push_input();
	bool res = do_include(system_include, headername);
	if (!res) {
		errorf(&pp_token.base.source_position,
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
	bool is_ifndef = (pp_token.kind == TP_ifndef);
	bool condition;
	next_preprocessing_token();

	if (skip_mode) {
		eat_pp_directive();
		pp_conditional_t *conditional = push_conditional();
		conditional->source_position  = pp_token.base.source_position;
		conditional->skip             = true;
		return;
	}

	if (pp_token.kind != TP_IDENTIFIER || info.at_line_begin) {
		errorf(&pp_token.base.source_position,
		       "expected identifier after #%s, got '%t'",
		       is_ifndef ? "ifndef" : "ifdef", &pp_token);
		eat_pp_directive();

		/* just take the true case in the hope to avoid further errors */
		condition = true;
	} else {
		symbol_t        *symbol        = pp_token.identifier.symbol;
		pp_definition_t *pp_definition = symbol->pp_definition;
		next_preprocessing_token();

		if (!info.at_line_begin) {
			errorf(&pp_token.base.source_position,
			       "extra tokens at end of #%s",
			       is_ifndef ? "ifndef" : "ifdef");
			eat_pp_directive();
		}

		/* evaluate wether we are in true or false case */
		condition = is_ifndef ? pp_definition == NULL : pp_definition != NULL;
	}

	pp_conditional_t *conditional = push_conditional();
	conditional->source_position  = pp_token.base.source_position;
	conditional->condition        = condition;

	if (!condition) {
		skip_mode = true;
	}
}

static void parse_else_directive(void)
{
	eat_pp(TP_else);

	if (!info.at_line_begin) {
		if (!skip_mode) {
			warningf(WARN_OTHER, &pp_token.base.source_position, "extra tokens at end of #else");
		}
		eat_pp_directive();
	}

	pp_conditional_t *conditional = conditional_stack;
	if (conditional == NULL) {
		errorf(&pp_token.base.source_position, "#else without prior #if");
		return;
	}

	if (conditional->in_else) {
		errorf(&pp_token.base.source_position,
		       "#else after #else (condition started %P)",
		       conditional->source_position);
		skip_mode = true;
		return;
	}

	conditional->in_else = true;
	if (!conditional->skip) {
		skip_mode = conditional->condition;
	}
	conditional->source_position = pp_token.base.source_position;
}

static void parse_endif_directive(void)
{
	eat_pp(TP_endif);

	if (!info.at_line_begin) {
		if (!skip_mode) {
			warningf(WARN_OTHER, &pp_token.base.source_position, "extra tokens at end of #endif");
		}
		eat_pp_directive();
	}

	pp_conditional_t *conditional = conditional_stack;
	if (conditional == NULL) {
		errorf(&pp_token.base.source_position, "#endif without prior #if");
		return;
	}

	if (!conditional->skip) {
		skip_mode = false;
	}
	pop_conditional();
}

static void parse_preprocessing_directive(void)
{
	in_pp_directive = true;
	eat_pp('#');

	if (skip_mode) {
		switch (pp_token.kind) {
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
		switch (pp_token.kind) {
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
		case TP_include:
			parse_include_directive();
			break;
		default:
			if (info.at_line_begin) {
				/* the nop directive "#" */
				break;
			}
			errorf(&pp_token.base.source_position,
				   "invalid preprocessing directive #%t", &pp_token);
			eat_pp_directive();
			break;
		}
	}

	in_pp_directive = false;
	assert(info.at_line_begin);
}

static void prepend_include_path(const char *path)
{
	searchpath_entry_t *entry = OALLOCZ(&config_obstack, searchpath_entry_t);
	entry->path = path;
	entry->next = searchpath;
	searchpath  = entry;
}

static void setup_include_path(void)
{
	/* built-in paths */
	prepend_include_path("/usr/include");

	/* parse environment variable */
	const char *cpath = getenv("CPATH");
	if (cpath != NULL && *cpath != '\0') {
		const char *begin = cpath;
		const char *c;
		do {
			c = begin;
			while (*c != '\0' && *c != ':')
				++c;

			size_t len = c-begin;
			if (len == 0) {
				/* for gcc compatibility (Matze: I would expect that
				 * nothing happens for an empty entry...) */
				prepend_include_path(".");
			} else {
				char *string = obstack_alloc(&config_obstack, len+1);
				memcpy(string, begin, len);
				string[len] = '\0';

				prepend_include_path(string);
			}

			begin = c+1;
			/* skip : */
			if (*begin == ':')
				++begin;
		} while(*c != '\0');
	}
}

int pptest_main(int argc, char **argv);
int pptest_main(int argc, char **argv)
{
	init_symbol_table();
	init_tokens();

	obstack_init(&config_obstack);
	obstack_init(&pp_obstack);
	obstack_init(&input_obstack);
	strset_init(&stringset);

	setup_include_path();

	/* simplistic commandline parser */
	const char *filename = NULL;
	for (int i = 1; i < argc; ++i) {
		const char *opt = argv[i];
		if (streq(opt, "-I")) {
			prepend_include_path(argv[++i]);
			continue;
		} else if (streq(opt, "-E")) {
			/* ignore */
		} else if (opt[0] == '-') {
			fprintf(stderr, "Unknown option '%s'\n", opt);
		} else {
			if (filename != NULL)
				fprintf(stderr, "Multiple inputs not supported\n");
			filename = argv[i];
		}
	}
	if (filename == NULL) {
		fprintf(stderr, "No input specified\n");
		return 1;
	}

	out = stdout;

	/* just here for gcc compatibility */
	fprintf(out, "# 1 \"%s\"\n", filename);
	fprintf(out, "# 1 \"<built-in>\"\n");
	fprintf(out, "# 1 \"<command-line>\"\n");

	FILE *file = fopen(filename, "r");
	if (file == NULL) {
		fprintf(stderr, "Couldn't open input '%s'\n", filename);
		return 1;
	}
	switch_input(file, filename);

	while (true) {
		if (pp_token.kind == '#' && info.at_line_begin) {
			parse_preprocessing_directive();
			continue;
		} else if (pp_token.kind == TP_EOF) {
			goto end_of_main_loop;
		} else if (pp_token.kind == TP_IDENTIFIER && !in_pp_directive) {
			symbol_t *symbol = pp_token.identifier.symbol;
			pp_definition_t *pp_definition = symbol->pp_definition;
			if (pp_definition != NULL && !pp_definition->is_expanding) {
				expansion_pos = pp_token.base.source_position;
				if (pp_definition->has_parameters) {
					source_position_t position = pp_token.base.source_position;
					add_token_info_t old_info = info;
					next_preprocessing_token();
					add_token_info_t new_info = info;

					/* no opening brace -> no expansion */
					if (pp_token.kind == '(') {
						eat_pp('(');

						/* parse arguments (TODO) */
						while (pp_token.kind != TP_EOF && pp_token.kind != ')')
							next_preprocessing_token();
					} else {
						token_t next_token = pp_token;
						/* restore identifier token */
						pp_token.kind                 = TP_IDENTIFIER;
						pp_token.identifier.symbol    = symbol;
						pp_token.base.source_position = position;
						info = old_info;
						emit_pp_token();

						info = new_info;
						pp_token = next_token;
						continue;
					}
					info = old_info;
				}
				pp_definition->expand_pos   = 0;
				pp_definition->is_expanding = true;
				current_expansion           = pp_definition;
				expand_next();
				continue;
			}
		}

		emit_pp_token();
		next_preprocessing_token();
	}
end_of_main_loop:

	fputc('\n', out);
	check_unclosed_conditionals();
	close_input();

	obstack_free(&input_obstack, NULL);
	obstack_free(&pp_obstack, NULL);
	obstack_free(&config_obstack, NULL);

	strset_destroy(&stringset);

	exit_tokens();
	exit_symbol_table();

	return 0;
}
