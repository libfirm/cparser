#include <config.h>

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#include "preprocessor.h"
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

typedef struct saved_token_t {
	token_t token;
	bool    had_whitespace;
} saved_token_t;

typedef struct whitespace_info_t {
	/** current token had whitespace in front of it */
	bool     had_whitespace;
	/** current token is at the beginning of a line.
	 * => a "#" at line begin starts a preprocessing directive. */
	bool     at_line_begin;
	/** number of spaces before the first token in a line */
	unsigned whitespace_at_line_begin;
} whitespace_info_t;

struct pp_definition_t {
	symbol_t          *symbol;
	source_position_t  source_position;
	pp_definition_t   *parent_expansion;
	size_t             expand_pos;
	whitespace_info_t  expand_info;
	bool               is_variadic    : 1;
	bool               is_expanding   : 1;
	bool               has_parameters : 1;
	bool               is_parameter   : 1;
	pp_definition_t   *function_definition;
	size_t             n_parameters;
	pp_definition_t   *parameters;

	/* replacement */
	size_t             list_len;
	saved_token_t     *token_list;
};

typedef struct pp_conditional_t pp_conditional_t;
struct pp_conditional_t {
	source_position_t  source_position;
	bool               condition;
	bool               in_else;
	/** conditional in skip mode (then+else gets skipped) */
	bool               skip;
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

token_t                  pp_token;
bool                     allow_dollar_in_symbol   = true;
static bool              resolve_escape_sequences = true;
static bool              error_on_unknown_chars   = true;
static bool              skip_mode;
static FILE             *out;
static struct obstack    pp_obstack;
static struct obstack    config_obstack;
static const char       *printed_input_name = NULL;
static source_position_t expansion_pos;
static pp_definition_t  *current_expansion  = NULL;
static pp_definition_t  *current_call       = NULL;
static pp_definition_t  *current_argument   = NULL;
static pp_definition_t  *argument_expanding = NULL;
static unsigned          argument_brace_count;
static strset_t          stringset;
static token_kind_t      last_token;

static searchpath_entry_t *searchpath;

static whitespace_info_t next_info; /* valid if had_whitespace is true */
static whitespace_info_t info;

static inline void next_char(void);
static void next_input_token(void);
static void print_line_directive(const source_position_t *pos, const char *add);

static symbol_t *symbol_colongreater;
static symbol_t *symbol_lesscolon;
static symbol_t *symbol_lesspercent;
static symbol_t *symbol_percentcolon;
static symbol_t *symbol_percentcolonpercentcolon;
static symbol_t *symbol_percentgreater;

static void init_symbols(void)
{
	symbol_colongreater             = symbol_table_insert(":>");
	symbol_lesscolon                = symbol_table_insert("<:");
	symbol_lesspercent              = symbol_table_insert("<%");
	symbol_percentcolon             = symbol_table_insert("%:");
	symbol_percentcolonpercentcolon = symbol_table_insert("%:%:");
	symbol_percentgreater           = symbol_table_insert("%>");
}

void switch_pp_input(FILE *const file, char const *const filename)
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
}

FILE *close_pp_input(void)
{
	input_free(input.input);

	FILE* const file = input.file;
	assert(file);

	input.input  = NULL;
	input.file   = NULL;
	input.bufend = NULL;
	input.bufpos = NULL;
	input.c      = EOF;

	return file;
}

static void push_input(void)
{
	pp_input_t *const saved_input = obstack_copy(&input_obstack, &input, sizeof(input));

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
		size_t const n = decode(input.input, input.buf + MAX_PUTBACK, lengthof(input.buf) - MAX_PUTBACK);
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

#define NEWLINE \
	'\r': \
		next_char(); \
		if (input.c == '\n') { \
	case '\n': \
			next_char(); \
		} \
		++input.position.lineno; \
		input.position.colno = 1; \
		goto newline; \
		newline // Let it look like an ordinary case label.

#define eat(c_type) (assert(input.c == c_type), next_char())

static void maybe_concat_lines(void)
{
	eat('\\');

	switch (input.c) {
	case NEWLINE:
		info.whitespace_at_line_begin = 0;
		return;

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

static bool is_universal_char_valid(utf32 const v)
{
	/* C11 §6.4.3:2 */
	if (v < 0xA0U && v != 0x24 && v != 0x40 && v != 0x60)
		return false;
	if (0xD800 <= v && v <= 0xDFFF)
		return false;
	return true;
}

static utf32 parse_universal_char(unsigned const n_digits)
{
	utf32 v = 0;
	for (unsigned k = n_digits; k != 0; --k) {
		if (isxdigit(input.c)) {
			v = 16 * v + digit_value(input.c);
			if (!resolve_escape_sequences)
				obstack_1grow(&symbol_obstack, input.c);
			next_char();
		} else {
			errorf(&input.position,
			       "short universal character name, expected %u more digits",
				   k);
			break;
		}
	}
	if (!is_universal_char_valid(v)) {
		errorf(&input.position,
		       "\\%c%0*X is not a valid universal character name",
		       n_digits == 4 ? 'u' : 'U', (int)n_digits, v);
	}
	return v;
}

static bool is_universal_char_valid_identifier(utf32 const v)
{
	/* C11 Annex D.1 */
	if (                v == 0x000A8) return true;
	if (                v == 0x000AA) return true;
	if (                v == 0x000AD) return true;
	if (                v == 0x000AF) return true;
	if (0x000B2 <= v && v <= 0x000B5) return true;
	if (0x000B7 <= v && v <= 0x000BA) return true;
	if (0x000BC <= v && v <= 0x000BE) return true;
	if (0x000C0 <= v && v <= 0x000D6) return true;
	if (0x000D8 <= v && v <= 0x000F6) return true;
	if (0x000F8 <= v && v <= 0x000FF) return true;
	if (0x00100 <= v && v <= 0x0167F) return true;
	if (0x01681 <= v && v <= 0x0180D) return true;
	if (0x0180F <= v && v <= 0x01FFF) return true;
	if (0x0200B <= v && v <= 0x0200D) return true;
	if (0x0202A <= v && v <= 0x0202E) return true;
	if (0x0203F <= v && v <= 0x02040) return true;
	if (                v == 0x02054) return true;
	if (0x02060 <= v && v <= 0x0206F) return true;
	if (0x02070 <= v && v <= 0x0218F) return true;
	if (0x02460 <= v && v <= 0x024FF) return true;
	if (0x02776 <= v && v <= 0x02793) return true;
	if (0x02C00 <= v && v <= 0x02DFF) return true;
	if (0x02E80 <= v && v <= 0x02FFF) return true;
	if (0x03004 <= v && v <= 0x03007) return true;
	if (0x03021 <= v && v <= 0x0302F) return true;
	if (0x03031 <= v && v <= 0x0303F) return true;
	if (0x03040 <= v && v <= 0x0D7FF) return true;
	if (0x0F900 <= v && v <= 0x0FD3D) return true;
	if (0x0FD40 <= v && v <= 0x0FDCF) return true;
	if (0x0FDF0 <= v && v <= 0x0FE44) return true;
	if (0x0FE47 <= v && v <= 0x0FFFD) return true;
	if (0x10000 <= v && v <= 0x1FFFD) return true;
	if (0x20000 <= v && v <= 0x2FFFD) return true;
	if (0x30000 <= v && v <= 0x3FFFD) return true;
	if (0x40000 <= v && v <= 0x4FFFD) return true;
	if (0x50000 <= v && v <= 0x5FFFD) return true;
	if (0x60000 <= v && v <= 0x6FFFD) return true;
	if (0x70000 <= v && v <= 0x7FFFD) return true;
	if (0x80000 <= v && v <= 0x8FFFD) return true;
	if (0x90000 <= v && v <= 0x9FFFD) return true;
	if (0xA0000 <= v && v <= 0xAFFFD) return true;
	if (0xB0000 <= v && v <= 0xBFFFD) return true;
	if (0xC0000 <= v && v <= 0xCFFFD) return true;
	if (0xD0000 <= v && v <= 0xDFFFD) return true;
	if (0xE0000 <= v && v <= 0xEFFFD) return true;
	return false;
}

static bool is_universal_char_valid_identifier_start(utf32 const v)
{
	/* C11 Annex D.2 */
	if (0x0300 <= v && v <= 0x036F) return false;
	if (0x1DC0 <= v && v <= 0x1DFF) return false;
	if (0x20D0 <= v && v <= 0x20FF) return false;
	if (0xFE20 <= v && v <= 0xFE2F) return false;
	return true;
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

	case 'U': return parse_universal_char(8);
	case 'u': return parse_universal_char(4);

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

static string_t sym_make_string(string_encoding_t const enc)
{
	obstack_1grow(&symbol_obstack, '\0');
	size_t      const len    = obstack_object_size(&symbol_obstack) - 1;
	char       *const string = obstack_finish(&symbol_obstack);
	char const *const result = identify_string(string);
	return (string_t){ result, len, enc };
}

string_t make_string(char const *const string)
{
	obstack_grow(&symbol_obstack, string, strlen(string));
	return sym_make_string(STRING_ENCODING_CHAR);
}

static void parse_string(utf32 const delimiter, token_kind_t const kind,
                         string_encoding_t const enc,
                         char const *const context)
{
	const unsigned start_linenr = input.position.lineno;

	eat(delimiter);

	while (true) {
		switch (input.c) {
		case '\\': {
			if (resolve_escape_sequences) {
				utf32 const tc = parse_escape_sequence();
				if (enc == STRING_ENCODING_CHAR) {
					if (tc >= 0x100) {
						warningf(WARN_OTHER, &pp_token.base.source_position, "escape sequence out of range");
					}
					obstack_1grow(&symbol_obstack, tc);
				} else {
					obstack_grow_utf8(&symbol_obstack, tc);
				}
			} else {
				obstack_1grow(&symbol_obstack, (char)input.c);
				next_char();
				obstack_1grow(&symbol_obstack, (char)input.c);
				next_char();
			}
			break;
		}

		case NEWLINE:
			errorf(&pp_token.base.source_position, "newline while parsing %s", context);
			break;

		case EOF: {
			source_position_t source_position;
			source_position.input_name = pp_token.base.source_position.input_name;
			source_position.lineno     = start_linenr;
			errorf(&source_position, "EOF while parsing %s", context);
			goto end_of_string;
		}

		default:
			if (input.c == delimiter) {
				next_char();
				goto end_of_string;
			} else {
				obstack_grow_utf8(&symbol_obstack, input.c);
				next_char();
				break;
			}
		}
	}

end_of_string:
	pp_token.kind           = kind;
	pp_token.literal.string = sym_make_string(enc);
}

static void parse_string_literal(string_encoding_t const enc)
{
	parse_string('"', T_STRING_LITERAL, enc, "string literal");
}

static void parse_character_constant(string_encoding_t const enc)
{
	parse_string('\'', T_CHARACTER_CONSTANT, enc, "character constant");
	if (pp_token.literal.string.size == 0) {
		parse_error("empty character constant");
	}
}

#define SYMBOL_CASES_WITHOUT_E_P \
	     '$': if (!allow_dollar_in_symbol) goto dollar_sign; \
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
	case '_'

#define SYMBOL_CASES \
	     SYMBOL_CASES_WITHOUT_E_P: \
	case 'e': \
	case 'p': \
	case 'E': \
	case 'P'

#define DIGIT_CASES \
	     '0':  \
	case '1':  \
	case '2':  \
	case '3':  \
	case '4':  \
	case '5':  \
	case '6':  \
	case '7':  \
	case '8':  \
	case '9'

static void start_expanding(pp_definition_t *definition)
{
	definition->parent_expansion = current_expansion;
	definition->expand_pos       = 0;
	definition->is_expanding     = true;
	if (definition->list_len > 0) {
		definition->token_list[0].had_whitespace
			= info.had_whitespace;
	}
	current_expansion = definition;
}

static void finished_expanding(pp_definition_t *definition)
{
	assert(definition->is_expanding);
	pp_definition_t *parent = definition->parent_expansion;
	definition->parent_expansion = NULL;
	definition->is_expanding     = false;

	/* stop further expanding once we expanded a parameter used in a
	 * sub macro-call */
	if (definition == argument_expanding)
		argument_expanding = NULL;

	assert(current_expansion == definition);
	current_expansion = parent;
}

static inline void set_punctuator(token_kind_t const kind)
{
	pp_token.kind        = kind;
	pp_token.base.symbol = token_symbols[kind];
}

static inline void set_digraph(token_kind_t const kind, symbol_t *const symbol)
{
	pp_token.kind        = kind;
	pp_token.base.symbol = symbol;
}

/**
 * returns next final token from a preprocessor macro expansion
 */
static bool expand_next(void)
{
	if (current_expansion == NULL)
		return false;

restart:;
	size_t pos = current_expansion->expand_pos;
	if (pos >= current_expansion->list_len) {
		finished_expanding(current_expansion);
		/* it was the outermost expansion, parse pptoken normally */
		if (current_expansion == NULL) {
			return false;
		}
		goto restart;
	}
	const saved_token_t *saved = &current_expansion->token_list[pos++];
	pp_token = saved->token;

	if (current_expansion->expand_pos > 0)
		info.had_whitespace = saved->had_whitespace;
	pp_token.base.source_position = expansion_pos;
	++current_expansion->expand_pos;

	return true;
}

/**
 * Returns the next token kind found when continuing the current expansions
 * without starting new sub-expansions.
 */
static token_kind_t peek_expansion(void)
{
	pp_definition_t *expansion = current_expansion;
	while (expansion != NULL && expansion->expand_pos >= expansion->list_len) {
		expansion = expansion->parent_expansion;
	}
	if (expansion == NULL)
		return T_EOF;
	return expansion->token_list[expansion->expand_pos].token.kind;
}

static void skip_line_comment(void)
{
	info.had_whitespace = true;
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
	info.had_whitespace = true;

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
				if (input.position.lineno != input.output_line)
					info.whitespace_at_line_begin = input.position.colno;
				next_char();
				return;
			}
			break;

		case NEWLINE:
			break;

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

static bool skip_till_newline(bool stop_at_non_whitespace)
{
	bool res = false;
	while (true) {
		switch (input.c) {
		case ' ':
		case '\t':
			next_char();
			continue;

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
			return true;

		case NEWLINE:
			return res;

		default:
			if (stop_at_non_whitespace)
				return false;
			res = true;
			next_char();
			continue;
		}
	}
}

static void skip_whitespace(void)
{
	while (true) {
		switch (input.c) {
		case ' ':
		case '\t':
			++info.whitespace_at_line_begin;
			info.had_whitespace = true;
			next_char();
			continue;

		case NEWLINE:
			info.at_line_begin  = true;
			info.had_whitespace = true;
			info.whitespace_at_line_begin = 0;
			continue;

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

static inline void eat_pp(pp_token_kind_t const kind)
{
	assert(pp_token.base.symbol->pp_ID == kind);
	(void) kind;
	next_input_token();
}

static inline void eat_token(token_kind_t const kind)
{
	assert(pp_token.kind == kind);
	(void)kind;
	next_input_token();
}

static void parse_symbol(void)
{
	assert(obstack_object_size(&symbol_obstack) == 0);
	while (true) {
		switch (input.c) {
		case DIGIT_CASES:
		case SYMBOL_CASES:
			obstack_1grow(&symbol_obstack, (char) input.c);
			next_char();
			break;

		case '\\':
			next_char();
			switch (input.c) {
			{
				unsigned n;
			case 'U': n = 8; goto universal;
			case 'u': n = 4; goto universal;
universal:
				if (!resolve_escape_sequences) {
					obstack_1grow(&symbol_obstack, '\\');
					obstack_1grow(&symbol_obstack, input.c);
				}
				next_char();
				utf32 const v = parse_universal_char(n);
				if (!is_universal_char_valid_identifier(v)) {
					if (is_universal_char_valid(v)) {
						errorf(&input.position,
							   "universal character \\%c%0*X is not valid in an identifier",
							   n == 4 ? 'u' : 'U', (int)n, v);
					}
				} else if (obstack_object_size(&symbol_obstack) == 0 && !is_universal_char_valid_identifier_start(v)) {
					errorf(&input.position,
						   "universal character \\%c%0*X is not valid as start of an identifier",
						   n == 4 ? 'u' : 'U', (int)n, v);
				} else if (resolve_escape_sequences) {
					obstack_grow_utf8(&symbol_obstack, v);
				}
				break;
			}

			default:
				put_back(input.c);
				input.c = '\\';
				goto end_symbol;
			}

		default:
dollar_sign:
			goto end_symbol;
		}
	}

end_symbol:
	obstack_1grow(&symbol_obstack, '\0');
	char *string = obstack_finish(&symbol_obstack);

	/* might be a wide string or character constant ( L"string"/L'c' ) */
	if (input.c == '"' && string[0] == 'L' && string[1] == '\0') {
		obstack_free(&symbol_obstack, string);
		parse_string_literal(STRING_ENCODING_WIDE);
		return;
	} else if (input.c == '\'' && string[0] == 'L' && string[1] == '\0') {
		obstack_free(&symbol_obstack, string);
		parse_character_constant(STRING_ENCODING_WIDE);
		return;
	}

	symbol_t *symbol = symbol_table_insert(string);

	pp_token.kind        = symbol->ID;
	pp_token.base.symbol = symbol;

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
		case DIGIT_CASES:
		case SYMBOL_CASES_WITHOUT_E_P:
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
dollar_sign:
			goto end_number;
		}
	}

end_number:
	pp_token.kind           = T_NUMBER;
	pp_token.literal.string = sym_make_string(STRING_ENCODING_CHAR);
}

#define MAYBE_PROLOG \
	next_char(); \
	switch (input.c) {

#define MAYBE(ch, kind) \
	case ch: \
		next_char(); \
		set_punctuator(kind); \
		return;

#define MAYBE_DIGRAPH(ch, kind, symbol) \
	case ch: \
		next_char(); \
		set_digraph(kind, symbol); \
		return;

#define ELSE_CODE(code) \
	default: \
		code \
	}

#define ELSE(kind) ELSE_CODE(set_punctuator(kind); return;)

/** identifies and returns the next preprocessing token contained in the
 * input stream. No macro expansion is performed. */
static void next_input_token(void)
{
	if (next_info.had_whitespace) {
		info = next_info;
		next_info.had_whitespace = false;
	} else {
		info.at_line_begin  = false;
		info.had_whitespace = false;
	}
restart:
	pp_token.base.source_position = input.position;
	pp_token.base.symbol          = NULL;

	switch (input.c) {
	case ' ':
	case '\t':
		info.whitespace_at_line_begin++;
		info.had_whitespace = true;
		next_char();
		goto restart;

	case NEWLINE:
		info.at_line_begin            = true;
		info.had_whitespace           = true;
		info.whitespace_at_line_begin = 0;
		goto restart;

	case SYMBOL_CASES:
		parse_symbol();
		return;

	case DIGIT_CASES:
		parse_number();
		return;

	case '"':
		parse_string_literal(STRING_ENCODING_CHAR);
		return;

	case '\'':
		parse_character_constant(STRING_ENCODING_CHAR);
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
				MAYBE('.', T_DOTDOTDOT)
				ELSE_CODE(
					put_back(input.c);
					input.c = '.';
					set_punctuator('.');
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
			goto restart;
		case '/':
			next_char();
			skip_line_comment();
			goto restart;
		ELSE('/')
	case '%':
		MAYBE_PROLOG
		MAYBE_DIGRAPH('>', '}', symbol_percentgreater)
		MAYBE('=', T_PERCENTEQUAL)
		case ':':
			MAYBE_PROLOG
			case '%':
				MAYBE_PROLOG
				MAYBE_DIGRAPH(':', T_HASHHASH, symbol_percentcolonpercentcolon)
				ELSE_CODE(
					put_back(input.c);
					input.c = '%';
					goto digraph_percentcolon;
				)
			ELSE_CODE(
digraph_percentcolon:
				set_digraph('#', symbol_percentcolon);
				return;
			)
		ELSE('%')
	case '<':
		MAYBE_PROLOG
		MAYBE_DIGRAPH(':', '[', symbol_lesscolon)
		MAYBE_DIGRAPH('%', '{', symbol_lesspercent)
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
		MAYBE_DIGRAPH('>', ']', symbol_colongreater)
		case ':':
			if (c_mode & _CXX) {
				next_char();
				set_punctuator(T_COLONCOLON);
				return;
			}
			/* FALLTHROUGH */
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
		set_punctuator(input.c);
		next_char();
		return;

	case EOF:
		if (input_stack != NULL) {
			fclose(close_pp_input());
			pop_restore_input();
			fputc('\n', out);
			if (input.c == (utf32)EOF)
				--input.position.lineno;
			print_line_directive(&input.position, "2");
			goto restart;
		} else {
			info.at_line_begin = true;
			set_punctuator(T_EOF);
		}
		return;

	case '\\':
		next_char();
		int next_c = input.c;
		put_back(input.c);
		input.c = '\\';
		if (next_c == 'U' || next_c == 'u') {
			parse_symbol();
			return;
		}
		/* FALLTHROUGH */
	default:
dollar_sign:
		if (error_on_unknown_chars) {
			errorf(&pp_token.base.source_position,
			       "unknown character '%lc' found\n", input.c);
			next_char();
			goto restart;
		} else {
			assert(obstack_object_size(&symbol_obstack) == 0);
			obstack_grow_utf8(&symbol_obstack, input.c);
			obstack_1grow(&symbol_obstack, '\0');
			char     *const string = obstack_finish(&symbol_obstack);
			symbol_t *const symbol = symbol_table_insert(string);
			if (symbol->string != string)
				obstack_free(&symbol_obstack, string);

			pp_token.kind        = T_UNKNOWN_CHAR;
			pp_token.base.symbol = symbol;
			next_char();
			return;
		}
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
	if (!out)
		return;

	fprintf(out, "# %u ", pos->lineno);
	print_quoted_string(pos->input_name);
	if (add != NULL) {
		fputc(' ', out);
		fputs(add, out);
	}

	printed_input_name = pos->input_name;
	input.output_line  = pos->lineno-1;
}

static bool emit_newlines(void)
{
	unsigned delta = pp_token.base.source_position.lineno - input.output_line;
	if (delta == 0)
		return false;

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

	for (unsigned i = 0; i < info.whitespace_at_line_begin; ++i)
		fputc(' ', out);

	return true;
}

void set_preprocessor_output(FILE *output)
{
	out = output;
	if (out != NULL) {
		error_on_unknown_chars   = false;
		resolve_escape_sequences = false;
	} else {
		error_on_unknown_chars   = true;
		resolve_escape_sequences = true;
	}
}

void emit_pp_token(void)
{
	if (!emit_newlines() &&
	    (info.had_whitespace || tokens_would_paste(last_token, pp_token.kind)))
		fputc(' ', out);

	switch (pp_token.kind) {
	case T_NUMBER:
		fputs(pp_token.literal.string.begin, out);
		break;

	case T_STRING_LITERAL:
		fputs(get_string_encoding_prefix(pp_token.literal.string.encoding), out);
		fputc('"', out);
		fputs(pp_token.literal.string.begin, out);
		fputc('"', out);
		break;

	case T_CHARACTER_CONSTANT:
		fputs(get_string_encoding_prefix(pp_token.literal.string.encoding), out);
		fputc('\'', out);
		fputs(pp_token.literal.string.begin, out);
		fputc('\'', out);
		break;

	case T_MACRO_PARAMETER:
		panic("macro parameter not expanded");

	default:
		fputs(pp_token.base.symbol->string, out);
		break;
	}
	last_token = pp_token.kind;
}

static void eat_pp_directive(void)
{
	while (!info.at_line_begin) {
		next_input_token();
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
	case T_NUMBER:
	case T_CHARACTER_CONSTANT:
	case T_STRING_LITERAL:
		return strings_equal(&token1->literal.string, &token2->literal.string);

	case T_MACRO_PARAMETER:
		return token1->macro_parameter.def->symbol
		    == token2->macro_parameter.def->symbol;

	default:
		return token1->base.symbol == token2->base.symbol;
	}
}

static bool pp_definitions_equal(const pp_definition_t *definition1,
                                 const pp_definition_t *definition2)
{
	if (definition1->list_len != definition2->list_len)
		return false;

	size_t               len = definition1->list_len;
	const saved_token_t *t1  = definition1->token_list;
	const saved_token_t *t2  = definition2->token_list;
	for (size_t i = 0; i < len; ++i, ++t1, ++t2) {
		if (!pp_tokens_equal(&t1->token, &t2->token))
			return false;
	}
	return true;
}

static void parse_define_directive(void)
{
	eat_pp(TP_define);
	if (skip_mode) {
		eat_pp_directive();
		return;
	}

	assert(obstack_object_size(&pp_obstack) == 0);

	if (pp_token.kind != T_IDENTIFIER || info.at_line_begin) {
		errorf(&pp_token.base.source_position,
		       "expected identifier after #define, got %K", &pp_token);
		goto error_out;
	}
	symbol_t *const symbol = pp_token.base.symbol;

	pp_definition_t *new_definition
		= obstack_alloc(&pp_obstack, sizeof(new_definition[0]));
	memset(new_definition, 0, sizeof(new_definition[0]));
	new_definition->symbol          = symbol;
	new_definition->source_position = input.position;

	/* this is probably the only place where spaces are significant in the
	 * lexer (except for the fact that they separate tokens). #define b(x)
	 * is something else than #define b (x) */
	if (input.c == '(') {
		eat_token(T_IDENTIFIER);
		eat_token('(');

		while (true) {
			switch (pp_token.kind) {
			case T_DOTDOTDOT:
				new_definition->is_variadic = true;
				eat_token(T_DOTDOTDOT);
				if (pp_token.kind != ')') {
					errorf(&input.position,
							"'...' not at end of macro argument list");
					goto error_out;
				}
				break;

			case T_IDENTIFIER: {
				pp_definition_t parameter;
				memset(&parameter, 0, sizeof(parameter));
				parameter.source_position = pp_token.base.source_position;
				parameter.symbol          = pp_token.base.symbol;
				parameter.is_parameter    = true;
				obstack_grow(&pp_obstack, &parameter, sizeof(parameter));
				eat_token(T_IDENTIFIER);

				if (pp_token.kind == ',') {
					eat_token(',');
					break;
				}

				if (pp_token.kind != ')') {
					errorf(&pp_token.base.source_position,
					       "expected ',' or ')' after identifier, got %K",
					       &pp_token);
					goto error_out;
				}
				break;
			}

			case ')':
				eat_token(')');
				goto finish_argument_list;

			default:
				errorf(&pp_token.base.source_position,
				       "expected identifier, '...' or ')' in #define argument list, got %K",
				       &pp_token);
				goto error_out;
			}
		}

	finish_argument_list:
		new_definition->has_parameters = true;
		size_t size = obstack_object_size(&pp_obstack);
		new_definition->n_parameters
			= size / sizeof(new_definition->parameters[0]);
		new_definition->parameters = obstack_finish(&pp_obstack);
		for (size_t i = 0; i < new_definition->n_parameters; ++i) {
			pp_definition_t *param    = &new_definition->parameters[i];
			symbol_t        *symbol   = param->symbol;
			pp_definition_t *previous = symbol->pp_definition;
			if (previous != NULL
			    && previous->function_definition == new_definition) {
				errorf(&param->source_position,
				       "duplicate macro parameter '%Y'", symbol);
				param->symbol = sym_anonymous;
				continue;
			}
			param->parent_expansion    = previous;
			param->function_definition = new_definition;
			symbol->pp_definition      = param;
		}
	} else {
		eat_token(T_IDENTIFIER);
	}

	/* construct token list */
	assert(obstack_object_size(&pp_obstack) == 0);
	while (!info.at_line_begin) {
		if (pp_token.kind == T_IDENTIFIER) {
			const symbol_t  *symbol     = pp_token.base.symbol;
			pp_definition_t *definition = symbol->pp_definition;
			if (definition != NULL
			    && definition->function_definition == new_definition) {
			    pp_token.kind                = T_MACRO_PARAMETER;
			    pp_token.macro_parameter.def = definition;
			}
		}
		saved_token_t saved_token;
		saved_token.token = pp_token;
		saved_token.had_whitespace = info.had_whitespace;
		obstack_grow(&pp_obstack, &saved_token, sizeof(saved_token));
		next_input_token();
	}

	new_definition->list_len   = obstack_object_size(&pp_obstack)
		/ sizeof(new_definition->token_list[0]);
	new_definition->token_list = obstack_finish(&pp_obstack);

	if (new_definition->has_parameters) {
		for (size_t i = 0; i < new_definition->n_parameters; ++i) {
			pp_definition_t *param      = &new_definition->parameters[i];
			symbol_t        *symbol     = param->symbol;
			if (symbol == sym_anonymous)
				continue;
			assert(symbol->pp_definition == param);
			assert(param->function_definition == new_definition);
			symbol->pp_definition   = param->parent_expansion;
			param->parent_expansion = NULL;
		}
	}

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
	if (skip_mode) {
		eat_pp_directive();
		return;
	}

	if (pp_token.kind != T_IDENTIFIER) {
		errorf(&input.position,
		       "expected identifier after #undef, got %K", &pp_token);
		eat_pp_directive();
		return;
	}

	pp_token.base.symbol->pp_definition = NULL;
	eat_token(T_IDENTIFIER);

	if (!info.at_line_begin) {
		warningf(WARN_OTHER, &input.position, "extra tokens at end of #undef directive");
	}
	eat_pp_directive();
}

/** behind an #include we can have the special headername lexems.
 * They're only allowed behind an #include so they're not recognized
 * by the normal next_preprocessing_token. We handle them as a special
 * exception here */
static void parse_headername(void)
{
	const source_position_t start_position = input.position;
	string_t                string         = { NULL, 0, STRING_ENCODING_CHAR };
	assert(obstack_object_size(&symbol_obstack) == 0);

	if (info.at_line_begin) {
		parse_error("expected headername after #include");
		goto finish_error;
	}

	/* check wether we have a "... or <... headername */
	switch (input.c) {
	{
		utf32 delimiter;
	case '<': delimiter = '>'; goto parse_name;
	case '"': delimiter = '"'; goto parse_name;
parse_name:
		next_char();
		while (true) {
			switch (input.c) {
			case NEWLINE:
			case EOF:
				errorf(&pp_token.base.source_position, "header name without closing '%c'", (char)delimiter);
				goto finish_error;

			default:
				if (input.c == delimiter) {
					next_char();
					goto finished_headername;
				} else {
					obstack_1grow(&symbol_obstack, (char)input.c);
					next_char();
				}
				break;
			}
		}
		/* we should never be here */
	}

	default:
		/* TODO: do normal pp_token parsing and concatenate results */
		panic("pp_token concat include not implemented yet");
	}

finished_headername:
	string = sym_make_string(STRING_ENCODING_CHAR);

finish_error:
	pp_token.base.source_position = start_position;
	pp_token.kind                 = T_HEADERNAME;
	pp_token.literal.string       = string;
}

static bool do_include(bool system_include, const char *headername)
{
	size_t headername_len = strlen(headername);
	if (!system_include) {
		/* put dirname of current input on obstack */
		const char *filename   = input.position.input_name;
		const char *last_slash = strrchr(filename, '/');
		if (last_slash != NULL) {
			size_t len = last_slash - filename;
			obstack_grow(&symbol_obstack, filename, len + 1);
			obstack_grow0(&symbol_obstack, headername, headername_len);
			char *complete_path = obstack_finish(&symbol_obstack);
			headername = identify_string(complete_path);
		}

		FILE *file = fopen(headername, "r");
		if (file != NULL) {
			switch_pp_input(file, headername);
			return true;
		}
	}

	assert(obstack_object_size(&symbol_obstack) == 0);
	/* check searchpath */
	for (searchpath_entry_t *entry = searchpath; entry != NULL;
	     entry = entry->next) {
	    const char *path = entry->path;
	    size_t      len  = strlen(path);
		obstack_grow(&symbol_obstack, path, len);
		if (path[len-1] != '/')
			obstack_1grow(&symbol_obstack, '/');
		obstack_grow(&symbol_obstack, headername, headername_len+1);

		char *complete_path = obstack_finish(&symbol_obstack);
		FILE *file          = fopen(complete_path, "r");
		if (file != NULL) {
			const char *filename = identify_string(complete_path);
			switch_pp_input(file, filename);
			return true;
		} else {
			obstack_free(&symbol_obstack, complete_path);
		}
	}

	return false;
}

static void parse_include_directive(void)
{
	if (skip_mode) {
		eat_pp_directive();
		return;
	}

	/* don't eat the TP_include here!
	 * we need an alternative parsing for the next token */
	skip_till_newline(true);
	bool system_include = input.c == '<';
	parse_headername();
	string_t headername = pp_token.literal.string;
	if (headername.begin == NULL) {
		eat_pp_directive();
		return;
	}

	bool had_nonwhitespace = skip_till_newline(false);
	if (had_nonwhitespace) {
		warningf(WARN_OTHER, &pp_token.base.source_position,
		         "extra tokens at end of #include directive");
	}

	if (n_inputs > INCLUDE_LIMIT) {
		errorf(&pp_token.base.source_position, "#include nested too deeply");
		/* eat \n or EOF */
		next_input_token();
		return;
	}

	/* switch inputs */
	info.whitespace_at_line_begin = 0;
	info.had_whitespace           = false;
	info.at_line_begin            = true;
	emit_newlines();
	push_input();
	bool res = do_include(system_include, pp_token.literal.string.begin);
	if (res) {
		next_input_token();
	} else {
		errorf(&pp_token.base.source_position, "failed including '%S': %s", &pp_token.literal.string, strerror(errno));
		pop_restore_input();
	}
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

void check_unclosed_conditionals(void)
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

static void parse_ifdef_ifndef_directive(bool const is_ifdef)
{
	bool condition;
	eat_pp(is_ifdef ? TP_ifdef : TP_ifndef);

	if (skip_mode) {
		eat_pp_directive();
		pp_conditional_t *conditional = push_conditional();
		conditional->source_position  = pp_token.base.source_position;
		conditional->skip             = true;
		return;
	}

	if (pp_token.kind != T_IDENTIFIER || info.at_line_begin) {
		errorf(&pp_token.base.source_position,
		       "expected identifier after #%s, got %K",
		       is_ifdef ? "ifdef" : "ifndef", &pp_token);
		eat_pp_directive();

		/* just take the true case in the hope to avoid further errors */
		condition = true;
	} else {
		/* evaluate wether we are in true or false case */
		condition = (bool)pp_token.base.symbol->pp_definition == is_ifdef;
		eat_token(T_IDENTIFIER);

		if (!info.at_line_begin) {
			errorf(&pp_token.base.source_position,
			       "extra tokens at end of #%s",
			       is_ifdef ? "ifdef" : "ifndef");
			eat_pp_directive();
		}
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
		       &conditional->source_position);
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

typedef enum stdc_pragma_kind_t {
	STDC_UNKNOWN,
	STDC_FP_CONTRACT,
	STDC_FENV_ACCESS,
	STDC_CX_LIMITED_RANGE
} stdc_pragma_kind_t;

typedef enum stdc_pragma_value_kind_t {
	STDC_VALUE_UNKNOWN,
	STDC_VALUE_ON,
	STDC_VALUE_OFF,
	STDC_VALUE_DEFAULT
} stdc_pragma_value_kind_t;

static void parse_pragma_directive(void)
{
	eat_pp(TP_pragma);

	if (pp_token.kind != T_IDENTIFIER) {
		warningf(WARN_UNKNOWN_PRAGMAS, &pp_token.base.source_position,
		         "expected identifier after #pragma");
		eat_pp_directive();
		return;
	}

	stdc_pragma_kind_t kind = STDC_UNKNOWN;
	if (pp_token.base.symbol->pp_ID == TP_STDC && c_mode & _C99) {
		/* a STDC pragma */
		next_input_token();

		switch (pp_token.base.symbol->pp_ID) {
		case TP_FP_CONTRACT:      kind = STDC_FP_CONTRACT;      break;
		case TP_FENV_ACCESS:      kind = STDC_FENV_ACCESS;      break;
		case TP_CX_LIMITED_RANGE: kind = STDC_CX_LIMITED_RANGE; break;
		default:                  break;
		}
		if (kind != STDC_UNKNOWN) {
			next_input_token();
			stdc_pragma_value_kind_t value;
			switch (pp_token.base.symbol->pp_ID) {
			case TP_ON:      value = STDC_VALUE_ON;      break;
			case TP_OFF:     value = STDC_VALUE_OFF;     break;
			case TP_DEFAULT: value = STDC_VALUE_DEFAULT; break;
			default:         value = STDC_VALUE_UNKNOWN; break;
			}
			if (value == STDC_VALUE_UNKNOWN) {
				kind = STDC_UNKNOWN;
				errorf(&pp_token.base.source_position, "bad STDC pragma argument");
			}
		}
	}
	eat_pp_directive();
	if (kind == STDC_UNKNOWN) {
		warningf(WARN_UNKNOWN_PRAGMAS, &pp_token.base.source_position,
		         "encountered unknown #pragma");
	}
}

static void parse_line_directive(void)
{
	if (pp_token.kind != T_NUMBER) {
		if (!skip_mode)
			parse_error("expected integer");
	} else {
		char      *end;
		long const line = strtol(pp_token.literal.string.begin, &end, 0);
		if (*end == '\0') {
			/* use offset -1 as this is about the next line */
			input.position.lineno = line - 1;
			/* force output of line */
			input.output_line = input.position.lineno - 20;
		} else {
			if (!skip_mode) {
				errorf(&input.position, "'%S' is not a valid line number",
					   &pp_token.literal.string);
			}
		}
		next_input_token();
		if (info.at_line_begin)
			return;
	}
	if (pp_token.kind == T_STRING_LITERAL
	    && pp_token.literal.string.encoding == STRING_ENCODING_CHAR) {
		input.position.input_name       = pp_token.literal.string.begin;
		input.position.is_system_header = false;
		next_input_token();

		/* attempt to parse numeric flags as outputted by gcc preprocessor */
		while (!info.at_line_begin && pp_token.kind == T_NUMBER) {
			/* flags:
			 * 1 - indicates start of a new file
			 * 2 - indicates return from a file
			 * 3 - indicates system header
			 * 4 - indicates implicit extern "C" in C++ mode
			 *
			 * currently we're only interested in "3"
			 */
			if (streq(pp_token.literal.string.begin, "3")) {
				input.position.is_system_header = true;
			}
			next_input_token();
		}
	}

	eat_pp_directive();
}

static void parse_preprocessing_directive(void)
{
	eat_token('#');

	if (info.at_line_begin) {
		/* empty directive */
		return;
	}

	if (pp_token.base.symbol) {
		switch (pp_token.base.symbol->pp_ID) {
		case TP_define:  parse_define_directive();            break;
		case TP_else:    parse_else_directive();              break;
		case TP_endif:   parse_endif_directive();             break;
		case TP_ifdef:   parse_ifdef_ifndef_directive(true);  break;
		case TP_ifndef:  parse_ifdef_ifndef_directive(false); break;
		case TP_include: parse_include_directive();           break;
		case TP_line:    next_input_token(); goto line_directive;
		case TP_pragma:  parse_pragma_directive();            break;
		case TP_undef:   parse_undef_directive();             break;
		default:         goto skip;
		}
	} else if (pp_token.kind == T_NUMBER) {
line_directive:
		parse_line_directive();
	} else {
skip:
		if (!skip_mode) {
			errorf(&pp_token.base.source_position, "invalid preprocessing directive #%K", &pp_token);
		}
		eat_pp_directive();
	}

	assert(info.at_line_begin);
}

static void finish_current_argument(void)
{
	if (current_argument == NULL)
		return;
	size_t size = obstack_object_size(&pp_obstack);
	current_argument->list_len   = size/sizeof(current_argument->token_list[0]);
	current_argument->token_list = obstack_finish(&pp_obstack);
}

void next_preprocessing_token(void)
{
restart:
	if (!expand_next()) {
		do {
			next_input_token();
			while (pp_token.kind == '#' && info.at_line_begin) {
				parse_preprocessing_directive();
			}
		} while (skip_mode && pp_token.kind != T_EOF);
	}

	const token_kind_t kind = pp_token.kind;
	if (current_call == NULL || argument_expanding != NULL) {
		if (kind == T_IDENTIFIER) {
			symbol_t        *const symbol        = pp_token.base.symbol;
			pp_definition_t *const pp_definition = symbol->pp_definition;
			if (pp_definition != NULL && !pp_definition->is_expanding) {
				if (pp_definition->has_parameters) {

					/* check if next token is a '(' */
					whitespace_info_t old_info   = info;
					token_kind_t      next_token = peek_expansion();
					if (next_token == T_EOF) {
						info.at_line_begin  = false;
						info.had_whitespace = false;
						skip_whitespace();
						if (input.c == '(') {
							next_token = '(';
						}
					}

					if (next_token == '(') {
						if (current_expansion == NULL)
							expansion_pos = pp_token.base.source_position;
						next_preprocessing_token();
						assert(pp_token.kind == '(');

						pp_definition->parent_expansion = current_expansion;
						current_call              = pp_definition;
						current_call->expand_pos  = 0;
						current_call->expand_info = old_info;
						if (current_call->n_parameters > 0) {
							current_argument = &current_call->parameters[0];
							assert(argument_brace_count == 0);
						}
						goto restart;
					} else {
						/* skip_whitespaces() skipped newlines and whitespace,
						 * remember results for next token */
						next_info = info;
						info      = old_info;
						return;
					}
				} else {
					if (current_expansion == NULL)
						expansion_pos = pp_token.base.source_position;
					start_expanding(pp_definition);
					goto restart;
				}
			}
		} else if (kind == T_MACRO_PARAMETER) {
			assert(current_expansion != NULL);
			start_expanding(pp_token.macro_parameter.def);
			goto restart;
		}
	}

	if (current_call != NULL) {
		/* current_call != NULL */
		if (kind == '(') {
			++argument_brace_count;
		} else if (kind == ')') {
			if (argument_brace_count > 0) {
				--argument_brace_count;
			} else {
				finish_current_argument();
				assert(kind == ')');
				start_expanding(current_call);
				info = current_call->expand_info;
				current_call     = NULL;
				current_argument = NULL;
				goto restart;
			}
		} else if (kind == ',' && argument_brace_count == 0) {
			finish_current_argument();
			current_call->expand_pos++;
			if (current_call->expand_pos >= current_call->n_parameters) {
				errorf(&pp_token.base.source_position,
					   "too many arguments passed for macro '%Y'",
					   current_call->symbol);
				current_argument = NULL;
			} else {
				current_argument
					= &current_call->parameters[current_call->expand_pos];
			}
			goto restart;
		} else if (kind == T_MACRO_PARAMETER) {
			/* parameters have to be fully expanded before being used as
			 * parameters for another macro-call */
			assert(current_expansion != NULL);
			pp_definition_t *argument = pp_token.macro_parameter.def;
			argument_expanding = argument;
			start_expanding(argument);
			goto restart;
		} else if (kind == T_EOF) {
			errorf(&expansion_pos,
			       "reached end of file while parsing arguments for '%Y'",
			       current_call->symbol);
			return;
		}
		if (current_argument != NULL) {
			saved_token_t saved;
			saved.token = pp_token;
			saved.had_whitespace = info.had_whitespace;
			obstack_grow(&pp_obstack, &saved, sizeof(saved));
		}
		goto restart;
	}
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
				char *const string = obstack_copy0(&config_obstack, begin, len);
				prepend_include_path(string);
			}

			begin = c+1;
			/* skip : */
			if (*begin == ':')
				++begin;
		} while(*c != '\0');
	}
}

void init_preprocessor(void)
{
	init_symbols();

	obstack_init(&config_obstack);
	obstack_init(&pp_obstack);
	obstack_init(&input_obstack);
	strset_init(&stringset);

	setup_include_path();
}

void exit_preprocessor(void)
{
	obstack_free(&input_obstack, NULL);
	obstack_free(&pp_obstack, NULL);
	obstack_free(&config_obstack, NULL);

	strset_destroy(&stringset);
}

int pptest_main(int argc, char **argv);
int pptest_main(int argc, char **argv)
{
	init_symbol_table();
	init_preprocessor();
	init_tokens();

	error_on_unknown_chars   = false;
	resolve_escape_sequences = false;

	/* simplistic commandline parser */
	const char *filename = NULL;
	const char *output = NULL;
	for (int i = 1; i < argc; ++i) {
		const char *opt = argv[i];
		if (streq(opt, "-I")) {
			prepend_include_path(argv[++i]);
			continue;
		} else if (streq(opt, "-E")) {
			/* ignore */
		} else if (streq(opt, "-o")) {
			output = argv[++i];
			continue;
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

	if (output == NULL) {
		out = stdout;
	} else {
		out = fopen(output, "w");
		if (out == NULL) {
			fprintf(stderr, "Couldn't open output '%s'\n", output);
			return 1;
		}
	}

	/* just here for gcc compatibility */
	fprintf(out, "# 1 \"%s\"\n", filename);
	fprintf(out, "# 1 \"<built-in>\"\n");
	fprintf(out, "# 1 \"<command-line>\"\n");

	FILE *file = fopen(filename, "r");
	if (file == NULL) {
		fprintf(stderr, "Couldn't open input '%s'\n", filename);
		return 1;
	}
	switch_pp_input(file, filename);

	for (;;) {
		next_preprocessing_token();
		if (pp_token.kind == T_EOF)
			break;
		emit_pp_token();
	}

	fputc('\n', out);
	check_unclosed_conditionals();
	fclose(close_pp_input());
	if (out != stdout)
		fclose(out);

	exit_tokens();
	exit_preprocessor();
	exit_symbol_table();

	return 0;
}
