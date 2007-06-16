#include <config.h>

#include "lexer_t.h"
#include "token_t.h"
#include "symbol_table_t.h"
#include "adt/error.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

//#define DEBUG_CHARS
#define MAX_PUTBACK 3

static int               c;
source_position_t source_position;
static FILE             *input;
static char              buf[1027];
static const char       *bufend;
static const char       *bufpos;
static strset_t          stringset;
//static FILE            **input_stack;
//static char            **buf_stack;

static
void error_prefix_at(const char *input_name, unsigned linenr)
{
	fprintf(stderr, "%s:%d: Error: ", input_name, linenr);
}

static
void error_prefix()
{
	error_prefix_at(source_position.input_name, source_position.linenr);
}

static
void parse_error(const char *msg)
{
	error_prefix();
	fprintf(stderr, "%s\n", msg);
}

static inline
void next_char()
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
#ifdef DEBUG_CHARS
	printf("nchar '%c'\n", c);
#endif
}

static inline
void put_back(int pc)
{
	char *p = (char*) bufpos - 1;
	bufpos--;
	assert(p >= buf);
	*p = pc;

#ifdef DEBUG_CHARS
	printf("putback '%c'\n", pc);
#endif
}


static
int replace_trigraph(void)
{
#define MATCH_TRIGRAPH(ch,replacement)           \
	case ch:                                     \
		c = replacement;                         \
		return 1;

	switch(c) {
	MATCH_TRIGRAPH('=', '#')
	MATCH_TRIGRAPH('(', '[')
	MATCH_TRIGRAPH('/', '\\')
	MATCH_TRIGRAPH(')', ']')
	MATCH_TRIGRAPH('\'', '^')
	MATCH_TRIGRAPH('<', '{')
	MATCH_TRIGRAPH('!', '|')
	MATCH_TRIGRAPH('>', '}')
	MATCH_TRIGRAPH('-', '~')
	default:
		break;
	}

	return 0;
}

#define SKIP_TRIGRAPHS(custom_putback, no_trigraph_code) \
	case '?':                                  \
		next_char();                           \
		if(c != '?') {                         \
			custom_putback;                    \
			put_back(c);                       \
			c = '?';                           \
			no_trigraph_code;                  \
		}                                      \
		next_char();                           \
		if(replace_trigraph()) {               \
			break;                             \
		}                                      \
		custom_putback;                        \
		put_back('?');                         \
		put_back(c);                           \
		c = '?';                               \
		no_trigraph_code;

#define EAT_NEWLINE(newline_code)              \
	if(c == '\r') {                            \
		next_char();                           \
		if(c == '\n')                          \
			next_char();                       \
		source_position.linenr++;              \
		newline_code;                          \
	} else if(c == '\n') {                     \
		next_char();                           \
		source_position.linenr++;              \
		newline_code;                          \
	}

static
void parse_symbol(token_t *token)
{
	symbol_t *symbol;
	char     *string;

	obstack_1grow(&symbol_obstack, c);
	next_char();

	while(1) {
		switch(c) {
		case '\\':
			next_char();
			EAT_NEWLINE(break;)
			goto end_symbol;

		case 'A' ... 'Z':
		case 'a' ... 'z':
		case '_':
			obstack_1grow(&symbol_obstack, c);
			next_char();
			break;

		case '?':
			next_char();
			if(c != '?') {
				put_back(c);
				c = '?';
				goto end_symbol;
			}
			next_char();
			if(replace_trigraph())
				break;
			put_back('?');
			put_back(c);
			c = '?';
			goto end_symbol;

		default:
			goto end_symbol;
		}
	}
end_symbol:
	obstack_1grow(&symbol_obstack, '\0');

	string = obstack_finish(&symbol_obstack);
	symbol = symbol_table_insert(string);

	if(symbol->ID > 0) {
		token->type = symbol->ID;
	} else {
		token->type = T_IDENTIFIER;
	}
	token->v.symbol = symbol;

	if(symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}
}

static
void parse_number_hex(token_t *token)
{
	assert(c == 'x' || c == 'X');
	next_char();

	if (!isdigit(c) &&
		!('A' <= c && c <= 'F') &&
		!('a' <= c && c <= 'f')) {
		parse_error("premature end of hex number literal");
		token->type = T_ERROR;
		return;
	}

	int value = 0;
	for(;;) {
		if (isdigit(c)) {
			value = 16 * value + c - '0';
		} else if ('A' <= c && c <= 'F') {
			value = 16 * value + c - 'A' + 10;
		} else if ('a' <= c && c <= 'f') {
			value = 16 * value + c - 'a' + 10;
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char();
	}
}

static
void parse_number_oct(token_t *token)
{
	assert(c == 'o' || c == 'O');
	next_char();

	int value = 0;
	for(;;) {
		if ('0' <= c && c <= '7') {
			value = 8 * value + c - '0';
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char();
	}
}

static
void parse_number_dec(token_t *token, int first_char)
{
	int value = 0;
	if(first_char > 0) {
		assert(first_char >= '0' && first_char <= '9');
		value = first_char - '0';
	}

	for(;;) {
		if (isdigit(c)) {
			value = 10 * value + c - '0';
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char();
	}
}

static
void parse_number(token_t *token)
{
	// TODO check for overflow
	// TODO check for various invalid inputs sequences

	if (c == '0') {
		next_char();
		switch (c) {
			case 'X':
			case 'x': parse_number_hex(token); break;
			case 'o':
			case 'O': parse_number_oct(token); break;
			default:  parse_number_dec(token, '0');
		}
	} else {
		parse_number_dec(token, 0);
	}
}

static
int parse_escape_sequence()
{
	while(1) {
		int ec = c;
		next_char();

		switch(ec) {
		case '"': return '"';
		case '\'': return'\'';
		case '\\':
			EAT_NEWLINE(break;)
			return '\\';
		case 'a': return '\a';
		case 'b': return '\b';
		case 'f': return '\f';
		case 'n': return '\n';
		case 'r': return '\r';
		case 't': return '\t';
		case 'v': return '\v';
		case 'x': /* TODO parse hex number ... */
			parse_error("hex escape sequences not implemented yet");
			return EOF;
		case 0 ... 8: /* TODO parse octal number ... */
			parse_error("octal escape sequences not implemented yet");
			return EOF;
		case '?':
			if(c != '?') {
				return '?';
			}
			/* might be a trigraph */
			next_char();
			if(replace_trigraph()) {
				break;
			}
			put_back(c);
			c = '?';
			return '?';

		case EOF:
			parse_error("reached end of file while parsing escape sequence");
			return EOF;
		default:
			parse_error("unknown escape sequence");
			return EOF;
		}
	}
}

static
void parse_string_literal(token_t *token)
{
	unsigned    start_linenr = source_position.linenr;
	char       *string;
	const char *result;

	assert(c == '"');
	next_char();

	while(1) {
		switch(c) {
		SKIP_TRIGRAPHS(,
			obstack_1grow(&symbol_obstack, '?');
			next_char();
			break;
		)

		case '\\':
			next_char();
			EAT_NEWLINE(break;)
			int ec = parse_escape_sequence();
			obstack_1grow(&symbol_obstack, ec);
			break;

		case EOF:
			error_prefix_at(source_position.input_name, start_linenr);
			fprintf(stderr, "string has no end\n");
			token->type = T_ERROR;
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

	token->type     = T_STRING_LITERAL;
	token->v.string = result;
}

#define MATCH_NEWLINE(code)                 \
	case '\r':                              \
		next_char();                        \
		if(c == '\n') {                     \
			next_char();                    \
		}                                   \
		source_position.linenr++;           \
		code;                               \
	case '\n':                              \
		next_char();                        \
		source_position.linenr++;           \
		code;

static
void parse_character_constant(token_t *token)
{
	assert(c == '\'');
	next_char();

	int found_char = 0;
	while(1) {
		switch(c) {
		SKIP_TRIGRAPHS(,
			found_char = '?';
			break;
		)

		case '\\':
			next_char();
			EAT_NEWLINE(break;)
			found_char = '\\';
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
			token->type = T_ERROR;
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
	token->type       = T_INTEGER;
	token->v.intvalue = found_char;
}

static
void skip_multiline_comment(void)
{
	unsigned start_linenr = source_position.linenr;
	int had_star = 0;

	while(1) {
		switch(c) {
		case '*':
			next_char();
			had_star = 1;
			break;

		case '/':
			next_char();
			if(had_star) {
				return;
			}
			had_star = 0;
			break;

		case '\\':
			next_char();
			EAT_NEWLINE(break;)
			had_star = 0;
			break;

		case '?':
			next_char();
			if(c != '?') {
				had_star = 0;
				break;
			}
			next_char();
			if(replace_trigraph())
				break;
			put_back(c);
			c = '?';
			had_star = 0;
			/* we don't put back the 2nd ? as the comment text is discarded
			 * anyway */
			break;

		MATCH_NEWLINE(had_star = 0; break;)

		case EOF:
			error_prefix_at(source_position.input_name, start_linenr);
			fprintf(stderr, "at end of file while looking for comment end\n");
			return;
		default:
			had_star = 0;
			next_char();
			break;
		}
	}
}

static
void skip_line_comment(void)
{
	while(1) {
		switch(c) {
		case '?':
			next_char();
			if(c != '?')
				break;
			next_char();
			if(replace_trigraph())
				break;
			put_back('?');
			/* we don't put back the 2nd ? as the comment text is discarded
			 * anyway */
			break;

		case '\\':
			next_char();
			if(c == '\n') {
				next_char();
				source_position.linenr++;
			}
			break;

		case EOF:
		case '\r':
		case '\n':
			return;

		default:
			next_char();
			break;
		}
	}
}

static
void lexer_next_preprocessing_token(token_t *token);

static
void eat_until_newline(void)
{
	/* TODO */
}

static
void error_directive(void)
{
	error_prefix();
	fprintf(stderr, "#error directive: \n");

	/* parse pp-tokens until new-line */
}

static
void define_directive(void)
{
	token_t temptoken;

	lexer_next_preprocessing_token(&temptoken);
	if(temptoken.type != T_IDENTIFIER) {
		parse_error("expected identifier after #define\n");
		eat_until_newline();
	}
}

static
void ifdef_directive(int is_ifndef)
{
	(void) is_ifndef;
	token_t temptoken;
	lexer_next_preprocessing_token(&temptoken);
	//expect_identifier();
	//extect_newline();
}

static
void endif_directive(void)
{
	//expect_newline();
}

static
void found_preprocessor_identifier(symbol_t *symbol)
{
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
	case TP_if:
	case TP_else:
	case TP_elif:
	case TP_undef:
	case TP_line:
	case TP_error:
		error_directive();
		break;
	case TP_pragma:
		break;
	}
}

static
void parse_preprocessor_directive(token_t *result_token)
{
	token_t temptoken;

	(void) result_token;
	lexer_next_preprocessing_token(&temptoken);
	switch(temptoken.type) {
	case T_IDENTIFIER:
		found_preprocessor_identifier(temptoken.v.symbol);
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
					token->type = set_type;                \
					return;

#define ELSE_CODE(code)                                    \
				SKIP_TRIGRAPHS(,                           \
					code;                                  \
				)                                          \
														   \
				case '\\':                                 \
					next_char();                           \
					EAT_NEWLINE(break;)                    \
					/* fallthrough */                      \
				default:                                   \
					code;                                  \
				}                                          \
			} /* end of while(1) */                        \
			break;

#define ELSE(set_type)                                     \
		ELSE_CODE(                                         \
			token->type = set_type;                        \
			return;                                        \
		)

static
void eat_whitespace()
{
	while(1) {
		switch(c) {
		case ' ':
		case '\t':
			next_char();
			break;

		case '\r':
		case '\n':
			return;

		case '\\':
			next_char();
			if(c == '\n') {
				next_char();
				source_position.linenr++;
				break;
			}

			put_back(c);
			c = '\\';
			return;

		SKIP_TRIGRAPHS(,
			return;
		)

		case '/':
			next_char();
			while(1) {
				switch(c) {
				case '*':
					next_char();
					skip_multiline_comment();
					eat_whitespace();
					return;
				case '/':
					next_char();
					skip_line_comment();
					eat_whitespace();
					return;

				SKIP_TRIGRAPHS(
						put_back('?');
					,
						c = '/';
						return;
				)

				case '\\':
					next_char();
					EAT_NEWLINE(break;)
					/* fallthrough */
				default:
					return;
				}
			}
			break;

		default:
			return;
		}
	}
}

static
void lexer_next_preprocessing_token(token_t *token)
{
	while(1) {
		switch(c) {
		case ' ':
		case '\t':
			next_char();
			break;

		MATCH_NEWLINE(
			eat_whitespace();
			if(c == '#') {
				next_char();
				parse_preprocessor_directive(token);
				return;
			}
			token->type = '\n';
			return;
		)

		case 'A' ... 'Z':
		case 'a' ... 'z':
		case '_':
			parse_symbol(token);
			return;

		case '0' ... '9':
			parse_number(token);
			return;

		case '"':
			parse_string_literal(token);
			return;

		case '\'':
			parse_character_constant(token);
			return;

		case '\\':
			next_char();
			if(c == '\n') {
				next_char();
				source_position.linenr++;
				break;
			} else {
				parse_error("unexpected '\\' found");
				token->type = T_ERROR;
			}
			return;

		case '.':
			MAYBE_PROLOG
				case '.':
					MAYBE_PROLOG
					MAYBE('.', T_DOTDOTDOT)
					ELSE_CODE(
						put_back(c);
						c = '.';
						token->type = '.';
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
					lexer_next_preprocessing_token(token);
					return;
				case '/':
					next_char();
					skip_line_comment();
					lexer_next_preprocessing_token(token);
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
								token->type = T_PERCENTCOLON;
								return;
							)
					ELSE(T_PERCENTCOLON)
			ELSE('%')
		case '<':
			MAYBE_PROLOG
			MAYBE(':', T_LESSCOLON)
			MAYBE('%', T_LESSPERCENT)
				case '<':
					MAYBE_PROLOG
					MAYBE('=', T_LESSLESSEQUAL)
					ELSE(T_LESSLESS)
			ELSE('<')
		case '>':
			MAYBE_PROLOG
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
			next_char();
			/* just a simple ? */
			if(c != '?') {
				token->type = '?';
				return;
			}
			/* might be a trigraph */
			next_char();
			if(replace_trigraph()) {
				break;
			}
			put_back(c);
			c = '?';
			token->type = '?';
			return;

		case '[':
		case ']':
		case '(':
		case ')':
		case '{':
		case '}':
		case '~':
		case ';':
		case ',':
			token->type = c;
			next_char();
			return;

		case EOF:
			token->type = T_EOF;
			return;

		default:
			next_char();
			error_prefix();
			fprintf(stderr, "unknown character '%c' found\n", c);
			token->type = T_ERROR;
			return;
		}
	}
}

void lexer_next_token(token_t *token)
{
	do {
		lexer_next_preprocessing_token(token);
	} while(token->type == '\n');
}

void init_lexer(void)
{
	strset_init(&stringset);
}

void lexer_open_stream(FILE *stream, const char *input_name)
{
	input                      = stream;
	source_position.linenr     = 0;
	source_position.input_name = input_name;

	/* we place a virtual '\n' at the beginning so the lexer knows we're at the
	 * beginning of a line */
	c = '\n';
}

void exit_lexer(void)
{
	strset_destroy(&stringset);
}

static __attribute__((unused))
void dbg_pos(const source_position_t source_position)
{
	fprintf(stdout, "%s:%d\n", source_position.input_name, source_position.linenr);
	fflush(stdout);
}
