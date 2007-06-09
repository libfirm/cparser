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
#define MAX_PUTBACK 2

static
void error_prefix_at(lexer_t *this, const char *input_name, unsigned linenr)
{
	(void) this;
	fprintf(stderr, "%s:%d: Error: ", input_name, linenr);
}

static
void error_prefix(lexer_t *this)
{
	error_prefix_at(this, this->source_position.input_name,
	                this->source_position.linenr);
}

static
void parse_error(lexer_t *this, const char *msg)
{
	error_prefix(this);
	fprintf(stderr, "%s\n", msg);
}

static inline
void next_char(lexer_t *this)
{
	this->bufpos++;
	if(this->bufpos >= this->bufend) {
		size_t s = fread(this->buf + MAX_PUTBACK, 1,
		                 sizeof(this->buf) - MAX_PUTBACK, this->input);
		if(s == 0) {
			this->c = EOF;
			return;
		}
		this->bufpos = this->buf + MAX_PUTBACK;
		this->bufend = this->buf + MAX_PUTBACK + s;
	}
	this->c = *(this->bufpos);
#ifdef DEBUG_CHARS
	printf("nchar '%c'\n", this->c);
#endif
}

static inline
void put_back(lexer_t *this, int c)
{
	char *p = (char*) this->bufpos - 1;
	this->bufpos--;
	assert(p >= this->buf);
	*p = c;

#ifdef DEBUG_CHARS
	printf("putback '%c'\n", c);
#endif
}

static
int replace_trigraph(lexer_t *this)
{
#define MATCH_TRIGRAPH(ch,replacement)           \
	case ch:                                     \
		this->c = replacement;                   \
		return 1;

	switch(this->c) {
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

#define SKIP_TRIGRAPHS(no_trigraph_code)       \
	case '?':                                  \
		next_char(this);                       \
		if(this->c != '?') {                   \
			put_back(this, this->c);           \
			this->c = '?';                     \
			no_trigraph_code;                  \
		}                                      \
		next_char(this);                       \
		if(replace_trigraph(this))             \
			break;                             \
		put_back(this, '?');                   \
		put_back(this, this->c);               \
		this->c = '?';                         \
		no_trigraph_code;                      \

static
void parse_symbol(lexer_t *this, token_t *token)
{
	symbol_t *symbol;
	char     *string;

	obstack_1grow(&symbol_obstack, this->c);
	next_char(this);

	while(1) {
		switch(this->c) {
		case '\\':
			next_char(this);
			if(this->c == '\n') {
				next_char(this);
				this->source_position.linenr++;
				break;
			}

		case 'A' ... 'Z':
		case 'a' ... 'z':
		case '_':
			obstack_1grow(&symbol_obstack, this->c);
			next_char(this);
			break;

		case '?':
			next_char(this);
			if(this->c != '?') {
				put_back(this, this->c);
				this->c = '?';
				goto end_symbol;
			}
			next_char(this);
			if(replace_trigraph(this))
				break;
			put_back(this, '?');
			put_back(this, this->c);
			this->c = '?';
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

#if 0
static
preprocessor_token_type_t parse_pp_symbol(lexer_t *this)
{
	do {
		obstack_1grow(&symbol_obstack, this->c);
		next_char(this);
	} while(is_ident_char(this->c));
	obstack_1grow(&symbol_obstack, '\0');

	char     *string = obstack_finish(&symbol_obstack);
	symbol_t *symbol = preprocessor_symbol_table_find(string);
	obstack_free(&symbol_obstack, string);

	if(symbol == 0)
		return TP_ERROR;

	return symbol->ID;
}
#endif

static
void parse_number_hex(lexer_t *this, token_t *token)
{
	assert(this->c == 'x' || this->c == 'X');
	next_char(this);

	if (!isdigit(this->c) &&
		!('A' <= this->c && this->c <= 'F') &&
		!('a' <= this->c && this->c <= 'f')) {
		parse_error(this, "premature end of hex number literal");
		token->type = T_ERROR;
		return;
	}

	int value = 0;
	for(;;) {
		if (isdigit(this->c)) {
			value = 16 * value + this->c - '0';
		} else if ('A' <= this->c && this->c <= 'F') {
			value = 16 * value + this->c - 'A' + 10;
		} else if ('a' <= this->c && this->c <= 'f') {
			value = 16 * value + this->c - 'a' + 10;
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char(this);
	}
}

static
void parse_number_oct(lexer_t *this, token_t *token)
{
	assert(this->c == 'o' || this->c == 'O');
	next_char(this);

	int value = 0;
	for(;;) {
		if ('0' <= this->c && this->c <= '7') {
			value = 8 * value + this->c - '0';
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char(this);
	}
}

static
void parse_number_dec(lexer_t *this, token_t *token, int first_char)
{
	int value = 0;
	if(first_char > 0) {
		assert(first_char >= '0' && first_char <= '9');
		value = first_char - '0';
	}

	for(;;) {
		if (isdigit(this->c)) {
			value = 10 * value + this->c - '0';
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char(this);
	}
}

static
void parse_number(lexer_t *this, token_t *token)
{
	// TODO check for overflow
	// TODO check for various invalid inputs sequences

	if (this->c == '0') {
		next_char(this);
		switch (this->c) {
			case 'X':
			case 'x': parse_number_hex(this, token); break;
			case 'o':
			case 'O': parse_number_oct(this, token); break;
			default:  parse_number_dec(this, token, '0');
		}
	} else {
		parse_number_dec(this, token, 0);
	}
}

static
int parse_escape_sequence(lexer_t *this)
{
	int c = this->c;
	next_char(this);

	switch(c) {
	case '"': return '"';
	case '\'': return'\'';
	case '\\':
		if(this->c == '\n') {
			this->source_position.linenr++;
			next_char(this);
			return parse_escape_sequence(this);
		}
	  	return '\\';
	case 'a': return '\a';
	case 'b': return '\b';
	case 'f': return '\f';
	case 'n': return '\n';
	case 'r': return '\r';
	case 't': return '\t';
	case 'v': return '\v';
	case 'x': /* TODO parse hex number ... */
		parse_error(this, "hex escape sequences not implemented yet");
		return EOF;
	case 0 ... 8: /* TODO parse octal number ... */
		parse_error(this, "octal escape sequences not implemented yet");
		return EOF;
	case '?':
		if(this->c != '?') {
			return '?';
		}
		/* might be a trigraph */
		next_char(this);
		if(replace_trigraph(this)) {
			return parse_escape_sequence(this);
		}
		put_back(this, this->c);
		this->c = '?';
		return '?';

	case EOF:
		parse_error(this, "reached end of file while parsing escape sequence");
		return EOF;
	default:
		parse_error(this, "unknown escape sequence\n");
		return EOF;
	}
}

static
void parse_string_literal(lexer_t *this, token_t *token)
{
	unsigned    start_linenr = this->source_position.linenr;
	char       *string;
	const char *result;

	assert(this->c == '"');
	next_char(this);

	while(1) {
		switch(this->c) {
		SKIP_TRIGRAPHS(
			obstack_1grow(&symbol_obstack, '?');
			next_char(this);
			break;
		)

		case '\\':
			next_char(this);
			if(this->c == '\n') {
				next_char(this);
				this->source_position.linenr++;
				break;
			}
			int c = parse_escape_sequence(this);
			obstack_1grow(&symbol_obstack, c);
			break;

		case EOF:
			error_prefix_at(this, this->source_position.input_name,
			                start_linenr);
			fprintf(stderr, "string has no end\n");
			token->type = T_ERROR;
			return;

		case '"':
			next_char(this);
			goto end_of_string;

		default:
			obstack_1grow(&symbol_obstack, this->c);
			next_char(this);
			break;
		}
	}

end_of_string:

	/* TODO: concatenate multiple strings separated by whitespace... */

	/* add finishing 0 to the string */
	obstack_1grow(&symbol_obstack, '\0');
	string = obstack_finish(&symbol_obstack);

	/* check if there is already a copy of the string */
	result = strset_insert(&this->stringset, string);
	if(result != string) {
		obstack_free(&symbol_obstack, string);
	}

	token->type     = T_STRING_LITERAL;
	token->v.string = result;
}

static
void parse_character_constant(lexer_t *this, token_t *token)
{
	assert(this->c == '\'');
	next_char(this);

	int found_char = 0;
	while(1) {
		switch(this->c) {
		SKIP_TRIGRAPHS(
			found_char = '?';
			break;
		)

		case '\\':
			next_char(this);
			if(this->c == '\n') {
				next_char(this);
				this->source_position.linenr++;
				break;
			}
			found_char = '\\';
			break;

		case '\n':
			next_char(this);
			parse_error(this, "newline while parsing character constant");
			this->source_position.linenr++;
			break;

		case '\'':
			next_char(this);
			goto end_of_char_constant;

		case EOF:
			parse_error(this, "EOF while parsing character constant");
			token->type = T_ERROR;
			return;

		default:
			if(found_char != 0) {
				parse_error(this, "more than 1 characters in character "
				            "constant");
				goto end_of_char_constant;
			} else {
				found_char = this->c;
				next_char(this);
			}
			break;
		}
	}

end_of_char_constant:
	token->type       = T_INTEGER;
	token->v.intvalue = found_char;
}

static
void skip_multiline_comment(lexer_t *this)
{
	unsigned start_linenr = this->source_position.linenr;

	while(1) {
		switch(this->c) {
		case '*':
			next_char(this);
			if(this->c == '/') {
				next_char(this);
				return;
			}
			break;
		case '?':
			next_char(this);
			if(this->c != '?')
				break;
			next_char(this);
			if(replace_trigraph(this))
				break;
			put_back(this, '?');
			/* we don't put back the 2nd ? as the comment text is discarded
			 * anyway */
			break;

		case '\n':
			this->source_position.linenr++;
			next_char(this);
			break;
		case EOF:
			error_prefix_at(this, this->source_position.input_name,
			                start_linenr);
			fprintf(stderr, "at end of file while looking for comment end\n");
			return;
		default:
			next_char(this);
			break;
		}
	}
}

static
void skip_line_comment(lexer_t *this)
{
	while(1) {
		switch(this->c) {
		case '?':
			next_char(this);
			if(this->c != '?')
				break;
			next_char(this);
			if(replace_trigraph(this))
				break;
			put_back(this, '?');
			/* we don't put back the 2nd ? as the comment text is discarded
			 * anyway */
			break;

		case '\\':
			next_char(this);
			if(this->c == '\n') {
				next_char(this);
				this->source_position.linenr++;
			}
			break;

		case EOF:
		case '\n':
			return;
		default:
			next_char(this);
			break;
		}
	}
}

#if 0
static
void eat_until_newline(lexer_t *this)
{
	while(this->c != '\n') {
		next_char(this);
	}
}
#endif

#if 0
static
void parse_preprocessor_directive(lexer_t *this, token_t *result_token)
{
	(void) result_token;
	/* skip whitespaces */
	while(this->c == ' ' || this->c == '\t' || this->c == '\r') {
		next_char(this);
	}
}
#endif

void preprocessor_next_token(lexer_t *this, token_t *token)
{
	/* skip whitespaces */
	while(this->c == ' ' || this->c == '\t' || this->c == '\r') {
		next_char(this);
	}

	switch(this->c) {
	case 'A' ... 'Z':
	case 'a' ... 'z':
	case '_':
		parse_symbol(this, token);
	}
}

void lexer_next_token(lexer_t *this, token_t *token)
{
	while(1) {
		switch(this->c) {
		case ' ':
		case '\t':
		case '\r':
			next_char(this);
			break;

		case '\n':
			this->source_position.linenr++;
			next_char(this);
			break;

		case 'A' ... 'Z':
		case 'a' ... 'z':
		case '_':
			parse_symbol(this, token);
			return;

		case '0' ... '9':
			parse_number(this, token);
			return;

		case '"':
			parse_string_literal(this, token);
			return;

		case '\'':
			parse_character_constant(this, token);
			return;

		case '\\':
			next_char(this);
			if(this->c == '\n') {
				next_char(this);
				this->source_position.linenr++;
				break;
			} else {
				parse_error(this, "unexpected '\\' found");
				token->type = T_ERROR;
			}
			return;

#define MAYBE_PROLOG                                       \
			next_char(this);                               \
			while(1) {                                     \
				switch(this->c) {

#define MAYBE(ch, set_type)                                \
				case ch:                                   \
					next_char(this);                       \
					token->type = set_type;                \
					return;

#define ELSE_CODE(code)                                    \
				SKIP_TRIGRAPHS(                            \
					code;                                  \
				)                                          \
														   \
				case '\\':                                 \
					next_char(this);                       \
					if(this->c == '\n') {                  \
						next_char(this);                   \
						this->source_position.linenr++;    \
						break;                             \
					}                                      \
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

		case '.':
			MAYBE_PROLOG
				case '.':
					MAYBE_PROLOG
					MAYBE('.', T_DOTDOTDOT)
					ELSE_CODE(
						put_back(this, this->c);
						this->c = '.';
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
					next_char(this);
					skip_multiline_comment(this);
					lexer_next_token(this, token);
					return;
				case '/':
					next_char(this);
					skip_line_comment(this);
					lexer_next_token(this, token);
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
								put_back(this, this->c);
								this->c = '%';
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
#if 0
			else {
				if(line_begin) {
					parse_preprocessor_directive(this, token);
					return;
				} else {
					token->type = '#';
				}
#else
			ELSE('#')
#endif

		case '?':
			next_char(this);
			/* just a simple ? */
			if(this->c != '?') {
				token->type = '?';
				return;
			}
			/* might be a trigraph */
			next_char(this);
			if(replace_trigraph(this)) {
				break;
			}
			put_back(this, this->c);
			this->c = '?';
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
			token->type = this->c;
			next_char(this);
			return;

		case EOF:
			token->type = T_EOF;
			return;

		default:
			next_char(this);
			error_prefix(this);
			fprintf(stderr, "unknown character '%c' found\n", this->c);
			token->type = T_ERROR;
			return;
		}
	}
}

void lexer_init(lexer_t *this, FILE *stream, const char *input_name)
{
	memset(this, 0, sizeof(this[0]));

	this->input = stream;

	this->source_position.linenr     = 0;
	this->source_position.input_name = input_name;
	strset_init(&this->stringset);

	/* we place a virtual '\n' at the beginning so the lexer knows we're at the
	 * beginning of a line */
	this->c = '\n';
}

void lexer_destroy(lexer_t *this)
{
	(void) this;
}

static __attribute__((unused))
void dbg_pos(const source_position_t source_position)
{
	fprintf(stdout, "%s:%d\n", source_position.input_name, source_position.linenr);
	fflush(stdout);
}
