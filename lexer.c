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
#define MAX_PUTBACK 1

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
		put_back(this, '?');
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
		case '?':
			next_char(this);
			if(this->c != '?') {
				obstack_1grow(&symbol_obstack, '?');
				break;
			}
			next_char(this);
			if(replace_trigraph(this))
				break;
			obstack_1grow(&symbol_obstack, '?');
			put_back(this, this->c);
			this->c = '?';
			break;

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
	int line_begin = 0;

	/* skip whitespaces */
	while(this->c == ' ' || this->c == '\t' || this->c == '\n'
	      || this->c == '\r') {
		if(this->c == '\n') {
			line_begin = 1;
			this->source_position.linenr++;
		}
		next_char(this);
	}

	switch(this->c) {
	case 'A' ... 'Z':
	case 'a' ... 'z':
	case '_':
		parse_symbol(this, token);
		break;

	case '0' ... '9':
		parse_number(this, token);
		break;

	case '"':
		parse_string_literal(this, token);
		break;

	case '\'':
		next_char(this);
		if(this->c == '\\') {
			next_char(this);
			token->type       = T_INTEGER;
			token->v.intvalue = parse_escape_sequence(this);
		} else {
			if(this->c == '\n') {
				parse_error(this, "newline while parsing character constant");
				this->source_position.linenr++;
			}
			token->type       = T_INTEGER;
			token->v.intvalue = this->c;
			next_char(this);
		}
		if(this->c != '\'') {
			parse_error(this, "multibyte character constant");
			token->type = T_ERROR;
		} else {
			next_char(this);
		}
		break;

	case '\\':
		next_char(this);
		if(this->c == '\n') {
			next_char(this);
			this->source_position.linenr++;
			lexer_next_token(this, token);
			return;
		} else {
			parse_error(this, "unexpected '\\' found");
			token->type = T_ERROR;
		}
		break;

#define MAYBE1(ch, set_type)                           \
		next_char(this);                               \
		while(1) {                                     \
			switch(this->c) {                          \
			case ch:                                   \
				next_char(this);                       \
				token->type = set_type;                \
				return;                                \

#define MAYBE(ch, set_type)                            \
			case ch:                                   \
				next_char(this);                       \
				token->type = set_type;                \
				return;

#define ELSE(set_type)                                 \
			case '?':                                  \
				next_char(this);                       \
				if(this->c != '?') {                   \
					put_back(this, this->c);           \
					this->c = '?';                     \
					token->type = set_type;            \
					return;                            \
				}                                      \
				next_char(this);                       \
				if(replace_trigraph(this))             \
					break;                             \
				put_back(this, '?');                   \
				put_back(this, this->c);               \
				this->c = '?';                         \
				token->type = set_type;                \
				return;                                \
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
				token->type = set_type;                \
				return;                                \
			}                                          \
		} /* end of while(1) */                        \
		break;

	case '.':
		next_char(this);
		if(this->c == '.') {
			next_char(this);
			if(this->c == '.') {
				next_char(this);
				token->type = T_DOTDOTDOT;
			} else {
				put_back(this, '.');
				token->type = '.';
			}
		} else {
			token->type = '.';
		}
		break;
	case '&':
		MAYBE1('&', T_ANDAND)
		MAYBE('=', T_ANDEQUAL)
		ELSE('&')
	case '*':
		MAYBE1('=', T_ASTERISKEQUAL)
		ELSE('*')
	case '+':
		MAYBE1('+', T_PLUSPLUS)
		MAYBE('=', T_PLUSEQUAL)
		ELSE('+')
	case '-':
		MAYBE1('-', T_MINUSMINUS)
		MAYBE('=', T_MINUSEQUAL)
		ELSE('-')
	case '!':
		MAYBE1('=', T_EXCLAMATIONMARKEQUAL)
		ELSE('!')
	case '/':
		MAYBE1('=', T_SLASHEQUAL)
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
		MAYBE1('=', T_PERCENTEQUAL)
			case ':':
				/* TODO find trigraphs... */
				next_char(this);
				if(this->c == '%') {
					next_char(this);
					if(this->c == ':') {
						next_char(this);
						token->type = T_PERCENTCOLONPERCENTCOLON;
					} else {
						put_back(this, '%');
						token->type = T_PERCENTCOLON;
					}
					return;
				}
				token->type = T_PERCENTCOLON;
				return;
		MAYBE('>', T_PERCENTGREATER)
		ELSE('%')
	case '<':
		MAYBE1(':', T_LESSCOLON)
		MAYBE('%', T_LESSPERCENT)
			case '<':
				/* TODO trigraphs... */
				next_char(this);
				if(this->c == '<') {
					next_char(this);
					if(this->c == '=') {
						next_char(this);
						token->type = T_LESSLESSEQUAL;
					} else {
						token->type = T_LESSLESS;
					}
				} else {
					token->type = T_LESS;
				}
				return;
		ELSE('<')
	case '>':
		next_char(this);
		while(1) {
			switch(this->c) {
			case '>':
				next_char(this);
				/* TODO trigraphs */
				if(this->c == '=') {
					next_char(this);
					token->type = T_GREATERGREATEREQUAL;
				} else {
					token->type = T_GREATERGREATER;
				}
				break;
		ELSE('>')
	case '^':
		MAYBE1('=', T_CARETEQUAL)
		ELSE('^')
	case '|':
		MAYBE1('=', T_PIPEEQUAL)
		MAYBE('|', T_PIPEPIPE)
		ELSE('|')
	case ':':
		MAYBE1('>', T_COLONGREATER)
		ELSE(':')
	case '=':
		MAYBE1('=', T_EQUALEQUAL)
		ELSE('=')
	case '#':
		MAYBE1('#', T_HASHHASH)
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
			break;
		}
		/* might be a trigraph */
		next_char(this);
		if(replace_trigraph(this)) {
			lexer_next_token(this, token);
			return;
		}
		put_back(this, this->c);
		this->c = '?';
		token->type = '?';
		break;

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
		break;

	case EOF:
		token->type = T_EOF;
		break;

	default:
		error_prefix(this);
		fprintf(stderr, "unknown character '%c' found\n", this->c);
		token->type = T_ERROR;
		next_char(this);
		break;
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
