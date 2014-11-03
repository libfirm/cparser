/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "driver/enable_posix.h"
#include "preprocessor.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <libfirm/firm_common.h>
#include <libfirm/irmode.h>
#include <libfirm/tv.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

#include "adt/array.h"
#include "adt/error.h"
#include "adt/pset_new.h"
#include "adt/separator_t.h"
#include "adt/strutil.h"
#include "adt/unicode.h"
#include "adt/util.h"
#include "ast/ast_t.h"
#include "ast/string_hash.h"
#include "ast/string_rep.h"
#include "ast/symbol_table_t.h"
#include "ast/symbol_t.h"
#include "driver/diagnostic.h"
#include "driver/lang_features.h"
#include "input.h"

#define MAX_PUTBACK   3
#define BUFSIZE       1024
#define INCLUDE_LIMIT 199  /* 199 is for gcc "compatibility" */

typedef struct whitespace_info_t {
	/** number of spaces before the first token in a line */
	unsigned whitespace_at_line_begin;
	/** current token is at the beginning of a line.
	 * => a "#" at line begin starts a preprocessing directive. */
	bool     at_line_begin;
} whitespace_info_t;

/* used to update "dynamic" definitions like __LINE__/__FILE__ */
typedef void (*update_func)(pp_definition_t *definition);

struct pp_definition_t {
	symbol_t        *symbol;
	pp_definition_t *function_definition;
	pp_definition_t *previous_definition;
	size_t           n_parameters;
	pp_definition_t *parameters;
	update_func      update;

	/* replacement */
	size_t           list_len;
	token_t         *token_list;

	position_t       pos;
	bool             is_expanding    : 1;
	bool             may_recurse     : 1;
	bool             has_parameters  : 1;
	bool             is_parameter    : 1;
	bool             is_variadic     : 1;
	bool             standard_define : 1;
	bool             not_specified   : 1;
};

typedef struct pp_expansion_state_t {
	pp_definition_t   *definition;
	size_t             list_len;
	token_t           *token_list;

	size_t             pos;
	/** the obstack level at the beginning of a macro call with parameters.
	 * We can free the obstack until this level after expanding the call */
	void              *obstack_level;

	whitespace_info_t  expand_info;
	bool               previous_is_expanding;
	bool               previous_may_recurse;
} pp_expansion_state_t;

typedef struct pp_argument_t {
	size_t   list_len;
	token_t *token_list;
	bool     is_expanding;
} pp_argument_t;

typedef struct pp_conditional_t pp_conditional_t;
struct pp_conditional_t {
	pp_conditional_t *parent;
	position_t        pos;
	bool              condition : 1;
	bool              in_else   : 1;
	/** conditional in skip mode (then+else gets skipped) */
	bool              skip      : 1;
};

typedef struct pp_input_t pp_input_t;
struct pp_input_t {
	utf32               c;
	utf32              *buf;
	const utf32        *bufend;
	const utf32        *bufpos;
	input_t            *input;
	pp_input_t         *parent;
	searchpath_entry_t *path;
	FILE               *file;
	position_t          pos;
	unsigned            output_line;
};

struct searchpath_entry_t {
	const char         *path;
	searchpath_entry_t *next;
	bool                is_system_path;
};

typedef struct macro_call_t {
	pp_definition_t *macro;
	pp_definition_t *parameter;
	token_t         *argument_tokens;
	void            *obstack_level;
	unsigned         argument_brace_count;
	unsigned         parameter_idx         : 30;
	bool             previous_is_expanding : 1;
	bool             previous_may_recurse  : 1;
} macro_call_t;

typedef struct include_t include_t;
struct include_t {
	include_t  *next;
	const char *filename;
	bool        is_system_header;
};

static pp_input_t      input;

static pp_input_t     *input_stack;
static unsigned        n_inputs;
static struct obstack  input_obstack;
static const char     *base_inputname;
static unsigned        counter; /**< counter for the __COUNTER__ macros */

static pp_conditional_t *conditional_stack;

token_t                      pp_token;
input_decoder_t             *input_decoder = &input_decode_utf8;
bool                         no_dollar_in_symbol;
static bool                  resolve_escape_sequences;
static bool                  skip_mode;
static bool                  stop_at_newline;
static FILE                 *out;
static struct obstack        pp_obstack;
static struct obstack        config_obstack;
static position_t            expansion_pos;
static pp_expansion_state_t *current_expansion;
static macro_call_t          current_call;
static whitespace_info_t     call_whitespace_info;
static bool                  call_space_before;
static pp_definition_t      *argument_expanding;
static token_kind_t          previous_token;
static pp_expansion_state_t *expansion_stack;
static pp_argument_t        *argument_stack;
static macro_call_t         *macro_call_stack;

static pset_new_t            includeset;
static include_t            *includes;
static include_t            *last_include;

struct searchpath_t {
	searchpath_entry_t  *first;
	searchpath_entry_t **anchor;
	bool                 is_system_path;
};

searchpath_t bracket_searchpath = { NULL, &bracket_searchpath.first, false };
searchpath_t quote_searchpath   = { NULL, &quote_searchpath.first,   false };
searchpath_t system_searchpath  = { NULL, &system_searchpath.first,  true  };
searchpath_t after_searchpath   = { NULL, &after_searchpath.first,   true  };

static bool              next_info_valid;
static whitespace_info_t next_info;
static bool              next_space_before;
static whitespace_info_t info;

static inline void next_char(void);
static void next_input_token(void);

static symbol_t *symbol_colongreater;
static symbol_t *symbol_lesscolon;
static symbol_t *symbol_lesspercent;
static symbol_t *symbol_percentcolon;
static symbol_t *symbol_percentcolonpercentcolon;
static symbol_t *symbol_percentgreater;

static symbol_t *symbol___VA_ARGS__;

static void init_symbols(void)
{
	symbol_colongreater             = symbol_table_insert(":>");
	symbol_lesscolon                = symbol_table_insert("<:");
	symbol_lesspercent              = symbol_table_insert("<%");
	symbol_percentcolon             = symbol_table_insert("%:");
	symbol_percentcolonpercentcolon = symbol_table_insert("%:%:");
	symbol_percentgreater           = symbol_table_insert("%>");

	symbol___VA_ARGS__ = symbol_table_insert("__VA_ARGS__");
}

static void print_line_directive(const position_t *pos, const char *add);

/**
 * Switch input to another file/stream. Assume input_name is identified in the
 * string hash.
 */
static void switch_input(input_t *const decoder, char const *const input_name,
                         searchpath_entry_t *const path,
                         bool const is_system_header)
{
	memset(&input, 0, sizeof(input));
	input.input                = decoder;
	input.buf                  = XMALLOCN(utf32, BUFSIZE + MAX_PUTBACK);
	input.bufend               = input.buf + MAX_PUTBACK;
	input.bufpos               = input.bufend;
	input.output_line          = 0;
	input.pos.input_name       = input_name;
	input.pos.lineno           = 1;
	input.pos.is_system_header = is_system_header;
	input.path                 = path;

	/* indicate that we're at a new input */
	const char *line_flag;
	if (input_stack == NULL) {
		/* base file */
		base_inputname = input_name;
		counter        = 0;
		line_flag      = NULL;
	} else {
		line_flag = "1";
	}
	print_line_directive(&input.pos, line_flag);

	/* place a virtual '\n' so we realize we're at line begin
	 * (remember this special case in input_error) */
	input.pos.lineno = 0;
	input.c          = '\n';

	/* track include (for dependency output) */
	if (input_name != builtin_position.input_name) {
		bool new_dep = pset_new_insert(&includeset, (void*)input_name);
		if (new_dep) {
			include_t *include = OALLOC(&pp_obstack, include_t);
			include->next             = NULL;
			include->filename         = input_name;
			include->is_system_header = is_system_header;
			if (last_include != NULL) {
				last_include->next = include;
			} else {
				includes = include;
			}
			last_include = include;
		}
	}
}

void switch_pp_input(FILE *const stream, char const *const input_name, searchpath_entry_t *const path, bool const is_system_header)
{
	input_t *const input = input_from_stream(stream, input_decoder);
	begin_string_construction();
	obstack_grow(&string_obst, input_name, strlen(input_name));
	const string_t *string = finish_string_construction(STRING_ENCODING_CHAR);
	switch_input(input, string->begin, path, is_system_header);
}

/**
 * Switch pp input file to a new file. Assume filename is already identified.
 */
static void switch_input_file(FILE *file, char const *const filename,
                              searchpath_entry_t *const path,
                              bool const is_system_header)
{
	input_t *decoder = input_from_stream(file, input_decoder);
	switch_input(decoder, filename, path, is_system_header);
	input.file = file;
}

void close_pp_input(void)
{
	free(input.buf);
	input_free(input.input);
}

static void close_pp_input_file(void)
{
	close_pp_input();
	fclose(input.file);
}

void print_pp_header(void)
{
	/* this is just here to make our output look similar to the gcc one */
	position_t first_pos = input.pos;
	first_pos.lineno = 1;
	print_line_directive(&first_pos, NULL);
	fputc('\n', out);
	fprintf(out, "# 1 \"<built-in>\"\n");
	fprintf(out, "# 1 \"<command-line>\"\n");
	print_line_directive(&first_pos, NULL);
}

static void push_input(void)
{
	pp_input_t *const saved_input = obstack_copy(&input_obstack, &input, sizeof(input));

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
	errorf(&pp_token.base.pos, "%s", msg);
}

static inline void next_real_char(void)
{
	assert(input.bufpos <= input.bufend);
	if (input.bufpos >= input.bufend) {
		size_t const n = decode(input.input, input.buf + MAX_PUTBACK, BUFSIZE);
		if (n == 0) {
			input.c = UTF32_EOF;
			return;
		}
		input.bufpos = input.buf + MAX_PUTBACK;
		input.bufend = input.bufpos + n;
	}
	input.c = *input.bufpos++;
	++input.pos.colno;
}

/**
 * Put the current input character back into the buffer and set the current
 * input character to @p c.
 *
 * @param c  the character to set the input character to
 */
static inline void put_back(utf32 const c)
{
	assert(input.bufpos > input.buf);
	*(--input.bufpos - input.buf + input.buf) = input.c;
	--input.pos.colno;
	input.c = c;
}

#define eat(c_type) (assert(input.c == c_type), next_char())

#define NEWLINE \
	     '\n': \
	case '\r'

#define EAT_NEWLINE \
	'\r': \
		eat('\r'); \
		if (input.c == '\n') { \
	case '\n': \
			eat('\n'); \
		} \
		++input.pos.lineno; \
		input.pos.colno = 1; \
		goto newline; \
		newline // Let it look like an ordinary case label.

static void maybe_concat_lines(void)
{
	next_real_char();

	switch (input.c) {
	case EAT_NEWLINE:
		info.whitespace_at_line_begin = 0;
		return;

	default:
		break;
	}

	put_back('\\');
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
		return;
	}

	if (LIKELY(input.c != '?'))
		return;

	next_real_char();
	if (LIKELY(input.c != '?')) {
		put_back('?');
		return;
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
		put_back('?');
		put_back('?');
		break;
	}
}

/**
 * Returns true if the given char is a octal digit.
 *
 * @param char  the character to check
 */
static int octal_digit_value(int chr)
{
	/* note: the c99 spec guarantees '0' to '9' having consecutive codes */
	if (chr < '0' || chr > '7')
		return -1;
	return chr - '0';
}

/**
 * Returns the value of a digit.
 * The only portable way to do it ...
 */
static int hex_digit_value(utf32 digit)
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
		return -1;
	}
}

/**
 * Parses an octal character sequence.
 *
 * @param first_digit  the already read first digit
 */
static utf32 parse_octal_sequence(const utf32 first_digit)
{
	int digit_value = octal_digit_value(first_digit);
	assert(digit_value >= 0);
	utf32 value = digit_value;
	digit_value = octal_digit_value(input.c);
	if (digit_value < 0)
		return value;
	value = 8 * value + digit_value;
	next_char();
	digit_value = octal_digit_value(input.c);
	if (digit_value < 0)
		return value;
	value = 8 * value + digit_value;
	next_char();
	return value;
}

/**
 * Parses a hex character sequence.
 */
static utf32 parse_hex_sequence(void)
{
	utf32 value = 0;
	int digit_value;
	while ((digit_value = hex_digit_value(input.c)) >= 0) {
		value = 16 * value + digit_value;
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
		int digit_value = hex_digit_value(input.c);
		if (digit_value >= 0) {
			v = 16 * v + digit_value;
			if (!resolve_escape_sequences)
				obstack_1grow(&symbol_obstack, input.c);
			next_char();
		} else {
			errorf(&input.pos,
			       "short universal character name, expected %u more digits",
				   k);
			break;
		}
	}
	if (!is_universal_char_valid(v)) {
		errorf(&input.pos,
		       "\\%c%0*X is not a valid universal character name",
		       n_digits == 4 ? 'u' : 'U', (int)n_digits, v);
	}
	return v;
}

static bool is_universal_char_valid_identifier_c99(utf32 const v)
{
	static const utf32 single_chars[] = {
		0x00AA, 0x00BA, 0x0386, 0x038C, 0x03DA, 0x03DC, 0x03DE, 0x03E0,
		0x1F59, 0x1F5B, 0x1F5D, 0x05BF, 0x09B2, 0x0A02, 0x0A5E, 0x0A74,
		0x0A8D, 0x0AD0, 0x0AE0, 0x0B9C, 0x0CDE, 0x0E84, 0x0E8A, 0x0E8D,
		0x0EA5, 0x0EA7, 0x0EC6, 0x0F00, 0x0F35, 0x0F37, 0x0F39, 0x0F97,
		0x0FB9, 0x00B5, 0x00B7, 0x02BB, 0x037A, 0x0559, 0x093D, 0x0B3D,
		0x1FBE, 0x2102, 0x2107, 0x2115, 0x2124, 0x2126, 0x2128
	};

	static const utf32 ranges[][2] = {
		{0x00C0, 0x00D6}, {0x00D8, 0x00F6}, {0x00F8, 0x01F5}, {0x01FA, 0x0217},
		{0x0250, 0x02A8}, {0x1E00, 0x1E9B}, {0x1EA0, 0x1EF9}, {0x0388, 0x038A},
		{0x038E, 0x03A1}, {0x03A3, 0x03CE}, {0x03D0, 0x03D6}, {0x03E2, 0x03F3},
		{0x1F00, 0x1F15}, {0x1F18, 0x1F1D}, {0x1F20, 0x1F45}, {0x1F48, 0x1F4D},
		{0x1F50, 0x1F57}, {0x1F5F, 0x1F7D}, {0x1F80, 0x1FB4}, {0x1FB6, 0x1FBC},
		{0x1FC2, 0x1FC4}, {0x1FC6, 0x1FCC}, {0x1FD0, 0x1FD3}, {0x1FD6, 0x1FDB},
		{0x1FE0, 0x1FEC}, {0x1FF2, 0x1FF4}, {0x1FF6, 0x1FFC}, {0x0401, 0x040C},
		{0x040E, 0x044F}, {0x0451, 0x045C}, {0x045E, 0x0481}, {0x0490, 0x04C4},
		{0x04C7, 0x04C8}, {0x04CB, 0x04CC}, {0x04D0, 0x04EB}, {0x04EE, 0x04F5},
		{0x04F8, 0x04F9}, {0x0531, 0x0556}, {0x0561, 0x0587}, {0x05B0, 0x05B9},
		{0x05BB, 0x05BD}, {0x05C1, 0x05C2}, {0x05D0, 0x05EA}, {0x05F0, 0x05F2},
		{0x0621, 0x063A}, {0x0640, 0x0652}, {0x0670, 0x06B7}, {0x06BA, 0x06BE},
		{0x06C0, 0x06CE}, {0x06D0, 0x06DC}, {0x06E5, 0x06E8}, {0x06EA, 0x06ED},
		{0x0901, 0x0903}, {0x0905, 0x0939}, {0x093E, 0x094D}, {0x0950, 0x0952},
		{0x0958, 0x0963}, {0x0981, 0x0983}, {0x0985, 0x098C}, {0x098F, 0x0990},
		{0x0993, 0x09A8}, {0x09AA, 0x09B0}, {0x09B6, 0x09B9}, {0x09BE, 0x09C4},
		{0x09C7, 0x09C8}, {0x09CB, 0x09CD}, {0x09DC, 0x09DD}, {0x09DF, 0x09E3},
		{0x09F0, 0x09F1}, {0x0A05, 0x0A0A}, {0x0A0F, 0x0A10}, {0x0A13, 0x0A28},
		{0x0A2A, 0x0A30}, {0x0A32, 0x0A33}, {0x0A35, 0x0A36}, {0x0A38, 0x0A39},
		{0x0A3E, 0x0A42}, {0x0A47, 0x0A48}, {0x0A4B, 0x0A4D}, {0x0A59, 0x0A5C},
		{0x0A81, 0x0A83}, {0x0A85, 0x0A8B}, {0x0A8F, 0x0A91}, {0x0A93, 0x0AA8},
		{0x0AAA, 0x0AB0}, {0x0AB2, 0x0AB3}, {0x0AB5, 0x0AB9}, {0x0ABD, 0x0AC5},
		{0x0AC7, 0x0AC9}, {0x0ACB, 0x0ACD}, {0x0B01, 0x0B03}, {0x0B05, 0x0B0C},
		{0x0B0F, 0x0B10}, {0x0B13, 0x0B28}, {0x0B2A, 0x0B30}, {0x0B32, 0x0B33},
		{0x0B36, 0x0B39}, {0x0B3E, 0x0B43}, {0x0B47, 0x0B48}, {0x0B4B, 0x0B4D},
		{0x0B5C, 0x0B5D}, {0x0B5F, 0x0B61}, {0x0B82, 0x0B83}, {0x0B85, 0x0B8A},
		{0x0B8E, 0x0B90}, {0x0B92, 0x0B95}, {0x0B99, 0x0B9A}, {0x0B9E, 0x0B9F},
		{0x0BA3, 0x0BA4}, {0x0BA8, 0x0BAA}, {0x0BAE, 0x0BB5}, {0x0BB7, 0x0BB9},
		{0x0BBE, 0x0BC2}, {0x0BC6, 0x0BC8}, {0x0BCA, 0x0BCD}, {0x0C01, 0x0C03},
		{0x0C05, 0x0C0C}, {0x0C0E, 0x0C10}, {0x0C12, 0x0C28}, {0x0C2A, 0x0C33},
		{0x0C35, 0x0C39}, {0x0C3E, 0x0C44}, {0x0C46, 0x0C48}, {0x0C4A, 0x0C4D},
		{0x0C60, 0x0C61}, {0x0C82, 0x0C83}, {0x0C85, 0x0C8C}, {0x0C8E, 0x0C90},
		{0x0C92, 0x0CA8}, {0x0CAA, 0x0CB3}, {0x0CB5, 0x0CB9}, {0x0CBE, 0x0CC4},
		{0x0CC6, 0x0CC8}, {0x0CCA, 0x0CCD}, {0x0CE0, 0x0CE1}, {0x0D02, 0x0D03},
		{0x0D05, 0x0D0C}, {0x0D0E, 0x0D10}, {0x0D12, 0x0D28}, {0x0D2A, 0x0D39},
		{0x0D3E, 0x0D43}, {0x0D46, 0x0D48}, {0x0D4A, 0x0D4D}, {0x0D60, 0x0D61},
		{0x0E01, 0x0E3A}, {0x0E40, 0x0E5B}, {0x0E81, 0x0E82}, {0x0E87, 0x0E88},
		{0x0E94, 0x0E97}, {0x0E99, 0x0E9F}, {0x0EA1, 0x0EA3}, {0x0EAA, 0x0EAB},
		{0x0EAD, 0x0EAE}, {0x0EB0, 0x0EB9}, {0x0EBB, 0x0EBD}, {0x0EC0, 0x0EC4},
		{0x0EC8, 0x0ECD}, {0x0EDC, 0x0EDD}, {0x0F18, 0x0F19}, {0x0F3E, 0x0F47},
		{0x0F49, 0x0F69}, {0x0F71, 0x0F84}, {0x0F86, 0x0F8B}, {0x0F90, 0x0F95},
		{0x0F99, 0x0FAD}, {0x0FB1, 0x0FB7}, {0x10A0, 0x10C5}, {0x10D0, 0x10F6},
		{0x3041, 0x3093}, {0x309B, 0x309C}, {0x30A1, 0x30F6}, {0x30FB, 0x30FC},
		{0x3105, 0x312C}, {0x4E00, 0x9FA5}, {0xAC00, 0xD7A3}, {0x0660, 0x0669},
		{0x06F0, 0x06F9}, {0x0966, 0x096F}, {0x09E6, 0x09EF}, {0x0A66, 0x0A6F},
		{0x0AE6, 0x0AEF}, {0x0B66, 0x0B6F}, {0x0BE7, 0x0BEF}, {0x0C66, 0x0C6F},
		{0x0CE6, 0x0CEF}, {0x0D66, 0x0D6F}, {0x0E50, 0x0E59}, {0x0ED0, 0x0ED9},
		{0x0F20, 0x0F33}, {0x02B0, 0x02B8}, {0x02BD, 0x02C1}, {0x02D0, 0x02D1},
		{0x02E0, 0x02E4}, {0x203F, 0x2040}, {0x210A, 0x2113}, {0x2118, 0x211D},
		{0x212A, 0x2131}, {0x2133, 0x2138}, {0x2160, 0x2182}, {0x3005, 0x3007},
		{0x3021, 0x3029},
	};
	for (size_t i = 0; i < sizeof(ranges)/sizeof(ranges[0]); ++i) {
		if (ranges[i][0] <= v && v <= ranges[i][1])
			return true;
	}
	for (size_t i = 0; i < sizeof(single_chars)/sizeof(single_chars[0]); ++i) {
		if (v == single_chars[i])
			return true;
	}
	return false;
}

static bool is_universal_char_valid_identifier_c11(utf32 const v)
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

static bool is_universal_char_valid_identifier(utf32 const v)
{
	if (dialect.c11)
		return is_universal_char_valid_identifier_c11(v);
	return is_universal_char_valid_identifier_c99(v);
}

static bool is_universal_char_invalid_identifier_start(utf32 const v)
{
	if (!dialect.c11)
		return false;

	/* C11 Annex D.2 */
	if (0x0300 <= v && v <= 0x036F) return true;
	if (0x1DC0 <= v && v <= 0x1DFF) return true;
	if (0x20D0 <= v && v <= 0x20FF) return true;
	if (0xFE20 <= v && v <= 0xFE2F) return true;
	return false;
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
	case UTF32_EOF:
		warningf(WARN_OTHER, &pp_token.base.pos,
		         "reached end of file while parsing escape sequence");
		return UTF32_EOF;
	/* \E is not documented, but handled, by GCC.  It is acceptable according
	 * to §6.11.4, whereas \e is not. */
	case 'E':
	case 'e':
		if (dialect.gnu)
			return 27;   /* hopefully 27 is ALWAYS the code for ESCAPE */
		break;

	case 'U': return parse_universal_char(8);
	case 'u': return parse_universal_char(4);

	default:
		break;
	}
	/* §6.4.4.4:8 footnote 64 */
	warningf(WARN_OTHER, &pp_token.base.pos, "unknown escape sequence");
	return UTF32_EOF;
}

static utf32 get_string_encoding_limit(string_encoding_t const enc)
{
	switch (enc) {
	case STRING_ENCODING_CHAR:   return 0xFF;
	case STRING_ENCODING_CHAR16: return 0xFFFF;
	case STRING_ENCODING_CHAR32: return 0xFFFFFFFF;
	case STRING_ENCODING_UTF8:   return 0xFFFFFFFF;
	case STRING_ENCODING_WIDE:   return 0xFFFFFFFF; // FIXME depends on settings
	}
	panic("invalid string encoding");
}

static void parse_string(utf32 const delimiter, token_kind_t const kind,
                         string_encoding_t const enc,
                         char const *const context)
{
	eat(delimiter);

	begin_string_construction();
	utf32 const limit = get_string_encoding_limit(enc);
	while (true) {
		switch (input.c) {
		case '\\': {
			if (resolve_escape_sequences) {
				utf32 const tc = parse_escape_sequence();
				if (tc > limit && tc != UTF32_EOF) {
					warningf(WARN_OTHER, &input.pos,
					         "escape sequence out of range");
				}
				if (enc == STRING_ENCODING_CHAR) {
					obstack_1grow(&string_obst, tc);
				} else {
					obstack_grow_utf8(&string_obst, tc);
				}
			} else {
				obstack_1grow(&string_obst, (char)input.c);
				next_char();
				obstack_1grow(&string_obst, (char)input.c);
				next_char();
			}
			break;
		}

		case NEWLINE:
			warningf(WARN_OTHER, &pp_token.base.pos,
			         "newline while parsing %s", context);
			goto end_of_string;

		case UTF32_EOF:
			warningf(WARN_OTHER, &pp_token.base.pos,
			         "EOF while parsing %s", context);
			goto end_of_string;

		default:
			if (input.c == delimiter) {
				eat(delimiter);
				goto end_of_string;
			} else {
				obstack_grow_utf8(&string_obst, input.c);
				next_char();
				break;
			}
		}
	}

end_of_string:
	pp_token.kind           = kind;
	pp_token.literal.string = finish_string_construction(enc);
}

static void parse_string_literal(string_encoding_t const enc)
{
	parse_string('"', T_STRING_LITERAL, enc, "string literal");
}

static void parse_character_constant(string_encoding_t const enc)
{
	parse_string('\'', T_CHARACTER_CONSTANT, enc, "character constant");
	if (pp_token.literal.string->size == 0) {
		parse_error("empty character constant");
	}
}

#define SYMBOL_CASES_WITHOUT_E_P \
	     '$': if (no_dollar_in_symbol) goto dollar_sign; \
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

#define WHITESPACE \
	     ' ': \
	case '\f': \
	case '\t': \
	case '\v'

static pp_expansion_state_t *push_expansion(pp_definition_t *definition)
{
	ARR_EXTEND(pp_expansion_state_t, expansion_stack, 1);
	size_t          const len    = ARR_LEN(expansion_stack);
	pp_expansion_state_t *result = &expansion_stack[len-1];
	memset(result, 0, sizeof(*result));
	result->definition = definition;
	result->list_len   = definition->list_len;
	result->token_list = definition->token_list;
	result->pos        = 0;
	current_expansion  = result;
	return result;
}

static void push_function_expansion(pp_definition_t *function)
{
	function->is_expanding = false;
	function->may_recurse  = false;

	size_t n_parameters = function->n_parameters;
	for (size_t i = 0; i < n_parameters; ++i) {
		pp_definition_t *parameter = &function->parameters[i];
		pp_argument_t    arg = {
			parameter->list_len,
			parameter->token_list,
			parameter->is_expanding
		};
		ARR_APP1(pp_argument_t, argument_stack, arg);
	}
}

static void pop_function_expansion(pp_definition_t *function)
{
	size_t argument_stack_top = ARR_LEN(argument_stack);
	size_t n_parameters       = function->n_parameters;
	assert(argument_stack_top >= n_parameters);
	for (size_t i = n_parameters; i-- > 0; ) {
		pp_definition_t     *parameter = &function->parameters[i];
		const pp_argument_t *arg       = &argument_stack[--argument_stack_top];
		parameter->list_len     = arg->list_len;
		parameter->token_list   = arg->token_list;
		parameter->is_expanding = arg->is_expanding;
	}
	ARR_SHRINKLEN(argument_stack, argument_stack_top);
}

static bool pop_expansion(void)
{
	assert(current_expansion != NULL);
	pp_expansion_state_t *expansion  = current_expansion;
	pp_definition_t      *definition = expansion->definition;
	if (definition->n_parameters > 0) {
		obstack_free(&pp_obstack, expansion->obstack_level);
		pop_function_expansion(definition);
	}
	definition->is_expanding = expansion->previous_is_expanding;
	definition->may_recurse  = expansion->previous_may_recurse;

	if (definition->is_parameter) {
		pp_definition_t *function = definition->function_definition;
		function->may_recurse = false;

		/* stop further expanding once we expanded a parameter used in a
		 * sub macro-call */
		if (definition == argument_expanding) {
			argument_expanding = NULL;
		}
	}

	size_t top = ARR_LEN(expansion_stack);
	assert(current_expansion == &expansion_stack[top-1]);
	ARR_SHRINKLEN(expansion_stack, top-1);
	if (top == 1) {
		current_expansion = NULL;
		return false;
	}

	current_expansion = &expansion_stack[top-2];
	return true;
}

static void push_macro_call(void)
{
	if (current_call.macro == NULL)
		return;
	ARR_APP1(macro_call_t, macro_call_stack, current_call);
	memset(&current_call, 0, sizeof(current_call));
}

static void pop_macro_call(void)
{
	size_t top = ARR_LEN(macro_call_stack);
	if (top == 0) {
		memset(&current_call, 0, sizeof(current_call));
	} else {
		current_call = macro_call_stack[top-1];
		ARR_SHRINKLEN(macro_call_stack, top-1);
	}
}

static void start_argument(pp_definition_t *parameter)
{
	current_call.parameter = parameter;
	current_call.argument_tokens = NEW_ARR_F(token_t, 0);
}

static void finish_argument(void)
{
	pp_definition_t *parameter = current_call.parameter;
	if (parameter == NULL)
		return;
	token_t *tokens = current_call.argument_tokens;
	size_t   len    = ARR_LEN(tokens);
	parameter->list_len   = len;
	parameter->token_list = obstack_copy(&pp_obstack, tokens, len*sizeof(tokens[0]));
	parameter->is_expanding = false;
	DEL_ARR_F(tokens);
}

static void start_call(pp_definition_t *definition, whitespace_info_t wsinfo,
                       bool space_before)
{
	assert(current_call.macro == NULL && current_call.parameter == NULL);
	current_call.macro                 = definition;
	current_call.previous_is_expanding = definition->is_expanding;
	current_call.previous_may_recurse  = definition->may_recurse;
	call_whitespace_info = wsinfo;
	call_space_before    = space_before;
	if (definition->n_parameters > 0) {
		start_argument(&definition->parameters[0]);
		push_function_expansion(definition);
	}
	/* fields of current_call should be clear from push_macro_call() */
	assert(current_call.argument_brace_count == 0);
	assert(current_call.parameter_idx == 0);
	assert(obstack_object_size(&pp_obstack) == 0);
	current_call.obstack_level = obstack_alloc(&pp_obstack, 0);
}

static pp_expansion_state_t *start_expanding(pp_definition_t *definition)
{
	pp_expansion_state_t *expansion = push_expansion(definition);
	if (definition->list_len > 0) {
		token_t *token = &definition->token_list[0];
		token->base.space_before = pp_token.base.space_before;
	}
	return expansion;
}

static void start_function_macro_expansion(const macro_call_t *call)
{
	pp_definition_t *macro = call->macro;
	finish_argument();
	/* check if enough arguments have been specified */
	unsigned parameter_idx = call->parameter_idx+1;
	/* variadic parameter may be left out */
	if (parameter_idx < macro->n_parameters) {
		pp_definition_t *parameter = &macro->parameters[parameter_idx];
		if (parameter->is_variadic) {
			parameter->not_specified = true;
			/* avoid further error */
			parameter_idx = macro->n_parameters;
		}
	}
	if (parameter_idx < macro->n_parameters) {
		errorf(&pp_token.base.pos,
		       "macro '%Y' requires %u arguments, but only %u provided",
		       macro->symbol, macro->n_parameters, call->parameter_idx+1);
		/* set remaining arguments to empty replacements */
		for ( ; parameter_idx < macro->n_parameters; ++parameter_idx) {
			pp_definition_t *parameter = &macro->parameters[parameter_idx];
			parameter->list_len      = 0;
			parameter->not_specified = true;
		}
	}

	pp_expansion_state_t *expansion  = start_expanding(call->macro);
	expansion->previous_is_expanding = call->previous_is_expanding;
	expansion->previous_may_recurse  = call->previous_may_recurse;
	macro->is_expanding              = true;
	if (macro->n_parameters > 0) {
		expansion->obstack_level = call->obstack_level;
	}
	current_expansion = expansion;
}

static void start_object_macro_expansion(pp_definition_t *definition)
{
	pp_expansion_state_t *expansion  = start_expanding(definition);
	expansion->previous_is_expanding = definition->is_expanding;
	expansion->previous_may_recurse  = definition->may_recurse;
	definition->is_expanding         = true;
	if (definition->is_parameter) {
		definition->function_definition->may_recurse = true;
	}
	current_expansion = expansion;
}

static void grow_escaped(struct obstack *obst, const char *string, size_t size)
{
	if (resolve_escape_sequences) {
		obstack_grow(obst, string, size);
	} else {
		for (size_t i = 0; i < size; ++i) {
			const char c = string[i];
			if (c == '\\' || c == '"')
				obstack_1grow(obst, '\\');
			obstack_1grow(obst, c);
		}
	}
}

static void grow_string_escaped(struct obstack *obst, const string_t *string,
                                char const *delimiter)
{
	char const *prefix = get_string_encoding_prefix(string->encoding);
	obstack_printf(obst, "%s%s", prefix, delimiter);
	size_t      size = string->size;
	const char *str  = string->begin;

	grow_escaped(obst, str, size);

	obstack_printf(obst, "%s", delimiter);
}

static void grow_token(struct obstack *obst, const token_t *token)
{
	switch (token->kind) {
	case T_NUMBER:
		obstack_grow(obst, token->literal.string->begin,
		             token->literal.string->size);
		break;

	case T_STRING_LITERAL: {
		char const *const delimiter = resolve_escape_sequences ? "\"" : "\\\"";
		grow_string_escaped(obst, token->literal.string, delimiter);
		break;
	}

	case T_CHARACTER_CONSTANT:
		grow_string_escaped(obst, token->literal.string, "'");
		break;

	case T_IDENTIFIER:
	default: {
		const char *str = token->base.symbol->string;
		size_t      len = strlen(str);
		obstack_grow(obst, str, len);
		break;
	}
	}
}

static token_t stringify(const pp_definition_t *definition, bool space_before)
{
	begin_string_construction();
	size_t list_len = definition->list_len;
	for (size_t p = 0; p < list_len; ++p) {
		const token_t *saved = &definition->token_list[p];
		if (p > 0 && saved->base.space_before)
			obstack_1grow(&string_obst, ' ');
		grow_token(&string_obst, saved);
	}
	return (token_t) {
		.literal = {
			.base = {
				.kind         = T_STRING_LITERAL,
				.space_before = space_before,
				.pos          = definition->pos,
				.symbol       = NULL
			},
			.string = finish_string_construction(STRING_ENCODING_CHAR)
		}
	};
}

static string_encoding_t identify_encoding_prefix(symbol_t *const sym)
{
	string_encoding_t enc = STRING_ENCODING_CHAR;
	switch (sym->pp_ID) {
	case TP_L:  return STRING_ENCODING_WIDE;
	case TP_U:  enc = STRING_ENCODING_CHAR32; break;
	case TP_u:  enc = STRING_ENCODING_CHAR16; break;
	case TP_u8: enc = STRING_ENCODING_UTF8;   break;
	default: break;
	}
	return dialect.c11 ? enc : STRING_ENCODING_CHAR;
}

static void obstack_grow_string(struct obstack *obst, const string_t *string)
{
	obstack_grow(obst, string->begin, string->size);
}

static bool is_identifierlike_token(token_t const *const t)
{
	symbol_t *const symbol = t->base.symbol;
	if (!symbol)
		return false;

	if (t->kind == T_IDENTIFIER)
		return true;

	switch (symbol->string[0]) {
	case SYMBOL_CASES:
dollar_sign:
		return t->kind != T_MACRO_PARAMETER;

	default:
		return false;
	}
}

static bool concat_identifier(const token_t *token0, const token_t *token1)
{
	char const *str1;
	size_t      len1;
	switch (token1->kind) {
	case T_STRING_LITERAL:
	case T_CHARACTER_CONSTANT: {
		string_encoding_t const enc = identify_encoding_prefix(token0->base.symbol);
		if (enc != STRING_ENCODING_CHAR
		    && token1->literal.string->encoding == STRING_ENCODING_CHAR) {
			pp_token = *token1;
			pp_token.literal.base.symbol     = NULL;
			begin_string_construction();
			obstack_grow_string(&string_obst, pp_token.literal.string);
			pp_token.literal.string = finish_string_construction(enc);
			return true;
		}
		return false;
	}

	case T_NUMBER:
		str1 = token1->literal.string->begin;
		len1 = token1->literal.string->size;
		for (size_t i = 0; i != len1; ++i) {
			switch (str1[i]) {
			case DIGIT_CASES:
			case SYMBOL_CASES:
dollar_sign:
				continue;
			}
			return false;
		}
		break;

	default:
		if (is_identifierlike_token(token1)) {
	case T_IDENTIFIER:
			str1 = token1->base.symbol->string;
			len1 = strlen(str1);
		} else {
			return false;
		}
	}

	assert(obstack_object_size(&symbol_obstack) == 0);
	char const *const str0 = token0->base.symbol->string;
	obstack_grow(&symbol_obstack, str0, strlen(str0));
	obstack_grow(&symbol_obstack, str1, len1);
	obstack_1grow(&symbol_obstack, '\0');
	char     *const string = obstack_finish(&symbol_obstack);
	symbol_t *const symbol = symbol_table_insert(string);
	if (symbol->string != string)
		obstack_free(&symbol_obstack, string);

	pp_token.kind        = symbol->ID;
	pp_token.base.symbol = symbol;
	return true;
}

static void make_number(void)
{
	pp_token.literal.base.kind   = T_NUMBER;
	pp_token.literal.base.symbol = NULL;
	pp_token.literal.string = finish_string_construction(STRING_ENCODING_CHAR);
}

static bool concat_number(const token_t *token0, const token_t *token1)
{
	begin_string_construction();
	const string_t *str0 = token0->literal.string;
	obstack_grow_string(&string_obst, str0);

	const token_kind_t kind1 = token1->kind;
	if (kind1 == T_NUMBER) {
		const string_t *str1 = token1->literal.string;
		obstack_grow_string(&string_obst, str1);
	} else if (kind1 == T_IDENTIFIER) {
		const char *str1 = token1->base.symbol->string;
		size_t      len1 = strlen(str1);
		obstack_grow(&string_obst, str1, len1);
	} else if (kind1 == '.') {
		obstack_1grow(&string_obst, '.');
	} else if (kind1 == T_DOTDOTDOT) {
		obstack_grow(&string_obst, "...", 3);
	} else {
		assert(str0->size > 0);
		char lastn = str0->begin[str0->size-1];
		if ((lastn == 'e' || lastn == 'E' || lastn == 'p' || lastn == 'P')
			&& (kind1 == '+' || kind1 == '-')) {
			obstack_1grow(&string_obst, (char)kind1);
		} else {
			abort_string_construction();
			return false;
		}
	}

	make_number();
	return true;
}

static bool concat_tokens(const position_t *pos,
                          const token_t *token0, const token_t *token1);

static bool concat_macro_parameters(const position_t *pos,
                                    const token_t *token0,
                                    const token_t *token1)
{
	/* gcc extension: , ## __VA_ARGS__ gives an empty result
	 * if __VA_ARGS__ is empty, otherwise the tokens get concatenated */
	bool gcc_ext = token0->kind == ',' && token1->kind == T_MACRO_PARAMETER
	            && token1->macro_parameter.def->is_variadic;
	pp_definition_t *def1 = NULL;
	if (gcc_ext) {
		def1 = token1->macro_parameter.def;
		if (def1->list_len == 0 && def1->not_specified) {
			pp_token = *token1;
			return true;
		}
	}

	assert(current_call.macro == NULL);
	assert(obstack_object_size(&pp_obstack) == 0);
	pp_definition_t *newdef = obstack_alloc(&pp_obstack, sizeof(*newdef));
	memset(newdef, 0, sizeof(*newdef));
	newdef->symbol       = sym_anonymous;
	newdef->is_parameter = true;
	if (gcc_ext) {
		obstack_grow(&pp_obstack, token0, sizeof(*token0));
		size_t len1 = def1->list_len;
		obstack_grow(&pp_obstack, def1->token_list,
					 len1*sizeof(def1->token_list[0]));
		newdef->function_definition = def1->function_definition;
		goto finish_newdef;
	}

	if (token0->kind == T_MACRO_PARAMETER) {
		pp_definition_t *def0 = token0->macro_parameter.def;
		size_t           len0 = def0->list_len;
		assert(len0 > 0);
		obstack_grow(&pp_obstack, def0->token_list,
		             (len0 - 1) * sizeof(def0->token_list[0]));
		token0     = &def0->token_list[len0-1];
		newdef->function_definition = def0->function_definition;
	}
	if (token1->kind == T_MACRO_PARAMETER) {
		def1 = token1->macro_parameter.def;
		assert(def1->list_len > 0);
		token1 = &def1->token_list[0];
		assert(newdef->function_definition == NULL
		       || newdef->function_definition == def1->function_definition);
		newdef->function_definition = def1->function_definition;
	}
	bool space_before = token0->base.space_before;
	if (!concat_tokens(pos, token0, token1)) {
		char *dummy = obstack_finish(&pp_obstack);
		obstack_free(&pp_obstack, dummy);
		return false;
	}
	pp_token.base.space_before = space_before;
	obstack_grow(&pp_obstack, &pp_token, sizeof(pp_token));
	if (def1 != NULL) {
		size_t len1 = def1->list_len;
		obstack_grow(&pp_obstack, &def1->token_list[1],
		             (len1-1)*sizeof(def1->token_list[0]));
	}

finish_newdef:;
	size_t size = obstack_object_size(&pp_obstack);
	assert(size % sizeof(newdef->token_list[0]) == 0);
	size_t len = size / sizeof(newdef->token_list[0]);
	token_t *list = (token_t*)obstack_finish(&pp_obstack);
	newdef->list_len   = len;
	newdef->token_list = list;

	pp_token.kind                = T_MACRO_PARAMETER;
	pp_token.base.space_before   = false;
	pp_token.macro_parameter.def = newdef;
	return true;
}

/** Set current token to a special token (newline, unknown char), esp.
 * avoid setting a token symbol */
static inline void set_special(token_kind_t const kind)
{
	pp_token.kind = kind;
}

/** Set current token to a punctuator (+, <<=, ...) token */
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

static bool is_digit(const char c)
{
	switch (c) {
	case DIGIT_CASES:
		return true;
	}
	return false;
}

static bool concat_tokens(const position_t *pos,
                          const token_t *token0, const token_t *token1)
{
	if (token0->kind == T_MACRO_PARAMETER) {
		pp_definition_t *def0     = token0->macro_parameter.def;
		size_t           list_len = def0->list_len;
		if (list_len == 0) {
			pp_token = *token1;
			return true;
		}
		if (list_len == 1) {
			token0 = &def0->token_list[0];
		}
	}
	if (token1->kind == T_MACRO_PARAMETER && (token0->kind != ','
	    || !token1->macro_parameter.def->is_variadic)) {
		pp_definition_t *def1     = token1->macro_parameter.def;
		size_t           list_len = def1->list_len;
		if (list_len == 0) {
			pp_token = *token0;
			return true;
		}
		if (list_len == 1) {
			token1 = &def1->token_list[0];
		}
	}

	token_kind_t kind1 = token1->kind;
	switch (token0->kind) {
	case '!':
		if (kind1 == '=') {
			set_punctuator(T_EXCLAMATIONMARKEQUAL);
			return true;
		}
		break;
	case '#':
		if (kind1 == '#' && token0->base.symbol == token1->base.symbol) {
			if (token0->base.symbol == symbol_percentcolon) {
				set_digraph(T_HASHHASH, symbol_percentcolonpercentcolon);
			} else {
				assert(token0->base.symbol == token_symbols['#']);
				set_punctuator(T_HASHHASH);
			}
			return true;
		}
		break;
	case '%':
		if (kind1 == ':') {
			set_digraph('#', symbol_percentcolon);
			return true;
		}
		if (kind1 == '=') { set_punctuator(T_PERCENTEQUAL);      return true; }
		if (kind1 == '>') {
			set_digraph('}', symbol_percentgreater);
			return true;
		}
		break;
	case '&':
		if (kind1 == '&') { set_punctuator(T_ANDAND);            return true; }
		if (kind1 == '=') { set_punctuator(T_ANDEQUAL);          return true; }
		break;
	case '*':
		if (kind1 == '=') { set_punctuator(T_ASTERISKEQUAL);     return true; }
		break;
	case '+':
		if (kind1 == '+') { set_punctuator(T_PLUSPLUS);          return true; }
		if (kind1 == '=') { set_punctuator(T_PLUSEQUAL);         return true; }
		break;
	case '-':
		if (kind1 == '-') { set_punctuator(T_MINUSMINUS);        return true; }
		if (kind1 == '=') { set_punctuator(T_MINUSEQUAL);        return true; }
		if (kind1 == '>') { set_punctuator(T_MINUSGREATER);      return true; }
		break;
	case '/':
		if (kind1 == '=') { set_punctuator(T_SLASHEQUAL);        return true; }
		break;
	case ':':
		if (kind1 == '>') {
			set_digraph(']', symbol_colongreater);
			return true;
		}
		if (kind1 == ':' && dialect.cpp) {
			set_punctuator(T_COLONCOLON);
			return true;
		}
		break;
	case '<':
		switch (kind1) {
		case '%':         set_digraph('{', symbol_lesspercent);  return true;
		case ':':         set_digraph('[', symbol_lesscolon);    return true;
		case '<':         set_punctuator(T_LESSLESS);            return true;
		case T_LESSEQUAL: set_punctuator(T_LESSLESSEQUAL);       return true;
		case '=':         set_punctuator(T_LESSEQUAL);           return true;
		default: break;
		}
		break;
	case T_LESSLESS:
		if (kind1 == '=') { set_punctuator(T_LESSLESSEQUAL);     return true; }
		break;
	case '=':
		if (kind1 == '=') { set_punctuator(T_EQUALEQUAL);        return true; }
	case '>':
		switch (kind1) {
		case '=':         { set_punctuator(T_GREATEREQUAL);      return true; }
		case '>':         { set_punctuator(T_GREATERGREATER);    return true; }
		case T_GREATEREQUAL: {
			set_punctuator(T_GREATERGREATEREQUAL);
			return true;
		default:
			break;
		}
		}
		break;
	case T_GREATERGREATER:
		if (kind1 == '=') {
			set_punctuator(T_GREATERGREATEREQUAL);
			return true;
		}
		break;
	case '^':
		if (kind1 == '=') { set_punctuator(T_CARETEQUAL);        return true; }
		break;
	case '|':
		if (kind1 == '=') { set_punctuator(T_PIPEEQUAL);         return true; }
		if (kind1 == '|') { set_punctuator(T_PIPEPIPE);          return true; }
		break;
	case '.':
		if (kind1 == T_NUMBER && is_digit(token1->literal.string->begin[0])) {
			begin_string_construction();
			obstack_1grow(&string_obst, '.');
			obstack_grow_string(&string_obst, token1->literal.string);
			make_number();
			return true;
		}
		break;
	case T_MACRO_PARAMETER:
		return concat_macro_parameters(pos, token0, token1);
	case T_NUMBER:
		if (concat_number(token0, token1))
			return true;
		break;

	default:
		if (is_identifierlike_token(token0)) {
	case T_IDENTIFIER:
			if (concat_identifier(token0, token1))
				return true;
		}
		break;
	}
	if (kind1 == T_MACRO_PARAMETER) {
		return concat_macro_parameters(pos, token0, token1);
	}
	errorf(pos,
	       "pasting %K and %K does not result in a valid preprocessing token",
	       token0, token1);
	return false;
}

/**
 * returns next final token from a preprocessor macro expansion
 */
static bool expand_next(void)
{
	if (current_expansion == NULL)
		return false;

	size_t pos = current_expansion->pos;
	while (pos >= current_expansion->list_len) {
		if (!pop_expansion())
			return false;
		pos = current_expansion->pos;
	}

	bool old_space_before = pp_token.base.space_before;
	pp_token = current_expansion->token_list[pos++];
	if (current_expansion->pos == 0)
		pp_token.base.space_before = old_space_before;

more_concat:
	if (pos < current_expansion->list_len) {
		const token_t *next      = &current_expansion->token_list[pos];
		token_kind_t   next_kind = next->kind;
		if (next_kind == T_HASHHASH && pos+1 < current_expansion->list_len
		    && !current_expansion->definition->is_parameter) {
			const token_t *next_but_one = &current_expansion->token_list[pos+1];
			size_t         advance      = 2;
			token_t        tmp;
			if (next_but_one->kind == '#'
			    && pos+2 < current_expansion->list_len) {
				const token_t *next_next_but_one
					= &current_expansion->token_list[pos+2];
				if (next_next_but_one->kind == T_MACRO_PARAMETER) {
					pp_definition_t *def
						= next_next_but_one->macro_parameter.def;
					assert(def != NULL && def->is_parameter);
					tmp          = stringify(def, next_next_but_one->base.space_before);
					next_but_one = &tmp;
					advance      = 3;
				}
			}
			bool space_before = pp_token.base.space_before;
			if (concat_tokens(&next->base.pos, &pp_token, next_but_one)) {
				pp_token.base.space_before = space_before;
				pos += advance;
				goto more_concat;
			}
		} else if (pp_token.kind == '#' && next_kind == T_MACRO_PARAMETER) {
			pp_definition_t *def = next->macro_parameter.def;
			assert(def != NULL && def->is_parameter);
			pp_token = stringify(def, pp_token.base.space_before);
			++pos;
		}
	}

	current_expansion->pos = pos;
	pp_token.base.pos      = expansion_pos;

	return true;
}

/**
 * Returns the next token kind found when continuing the current expansions
 * without starting new sub-expansions.
 */
static token_kind_t peek_expansion(bool may_pop)
{
	for (size_t i = ARR_LEN(expansion_stack); i-- > 0; ) {
		pp_expansion_state_t *e = &expansion_stack[i];
		if (e->pos < e->list_len)
			return e->token_list[e->pos].kind;
		if (!may_pop)
			return T_EOF;
	}
	return T_EOF;
}

static void skip_line_comment(void)
{
	while (true) {
		switch (input.c) {
		case UTF32_EOF:
		case NEWLINE:
			return;

		default:
			next_char();
			break;
		}
	}
}

static unsigned skip_multiline_comment(void)
{
	position_t const start_pos = input.pos;
	while (true) {
		switch (input.c) {
		case '/': {
			position_t pos = input.pos;
			eat('/');
			if (input.c == '*') {
				warningf(WARN_COMMENT, &pos, "'/*' within block comment");
			}
			break;
		}

		case '*':
			eat('*');
			if (input.c == '/') {
				unsigned const whitespace_at_line_begin =
					input.pos.lineno != input.output_line ? input.pos.colno : 0;
				eat('/');
				return whitespace_at_line_begin;
			}
			break;

		case EAT_NEWLINE:
			break;

		case UTF32_EOF:
			errorf(&start_pos, "unterminated comment");
			return 0;

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
		case WHITESPACE:
			next_char();
			continue;

		case '/':
			eat('/');
			if (input.c == '/') {
				eat('/');
				skip_line_comment();
				pp_token.base.space_before = true;
				continue;
			} else if (input.c == '*') {
				eat('*');
				pp_token.base.space_before    = true;
				info.whitespace_at_line_begin = skip_multiline_comment();
				continue;
			} else {
				put_back('/');
			}
			/* FALLTHROUGH */
		default:
			if (stop_at_non_whitespace)
				return false;
			res = true;
			next_char();
			continue;

		case UTF32_EOF:
		case NEWLINE:
			return res;
		}
	}
}

static whitespace_info_t skip_whitespace(void)
{
	whitespace_info_t wsinfo;
	memset(&wsinfo, 0, sizeof(wsinfo));

	while (true) {
		switch (input.c) {
		case WHITESPACE:
			++wsinfo.whitespace_at_line_begin;
			next_char();
			continue;

		case EAT_NEWLINE:
			wsinfo.at_line_begin            = true;
			wsinfo.whitespace_at_line_begin = 0;
			if (stop_at_newline) {
				--input.pos.lineno;
				put_back('\n');
				return wsinfo;
			}
			continue;

		case '/':
			eat('/');
			if (input.c == '/') {
				eat('/');
				skip_line_comment();
				continue;
			} else if (input.c == '*') {
				eat('*');
				wsinfo.whitespace_at_line_begin = skip_multiline_comment();
				continue;
			} else {
				put_back('/');
			}
			return wsinfo;

		default:
			return wsinfo;
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
			eat('\\');
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
						errorf(&input.pos,
							   "universal character \\%c%0*X is not valid in an identifier",
							   n == 4 ? 'u' : 'U', (int)n, v);
					}
				} else if (obstack_object_size(&symbol_obstack) == 0 && is_universal_char_invalid_identifier_start(v)) {
					errorf(&input.pos,
						   "universal character \\%c%0*X is not valid as start of an identifier",
						   n == 4 ? 'u' : 'U', (int)n, v);
				} else if (resolve_escape_sequences) {
					obstack_grow_utf8(&symbol_obstack, v);
				}
				break;
			}

			default:
				put_back('\\');
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

	symbol_t *symbol = symbol_table_insert(string);

	/* Might be a prefixed string or character constant: L/U/u/u8"string". */
	if (input.c == '"') {
		string_encoding_t const enc = identify_encoding_prefix(symbol);
		if (enc != STRING_ENCODING_CHAR) {
			parse_string_literal(enc);
			return;
		}
	} else if (input.c == '\'') {
		string_encoding_t const enc = identify_encoding_prefix(symbol);
		if (enc != STRING_ENCODING_CHAR) {
			if (enc == STRING_ENCODING_UTF8) {
				errorf(&pp_token.base.pos,
				       "'u8' is not a valid encoding for a chracter constant");
			}
			parse_character_constant(enc);
			return;
		}
	}

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
	begin_string_construction();
	obstack_1grow(&string_obst, (char) input.c);
	next_char();

	while (true) {
		switch (input.c) {
		case '.':
		case DIGIT_CASES:
		case SYMBOL_CASES_WITHOUT_E_P:
			obstack_1grow(&string_obst, (char) input.c);
			next_char();
			break;

		case 'e':
		case 'p':
		case 'E':
		case 'P':
			obstack_1grow(&string_obst, (char) input.c);
			next_char();
			if (input.c == '+' || input.c == '-') {
				obstack_1grow(&string_obst, (char) input.c);
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
	pp_token.literal.string = finish_string_construction(STRING_ENCODING_CHAR);
}

#define MAYBE_PROLOG \
	next_char(); \
	switch (input.c) {

#define MAYBE(ch, kind) \
	case ch: \
		eat(ch); \
		set_punctuator(kind); \
		return;

#define MAYBE_DIGRAPH(ch, kind, symbol) \
	case ch: \
		eat(ch); \
		set_digraph(kind, symbol); \
		return;

#define ELSE_CODE(code) \
	default: \
		code \
	}

#define ELSE(kind) ELSE_CODE(set_punctuator(kind); return;)

static void maybe_skip_newline(void)
{
	switch (input.c) {
	case EAT_NEWLINE:
		pp_token.base.space_before    = false;
		info.at_line_begin            = true;
		info.whitespace_at_line_begin = 0;
		break;
	}
}

/** identifies and returns the next preprocessing token contained in the
 * input stream. No macro expansion is performed. */
static void next_input_token(void)
{
	if (next_info_valid) {
		info                       = next_info;
		pp_token.base.space_before = next_space_before;
		next_info_valid = false;
	} else {
		info.at_line_begin         = false;
		pp_token.base.space_before = false;
	}
	pp_token.base.expansion_forbidden = false;

restart:
	pp_token.base.pos            = input.pos;
	pp_token.base.symbol         = NULL;

	switch (input.c) {
	case WHITESPACE:
		info.whitespace_at_line_begin++;
		pp_token.base.space_before = true;
		next_char();
		goto restart;

	case EAT_NEWLINE:
		pp_token.base.space_before    = false; /* see space_before decl. */
		info.at_line_begin            = true;
		info.whitespace_at_line_begin = 0;
		if (stop_at_newline) {
			--input.pos.lineno;
			set_special(T_NEWLINE);
			put_back('\n');
			return;
		}
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
			case DIGIT_CASES:
				put_back('.');
				parse_number();
				return;

			case '.':
				MAYBE_PROLOG
				MAYBE('.', T_DOTDOTDOT)
				ELSE_CODE(
					put_back('.');
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
			eat('*');
			info.whitespace_at_line_begin = skip_multiline_comment();
			pp_token.base.space_before    = true;
			goto restart;
		case '/':
			eat('/');
			skip_line_comment();
			pp_token.base.space_before = true;
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
					put_back('%');
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
			if (dialect.cpp) {
				eat(':');
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

	case UTF32_EOF:
		if (stop_at_newline) {
			set_special(T_NEWLINE);
			return;
		}

		if (input_stack != NULL) {
			close_pp_input_file();
			pop_restore_input();
			if (out)
				fputc('\n', out);
			maybe_skip_newline();
			print_line_directive(&input.pos, "2");
			goto restart;
		} else {
			info.at_line_begin = true;
			set_special(T_EOF);
		}
		return;

	case '\\':
		eat('\\');
		int next_c = input.c;
		put_back('\\');
		if (next_c == 'U' || next_c == 'u') {
			parse_symbol();
			return;
		}
		/* FALLTHROUGH */
	default:
dollar_sign:
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
	}
}

static void print_quoted_string(const char *const string)
{
	fputc('"', out);
	for (const char *c = string; *c != 0; ++c) {
		switch (*c) {
		case '"':  fputs("\\\"", out); break;
		case '\\': fputs("\\\\", out); break;
		case '\a': fputs("\\a", out);  break;
		case '\b': fputs("\\b", out);  break;
		case '\f': fputs("\\f", out);  break;
		case '\n': fputs("\\n", out);  break;
		case '\r': fputs("\\r", out);  break;
		case '\t': fputs("\\t", out);  break;
		case '\v': fputs("\\v", out);  break;
		case '\?': fputs("\\?", out);  break;
		default:
			if (!isprint((unsigned char)*c)) {
				fprintf(out, "\\%03o", (unsigned)*c);
				break;
			}
			fputc(*c, out);
			break;
		}
	}
	fputc('"', out);
}

static void print_line_directive(const position_t *pos, const char *add)
{
	if (!out)
		return;

	fprintf(out, "# %u ", pos->lineno);
	print_quoted_string(pos->input_name);
	if (add != NULL) {
		fputc(' ', out);
		fputs(add, out);
	}
	if (pos->is_system_header) {
		fputs(" 3", out);
	}

	input.output_line = pos->lineno - 1;
}

static bool emit_newlines(void)
{
	if (!out)
		return true;

	if (!info.at_line_begin)
		return false;
	unsigned delta = pp_token.base.pos.lineno - input.output_line;
	if (delta == 0)
		return false;

	if (delta >= 9) {
		fputc('\n', out);
		print_line_directive(&pp_token.base.pos, NULL);
		fputc('\n', out);
	} else {
		for (unsigned i = delta; i-- > 0; ) {
			fputc('\n', out);
		}
	}
	input.output_line = pp_token.base.pos.lineno;

	return true;
}

void set_preprocessor_output(FILE *output)
{
	out = output;
	resolve_escape_sequences = out == NULL;
}

/**
 * Print a filename and quote characters which are special to make.
 * This algorithm does not handle all corner cases by should replicate the
 * behaviour of gcc (4.8).
 */
static void print_makefile_escaped(const char *string, FILE *out)
{
	for (const char *c = string; *c != '\0'; ++c) {
		switch (*c) {
		case ' ':
		case '\t':
			for (const char *q = c-1; string < q && *q == '\\'; --q)
				fputc('\\', out);
			fputc('\\', out);
			goto normal_out;
		case '#':
			fputc('/', out);
			goto normal_out;
		case '$':
			fputc('$', out);
			goto normal_out;
		default:
		normal_out:
			fputc(*c, out);
			break;
		}
	}
}

void preprocessor_print_dependencies(FILE *output, bool show_system_headers,
                                     const char *target,
                                     bool dont_escape_target,
                                     bool print_phony_targets)
{
	if (dont_escape_target) {
		fputs(target, output);
	} else {
		print_makefile_escaped(target, output);
	}
	fputc(':', output);
	for (const include_t *i = includes; i != NULL; i = i->next) {
		if (i->is_system_header && !show_system_headers)
			continue;
		fputc(' ', output);
		print_makefile_escaped(i->filename, output);
	}
	fputc('\n', output);

	if (print_phony_targets) {
		bool first= true;
		for (const include_t *i = includes; i != NULL; i = i->next) {
			/* skip first (real) entry */
			if (first) {
				first = false;
				continue;
			}
			if (i->is_system_header && !show_system_headers)
				continue;
			fputc('\n', output);
			print_makefile_escaped(i->filename, output);
			fputs(":\n", output);
		}
	}
}

void emit_pp_token(void)
{
	bool had_newlines = emit_newlines();
	/* emit space before tokens */
	if (had_newlines) {
		unsigned whitespace = info.whitespace_at_line_begin;
		/* make sure there is at least 1 whitespace before a (macro-expanded)
		 * '#' at line begin. I'm not sure why this is good, but gcc does it. */
		if (pp_token.kind == '#' && whitespace == 0)
			++whitespace;
		/* in case of a macro expansion where the first tokens are empty
		 * arguments/macros we may have a space_before flag,
		 * with whitespace == 0, which we have to adjust here */
		if (whitespace == 0 && pp_token.base.space_before)
			++whitespace;
		for (unsigned i = whitespace; i-- > 0; )
			fputc(' ', out);
	} else if (pp_token.base.space_before
	  || tokens_would_paste(previous_token, pp_token.kind)) {
		fputc(' ', out);
	}

	switch (pp_token.kind) {
	case T_NUMBER:
		fputs(pp_token.literal.string->begin, out);
		break;

	case T_STRING_LITERAL:
		fputs(get_string_encoding_prefix(pp_token.literal.string->encoding), out);
		fputc('"', out);
		fputs(pp_token.literal.string->begin, out);
		fputc('"', out);
		break;

	case T_CHARACTER_CONSTANT:
		fputs(get_string_encoding_prefix(pp_token.literal.string->encoding), out);
		fputc('\'', out);
		fputs(pp_token.literal.string->begin, out);
		fputc('\'', out);
		break;

	default:
		fputs(pp_token.base.symbol->string, out);
		break;
	}
	previous_token = pp_token.kind;
}

static void eat_pp_directive(void)
{
	while (pp_token.kind != '\n' && pp_token.kind != T_EOF) {
		next_input_token();
	}
}

static bool pp_tokens_equal(const token_t *token1, const token_t *token2)
{
	if (token1->kind != token2->kind)
		return false;

	switch (token1->kind) {
	case T_NUMBER:
	case T_CHARACTER_CONSTANT:
	case T_STRING_LITERAL:
		return token1->literal.string == token2->literal.string;

	case T_MACRO_PARAMETER:
		return token1->macro_parameter.def->symbol
		    == token2->macro_parameter.def->symbol;

	default:
		return token1->base.symbol == token2->base.symbol;
	}
}

static bool pp_definitions_equal(const pp_definition_t *const definition1,
                                 const pp_definition_t *const definition2)
{
	size_t const n_parameters = definition1->n_parameters;
	if (n_parameters != definition2->n_parameters)
		return false;
	for (size_t p = n_parameters; p-- > 0; ) {
		const pp_definition_t *param1 = &definition1->parameters[p];
		const pp_definition_t *param2 = &definition2->parameters[p];
		if (param1->symbol != param2->symbol
		 || param1->is_variadic != param2->is_variadic)
			return false;
	}

	size_t const len = definition1->list_len;
	if (len != definition2->list_len)
		return false;
	for (size_t t = len; t-- > 0; ) {
		const token_t *const t1 = &definition1->token_list[t];
		const token_t *const t2 = &definition2->token_list[t];
		if (!pp_tokens_equal(t1, t2))
			return false;
		if (t > 0 && t1->base.space_before != t2->base.space_before)
			return false;
	}

	return true;
}

static pp_definition_t *add_define_(char const *const name,
                                    bool standard_define)
{
	size_t    const name_len = strlen(name);
	char     *const string   = obstack_copy(&symbol_obstack, name, name_len+1);
	symbol_t *const sym      = symbol_table_insert(string);
	if (sym->string != string)
		obstack_free(&symbol_obstack, string);

	pp_definition_t *const def = obstack_alloc(&pp_obstack, sizeof(def[0]));
	memset(def, 0, sizeof(*def));
	def->symbol          = sym;
	def->pos             = builtin_position;
	def->standard_define = standard_define;

	sym->pp_definition = def;
	return def;
}

void add_define(char const *const name, char const *const val,
                bool standard_define)
{
	pp_definition_t *const def = add_define_(name, standard_define);

	input_t *decoder = input_from_string(val, input_decode_utf8);
	switch_input(decoder, builtin_position.input_name, NULL, true);

	assert(obstack_object_size(&pp_obstack) == 0);
	for (;;) {
		next_input_token();
		if (pp_token.kind == T_EOF)
			break;

		obstack_grow(&pp_obstack, &pp_token, sizeof(pp_token));
	}

	close_pp_input();

	def->list_len   = obstack_object_size(&pp_obstack) / sizeof(def->token_list[0]);
	def->token_list = obstack_finish(&pp_obstack);
}

void add_define_string(char const *const name, char const *const val,
                       bool const standard_define)
{
	pp_definition_t *const def = add_define_(name, standard_define);

	begin_string_construction();
	size_t  const val_len = strlen(val);
	grow_escaped(&string_obst, val, val_len+1);
	string_t *string = finish_string_construction(STRING_ENCODING_CHAR);

	token_t stringtok;
	memset(&stringtok, 0, sizeof(stringtok));
	stringtok.kind           = T_STRING_LITERAL;
	stringtok.literal.string = string;

	assert(obstack_object_size(&pp_obstack) == 0);
	obstack_grow(&pp_obstack, &stringtok, sizeof(stringtok));
	def->list_len   = 1;
	def->token_list = obstack_finish(&pp_obstack);
}

string_t *make_string(char const *const string)
{
	begin_string_construction();
	size_t size = strlen(string);
	obstack_grow(&string_obst, string, size);
	return finish_string_construction(STRING_ENCODING_CHAR);
}

static void add_define_one(char const *const name)
{
	pp_definition_t *const def = add_define_(name, false);

	token_t onetok;
	memset(&onetok, 0, sizeof(onetok));
	onetok.literal.base.kind = T_NUMBER;
	onetok.literal.string    = make_string("1");

	assert(obstack_object_size(&pp_obstack) == 0);
	obstack_grow(&pp_obstack, &onetok, sizeof(onetok));
	def->list_len   = 1;
	def->token_list = obstack_finish(&pp_obstack);
}

void add_define_macro(char const *const name, char const *const macro_arg,
                      char const *const val, bool const standard_define)
{
	pp_definition_t *const def = add_define_(name, standard_define);

	input_t *decoder = input_from_string(val, input_decode_utf8);
	switch_input(decoder, builtin_position.input_name, NULL, true);

	symbol_t *const parameter_symbol = symbol_table_insert(macro_arg);
	pp_definition_t *parameter = OALLOCZ(&pp_obstack, pp_definition_t);
	parameter->pos          = builtin_position;
	parameter->symbol       = parameter_symbol;
	parameter->is_parameter = true;
	parameter->is_variadic  = false;

	assert(obstack_object_size(&pp_obstack) == 0);
	for (;;) {
		next_input_token();
		if (pp_token.kind == T_EOF)
			break;

		symbol_t *symbol = pp_token.base.symbol;
		if (symbol == parameter_symbol) {
			pp_token.kind                = T_MACRO_PARAMETER;
			pp_token.macro_parameter.def = parameter;
		}
		obstack_grow(&pp_obstack, &pp_token, sizeof(pp_token));
	}

	close_pp_input();

	def->has_parameters = true;
	def->n_parameters   = 1;
	def->parameters     = parameter;
	def->list_len
		= obstack_object_size(&pp_obstack) / sizeof(def->token_list[0]);
	def->token_list     = obstack_finish(&pp_obstack);
}

void parse_define(char const *opt)
{
	assert(obstack_object_size(&config_obstack) == 0);
	char const *p;
	for (p = opt; *p != '\0' && *p != '='; ++p) {
		obstack_1grow(&config_obstack, *p);
	}
	obstack_1grow(&config_obstack, '\0');
	char *name = obstack_finish(&config_obstack);
	if (*p == '\0') {
		add_define_one(name);
	} else {
		add_define(name, p+1, false);
	}
	obstack_free(&config_obstack, name);
}

static void add_define_dynamic_string(char const *const name, update_func update)
{
	pp_definition_t *const def = add_define_(name, true);

	token_t stringtok;
	memset(&stringtok, 0, sizeof(stringtok));
	stringtok.kind           = T_STRING_LITERAL;
	assert(obstack_object_size(&pp_obstack) == 0);
	obstack_grow(&pp_obstack, &stringtok, sizeof(stringtok));
	def->list_len   = 1;
	def->token_list = obstack_finish(&pp_obstack);
	def->update     = update;
}

static void add_define_dynamic_number(char const *const name, update_func update)
{
	pp_definition_t *const def = add_define_(name, true);

	token_t numbertok;
	memset(&numbertok, 0, sizeof(numbertok));
	numbertok.kind           = T_NUMBER;
	assert(obstack_object_size(&pp_obstack) == 0);
	obstack_grow(&pp_obstack, &numbertok, sizeof(numbertok));
	def->list_len   = 1;
	def->token_list = obstack_finish(&pp_obstack);
	def->update     = update;
}

static void update_definition_string_t(pp_definition_t *const definition,
                                       string_t *const str)
{
	token_t *token = &definition->token_list[0];
	assert(token->kind == T_STRING_LITERAL || token->kind == T_NUMBER);
	token->literal.string = str;
}

static void update_definition_string(pp_definition_t *const definition,
                                     const char *const value)
{
	/* TODO: do we need additional quoting? */
	update_definition_string_t(definition, make_string(value));
}

static void update_definition_int(pp_definition_t *const definition,
                                  unsigned value)
{
	begin_string_construction();
	obstack_printf(&string_obst, "%u", value);
	string_t *string = finish_string_construction(STRING_ENCODING_CHAR);
	update_definition_string_t(definition, string);
}

static void update_file(pp_definition_t *definition)
{
	const char *input_name = pp_token.base.pos.input_name;
	if (input_name == NULL)
		input_name = "";
	update_definition_string(definition, input_name);
}

static void update_base_file(pp_definition_t *definition)
{
	update_definition_string(definition, base_inputname);
}

static void update_line(pp_definition_t *definition)
{
	update_definition_int(definition, pp_token.base.pos.lineno);
}

static void update_include_level(pp_definition_t *definition)
{
	update_definition_int(definition, n_inputs);
}

static void update_counter(pp_definition_t *definition)
{
	update_definition_int(definition, counter);
	++counter;
}

static string_t *pp_date;
static string_t *pp_time;

static void get_date_time(void)
{
	if (pp_date != NULL)
		return;

	time_t const now = time(NULL);
	if (now == (time_t)-1)
		goto unknown_time;
	struct tm *const t = localtime(&now);
	if (t == NULL)
		goto unknown_time;
	char        buf[32];
	char const *str     = asctime_r(t, buf);
	if (str == NULL) {
unknown_time:
		str = "??? ??? ?? ??:??:?? ????";
	}

	begin_string_construction();
	obstack_grow(&string_obst, str, 10); /* Extract date part. */
	pp_date = finish_string_construction(STRING_ENCODING_CHAR);

	begin_string_construction();
	obstack_grow(&string_obst, str + 11, 8); /* Extract time part. */
	pp_time = finish_string_construction(STRING_ENCODING_CHAR);
}

static void update_date(pp_definition_t *definition)
{
	get_date_time();
	update_definition_string_t(definition, pp_date);
}

static void update_time(pp_definition_t *definition)
{
	get_date_time();
	update_definition_string_t(definition, pp_time);
}

static void update_timestamp(pp_definition_t *definition)
{
#if defined(HAVE_FILENO) && defined(HAVE_ASCTIME_R) && defined(HAVE_FSTAT)
	FILE *const file = input_get_file(input.input);
	if (file == NULL)
		goto unknown_timestamp;
	int const fd = fileno(file);
	struct stat st;
	if (fstat(fd, &st) != 0)
		goto unknown_timestamp;
	struct tm *const t = localtime(&st.st_mtime);
	if (t == NULL)
		goto unknown_timestamp;
	char        buf[32];
	char *const str = asctime_r(t, buf);
	if (str == NULL)
		goto unknown_timestamp;
	/* remove trailing '\n' */
	size_t const len = strlen(str);
	str[len - 1] = '\0';
	/* update definition */
	update_definition_string(definition, str);
	return;
unknown_timestamp:
#endif

	update_definition_string(definition, "??? ??? ?? ??:??:?? ????");
}

static void init_dynamic_macros(void)
{
	add_define_dynamic_number("__COUNTER__",       update_counter);
	add_define_dynamic_number("__INCLUDE_LEVEL__", update_include_level);
	add_define_dynamic_number("__LINE__",          update_line);
	add_define_dynamic_string("__BASE_FILE__",     update_base_file);
	add_define_dynamic_string("__DATE__",          update_date);
	add_define_dynamic_string("__FILE__",          update_file);
	add_define_dynamic_string("__TIMESTAMP__",     update_timestamp);
	add_define_dynamic_string("__TIME__",          update_time);
}

static void error_missing_macro_param(void)
{
	errorf(&pp_token.base.pos, "'#' is not followed by a macro parameter");
}

static bool is_defineable_token(char const *const context, bool defined_allowed)
{
	if (pp_token.kind == T_EOF || pp_token.kind == T_NEWLINE) {
		errorf(&pp_token.base.pos, "unexpected end of line after %s", context);
		return false;
	}

	if (!is_identifierlike_token(&pp_token)) {
		errorf(&pp_token.base.pos, "expected identifier after %s, got %K",
		       context, &pp_token);
		return false;
	}

	if (!defined_allowed && pp_token.base.symbol->pp_ID == TP_defined) {
		errorf(&pp_token.base.pos, "%K cannot be used as macro name in %s",
		       &pp_token, context);
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

	if (!is_defineable_token("#define", false))
		goto error_out;
	symbol_t *const macro_symbol = pp_token.base.symbol;

	pp_definition_t *new_definition
		= obstack_alloc(&pp_obstack, sizeof(new_definition[0]));
	memset(new_definition, 0, sizeof(new_definition[0]));
	new_definition->symbol = macro_symbol;
	new_definition->pos    = input.pos;
	next_input_token();

	/* spaces are significant: #define b(x) is different from #define b (x)*/
	if (pp_token.kind == '(' && !pp_token.base.space_before) {
		eat_token('(');

		while (true) {
			switch (pp_token.kind) {
				symbol_t *symbol;
				bool      is_variadic;
			case T_DOTDOTDOT:
				symbol      = symbol___VA_ARGS__;
				is_variadic = true;
				goto create_parameter;
			default:
				if (!is_identifierlike_token(&pp_token)) {
					errorf(&pp_token.base.pos,
						   "expected identifier, '...' or ')' in #define argument list, got %K",
						   &pp_token);
					goto error_out;
				}
				/* FALLTHROUGH */
			case T_IDENTIFIER: {
				symbol      = pp_token.base.symbol;
				is_variadic = false;
create_parameter:
				next_input_token();
				if (pp_token.kind == T_DOTDOTDOT) {
					eat_token(T_DOTDOTDOT);
					is_variadic = true;
				}

				pp_definition_t parameter;
				memset(&parameter, 0, sizeof(parameter));
				parameter.pos          = pp_token.base.pos;
				parameter.symbol       = symbol;
				parameter.is_parameter = true;
				parameter.is_variadic  = is_variadic;
				obstack_grow(&pp_obstack, &parameter, sizeof(parameter));

				if (pp_token.kind == ',') {
					if (is_variadic) {
						errorf(&input.pos, "'...' parameter must be last in macro argument list");
						goto error_out;
					}
					eat_token(',');
					break;
				}

				if (pp_token.kind != ')') {
					errorf(&pp_token.base.pos,
					       "expected ',' or ')' after identifier, got %K",
					       &pp_token);
					goto error_out;
				}
				break;
			}

			case ')':
				eat_token(')');
				goto finish_argument_list;
			}
		}

finish_argument_list:
		new_definition->has_parameters = true;
		size_t size = obstack_object_size(&pp_obstack);
		new_definition->n_parameters
			= size / sizeof(new_definition->parameters[0]);
		new_definition->parameters = obstack_finish(&pp_obstack);
		for (size_t i = 0; i < new_definition->n_parameters; ++i) {
			pp_definition_t *const param     = &new_definition->parameters[i];
			symbol_t        *const param_sym = param->symbol;
			pp_definition_t *const previous  = param_sym->pp_definition;
			if (previous != NULL
			    && previous->function_definition == new_definition) {
				errorf(&param->pos, "duplicate macro parameter '%Y'", param_sym);
				param->symbol = sym_anonymous;
				continue;
			}
			param->previous_definition = previous;
			param->function_definition = new_definition;
			param_sym->pp_definition   = param;
		}
	} else {
		if (!pp_token.base.space_before && pp_token.kind != T_NEWLINE) {
			warningf(WARN_OTHER, &pp_token.base.pos,
					 "missing whitespace after macro name");
		}
	}

	/* construct token list */
	assert(obstack_object_size(&pp_obstack) == 0);
	bool next_must_be_param = false;
	bool first              = true;
	while (pp_token.kind != T_NEWLINE) {
		symbol_t *symbol = pp_token.base.symbol;
		if (symbol != NULL) {
			pp_definition_t *const definition = symbol->pp_definition;
			if (definition != NULL
			    && definition->function_definition == new_definition) {
				pp_token.kind                = T_MACRO_PARAMETER;
				pp_token.macro_parameter.def = definition;
			} else if (symbol == symbol___VA_ARGS__) {
				warningf(WARN_OTHER, &pp_token.base.pos,
				         "__VA_ARGS__ must only appear in a C99 variadic macro");
			}
		}
		if (next_must_be_param && pp_token.kind != T_MACRO_PARAMETER) {
			error_missing_macro_param();
		}
		if (first)
			pp_token.base.space_before = false;
		obstack_grow(&pp_obstack, &pp_token, sizeof(pp_token));
		next_must_be_param
			= new_definition->has_parameters && pp_token.kind == '#';
		next_input_token();
		first = false;
	}
	if (next_must_be_param)
		error_missing_macro_param();

	size_t size     = obstack_object_size(&pp_obstack);
	size_t list_len = size/sizeof(new_definition->token_list[0]);
	new_definition->list_len   = list_len;
	new_definition->token_list = obstack_finish(&pp_obstack);

	if (list_len > 0) {
		const token_t *first_token = &new_definition->token_list[0];
		const token_t *last_token  = &new_definition->token_list[list_len-1];
		if (first_token->kind == T_HASHHASH) {
			errorf(&first_token->base.pos, "no token before '##'");
		}
		if (list_len > 1 && last_token->kind == T_HASHHASH) {
			errorf(&last_token->base.pos, "no token after '##'");
		}
	}

	if (new_definition->has_parameters) {
		for (size_t i = 0; i < new_definition->n_parameters; ++i) {
			pp_definition_t *const param     = &new_definition->parameters[i];
			symbol_t        *const param_sym = param->symbol;
			if (param_sym == sym_anonymous)
				continue;
			assert(param_sym->pp_definition == param);
			assert(param->function_definition == new_definition);
			param_sym->pp_definition   = param->previous_definition;
			param->previous_definition = NULL;
		}
	}

	pp_definition_t *old_definition = macro_symbol->pp_definition;
	if (old_definition != NULL) {
		if (old_definition->standard_define) {
			warningf(WARN_BUILTIN_MACRO_REDEFINED, &input.pos,
					 "redefining builtin macro '%Y'", macro_symbol);
		} else if (!pp_definitions_equal(old_definition, new_definition)) {
			if (warningf(WARN_OTHER, &input.pos, "multiple definitions of macro '%Y'", macro_symbol))
				notef(&old_definition->pos, "macro '%Y' first defined here", macro_symbol);
		} else {
			/* reuse the old definition */
			obstack_free(&pp_obstack, new_definition);
			new_definition = old_definition;
		}
	}

	macro_symbol->pp_definition = new_definition;
	return;

error_out:
	if (obstack_object_size(&pp_obstack) > 0) {
		char *ptr = obstack_finish(&pp_obstack);
		obstack_free(&pp_obstack, ptr);
	}
	eat_pp_directive();
}

static void do_undefine(symbol_t *symbol)
{
	pp_definition_t *old = symbol->pp_definition;
	if (old != NULL && old->standard_define) {
		warningf(WARN_BUILTIN_MACRO_REDEFINED, &input.pos,
		         "undefining builtin macro '%Y'", symbol);
	}
	symbol->pp_definition = NULL;
}

void undefine(char const *name)
{
	symbol_t *symbol = symbol_table_insert(name);
	do_undefine(symbol);
}

static void parse_undef_directive(void)
{
	eat_pp(TP_undef);
	if (skip_mode) {
		eat_pp_directive();
		return;
	}

	if (!is_defineable_token("#undef", false)) {
		eat_pp_directive();
		return;
	}

	symbol_t *symbol = pp_token.base.symbol;
	do_undefine(symbol);
	next_input_token();

	if (pp_token.kind != '\n') {
		warningf(WARN_OTHER, &input.pos,
		         "extra tokens at end of #undef directive");
	}
	eat_pp_directive();
}

/** behind an #include we can have the special headername lexems.
 * They're only allowed behind an #include so they're not recognized
 * by the normal next_preprocessing_token. We handle them as a special
 * exception here */
static const char *parse_headername(bool *system_include)
{
	if (input.c == '\n' || input.c == '\r') {
		parse_error("expected headername after #include");
		return NULL;
	}

	/* check whether we have a "... or <... headername */
	position_t pos = input.pos;
	switch (input.c) {
	{
		utf32 delimiter;
	case '<': delimiter = '>'; *system_include = true;  goto parse_name;
	case '"': delimiter = '"'; *system_include = false; goto parse_name;
parse_name:
		assert(obstack_object_size(&symbol_obstack) == 0);
		next_char();
		while (true) {
			switch (input.c) {
			case UTF32_EOF:
			case NEWLINE: {
				char *dummy = obstack_finish(&symbol_obstack);
				obstack_free(&symbol_obstack, dummy);
				errorf(&pp_token.base.pos,
				       "header name without closing '%c'", (char)delimiter);
				return NULL;
			}

			default:
				if (input.c == delimiter) {
					eat(delimiter);
					goto finish_headername;
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
		next_preprocessing_token();
		if (pp_token.kind == T_STRING_LITERAL) {
			*system_include = false;
			return pp_token.literal.string->begin;
		} else if (pp_token.kind == '<') {
			*system_include = true;
			token_t *tokens = NEW_ARR_F(token_t, 0);
			while (true) {
				next_preprocessing_token();
				if (pp_token.kind == T_EOF) {
					DEL_ARR_F(tokens);
					goto error_invalid_input;
				}
				if (pp_token.kind == '>')
					break;

				ARR_APP1(token_t, tokens, pp_token);
			}
			size_t n_tokens = ARR_LEN(tokens);
			assert(obstack_object_size(&symbol_obstack) == 0);
			for (size_t i = 0; i < n_tokens; ++i) {
				const token_t *token = &tokens[i];
				if (i > 0 && token->base.space_before)
					obstack_1grow(&symbol_obstack, ' ');
				grow_token(&symbol_obstack, token);
			}
			DEL_ARR_F(tokens);
			goto finish_headername;
		} else {
error_invalid_input:
			{
				char *dummy = obstack_finish(&symbol_obstack);
				obstack_free(&symbol_obstack, dummy);
			}

			errorf(&pp_token.base.pos,
			       "expected \"FILENAME\" or <FILENAME> after #include");
			return NULL;
		}
	}

finish_headername:
	obstack_1grow(&symbol_obstack, '\0');
	char *const  headername = obstack_finish(&symbol_obstack);
	pp_token.base.pos = pos;
	return headername;
}

static bool do_include(bool const bracket_include, bool const include_next,
                       char const *const headername)
{
	/* A file included from a system header is a system header, too. */
	bool const is_system_header = input.pos.is_system_header;

	size_t const headername_len = strlen(headername);
	/* is it an absolute path? */
	if (headername[0] == '/') {
		begin_string_construction();
		obstack_grow(&string_obst, headername, headername_len+1);
		const string_t *string = finish_string_construction(STRING_ENCODING_CHAR);
		const char *full_name = string->begin;
		FILE *file = fopen(full_name, "r");
		if (file == NULL)
			return false;
		switch_input_file(file, full_name, NULL, is_system_header);
		return true;
	}

	searchpath_entry_t *entry;
	if (include_next) {
		entry = input.path      ? input.path->next
		      : bracket_include ? bracket_searchpath.first
		      : quote_searchpath.first;
	} else if (bracket_include) {
		entry = bracket_searchpath.first;
	} else {
		/* put dirname of current input on obstack */
		const char *filename   = input.pos.input_name;
		const char *last_slash = strrchr(filename, '/');
		const char *full_name;
		begin_string_construction();
		if (last_slash != NULL) {
			size_t len = last_slash - filename;
			obstack_grow(&string_obst, filename, len + 1);
			obstack_grow(&string_obst, headername, headername_len);
		} else {
			obstack_grow(&string_obst, headername, headername_len);
		}
		const string_t *string
			= finish_string_construction(STRING_ENCODING_CHAR);
		full_name = string->begin;

		FILE *file = fopen(full_name, "r");
		if (file != NULL) {
			switch_input_file(file, full_name, NULL, is_system_header);
			return true;
		}
		entry = quote_searchpath.first;
	}

	assert(obstack_object_size(&symbol_obstack) == 0);
	/* check searchpath */
	for (; entry != NULL; entry = entry->next) {
		const char *path = entry->path;
		size_t      len  = strlen(path);
		obstack_grow(&symbol_obstack, path, len);
		if (path[len-1] != '/')
			obstack_1grow(&symbol_obstack, '/');
		obstack_grow(&symbol_obstack, headername, headername_len+1);

		size_t complete_len  = obstack_object_size(&symbol_obstack);
		char  *complete_path = obstack_finish(&symbol_obstack);
		FILE  *file          = fopen(complete_path, "r");
		if (file != NULL) {
			begin_string_construction();
			obstack_grow(&string_obst, complete_path, complete_len-1);
			obstack_free(&symbol_obstack, complete_path);
			const string_t *string
				= finish_string_construction(STRING_ENCODING_CHAR);
			const char *filename = string->begin;
			switch_input_file(file, filename, entry, entry->is_system_path);
			return true;
		}
		obstack_free(&symbol_obstack, complete_path);
	}

	return false;
}

static void parse_include_directive(bool const include_next)
{
	if (skip_mode) {
exit_skip:
		/* do not attempt to interpret headernames as tokens */
		skip_till_newline(false);
		eat_pp_directive();
		return;
	}

	/* do not eat the TP_include, since it would already parse the next token
	 * which needs special handling here. */
	skip_till_newline(true);
	bool system_include;
	const char *headername = parse_headername(&system_include);
	if (headername == NULL) {
		goto exit_skip;
	}
	bool had_nonwhitespace = skip_till_newline(false);
	if (had_nonwhitespace) {
		warningf(WARN_OTHER, &input.pos, "extra tokens at end of #include");
	}
	if (n_inputs > INCLUDE_LIMIT) {
		errorf(&pp_token.base.pos, "#include nested too deeply");
		goto exit_skip;
	}

	/* switch inputs */
	info.whitespace_at_line_begin = 0;
	info.at_line_begin            = true;
	emit_newlines();
	push_input();
	bool res = do_include(system_include, include_next, headername);
	if (res) {
		next_input_token();
	} else {
		errorf(&pp_token.base.pos, "failed including '%s': %s", headername, strerror(errno));
		pop_restore_input();
		goto exit_skip;
	}
}

static pp_conditional_t *push_conditional(void)
{
	pp_conditional_t *conditional
		= obstack_alloc(&pp_obstack, sizeof(*conditional));
	memset(conditional, 0, sizeof(*conditional));

	conditional->pos    = pp_token.base.pos;
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
			errorf(&conditional->pos, "unterminated #else");
		} else {
			errorf(&conditional->pos, "unterminated condition");
		}
		pop_conditional();
	}
}

static ir_tarval *pp_null;
static ir_tarval *pp_one;

static void       next_condition_token(void);
static bool       next_expansion_token(void);
static ir_tarval *parse_pp_expression(precedence_t prec);

static ir_tarval *parse_pp_operand(void)
{
	token_kind_t const kind = pp_token.kind;
	switch (kind) {
	case T_CHARACTER_CONSTANT: {
		char       const *const str = pp_token.literal.string->begin;
		string_encoding_t const enc = pp_token.literal.string->encoding;
		next_condition_token();
		switch (enc) {
		case STRING_ENCODING_CHAR:
		case STRING_ENCODING_UTF8: {
			long const v = str[0];
			return new_tarval_from_long(v, mode_Ls);
		}

		case STRING_ENCODING_CHAR16:
		case STRING_ENCODING_CHAR32:
		case STRING_ENCODING_WIDE: {
			char const *i = str;
			long const  v = read_utf8_char(&i);
			return new_tarval_from_long(v, mode_Ls);
		}
		}
		panic("invalid encoding");
	}

	case T_NUMBER: {
		char const *const str = pp_token.literal.string->begin;

		char const *i = str;
		while (hex_digit_value(*i) >= 0 || *i == 'X' || *i == 'x')
			++i;

		char const *const suffix      = i;
		bool              is_long     = false;
		bool              is_unsigned = false;
		for (;;) {
			if (*i == 'L' || *i == 'l') {
				if (is_long)
					goto error;
				is_long = true;
				if (*i == i[1])
					++i;
			} else if (*i == 'U' || *i == 'u') {
				if (is_unsigned)
					goto error;
				is_unsigned = true;
			} else {
				break;
			}
			++i;
		}

		ir_tarval *res;
		if (*i != '\0') {
error:
			errorf(&pp_token.base.pos, "invalid suffix '%s' on integer constant in preprocessor condition", suffix);
			res = tarval_bad;
		} else {
			ir_mode *const mode = is_unsigned ? mode_Lu : mode_Ls;
			res  = new_tarval_from_str(str, suffix - str, mode);
			if (res == tarval_bad) {
				errorf(&pp_token.base.pos, "invalid %K in preprocessor condition", &pp_token);
			}
		}
		next_condition_token();
		return res;
	}

	case '(': {
		next_condition_token();
		ir_tarval *const res = parse_pp_expression(PREC_BOTTOM);
		if (pp_token.kind == ')') {
			next_condition_token();
		} else {
			errorf(&pp_token.base.pos, "missing ')' in preprocessor condition");
		}
		return res;
	}

	case '+':
	case '-':
	case '~':
	case '!': {
		next_condition_token();
		ir_tarval *const v = parse_pp_operand();
		if (v == tarval_bad)
			return v;

		switch (kind) {
		case '+': return v;
		case '-': return tarval_neg(v);
		case '~': return tarval_not(v);
		case '!': return tarval_is_null(v) ? pp_one : pp_null;
		default:  panic("invalid operation");
		}
	}

	default:
		if (!is_identifierlike_token(&pp_token)) {
			errorf(&pp_token.base.pos, "unexpected %K in preprocessor condition", &pp_token);
			return tarval_bad;
		} else if (pp_token.base.symbol->pp_ID == TP_defined) {
			// Prevent macro expansion after 'defined'.
			bool has_paren = false;

			if (!expand_next()) {
				next_input_token();
				if (pp_token.kind == '\n') {
					errorf(&pp_token.base.pos, "unexpected end of preprocessor condition, expected '(' or identifier");
					return tarval_bad;
				}
				if (pp_token.kind == '(') {
					goto has_paren;
				} else {
					goto next;
				}
			} else if (pp_token.kind == '(') {
				if (!expand_next()) {
has_paren:
					next_input_token();
					if (pp_token.kind == '\n') {
						errorf(&pp_token.base.pos, "unexpected end of preprocessor condition, expected identifier");
						return tarval_bad;
					}
				}
				has_paren = true;
			}

			if (pp_token.kind == T_MACRO_PARAMETER) {
				start_object_macro_expansion(pp_token.macro_parameter.def);
				pp_expansion_state_t *const expansion_before = current_expansion;
restart:
				do {
					if (!expand_next())
						goto read_input;
				} while (!next_expansion_token());
				if (pp_token.kind == '(' && !has_paren) {
					has_paren = true;
					while (current_expansion->pos >= current_expansion->list_len) {
						if (current_expansion == expansion_before) {
							if (!expand_next()) {
read_input:
								next_input_token();
								if (pp_token.kind == '\n') {
									errorf(&pp_token.base.pos, "unexpected end of preprocessor condition, expected identifier");
									return tarval_bad;
								}
							}
							goto next;
						}
						pop_expansion();
					}
					goto restart;
				}
			}
next:;
			ir_tarval *res;
			if (is_identifierlike_token(&pp_token)) {
				res = pp_token.base.symbol->pp_definition ? pp_one : pp_null;
				next_condition_token();
			} else {
				errorf(&pp_token.base.pos, "unexpected %K in preprocessor condition, expected identifier", &pp_token);
				res = tarval_bad;
			}

			if (has_paren) {
				if (pp_token.kind == '\n') {
					errorf(&pp_token.base.pos, "unexpected end of preprocessor condition, expected ')'");
				} else if (pp_token.kind == ')') {
					next_condition_token();
				} else {
					errorf(&pp_token.base.pos, "unexpected %K in preprocessor condition, expected ')'", &pp_token);
				}
			}

			return res;
		} else {
			warningf(WARN_UNDEF, &pp_token.base.pos, "'%s' is not defined", pp_token.base.symbol->string);
			next_condition_token();
			return pp_null;
		}
	}
}

static ir_tarval *parse_pp_expression(precedence_t const prev_prec)
{
	ir_tarval *res = parse_pp_operand();
	for (;;) {
		precedence_t       prec;
		token_kind_t const kind = pp_token.kind;
		switch (kind) {
		case '*':
		case '/':
		case '%':                    prec = PREC_MULTIPLICATIVE; break;
		case '+':
		case '-':                    prec = PREC_ADDITIVE;       break;
		case T_LESSLESS:
		case T_GREATERGREATER:       prec = PREC_SHIFT;          break;
		case T_LESS:
		case T_GREATER:
		case T_LESSEQUAL:
		case T_GREATEREQUAL:         prec = PREC_RELATIONAL;     break;
		case T_EQUALEQUAL:
		case T_EXCLAMATIONMARKEQUAL: prec = PREC_EQUALITY;       break;
		case '&':                    prec = PREC_AND;            break;
		case '^':                    prec = PREC_XOR;            break;
		case '|':                    prec = PREC_OR;             break;
		case T_ANDAND:               prec = PREC_LOGICAL_AND;    break;
		case T_PIPEPIPE:             prec = PREC_LOGICAL_OR;     break;
		case '?':                    prec = PREC_CONDITIONAL;    break;

		default:
			return res;
		}

		if (prev_prec >= prec)
			return res;

		next_condition_token();
		if (pp_token.kind == '\n') {
			errorf(&pp_token.base.pos, "unexpected end of preprocessor condition");
			return pp_null;
		}

		if (kind == '?')
			prec = PREC_BOTTOM;
		ir_tarval *right = parse_pp_expression(prec);

		if ((res != tarval_bad && right != tarval_bad) || kind == '?') {
			ir_mode *mode = get_tarval_mode(res);
			if (kind != T_LESSLESS && kind != T_GREATERGREATER && kind != '?') {
				bool const lsigned = mode_is_signed(mode);
				bool const rsigned = mode_is_signed(get_tarval_mode(right));
				if (lsigned != rsigned) {
					mode = mode_Lu;
					if (lsigned) {
						res = tarval_convert_to(res, mode);
					} else {
						right = tarval_convert_to(right, mode);
					}
				}
			}

			switch (kind) {
			case '*':              res = tarval_mul(res, right); break;
			case '/':              res = tarval_div(res, right); break;
			case '%':              res = tarval_mod(res, right); break;
			case '+':              res = tarval_add(res, right); break;
			case '-':              res = tarval_sub(res, right, mode); break;
			case T_LESSLESS:       res = tarval_shl(res, right); break;
			case T_GREATERGREATER: res = (mode_is_signed(mode) ? tarval_shrs : tarval_shr)(res, right); break;

			{
				ir_relation rel;
			case T_LESS:                 rel = ir_relation_less;          goto cmp;
			case T_GREATER:              rel = ir_relation_greater;       goto cmp;
			case T_LESSEQUAL:            rel = ir_relation_less_equal;    goto cmp;
			case T_GREATEREQUAL:         rel = ir_relation_greater_equal; goto cmp;
			case T_EQUALEQUAL:           rel = ir_relation_equal;         goto cmp;
			case T_EXCLAMATIONMARKEQUAL: rel = ir_relation_less_greater;  goto cmp;
cmp:
				res = tarval_cmp(res, right) & rel ? pp_one : pp_null;
				break;
			}

			case '&':        res = tarval_and(res, right); break;
			case '^':        res = tarval_eor(res, right); break;
			case '|':        res = tarval_or (res, right); break;
			case T_ANDAND:   res = !tarval_is_null(res) && !tarval_is_null(right) ? pp_one : pp_null; break;
			case T_PIPEPIPE: res = !tarval_is_null(res) || !tarval_is_null(right) ? pp_one : pp_null; break;

			case '?': {
				ir_tarval *t = right;
				if (pp_token.kind != ':') {
					errorf(&pp_token.base.pos, "unexpected %K in preprocessor condition, expected ':'", &pp_token);
				} else {
					next_condition_token();
				}
				ir_tarval *f = parse_pp_expression(PREC_CONDITIONAL);

				if (res != tarval_bad) {
					if (t != tarval_bad && f != tarval_bad) {
						bool const tsigned = mode_is_signed(get_tarval_mode(t));
						bool const fsigned = mode_is_signed(get_tarval_mode(f));
						if (tsigned != fsigned) {
							if (tsigned) {
								t = tarval_convert_to(t, mode_Lu);
							} else {
								f = tarval_convert_to(f, mode_Lu);
							}
						}
					}

					res = tarval_is_null(res) ? f : t;
				}
				break;
			}

			default: panic("invalid operation");
			}
		} else {
			res = tarval_bad;
		}
	}
}

static bool parse_pp_condition(void)
{
	bool const old_resolve_escape_sequences = resolve_escape_sequences;
	resolve_escape_sequences = true;

	next_condition_token();
	bool const res = !tarval_is_null(parse_pp_expression(PREC_BOTTOM));
	if (pp_token.kind != '\n') {
		errorf(&pp_token.base.pos, "extra tokens at end of condition");
		eat_pp_directive();
	}

	resolve_escape_sequences = old_resolve_escape_sequences;

	return res;
}

static void parse_elif_directive(void)
{
	pp_conditional_t *const cond = conditional_stack;
	if (cond == NULL) {
		errorf(&pp_token.base.pos, "#elif without prior #if");
		eat_pp_directive();
		return;
	}

	if (cond->in_else) {
		errorf(&pp_token.base.pos, "#elif after #else");
		notef(&cond->pos, "condition started here");
		skip_mode = true;
		eat_pp_directive();
		return;
	}

	cond->pos = pp_token.base.pos;

	if (cond->skip || cond->condition) {
		eat_pp_directive();
		skip_mode = true;
	} else {
		skip_mode       = false;
		cond->condition = parse_pp_condition();
		skip_mode       = !cond->condition;
	}
}

static void parse_if_directive(void)
{
	pp_conditional_t *const cond = push_conditional();

	if (skip_mode) {
		cond->skip = true;
		eat_pp_directive();
		return;
	}

	cond->condition = parse_pp_condition();
	skip_mode       = !cond->condition;
}

static void parse_ifdef_ifndef_directive(bool const is_ifdef)
{
	pp_conditional_t *const conditional = push_conditional();
	bool condition;
	eat_pp(is_ifdef ? TP_ifdef : TP_ifndef);

	if (skip_mode) {
		eat_pp_directive();
		conditional->skip = true;
		return;
	}

	char const *const ctx = is_ifdef ? "#ifdef" : "#ifndef";
	if (!is_defineable_token(ctx, true)) {
		eat_pp_directive();

		/* just take the true case in the hope to avoid further errors */
		condition = true;
	} else {
		/* evaluate whether we are in true or false case */
		condition = (bool)pp_token.base.symbol->pp_definition == is_ifdef;
		next_input_token();

		if (pp_token.kind != T_NEWLINE) {
			errorf(&pp_token.base.pos, "extra tokens at end of %s", ctx);
			eat_pp_directive();
		}
	}

	conditional->condition = condition;

	if (!condition) {
		skip_mode = true;
	}
}

static void parse_else_directive(void)
{
	eat_pp(TP_else);

	if (pp_token.kind != '\n') {
		if (!skip_mode) {
			warningf(WARN_ENDIF_LABELS, &pp_token.base.pos, "extra tokens at end of #else");
		}
		eat_pp_directive();
	}

	pp_conditional_t *conditional = conditional_stack;
	if (conditional == NULL) {
		errorf(&pp_token.base.pos, "#else without prior #if");
		return;
	}

	if (conditional->in_else) {
		errorf(&pp_token.base.pos, "#else after #else");
		notef(&conditional->pos, "condition started here");
		skip_mode = true;
		return;
	}

	conditional->in_else = true;
	if (!conditional->skip) {
		skip_mode = conditional->condition;
	}
	conditional->pos = pp_token.base.pos;
}

static void parse_endif_directive(void)
{
	eat_pp(TP_endif);

	if (pp_token.kind != '\n') {
		if (!skip_mode) {
			warningf(WARN_ENDIF_LABELS, &pp_token.base.pos, "extra tokens at end of #endif");
		}
		eat_pp_directive();
	}

	pp_conditional_t *conditional = conditional_stack;
	if (conditional == NULL) {
		errorf(&pp_token.base.pos, "#endif without prior #if");
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
	if (skip_mode) {
		eat_pp_directive();
		return;
	}

	if (pp_token.kind != T_IDENTIFIER) {
		warningf(WARN_UNKNOWN_PRAGMAS, &pp_token.base.pos,
		         "expected identifier after #pragma");
		eat_pp_directive();
		return;
	}

	stdc_pragma_kind_t kind = STDC_UNKNOWN;
	if (pp_token.base.symbol->pp_ID == TP_STDC && dialect.c99) {
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
				errorf(&pp_token.base.pos, "bad STDC pragma argument");
			}
		}
	}
	eat_pp_directive();
	if (kind == STDC_UNKNOWN) {
		warningf(WARN_UNKNOWN_PRAGMAS, &pp_token.base.pos,
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
		long const line = strtol(pp_token.literal.string->begin, &end, 0);
		if (*end == '\0') {
			/* use offset -1 as this is about the next line */
			input.pos.lineno = line - 1;
			/* force output of line */
			input.output_line = input.pos.lineno - 20;
		} else {
			if (!skip_mode) {
				errorf(&input.pos, "'%S' is not a valid line number",
					   &pp_token.literal.string);
			}
		}
		next_input_token();
		if (pp_token.kind == '\n')
			return;
	}
	if (pp_token.kind == T_STRING_LITERAL
	    && pp_token.literal.string->encoding == STRING_ENCODING_CHAR) {
		input.pos.input_name       = pp_token.literal.string->begin;
		input.pos.is_system_header = false;
		next_input_token();

		/* attempt to parse numeric flags as outputted by gcc preprocessor */
		while (pp_token.kind == T_NUMBER) {
			/* flags:
			 * 1 - indicates start of a new file
			 * 2 - indicates return from a file
			 * 3 - indicates system header
			 * 4 - indicates implicit extern "C" in C++ mode
			 *
			 * currently we're only interested in "3"
			 */
			if (streq(pp_token.literal.string->begin, "3")) {
				input.pos.is_system_header = true;
			}
			next_input_token();
		}
	}

	eat_pp_directive();
}

static void parse_error_directive(void)
{
	if (skip_mode) {
		eat_pp_directive();
		return;
	}

	bool const old_resolve_escape_sequences = resolve_escape_sequences;
	resolve_escape_sequences = false;

	position_t const pos = pp_token.base.pos;
	do {
		if (pp_token.base.space_before && obstack_object_size(&pp_obstack) !=0 )
			obstack_1grow(&pp_obstack, ' ');

		switch (pp_token.kind) {
		case T_NUMBER: {
			string_t const *const str = pp_token.literal.string;
			obstack_grow(&pp_obstack, str->begin, str->size);
			break;
		}

		{
			char delim;
		case T_STRING_LITERAL:     delim =  '"'; goto string;
		case T_CHARACTER_CONSTANT: delim = '\''; goto string;
string:;
			string_t const *const str = pp_token.literal.string;
			char     const *const enc = get_string_encoding_prefix(str->encoding);
			obstack_printf(&pp_obstack, "%s%c%s%c", enc, delim, str->begin, delim);
			break;
		}

		default: {
			char const *const str = pp_token.base.symbol->string;
			obstack_grow(&pp_obstack, str, strlen(str));
			break;
		}
		}

		next_input_token();
	} while (pp_token.kind != '\n');

	resolve_escape_sequences = old_resolve_escape_sequences;

	obstack_1grow(&pp_obstack, '\0');
	char *const str = obstack_finish(&pp_obstack);
	errorf(&pos, "#%s", str);
	obstack_free(&pp_obstack, str);
}

static void parse_preprocessing_directive(void)
{
	if (current_call.macro != NULL) {
		warningf(WARN_OTHER, &pp_token.base.pos,
		         "preprocessing directive in macro argument");
		push_macro_call();
	}

	assert(stop_at_newline == false);
	stop_at_newline = true;
	eat_token('#');

	if (pp_token.kind == '\n') {
		/* empty directive */
	} else if (pp_token.base.symbol) {
		switch (pp_token.base.symbol->pp_ID) {
		case TP_define:       parse_define_directive();            break;
		case TP_elif:         parse_elif_directive();              break;
		case TP_else:         parse_else_directive();              break;
		case TP_endif:        parse_endif_directive();             break;
		case TP_error:        parse_error_directive();             break;
		case TP_if:           parse_if_directive();                break;
		case TP_ifdef:        parse_ifdef_ifndef_directive(true);  break;
		case TP_ifndef:       parse_ifdef_ifndef_directive(false); break;
		case TP_include:      parse_include_directive(false);      break;
		case TP_include_next: parse_include_directive(true);       break;
		case TP_line:         next_input_token(); goto line_directive;
		case TP_pragma:       parse_pragma_directive();            break;
		case TP_undef:        parse_undef_directive();             break;
		default:              goto skip;
		}
	} else if (pp_token.kind == T_NUMBER) {
line_directive:
		parse_line_directive();
	} else {
skip:
		if (!skip_mode) {
			errorf(&pp_token.base.pos, "invalid preprocessing directive #%K", &pp_token);
		}
		eat_pp_directive();
	}

	pop_macro_call();

	stop_at_newline = false;
	eat_token(T_NEWLINE);
}

static bool next_expansion_token(void)
{
	const token_kind_t kind = pp_token.kind;
	if (current_call.macro == NULL || argument_expanding != NULL) {
		symbol_t *const symbol = pp_token.base.symbol;
		if (symbol) {
			if (kind == T_MACRO_PARAMETER) {
				pp_definition_t *def = pp_token.macro_parameter.def;
				assert(current_expansion != NULL);
				start_object_macro_expansion(def);
				return false;
			}

			pp_definition_t *const pp_definition = symbol->pp_definition;
			if (pp_definition != NULL && !pp_token.base.expansion_forbidden) {
				if (pp_definition->is_expanding)
					pp_token.base.expansion_forbidden = true;

				if (pp_definition->is_expanding && !pp_definition->may_recurse) {
					pp_token.base.expansion_forbidden = true;
				} else if (pp_definition->has_parameters) {
					/* check if next token is a '(' */
					if (current_expansion != NULL) {
						bool         may_pop    = !pp_token.base.expansion_forbidden;
						token_kind_t next_token = peek_expansion(may_pop);
						if (next_token != '(') {
							if (may_pop)
								goto try_input;
							goto have_token;
						}
					} else {
try_input:;
						whitespace_info_t skipinfo = skip_whitespace();
						if (input.c != '(') {
							next_info         = skipinfo;
							next_space_before = pp_token.base.space_before
								| (skipinfo.whitespace_at_line_begin > 0);
							next_info_valid     = true;
							goto have_token;
						}
					}
					if (current_expansion == NULL)
						expansion_pos = pp_token.base.pos;
					push_macro_call();
					whitespace_info_t oldinfo      = info;
					bool              space_before = pp_token.base.space_before;
					next_preprocessing_token();
					assert(pp_token.kind == '(');

					start_call(pp_definition, oldinfo, space_before);
					return false;
				} else {
					if (current_expansion == NULL)
						expansion_pos = pp_token.base.pos;
					if (pp_definition->update != NULL)
						pp_definition->update(pp_definition);
					start_object_macro_expansion(pp_definition);
					return false;
				}
			}
		}
	}

have_token:
	if (current_call.macro != NULL) {
		if (kind == '(') {
			++current_call.argument_brace_count;
		} else if (kind == ')') {
			if (current_call.argument_brace_count > 0) {
				--current_call.argument_brace_count;
			} else {
				assert(kind == ')');
				start_function_macro_expansion(&current_call);
				pop_macro_call();
				info = call_whitespace_info;
				pp_token.base.space_before = call_space_before;
				return false;
			}
		} else if (kind == ',' && current_call.argument_brace_count == 0
		       && (current_call.parameter == NULL
		           || !current_call.parameter->is_variadic)) {
			finish_argument();
			current_call.parameter_idx++;
			if (current_call.parameter_idx >= current_call.macro->n_parameters) {
				errorf(&pp_token.base.pos,
				       "too many arguments passed for macro '%Y'",
				       current_call.macro->symbol);
				current_call.parameter = NULL;
			} else {
				start_argument(&current_call.macro->parameters[current_call.parameter_idx]);
			}
			return false;
		} else if (kind == T_MACRO_PARAMETER) {
			/* parameters have to be fully expanded before being used as
			 * parameters for another macro-call */
			pp_definition_t *argument = pp_token.macro_parameter.def;
			argument_expanding = argument;
			start_object_macro_expansion(argument);
			return false;
		} else if (kind == T_EOF) {
			errorf(&expansion_pos,
			       "reached end of file while parsing arguments for '%Y'",
			       current_call.macro->symbol);
			return true;
		}
		if (current_call.parameter != NULL) {
			if (current_expansion == NULL)
				pp_token.base.space_before |= info.at_line_begin;
			ARR_APP1(token_t, current_call.argument_tokens, pp_token);
		}
		return false;
	}

	return true;
}

void next_preprocessing_token(void)
{
	do {
		if (expand_next())
			continue;

		do {
			next_input_token();
			while (pp_token.kind == '#' && info.at_line_begin) {
				parse_preprocessing_directive();
			}
		} while (skip_mode && pp_token.kind != T_EOF);
	} while (!next_expansion_token());
}

static void next_condition_token(void)
{
	do {
		if (!expand_next())
			next_input_token();
	} while (!next_expansion_token());
}

void append_include_path(searchpath_t *paths, const char *path)
{
	searchpath_entry_t *entry = OALLOCZ(&config_obstack, searchpath_entry_t);
	entry->path           = path;
	entry->is_system_path = paths->is_system_path;

	*paths->anchor = entry;
	paths->anchor  = &entry->next;
}

void append_env_paths(searchpath_t *paths, const char *envvar)
{
	const char *val = getenv(envvar);
	if (val != NULL && *val != '\0') {
		const char *begin = val;
		const char *c;
		do {
			c = begin;
			while (*c != '\0' && *c != ':')
				++c;

			size_t len = c-begin;
			if (len == 0) {
				/* use "." for gcc compatibility (Matze: I would expect that
				 * nothing happens for an empty entry...) */
				append_include_path(paths, ".");
			} else {
				char *const string = obstack_copy0(&config_obstack, begin, len);
				append_include_path(paths, string);
			}

			begin = c+1;
			/* skip : */
			if (*begin == ':')
				++begin;
		} while (*c != '\0');
	}
}

static void append_searchpath(searchpath_t *path, const searchpath_t *append)
{
	*path->anchor = append->first;
}

static void setup_include_path(void)
{
	/* append system search path to bracket searchpath */
	append_searchpath(&system_searchpath,  &after_searchpath);
	append_searchpath(&bracket_searchpath, &system_searchpath);
	append_searchpath(&quote_searchpath, &bracket_searchpath);
}

void print_include_paths(void)
{
	fprintf(stderr, "#include \"...\" search starts here:\n");
	for (searchpath_entry_t *entry = quote_searchpath.first;
		 entry != bracket_searchpath.first; entry = entry->next) {
		fprintf(stderr, " %s\n", entry->path);
	}
	fprintf(stderr, "#include <...> search starts here:\n");
	for (searchpath_entry_t *entry = bracket_searchpath.first;
		 entry != NULL; entry = entry->next) {
		fprintf(stderr, " %s\n", entry->path);
	}
	fprintf(stderr, "End of search list.\n");
}

void print_defines(void)
{
	set_preprocessor_output(stdout);
	memset(&info, 0, sizeof(info));

	/* scan the symbol table for defines */
	symbol_table_iterator_t iter;
	symbol_table_iterator_init(&iter);
	for (symbol_t *symbol; (symbol = symbol_table_iterator_next(&iter)) != NULL;) {
		pp_definition_t *definition = symbol->pp_definition;
		if (definition == NULL)
			continue;
		if (definition->update != NULL)
			definition->update(definition);
		fprintf(out, "#define %s", definition->symbol->string);
		if (definition->has_parameters) {
			fputc('(', out);
			separator_t sep = { "", ", " };
			for (size_t i = 0; i != definition->n_parameters; ++i) {
				fputs(sep_next(&sep), out);
				pp_definition_t *const param = &definition->parameters[i];
				if (!param->is_variadic) {
					fprintf(out, "%s", param->symbol->string);
				} else if (param->symbol == symbol___VA_ARGS__) {
					fputs("...", out);
				} else {
					fprintf(out, "%s...", param->symbol->string);
				}
			}
			fputc(')', out);
		}
		fputc(' ', out);
		memset(&previous_token, 0, sizeof(previous_token));
		if (definition->list_len > 0)
			definition->token_list[0].base.space_before = false;
		for (size_t t = 0; t < definition->list_len; ++t) {
			const token_t *token = &definition->token_list[t];
			pp_token = *token;
			emit_pp_token();
		}
		fputc('\n', out);
	}
}

static void input_error(unsigned const delta_lines, unsigned const delta_cols, char const *const message)
{
	position_t pos = input.pos;
	/* see hack at end for switch_input() */
	if (pos.lineno == 0)
		pos.lineno = 1;
	if (delta_lines > 0) {
		pos.lineno += delta_lines;
		pos.colno = delta_cols;
	} else {
		pos.colno += delta_cols;
	}
	warningf(WARN_INVALID_BYTE_SEQUENCE, &pos, "%s", message);
}

void init_preprocessor(void)
{
	init_string_hash();
	init_symbol_table();
	init_tokens();
	obstack_init(&config_obstack);
	obstack_init(&pp_obstack);
	init_symbols();
	input.pos.input_name = "<commandline>";
	input.pos.lineno     = 0;
	input.pos.colno      = 0;

	init_dynamic_macros();
}

void setup_preprocessor(void)
{
	obstack_init(&input_obstack);
	expansion_stack  = NEW_ARR_F(pp_expansion_state_t, 0);
	argument_stack   = NEW_ARR_F(pp_argument_t, 0);
	macro_call_stack = NEW_ARR_F(macro_call_t, 0);
	pset_new_init(&includeset);
	includes = NULL;
	last_include = NULL;

	setup_include_path();

	set_input_error_callback(input_error);

	pp_null = get_mode_null(mode_Ls);
	pp_one  = get_mode_one(mode_Ls);
	set_preprocessor_output(NULL);
}

void exit_preprocessor(void)
{
	if (macro_call_stack == NULL)
		return;
	pset_new_destroy(&includeset);
	DEL_ARR_F(macro_call_stack);
	DEL_ARR_F(argument_stack);
	DEL_ARR_F(expansion_stack);
	obstack_free(&input_obstack, NULL);
	obstack_free(&pp_obstack, NULL);
	obstack_free(&config_obstack, NULL);
	exit_tokens();
	exit_symbol_table();
	exit_string_hash();
}
