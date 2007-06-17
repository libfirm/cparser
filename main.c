#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "lexer_t.h"
#include "token_t.h"
#include "type_hash.h"
#include "parser.h"

#if 0
static
void get_output_name(char *buf, size_t buflen, const char *inputname,
                     const char *newext)
{
	size_t last_dot = 0xffffffff;
	size_t i = 0;
	for(const char *c = inputname; *c != 0; ++c) {
		if(*c == '.')
			last_dot = i;
		++i;
	}
	if(last_dot == 0xffffffff)
		last_dot = i;

	if(last_dot >= buflen)
		panic("filename too long");
	memcpy(buf, inputname, last_dot);

	size_t extlen = strlen(newext) + 1;
	if(extlen + last_dot >= buflen)
		panic("filename too long");
	memcpy(buf+last_dot, newext, extlen);
}
#endif

static
void compile(const char *fname)
{
	FILE *in = fopen(fname, "r");
	if(in == NULL) {
		fprintf(stderr, "Couldn't open '%s': %s\n", fname, strerror(errno));
		exit(1);
	}

	lexer_open_stream(in, fname);

#if 0
	token_t token;
	do {
		lexer_next_token(&token);
		print_token(stdout, &token);
		puts("");
	} while(token.type != T_EOF);
#else
	parse();
#endif

	fclose(in);
}

int main(int argc, char **argv)
{
	init_symbol_table();
	init_tokens();
	init_lexer();
	init_types();
	init_typehash();
	init_ast();
	init_parser();

	for(int i = 1; i < argc; ++i) {
		compile(argv[i]);
	}

	exit_parser();
	exit_ast();
	exit_typehash();
	exit_types();
	exit_lexer();
	exit_tokens();
	exit_symbol_table();
	return 0;
}
