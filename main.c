#include <config.h>

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

#ifndef WITH_LIBCORE
#define WITH_LIBCORE
#endif

#include <libfirm/firm.h>
#include <libfirm/be.h>

#include "lexer.h"
#include "token_t.h"
#include "type_hash.h"
#include "parser.h"
#include "ast2firm.h"
#include "driver/firm_cmdline.h"
#include "adt/error.h"
#include "write_fluffy.h"
#include "driver/firm_opt.h"

#ifndef PREPROCESSOR
#define PREPROCESSOR "cpp -std=c99 -U__WCHAR_TYPE__ -D__WCHAR_TYPE__=int"
#endif

#ifndef LINKER
#define LINKER       "gcc"
#endif

#ifndef ASSEMBLER
#define ASSEMBLER "as"
#endif

#ifdef _WIN32
/* remap some names */
#define popen(cmd, mode)  _popen(cmd, mode)
#define pclose(file)      _pclose(file)
#endif /* _WIN32 */

static int            verbose;
static bool           do_dump;
static struct obstack cppflags_obst;

static void initialize_firm(void)
{
	firm_early_init();

	dump_consts_local(1);
	dump_keepalive_edges(1);
}

static void dump(ir_graph *irg, const char *suffix)
{
	if(do_dump) {
		dump_ir_block_graph(irg, suffix);
	}
}

static void get_output_name(char *buf, size_t buflen, const char *inputname,
                            const char *newext)
{
	size_t last_dot = 0xffffffff;
	size_t i = 0;

	if(inputname == NULL) {
		snprintf(buf, buflen, "a%s", newext);
		return;
	}

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

static translation_unit_t *do_parsing(FILE *const in, const char *const input)
{
	lexer_open_stream(in, input);
	translation_unit_t *unit = parse();
	return unit;
}

static void lextest(FILE *in, const char *fname)
{
	lexer_open_stream(in, fname);

	do {
		lexer_next_preprocessing_token();
		print_token(stdout, &lexer_token);
		puts("");
	} while(lexer_token.type != T_EOF);
}

static FILE* preprocess(FILE* in, const char *fname)
{
	char buf[4096];
	const char *flags = obstack_finish(&cppflags_obst);

	if(in != stdin) {
		snprintf(buf, sizeof(buf), PREPROCESSOR " %s %s", flags, fname);
	} else {
		/* read from stdin */
		snprintf(buf, sizeof(buf), PREPROCESSOR " %s -", flags);
	}

	if(verbose) {
		puts(buf);
	}
	FILE* f = popen(buf, "r");
	if (f == NULL) {
		fprintf(stderr, "invoking preprocessor failed\n");
		exit(1);
	}
	return f;
}

static void do_link(const char *out, const char *in)
{
	char buf[4096];

	snprintf(buf, sizeof(buf), "%s %s -o %s", LINKER, in, out);
	if(verbose) {
		puts(buf);
	}
	int err = system(buf);
	if(err != 0) {
		fprintf(stderr, "linker reported an error\n");
		exit(1);
	}
}

static void assemble(const char *out, const char *in)
{
	char buf[4096];

	snprintf(buf, sizeof(buf), "%s %s -o %s", ASSEMBLER, in, out);
	if(verbose) {
		puts(buf);
	}

	int err = system(buf);
	if(err != 0) {
		fprintf(stderr, "assembler reported an error\n");
		exit(1);
	}
}

static const char *try_dir(const char *dir)
{
	if(dir == NULL)
		return dir;
	if(access(dir, R_OK | W_OK | X_OK) == 0)
		return dir;
	return NULL;
}

static const char *get_tempdir(void)
{
	static const char *tmpdir = NULL;

	if(tmpdir != NULL)
		return tmpdir;

	if(tmpdir == NULL)
		tmpdir = try_dir(getenv("TMPDIR"));
	if(tmpdir == NULL)
		tmpdir = try_dir(getenv("TMP"));
	if(tmpdir == NULL)
		tmpdir = try_dir(getenv("TEMP"));

#ifdef P_tmpdir
	if(tmpdir == NULL)
		tmpdir = try_dir(P_tmpdir);
#endif

	if(tmpdir == NULL)
		tmpdir = try_dir("/var/tmp");
	if(tmpdir == NULL)
		tmpdir = try_dir("/usr/tmp");
	if(tmpdir == NULL)
		tmpdir = try_dir("/tmp");

	if(tmpdir == NULL)
		tmpdir = ".";

	return tmpdir;
}

/**
 * an own version of tmpnam, which: writes in a buffer, appends a user specified
 * suffix, emits no warnings during linking (like glibc/gnu ld do for tmpnam)...
 */
static FILE *make_temp_file(char *buffer, size_t buflen,
                            const char *prefix, const char *suffix)
{
	const char *tempdir = get_tempdir();

	/* oh well... mkstemp doesn't accept a suffix after XXXXXX... */
	(void) suffix;
	suffix = "";

	snprintf(buffer, buflen, "%s/%sXXXXXX%s", tempdir, prefix, suffix);

	int fd = mkstemp(buffer);
	if(fd == -1) {
		fprintf(stderr, "couldn't create temporary file: %s\n",
		        strerror(errno));
		exit(1);
	}
	FILE *out = fdopen(fd, "w");
	if(out == NULL) {
		fprintf(stderr, "couldn't create temporary file FILE*\n");
		exit(1);
	}

	return out;
}

/**
 * Do the necessary lowering for compound parameters.
 */
void lower_compound_params(void)
{
	lower_params_t params;

	params.def_ptr_alignment    = 4;
	params.flags                = LF_COMPOUND_RETURN | LF_RETURN_HIDDEN;
	params.hidden_params        = ADD_HIDDEN_ALWAYS_IN_FRONT;
	params.find_pointer_type    = NULL;
	params.ret_compound_in_regs = NULL;
	lower_calls_with_compounds(&params);
}

static void create_firm_prog(translation_unit_t *unit)
{
	translation_unit_to_firm(unit);

	int n_irgs = get_irp_n_irgs();
	for(int i = 0; i < n_irgs; ++i) {
		ir_graph *const irg = get_irp_irg(i);
		dump(irg, "-start");
	}

	lower_compound_params();
	lower_highlevel();

	for(int i = 0; i < n_irgs; ++i) {
		ir_graph *const irg = get_irp_irg(i);
		dump(irg, "-lower");
	}
}

typedef enum compile_mode_t {
	ParseOnly,
	Compile,
	CompileDump,
	CompileAssemble,
	CompileAssembleLink,
	LexTest,
	PrintAst,
	PrintFluffy
} compile_mode_t;

static void usage(const char *argv0)
{
	fprintf(stderr, "Usage %s input [-o output] [-c]\n", argv0);
}

int main(int argc, char **argv)
{
	initialize_firm();

	init_symbol_table();
	init_tokens();
	init_types();
	init_typehash();
	init_lexer();
	init_ast();
	init_parser();
	init_ast2firm();

	const char     *input        = NULL;
	const char     *outname      = NULL;
	const char     *dumpfunction = NULL;
	compile_mode_t  mode         = CompileAssembleLink;

	obstack_init(&cppflags_obst);

#define GET_ARG_AFTER(def, args)                                             \
	def = &arg[sizeof(args)-1];                                              \
	if(def[0] == '\0') {                                                     \
		++i;                                                                 \
		if(i >= argc) {                                                      \
			fprintf(stderr, "error: expected argument after '" args "'\n");  \
			argument_errors = true;                                          \
			break;                                                           \
		}                                                                    \
		def = argv[i];                                                       \
		if(def[0] == '-' && def[1] != '\0') {                                \
			fprintf(stderr, "error: expected argument after '" args "'\n");  \
			argument_errors = true;                                          \
			continue;                                                        \
		}                                                                    \
	}

	bool help_displayed  = false;
	bool argument_errors = false;
	for(int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if(strncmp(arg, "-o", 2) == 0) {
			GET_ARG_AFTER(outname, "-o");
		} else if(strcmp(arg, "-c") == 0) {
			mode = CompileAssemble;
		} else if(strcmp(arg, "-S") == 0) {
			mode = Compile;
		} else if(strcmp(arg, "--lextest") == 0) {
			mode = LexTest;
		} else if(strcmp(arg, "--print-ast") == 0) {
			mode = PrintAst;
		} else if(strcmp(arg, "--print-fluffy") == 0) {
			mode = PrintFluffy;
		} else if(strcmp(arg, "-fsyntax-only") == 0) {
			mode = ParseOnly;
		} else if(strncmp(arg, "-I", 2) == 0) {
			const char *opt;
			GET_ARG_AFTER(opt, "-I");
			obstack_printf(&cppflags_obst, " -I%s", opt);
		} else if(strncmp(arg, "-D", 2) == 0) {
			const char *opt;
			GET_ARG_AFTER(opt, "-D");
			obstack_printf(&cppflags_obst, " -D%s", opt);
		} else if(strcmp(arg, "--dump") == 0) {
			do_dump = true;
		} else if(strcmp(arg, "--dump-function") == 0) {
			++i;
			if(i >= argc) {
				fprintf(stderr, "error: "
				        "expected argument after '--dump-function'\n");
				argument_errors = true;
				break;
			}
			dumpfunction = argv[i];
			mode         = CompileDump;
		} else if(strcmp(arg, "-v") == 0) {
			verbose = 1;
		} else if(arg[0] == '-' && arg[1] == 'f') {
			const char *opt;
			GET_ARG_AFTER(opt, "-f");
			int res = firm_option(opt);
			if (res == 0) {
				fprintf(stderr, "error: unknown Firm option '-f %s'\n", opt);
				argument_errors = true;
				continue;
			} else if (res == -1) {
				help_displayed = true;
			}
		} else if(arg[0] == '-' && arg[1] == 'b') {
			const char *opt;
			GET_ARG_AFTER(opt, "-b");
			int res = firm_be_option(opt);
			if (res == 0) {
				fprintf(stderr, "error: unknown Firm backend option '-b %s'\n",
				        opt);
				argument_errors = true;
			} else if (res == -1) {
				help_displayed = true;
			}
		} else if(arg[0] == '-') {
			if (arg[1] == '\0') {
				if(input != NULL) {
					fprintf(stderr, "error: multiple input files specified\n");
					argument_errors = true;
				} else {
					input = arg;
				}
			} else if(strcmp(arg, "-pedantic") == 0) {
				fprintf(stderr, "warning: ignoring gcc option '%s'\n", arg);
			} else if(arg[1] == 'O' ||
					arg[1] == 'f' ||
					arg[1] == 'W' ||
					arg[1] == 'g' ||
					strncmp(arg + 1, "std=", 4) == 0) {
				fprintf(stderr, "warning: ignoring gcc option '%s'\n", arg);
			} else {
				fprintf(stderr, "error: unknown argument '%s'\n", arg);
				argument_errors = true;
			}
		} else {
			if(input != NULL) {
				fprintf(stderr, "error: multiple input files specified\n");
				argument_errors = true;
			} else {
				input = arg;
			}
		}
	}

	if(help_displayed) {
		return !argument_errors;
	}
	if(argument_errors) {
		usage(argv[0]);
		return 1;
	}

	FILE *out;
	char  outnamebuf[4096];
	if(outname == NULL) {
		switch(mode) {
		case PrintAst:
		case PrintFluffy:
		case LexTest:
		case ParseOnly:
			break;
		case Compile:
			get_output_name(outnamebuf, sizeof(outnamebuf), input, ".s");
			outname = outnamebuf;
			break;
		case CompileAssemble:
			get_output_name(outnamebuf, sizeof(outnamebuf), input, ".o");
			outname = outnamebuf;
			break;
		case CompileDump:
			get_output_name(outnamebuf, sizeof(outnamebuf), dumpfunction,
			                ".vcg");
			outname = outnamebuf;
			break;
		case CompileAssembleLink:
			outname = "a.out";
			break;
		}
	}

	if(outname != NULL) {
		if(strcmp(outname, "-") == 0) {
			out = stdout;
		} else {
			out = fopen(outname, "w");
			if(out == NULL) {
				fprintf(stderr, "Couldn't open '%s' for writing: %s\n", outname,
				        strerror(errno));
				return 1;
			}
		}
	}

	FILE *in;
	if(input == NULL) {
		fprintf(stderr, "%s: no input files\n", argv[0]);
		return 1;
	} else if(strcmp(input, "-") == 0) {
		in    = stdin;
		input = "<stdin>";
	} else {
		in = fopen(input, "r");
		if(in == NULL) {
			fprintf(stderr, "Couldn't open '%s': %s\n", input, strerror(errno));
			return 1;
		}
	}

	if(mode == LexTest) {
		lextest(in, input);
		fclose(in);
		return 0;
	}

	FILE *preprocessed_in = preprocess(in, input);
	translation_unit_t *const unit = do_parsing(preprocessed_in, input);
	pclose(preprocessed_in);
	if(unit == NULL)
		return 1;

	if(mode == PrintAst) {
		ast_set_output(out);
		print_ast(unit);
		return 0;
	}
	if(mode == PrintFluffy) {
		write_fluffy_decls(out, unit);
	}

	gen_firm_init();
	create_firm_prog(unit);

	if(mode == ParseOnly) {
		return 0;
	}

	FILE *asm_out;
	char  asm_tempfile[1024];
	if(mode == CompileDump) {
		asm_out = NULL;
		firm_be_opt.selection = BE_NONE;
	} else if(mode == Compile) {
		asm_out = out;
	} else {
		asm_out
			= make_temp_file(asm_tempfile, sizeof(asm_tempfile), "cc", ".s");
	}
	gen_firm_finish(asm_out, input, /*c_mode=*/1, /*firm_const_exists=*/0);

	if(mode == CompileDump) {
		/* find irg */
		ident    *id     = new_id_from_str(dumpfunction);
		ir_graph *irg    = NULL;
		int       n_irgs = get_irp_n_irgs();
		for(int i = 0; i < n_irgs; ++i) {
			ir_graph *tirg   = get_irp_irg(i);
			ident    *irg_id = get_entity_ident(get_irg_entity(tirg));
			if(irg_id == id) {
				irg = tirg;
				break;
			}
		}

		if(irg == NULL) {
			fprintf(stderr, "No graph for function '%s' found\n", dumpfunction);
			return 1;
		}

		dump_ir_block_graph_file(irg, out);
		fclose(out);
		return 0;
	}

	fclose(asm_out);

	/* assemble assembler and create object file */
	char obj_tfile[1024];
	if(mode == CompileAssemble || mode == CompileAssembleLink) {
		const char *obj_outfile;
		if(mode == CompileAssemble) {
			fclose(out);
			obj_outfile = outname;
		} else {
			FILE *tempf
				= make_temp_file(obj_tfile, sizeof(obj_tfile), "cc", ".o");
			fclose(tempf);
			obj_outfile = obj_tfile;
		}

		assemble(obj_outfile, asm_tempfile);
	}

	/* link object file */
	if(mode == CompileAssembleLink) {
		do_link(outname, obj_tfile);
	}

	obstack_free(&cppflags_obst, NULL);

	exit_ast2firm();
	exit_parser();
	exit_ast();
	exit_lexer();
	exit_typehash();
	exit_types();
	exit_tokens();
	exit_symbol_table();
	return 0;
}
