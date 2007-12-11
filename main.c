#include <config.h>

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

#ifdef _WIN32

#include <fcntl.h>
#include <io.h>

/* no eXecute on Win32 */
#define X_OK 0
#define W_OK 2
#define R_OK 4

#define O_RDWR          _O_RDWR
#define O_CREAT         _O_CREAT
#define O_EXCL          _O_EXCL
#define O_BINARY        _O_BINARY

/* remap some names, we are not in the POSIX world */
#define access(fname, mode)      _access(fname, mode)
#define mktemp(tmpl)             _mktemp(tmpl)
#define open(fname, oflag, mode) _open(fname, oflag, mode)
#define fdopen(fd, mode)         _fdopen(fd, mode)
#define popen(cmd, mode)         _popen(cmd, mode)
#define pclose(file)             _pclose(file)

#else
#include <unistd.h>
#define HAVE_MKSTEMP
#endif

#ifndef WITH_LIBCORE
#define WITH_LIBCORE
#endif

#include <libfirm/firm.h>
#include <libfirm/be.h>

#include "lexer.h"
#include "token_t.h"
#include "types.h"
#include "type_hash.h"
#include "parser.h"
#include "ast2firm.h"
#include "diagnostic.h"
#include "lang_features.h"
#include "driver/firm_opt.h"
#include "driver/firm_cmdline.h"
#include "adt/error.h"
#include "write_fluffy.h"
#include "revision.h"

#ifndef PREPROCESSOR
#define PREPROCESSOR "cpp -std=c99 -U__WCHAR_TYPE__ -D__WCHAR_TYPE__=int"
#endif

#ifndef LINKER
#define LINKER    "gcc -m32"
#endif

#ifndef ASSEMBLER
#define ASSEMBLER "as --32"
#endif

/** The current c mode/dialect. */
unsigned int c_mode = _C89|_C99|_GNUC;

/** The 'machine size', 16, 32 or 64 bit, 32bit is the default. */
unsigned int machine_size = 32;

/** true if the char type is signed. */
bool char_is_signed = true;

/** true for strict language checking. */
bool strict_mode = false;

static int            verbose;
static struct obstack cppflags_obst;

#if defined(_DEBUG) || defined(FIRM_DEBUG)
/**
 * Debug printf implementation.
 *
 * @param fmt  printf style format parameter
 */
void dbg_printf(const char *fmt, ...)
{
	va_list list;

	if (firm_dump.debug_print) {
		va_start(list, fmt);
		vprintf(fmt, list);
		va_end(list);
	}  /* if */
}
#endif /* defined(_DEBUG) || defined(FIRM_DEBUG) */

static void initialize_firm(void)
{
	firm_early_init();

	dump_consts_local(1);
	dump_keepalive_edges(1);
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

#ifndef HAVE_MKSTEMP
/* cheap and nasty mkstemp replacement */
static int mkstemp(char *templ)
{
	mktemp(templ);
	return open(templ, O_RDWR|O_CREAT|O_EXCL|O_BINARY, 0600);
}
#endif

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
		} else if(strcmp(arg, "--gcc") == 0) {
			c_mode |= _GNUC;
		} else if(strcmp(arg, "--no-gcc") == 0) {
			c_mode &= ~_GNUC;
		} else if(strcmp(arg, "--ms") == 0) {
			c_mode |= _MS;
		} else if(strcmp(arg, "--signed-chars") == 0) {
			char_is_signed = true;
		} else if(strcmp(arg, "--unsigned-chars") == 0) {
			char_is_signed = false;
		} else if(strcmp(arg, "--strict") == 0) {
			strict_mode = true;
		} else if(strcmp(arg, "--no-ms") == 0) {
			c_mode &= ~_MS;
		} else if(strcmp(arg, "--lextest") == 0) {
			mode = LexTest;
		} else if(strcmp(arg, "--print-ast") == 0) {
			mode = PrintAst;
		} else if(strcmp(arg, "--print-fluffy") == 0) {
			mode = PrintFluffy;
		} else if(strcmp(arg, "--version") == 0) {
			firm_version_t ver;
			firm_get_version(&ver);
			printf("cparser (%d.%d %s) using libFirm (%u.%u", 0, 1, cparser_REVISION, ver.major, ver.minor);
			if(ver.revision[0] != 0) {
				putchar(' ');
				fputs(ver.revision, stdout);
			}
			if(ver.build[0] != 0) {
				putchar(' ');
				fputs(ver.build, stdout);
			}
			puts(")\n");
			exit(EXIT_SUCCESS);
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
		} else if(strncmp(arg, "-U", 2) == 0) {
			const char *opt;
			GET_ARG_AFTER(opt, "-U");
			obstack_printf(&cppflags_obst, " -U%s", opt);
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

			if(strcmp(opt, "omit-frame-pointer") == 0) {
				firm_be_option("omitfp");
			} else if(strcmp(opt, "no-omit-frame-pointer") == 0) {
				firm_be_option("omitfp=no");
			} else {
				int res = firm_option(opt);
				if (res == 0) {
					fprintf(stderr, "error: unknown Firm option '-f %s'\n",
					        opt);
					argument_errors = true;
					continue;
				} else if (res == -1) {
					help_displayed = true;
				}
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
		} else if(arg[0] == '-' && arg[1] == 'W') {
			const char *opt;
			GET_ARG_AFTER(opt, "-W");
			if (strcmp(opt, "error") == 0) {
				warnings_are_errors = true;
			} else {
				fprintf(stderr, "warning: ignoring gcc option -W%s\n", opt);
			}
		} else if(arg[0] == '-' && arg[1] == 'm') {
			const char *opt;
			GET_ARG_AFTER(opt, "-m");
			char *endptr;
			long int value = strtol(opt, &endptr, 10);
			if (*endptr != '\0') {
				fprintf(stderr, "error: wrong option '-m %s'\n",  opt);
				argument_errors = true;
			}
			if (value != 16 && value != 32 && value != 64) {
				fprintf(stderr, "error: option -m supports only 16, 32 or 64\n");
				argument_errors = true;
			} else {
				machine_size = (unsigned int)value;
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

	/* we do the lowering in ast2firm */
	firm_opt.lower_bitfields = FALSE;

	if(help_displayed) {
		return !argument_errors;
	}
	if(argument_errors) {
		usage(argv[0]);
		return 1;
	}

	gen_firm_init();
	init_symbol_table();
	init_tokens();
	init_types();
	init_typehash();
	init_basic_types();
	init_lexer();
	init_ast();
	init_parser();
	init_ast2firm();

	FILE *out = NULL;
	char  outnamebuf[4096];
	if(outname == NULL) {
		switch(mode) {
		case PrintAst:
		case PrintFluffy:
		case LexTest:
			if(outname == NULL)
				outname = "-";
			break;
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
	if(unit == NULL) {
		/* parsing failed because of errors */
		fprintf(stderr, "%u error(s), %u warning(s)\n", error_count, warning_count);
		return EXIT_FAILURE;
	}
	if (warning_count > 0) {
		fprintf(stderr, "%u warning(s)\n", warning_count);
	}

	if(mode == PrintAst) {
		type_set_output(out);
		ast_set_output(out);
		print_ast(unit);
		return 0;
	}
	if(mode == PrintFluffy) {
		type_set_output(out);
		ast_set_output(out);
		write_fluffy_decls(out, unit);
	}

	translation_unit_to_firm(unit);

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
