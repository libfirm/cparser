/*
 * This file is part of cparser.
 * Copyright (C) 2007-2008 Matthias Braun <matze@braunis.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
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
#include "write_caml.h"
#include "revision.h"
#include "warning.h"

#ifndef PREPROCESSOR
#define PREPROCESSOR "cpp -std=c99 -U__WCHAR_TYPE__ -D__WCHAR_TYPE__=int -U__SIZE_TYPE__ -D__SIZE_TYPE__=__SIZE_TYPE__ -m32 -U__STRICT_ANSI__"
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

/** use builtins for some libc functions */
bool use_builtins = false;

/* to switch on printing of implicit casts */
extern bool print_implicit_casts;

/* to switch on printing of parenthesis to indicate operator precedence */
extern bool print_parenthesis;

static int            verbose;
static struct obstack cppflags_obst, ldflags_obst;

typedef struct file_list_entry_t file_list_entry_t;

typedef enum filetype_t {
	FILETYPE_AUTODETECT,
	FILETYPE_C,
	FILETYPE_PREPROCESSED_C,
	FILETYPE_ASSEMBLER,
	FILETYPE_PREPROCESSED_ASSEMBLER,
	FILETYPE_OBJECT,
	FILETYPE_UNKNOWN
} filetype_t;

struct file_list_entry_t {
	const char        *name; /**< filename or NULL for stdin */
	filetype_t         type;
	file_list_entry_t *next;
};

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

#include "builtins.h"

static translation_unit_t *do_parsing(FILE *const in, const char *const input_name)
{
	start_parsing();

	if (use_builtins) {
		lexer_open_buffer(builtins, sizeof(builtins)-1, "<builtin>");
		parse();
	}

	lexer_open_stream(in, input_name);
	parse();

	translation_unit_t *unit = finish_parsing();
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

static void add_flag(struct obstack *obst, const char *format, ...)
{
	va_list ap;
	va_start(ap, format);

	char buf[4096];
	vsnprintf(buf, sizeof(buf), format, ap);

	/* escape stuff... */
	obstack_1grow(obst, ' ');
	for (char *c = buf; *c != '\0'; ++c) {
		switch(*c) {
		case '"':
		case '\'':
		case '`':
		case ' ':
		case '\t':
		case '\n':
		case '\r':
		case '\\':
		case '$':
		case '(':
		case ')':
			obstack_1grow(obst, '\\');
			/* fallthrough */
		default:
			obstack_1grow(obst, *c);
			break;
		}
	}

	va_end(ap);
}

static FILE *preprocess(const char *fname)
{
	char buf[4096];
	obstack_1grow(&cppflags_obst, '\0');
	const char *flags = obstack_finish(&cppflags_obst);

	snprintf(buf, sizeof(buf), PREPROCESSOR " %s %s", flags, fname);
	if(verbose) {
		puts(buf);
	}

	FILE *f = popen(buf, "r");
	if(f == NULL) {
		fprintf(stderr, "invoking preprocessor failed\n");
		exit(1);
	}
	return f;
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
 * an own version of tmpnam, which: writes in a buffer, emits no warnings
 * during linking (like glibc/gnu ld do for tmpnam)...
 */
static FILE *make_temp_file(char *buffer, size_t buflen, const char *prefix)
{
	const char *tempdir = get_tempdir();

	snprintf(buffer, buflen, "%s/%sXXXXXX", tempdir, prefix);

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
	BenchmarkParser,
	PreprocessOnly,
	ParseOnly,
	Compile,
	CompileDump,
	CompileAssemble,
	CompileAssembleLink,
	LexTest,
	PrintAst,
	PrintFluffy,
	PrintCaml
} compile_mode_t;

static void usage(const char *argv0)
{
	fprintf(stderr, "Usage %s input [-o output] [-c]\n", argv0);
}

static void print_cparser_version(void) {
	firm_version_t ver;
	firm_get_version(&ver);

	printf("cparser (%s) using libFirm (%u.%u",
		cparser_REVISION, ver.major, ver.minor);
	if(ver.revision[0] != 0) {
		putchar(' ');
		fputs(ver.revision, stdout);
	}
	if(ver.build[0] != 0) {
		putchar(' ');
		fputs(ver.build, stdout);
	}
	puts(")\n");
}

static void set_be_option(const char *arg)
{
	int res = firm_be_option(arg);
	(void) res;
	assert(res);
}

static void set_option(const char *arg)
{
	int res = firm_option(arg);
	(void) res;
	assert(res);
}

static void copy_file(FILE *dest, FILE *input)
{
	char buf[16384];

	while (!feof(input) && !ferror(dest)) {
		size_t read = fread(buf, 1, sizeof(buf), input);
		if(fwrite(buf, 1, read, dest) != read) {
			perror("couldn't write output");
		}
	}
}

static FILE *open_file(const char *filename)
{
	if (strcmp(filename, "-") == 0) {
		return stdin;
	}

	FILE *in = fopen(filename, "r");
	if(in == NULL) {
		fprintf(stderr, "Couldn't open '%s': %s\n", filename,
				strerror(errno));
		exit(1);
	}

	return in;
}

static filetype_t get_filetype_from_string(const char *string)
{
	if (strcmp(string, "c") == 0 || strcmp(string, "c-header") == 0)
		return FILETYPE_C;
	if (strcmp(string, "assembler") == 0)
		return FILETYPE_PREPROCESSED_ASSEMBLER;
	if (strcmp(string, "assembler-with-cpp") == 0)
		return FILETYPE_ASSEMBLER;
	if (strcmp(string, "none") == 0)
		return FILETYPE_AUTODETECT;

	return FILETYPE_UNKNOWN;
}

int main(int argc, char **argv)
{
	initialize_firm();

	const char        *outname      = NULL;
	const char        *dumpfunction = NULL;
	compile_mode_t     mode         = CompileAssembleLink;
	int                opt_level    = 1;
	int                result       = EXIT_SUCCESS;
	char               cpu_arch[16] = "ia32";
	file_list_entry_t *files        = NULL;
	file_list_entry_t *last_file    = NULL;
	struct obstack     file_obst;

	obstack_init(&cppflags_obst);
	obstack_init(&ldflags_obst);
	obstack_init(&file_obst);

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

#define SINGLE_OPTION(ch) (option[0] == (ch) && option[1] == '\0')

	/* early options parsing (find out optimisation level) */
	for(int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if(arg[0] != '-')
			continue;

		const char *option = &arg[1];
		if(option[0] == 'O') {
			sscanf(&option[1], "%d", &opt_level);
		}
	}

	/* apply optimisation level */
	switch(opt_level) {
	case 0:
		set_option("no-opt");
		break;
	case 1:
		set_option("no-inline");
		break;
	default:
	case 4:
		set_option("strict-aliasing");
		/* fallthrough */
	case 3:
		set_option("cond-eval");
		set_option("if-conv");
		use_builtins = true;
		/* fallthrough */
	case 2:
		set_option("inline");
		set_option("deconv");
		set_be_option("omitfp");
		break;
	}

	/* parse rest of options */
	filetype_t  forced_filetype = FILETYPE_AUTODETECT;
	bool        help_displayed  = false;
	bool        argument_errors = false;
	for(int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if(arg[0] == '-' && arg[1] != 0) {
			/* an option */
			const char *option = &arg[1];
			if(option[0] == 'o') {
				GET_ARG_AFTER(outname, "-o");
			} else if(option[0] == 'g') {
				set_be_option("debuginfo=stabs");
				set_be_option("omitfp=no");
				set_be_option("ia32-nooptcc=yes");
			} else if(SINGLE_OPTION('c')) {
				mode = CompileAssemble;
			} else if(SINGLE_OPTION('E')) {
				mode = PreprocessOnly;
			} else if(SINGLE_OPTION('S')) {
				mode = Compile;
			} else if(option[0] == 'O') {
				continue;
			} else if(option[0] == 'I') {
				const char *opt;
				GET_ARG_AFTER(opt, "-I");
				add_flag(&cppflags_obst, "-I%s", opt);
			} else if(option[0] == 'D') {
				const char *opt;
				GET_ARG_AFTER(opt, "-D");
				add_flag(&cppflags_obst, "-D%s", opt);
			} else if(option[0] == 'U') {
				const char *opt;
				GET_ARG_AFTER(opt, "-U");
				add_flag(&cppflags_obst, "-U%s", opt);
			} else if(option[0] == 'l') {
				const char *opt;
				GET_ARG_AFTER(opt, "-l");
				add_flag(&ldflags_obst, "-l%s", opt);
			} else if(option[0] == 'L') {
				const char *opt;
				GET_ARG_AFTER(opt, "-L");
				add_flag(&ldflags_obst, "-L%s", opt);
			} else if(SINGLE_OPTION('v')) {
				verbose = 1;
			} else if(SINGLE_OPTION('w')) {
				inhibit_all_warnings = true;
			} else if(option[0] == 'x') {
				const char *opt;
				GET_ARG_AFTER(opt, "-x");
				forced_filetype = get_filetype_from_string(opt);
				if (forced_filetype == FILETYPE_UNKNOWN) {
					fprintf(stderr, "Unknown language '%s'\n", opt);
					argument_errors = true;
				}
			} else if(strcmp(option, "M") == 0) {
				mode = PreprocessOnly;
				add_flag(&cppflags_obst, "-M");
			} else if(strcmp(option, "MMD") == 0
					|| strcmp(option, "MD") == 0
					|| strcmp(option, "MM") == 0) {
				add_flag(&cppflags_obst, "-%s", option);
			} else if(strcmp(option, "MT") == 0
					|| strcmp(option, "MQ") == 0
					|| strcmp(option, "MF") == 0) {
				const char *opt;
				GET_ARG_AFTER(opt, "-MT");
				add_flag(&cppflags_obst, "-%s", option);
				add_flag(&cppflags_obst, "%s", opt);
			} else if(strcmp(option, "pipe") == 0) {
				/* here for gcc compatibility */
			} else if(option[0] == 'f') {
				const char *opt;
				GET_ARG_AFTER(opt, "-f");

				if(strcmp(opt, "syntax-only") == 0) {
					mode = ParseOnly;
				} else if(strcmp(opt, "omit-frame-pointer") == 0) {
					set_be_option("omitfp");
				} else if(strcmp(opt, "no-omit-frame-pointer") == 0) {
					set_be_option("omitfp=no");
				} else if(strcmp(opt, "strength-reduce") == 0) {
					firm_option("strength-red");
				} else if(strcmp(opt, "fast-math") == 0
						|| strcmp(opt, "unroll-loops") == 0
						|| strcmp(opt, "expensive-optimizations") == 0
						|| strcmp(opt, "no-common") == 0
						|| strcmp(opt, "PIC") == 0
						|| strncmp(opt, "align-loops=", sizeof("align-loops=")-1) == 0
						|| strncmp(opt, "align-jumps=", sizeof("align-jumps=")-1) == 0
						|| strncmp(opt, "align-functions=", sizeof("align-functions=")-1) == 0) {
					fprintf(stderr, "ignoring gcc option '-f %s'\n", opt);
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
			} else if(option[0] == 'b') {
				const char *opt;
				GET_ARG_AFTER(opt, "-b");
				int res = firm_be_option(opt);
				if (res == 0) {
					fprintf(stderr, "error: unknown Firm backend option '-b %s'\n",
					        opt);
					argument_errors = true;
				} else if (res == -1) {
					help_displayed = true;
				} else {
					if (strncmp(opt, "isa=", 4) == 0)
						strncpy(cpu_arch, opt, sizeof(cpu_arch));
				}
			} else if(option[0] == 'W') {
				if(strncmp(option + 1, "l,", 2) == 0)	// a gcc-style linker option
				{
					const char *opt;
					GET_ARG_AFTER(opt, "-Wl,");
					add_flag(&ldflags_obst, "-Wl,%s", opt);
				}
				else set_warning_opt(&option[1]);
			} else if(option[0] == 'm') {
				/* -m options */
				const char *opt;
				char arch_opt[64];

				GET_ARG_AFTER(opt, "-m");
				if(strncmp(opt, "arch=", 5) == 0) {
					GET_ARG_AFTER(opt, "-march=");
					snprintf(arch_opt, sizeof(arch_opt), "%s-arch=%s", cpu_arch, opt);
					int res = firm_be_option(arch_opt);
					if (res == 0)
						argument_errors = true;
					else {
						snprintf(arch_opt, sizeof(arch_opt), "%s-opt=%s", cpu_arch, opt);
						int res = firm_be_option(arch_opt);
						if (res == 0)
							argument_errors = true;
					}
				} else if(strncmp(opt, "tune=", 5) == 0) {
					GET_ARG_AFTER(opt, "-mtune=");
					snprintf(arch_opt, sizeof(arch_opt), "%s-opt=%s", cpu_arch, opt);
					int res = firm_be_option(arch_opt);
					if (res == 0)
						argument_errors = true;
				} else if(strncmp(opt, "cpu=", 4) == 0) {
					GET_ARG_AFTER(opt, "-mcpu=");
					snprintf(arch_opt, sizeof(arch_opt), "%s-arch=%s", cpu_arch, opt);
					int res = firm_be_option(arch_opt);
					if (res == 0)
						argument_errors = true;
				} else if(strncmp(opt, "fpmath=", 7) == 0) {
					GET_ARG_AFTER(opt, "-mfpmath=");
					if(strcmp(opt, "387") == 0)
						opt = "x87";
					else if(strcmp(opt, "sse") == 0)
						opt = "sse2";
					else {
						fprintf(stderr, "error: option -mfpumath supports only 387 or sse\n");
						argument_errors = true;
					}
					if(!argument_errors) {
						snprintf(arch_opt, sizeof(arch_opt), "%s-fpunit=%s", cpu_arch, opt);
						int res = firm_be_option(arch_opt);
						if (res == 0)
							argument_errors = true;
					}
				} else if(strncmp(opt, "preferred-stack-boundary=", 25) == 0) {
					GET_ARG_AFTER(opt, "-mpreferred-stack-boundary=");
					snprintf(arch_opt, sizeof(arch_opt), "%s-stackalign=%s", cpu_arch, opt);
					int res = firm_be_option(arch_opt);
					if (res == 0)
						argument_errors = true;
				} else if(strcmp(opt, "omit-leaf-frame-pointer") == 0) {
					set_be_option("omitleaffp=1");
				} else if(strcmp(opt, "no-omit-leaf-frame-pointer") == 0) {
					set_be_option("omitleaffp=0");
				} else {
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
				}
			} else if(strcmp(option, "pg") == 0) {
				set_be_option("gprof");
				add_flag(&ldflags_obst, "-pg");
			} else if(strcmp(option, "pedantic") == 0) {
				fprintf(stderr, "warning: ignoring gcc option '%s'\n", arg);
			} else if(strcmp(option, "shared") == 0) {
				add_flag(&ldflags_obst, "-shared");
			} else if(strncmp(option, "std=", 4) == 0) {
				if(strcmp(&option[4], "c99") == 0) {
					c_mode = _C89|_C99;
				} else if(strcmp(&option[4], "c89") == 0) {
					c_mode = _C89;
				} else if(strcmp(&option[4], "gnu99") == 0) {
					c_mode = _C89|_C99|_GNUC;
				} else if(strcmp(&option[4], "microsoft") == 0) {
					c_mode = _C89|_C99|_MS;
				} else
					fprintf(stderr, "warning: ignoring gcc option '%s'\n", arg);
			} else if(strcmp(option, "version") == 0) {
				print_cparser_version();
			} else if (option[0] == '-') {
				/* double dash option */
				++option;
				if(strcmp(option, "gcc") == 0) {
					c_mode |= _GNUC;
				} else if(strcmp(option, "no-gcc") == 0) {
					c_mode &= ~_GNUC;
				} else if(strcmp(option, "ms") == 0) {
					c_mode |= _MS;
				} else if(strcmp(option, "no-ms") == 0) {
					c_mode &= ~_MS;
				} else if(strcmp(option, "signed-chars") == 0) {
					char_is_signed = true;
				} else if(strcmp(option, "unsigned-chars") == 0) {
					char_is_signed = false;
				} else if(strcmp(option, "strict") == 0) {
					strict_mode = true;
				} else if(strcmp(option, "lextest") == 0) {
					mode = LexTest;
				} else if(strcmp(option, "benchmark") == 0) {
					mode = BenchmarkParser;
				} else if(strcmp(option, "print-ast") == 0) {
					mode = PrintAst;
				} else if(strcmp(option, "print-implicit-cast") == 0) {
					print_implicit_casts = true;
				} else if(strcmp(option, "print-parenthesis") == 0) {
					print_parenthesis = true;
				} else if(strcmp(option, "print-fluffy") == 0) {
					mode = PrintFluffy;
				} else if(strcmp(option, "print-caml") == 0) {
					mode = PrintCaml;
				} else if(strcmp(option, "version") == 0) {
					print_cparser_version();
					exit(EXIT_SUCCESS);
				} else if(strcmp(option, "dump-function") == 0) {
					++i;
					if(i >= argc) {
						fprintf(stderr, "error: "
						        "expected argument after '--dump-function'\n");
						argument_errors = true;
						break;
					}
					dumpfunction = argv[i];
					mode         = CompileDump;
				} else {
					fprintf(stderr, "error: unknown argument '%s'\n", arg);
					argument_errors = true;
				}
			} else {
				fprintf(stderr, "error: unknown argument '%s'\n", arg);
				argument_errors = true;
			}
		} else {
			filetype_t  type     = forced_filetype;
			const char *filename = arg;
			if (type == FILETYPE_AUTODETECT) {
				size_t      len      = strlen(arg);
				if (len < 2 && arg[0] == '-') {
					/* - implicitely means C source file */
					type     = FILETYPE_C;
					filename = NULL;
				} else if (len > 2 && arg[len-2] == '.') {
					switch(arg[len-1]) {
					case 'c': type = FILETYPE_C; break;
					case 'h': type = FILETYPE_C; break;
					case 's': type = FILETYPE_PREPROCESSED_ASSEMBLER; break;
					case 'S': type = FILETYPE_ASSEMBLER; break;

					case 'a':
					case 'o': type = FILETYPE_OBJECT; break;
					}
				} else if (len > 3 && arg[len-3] == '.') {
					if(strcmp(arg + len - 2, "so") == 0) {
						type = FILETYPE_OBJECT;
					}
				}

				if (type == FILETYPE_AUTODETECT) {
					fprintf(stderr, "'%s': file format not recognized\n", arg);
					continue;
				}
			}

			file_list_entry_t *entry
				= obstack_alloc(&file_obst, sizeof(entry[0]));
			memset(entry, 0, sizeof(entry[0]));
			entry->name = arg;
			entry->type = type;

			if (last_file != NULL) {
				last_file->next = entry;
			} else {
				files = entry;
			}
			last_file = entry;
		}
	}

	if (files == NULL) {
		fprintf(stderr, "error: no input files specified\n");
		argument_errors = true;
	}

	if (help_displayed) {
		return !argument_errors;
	}
	if (argument_errors) {
		usage(argv[0]);
		return 1;
	}

	/* we do the lowering in ast2firm */
	firm_opt.lower_bitfields = FALSE;

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

	char outnamebuf[4096];
	if (outname == NULL) {
		const char *filename = files->name;

		switch(mode) {
		case BenchmarkParser:
		case PrintAst:
		case PrintFluffy:
		case PrintCaml:
		case LexTest:
		case PreprocessOnly:
			outname = "-";
			break;
		case ParseOnly:
			break;
		case Compile:
			get_output_name(outnamebuf, sizeof(outnamebuf), filename, ".s");
			outname = outnamebuf;
			break;
		case CompileAssemble:
			get_output_name(outnamebuf, sizeof(outnamebuf), filename, ".o");
			outname = outnamebuf;
			break;
		case CompileDump:
			get_output_name(outnamebuf, sizeof(outnamebuf), dumpfunction,
			                ".vcg");
			outname = outnamebuf;
			break;
		case CompileAssembleLink:
#ifdef _WIN32
			outname = "a.exe";
#else
			outname = "a.out";
#endif
			break;
		}
	}

	assert(outname != NULL);

	FILE *out;
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

	file_list_entry_t *file;
	for (file = files; file != NULL; file = file->next) {
		char        asm_tempfile[1024];
		const char *filename = file->name;
		filetype_t  filetype = file->type;

		if (file->type == FILETYPE_OBJECT)
			continue;

		FILE *in = NULL;
		if (mode == LexTest) {
			if (in == NULL)
				in = open_file(filename);
			lextest(in, filename);
			fclose(in);
			exit(EXIT_SUCCESS);
		}

		FILE *preprocessed_in = NULL;
		if (filetype == FILETYPE_C || filetype == FILETYPE_ASSEMBLER) {
			/* no support for input on FILE* yet */
			if (in != NULL)
				panic("internal compiler error: in for preprocessor != NULL");

			preprocessed_in = preprocess(filename);
			if (mode == PreprocessOnly) {
				copy_file(out, preprocessed_in);
				int result = pclose(preprocessed_in);
				fclose(out);
				return result;
			}

			if (filetype == FILETYPE_C) {
				filetype = FILETYPE_PREPROCESSED_C;
			} else if (filetype == FILETYPE_ASSEMBLER) {
				filetype = FILETYPE_PREPROCESSED_ASSEMBLER;
			} else {
				panic("internal compiler error: unknown filetype at preproc");
			}

			in = preprocessed_in;
		}

		FILE *asm_out;
		if(mode == Compile) {
			asm_out = out;
		} else {
			asm_out = make_temp_file(asm_tempfile, sizeof(asm_tempfile),
			                         "ccs");
		}

		if (in == NULL)
			in = open_file(filename);

		/* preprocess and compile */
		if (filetype == FILETYPE_PREPROCESSED_C) {
			translation_unit_t *const unit = do_parsing(in, filename);
			if (in == preprocessed_in) {
				int pp_result = pclose(preprocessed_in);
				if (pp_result != EXIT_SUCCESS) {
					exit(EXIT_FAILURE);
				}
			}

			/* prints the AST even if errors occurred */
			if (mode == PrintAst) {
				type_set_output(out);
				ast_set_output(out);
				print_ast(unit);
			}

			if(error_count > 0) {
				/* parsing failed because of errors */
				fprintf(stderr, "%u error(s), %u warning(s)\n", error_count,
				        warning_count);
				result = EXIT_FAILURE;
				continue;
			} else if(warning_count > 0) {
				fprintf(stderr, "%u warning(s)\n", warning_count);
			}

			if(mode == BenchmarkParser) {
				return result;
			} else if(mode == PrintFluffy) {
				write_fluffy_decls(out, unit);
				continue;
			} else if (mode == PrintCaml) {
				write_caml_decls(out, unit);
				continue;
			}

			translation_unit_to_firm(unit);

			if (mode == ParseOnly) {
				continue;
			}

			if (mode == CompileDump) {
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
					fprintf(stderr, "No graph for function '%s' found\n",
					        dumpfunction);
					exit(1);
				}

				dump_ir_block_graph_file(irg, out);
				fclose(out);
				exit(0);
			}

			gen_firm_finish(asm_out, filename, /*c_mode=*/1,
			                /*firm_const_exists=*/0);
			if (asm_out != out) {
				fclose(asm_out);
			}

		} else if (filetype == FILETYPE_PREPROCESSED_ASSEMBLER) {
			copy_file(asm_out, in);
			if (in == preprocessed_in) {
				int pp_result = pclose(preprocessed_in);
				if (pp_result != EXIT_SUCCESS) {
					return pp_result;
				}
			}
			if(asm_out != out) {
				fclose(asm_out);
			}
		}

		if (mode == Compile)
			continue;

		/* if we're here then we have preprocessed assembly */
		filename = asm_tempfile;
		filetype = FILETYPE_PREPROCESSED_ASSEMBLER;

		/* assemble */
		if (filetype == FILETYPE_PREPROCESSED_ASSEMBLER) {
			char        temp[1024];
			const char *filename_o;
			if(mode == CompileAssemble) {
				fclose(out);
				filename_o = outname;
			} else {
				FILE *tempf = make_temp_file(temp, sizeof(temp), "cco");
				fclose(tempf);
				filename_o = temp;
			}

			assemble(filename_o, filename);

			size_t len = strlen(filename_o) + 1;
			filename = obstack_copy(&file_obst, filename_o, len);
			filetype = FILETYPE_OBJECT;
		}

		/* ok we're done here, process next file */
		file->name = filename;
		file->type = filetype;
	}

	if (result != EXIT_SUCCESS)
		return result;

	/* link program file */
	if(mode == CompileAssembleLink) {
		obstack_1grow(&ldflags_obst, '\0');
		const char *flags = obstack_finish(&ldflags_obst);

		/* construct commandline */
		obstack_printf(&file_obst, "%s", LINKER);
		for (file_list_entry_t *entry = files; entry != NULL;
				entry = entry->next) {
			if (entry->type != FILETYPE_OBJECT)
				continue;

			add_flag(&file_obst, "%s", entry->name);
		}

		add_flag(&file_obst, "-o");
		add_flag(&file_obst, outname);
		obstack_printf(&file_obst, "%s", flags);
		obstack_1grow(&file_obst, '\0');

		char *commandline = obstack_finish(&file_obst);

		if(verbose) {
			puts(commandline);
		}
		int err = system(commandline);
		if(err != EXIT_SUCCESS) {
			fprintf(stderr, "linker reported an error\n");
			exit(1);
		}
	}

	obstack_free(&cppflags_obst, NULL);
	obstack_free(&ldflags_obst, NULL);
	obstack_free(&file_obst, NULL);

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
