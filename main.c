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

static inline bool streq(char const* a, char const* b)
{
	return strcmp(a, b) == 0;
}

static inline bool strstart(char const* str, char const* start)
{
	do {
		if (*start == '\0')
			return true;
	} while (*str++ == *start++);
	return false;
}

static FILE *open_file(const char *filename)
{
	if (streq(filename, "-")) {
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
	if (streq(string, "c") || streq(string, "c-header"))
		return FILETYPE_C;
	if (streq(string, "assembler"))
		return FILETYPE_PREPROCESSED_ASSEMBLER;
	if (streq(string, "assembler-with-cpp"))
		return FILETYPE_ASSEMBLER;
	if (streq(string, "none"))
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
			} else if (streq(option, "M")) {
				mode = PreprocessOnly;
				add_flag(&cppflags_obst, "-M");
			} else if (streq(option, "MMD") ||
			           streq(option, "MD")  ||
			           streq(option, "MM")  ||
			           streq(option, "MP")) {
				add_flag(&cppflags_obst, "-%s", option);
			} else if (streq(option, "MT") ||
			           streq(option, "MQ") ||
			           streq(option, "MF")) {
				const char *opt;
				GET_ARG_AFTER(opt, "-MT");
				add_flag(&cppflags_obst, "-%s", option);
				add_flag(&cppflags_obst, "%s", opt);
			} else if (streq(option, "pipe")) {
				/* here for gcc compatibility */
			} else if (option[0] == 'f') {
				char const *orig_opt;
				GET_ARG_AFTER(orig_opt, "-f");

				if (strstart(orig_opt, "align-loops=") ||
				    strstart(orig_opt, "align-jumps=") ||
				    strstart(orig_opt, "align-functions=")) {
					fprintf(stderr, "ignoring gcc option '-f%s'\n", orig_opt);
				} else {
					char const *opt         = orig_opt;
					bool        truth_value = true;
					if (opt[0] == 'n' && opt[1] == 'o' && opt[2] == '-') {
						truth_value = false;
						opt += 3;
					}

					if (streq(opt, "dollars-in-identifiers")) {
						allow_dollar_in_symbol = truth_value;
					} else if (streq(opt, "short-wchar")) {
						opt_short_wchar_t = truth_value;
					} else if (streq(opt, "syntax-only")) {
						mode = truth_value ? ParseOnly : CompileAssembleLink;
					} else if (streq(opt, "omit-frame-pointer")) {
						set_be_option(truth_value ? "omitfp" : "omitfp=no");
					} else if (streq(opt, "strength-reduce")) {
						firm_option("strength-red");
					} else if (streq(opt, "fast-math")               ||
					           streq(opt, "jump-tables")             ||
					           streq(opt, "unroll-loops")            ||
					           streq(opt, "expensive-optimizations") ||
					           streq(opt, "common")                  ||
					           streq(opt, "PIC")                     ||
					           streq(opt, "align-loops")             ||
					           streq(opt, "align-jumps")             ||
					           streq(opt, "align-functions")) {
						fprintf(stderr, "ignoring gcc option '-f%s'\n", orig_opt);
					} else {
						int res = firm_option(orig_opt);
						if (res == 0) {
							fprintf(stderr, "error: unknown Firm option '-f%s'\n",
							        orig_opt);
							argument_errors = true;
							continue;
						} else if (res == -1) {
							help_displayed = true;
						}
					}
				}
			} else if (option[0] == 'b') {
				const char *opt;
				GET_ARG_AFTER(opt, "-b");
				int res = firm_be_option(opt);
				if (res == 0) {
					fprintf(stderr, "error: unknown Firm backend option '-b %s'\n",
					        opt);
					argument_errors = true;
				} else if (res == -1) {
					help_displayed = true;
				} else if (strstart(opt, "isa=")) {
					strncpy(cpu_arch, opt, sizeof(cpu_arch));
				}
			} else if (option[0] == 'W') {
				if (strstart(option + 1, "l,")) // a gcc-style linker option
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
				if (strstart(opt, "arch=")) {
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
				} else if (strstart(opt, "tune=")) {
					GET_ARG_AFTER(opt, "-mtune=");
					snprintf(arch_opt, sizeof(arch_opt), "%s-opt=%s", cpu_arch, opt);
					int res = firm_be_option(arch_opt);
					if (res == 0)
						argument_errors = true;
				} else if (strstart(opt, "cpu=")) {
					GET_ARG_AFTER(opt, "-mcpu=");
					snprintf(arch_opt, sizeof(arch_opt), "%s-arch=%s", cpu_arch, opt);
					int res = firm_be_option(arch_opt);
					if (res == 0)
						argument_errors = true;
				} else if (strstart(opt, "fpmath=")) {
					GET_ARG_AFTER(opt, "-mfpmath=");
					if (streq(opt, "387"))
						opt = "x87";
					else if (streq(opt, "sse"))
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
				} else if (strstart(opt, "preferred-stack-boundary=")) {
					GET_ARG_AFTER(opt, "-mpreferred-stack-boundary=");
					snprintf(arch_opt, sizeof(arch_opt), "%s-stackalign=%s", cpu_arch, opt);
					int res = firm_be_option(arch_opt);
					if (res == 0)
						argument_errors = true;
				} else if (streq(opt, "omit-leaf-frame-pointer")) {
					set_be_option("omitleaffp=1");
				} else if (streq(opt, "no-omit-leaf-frame-pointer")) {
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
			} else if (streq(option, "pg")) {
				set_be_option("gprof");
				add_flag(&ldflags_obst, "-pg");
			} else if (streq(option, "pedantic") ||
			           streq(option, "ansi")) {
				fprintf(stderr, "warning: ignoring gcc option '%s'\n", arg);
			} else if (streq(option, "shared")) {
				add_flag(&ldflags_obst, "-shared");
			} else if (strstart(option, "std=")) {
				if (streq(&option[4], "c99")) {
					c_mode = _C89|_C99;
				} else if (streq(&option[4], "c89")) {
					c_mode = _C89;
				} else if (streq(&option[4], "gnu99")) {
					c_mode = _C89|_C99|_GNUC;
				} else if (streq(&option[4], "microsoft")) {
					c_mode = _C89|_C99|_MS;
				} else
					fprintf(stderr, "warning: ignoring gcc option '%s'\n", arg);
			} else if (streq(option, "version")) {
				print_cparser_version();
			} else if (option[0] == '-') {
				/* double dash option */
				++option;
				if (streq(option, "gcc")) {
					c_mode |= _GNUC;
				} else if (streq(option, "no-gcc")) {
					c_mode &= ~_GNUC;
				} else if (streq(option, "ms")) {
					c_mode |= _MS;
				} else if (streq(option, "no-ms")) {
					c_mode &= ~_MS;
				} else if (streq(option, "signed-chars")) {
					char_is_signed = true;
				} else if (streq(option, "unsigned-chars")) {
					char_is_signed = false;
				} else if (streq(option, "strict")) {
					strict_mode = true;
				} else if (streq(option, "lextest")) {
					mode = LexTest;
				} else if (streq(option, "benchmark")) {
					mode = BenchmarkParser;
				} else if (streq(option, "print-ast")) {
					mode = PrintAst;
				} else if (streq(option, "print-implicit-cast")) {
					print_implicit_casts = true;
				} else if (streq(option, "print-parenthesis")) {
					print_parenthesis = true;
				} else if (streq(option, "print-fluffy")) {
					mode = PrintFluffy;
				} else if (streq(option, "print-caml")) {
					mode = PrintCaml;
				} else if (streq(option, "version")) {
					print_cparser_version();
					exit(EXIT_SUCCESS);
				} else if (streq(option, "dump-function")) {
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
				size_t const len = strlen(arg);
				if (len < 2 && arg[0] == '-') {
					/* - implicitly means C source file */
					type     = FILETYPE_C;
					filename = NULL;
				} else if (len > 2 && arg[len - 2] == '.') {
					switch (arg[len - 1]) {
					case 'c': type = FILETYPE_C;                      break;
					case 'h': type = FILETYPE_C;                      break;
					case 's': type = FILETYPE_PREPROCESSED_ASSEMBLER; break;
					case 'S': type = FILETYPE_ASSEMBLER;              break;

					case 'a':
					case 'o': type = FILETYPE_OBJECT;                 break;
					}
				} else if (len > 3 && arg[len - 3] == '.') {
					if (streq(arg + len - 2, "so")) {
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
	if (streq(outname, "-")) {
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
