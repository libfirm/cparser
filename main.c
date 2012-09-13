/*
 * This file is part of cparser.
 * Copyright (C) 2007-2009 Matthias Braun <matze@braunis.de>
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
#define unlink(filename)         _unlink(filename)

#else
#include <unistd.h>
#define HAVE_MKSTEMP
#endif

#include <libfirm/firm.h>
#include <libfirm/be.h>

#include "preprocessor.h"
#include "token_t.h"
#include "types.h"
#include "type_hash.h"
#include "parser.h"
#include "type_t.h"
#include "ast2firm.h"
#include "diagnostic.h"
#include "lang_features.h"
#include "driver/firm_opt.h"
#include "driver/firm_timing.h"
#include "driver/firm_machine.h"
#include "adt/error.h"
#include "adt/strutil.h"
#include "adt/array.h"
#include "wrappergen/write_fluffy.h"
#include "wrappergen/write_jna.h"
#include "wrappergen/write_compoundsizes.h"
#include "revision.h"
#include "warning.h"
#include "help.h"
#include "mangle.h"
#include "printer.h"

#ifndef PREPROCESSOR
#ifndef __WIN32__
#define PREPROCESSOR "gcc -E -U__STRICT_ANSI__ -U__BLOCKS__"
#else
#define PREPROCESSOR "cpp -U__STRICT_ANSI__"
#endif
#endif

#ifndef LINKER
#define LINKER    "gcc"
#endif

#ifndef ASSEMBLER
#define ASSEMBLER "gcc -c -xassembler"
#endif

unsigned int        c_mode                    = _C89 | _C99 | _GNUC;
bool                byte_order_big_endian     = false;
bool                strict_mode               = false;
bool                enable_main_collect2_hack = false;
bool                freestanding              = false;
unsigned            architecture_modulo_shift = 0;

static bool               char_is_signed      = true;
static atomic_type_kind_t wchar_atomic_kind   = ATOMIC_TYPE_INT;
static unsigned           features_on         = 0;
static unsigned           features_off        = 0;
static const char        *dumpfunction        = NULL;
static struct obstack     file_obst;
static const char        *external_preprocessor = PREPROCESSOR;

static machine_triple_t *target_machine;
static const char       *target_triple;
static int               verbose;
static struct obstack    cppflags_obst;
static struct obstack    ldflags_obst;
static struct obstack    asflags_obst;
static char              dep_target[1024];
static const char       *outname;
static bool              define_intmax_types;
static const char       *input_encoding;

typedef enum lang_standard_t {
	STANDARD_DEFAULT, /* gnu99 (for C, GCC does gnu89) or gnu++98 (for C++) */
	STANDARD_ANSI,    /* ISO C90 (for C) or ISO C++ 1998 (for C++) */
	STANDARD_C89,     /* ISO C90 (sic) */
	STANDARD_C89AMD1, /* ISO C90 as modified in amendment 1 */
	STANDARD_C99,     /* ISO C99 */
	STANDARD_C11,     /* ISO C11 */
	STANDARD_GNU89,   /* ISO C90 plus GNU extensions (including some C99) */
	STANDARD_GNU99,   /* ISO C99 plus GNU extensions */
	STANDARD_GNU11,   /* ISO C11 plus GNU extensions */
	STANDARD_CXX98,   /* ISO C++ 1998 plus amendments */
	STANDARD_GNUXX98  /* ISO C++ 1998 plus amendments and GNU extensions */
} lang_standard_t;

typedef enum compilation_unit_type_t {
	COMPILATION_UNIT_AUTODETECT,
	COMPILATION_UNIT_C,
	COMPILATION_UNIT_PREPROCESSED_C,
	COMPILATION_UNIT_CXX,
	COMPILATION_UNIT_PREPROCESSED_CXX,
	COMPILATION_UNIT_AST,
	COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION,
	COMPILATION_UNIT_ASSEMBLER,
	COMPILATION_UNIT_PREPROCESSED_ASSEMBLER,
	COMPILATION_UNIT_OBJECT,
	COMPILATION_UNIT_IR,
	COMPILATION_UNIT_UNKNOWN
} compilation_unit_type_t;

typedef struct compilation_unit_t compilation_unit_t;
struct compilation_unit_t {
	const char             *name;  /**< filename or "-" for stdin */
	FILE                   *input; /**< input (NULL if not opened yet) */
	bool                    input_is_pipe;
	compilation_unit_type_t type;
	lang_standard_t         standard;
	translation_unit_t     *ast;
	bool                    parse_errors;
	compilation_unit_t     *next;
};

static char **temp_files;

static void get_output_name(char *buf, size_t buflen, const char *inputname,
                            const char *newext)
{
	if (inputname == NULL)
		inputname = "a";

	char const *const last_slash = strrchr(inputname, '/');
	char const *const filename   =
		last_slash != NULL ? last_slash + 1 : inputname;
	char const *const last_dot   = strrchr(filename, '.');
	char const *const name_end   =
		last_dot != NULL ? last_dot : strchr(filename, '\0');

	int const len = snprintf(buf, buflen, "%.*s%s",
			(int)(name_end - filename), filename, newext);
#ifdef _WIN32
	if (len < 0 || buflen <= (size_t)len)
#else
	if (buflen <= (size_t)len)
#endif
		panic("filename too long");
}

static bool close_input(compilation_unit_t *unit)
{
	assert(unit->input);
	bool res;
	if (unit->input == stdin) {
		res = true;
	} else if (unit->input_is_pipe) {
		res = pclose(unit->input) == EXIT_SUCCESS;
	} else {
		fclose(unit->input);
		res = true;
	}
	unit->input = NULL;
	unit->name  = NULL;
	return res;
}

static void print_error_summary(void)
{
	if (error_count > 0) {
		/* parsing failed because of errors */
		fprintf(stderr, "%u error(s), %u warning(s)\n", error_count,
				warning_count);
	} else if (warning_count > 0) {
		fprintf(stderr, "%u warning(s)\n", warning_count);
	}
}

static void do_parsing(compilation_unit_t *unit)
{
	ir_timer_t *t_parsing = ir_timer_new();
	timer_register(t_parsing, "Frontend: Parsing");
	timer_push(t_parsing);

	start_parsing();

	switch_pp_input(unit->input, unit->name, NULL, false);
	parse();
	unit->ast = finish_parsing();
	check_unclosed_conditionals();
	close_pp_input();
	bool res = close_input(unit);

	print_error_summary();

	unit->type         = COMPILATION_UNIT_AST;
	unit->parse_errors = error_count > 0 || !res;
	timer_pop(t_parsing);
}

static void add_flag(struct obstack *obst, const char *format, ...)
{
	char buf[65536];
	va_list ap;

	va_start(ap, format);
#ifdef _WIN32
	int len =
#endif
		vsnprintf(buf, sizeof(buf), format, ap);
	va_end(ap);

	obstack_1grow(obst, ' ');
#ifdef _WIN32
	obstack_1grow(obst, '"');
	obstack_grow(obst, buf, len);
	obstack_1grow(obst, '"');
#else
	/* escape stuff... */
	for (char *c = buf; *c != '\0'; ++c) {
		switch(*c) {
		case ' ':
		case '"':
		case '$':
		case '&':
		case '(':
		case ')':
		case ';':
		case '<':
		case '>':
		case '\'':
		case '\\':
		case '\n':
		case '\r':
		case '\t':
		case '`':
		case '|':
			obstack_1grow(obst, '\\');
			/* FALLTHROUGH */
		default:
			obstack_1grow(obst, *c);
			break;
		}
	}
#endif
}

static const char *type_to_string(type_t *type)
{
	assert(type->kind == TYPE_ATOMIC);
	return get_atomic_kind_name(type->atomic.akind);
}

static char const* str_lang_standard(lang_standard_t const standard)
{
	switch (standard) {
	case STANDARD_C89:     return "c89";
	case STANDARD_C89AMD1: return "iso9899:199409";
	case STANDARD_C99:     return "c99";
	case STANDARD_C11:     return "c11";
	case STANDARD_GNU89:   return "gnu89";
	case STANDARD_GNU99:   return "gnu99";
	case STANDARD_GNU11:   return "gnu11";
	case STANDARD_CXX98:   return "c++98";
	case STANDARD_GNUXX98: return "gnu++98";
	case STANDARD_ANSI:    break;
	case STANDARD_DEFAULT: break;
	}
	panic("invalid standard");
}

static bool run_external_preprocessor(compilation_unit_t *unit)
{
	static const char *common_flags = NULL;

	if (common_flags == NULL) {
		obstack_1grow(&cppflags_obst, '\0');
		const char *flags = obstack_finish(&cppflags_obst);

		/* setup default defines */
		add_flag(&cppflags_obst, "-U__WCHAR_TYPE__");
		add_flag(&cppflags_obst, "-D__WCHAR_TYPE__=%s", type_to_string(type_wchar_t));
		add_flag(&cppflags_obst, "-U__SIZE_TYPE__");
		add_flag(&cppflags_obst, "-D__SIZE_TYPE__=%s", type_to_string(type_size_t));

		add_flag(&cppflags_obst, "-U__VERSION__");
		add_flag(&cppflags_obst, "-D__VERSION__=\"%s\"", cparser_REVISION);

		if (define_intmax_types) {
			add_flag(&cppflags_obst, "-U__INTMAX_TYPE__");
			add_flag(&cppflags_obst, "-D__INTMAX_TYPE__=%s", type_to_string(type_intmax_t));
			add_flag(&cppflags_obst, "-U__UINTMAX_TYPE__");
			add_flag(&cppflags_obst, "-D__UINTMAX_TYPE__=%s", type_to_string(type_uintmax_t));
		}

		if (flags[0] != '\0') {
			size_t len = strlen(flags);
			obstack_1grow(&cppflags_obst, ' ');
			obstack_grow(&cppflags_obst, flags, len);
		}
		obstack_1grow(&cppflags_obst, '\0');
		common_flags = obstack_finish(&cppflags_obst);
	}

	assert(obstack_object_size(&cppflags_obst) == 0);

	const char *preprocessor = getenv("CPARSER_PP");
	if (preprocessor != NULL) {
		obstack_printf(&cppflags_obst, "%s ", preprocessor);
	} else {
		if (target_triple != NULL)
			obstack_printf(&cppflags_obst, "%s-", target_triple);
		obstack_printf(&cppflags_obst, "%s", external_preprocessor);
	}

	char const *lang;
	switch (unit->type) {
	case COMPILATION_UNIT_C:         lang = "c";                  break;
	case COMPILATION_UNIT_CXX:       lang = "c++";                break;
	case COMPILATION_UNIT_ASSEMBLER: lang = "assembler-with-cpp"; break;
	default:                         lang = NULL;                 break;
	}
	if (lang)
		add_flag(&cppflags_obst, "-x%s", lang);

	add_flag(&cppflags_obst, "-std=%s", str_lang_standard(unit->standard));

	obstack_printf(&cppflags_obst, "%s", common_flags);

	/* handle dependency generation */
	if (dep_target[0] != '\0') {
		add_flag(&cppflags_obst, "-MF");
		add_flag(&cppflags_obst, dep_target);
		if (outname != NULL) {
			add_flag(&cppflags_obst, "-MQ");
			add_flag(&cppflags_obst, outname);
		}
	}
	assert(unit->input == NULL);
	add_flag(&cppflags_obst, unit->name);
	obstack_1grow(&cppflags_obst, '\0');

	char *commandline = obstack_finish(&cppflags_obst);
	if (verbose) {
		puts(commandline);
	}
	FILE *f = popen(commandline, "r");
	if (f == NULL) {
		fprintf(stderr, "invoking preprocessor failed\n");
		return false;
	}
	/* we do not really need that anymore */
	obstack_free(&cppflags_obst, commandline);

	unit->input         = f;
	unit->input_is_pipe = true;
	switch (unit->type) {
	case COMPILATION_UNIT_ASSEMBLER:
		unit->type = COMPILATION_UNIT_PREPROCESSED_ASSEMBLER;
		break;
	case COMPILATION_UNIT_C:
		unit->type = COMPILATION_UNIT_PREPROCESSED_C;
		break;
	case COMPILATION_UNIT_CXX:
		unit->type = COMPILATION_UNIT_PREPROCESSED_CXX;
		break;
	default:
		unit->type = COMPILATION_UNIT_UNKNOWN;
		break;
	}

	return true;
}

static void assemble(const char *out, const char *in)
{
	obstack_1grow(&asflags_obst, '\0');
	const char *flags = obstack_finish(&asflags_obst);

	const char *assembler = getenv("CPARSER_AS");
	if (assembler != NULL) {
		obstack_printf(&asflags_obst, "%s", assembler);
	} else {
		if (target_triple != NULL)
			obstack_printf(&asflags_obst, "%s-", target_triple);
		obstack_printf(&asflags_obst, "%s", ASSEMBLER);
	}
	if (flags[0] != '\0')
		obstack_printf(&asflags_obst, " %s", flags);

	obstack_printf(&asflags_obst, " %s -o %s", in, out);
	obstack_1grow(&asflags_obst, '\0');

	char *commandline = obstack_finish(&asflags_obst);
	if (verbose) {
		puts(commandline);
	}
	int err = system(commandline);
	if (err != EXIT_SUCCESS) {
		fprintf(stderr, "assembler reported an error\n");
		exit(EXIT_FAILURE);
	}
	obstack_free(&asflags_obst, commandline);
}

static void print_file_name(const char *file)
{
	add_flag(&ldflags_obst, "-print-file-name=%s", file);

	obstack_1grow(&ldflags_obst, '\0');
	const char *flags = obstack_finish(&ldflags_obst);

	/* construct commandline */
	const char *linker = getenv("CPARSER_LINK");
	if (linker != NULL) {
		obstack_printf(&ldflags_obst, "%s ", linker);
	} else {
		if (target_triple != NULL)
			obstack_printf(&ldflags_obst, "%s-", target_triple);
		obstack_printf(&ldflags_obst, "%s ", LINKER);
	}
	obstack_printf(&ldflags_obst, "%s ", linker);
	obstack_printf(&ldflags_obst, "%s", flags);
	obstack_1grow(&ldflags_obst, '\0');

	char *commandline = obstack_finish(&ldflags_obst);
	if (verbose) {
		puts(commandline);
	}
	int err = system(commandline);
	if (err != EXIT_SUCCESS) {
		fprintf(stderr, "linker reported an error\n");
		exit(EXIT_FAILURE);
	}
	obstack_free(&ldflags_obst, commandline);
}

static const char *try_dir(const char *dir)
{
	if (dir == NULL)
		return dir;
	if (access(dir, R_OK | W_OK | X_OK) == 0)
		return dir;
	return NULL;
}

static const char *get_tempdir(void)
{
	static const char *tmpdir = NULL;

	if (tmpdir != NULL)
		return tmpdir;

	if (tmpdir == NULL)
		tmpdir = try_dir(getenv("TMPDIR"));
	if (tmpdir == NULL)
		tmpdir = try_dir(getenv("TMP"));
	if (tmpdir == NULL)
		tmpdir = try_dir(getenv("TEMP"));

#ifdef P_tmpdir
	if (tmpdir == NULL)
		tmpdir = try_dir(P_tmpdir);
#endif

	if (tmpdir == NULL)
		tmpdir = try_dir("/var/tmp");
	if (tmpdir == NULL)
		tmpdir = try_dir("/usr/tmp");
	if (tmpdir == NULL)
		tmpdir = try_dir("/tmp");

	if (tmpdir == NULL)
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
 * custom version of tmpnam, which: writes to an obstack, emits no warnings
 * during linking (like glibc/gnu ld do for tmpnam)...
 */
static FILE *make_temp_file(const char *prefix, const char **name_result)
{
	const char *tempdir = get_tempdir();
	assert(obstack_object_size(&file_obst) == 0);
	obstack_printf(&file_obst, "%s/%sXXXXXX", tempdir, prefix);
	obstack_1grow(&file_obst, '\0');

	char *name = obstack_finish(&file_obst);
	int fd = mkstemp(name);
	if (fd == -1) {
		fprintf(stderr, "could not create temporary file: %s\n",
		        strerror(errno));
		return NULL;
	}
	FILE *out = fdopen(fd, "w");
	if (out == NULL) {
		fprintf(stderr, "could not open temporary file as FILE*\n");
		return NULL;
	}

	ARR_APP1(char*, temp_files, name);
	*name_result = name;
	return out;
}

static void free_temp_files(void)
{
	if (temp_files == NULL)
		return;

	size_t n_temp_files = ARR_LEN(temp_files);
	size_t i;
	for (i = 0; i < n_temp_files; ++i) {
		char *file = temp_files[i];
		unlink(file);
	}
	DEL_ARR_F(temp_files);
	temp_files = NULL;
}

typedef enum compile_mode_t {
	BenchmarkParser,
	PreprocessOnly,
	ParseOnly,
	Compile,
	CompileDump,
	CompileExportIR,
	CompileAssemble,
	CompileAssembleLink,
	PrintAst,
	PrintFluffy,
	PrintJna,
	PrintCompoundSizes,
} compile_mode_t;

static void usage(const char *argv0)
{
	fprintf(stderr, "Usage %s [options] input [-o output]\n", argv0);
}

static void print_cparser_version(void)
{
	printf("cparser (%s) using libFirm (%u.%u",
	       cparser_REVISION, ir_get_version_major(),
	       ir_get_version_minor());

	const char *revision = ir_get_version_revision();
	if (revision[0] != 0) {
		putchar('-');
		fputs(revision, stdout);
	}

	const char *build = ir_get_version_build();
	if (build[0] != 0) {
		putchar(' ');
		fputs(build, stdout);
	}
	puts(")");
	puts("This is free software; see the source for copying conditions.  There is NO\n"
	     "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n");
}

static void print_cparser_version_short(void)
{
	puts(cparser_REVISION);
}

static void print_help_basic(const char *argv0)
{
	usage(argv0);
	puts("");
	put_help("--help",                   "Display this information");
	put_help("--version",                "Display compiler version");
	put_help("--help-parser",            "Display information about parser options");
	put_help("--help-warnings",          "Display information about warning options");
	put_help("--help-codegen",           "Display information about code-generation options");
	put_help("--help-optimization",      "Display information about optimization options");
	put_help("--help-linker",            "Display information about linker options");
	put_help("--help-language-tools",    "Display information about language tools options");
	put_help("--help-debug",             "Display information about compiler debugging options");
	put_help("--help-firm",              "Display information about direct firm options");
	put_help("--help-all",               "Display information about all options");
	put_help("-c",                       "Compile and assemble but do not link");
	put_help("-E",                       "Preprocess only");
	put_help("-S",                       "Compile but do not assembler or link");
	put_help("-o",                       "Specify output file");
	put_help("-v",                       "Verbose output (show invocation of sub-processes)");
	put_help("-x",                       "Force input language:");
	put_choice("c",                      "C");
	put_choice("c++",                    "C++");
	put_choice("assembler",              "Assembler (no preprocessing)");
	put_choice("assembler-with-cpp",     "Assembler with preprocessing");
	put_choice("none",                   "Autodetection");
	put_help("-pipe",                    "Ignored (gcc compatibility)");
}

static void print_help_preprocessor(void)
{
	put_help("-nostdinc",                "Do not search standard system include directories");
	put_help("-trigraphs",               "Support ISO C trigraphs");
	put_help("-isystem",                 "");
	put_help("-include",                 "");
	put_help("-I PATH",                  "");
	put_help("-D SYMBOL[=value]",        "");
	put_help("-U SYMBOL",                "");
	put_help("-Wp,OPTION",               "Pass option directly to preprocessor");
	put_help("-M",                       "");
	put_help("-MD",                      "");
	put_help("-MMD",                     "");
	put_help("-MM",                      "");
	put_help("-MP",                      "");
	put_help("-MT",                      "");
	put_help("-MQ",                      "");
	put_help("-MF",                      "");
}

static void print_help_parser(void)
{
	put_help("-finput-charset=CHARSET",  "Select encoding of input files");
	put_help("-fmessage-length=LEN",     "Ignored (gcc compatibility)");
	put_help("-fshort-wchar",            "Type \"wchar_t\" is unsigned short instead of int");
	put_help("-fshow-column",            "Show the column number in diagnostic messages");
	put_help("-fsigned-char",            "Type \"char\" is a signed type");
	put_help("-funsigned-char",          "Type \"char\" is an unsigned type");
	put_help("--ms",                     "Enable msvc extensions");
	put_help("--no-ms",                  "Disable msvc extensions");
	put_help("--gcc",                    "Enable gcc extensions");
	put_help("--no-gcc",                 "Disable gcc extensions");
	put_help("-std=STANDARD",            "Specify language standard:");
	put_choice("c99",                    "ISO C99 standard");
	put_choice("c89",                    "ISO C89 standard");
	put_choice("c90",                    "Same as -std=c89");
	put_choice("c11",                    "ISO C11 standard");
	put_choice("c9x",                    "Deprecated");
	put_choice("c++",                    "ISO C++ 98");
	put_choice("c++98",                  "ISO C++ 98");
	put_choice("gnu99",                  "ISO C99 + GNU extensions (default)");
	put_choice("gnu89",                  "ISO C89 + GNU extensions");
	put_choice("gnu11",                  "ISO C11 + GNU extensions");
	put_choice("gnu9x",                  "Deprecated");
	put_choice("iso9899:1990",           "ISO C89");
	put_choice("iso9899:199409",         "ISO C90");
	put_choice("iso9899:1999",           "ISO C99");
	put_choice("iso9899:199x",           "Deprecated");
	put_help("-pedantic",                "Ignored (gcc compatibility)");
	put_help("-ansi",                    "-std=c90 (for C) or -std=c++98 (for C++)");
	put_help("--strict",                 "Enable strict conformance checking");
}

static void print_help_warnings(void)
{
	put_help("-f[no-]diagnostics-show-option", "Show the switch, which controls a warning, after each warning");
	put_help("-w",                             "Disable all warnings");
	put_help("-Wno-trigraphs",                 "Warn if input contains trigraphs");
	put_help("-Wundef",                        "Warn if an undefined macro is used in an #if");
	put_help("-Wmissing-include-dirs",         "Warn about missing user-specified include directories");
	put_help("-Wendif-labels",                 "Warn about stray text after #elif and #endif");
	put_help("-Winit-self",                    "Ignored (gcc compatibility)");
	put_help("-Wformat-y2k",                   "Ignored (gcc compatibility)");
	put_help("-Wformat-security",              "Ignored (gcc compatibility)");
	put_help("-Wold-style-declaration",        "Ignored (gcc compatibility)");
	put_help("-Wtype-limits",                  "Ignored (gcc compatibility)");
	print_warning_opt_help();
}

static void print_help_optimization(void)
{
	put_help("-O LEVEL",                 "Select optimization level (0-4)");
	firm_option_help(put_help);
	put_help("-fexpensive-optimizations","Ignored (gcc compatibility)");
}

static void print_help_codegeneration(void)
{
	put_help("-g",                       "Generate debug information");
	put_help("-pg",                      "Instrument code for gnu gprof");
	put_help("-fomit-frame-pointer",     "Produce code without frame pointer where possible");
	put_help("-ffreestanding",           "Compile in freestanding mode (see ISO C standard)");
	put_help("-fhosted",                 "Compile in hosted (not freestanding) mode");
	put_help("-fprofile-generate",       "Generate instrumented code to collect profile information");
	put_help("-fprofile-use",            "Use profile information generated by instrumented binaries");
	put_help("-ffp-precise",             "Precise floating point model");
	put_help("-ffp-fast",                "Imprecise floating point model");
	put_help("-ffp-strict",              "Strict floating point model");
	put_help("-pthread",                 "Use pthread threading library");
	put_help("-mtarget=TARGET",          "Specify target architecture as CPU-manufacturer-OS triple");
	put_help("-mtriple=TARGET",          "Alias for -mtarget (clang compatibility)");
	put_help("-march=ARCH",              "");
	put_help("-mtune=ARCH",              "");
	put_help("-mcpu=CPU",                "");
	put_help("-mfpmath=",                "");
	put_help("-mpreferred-stack-boundary=", "");
	put_help("-mrtd",                    "");
	put_help("-mregparm=",               "Not supported yet");
	put_help("-msoft-float",             "Not supported yet");
	put_help("-m32",                     "Generate 32bit code");
	put_help("-m64",                     "Generate 64bit code");
	put_help("-fverbose-asm",            "Ignored (gcc compatibility)");
	put_help("-fjump-tables",            "Ignored (gcc compatibility)");
	put_help("-fcommon",                 "Ignored (gcc compatibility)");
	put_help("-foptimize-sibling-calls", "Ignored (gcc compatibility)");
	put_help("-falign-loops",            "Ignored (gcc compatibility)");
	put_help("-falign-jumps",            "Ignored (gcc compatibility)");
	put_help("-falign-functions",        "Ignored (gcc compatibility)");
	put_help("-fPIC",                    "Ignored (gcc compatibility)");
	put_help("-ffast-math",              "Same as -ffp-fast (gcc compatibility)");
	puts("");
	puts("\tMost of these options can be used with a no- prefix to disable them");
	puts("\te.g. -fno-omit-frame-pointer");
}

static void print_help_linker(void)
{
	put_help("-l LIBRARY",               "");
	put_help("-L PATH",                  "");
	put_help("-s",                       "Do not produce symbol table and relocation information");
	put_help("-shared",                  "Produce a shared library");
	put_help("-static",                  "Produce statically linked binary");
	put_help("-Wl,OPTION",               "Pass option directly to linker");
}

static void print_help_debug(void)
{
	put_help("--print-ast",              "Preprocess, parse and print AST");
	put_help("--print-implicit-cast",    "");
	put_help("--print-parenthesis",      "");
	put_help("--benchmark",              "Preprocess and parse, produces no output");
	put_help("--time",                   "Measure time of compiler passes");
	put_help("--dump-function func",     "Preprocess, parse and output vcg graph of func");
	put_help("--export-ir",              "Preprocess, parse and output compiler intermediate representation");
}

static void print_help_language_tools(void)
{
	put_help("--print-fluffy",           "Preprocess, parse and generate declarations for the fluffy language");
	put_help("--print-jna",              "Preprocess, parse and generate declarations for JNA");
	put_help("--jna-limit filename",     "");
	put_help("--jna-libname name",       "");
}

static void print_help_firm(void)
{
	put_help("-bOPTION",                 "Directly pass option to libFirm backend");
	int res = be_parse_arg("help");
	(void) res;
	assert(res);
}

typedef enum {
	HELP_NONE          = 0,
	HELP_BASIC         = 1 << 0,
	HELP_PREPROCESSOR  = 1 << 1,
	HELP_PARSER        = 1 << 2,
	HELP_WARNINGS      = 1 << 3,
	HELP_OPTIMIZATION  = 1 << 4,
	HELP_CODEGEN       = 1 << 5,
	HELP_LINKER        = 1 << 6,
	HELP_LANGUAGETOOLS = 1 << 7,
	HELP_DEBUG         = 1 << 8,
	HELP_FIRM          = 1 << 9,

	HELP_ALL           = -1
} help_sections_t;

static void print_help(const char *argv0, help_sections_t sections)
{
	if (sections & HELP_BASIC)         print_help_basic(argv0);
	if (sections & HELP_PREPROCESSOR)  print_help_preprocessor();
	if (sections & HELP_PARSER)        print_help_parser();
	if (sections & HELP_WARNINGS)      print_help_warnings();
	if (sections & HELP_OPTIMIZATION)  print_help_optimization();
	if (sections & HELP_CODEGEN)       print_help_codegeneration();
	if (sections & HELP_LINKER)        print_help_linker();
	if (sections & HELP_LANGUAGETOOLS) print_help_language_tools();
	if (sections & HELP_DEBUG)         print_help_debug();
	if (sections & HELP_FIRM)          print_help_firm();
}

static void set_be_option(const char *arg)
{
	int res = be_parse_arg(arg);
	(void) res;
	assert(res);
}

static compilation_unit_type_t get_unit_type_from_string(const char *string)
{
	if (streq(string, "c") || streq(string, "c-header"))
		return COMPILATION_UNIT_C;
	if (streq(string, "c++") || streq(string, "c++-header"))
		return COMPILATION_UNIT_CXX;
	if (streq(string, "assembler"))
		return COMPILATION_UNIT_PREPROCESSED_ASSEMBLER;
	if (streq(string, "assembler-with-cpp"))
		return COMPILATION_UNIT_ASSEMBLER;
	if (streq(string, "none"))
		return COMPILATION_UNIT_AUTODETECT;

	return COMPILATION_UNIT_UNKNOWN;
}

static bool init_os_support(void)
{
	wchar_atomic_kind         = ATOMIC_TYPE_INT;
	enable_main_collect2_hack = false;
	define_intmax_types       = false;

	if (firm_is_unixish_os(target_machine)) {
		set_create_ld_ident(create_name_linux_elf);
	} else if (firm_is_darwin_os(target_machine)) {
		set_create_ld_ident(create_name_macho);
		define_intmax_types = true;
	} else if (firm_is_windows_os(target_machine)) {
		wchar_atomic_kind         = ATOMIC_TYPE_USHORT;
		enable_main_collect2_hack = true;
		set_create_ld_ident(create_name_win32);
	} else {
		return false;
	}

	return true;
}

static bool parse_target_triple(const char *arg)
{
	machine_triple_t *triple = firm_parse_machine_triple(arg);
	if (triple == NULL) {
		fprintf(stderr, "Target-triple is not in the form 'cpu_type-manufacturer-operating_system'\n");
		return false;
	}
	target_machine = triple;
	return true;
}

static unsigned decide_modulo_shift(unsigned type_size)
{
	if (architecture_modulo_shift == 0)
		return 0;
	if (type_size < architecture_modulo_shift)
		return architecture_modulo_shift;
	return type_size;
}

static bool is_ia32_cpu(const char *architecture)
{
	return streq(architecture, "i386")
	    || streq(architecture, "i486")
	    || streq(architecture, "i586")
	    || streq(architecture, "i686")
	    || streq(architecture, "i786");
}

static const char *setup_isa_from_tripel(const machine_triple_t *machine)
{
	const char *cpu = machine->cpu_type;

	if (is_ia32_cpu(cpu)) {
		return "ia32";
	} else if (streq(cpu, "x86_64")) {
		return "amd64";
	} else if (streq(cpu, "sparc")) {
		return "sparc";
	} else if (streq(cpu, "arm")) {
		return "arm";
	} else {
		fprintf(stderr, "Unknown cpu '%s' in target-triple\n", cpu);
		return NULL;
	}
}

static const char *setup_target_machine(void)
{
	if (!setup_firm_for_machine(target_machine))
		exit(1);

	const char *isa = setup_isa_from_tripel(target_machine);

	if (isa == NULL)
		exit(1);

	init_os_support();

	return isa;
}

/**
 * initialize cparser type properties based on a firm type
 */
static void set_typeprops_type(atomic_type_properties_t* props, ir_type *type)
{
	props->size             = get_type_size_bytes(type);
	props->alignment        = get_type_alignment_bytes(type);
	props->struct_alignment = props->alignment;
}

/**
 * Copy atomic type properties except the integer conversion rank
 */
static void copy_typeprops(atomic_type_properties_t *dest,
                           const atomic_type_properties_t *src)
{
	dest->size             = src->size;
	dest->alignment        = src->alignment;
	dest->struct_alignment = src->struct_alignment;
	dest->flags            = src->flags;
}

static void init_types_and_adjust(void)
{
	const backend_params *be_params = be_get_backend_param();
	unsigned machine_size = be_params->machine_size;
	init_types(machine_size);

	atomic_type_properties_t *props = atomic_type_properties;

	/* adjust types as requested by target architecture */
	ir_type *type_long_double = be_params->type_long_double;
	if (type_long_double != NULL) {
		set_typeprops_type(&props[ATOMIC_TYPE_LONG_DOUBLE], type_long_double);
		atomic_modes[ATOMIC_TYPE_LONG_DOUBLE] = get_type_mode(type_long_double);
	}

	ir_type *type_long_long = be_params->type_long_long;
	if (type_long_long != NULL)
		set_typeprops_type(&props[ATOMIC_TYPE_LONGLONG], type_long_long);

	ir_type *type_unsigned_long_long = be_params->type_unsigned_long_long;
	if (type_unsigned_long_long != NULL)
		set_typeprops_type(&props[ATOMIC_TYPE_ULONGLONG], type_unsigned_long_long);

	/* operating system ABI specifics */
	if (firm_is_darwin_os(target_machine)) {
		if (machine_size == 32) {
			props[ATOMIC_TYPE_LONGLONG].struct_alignment    =  4;
			props[ATOMIC_TYPE_ULONGLONG].struct_alignment   =  4;
			props[ATOMIC_TYPE_DOUBLE].struct_alignment      =  4;
			props[ATOMIC_TYPE_LONG_DOUBLE].size             = 16;
			props[ATOMIC_TYPE_LONG_DOUBLE].alignment        = 16;
			props[ATOMIC_TYPE_LONG_DOUBLE].struct_alignment = 16;
		}
	} else if (firm_is_windows_os(target_machine)) {
		if (machine_size == 64) {
			/* to ease porting of old c-code microsoft decided to use 32bits
			 * even for long */
			props[ATOMIC_TYPE_LONG]  = props[ATOMIC_TYPE_INT];
			props[ATOMIC_TYPE_ULONG] = props[ATOMIC_TYPE_UINT];
		}

		/* on windows long double is not supported */
		props[ATOMIC_TYPE_LONG_DOUBLE] = props[ATOMIC_TYPE_DOUBLE];
	} else if (firm_is_unixish_os(target_machine)) {
		if (is_ia32_cpu(target_machine->cpu_type)) {
			/* System V has a broken alignment for double so we have to add
			 * a hack here */
			props[ATOMIC_TYPE_DOUBLE].struct_alignment    = 4;
			props[ATOMIC_TYPE_LONGLONG].struct_alignment  = 4;
			props[ATOMIC_TYPE_ULONGLONG].struct_alignment = 4;
		}
	}

	/* stuff decided after processing operating system specifics and
	 * commandline flags */
	if (char_is_signed) {
		props[ATOMIC_TYPE_CHAR].flags |= ATOMIC_TYPE_FLAG_SIGNED;
	} else {
		props[ATOMIC_TYPE_CHAR].flags &= ~ATOMIC_TYPE_FLAG_SIGNED;
	}
	/* copy over wchar_t properties (including rank) */
	props[ATOMIC_TYPE_WCHAR_T] = props[wchar_atomic_kind];

	/* initialize defaults for unsupported types */
	if (type_long_long == NULL) {
		copy_typeprops(&props[ATOMIC_TYPE_LONGLONG], &props[ATOMIC_TYPE_LONG]);
	}
	if (type_unsigned_long_long == NULL) {
		copy_typeprops(&props[ATOMIC_TYPE_ULONGLONG],
		               &props[ATOMIC_TYPE_ULONG]);
	}
	if (type_long_double == NULL) {
		copy_typeprops(&props[ATOMIC_TYPE_LONG_DOUBLE],
		               &props[ATOMIC_TYPE_DOUBLE]);
	}

	/* initialize firm pointer modes */
	char               name[64];
	unsigned           bit_size     = machine_size;
	unsigned           modulo_shift = decide_modulo_shift(bit_size);

	snprintf(name, sizeof(name), "p%u", machine_size);
	ir_mode *ptr_mode = new_reference_mode(name, irma_twos_complement, bit_size, modulo_shift);

	if (machine_size == 16) {
		set_reference_mode_signed_eq(ptr_mode, mode_Hs);
		set_reference_mode_unsigned_eq(ptr_mode, mode_Hu);
	} else if (machine_size == 32) {
		set_reference_mode_signed_eq(ptr_mode, mode_Is);
		set_reference_mode_unsigned_eq(ptr_mode, mode_Iu);
	} else if (machine_size == 64) {
		set_reference_mode_signed_eq(ptr_mode, mode_Ls);
		set_reference_mode_unsigned_eq(ptr_mode, mode_Lu);
	} else {
		panic("strange machine_size when determining pointer modes");
	}

	/* Hmm, pointers should be machine size */
	set_modeP_data(ptr_mode);
	set_modeP_code(ptr_mode);

	byte_order_big_endian = be_params->byte_order_big_endian;
	if (be_params->modulo_shift_efficient) {
		architecture_modulo_shift = machine_size;
	} else {
		architecture_modulo_shift = 0;
	}
}

static void setup_cmode(const compilation_unit_t *unit)
{
	compilation_unit_type_t type     = unit->type;
	lang_standard_t         standard = unit->standard;
	if (type == COMPILATION_UNIT_PREPROCESSED_C || type == COMPILATION_UNIT_C) {
		switch (standard) {
		case STANDARD_C89:     c_mode = _C89;                       break;
							   /* TODO determine difference between these two */
		case STANDARD_C89AMD1: c_mode = _C89;                       break;
		case STANDARD_C99:     c_mode = _C89 | _C99;                break;
		case STANDARD_C11:     c_mode = _C89 | _C99 | _C11;         break;
		case STANDARD_GNU89:   c_mode = _C89 |               _GNUC; break;
		case STANDARD_GNU11:   c_mode = _C89 | _C99 | _C11 | _GNUC; break;

		case STANDARD_ANSI:
		case STANDARD_CXX98:
		case STANDARD_GNUXX98:
		case STANDARD_DEFAULT:
			fprintf(stderr, "warning: command line option \"-std=%s\" is not valid for C\n", str_lang_standard(standard));
			/* FALLTHROUGH */
		case STANDARD_GNU99:   c_mode = _C89 | _C99 | _GNUC; break;
		}
	} else if (type == COMPILATION_UNIT_PREPROCESSED_CXX
	           || type == COMPILATION_UNIT_CXX) {
		switch (standard) {
		case STANDARD_CXX98: c_mode = _CXX; break;

		case STANDARD_ANSI:
		case STANDARD_C89:
		case STANDARD_C89AMD1:
		case STANDARD_C99:
		case STANDARD_C11:
		case STANDARD_GNU89:
		case STANDARD_GNU99:
		case STANDARD_GNU11:
		case STANDARD_DEFAULT:
			fprintf(stderr, "warning: command line option \"-std=%s\" is not valid for C++\n", str_lang_standard(standard));
			/* FALLTHROUGH */
		case STANDARD_GNUXX98: c_mode = _CXX | _GNUC; break;
		}
	}

	c_mode |= features_on;
	c_mode &= ~features_off;
}

static void determine_unit_standard(compilation_unit_t *unit,
                                    lang_standard_t standard)
{
	unit->standard = standard;
	switch (standard) {
	case STANDARD_ANSI:
		switch (unit->type) {
		case COMPILATION_UNIT_C:
		case COMPILATION_UNIT_PREPROCESSED_C:
			unit->standard = STANDARD_C89;
			break;
		case COMPILATION_UNIT_CXX:
		case COMPILATION_UNIT_PREPROCESSED_CXX:
			unit->standard = STANDARD_CXX98;
			break;
		default:
			break;
		}
		break;

	case STANDARD_DEFAULT:
		switch (unit->type) {
		case COMPILATION_UNIT_C:
		case COMPILATION_UNIT_PREPROCESSED_C:
			unit->standard = STANDARD_GNU99;
			break;
		case COMPILATION_UNIT_CXX:
		case COMPILATION_UNIT_PREPROCESSED_CXX:
			unit->standard = STANDARD_GNUXX98;
			break;
		default:
			break;
		}
		break;

	default:
		break;
	}
}

static bool output_preprocessor_tokens(compilation_unit_t *unit, FILE *out)
{
	/* just here for gcc compatibility */
	fprintf(out, "# 1 \"%s\"\n", unit->name);
	fprintf(out, "# 1 \"<built-in>\"\n");
	fprintf(out, "# 1 \"<command-line>\"\n");

	set_preprocessor_output(out);
	switch_pp_input(unit->input, unit->name, NULL, false);

	for (;;) {
		next_preprocessing_token();
		if (pp_token.kind == T_EOF)
			break;
		emit_pp_token();
	}

	fputc('\n', out);
	check_unclosed_conditionals();
	close_pp_input();
	print_error_summary();
	set_preprocessor_output(NULL);

	if (unit->type == COMPILATION_UNIT_C) {
		unit->type = COMPILATION_UNIT_PREPROCESSED_C;
	} else if (unit->type == COMPILATION_UNIT_CXX) {
		unit->type = COMPILATION_UNIT_PREPROCESSED_CXX;
	}
	bool res = close_input(unit);
	return res && error_count == 0;
}

static bool open_input(compilation_unit_t *unit)
{
	/* input already available as FILE? */
	if (unit->input != NULL)
		return true;

	const char *const inputname = unit->name;
	unit->input_is_pipe = false;
	if (streq(inputname, "-")) {
		unit->input   = stdin;
	} else {
		unit->input = fopen(inputname, "r");
		if (unit->input == NULL) {
			fprintf(stderr, "Could not open '%s': %s\n", inputname,
					strerror(errno));
			return false;
		}
	}
	return true;
}

static int compilation_loop(compile_mode_t mode, compilation_unit_t *units,
							lang_standard_t standard, FILE *out)
{
	int  result                   = EXIT_SUCCESS;
	bool already_constructed_firm = false;
	for (compilation_unit_t *unit = units; unit != NULL; unit = unit->next) {
		const char *const inputname = unit->name;

		determine_unit_standard(unit, standard);
		setup_cmode(unit);

again:
		switch (unit->type) {
		case COMPILATION_UNIT_IR: {
			bool res = open_input(unit);
			if (!res) {
				result = EXIT_FAILURE;
				continue;
			}
			res = !ir_import_file(unit->input, unit->name);
			if (!res) {
				fprintf(stderr, "Import of firm graph from '%s' failed\n",
				        inputname);
				result = EXIT_FAILURE;
				continue;
			}
			unit->type = COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION;
			goto again;
		}
		case COMPILATION_UNIT_ASSEMBLER:
			panic("TODO: preprocess for assembler");
		case COMPILATION_UNIT_C:
		case COMPILATION_UNIT_CXX:
			if (external_preprocessor != NULL) {
				bool res = run_external_preprocessor(unit);
				if (!res) {
					result = EXIT_FAILURE;
					continue;
				}
				goto again;
			}
			/* FALLTHROUGH */

		case COMPILATION_UNIT_PREPROCESSED_C:
		case COMPILATION_UNIT_PREPROCESSED_CXX: {
			bool res = open_input(unit);
			if (!res) {
				result = EXIT_FAILURE;
				continue;
			}
			init_tokens();

			if (mode == PreprocessOnly) {
				bool res = output_preprocessor_tokens(unit, out);
				if (!res) {
					result = EXIT_FAILURE;
					continue;
				}
				continue;
			}

			/* do the actual parsing */
			do_parsing(unit);
			goto again;
		}
		case COMPILATION_UNIT_AST:
			/* prints the AST even if errors occurred */
			if (mode == PrintAst) {
				print_to_file(out);
				print_ast(unit->ast);
			}
			if (unit->parse_errors) {
				result = EXIT_FAILURE;
				break;
			}

			if (mode == BenchmarkParser) {
				break;
			} else if (mode == PrintFluffy) {
				write_fluffy_decls(out, unit->ast);
				break;
			} else if (mode == PrintJna) {
				write_jna_decls(out, unit->ast);
				break;
			} else if (mode == PrintCompoundSizes) {
				write_compoundsizes(out, unit->ast);
				break;
			}

			/* build the firm graph */
			ir_timer_t *t_construct = ir_timer_new();
			timer_register(t_construct, "Frontend: Graph construction");
			timer_push(t_construct);
			if (already_constructed_firm) {
				panic("compiling multiple files/translation units not possible");
			}
			init_implicit_optimizations();
			translation_unit_to_firm(unit->ast);
			already_constructed_firm = true;
			timer_pop(t_construct);
			unit->type = COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION;
			goto again;

		case COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION:
			if (mode == ParseOnly)
				continue;

			if (mode == CompileDump) {
				/* find irg */
				ident    *id     = new_id_from_str(dumpfunction);
				ir_graph *irg    = NULL;
				int       n_irgs = get_irp_n_irgs();
				for (int i = 0; i < n_irgs; ++i) {
					ir_graph *tirg   = get_irp_irg(i);
					ident    *irg_id = get_entity_ident(get_irg_entity(tirg));
					if (irg_id == id) {
						irg = tirg;
						break;
					}
				}

				if (irg == NULL) {
					fprintf(stderr, "No graph for function '%s' found\n",
					        dumpfunction);
					return EXIT_FAILURE;
				}

				dump_ir_graph_file(out, irg);
				fclose(out);
				return EXIT_SUCCESS;
			}

			if (mode == CompileExportIR) {
				ir_export_file(out);
				if (ferror(out) != 0) {
					fprintf(stderr, "Error while writing to output\n");
					return EXIT_FAILURE;
				}
				return EXIT_SUCCESS;
			}

			FILE *asm_out;
			if (mode == Compile) {
				asm_out = out;
			} else {
				asm_out = make_temp_file("ccs", &unit->name);
			}
			generate_code(asm_out, inputname);
			if (asm_out != out) {
				fclose(asm_out);
			}
			unit->type = COMPILATION_UNIT_PREPROCESSED_ASSEMBLER;
			goto again;
		case COMPILATION_UNIT_PREPROCESSED_ASSEMBLER:
			if (mode != CompileAssemble && mode != CompileAssembleLink)
				break;

			/* assemble */
			const char *input = unit->name;
			if (mode == CompileAssemble) {
				fclose(out);
				unit->name = outname;
			} else {
				FILE *tempf = make_temp_file("cco", &unit->name);
				/* hackish... */
				fclose(tempf);
			}

			assemble(unit->name, input);

			unit->type = COMPILATION_UNIT_OBJECT;
			goto again;
		case COMPILATION_UNIT_UNKNOWN:
		case COMPILATION_UNIT_AUTODETECT:
		case COMPILATION_UNIT_OBJECT:
			break;
		}
	}
	return result;
}

static int link_program(compilation_unit_t *units)
{
	obstack_1grow(&ldflags_obst, '\0');
	const char *flags = obstack_finish(&ldflags_obst);

	/* construct commandline */
	const char *linker = getenv("CPARSER_LINK");
	if (linker != NULL) {
		obstack_printf(&file_obst, "%s ", linker);
	} else {
		if (target_triple != NULL)
			obstack_printf(&file_obst, "%s-", target_triple);
		obstack_printf(&file_obst, "%s ", LINKER);
	}

	for (compilation_unit_t *unit = units; unit != NULL; unit = unit->next) {
		if (unit->type != COMPILATION_UNIT_OBJECT)
			continue;

		add_flag(&file_obst, "%s", unit->name);
	}

	add_flag(&file_obst, "-o");
	add_flag(&file_obst, outname);
	obstack_printf(&file_obst, "%s", flags);
	obstack_1grow(&file_obst, '\0');

	char *commandline = obstack_finish(&file_obst);

	if (verbose) {
		puts(commandline);
	}
	int err = system(commandline);
	if (err != EXIT_SUCCESS) {
		fprintf(stderr, "linker reported an error\n");
		return EXIT_FAILURE;
	}
	return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
	const char         *print_file_name_file = NULL;
	compile_mode_t      mode                 = CompileAssembleLink;
	int                 opt_level            = 1;
	char                cpu_arch[16]         = "ia32";
	compilation_unit_t *units                = NULL;
	compilation_unit_t *last_unit            = NULL;
	bool                construct_dep_target = false;
	bool                do_timing            = false;
	bool                profile_generate     = false;
	bool                profile_use          = false;

	/* hack for now... */
	if (strstr(argv[0], "pptest") != NULL) {
		extern int pptest_main(int argc, char **argv);
		return pptest_main(argc, argv);
	}

	temp_files = NEW_ARR_F(char*, 0);
	atexit(free_temp_files);

	obstack_init(&cppflags_obst);
	obstack_init(&ldflags_obst);
	obstack_init(&asflags_obst);
	obstack_init(&file_obst);
	init_include_paths();

#define GET_ARG_AFTER(def, args)                                             \
	do {                                                                     \
	def = &arg[sizeof(args)-1];                                              \
	if (def[0] == '\0') {                                                    \
		++i;                                                                 \
		if (i >= argc) {                                                     \
			fprintf(stderr, "error: expected argument after '" args "'\n");  \
			argument_errors = true;                                          \
			break;                                                           \
		}                                                                    \
		def = argv[i];                                                       \
		if (def[0] == '-' && def[1] != '\0') {                               \
			fprintf(stderr, "error: expected argument after '" args "'\n");  \
			argument_errors = true;                                          \
			continue;                                                        \
		}                                                                    \
	}                                                                        \
	} while (0)

#define SINGLE_OPTION(ch) (option[0] == (ch) && option[1] == '\0')

	/* initialize this early because it has to parse options */
	gen_firm_init();

	/* early options parsing (find out optimization level and OS) */
	for (int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if (arg[0] != '-')
			continue;

		const char *option = &arg[1];
		if (option[0] == 'O') {
			sscanf(&option[1], "%d", &opt_level);
		}
	}

	if (target_machine == NULL) {
		target_machine = firm_get_host_machine();
	}
	choose_optimization_pack(opt_level);
	setup_target_machine();

	/* parse rest of options */
	lang_standard_t         standard        = STANDARD_DEFAULT;
	compilation_unit_type_t forced_unittype = COMPILATION_UNIT_AUTODETECT;
	help_sections_t         help            = HELP_NONE;
	bool                    argument_errors = false;
	for (int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if (arg[0] == '-' && arg[1] != '\0') {
			/* an option */
			const char *option = &arg[1];
			if (option[0] == 'o') {
				GET_ARG_AFTER(outname, "-o");
			} else if (option[0] == 'g') {
				/* TODO: parse -gX with 0<=x<=3... */
				set_be_option("debug=frameinfo");
				set_be_option("ia32-nooptcc=yes");
			} else if (SINGLE_OPTION('c')) {
				mode = CompileAssemble;
			} else if (SINGLE_OPTION('E')) {
				mode = PreprocessOnly;
			} else if (SINGLE_OPTION('s')) {
				add_flag(&ldflags_obst, "-s");
			} else if (SINGLE_OPTION('S')) {
				mode = Compile;
			} else if (option[0] == 'O') {
				continue;
			} else if (option[0] == 'I') {
				const char *opt;
				GET_ARG_AFTER(opt, "-I");
				add_flag(&cppflags_obst, "-I%s", opt);
				append_include_path(&bracket_searchpath, opt);
			} else if (option[0] == 'D') {
				const char *opt;
				GET_ARG_AFTER(opt, "-D");
				add_flag(&cppflags_obst, "-D%s", opt);
			} else if (option[0] == 'U') {
				const char *opt;
				GET_ARG_AFTER(opt, "-U");
				add_flag(&cppflags_obst, "-U%s", opt);
			} else if (option[0] == 'l') {
				const char *opt;
				GET_ARG_AFTER(opt, "-l");
				add_flag(&ldflags_obst, "-l%s", opt);
			} else if (option[0] == 'L') {
				const char *opt;
				GET_ARG_AFTER(opt, "-L");
				add_flag(&ldflags_obst, "-L%s", opt);
			} else if (SINGLE_OPTION('v')) {
				verbose = 1;
			} else if (SINGLE_OPTION('w')) {
				add_flag(&cppflags_obst, "-w");
				disable_all_warnings();
			} else if (option[0] == 'x') {
				const char *opt;
				GET_ARG_AFTER(opt, "-x");
				forced_unittype = get_unit_type_from_string(opt);
				if (forced_unittype == COMPILATION_UNIT_UNKNOWN) {
					fprintf(stderr, "Unknown language '%s'\n", opt);
					argument_errors = true;
				}
			} else if (streq(option, "M")) {
				mode = PreprocessOnly;
				add_flag(&cppflags_obst, "-M");
			} else if (streq(option, "MMD") ||
			           streq(option, "MD")) {
			    construct_dep_target = true;
				add_flag(&cppflags_obst, "-%s", option);
			} else if (streq(option, "MM")  ||
			           streq(option, "MP")) {
				add_flag(&cppflags_obst, "-%s", option);
			} else if (streq(option, "MT") ||
			           streq(option, "MQ") ||
			           streq(option, "MF")) {
				const char *opt;
				GET_ARG_AFTER(opt, "-MT");
				add_flag(&cppflags_obst, "-%s", option);
				add_flag(&cppflags_obst, "%s", opt);
			} else if (streq(option, "include")) {
				const char *opt;
				GET_ARG_AFTER(opt, "-include");
				add_flag(&cppflags_obst, "-include");
				add_flag(&cppflags_obst, "%s", opt);
			} else if (streq(option, "idirafter")) {
				const char *opt;
				GET_ARG_AFTER(opt, "-idirafter");
				add_flag(&cppflags_obst, "-idirafter");
				add_flag(&cppflags_obst, "%s", opt);
				append_include_path(&after_searchpath, opt);
			} else if (streq(option, "isystem")) {
				const char *opt;
				GET_ARG_AFTER(opt, "-isystem");
				add_flag(&cppflags_obst, "-isystem");
				add_flag(&cppflags_obst, "%s", opt);
				append_include_path(&system_searchpath, opt);
			} else if (streq(option, "iquote")) {
				const char *opt;
				GET_ARG_AFTER(opt, "-iquote");
				add_flag(&cppflags_obst, "-iquote");
				add_flag(&cppflags_obst, "%s", opt);
				append_include_path(&quote_searchpath, opt);
			} else if (streq(option, "pthread")) {
				/* set flags for the preprocessor */
				add_flag(&cppflags_obst, "-D_REENTRANT");
				/* set flags for the linker */
				add_flag(&ldflags_obst, "-lpthread");
			} else if (streq(option, "nostdinc")
					|| streq(option, "trigraphs")) {
				/* pass these through to the preprocessor */
				add_flag(&cppflags_obst, "%s", arg);
			} else if (streq(option, "pipe")) {
				/* here for gcc compatibility */
			} else if (streq(option, "static")) {
				add_flag(&ldflags_obst, "-static");
			} else if (streq(option, "shared")) {
				add_flag(&ldflags_obst, "-shared");
			} else if (option[0] == 'f') {
				char const *orig_opt;
				GET_ARG_AFTER(orig_opt, "-f");

				if (strstart(orig_opt, "input-charset=")) {
					char const* const encoding = strchr(orig_opt, '=') + 1;
					input_encoding = encoding;
				} else if (strstart(orig_opt, "align-loops=") ||
				           strstart(orig_opt, "align-jumps=") ||
				           strstart(orig_opt, "align-functions=")) {
					fprintf(stderr, "ignoring gcc option '-f%s'\n", orig_opt);
				} else if (strstart(orig_opt, "visibility=")) {
					const char *val = strchr(orig_opt, '=')+1;
					elf_visibility_tag_t visibility
						= get_elf_visibility_from_string(val);
					if (visibility == ELF_VISIBILITY_ERROR) {
						fprintf(stderr, "invalid visibility '%s' specified\n",
						        val);
						argument_errors = true;
					} else {
						set_default_visibility(visibility);
					}
				} else if (strstart(orig_opt, "message-length=")) {
					/* ignore: would only affect error message format */
				} else if (streq(orig_opt, "fast-math") ||
				           streq(orig_opt, "fp-fast")) {
					firm_fp_model = fp_model_fast;
				} else if (streq(orig_opt, "fp-precise")) {
					firm_fp_model = fp_model_precise;
				} else if (streq(orig_opt, "fp-strict")) {
					firm_fp_model = fp_model_strict;
				} else if (streq(orig_opt, "help")) {
					fprintf(stderr, "warning: -fhelp is deprecated\n");
					help |= HELP_OPTIMIZATION;
				} else {
					/* -f options which have an -fno- variant */
					char const *opt         = orig_opt;
					bool        truth_value = true;
					if (opt[0] == 'n' && opt[1] == 'o' && opt[2] == '-') {
						truth_value = false;
						opt += 3;
					}

					if (streq(opt, "diagnostics-show-option")) {
						diagnostics_show_option = truth_value;
					} else if (streq(opt, "dollars-in-identifiers")) {
						allow_dollar_in_symbol = truth_value;
					} else if (streq(opt, "omit-frame-pointer")) {
						set_be_option(truth_value ? "omitfp" : "omitfp=no");
					} else if (streq(opt, "short-wchar")) {
						wchar_atomic_kind = truth_value ? ATOMIC_TYPE_USHORT
							: ATOMIC_TYPE_INT;
					} else if (streq(opt, "show-column")) {
						show_column = truth_value;
					} else if (streq(opt, "signed-char")) {
						char_is_signed = truth_value;
					} else if (streq(opt, "strength-reduce")) {
						/* does nothing, for gcc compatibility (even gcc does
						 * nothing for this switch anymore) */
					} else if (streq(opt, "syntax-only")) {
						mode = truth_value ? ParseOnly : CompileAssembleLink;
					} else if (streq(opt, "unsigned-char")) {
						char_is_signed = !truth_value;
					} else if (streq(opt, "freestanding")) {
						freestanding = truth_value;
					} else if (streq(opt, "hosted")) {
						freestanding = !truth_value;
					} else if (streq(opt, "profile-generate")) {
						profile_generate = truth_value;
					} else if (streq(opt, "profile-use")) {
						profile_use = truth_value;
					} else if (!truth_value &&
					           streq(opt, "asynchronous-unwind-tables")) {
					    /* nothing todo, a gcc feature which we do not support
					     * anyway was deactivated */
					} else if (streq(opt, "verbose-asm")) {
						/* ignore: we always print verbose assembler */
					} else if (streq(opt, "jump-tables")             ||
					           streq(opt, "expensive-optimizations") ||
					           streq(opt, "common")                  ||
					           streq(opt, "optimize-sibling-calls")  ||
					           streq(opt, "align-loops")             ||
					           streq(opt, "align-jumps")             ||
					           streq(opt, "align-functions")         ||
					           streq(opt, "PIC")                     ||
					           streq(opt, "stack-protector")         ||
					           streq(opt, "stack-protector-all")) {
						fprintf(stderr, "ignoring gcc option '-f%s'\n", orig_opt);
					} else {
						int res = firm_option(orig_opt);
						if (res == 0) {
							fprintf(stderr, "error: unknown Firm option '-f%s'\n",
							        orig_opt);
							argument_errors = true;
							continue;
						}
					}
				}
			} else if (option[0] == 'b') {
				const char *opt;
				GET_ARG_AFTER(opt, "-b");

				if (streq(opt, "help")) {
					fprintf(stderr, "warning: -bhelp is deprecated (use --help-firm)\n");
					help |= HELP_FIRM;
				} else {
					int res = be_parse_arg(opt);
					if (res == 0) {
						fprintf(stderr, "error: unknown Firm backend option '-b %s'\n",
								opt);
						argument_errors = true;
					} else if (strstart(opt, "isa=")) {
						strncpy(cpu_arch, opt, sizeof(cpu_arch));
					}
				}
			} else if (option[0] == 'W') {
				if (strstart(option + 1, "p,")) {
					// pass options directly to the preprocessor
					const char *opt;
					GET_ARG_AFTER(opt, "-Wp,");
					add_flag(&cppflags_obst, "-Wp,%s", opt);
				} else if (strstart(option + 1, "l,")) {
					// pass options directly to the linker
					const char *opt;
					GET_ARG_AFTER(opt, "-Wl,");
					add_flag(&ldflags_obst, "-Wl,%s", opt);
				} else if (streq(option + 1, "no-trigraphs")
							|| streq(option + 1, "undef")
							|| streq(option + 1, "missing-include-dirs")
							|| streq(option + 1, "endif-labels")) {
					add_flag(&cppflags_obst, "%s", arg);
				} else if (streq(option+1, "init-self")) {
					/* ignored (same as gcc does) */
				} else if (streq(option+1, "format-y2k")
				           || streq(option+1, "format-security")
				           || streq(option+1, "old-style-declaration")
				           || streq(option+1, "type-limits")) {
				    /* ignore (gcc compatibility) */
				} else {
					set_warning_opt(&option[1]);
				}
			} else if (option[0] == 'm') {
				/* -m options */
				const char *opt;
				char arch_opt[64];

				GET_ARG_AFTER(opt, "-m");
				if (strstart(opt, "target=")) {
					GET_ARG_AFTER(opt, "-mtarget=");
					if (!parse_target_triple(opt)) {
						argument_errors = true;
					} else {
						const char *isa = setup_target_machine();
						strncpy(cpu_arch, isa, sizeof(cpu_arch));
						target_triple = opt;
					}
				} else if (strstart(opt, "triple=")) {
					GET_ARG_AFTER(opt, "-mtriple=");
					if (!parse_target_triple(opt)) {
						argument_errors = true;
					} else {
						const char *isa = setup_target_machine();
						strncpy(cpu_arch, isa, sizeof(cpu_arch));
						target_triple = opt;
					}
				} else if (strstart(opt, "arch=")) {
					GET_ARG_AFTER(opt, "-march=");
					snprintf(arch_opt, sizeof(arch_opt), "%s-arch=%s", cpu_arch, opt);
					int res = be_parse_arg(arch_opt);
					snprintf(arch_opt, sizeof(arch_opt), "%s-opt=%s", cpu_arch, opt);
					res &= be_parse_arg(arch_opt);

					if (res == 0) {
						fprintf(stderr, "Unknown architecture '%s'\n", arch_opt);
						argument_errors = true;
					}
				} else if (strstart(opt, "tune=")) {
					GET_ARG_AFTER(opt, "-mtune=");
					snprintf(arch_opt, sizeof(arch_opt), "%s-opt=%s", cpu_arch, opt);
					int res = be_parse_arg(arch_opt);
					if (res == 0)
						argument_errors = true;
				} else if (strstart(opt, "cpu=")) {
					GET_ARG_AFTER(opt, "-mcpu=");
					snprintf(arch_opt, sizeof(arch_opt), "%s-arch=%s", cpu_arch, opt);
					int res = be_parse_arg(arch_opt);
					if (res == 0)
						argument_errors = true;
				} else if (strstart(opt, "fpmath=")) {
					GET_ARG_AFTER(opt, "-mfpmath=");
					if (streq(opt, "387"))
						opt = "x87";
					else if (streq(opt, "sse"))
						opt = "sse2";
					else {
						fprintf(stderr, "error: option -mfpmath supports only 387 or sse\n");
						argument_errors = true;
					}
					if (!argument_errors) {
						snprintf(arch_opt, sizeof(arch_opt), "%s-fpunit=%s", cpu_arch, opt);
						int res = be_parse_arg(arch_opt);
						if (res == 0)
							argument_errors = true;
					}
				} else if (strstart(opt, "preferred-stack-boundary=")) {
					GET_ARG_AFTER(opt, "-mpreferred-stack-boundary=");
					snprintf(arch_opt, sizeof(arch_opt), "%s-stackalign=%s", cpu_arch, opt);
					int res = be_parse_arg(arch_opt);
					if (res == 0)
						argument_errors = true;
				} else if (streq(opt, "rtd")) {
					default_calling_convention = CC_STDCALL;
				} else if (strstart(opt, "regparm=")) {
					fprintf(stderr, "error: regparm convention not supported yet\n");
					argument_errors = true;
				} else if (streq(opt, "soft-float")) {
					add_flag(&ldflags_obst, "-msoft-float");
					snprintf(arch_opt, sizeof(arch_opt), "%s-fpunit=softfloat", cpu_arch);
					int res = be_parse_arg(arch_opt);
					if (res == 0)
						argument_errors = true;
				} else if (streq(opt, "sse2")) {
					/* ignore for now, our x86 backend always uses sse when
					 * sse is requested */
				} else {
					long int value = strtol(opt, NULL, 10);
					if (value == 0) {
						fprintf(stderr, "error: wrong option '-m %s'\n",  opt);
						argument_errors = true;
					} else if (value != 16 && value != 32 && value != 64) {
						fprintf(stderr, "error: option -m supports only 16, 32 or 64\n");
						argument_errors = true;
					} else {
						unsigned machine_size = (unsigned)value;
						/* TODO: choose/change backend based on this */
						add_flag(&cppflags_obst, "-m%u", machine_size);
						add_flag(&asflags_obst, "-m%u", machine_size);
						add_flag(&ldflags_obst, "-m%u", machine_size);
					}
				}
			} else if (streq(option, "pg")) {
				set_be_option("gprof");
				add_flag(&ldflags_obst, "-pg");
			} else if (streq(option, "ansi")) {
				standard = STANDARD_ANSI;
			} else if (streq(option, "pedantic")) {
				fprintf(stderr, "warning: ignoring gcc option '%s'\n", arg);
			} else if (strstart(option, "std=")) {
				const char *const o = &option[4];
				standard =
					streq(o, "c++")            ? STANDARD_CXX98   :
					streq(o, "c++98")          ? STANDARD_CXX98   :
					streq(o, "c11")            ? STANDARD_C11     :
					streq(o, "c1x")            ? STANDARD_C11     : // deprecated
					streq(o, "c89")            ? STANDARD_C89     :
					streq(o, "c90")            ? STANDARD_C89     :
					streq(o, "c99")            ? STANDARD_C99     :
					streq(o, "c9x")            ? STANDARD_C99     : // deprecated
					streq(o, "gnu++98")        ? STANDARD_GNUXX98 :
					streq(o, "gnu11")          ? STANDARD_GNU11   :
					streq(o, "gnu1x")          ? STANDARD_GNU11   : // deprecated
					streq(o, "gnu89")          ? STANDARD_GNU89   :
					streq(o, "gnu99")          ? STANDARD_GNU99   :
					streq(o, "gnu9x")          ? STANDARD_GNU99   : // deprecated
					streq(o, "iso9899:1990")   ? STANDARD_C89     :
					streq(o, "iso9899:199409") ? STANDARD_C89AMD1 :
					streq(o, "iso9899:1999")   ? STANDARD_C99     :
					streq(o, "iso9899:199x")   ? STANDARD_C99     : // deprecated
					streq(o, "iso9899:2011")   ? STANDARD_C11     :
					(fprintf(stderr, "warning: ignoring gcc option '%s'\n", arg), standard);
			} else if (streq(option, "version")) {
				print_cparser_version();
				return EXIT_SUCCESS;
			} else if (streq(option, "dumpversion")) {
				/* gcc compatibility option */
				print_cparser_version_short();
				return EXIT_SUCCESS;
			} else if (strstart(option, "print-file-name=")) {
				GET_ARG_AFTER(print_file_name_file, "-print-file-name=");
			} else if (option[0] == '-') {
				/* double dash option */
				++option;
				if (streq(option, "gcc")) {
					features_on  |=  _GNUC;
					features_off &= ~_GNUC;
				} else if (streq(option, "no-gcc")) {
					features_on  &= ~_GNUC;
					features_off |=  _GNUC;
				} else if (streq(option, "ms")) {
					features_on  |=  _MS;
					features_off &= ~_MS;
				} else if (streq(option, "no-ms")) {
					features_on  &= ~_MS;
					features_off |=  _MS;
				} else if (streq(option, "strict")) {
					strict_mode = true;
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
				} else if (streq(option, "print-compound-sizes")) {
					mode = PrintCompoundSizes;
				} else if (streq(option, "print-jna")) {
					mode = PrintJna;
				} else if (streq(option, "jna-limit")) {
					++i;
					if (i >= argc) {
						fprintf(stderr, "error: "
						        "expected argument after '--jna-limit'\n");
						argument_errors = true;
						break;
					}
					jna_limit_output(argv[i]);
				} else if (streq(option, "jna-libname")) {
					++i;
					if (i >= argc) {
						fprintf(stderr, "error: "
						        "expected argument after '--jna-libname'\n");
						argument_errors = true;
						break;
					}
					jna_set_libname(argv[i]);
				} else if (streq(option, "external-pp")) {
					if (i+1 < argc && argv[i+1][0] != '-') {
						++i;
						external_preprocessor = argv[i+1];
					} else {
						external_preprocessor = PREPROCESSOR;
					}
				} else if (streq(option, "no-external-pp")) {
					external_preprocessor = NULL;
				} else if (streq(option, "time")) {
					do_timing = true;
				} else if (streq(option, "version")) {
					print_cparser_version();
					return EXIT_SUCCESS;
				} else if (streq(option, "help")) {
					help |= HELP_BASIC;
				} else if (streq(option, "help-parser")) {
					help |= HELP_PARSER;
				} else if (streq(option, "help-warnings")) {
					help |= HELP_WARNINGS;
				} else if (streq(option, "help-codegen")) {
					help |= HELP_CODEGEN;
				} else if (streq(option, "help-linker")) {
					help |= HELP_LINKER;
				} else if (streq(option, "help-optimization")) {
					help |= HELP_OPTIMIZATION;
				} else if (streq(option, "help-language-tools")) {
					help |= HELP_LANGUAGETOOLS;
				} else if (streq(option, "help-debug")) {
					help |= HELP_DEBUG;
				} else if (streq(option, "help-firm")) {
					help |= HELP_FIRM;
				} else if (streq(option, "help-all")) {
					help |= HELP_ALL;
				} else if (streq(option, "dump-function")) {
					++i;
					if (i >= argc) {
						fprintf(stderr, "error: "
						        "expected argument after '--dump-function'\n");
						argument_errors = true;
						break;
					}
					dumpfunction = argv[i];
					mode         = CompileDump;
				} else if (streq(option, "export-ir")) {
					mode = CompileExportIR;
				} else if (streq(option, "unroll-loops")) {
					/* ignore (gcc compatibility) */
				} else {
					fprintf(stderr, "error: unknown argument '%s'\n", arg);
					argument_errors = true;
				}
			} else {
				fprintf(stderr, "error: unknown argument '%s'\n", arg);
				argument_errors = true;
			}
		} else {
			compilation_unit_type_t type = forced_unittype;
			if (type == COMPILATION_UNIT_AUTODETECT) {
				if (streq(arg, "-")) {
					/* - implicitly means C source file */
					type = COMPILATION_UNIT_C;
				} else {
					const char *suffix = strrchr(arg, '.');
					/* Ensure there is at least one char before the suffix */
					if (suffix != NULL && suffix != arg) {
						++suffix;
						type =
							streq(suffix, "S")   ? COMPILATION_UNIT_ASSEMBLER              :
							streq(suffix, "a")   ? COMPILATION_UNIT_OBJECT                 :
							streq(suffix, "c")   ? COMPILATION_UNIT_C                      :
							streq(suffix, "i")   ? COMPILATION_UNIT_PREPROCESSED_C         :
							streq(suffix, "C")   ? COMPILATION_UNIT_CXX                    :
							streq(suffix, "cc")  ? COMPILATION_UNIT_CXX                    :
							streq(suffix, "cp")  ? COMPILATION_UNIT_CXX                    :
							streq(suffix, "cpp") ? COMPILATION_UNIT_CXX                    :
							streq(suffix, "CPP") ? COMPILATION_UNIT_CXX                    :
							streq(suffix, "cxx") ? COMPILATION_UNIT_CXX                    :
							streq(suffix, "c++") ? COMPILATION_UNIT_CXX                    :
							streq(suffix, "ii")  ? COMPILATION_UNIT_PREPROCESSED_CXX       :
							streq(suffix, "h")   ? COMPILATION_UNIT_C                      :
							streq(suffix, "ir")  ? COMPILATION_UNIT_IR                     :
							streq(suffix, "o")   ? COMPILATION_UNIT_OBJECT                 :
							streq(suffix, "s")   ? COMPILATION_UNIT_PREPROCESSED_ASSEMBLER :
							streq(suffix, "so")  ? COMPILATION_UNIT_OBJECT                 :
							COMPILATION_UNIT_OBJECT; /* gcc behavior: unknown file extension means object file */
					}
				}
			}

			compilation_unit_t *entry = OALLOCZ(&file_obst, compilation_unit_t);
			entry->name = arg;
			entry->type = type;

			if (last_unit != NULL) {
				last_unit->next = entry;
			} else {
				units = entry;
			}
			last_unit = entry;
		}
	}

	if (help != HELP_NONE) {
		print_help(argv[0], help);
		return !argument_errors;
	}

	if (print_file_name_file != NULL) {
		print_file_name(print_file_name_file);
		return EXIT_SUCCESS;
	}
	if (units == NULL) {
		fprintf(stderr, "error: no input files specified\n");
		argument_errors = true;
	}

	if (argument_errors) {
		usage(argv[0]);
		return EXIT_FAILURE;
	}

	/* apply some effects from switches */
	c_mode |= features_on;
	c_mode &= ~features_off;
	if (profile_generate) {
		add_flag(&ldflags_obst, "-lfirmprof");
		set_be_option("profilegenerate");
	}
	if (profile_use) {
		set_be_option("profileuse");
	}

	init_symbol_table();
	init_types_and_adjust();
	init_typehash();
	init_basic_types();
	if (c_mode & _CXX) {
		init_wchar_types(ATOMIC_TYPE_WCHAR_T);
	} else {
		init_wchar_types(wchar_atomic_kind);
	}
	init_preprocessor();
	init_ast();
	init_parser();
	init_ast2firm();
	init_mangle();

	if (do_timing)
		timer_init();

	if (construct_dep_target) {
		if (outname != 0 && strlen(outname) >= 2) {
			get_output_name(dep_target, sizeof(dep_target), outname, ".d");
		} else {
			get_output_name(dep_target, sizeof(dep_target), units->name, ".d");
		}
	} else {
		dep_target[0] = '\0';
	}

	char outnamebuf[4096];
	if (outname == NULL) {
		const char *filename = units->name;

		switch(mode) {
		case BenchmarkParser:
		case PrintAst:
		case PrintFluffy:
		case PrintJna:
		case PrintCompoundSizes:
		case PreprocessOnly:
		case ParseOnly:
			outname = "-";
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
		case CompileExportIR:
			get_output_name(outnamebuf, sizeof(outnamebuf), filename, ".ir");
			outname = outnamebuf;
			break;
		case CompileAssembleLink:
			if (firm_is_windows_os(target_machine)) {
				outname = "a.exe";
			} else {
				outname = "a.out";
			}
			break;
		}
	}

	assert(outname != NULL);

	FILE *out;
	if (streq(outname, "-")) {
		out = stdout;
	} else {
		out = fopen(outname, "w");
		if (out == NULL) {
			fprintf(stderr, "Could not open '%s' for writing: %s\n", outname,
					strerror(errno));
			return EXIT_FAILURE;
		}
	}

	int result = compilation_loop(mode, units, standard, out);
	if (result != EXIT_SUCCESS) {
		if (out != stdout)
			unlink(outname);
		return result;
	}

	/* link program file */
	if (mode == CompileAssembleLink) {
		int result = link_program(units);
		if (result != EXIT_SUCCESS) {
			if (out != stdout)
				unlink(outname);
			return result;
		}
	}

	if (do_timing)
		timer_term(stderr);

	free_temp_files();
	obstack_free(&cppflags_obst, NULL);
	obstack_free(&ldflags_obst, NULL);
	obstack_free(&asflags_obst, NULL);
	obstack_free(&file_obst, NULL);

	gen_firm_finish();
	exit_mangle();
	exit_ast2firm();
	exit_parser();
	exit_ast();
	exit_preprocessor();
	exit_typehash();
	exit_types();
	exit_tokens();
	exit_symbol_table();
	return EXIT_SUCCESS;
}
