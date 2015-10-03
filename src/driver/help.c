/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "help.h"

#include <assert.h>
#include <libfirm/be.h>
#include <stdio.h>
#include <stdlib.h>

#include "firm/firm_opt.h"
#include "warning.h"

help_sections_t help;

/** Print help for a simpel_arg() option. */
void help_simple(const char *option, const char *explanation)
{
	printf("\t%-15s  %s\n", option, explanation);
}

/** Print help for a prefix_arg() option. */
static void help_prefix(const char *option, const char *optname,
                        const char *explanation)
{
	char buf[128];
	snprintf(buf, sizeof(buf), "%s %s", option, optname);
	help_simple(buf, explanation);
}

/** Print help for an accept_prefix() option. */
static void help_aprefix(const char *prefix, const char *optname,
                         const char *explanation)
{
	char buf[128];
	snprintf(buf, sizeof(buf), "%s%s", prefix, optname);
	help_simple(buf, explanation);
}

/** Print help for a spaced_arg() option. */
static void help_spaced(const char *option, const char *optname,
                        const char *explanation)
{
	help_prefix(option, optname, explanation);
}

/** Print help for an equals_arg() option. */
static void help_equals(const char *option, const char *optname,
                        const char *explanation)
{
	char buf[128];
	snprintf(buf, sizeof(buf), "%s=%s", option, optname);
	help_simple(buf, explanation);
}

static void help_f_yesno(const char *option, const char *explanation)
{
	help_simple(option, explanation);
}

void put_choice(const char *choice, const char *explanation)
{
	printf("\t    %-11s  %s\n", choice, explanation);
}

void help_usage(const char *argv0)
{
	fprintf(stderr, "Usage %s [options] input [-o output]\n", argv0);
}

static void print_help_basic(const char *argv0)
{
	help_usage(argv0);
	puts("");
	help_simple("--help",                   "Display this information");
	help_simple("--version",                "Display compiler version");
	help_simple("--help-preprocessor",      "Display information about preprocessor options");
	help_simple("--help-parser",            "Display information about parser options");
	help_simple("--help-warnings",          "Display information about warning options");
	help_simple("--help-codegen",           "Display information about code-generation options");
	help_simple("--help-optimization",      "Display information about optimization options");
	help_simple("--help-linker",            "Display information about linker options");
	help_simple("--help-language-tools",    "Display information about language tools options");
	help_simple("--help-debug",             "Display information about compiler debugging options");
	help_simple("--help-firm",              "Display information about direct firm options");
	help_simple("--help-all",               "Display information about all options");
	help_simple("-c",                       "Compile and assemble but do not link");
	help_simple("-E",                       "Preprocess only");
	help_simple("-S",                       "Compile but do not assembler or link");
	help_f_yesno("-fsyntax-only",           "Check the syntax but do not produce code");
	help_simple("-o",                       "Specify output file");
	help_simple("-v",                       "Verbose output (show invocation of sub-processes)");
	help_prefix("-x", "LANGUAGE",           "Force input language:");
	put_choice("c",                      "C");
	put_choice("c++",                    "C++");
	put_choice("assembler",              "Assembler (no preprocessing)");
	put_choice("assembler-with-cpp",     "Assembler with preprocessing");
	put_choice("none",                   "Autodetection");
	help_simple("-dumpversion",             "Display shortened compiler version");
	help_simple("-dumpmachine",             "Display target machine triple");
	help_simple("-pipe",                    "Ignored (gcc compatibility)");
	/* Do not document these: support/check_option.py will see them:
	 * help_simple("-version", "");
	 * help_simple("-fno-syntax-only", "");
	 */
}

static void print_help_preprocessor(void)
{
	help_prefix("-I", "DIR",                "Append to header searchpath");
	help_prefix("-D", "SYMBOL[=value]",     "Define preprocessor symbol");
	help_prefix("-U", "SYMBOL",             "Undefine preprocessor symbol");
	help_equals("--sysroot", "DIR" ,        "set (default) root directory for (headers and) libraries");
	help_prefix("-isysroot", "DIR",         "set root directory for headers");
	help_simple("-include",                 "");
	help_simple("-no-integrated-cpp",       "Force use of external preprocessor");
	help_simple("-integrated-cpp",          "Force use of integrated preprocessor");
	help_simple("-nostdinc",                "Do not search standard system include directories");
	help_simple("-trigraphs",               "Support ISO C trigraphs");
	help_simple("-M",                       "");
	help_simple("-MD",                      "");
	help_simple("-MMD",                     "");
	help_simple("-MM",                      "");
	help_simple("-MP",                      "");
	help_prefix("-MT", "target",            "");
	help_prefix("-MQ", "target",            "");
	help_prefix("-MF", "file",              "");
	help_prefix("-idirafter", "DIR",        "Append to header searchpath (after -I dirs)");
	help_prefix("-iquote", "DIR",           "Append to header searchpath for #include with quoted argument");
	help_prefix("-isystem", "DIR",          "Append to system header searchpath");
	help_aprefix("-Wp,", "OPTION",          "Pass option directly to preprocessor");
	help_spaced("-Xpreprocessor", "OPTION", "Pass option directly to preprocessor");
	help_equals("-finput-charset", "CHARSET", "Select encoding of input files");
	help_f_yesno("-fdollars-in-identifiers", "Accept dollar symbol in identifiers");
	/* Undocumented:
	 * help_simple("-fno-dollars-in-identifiers", "");
	 */
}

static void print_help_parser(void)
{
	help_equals("-fmessage-length", "LEN",  "Ignored (gcc compatibility)");
	help_simple("-fshort-wchar",            "Type \"wchar_t\" is unsigned short instead of int");
	help_simple("-fshow-column",            "Show the column number in diagnostic messages");
	help_simple("-fcolor-diagnostics",      "Use colors in diagnostics");
	help_simple("-fdiagnostics-color",      "Use colors (gcc compatibility)");
	help_simple("-fsigned-char",            "Type \"char\" is a signed type");
	help_simple("-funsigned-char",          "Type \"char\" is an unsigned type");
	help_simple("--ms",                     "Enable msvc extensions");
	help_simple("--no-ms",                  "Disable msvc extensions");
	help_simple("--gcc",                    "Enable gcc extensions");
	help_simple("--no-gcc",                 "Disable gcc extensions");
	help_equals("-std", "STANDARD",         "Specify language standard:");
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
	help_simple("-ansi",                    "-std=c90 (for C) or -std=c++98 (for C++)");
	help_f_yesno("-ffreestanding",          "Compile in freestanding mode (see ISO C standard)");
	help_f_yesno("-fhosted",                "Compile in hosted (not freestanding) mode");
	help_f_yesno("-fbuiltin",               "Expect standard semantic from libc function names");
	/* Undocumented:
	 * help_equals("--std", "", "");
	 */
}

static void print_help_warnings(void)
{
	help_f_yesno("-fdiagnostics-show-option", "Show the switch, which controls a warning, after each warning");
	help_simple("-w",                         "Disable all warnings");
	help_simple("-pedantic",                  "Enable pedantic warnings (equivalent to -Wpedantic)");
	help_simple("-pedantic-errors",           "Error on pedantic warnings (equivalent to -Werror=pedantic)");
	help_simple("-Winit-self",                "Ignored (gcc compatibility)");
	help_simple("-Wformat-y2k",               "Ignored (gcc compatibility)");
	help_simple("-Wformat-security",          "Ignored (gcc compatibility)");
	help_simple("-Wold-style-declaration",    "Ignored (gcc compatibility)");
	help_simple("-Wtype-limits",              "Ignored (gcc compatibility)");
	print_warning_opt_help();
}

static void print_help_optimization(void)
{
	help_aprefix("-O", "LEVEL",              "Select optimization level");
	put_choice("0",                          "Disable all optimizations");
	put_choice("1",                          "Only perform fast basic optimizations");
	put_choice("2",                          "Optimize in a way suitable for most software");
	put_choice("3",                          "Aggressively optimize at the expense of compilation speed and code size");
	put_choice("g",                          "Optimize without degrading the ability to debug");
	put_choice("s",                          "Optimize for code size");
	put_choice("z",                          "Aggressively optimize for code size");
	firm_option_help(help_simple);
	help_simple("-fexpensive-optimizations", "Ignored (gcc compatibility)");
	/* Undocumented:
	 * help_simple("-fhelp", "");
	 */
}

static void print_help_codegeneration(void)
{
	help_spaced("-target", "TARGET",              "Specify target architecture as CPU-manufacturer-OS triple");
	help_equals("--target", "TARGET",             "Same as -target TARGET");
	help_aprefix("-m", "OPTION",                  "Pass target specific option");
	help_simple("-m16",                           "Select 16 bit variant of target architecture");
	help_simple("-m32",                           "Select 32 bit variant of target architecture");
	help_simple("-m64",                           "Select 64 bit variant of target architecture");
	help_aprefix("-g", "LEVEL",                   "Generate debug information");
	help_simple("-pg",                            "Instrument code for gnu gprof");
	help_f_yesno("-fPIC",                         "Produce position independent code");
	help_f_yesno("-fpic",                         "Produce position independent code");
	help_f_yesno("-fplt",                         "Use procedure linkage table for PIC calls (default)");
	help_equals("-fvisibility", "VISIBILITY",     "Set default visibility for external symbols");
	put_choice("default", "");
	put_choice("internal", "");
	put_choice("hidden", "");
	put_choice("protected", "");
	help_f_yesno("-fomit-frame-pointer",          "Produce code without frame pointer where possible");
	help_f_yesno("-fprofile-generate",            "Generate instrumented code to collect profile information");
	help_f_yesno("-fprofile-use",                 "Use profile information generated by instrumented binaries");
	help_simple("-pthread",                       "Use pthread threading library");
	help_simple("-fexceptions",                   "Enable exception handling");
	help_f_yesno("-ffast-math",                   "Enable imprecise floatingpoint transformations");
	help_f_yesno("-fverbose-asm",                 "Enable verbose assembly output");
	help_f_yesno("-frounding-math",               "Ignored (gcc compatibility)");
	help_simple("-fexcess-precision=standard",    "Ignored (gcc compatibility)");
	help_simple("--unroll-loops",                 "Ignored (gcc compatibility)");
	help_f_yesno("-fjump-tables",                 "Ignored (gcc compatibility)");
	help_f_yesno("-fcommon",                      "Ignored (gcc compatibility)");
	help_f_yesno("-foptimize-sibling-calls",      "Ignored (gcc compatibility)");
	help_f_yesno("-fstrength-reduce",             "Ignored (gcc compatibility)");
	help_f_yesno("-fasynchronous-unwind-tables",  "Ignored (gcc compatibility)");
	help_f_yesno("-funwind-tables",               "Ignored (gcc compatibility)");
	help_f_yesno("-fstack-protector",             "Ignored (gcc compatibility)");
	help_f_yesno("-fstack-protector-all",         "Ignored (gcc compatibility)");
	help_equals("-falign-loops", "ALIGNMENT",     "Ignored (gcc compatibility)");
	help_equals("-falign-jumps", "ALIGNEMNT",     "Ignored (gcc compatibility)");
	help_equals("-falign-functions", "ALIGNMENT", "Ignored (gcc compatibility)");
	puts("");
	puts("\tMost of these options can be used with a no- prefix to disable them");
	puts("\te.g. -fno-omit-frame-pointer");
}

static void print_help_linker(void)
{
	help_prefix("-l", "LIBRARY",            "Link with specified library");
	help_prefix("-L", "DIR",                "Append to library search path");
	help_simple("-s",                       "Do not produce symbol table and relocation information");
	help_simple("-shared",                  "Produce a shared library");
	help_simple("-static",                  "Produce statically linked binary");
	help_simple("-nodefaultlibs",           "Do not link compiler default libraries");
	help_simple("-nostartfiles",            "Do not link startup files");
	help_simple("-nostdlib",                "Do not link default language library");
	help_simple("-pie",                     "Produce position independent executable");
	help_simple("-no-pie",                  "Do not produce a position independent executable");
	help_simple("-rdynamic",                "Export all dynamic symbols");
	help_simple("-shared-libgcc",           "Link to shared libgcc library");
	help_simple("-static-libgcc",           "Link to libgcc statically");
	help_simple("-symbolic",                "Bind reference to global symbols");
	help_aprefix("-Wa,", "OPTION",          "Pass option directly to assembler");
	help_spaced("-Xassembler", "OPTION",    "Pass option directly to assembler");
	help_aprefix("-Wl,", "OPTION",          "Pass option directly to linker");
	help_spaced("-Xlinker", "OPTION",       "Pass option directly to linker");
	help_equals("-print-file-name", "LIB",  "Print full path to library LIB");
}

static void print_help_debug(void)
{
	help_simple("--print-ast",              "Preprocess, parse and print AST");
	help_simple("--print-implicit-cast",    "");
	help_simple("--print-parenthesis",      "");
	help_simple("--benchmark",              "Preprocess and parse, produces no output");
	help_simple("--time",                   "Measure time of compiler passes");
	help_simple("--statev",                 "Produce statev output");
	help_equals("--filtev", "FILTER",       "Set statev filter regex");
	help_spaced("--dump-function", "FUNC",  "Preprocess, parse and output vcg graph of func");
	help_simple("--export-ir",              "Preprocess, parse and output compiler intermediate representation");
	help_simple("--jittest",                "Jit compile and exeucte main() function");
}

static void print_help_language_tools(void)
{
	help_simple("--print-fluffy",           "Preprocess, parse and generate declarations for the fluffy language");
	help_simple("--print-jna",              "Preprocess, parse and generate declarations for JNA");
	help_spaced("--jna-limit", "FILENAME",  "");
	help_spaced("--jna-libname", "NAME",    "");
	help_simple("--print-compound-sizes",   "Preprocess, parse and print sizes of compound types marked with dllexport");
}

static void print_help_firm(void)
{
	help_simple("-bhelp",                   "Display firm codegeneration options");
	help_equals("-bisa", "ARCH",            "Select architecture for firm backend");
	help_aprefix("-b", "OPTION",            "Directly pass option to libFirm backend");
	int res = be_parse_arg("help");
	(void) res;
	assert(res);
}

int action_help(const char *argv0)
{
	if (help & HELP_BASIC)         print_help_basic(argv0);
	if (help & HELP_PREPROCESSOR)  print_help_preprocessor();
	if (help & HELP_PARSER)        print_help_parser();
	if (help & HELP_WARNINGS)      print_help_warnings();
	if (help & HELP_OPTIMIZATION)  print_help_optimization();
	if (help & HELP_CODEGEN)       print_help_codegeneration();
	if (help & HELP_LINKER)        print_help_linker();
	if (help & HELP_LANGUAGETOOLS) print_help_language_tools();
	if (help & HELP_DEBUG)         print_help_debug();
	if (help & HELP_FIRM)          print_help_firm();
	return EXIT_SUCCESS;
}
