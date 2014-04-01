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

void put_help(const char *option, const char *explanation)
{
	printf("\t%-15s  %s\n", option, explanation);
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
	put_help("--help",                   "Display this information");
	put_help("--version",                "Display compiler version");
	put_help("--help-preprocessor",      "Display information about preprocessor options");
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
	put_help("-no-integrated-cpp",       "Use an external preprocessor");
	put_help("-nostdinc",                "Do not search standard system include directories");
	put_help("-trigraphs",               "Support ISO C trigraphs");
	put_help("-isystem",                 "");
	put_help("-include",                 "");
	put_help("-I PATH",                  "");
	put_help("-D SYMBOL[=value]",        "");
	put_help("-U SYMBOL",                "");
	put_help("-Wp,OPTION",               "Pass option directly to preprocessor");
	put_help("-Xpreprocessor OPTION",    "Pass option directly to preprocessor");
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
	put_help("-fcolor-diagnostics",      "Use colors in diagnostics");
	put_help("-fdiagnostics-color",      "Use colors (gcc compatibility)");
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
	put_help("-pedantic",                "be pedantic about C standard");
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
	put_help("-pthread",                 "Use pthread threading library");
	put_help("-mtarget=TARGET",          "Specify target architecture as CPU-manufacturer-OS triple");
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
	put_help("-Wa,OPTION",               "Pass option directly to assembler");
	put_help("-Xassembler OPTION",       "Pass option directly to assembler");
	put_help("-Wl,OPTION",               "Pass option directly to linker");
	put_help("-Xlinker OPTION",          "Pass option directly to linker");
}

static void print_help_debug(void)
{
	put_help("--print-ast",              "Preprocess, parse and print AST");
	put_help("--print-implicit-cast",    "");
	put_help("--print-parenthesis",      "");
	put_help("--benchmark",              "Preprocess and parse, produces no output");
	put_help("--time",                   "Measure time of compiler passes");
	put_help("--statev",                 "Produce statev output");
	put_help("--filtev=filter",          "Set statev filter regex");
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
