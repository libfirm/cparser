/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#include "options.h"

#include <assert.h>
#include <libfirm/be.h>
#include <string.h>

#include "actions.h"
#include "adt/panic.h"
#include "adt/strutil.h"
#include "adt/util.h"
#include "ast/ast_t.h"
#include "ast/dialect.h"
#include "c_driver.h"
#include "diagnostic.h"
#include "driver.h"
#include "firm/ast2firm.h"
#include "firm/firm_opt.h"
#include "firm/mangle.h"
#include "help.h"
#include "parser/parser.h"
#include "parser/preprocessor.h"
#include "predefs.h"
#include "target.h"
#include "wrappergen/write_jna.h"

codegen_option_t  *codegen_options        = NULL;
codegen_option_t **codegen_options_anchor = &codegen_options;
bool               profile_generate;
bool               profile_use;

static compilation_unit_type_t forced_unittype = COMPILATION_UNIT_AUTODETECT;

bool simple_arg(const char *arg, options_state_t *s)
{
	assert(s->argv[s->i][0] == '-');
	const char *option = &s->argv[s->i][1];
	return streq(option, arg);
}

static const char *prefix_arg(const char *prefix, options_state_t *s)
{
	const char *option = &s->argv[s->i][1];
	if (!strstart(option, prefix))
		return NULL;

	const size_t prefix_len = strlen(prefix);
	const char  *def        = &option[prefix_len];
	if (def[0] != '\0')
		return def;

	if (s->i+1 >= s->argc) {
		errorf(NULL, "expected argument after '-%s'", prefix);
		s->argument_errors = true;
		return NULL;
	}
	return s->argv[++s->i];
}

char const *spaced_arg(char const *const arg, options_state_t *const s)
{
	char const *const option = &s->argv[s->i][1];
	if (streq(option, arg)) {
		if (s->i + 1 < s->argc)
			return s->argv[++s->i];
		errorf(NULL, "expected argument after '-%s'", arg);
		s->argument_errors = true;
	}
	return NULL;
}

static const char *equals_arg(const char *prefix, options_state_t *s)
{
	char const *const option = &s->argv[s->i][1];
	char const *const equals = strstart(option, prefix);
	if (equals) {
		if (equals[0] == '=') {
			char const *const arg = equals + 1;
			if (arg[0] != '\0')
				return arg;
			errorf(NULL, "expected argument after '-%s'", option);
			s->argument_errors = true;
		} else if (equals[0] == '\0') {
			errorf(NULL, "expected '=' and argument after '-%s'", option);
			s->argument_errors = true;
		}
	}
	return NULL;
}

static const char *f_no_arg(bool *truth_value, options_state_t *s)
{
	const char *option = &s->argv[s->i][1];
	if (option[0] != 'f')
		return NULL;
	++option;

	if (option[0] == 'n' && option[1] == 'o' && option[2] == '-') {
		*truth_value = false;
		option += 3;
	} else {
		*truth_value = true;
	}
	return option;
}

static bool f_yesno_arg(char const *const arg, options_state_t const *const s)
{
	const char *option = s->argv[s->i];
	assert(arg[0] == '-' && arg[1] == 'f');
	assert(option[0] == '-' && option[1] == 'f');
	if (option[2] == 'n' && option[3] == 'o' && option[4] == '-')
		option += 3;
	return streq(&arg[2], &option[2]);
}

static bool accept_prefix(options_state_t *const s, char const *const prefix,
                          bool expect_arg, char const **const arg)
{
	const char *option = s->argv[s->i];
	assert(option[0] == '-');
	assert(prefix[0] == '-');
	if (!strstart(&option[1], &prefix[1]))
		return false;

	const size_t prefix_len = strlen(prefix);
	*arg = option + prefix_len;
	if (expect_arg && (*arg)[0] == '\0') {
		errorf(NULL, "expected argument after '-%s'", prefix);
		s->argument_errors = true;
		return false;
	}
	return true;
}

bool options_parse_preprocessor(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;
	const char *option = &full_option[1];

	const char *arg;
	bool is_MD = false;
	if ((arg = prefix_arg("I", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-I%s", arg);
		append_include_path(&bracket_searchpath, arg, false);
	} else if ((arg = prefix_arg("D", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-D%s", arg);
		record_cmdline_define(true, arg);
	} else if ((arg = prefix_arg("U", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-U%s", arg);
		record_cmdline_define(false, arg);
	} else if (simple_arg("MMD", s) || (is_MD = simple_arg("MD", s))) {
		construct_dep_target = true;
		include_system_headers_in_dependencies = is_MD;
		driver_add_flag(&cppflags_obst, "-%s", option);
	} else if (simple_arg("MP", s)) {
		print_phony_targets = true;
		driver_add_flag(&cppflags_obst, "-%s", option);
	} else if ((arg = prefix_arg("MF", s)) != NULL) {
		dependency_file = arg;
		goto add_arg_opt;
	} else if ((arg = prefix_arg("MT", s)) != NULL) {
		dependency_target = arg;
		dont_escape_target = true;
		goto add_arg_opt;
	} else if ((arg = prefix_arg("MQ", s)) != NULL) {
		dependency_target = arg;
		dont_escape_target = false;
add_arg_opt:
		driver_add_flag(&cppflags_obst, "-%s", option);
		driver_add_flag(&cppflags_obst, "%s", arg);
	} else if ((arg = prefix_arg("include", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-include");
		driver_add_flag(&cppflags_obst, "%s", arg);
	} else if ((arg = prefix_arg("idirafter", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-idirafter");
		driver_add_flag(&cppflags_obst, "%s", arg);
		append_include_path(&after_searchpath, arg, false);
	} else if ((arg = prefix_arg("isystem", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-isystem");
		driver_add_flag(&cppflags_obst, "%s", arg);
		append_include_path(&system_searchpath, arg, false);
	} else if ((arg = prefix_arg("iquote", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-iquote");
		driver_add_flag(&cppflags_obst, "%s", arg);
		append_include_path(&quote_searchpath, arg, false);
	} else if (simple_arg("nostdinc", s)) {
		driver_no_stdinc = true;
		driver_add_flag(&cppflags_obst, "-%s", option);
	} else if ((arg = equals_arg("finput-charset", s)) != NULL) {
		input_decoder = input_get_decoder(arg);
		if (input_decoder == NULL) {
			errorf(NULL, "input encoding \"%s\" not supported", arg);
		}
	} else if ((arg = spaced_arg("Xpreprocessor", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-Xpreprocessor");
		driver_add_flag(&cppflags_obst, arg);
	} else if (accept_prefix(s, "-Wp,", true, &arg)) {
		driver_add_flag(&cppflags_obst, "%s", full_option);
	} else if (simple_arg("integrated-cpp", s)) {
		driver_use_integrated_preprocessor = true;
	} else if (simple_arg("no-integrated-cpp", s)) {
		driver_use_integrated_preprocessor = false;
	} else if (simple_arg("trigraphs", s)) {
		/* pass these through to the preprocessor */
		driver_add_flag(&cppflags_obst, "-%s", option);
	} else if (simple_arg("fdollars-in-identifiers", s)) {
		no_dollar_in_symbol = false;
	} else if (simple_arg("fno-dollars-in-identifiers", s)) {
		no_dollar_in_symbol = true;
	} else {
		return false;
	}
	return true;
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

static void set_language_standard(char const *const name)
{
	typedef struct name_std_t {
		char const     *name;
		lang_standard_t std;
	} name_std_t;
	static name_std_t const stds[] = {
		{ "c++",            STANDARD_CXX98   },
		{ "c++98",          STANDARD_CXX98   },
		{ "c11",            STANDARD_C11     },
		{ "c1x",            STANDARD_C11     }, // deprecated
		{ "c89",            STANDARD_C89     },
		{ "c90",            STANDARD_C89     },
		{ "c99",            STANDARD_C99     },
		{ "c9x",            STANDARD_C99     }, // deprecated
		{ "gnu++98",        STANDARD_GNUXX98 },
		{ "gnu11",          STANDARD_GNU11   },
		{ "gnu1x",          STANDARD_GNU11   }, // deprecated
		{ "gnu89",          STANDARD_GNU89   },
		{ "gnu99",          STANDARD_GNU99   },
		{ "gnu9x",          STANDARD_GNU99   }, // deprecated
		{ "iso9899:1990",   STANDARD_C89     },
		{ "iso9899:199409", STANDARD_C89AMD1 },
		{ "iso9899:1999",   STANDARD_C99     },
		{ "iso9899:199x",   STANDARD_C99     }, // deprecated
		{ "iso9899:2011",   STANDARD_C11     },
	};
	for (name_std_t const *i = stds; i != endof(stds); ++i) {
		if (streq(i->name, name)) {
			standard = i->std;
			return;
		}
	}
	errorf(NULL, "unknown language standard '%s' for '-std'", name);
}

bool options_parse_driver(options_state_t *s)
{
	const char *option = s->argv[s->i];
	if (option[0] != '-' || option[1] == '\0') {
		/* argument is not an option but an input filename */
		compilation_unit_type_t type = forced_unittype;
		if (type == COMPILATION_UNIT_AUTODETECT && streq(option, "-")) {
			/* - implicitly means C source file */
			type = COMPILATION_UNIT_C;
		}
		driver_add_input(option, type);
		s->had_inputs = true;
		return true;
	}
	++option;

	const char *arg;
	if ((arg = prefix_arg("o", s)) != NULL) {
		outname = arg;
	} else if ((arg = prefix_arg("x", s)) != NULL) {
		forced_unittype = get_unit_type_from_string(arg);
		if (forced_unittype == COMPILATION_UNIT_UNKNOWN) {
			errorf(NULL, "unknown language '%s'", arg);
			s->argument_errors = true;
			return false;
		}
	} else if (simple_arg("pipe", s)) {
		/* here for gcc compatibility */
	} else if ((arg = equals_arg("std", s)) != NULL
	        || (arg = equals_arg("-std", s)) != NULL) {
		set_language_standard(arg);
	} else if (simple_arg("ansi", s)) {
		standard = STANDARD_ANSI;
	} else if (simple_arg("-gcc", s)) {
		features_on  |=  _GNUC;
		features_off &= ~_GNUC;
	} else if (simple_arg("-no-gcc", s)) {
		features_on  &= ~_GNUC;
		features_off |=  _GNUC;
	} else if (simple_arg("-ms", s)) {
		features_on  |=  _MS;
		features_off &= ~_MS;
	} else if (simple_arg("-no-ms", s)) {
		features_on  &= ~_MS;
		features_off |=  _MS;
	} else if (simple_arg("-print-implicit-cast", s)) {
		print_implicit_casts = true;
	} else if (simple_arg("-print-parenthesis", s)) {
		print_parenthesis = true;
	} else if ((arg = spaced_arg("-jna-limit", s)) != NULL) {
		jna_limit_output(arg);
	} else if ((arg = spaced_arg("-jna-libname", s)) != NULL) {
		jna_set_libname(arg);
	} else if (simple_arg("v", s)) {
		driver_verbose = true;
	} else if (simple_arg("-time", s)) {
		do_timing    = true;
		print_timing = true;
	} else if (simple_arg("-statev", s)) {
		do_timing      = true;
		produce_statev = true;
	} else if ((arg = equals_arg("-filtev", s)) != NULL) {
		filtev = arg;
	} else if (simple_arg("version", s) || simple_arg("-version", s)) {
		s->action = action_version;
	} else if (simple_arg("dumpversion", s)) {
		s->action = action_version_short;
	} else if (simple_arg("dumpmachine", s)) {
		s->action = action_dumpmachine;
	} else if (option[0] == 'd') {
		/* scan debug flags */
		for (const char *flag = &option[1]; *flag != '\0'; ++flag) {
			if (*flag == 'M')
				dump_defines = true;
		}
	} else {
		return false;
	}
	return true;
}

bool options_parse_linker(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;

	const char *arg;
	if ((arg = prefix_arg("l", s)) != NULL) {
		driver_add_flag(&ldflags_obst, "-l%s", arg);
	} else if ((arg = prefix_arg("L", s)) != NULL) {
		driver_add_flag(&ldflags_obst, "-L%s", arg);
	} else if (simple_arg("static", s)
	        || simple_arg("no-pie", s)
	        || simple_arg("nodefaultlibs", s)
	        || simple_arg("nostartfiles", s)
	        || simple_arg("nostdlib", s)
	        || simple_arg("pie", s)
	        || simple_arg("rdynamic", s)
	        || simple_arg("s", s)
	        || simple_arg("shared", s)
	        || simple_arg("shared-libgcc", s)
	        || simple_arg("static-libgcc", s)
	        || simple_arg("symbolic", s)
	        || accept_prefix(s, "-Wl,", true, &arg)) {
	    driver_add_flag(&ldflags_obst, full_option);
	} else if ((arg = spaced_arg("Xlinker", s)) != NULL) {
		driver_add_flag(&ldflags_obst, "-Xlinker");
		driver_add_flag(&ldflags_obst, arg);
	} else if (simple_arg("pg", s)) {
		set_be_option("gprof");
		driver_add_flag(&ldflags_obst, "-pg");
	} else if ((arg = equals_arg("print-file-name", s)) != NULL) {
		print_file_name_file = arg;
		s->action = action_print_file_name;
	} else {
		return false;
	}
	return true;
}

bool options_parse_assembler(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;

	const char *arg;
	if (accept_prefix(s, "-Wa,", true, &arg)) {
		driver_add_flag(&asflags_obst, "%s", full_option);
	} else if ((arg = spaced_arg("Xassembler", s)) != NULL) {
		driver_add_flag(&asflags_obst, "-Xassembler");
		driver_add_flag(&asflags_obst, arg);
	} else {
		return false;
	}
	return true;
}

bool options_parse_codegen(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;
	const char *option = &full_option[1];

	const char *arg;
	if ((arg = equals_arg("falign-loops", s)) != NULL
	 || (arg = equals_arg("falign-jumps", s)) != NULL
	 || (arg = equals_arg("falign-functions", s)) != NULL) {
		warningf(WARN_COMPAT_OPTION, NULL,
		         "ignoring gcc option '%s'", full_option);
	} else if ((arg = equals_arg("fvisibility", s)) != NULL) {
		elf_visibility_t visibility = get_elf_visibility_from_string(arg);
		if (visibility == ELF_VISIBILITY_ERROR) {
			errorf(NULL, "invalid visibility '%s' specified", arg);
			s->argument_errors = true;
		} else {
			set_default_visibility(visibility);
		}
	} else if (accept_prefix(s, "-b", true, &arg)) {
		if (!be_parse_arg(arg)) {
			errorf(NULL, "invalid backend option '%s' (unknown option or invalid argument)",
			       full_option);
			s->argument_errors = true;
		} else if ((arg = equals_arg("bisa", s)) != NULL) {
			/* This is a quick and dirty option to try out new firm targets.
			 * Sooner rather than later the new target should be added properly
			 * to target.c! */
			target.firm_isa_specified = true;
			target.firm_isa = arg;
		}
	} else if (simple_arg("-unroll-loops", s)) {
		/* ignore (gcc compatibility) */
	} else if (simple_arg("fexcess-precision=standard", s)) {
		/* ignore (gcc compatibility) we always adhere to the C99 standard
		 * anyway in this respect */
	} else if (accept_prefix(s, "-g", false, &arg)) {
		if (streq(arg, "0")) {
			set_be_option("debug=none");
			set_be_option("ia32-optcc=true");
		} else {
			set_be_option("debug=frameinfo");
			set_be_option("ia32-optcc=false");
		}
	} else if (accept_prefix(s, "-m", false, &arg)) {
		arg = &option[1];
		/* remember option for backend */
		assert(obstack_object_size(&codegenflags_obst) == 0);
		obstack_blank(&codegenflags_obst, sizeof(codegen_option_t));
		size_t len = strlen(arg);
		obstack_grow(&codegenflags_obst, arg, len);
		codegen_option_t *const cg_option = obstack_nul_finish(&codegenflags_obst);
		cg_option->next = NULL;

		*codegen_options_anchor = cg_option;
		codegen_options_anchor  = &cg_option->next;
	} else {
		bool truth_value;
		const char *fopt;
		if ((fopt = f_no_arg(&truth_value, s)) != NULL) {
			if (f_yesno_arg("-ffast-math", s)) {
				ir_allow_imprecise_float_transforms(truth_value);
			} else if (f_yesno_arg("-fomit-frame-pointer", s)) {
				set_be_option(truth_value ? "omitfp" : "omitfp=no");
			} else if (f_yesno_arg("-fexceptions", s)) {
				set_support_exceptions(true);
				set_be_option(truth_value ? "exceptions" : "exceptions=no");
			} else if (f_yesno_arg("-fstrength-reduce", s)) {
				/* does nothing, for gcc compatibility (even gcc does
				 * nothing for this switch anymore) */
			} else if (!truth_value
			           && (f_yesno_arg("-fasynchronous-unwind-tables", s)
			               || f_yesno_arg("-funwind-tables", s))) {
				/* do nothing: a gcc feature which we do not support
				 * anyway was deactivated */
			} else if (f_yesno_arg("-frounding-math", s)) {
				/* ignore for gcc compatibility: we don't have any unsafe
				 * optimizations in that area */
			} else if (f_yesno_arg("-fverbose-asm", s)) {
				set_be_option(truth_value ? "verboseasm" : "verboseasm=no");
			} else if (f_yesno_arg("-fPIC", s)) {
				target.pic_mode = truth_value ? 2 : 0;
			} else if (f_yesno_arg("-fpic", s)) {
				target.pic_mode = truth_value ? 1 : 0;
			} else if (f_yesno_arg("-fplt", s)) {
				target.pic_no_plt = !truth_value;
			} else if (f_yesno_arg("-fjump-tables", s)             ||
			           f_yesno_arg("-fexpensive-optimizations", s) ||
			           f_yesno_arg("-fcommon", s)                  ||
			           f_yesno_arg("-foptimize-sibling-calls", s)  ||
			           f_yesno_arg("-falign-loops", s)             ||
			           f_yesno_arg("-falign-jumps", s)             ||
			           f_yesno_arg("-falign-functions", s)         ||
			           f_yesno_arg("-fstack-protector", s)         ||
			           f_yesno_arg("-fstack-protector-all", s)) {
				/* better warn the user for these as he might have expected
				 * that something happens */
				warningf(WARN_COMPAT_OPTION, NULL,
				         "ignoring gcc option '-f%s'", fopt);
			} else if (firm_option(&option[1])) {
				/* parsed a firm option */
			} else {
				return false;
			}
		} else {
			return false;
		}
	}
	return true;
}

bool options_parse_diagnostics(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;
	const char *option = &full_option[1];

	if (simple_arg("w", s)) {
		driver_add_flag(&cppflags_obst, "-w");
		disable_all_warnings();
	} else if (simple_arg("pedantic", s)) {
		dialect.strict = true;
		set_warning_opt("pedantic");
		set_warning_opt("error=return-type");
	} else if (simple_arg("pedantic-errors", s)) {
		dialect.strict = true;
		set_warning_opt("error=pedantic");
		set_warning_opt("error=return-type");
	} else if (option[0] == 'W') {
		if (simple_arg("Winit-self", s)) {
			/* ignored (same as gcc does) */
		} else if (simple_arg("Wformat-y2k", s)
		        || simple_arg("Wformat-security", s)
		        || simple_arg("Wold-style-declaration", s)
		        || simple_arg("Wtype-limits", s)) {
			/* ignore (gcc compatibility) */
		} else if (option[1] != '\0' && option[2] == ',') {
			/* this is not a warning option */
			return false;
		} else {
			set_warning_opt(&option[1]);
		}
	} else if (option[0] == 'f') {
		const char *arg;

		if ((arg = equals_arg("fmessage-length", s)) != NULL) {
			(void)arg;
			/* not supported yet */
		} else {
			bool truth_value;
			const char *fopt;
			if ((fopt = f_no_arg(&truth_value, s)) != NULL) {
				if (f_yesno_arg("-fdiagnostics-show-option", s)) {
					diagnostics_show_option = truth_value;
				} else if (f_yesno_arg("-fshow-column", s)) {
					show_column = truth_value;
				} else if (f_yesno_arg("-fcolor-diagnostics", s)
				        || f_yesno_arg("-fdiagnostics-color", s)) {
					diagnostic_enable_color(truth_value
						? (colorterm != 0 ? colorterm : 8)
						: 0);
				} else {
					return false;
				}
			} else {
				return false;
			}
		}
	} else {
		return false;
	}
	return true;
}

bool options_parse_c_dialect(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;

	bool truth_value;
	const char *fopt;
	if ((fopt = f_no_arg(&truth_value, s)) != NULL) {
		if (f_yesno_arg("-fshort-wchar", s)) {
			dialect.wchar_atomic_kind = truth_value ? ATOMIC_TYPE_USHORT
			                                        : ATOMIC_TYPE_INT;
		} else if (f_yesno_arg("-fsigned-char", s)) {
			dialect.char_is_signed = truth_value;
		} else if (f_yesno_arg("-funsigned-char", s)) {
			dialect.char_is_signed = !truth_value;
		} else if (f_yesno_arg("-ffreestanding", s)) {
			dialect.freestanding = truth_value;
			dialect.no_builtins = truth_value;
		} else if (f_yesno_arg("-fhosted", s)) {
			dialect.freestanding = !truth_value;
			dialect.no_builtins = !truth_value;
		} else if (f_yesno_arg("-fbuiltin", s)) {
			dialect.no_builtins = !truth_value;
		} else {
			return false;
		}
	} else {
		return false;
	}
	return true;
}

bool options_parse_help(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;

	if (simple_arg("fhelp", s)) {
		fprintf(stderr,
		        "warning: -fhelp is deprecated (use --help-optimization)\n");
		help |= HELP_OPTIMIZATION;
	} else if (simple_arg("bhelp", s)) {
		fprintf(stderr, "warning: -bhelp is deprecated (use --help-firm)\n");
		help |= HELP_FIRM;
	} else if (simple_arg("-help", s)) {
		help |= HELP_BASIC;
	} else if (simple_arg("-help-preprocessor", s)) {
		help |= HELP_PREPROCESSOR;
	} else if (simple_arg("-help-parser", s)) {
		help |= HELP_PARSER;
	} else if (simple_arg("-help-warnings", s)) {
		help |= HELP_WARNINGS;
	} else if (simple_arg("-help-codegen", s)) {
		help |= HELP_CODEGEN;
	} else if (simple_arg("-help-linker", s)) {
		help |= HELP_LINKER;
	} else if (simple_arg("-help-optimization", s)) {
		help |= HELP_OPTIMIZATION;
	} else if (simple_arg("-help-language-tools", s)) {
		help |= HELP_LANGUAGETOOLS;
	} else if (simple_arg("-help-debug", s)) {
		help |= HELP_DEBUG;
	} else if (simple_arg("-help-firm", s)) {
		help |= HELP_FIRM;
	} else if (simple_arg("-help-all", s)) {
		help |= HELP_ALL;
	} else {
		return false;
	}
	s->action = action_help;
	return true;
}

static bool parse_target_triple(const char *arg)
{
	machine_triple_t *triple = parse_machine_triple(arg);
	if (triple == NULL) {
		errorf(NULL, "target-triple '%s' is not in the form 'cpu_type-manufacturer-operating_system'", arg);
		return false;
	}
	target.machine = triple;
	target.triple  = arg;
	return true;
}

bool options_parse_early_target(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;
	const char *option = &full_option[1];

	const char *arg;
	if (simple_arg("pthread", s)) {
		/* set flags for the preprocessor */
		driver_add_flag(&cppflags_obst, "-D_REENTRANT");
		/* set flags for the linker */
		driver_add_flag(&ldflags_obst, "-lpthread");
	} else if ((arg = spaced_arg("target", s)) != NULL) {
		if (!parse_target_triple(arg))
			s->argument_errors = true;
		/* remove argument so we do not parse it again in later phases */
		s->argv[s->i-1] = NULL;
	} else if ((arg = equals_arg("-target", s)) != NULL) {
		if (!parse_target_triple(arg))
			s->argument_errors = true;
	} else if (simple_arg("m64", s) || simple_arg("m32", s)
	        || simple_arg("m16", s)) {
		driver_add_flag(&cppflags_obst, full_option);
		driver_add_flag(&asflags_obst, full_option);
		driver_add_flag(&ldflags_obst, full_option);
		target_size_override = atoi(option+1);
	} else {
		bool truth_value;
		const char *fopt;
		if ((fopt = f_no_arg(&truth_value, s)) != NULL) {
			if (f_yesno_arg("-fprofile-generate", s)) {
				profile_generate = truth_value;
			} else if (f_yesno_arg("-fprofile-use", s)) {
				profile_use = truth_value;
			} else {
				return false;
			}
		} else {
			return false;
		}
	}
	/* Remove argument so we do not parse it again in later phases */
	return true;
}

bool options_parse_early_codegen(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;

	const char *arg;
	if (accept_prefix(s, "-O", false, &arg)) {
		unsigned opt_level;
		if (arg[0] == '\0') {
			opt_level = 1; /* '-O' is equivalent to '-O1'. */
		} else if (is_digit(arg[0])) {
			opt_level = arg[0] - '0';
		} else if (streq(arg, "fast")) {
			opt_level = 3; /* TODO stub. */
		} else if (streq(arg, "g")) {
			opt_level = 0; /* TODO stub. */
		} else if (streq(arg, "s")) {
			opt_level = 2; /* TODO For now, until we have a real '-Os'. */
			predef_optimize_size = true;
		} else if (streq(arg, "z")) {
			opt_level = 2; /* TODO For now until we have a real '-Oz'. */
			predef_optimize_size = true;
		} else {
			errorf(NULL, "invalid optimization option '%s'", full_option);
			s->argument_errors = true;
			return true;
		}
		choose_optimization_pack(opt_level);
		predef_optimize = opt_level > 0;
	} else
		return false;
	/* Remove argument so we do not parse it again in later phases */
	return true;
}

bool options_parse_early_sysroot(options_state_t *const s)
{
	char const *arg;
	if ((arg = spaced_arg("isysroot", s)) != NULL) {
		isysroot = arg;
		s->argv[s->i - 1] = NULL;
	} else if ((arg = prefix_arg("isysroot", s)) != NULL) {
		isysroot = arg;
	} else if ((arg = spaced_arg("-sysroot", s)) != NULL) {
		lsysroot = arg;
		s->argv[s->i - 1] = NULL;
	} else if ((arg = equals_arg("-sysroot", s)) != NULL) {
		lsysroot = arg;
	} else {
		return false;
	}

	return true;
}
