/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#include "options.h"

#include <assert.h>
#include <libfirm/be.h>
#include <string.h>

#include "actions.h"
#include "adt/error.h"
#include "adt/strutil.h"
#include "adt/util.h"
#include "ast/ast_t.h"
#include "c_driver.h"
#include "diagnostic.h"
#include "driver.h"
#include "firm/ast2firm.h"
#include "firm/firm_opt.h"
#include "firm/mangle.h"
#include "help.h"
#include "lang_features.h"
#include "parser/parser.h"
#include "parser/preprocessor.h"
#include "predefs.h"
#include "target.h"
#include "wrappergen/write_compoundsizes.h"
#include "wrappergen/write_fluffy.h"
#include "wrappergen/write_jna.h"

codegen_option_t  *codegen_options        = NULL;
codegen_option_t **codegen_options_anchor = &codegen_options;
char               firm_isa[16];
bool               profile_generate;
bool               profile_use;

static compilation_unit_type_t forced_unittype = COMPILATION_UNIT_AUTODETECT;

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
	def = s->argv[s->i+1];
	if (def[0] == '-' && def[1] != '\0') {
		errorf(NULL, "expected argument after '-%s'", prefix);
		s->argument_errors = true;
		return NULL;
	}
	++s->i;
	return def;
}

const char *spaced_arg(const char *arg, options_state_t *s,
                       bool arg_may_be_option)
{
	const char *option = &s->argv[s->i][1];
	if (!streq(option, arg))
		return NULL;

	if (s->i+1 >= s->argc) {
		errorf(NULL, "expected argument after '-%s'", arg);
		s->argument_errors = true;
		return NULL;
	}
	const char *res = s->argv[s->i+1];
	if (!arg_may_be_option && res[0] == '-' && res[1] != '\0') {
		errorf(NULL, "expected argument after '-%s'", arg);
		s->argument_errors = true;
		return NULL;
	}
	++s->i;
	return res;
}

static const char *equals_arg(const char *prefix, options_state_t *s)
{
	const char *option = &s->argv[s->i][1];
	if (!strstart(option, prefix))
		return NULL;

	const char *arg = option + strlen(prefix);
	if (arg[0] == '\0') {
		errorf(NULL, "expected argument after '-%s'", prefix);
		s->argument_errors = true;
		return NULL;
	}
	return arg;
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

static void set_be_option(const char *arg)
{
	int res = be_parse_arg(arg);
	if (!res)
		panic("setting firm backend option failed");
}

bool options_parse_preprocessor(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;
	const char *option = &full_option[1];

	const char *arg;
	if ((arg = prefix_arg("I", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-I%s", arg);
		append_include_path(&bracket_searchpath, arg);
	} else if ((arg = prefix_arg("D", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-D%s", arg);
		parse_define(arg);
	} else if ((arg = prefix_arg("U", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-U%s", arg);
		undefine(arg);
	} else if (streq(option, "MMD") || streq(option, "MD")) {
		construct_dep_target = true;
		driver_add_flag(&cppflags_obst, "-%s", option);
	} else if (streq(option, "MP")) {
		driver_add_flag(&cppflags_obst, "-%s", option);
	} else if ((arg = prefix_arg("MT", s)) != NULL
	        || (arg = prefix_arg("MQ", s)) != NULL
	        || (arg = prefix_arg("MF", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-%s", option);
		driver_add_flag(&cppflags_obst, "%s", arg);
	} else if ((arg = prefix_arg("include", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-include");
		driver_add_flag(&cppflags_obst, "%s", arg);
	} else if ((arg = prefix_arg("idirafter", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-idirafter");
		driver_add_flag(&cppflags_obst, "%s", arg);
		append_include_path(&after_searchpath, arg);
	} else if ((arg = prefix_arg("isystem", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-isystem");
		driver_add_flag(&cppflags_obst, "%s", arg);
		append_include_path(&system_searchpath, arg);
	} else if ((arg = prefix_arg("iquote", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-iquote");
		driver_add_flag(&cppflags_obst, "%s", arg);
		append_include_path(&quote_searchpath, arg);
	} else if (streq(option, "nostdinc")) {
		driver_no_stdinc = true;
		driver_add_flag(&cppflags_obst, "-%s", option);
	} else if ((arg = equals_arg("finput-charset=", s)) != NULL) {
		input_decoder = input_get_decoder(arg);
		if (input_decoder == NULL) {
			errorf(NULL, "input encoding \"%s\" not supported", arg);
		}
	} else if ((arg = spaced_arg("Xpreprocessor", s, true)) != NULL) {
		driver_add_flag(&cppflags_obst, "-Xpreprocessor");
		driver_add_flag(&cppflags_obst, arg);
	} else if (strstart(option, "Wp,")) {
		driver_add_flag(&cppflags_obst, "%s", full_option);
	} else if (streq(option, "integrated-cpp")) {
		driver_use_integrated_preprocessor = true;
	} else if (streq(option, "no-integrated-cpp")) {
		driver_use_integrated_preprocessor = false;
	} else if (streq(option, "-trigraphs")) {
		/* pass these through to the preprocessor */
		driver_add_flag(&cppflags_obst, "-%s", option);
	} else if (streq(option, "fdollars-in-identifiers")) {
		allow_dollar_in_symbol = true;
	} else if (streq(option, "fno-dollars-in-identifiers")) {
		allow_dollar_in_symbol = false;
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
	if (option[0] == 'O') {
		/* -O flags have already been handled in early option parsing */
	} else if ((arg = prefix_arg("o", s)) != NULL) {
		outname = arg;
	} else if ((arg = prefix_arg("x", s)) != NULL) {
		forced_unittype = get_unit_type_from_string(arg);
		if (forced_unittype == COMPILATION_UNIT_UNKNOWN) {
			errorf(NULL, "unknown language '%s'", arg);
			s->argument_errors = true;
			return false;
		}
	} else if (streq(option, "pipe")) {
		/* here for gcc compatibility */
	} else if ((arg = equals_arg("std=", s)) != NULL) {
		standard =
			streq(arg, "c++")            ? STANDARD_CXX98   :
			streq(arg, "c++98")          ? STANDARD_CXX98   :
			streq(arg, "c11")            ? STANDARD_C11     :
			streq(arg, "c1x")            ? STANDARD_C11     : // deprecated
			streq(arg, "c89")            ? STANDARD_C89     :
			streq(arg, "c90")            ? STANDARD_C89     :
			streq(arg, "c99")            ? STANDARD_C99     :
			streq(arg, "c9x")            ? STANDARD_C99     : // deprecated
			streq(arg, "gnu++98")        ? STANDARD_GNUXX98 :
			streq(arg, "gnu11")          ? STANDARD_GNU11   :
			streq(arg, "gnu1x")          ? STANDARD_GNU11   : // deprecated
			streq(arg, "gnu89")          ? STANDARD_GNU89   :
			streq(arg, "gnu99")          ? STANDARD_GNU99   :
			streq(arg, "gnu9x")          ? STANDARD_GNU99   : // deprecated
			streq(arg, "iso9899:1990")   ? STANDARD_C89     :
			streq(arg, "iso9899:199409") ? STANDARD_C89AMD1 :
			streq(arg, "iso9899:1999")   ? STANDARD_C99     :
			streq(arg, "iso9899:199x")   ? STANDARD_C99     : // deprecated
			streq(arg, "iso9899:2011")   ? STANDARD_C11     :
			(warningf(WARN_COMPAT_OPTION, NULL,
			          "ignoring gcc option '-%s'", option), standard);
	} else if (streq(option, "ansi")) {
		standard = STANDARD_ANSI;
	} else if (streq(option, "-gcc")) {
		features_on  |=  _GNUC;
		features_off &= ~_GNUC;
	} else if (streq(option, "-no-gcc")) {
		features_on  &= ~_GNUC;
		features_off |=  _GNUC;
	} else if (streq(option, "-ms")) {
		features_on  |=  _MS;
		features_off &= ~_MS;
	} else if (streq(option, "-no-ms")) {
		features_on  &= ~_MS;
		features_off |=  _MS;
	} else if (streq(option, "-print-implicit-cast")) {
		print_implicit_casts = true;
	} else if (streq(option, "-print-parenthesis")) {
		print_parenthesis = true;
	} else if ((arg = spaced_arg("-jna-limit", s, false)) != NULL) {
		jna_limit_output(arg);
	} else if ((arg = spaced_arg("-jna-libname", s, false)) != NULL) {
		jna_set_libname(arg);
	} else if (streq(option, "v")) {
		driver_verbose = true;
	} else if (streq(option, "-time")) {
		do_timing    = true;
		print_timing = true;
	} else if (streq(option, "-statev")) {
		do_timing      = true;
		produce_statev = true;
	} else if ((arg = equals_arg("-filtev=", s)) != NULL) {
		filtev = arg;
	} else if ((arg = equals_arg("print-file-name=", s)) != NULL) {
		print_file_name_file = arg;
		s->action = action_print_file_name;
	} else if (streq(option, "version") || streq(option, "-version")) {
		s->action = action_version;
	} else if (streq(option, "dumpversion")) {
		s->action = action_version_short;
	} else if (streq(option, "dumpmachine")) {
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
	const char *option = &full_option[1];

	const char *arg;
	if ((arg = prefix_arg("l", s)) != NULL) {
		driver_add_flag(&ldflags_obst, "-l%s", arg);
	} else if ((arg = prefix_arg("L", s)) != NULL) {
		driver_add_flag(&ldflags_obst, "-L%s", arg);
	} else if (streq(option, "static")
	        || streq(option, "shared")
	        || streq(option, "s")
	        || strstart(option, "Wl,")) {
	    driver_add_flag(&ldflags_obst, full_option);
	} else if ((arg = spaced_arg("Xlinker", s, true)) != NULL) {
		driver_add_flag(&ldflags_obst, "-Xlinker");
		driver_add_flag(&ldflags_obst, arg);
	} else if (streq(option, "pg")) {
		set_be_option("gprof");
		driver_add_flag(&ldflags_obst, "-pg");
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
	const char *option = &full_option[1];

	const char *arg;
	if (strstart(option, "Wa,")) {
		driver_add_flag(&asflags_obst, "%s", full_option);
	} else if ((arg = spaced_arg("Xassembler", s, true)) != NULL) {
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
	if ((arg = equals_arg("falign-loops=", s)) != NULL
     || (arg = equals_arg("falign-jumps=", s)) != NULL
     || (arg = equals_arg("falign-functions=", s)) != NULL) {
		warningf(WARN_COMPAT_OPTION, NULL,
				 "ignoring gcc option '%s'", full_option);
	} else if ((arg = equals_arg("fvisibility=", s)) != NULL) {
		elf_visibility_t visibility = get_elf_visibility_from_string(arg);
		if (visibility == ELF_VISIBILITY_ERROR) {
			errorf(NULL, "invalid visibility '%s' specified", arg);
			s->argument_errors = true;
		} else {
			set_default_visibility(visibility);
		}
	} else if (option[0] == 'b') {
		if ((arg = equals_arg("bisa=", s)) != NULL) {
			snprintf(firm_isa, sizeof(firm_isa), "%s", arg);
		}
		if (!be_parse_arg(&option[1]))
			return false;
	} else if (streq(option, "-unroll-loops")) {
		/* ignore (gcc compatibility) */
	} else if (streq(option, "g") || streq(option, "g0") || streq(option, "g1")
			|| streq(option, "g2") || streq(option, "g3")) {
		set_be_option("debug=frameinfo");
		set_be_option("ia32-optcc=false");
	} else if (option[0] == 'm' && !strstart(option, "mtarget")) {
		arg = &option[1];
		/* remember option for backend */
		assert(obstack_object_size(&codegenflags_obst) == 0);
		obstack_blank(&codegenflags_obst, sizeof(codegen_option_t));
		size_t len = strlen(arg);
		obstack_grow0(&codegenflags_obst, arg, len);
		codegen_option_t *cg_option = obstack_finish(&codegenflags_obst);
		cg_option->next             = NULL;

		*codegen_options_anchor = cg_option;
		codegen_options_anchor  = &cg_option->next;
	} else {
		bool truth_value;
		const char *fopt;
		if ((fopt = f_no_arg(&truth_value, s)) != NULL) {
			if (streq(fopt, "fast-math")) {
				ir_allow_imprecise_float_transforms(truth_value);
			} else if (streq(fopt, "omit-frame-pointer")) {
				set_be_option(truth_value ? "omitfp" : "omitfp=no");
			} else if (streq(fopt, "strength-reduce")) {
				/* does nothing, for gcc compatibility (even gcc does
				 * nothing for this switch anymore) */
			} else if (!truth_value && streq(fopt, "asynchronous-unwind-tables")) {
				/* nothing todo, a gcc feature which we do not support
				 * anyway was deactivated */
			} else if (streq(fopt, "verbose-asm")) {
				/* ignore: we always print verbose assembler */
			} else if (streq(fopt, "jump-tables")             ||
					   streq(fopt, "expensive-optimizations") ||
					   streq(fopt, "common")                  ||
					   streq(fopt, "optimize-sibling-calls")  ||
					   streq(fopt, "align-loops")             ||
					   streq(fopt, "align-jumps")             ||
					   streq(fopt, "align-functions")         ||
					   streq(fopt, "PIC")                     ||
					   streq(fopt, "stack-protector")         ||
					   streq(fopt, "stack-protector-all")) {
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

	if (streq(option, "w")) {
		driver_add_flag(&cppflags_obst, "-w");
		disable_all_warnings();
	} else if (streq(option, "pedantic")) {
		dialect.strict = true;
		set_warning_opt("pedantic");
	} else if (option[0] == 'W') {
		if (streq(option+1, "init-self")) {
			/* ignored (same as gcc does) */
		} else if (streq(option+1, "format-y2k")
		        || streq(option+1, "format-security")
		        || streq(option+1, "old-style-declaration")
		        || streq(option+1, "type-limits")) {
			/* ignore (gcc compatibility) */
		} else if (option[1] != '\0' && option[2] == ',') {
			/* this is not a warning option */
			return false;
		} else {
			set_warning_opt(&option[1]);
		}
	} else if (option[0] == 'f') {
		const char *arg;

		if ((arg = equals_arg("message-length=", s)) != NULL) {
			(void)arg;
			/* not supported yet */
		} else {
			bool truth_value;
			const char *fopt;
			if ((fopt = f_no_arg(&truth_value, s)) != NULL) {
				if (streq(fopt, "diagnostics-show-option")) {
					diagnostics_show_option = truth_value;
				} else if (streq(fopt, "show-column")) {
					show_column = truth_value;
				} else if (streq(fopt, "color-diagnostics")
						|| streq(fopt, "diagnostics-color")) {
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
		if (streq(fopt, "short-wchar")) {
			dialect.wchar_atomic_kind = truth_value ? ATOMIC_TYPE_USHORT
			                                        : ATOMIC_TYPE_INT;
		} else if (streq(fopt, "signed-char")) {
			dialect.char_is_signed = truth_value;
		} else if (streq(fopt, "unsigned-char")) {
			dialect.char_is_signed = !truth_value;
		} else if (streq(fopt, "freestanding")) {
			dialect.freestanding = truth_value;
		} else if (streq(fopt, "hosted")) {
			dialect.freestanding = !truth_value;
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
	const char *option = &full_option[1];

	if (streq(option, "fhelp")) {
		fprintf(stderr,
		        "warning: -fhelp is deprecated (use --help-optimization)\n");
		help |= HELP_OPTIMIZATION;
	} else if (streq(option, "bhelp")) {
		fprintf(stderr, "warning: -bhelp is deprecated (use --help-firm)\n");
		help |= HELP_FIRM;
	} else if (streq(option, "-help")) {
		help |= HELP_BASIC;
	} else if (streq(option, "-help-preprocessor")) {
		help |= HELP_PREPROCESSOR;
	} else if (streq(option, "-help-parser")) {
		help |= HELP_PARSER;
	} else if (streq(option, "-help-warnings")) {
		help |= HELP_WARNINGS;
	} else if (streq(option, "-help-codegen")) {
		help |= HELP_CODEGEN;
	} else if (streq(option, "-help-linker")) {
		help |= HELP_LINKER;
	} else if (streq(option, "-help-optimization")) {
		help |= HELP_OPTIMIZATION;
	} else if (streq(option, "-help-language-tools")) {
		help |= HELP_LANGUAGETOOLS;
	} else if (streq(option, "-help-debug")) {
		help |= HELP_DEBUG;
	} else if (streq(option, "-help-firm")) {
		help |= HELP_FIRM;
	} else if (streq(option, "-help-all")) {
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
	return true;
}

bool options_parse_meta(options_state_t *s)
{
	const char *full_option = s->argv[s->i];
	if (full_option[0] != '-')
		return false;
	const char *option = &full_option[1];

	const char *arg;
	if (streq(option, "pthread")) {
		/* set flags for the preprocessor */
		driver_add_flag(&cppflags_obst, "-D_REENTRANT");
		/* set flags for the linker */
		driver_add_flag(&ldflags_obst, "-lpthread");
	} else if ((arg = equals_arg("mtarget=", s)) != NULL) {
		if (parse_target_triple(arg)) {
			target.triple = arg;
		} else {
			s->argument_errors = true;
		}
	} else {
		bool truth_value;
		const char *fopt;
		if ((fopt = f_no_arg(&truth_value, s)) != NULL) {
			if (streq(fopt, "profile-generate")) {
				profile_generate = truth_value;
			} else if (streq(fopt, "profile-use")) {
				profile_use = truth_value;
			} else {
				return false;
			}
		} else {
			return false;
		}
	}
	return true;
}

void options_parse_early(options_state_t *state)
{
	// disable backend verifiaction
#ifdef NO_DEFAULT_VERIFY
	be_parse_arg("verify=off");
#endif

	unsigned opt_level = 1;
	bool     opt_size  = false;
	for (int i = 1; i < state->argc; ++i) {
		const char *arg = state->argv[i];
		if (arg[0] != '-')
			continue;

		const char *option = &arg[1];
		if (option[0] == 'O') {
			char const *const opt = option + 1;
			if ('0' <= opt[0] && opt[0] <= '9' && opt[1] == '\0') {
				opt_level = opt[0] - '0';
			} else if (opt[0] == '\0') {
				opt_level = 1; /* '-O' is equivalent to '-O1'. */
			} else if (streq(opt, "fast")) {
				opt_level = 3; /* TODO stub. */
			} else if (streq(opt, "g")) {
				opt_level = 0; /* TODO stub. */
			} else if (streq(opt, "s")) {
				opt_level = 2; /* TODO For now, until we have a real '-Os'. */
				opt_size  = true;
			} else {
				errorf(NULL, "invalid optimization option '%s'", arg);
				state->argument_errors = true;
			}
		}
	}
	choose_optimization_pack(opt_level);
	predef_optimize      = opt_level > 0;
	predef_optimize_size = opt_size;
}
