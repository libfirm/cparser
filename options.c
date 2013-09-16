/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#include "options.h"

#include <assert.h>
#include <libfirm/be.h>
#include <string.h>

#include "adt/strutil.h"
#include "diagnostic.h"
#include "driver.h"
#include "ast_t.h"
#include "preprocessor.h"
#include "lang_features.h"
#include "wrappergen/write_compoundsizes.h"
#include "wrappergen/write_fluffy.h"
#include "wrappergen/write_jna.h"

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

static const char *spaced_arg(const char *arg, options_state_t *s)
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
	if (res[0] == '-' && res[1] != '\0') {
		errorf(NULL, "expected argument after '-%s'", arg);
		s->argument_errors = true;
		return NULL;
	}
	++s->i;
	return res;
}

static void set_be_option(const char *arg)
{
	int res = be_parse_arg(arg);
	(void) res;
	assert(res);
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
	} else if (streq(option, "M") || streq(option, "MM")) {
		mode = MODE_GENERATE_DEPENDENCIES;
		driver_add_flag(&cppflags_obst, "-%s", option);
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
		/* TODO */
		//stdinc = false;
		driver_add_flag(&cppflags_obst, "-%s", option);
	} else if (streq(option, "finput-charset=")) {
		char const* const encoding = strchr(option, '=') + 1;
		input_decoder = input_get_decoder(encoding);
		if (input_decoder == NULL) {
			errorf(NULL, "input encoding \"%s\" not supported",
				   encoding);
		}
	} else if ((arg = prefix_arg("Xpreprocessor", s)) != NULL) {
		driver_add_flag(&cppflags_obst, "-Xpreprocessor");
		driver_add_flag(&cppflags_obst, arg);
	} else if (strstart(option, "Wp,")) {
		driver_add_flag(&cppflags_obst, "%s", full_option);
	} else if (streq(option, "-external-pp")) {
		driver_use_external_preprocessor = true;
	} else if (streq(option, "-no-external-pp")) {
		driver_use_external_preprocessor = false;
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
	} else if (streq(option, "c")) {
		mode = MODE_COMPILE_ASSEMBLE;
	} else if (streq(option, "E")) {
		mode = MODE_PREPROCESS_ONLY;
	} else if (streq(option, "S")) {
		mode = MODE_COMPILE;
	} else if ((arg = prefix_arg("x", s)) != NULL) {
		forced_unittype = get_unit_type_from_string(arg);
		if (forced_unittype == COMPILATION_UNIT_UNKNOWN) {
			errorf(NULL, "unknown language '%s'", arg);
			s->argument_errors = true;
			return false;
		}
	} else if (streq(option, "-pipe")) {
		/* here for gcc compatibility */
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
	} else if (streq(option, "-benchmark")) {
		mode = MODE_BENCHMARK_PARSER;
	} else if (streq(option, "-print-ast")) {
		mode = MODE_PRINT_AST;
	} else if (streq(option, "-print-implicit-cast")) {
		print_implicit_casts = true;
	} else if (streq(option, "-print-parenthesis")) {
		print_parenthesis = true;
	} else if (streq(option, "-print-fluffy")) {
		mode = MODE_PRINT_FLUFFY;
	} else if (streq(option, "-print-compound-sizes")) {
		mode = MODE_PRINT_COMPOUND_SIZE;
	} else if (streq(option, "-print-jna")) {
		mode = MODE_PRINT_JNA;
	} else if ((arg = spaced_arg("-jna-limit", s)) != NULL) {
		jna_limit_output(arg);
	} else if ((arg = spaced_arg("-jna-libname", s)) != NULL) {
		jna_set_libname(arg);
	} else if ((arg = spaced_arg("-dump-function", s)) != NULL) {
		dumpfunction = arg;
		mode         = MODE_COMPILE_DUMP;
	} else if (streq(option, "-export-ir")) {
		mode = MODE_COMPILE_EXPORTIR;
	} else if (streq(option, "fsyntax-only")) {
		mode = MODE_PARSE_ONLY;
	} else if (streq(option, "fno-syntax-only")) {
	} else if (streq(option, "v")) {
		driver_verbose = true;
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
	} else if ((arg = prefix_arg("Xlinker", s)) != NULL) {
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
	} else if ((arg = prefix_arg("Xassembler", s)) != NULL) {
		driver_add_flag(&asflags_obst, "-Xassembler");
		driver_add_flag(&asflags_obst, arg);
	} else {
		return false;
	}
	return true;
}
