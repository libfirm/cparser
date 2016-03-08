/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#include "enable_posix.h"
#include "driver_t.h"

#include <assert.h>
#include <errno.h>
#include <libfirm/statev.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "adt/panic.h"
#include "adt/strutil.h"
#include "adt/util.h"
#include "c_driver.h"
#include "diagnostic.h"
#include "timing.h"

const char         *outname;
bool                produce_statev;
const char         *filtev;
int                 colorterm;
bool                do_timing;
bool                print_timing;
const char         *driver_default_exe_output = "a.out";
struct obstack      file_obst;
compilation_unit_t *units;

static compilation_unit_t **unit_anchor = &units;

void driver_add_flag(struct obstack *obst, const char *format, ...)
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
		switch (*c) {
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

const char *get_output_name(const char *inputname, const char *newext)
{
	if (inputname == NULL)
		inputname = "a";

	char const       *filename;
	char const *const name_end = find_extension(inputname, &filename);

	assert(obstack_object_size(&file_obst) == 0);
	obstack_grow(&file_obst, filename, name_end-filename);
	size_t extlen = strlen(newext);
	obstack_grow(&file_obst, newext, extlen);
	return obstack_nul_finish(&file_obst);
}

FILE *open_temp_file(const char *basename, const char *extension,
                     const char **final_name)
{
	char uniquenum_extension[32];
	static unsigned nextnum = 0;
	snprintf(uniquenum_extension, sizeof(uniquenum_extension), "-%u%s",
	         nextnum++, extension);
	const char *tmpname = get_output_name(basename, uniquenum_extension);
	return make_temp_file(tmpname, final_name);
}

bool open_input(compilation_unit_t *unit)
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
			position_t const pos = { inputname, 0, 0, 0 };
			errorf(&pos, "could not open: %s", strerror(errno));
			return false;
		}
	}
	return true;
}

bool close_input(compilation_unit_t *unit)
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

bool open_output(compilation_env_t *env)
{
	const char *outname = env->outname;
	FILE       *out;
	if (outname == NULL || streq(outname, "-")) {
		out = stdout;
	} else {
		out = fopen(outname, "w");
		if (out == NULL) {
			position_t const pos = { outname, 0, 0, 0 };
			errorf(&pos, "could not open for writing: %s", strerror(errno));
			return false;
		}
	}
	env->out = out;
	return true;
}

bool open_output_for_unit(compilation_env_t *env, compilation_unit_t *unit,
                          const char *default_extension)
{
	if (env->outname == NULL) {
		env->outname = get_output_name(unit->original_name, default_extension);
	}
	return open_output(env);
}

void close_output(compilation_env_t *env)
{
	FILE *out = env->out;
	if (out != stdout) {
		fclose(out);
	}
}

void copy_file(FILE *dest, FILE *input)
{
	char buf[16384];

	for (;;) {
		size_t bytes_read = fread(buf, 1, sizeof(buf), input);
		if (bytes_read == 0)
			break;
		if (fwrite(buf, 1, bytes_read, dest) != bytes_read) {
			perror("could not write output");
		}
	}
}

bool do_copy_file(compilation_env_t *env, compilation_unit_t *unit)
{
	if (!open_input(unit))
		return false;
	if (!open_output(env))
		return false;
	copy_file(env->out, unit->input);
	close_output(env);
	return close_input(unit);
}

static compilation_unit_handler handlers[COMPILATION_UNIT_LAST+1];
static bool                     stop_after[COMPILATION_UNIT_LAST+1];

void set_unit_handler(compilation_unit_type_t type,
                      compilation_unit_handler handler, bool stop_after_val)
{
	assert(type <= COMPILATION_UNIT_LAST);
	handlers[type] = handler;
	stop_after[type] = stop_after_val;
}

bool process_unit(compilation_env_t *env, compilation_unit_t *unit)
{
	bool res;
	for (;;) {
		compilation_unit_type_t type = unit->type;
		compilation_unit_handler handler = handlers[type];
		if (!handler)
			panic("incomplete handler chain on '%s'", unit->name);
		if (!handler(env, unit)) {
			res = false;
			break;
		} else
		if (stop_after[type]) {
			res = true;
			break;
		}
		assert(unit->type != type); /* handler should have changed the type */
	}
	print_diagnostic_summary();
	return res;
}

bool process_all_units(compilation_env_t *env)
{
	if (units == NULL) {
		errorf(NULL, "no input files specified");
		return false;
	}

	for (compilation_unit_t *unit = units; unit != NULL; unit = unit->next) {
		if (unit->type == COMPILATION_UNIT_AUTODETECT)
			unit->type = autodetect_input(unit->name);

		stat_ev_ctx_push_str("compilation_unit", unit->name);
		bool ok = process_unit(env, unit);
		stat_ev_ctx_pop("compilation_unit");
		if (!ok) {
			return false;
		}
	}
	return true;
}

void begin_statistics(void)
{
	if (produce_statev) {
		/* attempt to guess a good name for the file */
		const char *first_cup = units->name;
		if (first_cup != NULL) {
			char const *const pos = find_extension(first_cup, NULL);
			size_t      const len = pos - first_cup;
			char              buf[len + 1];
			memcpy(buf, first_cup, len);
			buf[len] = '\0';

			stat_ev_begin(buf, filtev);
		}
	}
}

void end_statistics(void)
{
	if (stat_ev_enabled)
		stat_ev_end();
}

void driver_add_input(const char *filename, compilation_unit_type_t type)
{
	compilation_unit_t *entry = OALLOCZ(&file_obst, compilation_unit_t);
	entry->name          = filename;
	entry->original_name = filename;
	entry->type          = type;

	*unit_anchor = entry;
	unit_anchor  = &entry->next;
}

compilation_unit_type_t autodetect_input(char const *const path)
{
	char const *name;
	char const *suffix = find_extension(path, &name);
	/* Ensure there is at least one char before the suffix */
	if (suffix == name)
		return COMPILATION_UNIT_OBJECT;
	++suffix;
	return
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
		COMPILATION_UNIT_OBJECT; /* gcc behavior: unknown file extension means
		                            object file */
}

static int detect_color_terminal(void)
{
	/* we want to avoid bloated linking against termcap/ncurses, so we use a
	 * simple detection heuristic (similar to one git uses) */
	if (!isatty(STDOUT_FILENO) || !isatty(STDERR_FILENO))
		return 0;

	char *term = getenv("TERM");
	if (term == NULL || streq(term, "dumb"))
		return 0;
	if (strstr(term, "256color") != 0)
		return 256;
	return 8;
}

void init_driver(void)
{
	obstack_init(&file_obst);

	colorterm = detect_color_terminal();
	diagnostic_enable_color(colorterm);
}

void exit_driver(void)
{
	obstack_free(&file_obst, NULL);
}
