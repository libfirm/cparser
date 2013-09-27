/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#include "enable_posix.h"
#include "tempfile.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/array.h"
#include "adt/error.h"
#include "adt/obst.h"

static char         **temp_files;
static struct obstack file_obst;

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

FILE *make_temp_file(const char *prefix, const char **name_result)
{
	const char *tempdir = get_tempdir();
	assert(obstack_object_size(&file_obst) == 0);
	obstack_printf(&file_obst, "%s/%sXXXXXX", tempdir, prefix);
	obstack_1grow(&file_obst, '\0');

	char *name = obstack_finish(&file_obst);
	int fd = mkstemp(name);
	if (fd == -1) {
		fprintf(stderr, "error: could not create temporary file: %s",
		        strerror(errno));
		panic("abort");
	}
	FILE *out = fdopen(fd, "w");
	if (out == NULL) {
		panic("could not open temporary file as FILE*");
	}

	ARR_APP1(char*, temp_files, name);
	*name_result = name;
	return out;
}

void init_temp_files(void)
{
	temp_files = NEW_ARR_F(char*, 0);
	atexit(free_temp_files);

	obstack_init(&file_obst);
}

void free_temp_files(void)
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

	obstack_free(&file_obst, NULL);
}
