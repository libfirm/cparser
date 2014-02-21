#ifndef TEMPFILE_H
#define TEMPFILE_H

#include <stdio.h>

/**
 * Creates temporary files named $name_orig inside a temporary
 * directory created with mkdtemp().
 */
FILE *make_temp_file(const char *name_orig, const char **name_result);

void init_temp_files(void);
void exit_temp_files(void);

#endif
