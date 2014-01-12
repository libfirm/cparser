#ifndef TEMPFILE_H
#define TEMPFILE_H

#include <stdio.h>

/**
 * custom version of tmpnam, which: writes to an obstack, emits no warnings
 * during linking (like glibc/gnu ld do for tmpnam)...
 */
FILE *make_temp_file(const char *suffix, const char **name_result);

void init_temp_files(void);
void free_temp_files(void);

#endif
