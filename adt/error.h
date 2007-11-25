// placeholder file...
#include <stdio.h>
#include <stdlib.h>
#include "config.h"

static inline NORETURN
void panic(const char *msg)
{ fprintf(stderr, "Panic: %s\n", msg); abort(); }
