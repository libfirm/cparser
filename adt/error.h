// placeholder file...
#include <stdio.h>

void abort(void);

static inline __attribute__((noreturn))
void panic(const char *msg)
{ fprintf(stderr, "Panic: %s\n", msg); abort(); }
