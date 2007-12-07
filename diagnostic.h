#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include "token_t.h"


void diagnosticf(const char *fmt, ...);
void errorf(source_position_t pos, const char *fmt, ...);
void warningf(source_position_t pos, const char *fmt, ...);

extern bool found_error;

#endif
