/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include <stdbool.h>
#include "token_t.h"
#include "warning.h"

/* define a NORETURN attribute */
#ifndef NORETURN
# if defined(__GNUC__)
#  if __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 70)
#   define NORETURN void __attribute__ ((noreturn))
#  endif /* __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 70) */
# endif /* defined(__GNUC__) */

# if defined(_MSC_VER)
#  define NORETURN void __declspec(noreturn)
# endif /* defined(_MSC_VER) */

/* If not set above, use "void" for DOES_NOT_RETURN. */
# ifndef NORETURN
# define NORETURN void
# endif /* ifndef NORETURN */
#endif /* ifndef NORETURN */

/**
 * Issue a diagnostic message.
 * Format types:
 *  %E   expression_t const*
 *  %K   token_t const*
 *  %k   token_kind_t
 *  %#k  va_list*, char const*
 *  %N   entity_t const*
 *  %#N  entity_t const*
 *  %P   position_t const*
 *  %Q   unsigned (qualifier)
 *  %S   string_t const*
 *  %T   type_t const*
 *  %#T  type_t const*, symbol_t const*
 *  %Y   symbol_t const*
 */
void diagnosticf(const char *fmt, ...);
void errorf(const position_t *pos, const char *fmt, ...);
void warningf(warning_t, const position_t *pos, const char *fmt, ...);
NORETURN internal_errorf(const position_t *pos, const char *fmt, ...);

extern unsigned error_count;
extern unsigned warning_count;
extern bool     show_column;             /**< Show column in diagnostic messages */
extern bool     diagnostics_show_option; /**< Show the switch, which controls a warning. */

#endif
