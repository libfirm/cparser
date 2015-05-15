/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include <stdbool.h>

#include "ast/position.h"
#include "warning.h"

/**
 * Issue a diagnostic message.
 * Format types:
 *  %E   expression_t const*
 *  %K   token_t const*
 *  %k   token_kind_t
 *  %lk  va_list*
 *  %N   entity_t const*
 *  %#N  entity_t const*
 *  %Q   unsigned (qualifier)
 *  %S   string_t const*
 *  %T   type_t const*
 *  %Y   symbol_t const*
 */
void errorf(const position_t *pos, const char *fmt, ...);

/**
 * Issue a diagnostic message with the prefix "note:".
 */
void notef(position_t const *pos, char const *fmt, ...);

/**
 * Issue a diagnostic message with the prefix "warning:" and increase the
 * warning count, if the given warning is activated.
 *
 * @return Whether the diagnostic was shown.
 */
bool warningf(warning_t, position_t const *pos, char const *fmt, ...);

extern unsigned error_count;
extern unsigned warning_count;
extern bool     show_column;             /**< Show column in diagnostic messages */
extern bool     diagnostics_show_option; /**< Show the switch, which controls a warning. */

/** enable color output, allowed values for n_cols are 0, 8 and 256 */
void diagnostic_enable_color(int n_cols);

#endif
