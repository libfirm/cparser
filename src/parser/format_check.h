/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef FORMAT_CHECK_H
#define FORMAT_CHECK_H

#include "ast/ast.h"

typedef enum {
	FORMAT_PRINTF,   /**< printf style format */
	FORMAT_SCANF,    /**< scanf style format */
	FORMAT_STRFTIME, /**< strftime time format */
	FORMAT_STRFMON   /**< strfmon monetary format */
} format_kind_t;

void check_format(const call_expression_t *call);

#endif
