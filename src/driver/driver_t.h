/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#ifndef DRIVER_T_H
#define DRIVER_T_H

#include <stdbool.h>
#include <stdio.h>

#include "c_driver.h"
#include "driver.h"

struct compilation_unit_t {
	const char             *name;  /**< filename or "-" for stdin */
	FILE                   *input; /**< input (NULL if not opened yet) */
	bool                    input_is_pipe;
	const char             *original_name;
	compilation_unit_type_t type;
	lang_standard_t         standard;
	translation_unit_t     *ast;
	compilation_unit_t     *next;
};

struct compilation_env_t {
	FILE       *out;
	const char *outname;
};

#endif
