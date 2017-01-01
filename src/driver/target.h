/*
 * This file is part of cparser.
 * Copyright (C) 2014 Matthias Braun <matze@braunis.de>
 */
#ifndef TARGET_H
#define TARGET_H

#include <stdbool.h>
#include <libfirm/irmode.h>
#include <libfirm/target.h>

#include "adt/util.h"
#include "firm/firm_opt.h"
#include "options.h"

typedef struct target_t {
	char          user_label_prefix;
	unsigned char biggest_alignment;
	bool pic                   : 1;
	bool set_pic               : 1;
	bool pic_noplt             : 1;
	bool set_noplt             : 1;
	bool byte_order_big_endian : 1;
	bool set_use_frame_pointer : 1;
	bool use_frame_pointer     : 1;
	/** parsed machine-triple of target machine. Try not to use this if possible
	 * but create specific variables for language/target features instead. */
	ir_machine_triple_t *machine;
	/** target triple as a string */
	const char *triple;
} target_t;

extern target_t target;

void init_firm_target(void);
bool target_setup(void);

void target_adjust_types_and_dialect(void);

void set_target_option(char const *arg);

typedef struct codegen_option_t codegen_option_t;
struct codegen_option_t {
	codegen_option_t *next;
	char              option[];
};

extern optimization_level_t opt_level;
extern codegen_option_t    *codegen_options;
extern codegen_option_t   **codegen_options_anchor;
extern bool                 profile_generate;
extern bool                 profile_use;
extern const char          *multilib_directory_target_triple;
extern unsigned             target_size_override;
extern bool                 set_wchar;
extern bool                 short_wchar;
extern bool                 unsigned_char;

#endif
