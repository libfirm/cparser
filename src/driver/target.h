/*
 * This file is part of cparser.
 * Copyright (C) 2014 Matthias Braun <matze@braunis.de>
 */
#ifndef TARGET_H
#define TARGET_H

#include <stdbool.h>
#include <libfirm/irmode.h>

#include "adt/util.h"
#include "machine_triple.h"
#include "firm/firm_opt.h"
#include "options.h"

typedef enum object_format_t {
	OBJECT_FORMAT_ELF,
	OBJECT_FORMAT_MACH_O,
	OBJECT_FORMAT_PE_COFF,
} object_format_t;

/**
 * Name+Value of a target specific preprocessor define. This is necessary to
 * avoid doing target specific decisions outside of target.c
 */
typedef struct target_define_t target_define_t;
struct target_define_t {
	char      const *name;
	char      const *value;
	target_define_t *next;
	bool (*condition)(void);
};

typedef struct target_t {
	/**
	 * whether architecture shift instructions usually perform modulo bit_size
	 * on the shift amount, if yes this equals to the machine_size.
	 */
	unsigned int      modulo_shift;
	float_int_conversion_overflow_style_t float_int_overflow;
	char              user_label_prefix;
	unsigned char     biggest_alignment;
	/** position independent code generation mode */
	int               pic_mode : 4;
	bool              pic_no_plt : 1;
	/** byte-order: true = big-endian, false = little-endian */
	bool byte_order_big_endian : 1;
	/** firm_isa was explicitely specified on the commandline */
	bool firm_isa_specified    : 1;
	ENUMBF(object_format_t) object_format : 2;
	target_define_t  *defines;
	const char       *firm_isa;
	const char       *firm_arch;
	/** parsed machine-triple of target machine. Try not to use this if possible
	 * but create specific variables for language/target features instead. */
	machine_triple_t *machine;
	/** target triple as a string */
	const char       *triple;
} target_t;

extern target_t target;

void target_set_defaults(void);
bool target_setup(void);
void warn_experimental_target(void);

void target_adjust_types_and_dialect(void);

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

#endif
