/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef LANG_FEATURES_H
#define LANG_FEATURES_H

#include <stdbool.h>
#include <libfirm/irmode.h>

#include "adt/util.h"
#include "ast/type.h"
#include "machine_triple.h"

#define BITS_PER_BYTE    8

typedef enum lang_features_t {
	_C89  = 1U << 0,
	_C99  = 1U << 1,
	_C11  = 1U << 2,
	_CXX  = 1U << 3,
	_GNUC = 1U << 4,
	_MS   = 1U << 5,
	_ALL  = 0xFF
} lang_features_t;

typedef struct c_dialect_t {
	atomic_type_kind_t wchar_atomic_kind;
	atomic_type_kind_t pointer_sized_int;
	atomic_type_kind_t pointer_sized_uint;
	bool               freestanding   : 1;
	bool               char_is_signed : 1;
	bool               strict         : 1;
	bool               c89            : 1;
	bool               c99            : 1;
	bool               c11            : 1;
	bool               cpp            : 1;
	bool               gnu            : 1;
	bool               ms             : 1;
	bool               long_double_x87_80bit_float : 1;
	unsigned char      long_double_size;
	unsigned char      long_long_and_double_struct_align;
	unsigned char      long_long_size;
	unsigned char      int_size;
	unsigned char      pointer_size;
	unsigned char      long_size;
	lang_features_t    features;
} c_dialect_t;

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
	unsigned int modulo_shift;
	float_int_conversion_overflow_style_t float_int_overflow;
	const char       *user_label_prefix;
	unsigned char     biggest_alignment;
	/** position independent code generation mode */
	int               pic_mode : 4;
	bool              pic_no_plt : 1;
	/** byte-order: true = big-endian, false = little-endian */
	bool byte_order_big_endian : 1;
	/** enable hack to add call to __main into the main function (mingw) */
	bool enable_main_collect2_hack : 1;
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

extern c_dialect_t dialect;
extern target_t    target;

#endif
