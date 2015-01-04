/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#include "predefs.h"

#include <stdarg.h>
#include <string.h>

#include "adt/error.h"
#include "adt/strutil.h"
#include "ast/constfoldbits.h"
#include "ast/type_t.h"
#include "ast/types.h"
#include "firm/ast2firm.h"
#include "firm/firm_opt.h"
#include "lang_features.h"
#include "machine_triple.h"
#include "parser/preprocessor.h"
#include "version.h"

bool predef_optimize;
bool predef_optimize_size;

static void add_define_prop_fmt(const char *name_template, const char *name,
                                const char *value_fmt, ...)
{
	char name_prop[64];
	snprintf(name_prop, sizeof(name_prop), name_template, name);

	va_list ap;
	va_start(ap, value_fmt);
	char value[128];
	vsnprintf(value, sizeof(value), value_fmt, ap);
	add_define(name_prop, value, false);
	va_end(ap);
}

static const char *get_max_string(atomic_type_kind_t akind)
{
	/* float not implemented yet */
	unsigned flags = get_atomic_type_flags(akind);
	assert(flags & ATOMIC_TYPE_FLAG_INTEGER);

	unsigned bits = get_atomic_type_size(akind) * BITS_PER_BYTE;
	if (flags & ATOMIC_TYPE_FLAG_SIGNED)
		--bits;
	switch (bits) {
	case 7:  return "127";
	case 8:  return "255";
	case 15: return "32767";
	case 16: return "65535";
	case 31: return "2147483647";
	case 32: return "4294967295";
	case 63: return "9223372036854775807";
	case 64: return "18446744073709551615";
	}
	panic("unexpected number of bits requested");
}

static const char *get_literal_suffix(atomic_type_kind_t kind)
{
	switch (kind) {
	case ATOMIC_TYPE_BOOL:
	case ATOMIC_TYPE_CHAR:
	case ATOMIC_TYPE_SCHAR:
	case ATOMIC_TYPE_UCHAR:
	case ATOMIC_TYPE_SHORT:
	case ATOMIC_TYPE_USHORT:
	case ATOMIC_TYPE_INT:
	case ATOMIC_TYPE_DOUBLE:
	case ATOMIC_TYPE_WCHAR_T:
		return "";
	case ATOMIC_TYPE_UINT:        return "U";
	case ATOMIC_TYPE_LONG:        return "L";
	case ATOMIC_TYPE_ULONG:       return "UL";
	case ATOMIC_TYPE_LONGLONG:    return "LL";
	case ATOMIC_TYPE_ULONGLONG:   return "ULL";
	case ATOMIC_TYPE_FLOAT:       return "F";
	case ATOMIC_TYPE_LONG_DOUBLE: return "L";
	}
	panic("invalid kind in get_literal_suffix");
}

static void define_type_max(const char *name, atomic_type_kind_t akind)
{
	add_define_prop_fmt("__%s_MAX__", name, "%s%s", get_max_string(akind),
	                    get_literal_suffix(akind));
}

static void define_type_min(const char *name, atomic_type_kind_t akind)
{
	unsigned flags = get_atomic_type_flags(akind);
	if (flags & ATOMIC_TYPE_FLAG_SIGNED) {
		/* float not implemented yet */
		assert(flags & ATOMIC_TYPE_FLAG_INTEGER);
		add_define_prop_fmt("__%s_MIN__", name, "(-__%s_MAX__ - 1)", name);
	} else {
		add_define_prop_fmt("__%s_MIN__", name, "0%s",
		                    get_literal_suffix(akind));
	}
}

static void define_type_type_max(const char *name, atomic_type_kind_t akind)
{
	const char *type = get_atomic_kind_name(akind);
	add_define_prop_fmt("__%s_TYPE__", name, "%s", type);
	define_type_max(name, akind);
}

static void add_define_int(const char *name, int value)
{
	add_define_prop_fmt("%s", name, "%d", value);
}

static void define_sizeof(const char *name, atomic_type_kind_t akind)
{
	int size = get_atomic_type_size(akind);
	add_define_prop_fmt("__SIZEOF_%s__", name, "%d", size);
}

static void define_type_c(const char *name, atomic_type_kind_t akind)
{
	char buf[32];
	const char *suffix = get_literal_suffix(akind);
	const char *val;
	if (suffix[0] != '\0') {
		snprintf(buf, sizeof(buf), "c ## %s", suffix);
		val = buf;
	} else {
		val = "c";
	}
	add_define_macro(name, "c", val, false);
}

static void define_int_n_types(unsigned size, atomic_type_kind_t unsigned_kind,
                               atomic_type_kind_t signed_kind)
{
	char buf[32];

	assert(size == get_atomic_type_size(signed_kind) * BITS_PER_BYTE);
	snprintf(buf, sizeof(buf), "INT%u", size);
	define_type_type_max(buf, signed_kind);
	snprintf(buf, sizeof(buf), "__INT%u_C", size);
	define_type_c(buf, signed_kind);
	snprintf(buf, sizeof(buf), "INT_LEAST%u", size);
	define_type_type_max(buf, signed_kind);
	snprintf(buf, sizeof(buf), "INT_FAST%u", size);
	define_type_type_max(buf, signed_kind);

	assert(size == get_atomic_type_size(unsigned_kind) * BITS_PER_BYTE);
	snprintf(buf, sizeof(buf), "UINT%u", size);
	define_type_type_max(buf, unsigned_kind);
	snprintf(buf, sizeof(buf), "__UINT%u_C", size);
	define_type_c(buf, unsigned_kind);
	snprintf(buf, sizeof(buf), "UINT_LEAST%u", size);
	define_type_type_max(buf, unsigned_kind);
	snprintf(buf, sizeof(buf), "UINT_FAST%u", size);
	define_type_type_max(buf, unsigned_kind);
}

static void define_float_properties(const char *prefix,
                                    atomic_type_kind_t akind)
{
	ir_mode *mode = atomic_modes[akind];
	unsigned d;
	if (get_mode_arithmetic(mode) == irma_ieee754
	    && get_mode_exponent_size(mode) == 8
	    && get_mode_mantissa_size(mode) == 23) {
	    d = 0;
	} else if (get_mode_arithmetic(mode) == irma_ieee754
	           && get_mode_exponent_size(mode) == 11
	           && get_mode_mantissa_size(mode) == 52) {
	    d = 1;
	} else if (get_mode_arithmetic(mode) == irma_x86_extended_float
	           && get_mode_exponent_size(mode) == 15
	           && get_mode_mantissa_size(mode) == 64) {
		d = 2;
	} else if (get_mode_arithmetic(mode) == irma_ieee754
	           && get_mode_exponent_size(mode) == 15
	           && get_mode_mantissa_size(mode) == 112) {
	    d = 3;
	} else {
		panic("unexpected long double mode");
	}

	static const char *const mant_digs[]    = { "24", "53", "64", "113" };
	static const char *const digs[]         = { "6", "15", "18", "33" };
	static const char *const min_exps[]     = { "(-125)", "(-1021)", "(-16381)", "(-16381)" };
	static const char *const min_10_exps[]  = { "(-37)", "(-307)", "(-4931)", "(-4931)" };
	static const char *const decimal_digs[] = { "9", "17", "21", "36" };
	static const char *const max_exps[]     = { "128", "1024", "16384", "16384" };
	static const char *const max_10_exps[]  = { "38", "308", "4932", "4932" };
	static const char *const maxs[]         = {
		"3.40282346638528859812e+38F", "((double)1.79769313486231570815e+308L)",
		"1.18973149535723176502e+4932L",
		"1.189731495357231765085759326628007016E+4932L"};
	static const char *const mins[]         = {
		"1.17549435082228750797e-38F", "((double)2.22507385850720138309e-308L)",
		"3.36210314311209350626e-4932L",
		"3.36210314311209350626267781732175260e-4932L"};
	static const char *const epsilons[]     = {
		"1.19209289550781250000e-7F", "((double)2.22044604925031308085e-16L)",
		"1.08420217248550443401e-19L",
		"1.92592994438723585305597794258492732e-34L" };
	static const char *const denorm_mins[]  = {
		"1.40129846432481707092e-45F", "((double)4.94065645841246544177e-324L)",
		"3.64519953188247460253e-4951L",
		"6.47517511943802511092443895822764655e-4966L" };
	add_define_prop_fmt("__%s_MANT_DIG__",      prefix, "%s", mant_digs[d]);
	add_define_prop_fmt("__%s_DIG__",           prefix, "%s", digs[d]);
	add_define_prop_fmt("__%s_MIN_EXP__",       prefix, "%s", min_exps[d]);
	add_define_prop_fmt("__%s_MIN_10_EXP__",    prefix, "%s", min_10_exps[d]);
	add_define_prop_fmt("__%s_MAX_EXP__",       prefix, "%s", max_exps[d]);
	add_define_prop_fmt("__%s_MAX_10_EXP__",    prefix, "%s", max_10_exps[d]);
	add_define_prop_fmt("__%s_DECIMAL_DIG__",   prefix, "%s", decimal_digs[d]);
	add_define_prop_fmt("__%s_MAX__",           prefix, "%s", maxs[d]);
	add_define_prop_fmt("__%s_MIN__",           prefix, "%s", mins[d]);
	add_define_prop_fmt("__%s_EPSILON__",       prefix, "%s", epsilons[d]);
	add_define_prop_fmt("__%s_DENORM_MIN__",    prefix, "%s", denorm_mins[d]);
	add_define_prop_fmt("__%s_HAS_DENORM__",    prefix, "1");
	add_define_prop_fmt("__%s_HAS_INFINITY__",  prefix, "1");
	add_define_prop_fmt("__%s_HAS_QUIET_NAN__", prefix, "1");
}

void add_predefined_macros(void)
{
	add_define("__STDC__", "1", true);
	/* C99 predefined macros, but defining them for other language standards too
	 * shouldn't hurt */
	add_define("__STDC_HOSTED__", dialect.freestanding ? "0" : "1", true);

	if (dialect.c99)
		add_define("__STDC_VERSION__", "199901L", true);
	if (dialect.cpp)
		add_define("__cplusplus", "1", true);
	if (!dialect.gnu && !dialect.ms && !dialect.cpp)
		add_define("__STRICT_ANSI__", "1", false);

	add_define_string("__VERSION__", CPARSER_VERSION, false);

	/* we are cparser */
	add_define("__CPARSER__",            CPARSER_MAJOR, false);
	add_define("__CPARSER_MINOR__",      CPARSER_MINOR, false);
	add_define("__CPARSER_PATCHLEVEL__", CPARSER_PATCHLEVEL, false);

	/* let's pretend we are a GCC compiler */
	add_define("__GNUC__",            "4", false);
	add_define("__GNUC_MINOR__",      "6", false);
	add_define("__GNUC_PATCHLEVEL__", "0", false);
	if (dialect.cpp)
		add_define("__GNUG__", "4", false);

	if (!firm_is_inlining_enabled())
		add_define("__NO_INLINE__", "1", false);
	if (dialect.c99) {
		add_define("__GNUC_STDC_INLINE__", "1", false);
	} else {
		add_define("__GNUC_GNU_INLINE__", "1", false);
	}

	/* TODO: I'd really like to enable these, but for now they enable some form
	 * of x87 inline assembly in the glibc/linux headers which we don't support
	 * yet */
#if 0
	if (predef_optimize)
		add_define("__OPTIMIZE__", "1", false);
	if (predef_optimize_size)
		add_define("__OPTIMIZE_SIZE__", "1", false);
#endif

	/* no support for the XXX_chk functions in cparser yet */
	add_define("_FORTIFY_SOURCE", "0", false);

	const char *operating_system = target.machine->operating_system;
	if (is_unixish_os(operating_system)) {
		if (dialect.gnu)
			add_define("unix",     "1", false);
		add_define("__unix",   "1", false);
		add_define("__unix__", "1", false);
		add_define("__ELF__",  "1", false);
		if (strstr(operating_system, "linux") != NULL) {
			if (dialect.gnu)
				add_define("linux",     "1", false);
			add_define("__linux",   "1", false);
			add_define("__linux__", "1", false);
			if (strstr(operating_system, "gnu") != NULL) {
				add_define("__gnu_linux__", "1", false);
			}
		}
	} else if (is_darwin_os(operating_system)) {
		add_define("__MACH__",     "1", false);
		add_define("__APPLE__",    "1", false);
		add_define("__APPLE_CC__", "1", false);
		add_define("__weak",       "",  false);
		add_define("__strong",     "",  false);
		/* TODO: when are these disabled? */
		add_define("__CONSTANT_CFSTRINGS__", "1", false);
		add_define("__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__", "1050",
		           false);
		/* TODO: may also be __STATIC__ */
		add_define("__DYNAMIC__",  "1", false);
		/* TODO: _REENTRANT for -pthreads */
		if (!target.byte_order_big_endian) {
			add_define("__LITTLE_ENDIAN__", "1", false);
		}
	}
	add_define("__ORDER_BIG_ENDIAN__",    "4321", false);
	add_define("__ORDER_LITTLE_ENDIAN__", "1234", false);
	add_define("__ORDER_PDP_ENDIAN__",    "3412", false);
	add_define("__BYTE_ORDER__", target.byte_order_big_endian
	           ? "__ORDER_BIG_ENDIAN__" : "__ORDER_LITTLE_ENDIAN__", false);
	add_define("__FLOAT_WORD_ORDER__", target.byte_order_big_endian
	           ? "__ORDER_BIG_ENDIAN__" : "__ORDER_LITTLE_ENDIAN__", false);

	add_define("__FINITE_MATH_ONLY__",    "0",    false);

	if (get_atomic_type_size(ATOMIC_TYPE_LONG) == 8
	    && get_type_size(type_void_ptr) == 8
	    && get_atomic_type_size(ATOMIC_TYPE_INT) == 4) {
		add_define("_LP64",    "1", false);
		add_define("__LP64__", "1", false);
	}

	const char *cpu          = target.machine->cpu_type;
	const char *manufacturer = target.machine->manufacturer;
	if (is_ia32_cpu(cpu)) {
		if (dialect.gnu)
			add_define("i386",     "1", false);
		add_define("__i386",   "1", false);
		add_define("__i386__", "1", false);
		if (streq(cpu, "i486")) {
			add_define("__i486",   "1", false);
			add_define("__i486__", "1", false);
		} else if (streq(cpu, "i586")) {
			add_define("__i586",      "1", false);
			add_define("__i586__",    "1", false);
			add_define("__pentium",   "1", false);
			add_define("__pentium__", "1", false);
			//add_define("__pentium_mmx__", "1", false);
		} else if (streq(cpu, "i686")) {
			add_define("__pentiumpro",   "1", false);
			add_define("__pentiumpro__", "1", false);
			add_define("__i686",         "1", false);
			add_define("__i686__",       "1", false);
		} else if (streq(cpu, "i786")) {
			add_define("__pentium4",     "1", false);
			add_define("__pentium4__",   "1", false);
		}
	} else if (streq(cpu, "sparc")) {
		add_define("sparc",     "1", false);
		add_define("__sparc",   "1", false);
		add_define("__sparc__", "1", false);
		/* we always produce sparc V8 code at the moment */
		add_define("__sparc_v8__", "1", false);
		if (strstr(manufacturer, "leon") != NULL) {
			add_define("__leon__", "1", false);
		}
	} else if (streq(cpu, "arm")) {
		/* TODO: test, what about
		 * ARM_FEATURE_UNALIGNED, ARMEL, ARM_ARCH_7A, ARM_FEATURE_DSP, ... */
		add_define("__arm__",   "1", false);
		if (strstr(operating_system, "eabi") != NULL) {
			add_define("__ARM_EABI__", "1", false);
		}
	} else if (streq(cpu, "x86_64")) {
		add_define("__x86_64",   "1", false);
		add_define("__x86_64__", "1", false);
		add_define("__amd64",    "1", false);
		add_define("__amd64__",  "1", false);
	}
	ir_mode *float_mode = be_get_mode_float_arithmetic();
	const char *flt_eval_metod
		= float_mode == NULL ? "0"
		: get_mode_size_bytes(float_mode) > get_type_size(type_double)  ? "2"
		: get_mode_size_bytes(float_mode) == get_type_size(type_double) ? "1"
		: "-1";
	add_define("__FLT_EVAL_METHOD__", flt_eval_metod, false);

	if (target.pic_mode != 0) {
		add_define_int("__PIC__", target.pic_mode);
		add_define_int("__pic__", target.pic_mode);
	}

	/* TODO: query from backend? */
	add_define("__USER_LABEL_PREFIX__", target.user_label_prefix, false);
	add_define("__REGISTER_PREFIX__", "", false);
	/* TODO: GCC_HAVE_SYNC_COMPARE_AND_SWAP_XX */

	atomic_type_properties_t *props = atomic_type_properties;
	if (!(props[ATOMIC_TYPE_CHAR].flags & ATOMIC_TYPE_FLAG_SIGNED)) {
		add_define("__CHAR_UNSIGNED__", "1", false);
	}
	if (!(props[ATOMIC_TYPE_WCHAR_T].flags & ATOMIC_TYPE_FLAG_SIGNED)) {
		add_define("__WCHAR_UNSIGNED__", "1", false);
	}
	add_define_int("__CHAR_BIT__", BITS_PER_BYTE);

	assert(type_size_t->kind    == TYPE_ATOMIC);
	assert(type_wint_t->kind    == TYPE_ATOMIC);
	assert(type_ptrdiff_t->kind == TYPE_ATOMIC);

	define_sizeof("SHORT",       ATOMIC_TYPE_SHORT);
	define_sizeof("INT",         ATOMIC_TYPE_INT);
	define_sizeof("LONG",        ATOMIC_TYPE_LONG);
	define_sizeof("LONG_LONG",   ATOMIC_TYPE_LONGLONG);
	define_sizeof("FLOAT",       ATOMIC_TYPE_FLOAT);
	define_sizeof("DOUBLE",      ATOMIC_TYPE_DOUBLE);
	define_sizeof("LONG_DOUBLE", ATOMIC_TYPE_LONG_DOUBLE);
	define_sizeof("SIZE_T",      type_size_t->atomic.akind);
	define_sizeof("WCHAR_T",     type_wchar_t->atomic.akind);
	define_sizeof("WINT_T",      type_wint_t->atomic.akind);
	define_sizeof("PTRDIFF_T",   type_ptrdiff_t->atomic.akind);
	add_define_int("__SIZEOF_POINTER__", get_type_size(type_void_ptr));

	define_type_max("SCHAR",     ATOMIC_TYPE_SCHAR);
	define_type_max("SHRT",      ATOMIC_TYPE_SHORT);
	define_type_max("INT",       ATOMIC_TYPE_INT);
	define_type_max("LONG",      ATOMIC_TYPE_LONG);
	define_type_max("LONG_LONG", ATOMIC_TYPE_LONGLONG);

	define_type_type_max("WCHAR",   type_wchar_t->atomic.akind);
	define_type_min(     "WCHAR",   type_wchar_t->atomic.akind);
	define_type_type_max("SIZE",    type_size_t->atomic.akind);
	define_type_type_max("WINT",    type_wint_t->atomic.akind);
	define_type_min(     "WINT",    type_wint_t->atomic.akind);
	define_type_type_max("PTRDIFF", type_ptrdiff_t->atomic.akind);

	/* TODO: what to choose here... */
	define_type_type_max("SIG_ATOMIC", ATOMIC_TYPE_INT);
	define_type_min(     "SIG_ATOMIC", ATOMIC_TYPE_INT);

	define_int_n_types(8,  ATOMIC_TYPE_UCHAR,  ATOMIC_TYPE_SCHAR);
	define_int_n_types(16, ATOMIC_TYPE_USHORT, ATOMIC_TYPE_SHORT);
	define_int_n_types(32, ATOMIC_TYPE_UINT,   ATOMIC_TYPE_INT);
	atomic_type_kind_t akind_uintptr;
	atomic_type_kind_t akind_intptr;
	if (get_type_size(type_void_ptr) == 4 &&
		get_atomic_type_size(ATOMIC_TYPE_INT) == 4) {
		akind_intptr = ATOMIC_TYPE_INT;
		akind_uintptr = ATOMIC_TYPE_UINT;
	} else if (get_type_size(type_void_ptr) == 8 &&
	           get_atomic_type_size(ATOMIC_TYPE_LONG) == 8) {
		akind_intptr = ATOMIC_TYPE_LONG;
		akind_uintptr = ATOMIC_TYPE_ULONG;
	} else if (get_type_size(type_void_ptr) == 8 &&
			   get_atomic_type_size(ATOMIC_TYPE_LONGLONG) == 8) {
		akind_intptr = ATOMIC_TYPE_LONGLONG;
		akind_uintptr = ATOMIC_TYPE_ULONGLONG;
	} else {
		panic("Couldn't determine uintptr type for target");
	}
	define_type_type_max("UINTPTR", akind_uintptr);
	define_type_type_max("INTPTR",  akind_intptr);
	if (props[ATOMIC_TYPE_LONG].size == 8) {
		define_int_n_types(64, ATOMIC_TYPE_ULONG, ATOMIC_TYPE_LONG);
	} else if (props[ATOMIC_TYPE_LONGLONG].size == 8) {
		define_int_n_types(64, ATOMIC_TYPE_ULONGLONG, ATOMIC_TYPE_LONGLONG);
	}

	unsigned           intmax_size  = 0;
	atomic_type_kind_t intmax_kind  = ATOMIC_TYPE_FIRST;
	unsigned           uintmax_size = 0;
	atomic_type_kind_t uintmax_kind = ATOMIC_TYPE_FIRST;
	for (atomic_type_kind_t i = ATOMIC_TYPE_FIRST; i <= ATOMIC_TYPE_LAST; ++i) {
		unsigned flags = get_atomic_type_flags(i);
		if (!(flags & ATOMIC_TYPE_FLAG_INTEGER))
			continue;
		unsigned size = get_atomic_type_size(i);
		if (flags & ATOMIC_TYPE_FLAG_SIGNED) {
			if (size > intmax_size) {
				intmax_kind = i;
				intmax_size = size;
			}
		} else {
			if (size > uintmax_size) {
				uintmax_kind = i;
				uintmax_size = size;
			}
		}
	}
	define_type_type_max("UINTMAX", uintmax_kind);
	define_type_c("__UINTMAX_C", uintmax_kind);
	define_type_type_max("INTMAX",  intmax_kind);
	define_type_c("__INTMAX_C", intmax_kind);

	/* TODO: less hardcoding for the following... */
	define_float_properties("FLT",  ATOMIC_TYPE_FLOAT);
	define_float_properties("DBL",  ATOMIC_TYPE_DOUBLE);
	define_float_properties("LDBL", ATOMIC_TYPE_LONG_DOUBLE);
	add_define("__FLT_RADIX__",   "2",                    false);
	add_define("__DECIMAL_DIG__", "__LDBL_DECIMAL_DIG__", false);

	/* TODO: __CHAR16_TYPE__, __CHAR32_TYPE__ */
	/* TODO: query this from backend? (if we just look for all alignments in
	 *       our atomic types it's usually < 16... */
	add_define_int("__BIGGEST_ALIGNMENT__", 16);
}
