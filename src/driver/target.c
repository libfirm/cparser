/*
 * This file is part of cparser.
 * Copyright (C) 2014 Matthias Braun <matze@braunis.de>
 */
#include <libfirm/be.h>
#include <libfirm/firm.h>

#include "adt/panic.h"
#include "adt/strutil.h"
#include "adt/util.h"
#include "ast/dialect.h"
#include "ast/type_t.h"
#include "ast/types.h"
#include "c_driver.h"
#include "diagnostic.h"
#include "firm/ast2firm.h"
#include "firm/firm_opt.h"
#include "firm/mangle.h"
#include "target.h"
#include "warning.h"

target_t target = {
	.biggest_alignment = 16,
	.pic_mode          = -1,
	.user_label_prefix = "",
};
const char *multilib_directory_target_triple;
unsigned target_size_override;

void target_adjust_types_and_dialect(void)
{
	init_types(dialect.int_size, dialect.long_size, dialect.pointer_size);

	atomic_type_properties_t *props = atomic_type_properties;

	/* Adjustments for some systems */
	props[ATOMIC_TYPE_LONGLONG].size             = dialect.long_long_size;
	props[ATOMIC_TYPE_LONGLONG].alignment        = dialect.long_long_size;
	props[ATOMIC_TYPE_LONGLONG].struct_alignment = dialect.long_long_size;
	props[ATOMIC_TYPE_ULONGLONG].size             = dialect.long_long_size;
	props[ATOMIC_TYPE_ULONGLONG].alignment        = dialect.long_long_size;
	props[ATOMIC_TYPE_ULONGLONG].struct_alignment = dialect.long_long_size;

	unsigned const ll_d_struct_align
		= dialect.long_long_and_double_struct_align;
	if (ll_d_struct_align > 0) {
		props[ATOMIC_TYPE_LONGLONG].struct_alignment  = ll_d_struct_align;
		props[ATOMIC_TYPE_ULONGLONG].struct_alignment = ll_d_struct_align;
		props[ATOMIC_TYPE_DOUBLE].struct_alignment    = ll_d_struct_align;
	}

	unsigned const size = dialect.long_double_size;
	unsigned const alignment = size == 12 && dialect.long_double_x87_80bit_float
	                         ? 4 : size;
	props[ATOMIC_TYPE_LONG_DOUBLE].size             = size;
	props[ATOMIC_TYPE_LONG_DOUBLE].alignment        = alignment;
	props[ATOMIC_TYPE_LONG_DOUBLE].struct_alignment = alignment;

	/* stuff decided after processing operating system specifics and
	 * commandline flags */
	if (dialect.char_is_signed) {
		props[ATOMIC_TYPE_CHAR].flags |= ATOMIC_TYPE_FLAG_SIGNED;
	} else {
		props[ATOMIC_TYPE_CHAR].flags &= ~ATOMIC_TYPE_FLAG_SIGNED;
	}
	/* copy over wchar_t properties (including rank) */
	props[ATOMIC_TYPE_WCHAR_T] = props[dialect.wchar_atomic_kind];

	static bool had_cpp_warning;
	if (dialect.cpp && !had_cpp_warning) {
		warningf(WARN_EXPERIMENTAL, NULL,
		         "C++ support is highly experimental and unfinished");
		had_cpp_warning = true;
	}

	if (target.firm_isa_specified) {
		backend_params const *const p = be_get_backend_param();
		target.float_int_overflow    = p->float_int_overflow;
		target.byte_order_big_endian = p->byte_order_big_endian;
		dialect.pointer_size         = p->machine_size / BITS_PER_BYTE;
	} else {
		/* The frontend should do all decisions and should not be influenced by
		 * outside influences like the firm backend. So we just check here that
		 * our decisions match the firm backend. */
		assert(be_get_backend_param()->machine_size % BITS_PER_BYTE == 0);
		assert(dialect.pointer_size
		       == be_get_backend_param()->machine_size / BITS_PER_BYTE);
		assert(target.byte_order_big_endian
		       == be_get_backend_param()->byte_order_big_endian);
		assert(target.float_int_overflow
		       == be_get_backend_param()->float_int_overflow);
	}
}

static ident *compilerlib_name_mangle(ident *id, ir_type *mt)
{
	(void)mt;
	char const *const prefix = target.user_label_prefix;
	if (*prefix == '\0')
		return id; /* Shortcut for empty prefix */
	return new_id_fmt("%s%s", prefix, id);
}

/** Add a target specific preprocessor define. */
static target_define_t *ppdef(const char *name, const char *value)
{
	target_define_t *define = XMALLOCZ(target_define_t);
	define->name = name;
	define->value = value;
	define->next = target.defines;
	target.defines = define;
	return define;
}

/** Add a target specific preprocessor define. This calls \p condition_func
 * to check whether the define should really be added. */
static target_define_t *ppdefc(const char *name, const char *value,
                               bool (*condition_func)(void))
{
	target_define_t *define = ppdef(name, value);
	define->condition = condition_func;
	return define;
}

static bool cond_not_strict(void)
{
	return dialect.gnu;
}

static bool cond_is_little_endian(void)
{
	return !target.byte_order_big_endian;
}

static void init_generic_elf(void)
{
	driver_default_exe_output = "a.out";
	set_create_ld_ident(create_name_elf);
	target.object_format = OBJECT_FORMAT_ELF;
	set_be_option("ia32-struct_in_reg=no");
}

static void init_unix(void)
{
	ppdef( "__unix",    "1");
	ppdef( "__unix__",  "1");
	ppdefc("unix",      "1", cond_not_strict);
}

static bool is_ia32_cpu(const char *cpu)
{
	/* i386, i486, i586, i686, i786 */
	return cpu[0] == 'i' && cpu[2] == '8' && cpu[3] == '6'
	    && cpu[1] >= '3' && cpu[1] <= '7';
}

static bool is_amd64_cpu(char const *const cpu)
{
	return streq(cpu, "x86_64") || streq(cpu, "amd64");
}

static void set_options_for_machine(machine_triple_t const *const machine)
{
	/* Note: Code here should only check the target triple! Querying other
	 * target features is not allowed as subsequent commandline options may
	 * change those. Example:
	 * ppdefc("X", "Y", cond_not_strict); // Correct: cond_not_strict is
	 *                                    // evaluated later
	 * if (dialect.gnu)
	 *    ppdef("X", "Y"); // Wrong: language dialect/target is not final yet
	 */
	const char *const cpu          = machine->cpu_type;
	const char *const manufacturer = machine->manufacturer;
	const char *const os           = machine->operating_system;
	unsigned                              pointer_size;
	unsigned                              long_double_size;
	unsigned                              modulo_shift;
	float_int_conversion_overflow_style_t float_int_overflow;
	const char                           *firm_isa;
	if (target.firm_isa_specified) {
		/* Firm ISA was specified on the commandline, this may be used to
		 * experiment with new firm backends. */
		firm_isa = target.firm_isa;
		unsigned size = target_size_override != 0
		              ? target_size_override/BITS_PER_BYTE : 4;
		pointer_size       = size;
		long_double_size   = 8;
		modulo_shift       = size * BITS_PER_BYTE;
		float_int_overflow = ir_overflow_indefinite;

	/* i386, i486, i586, i686, i786 */
	} else if (is_ia32_cpu(cpu)) {
		ppdefc("i386",     "1", cond_not_strict);
		ppdef( "__i386",   "1");
		ppdef( "__i386__", "1");
		switch (cpu[1]) {
		case '4':
			ppdef("__i486",   "1");
			ppdef("__i486__", "1");
			break;
		case '5':
			ppdef("__i586",      "1");
			ppdef("__i586__",    "1");
			ppdef("__pentium",   "1");
			ppdef("__pentium__", "1");
			//ppdef("__pentium_mmx__", "1");
			break;
		case '6':
			ppdef("__pentiumpro",   "1");
			ppdef("__pentiumpro__", "1");
			ppdef("__i686",         "1");
			ppdef("__i686__",       "1");
			break;
		case '7':
			ppdef("__pentium4",     "1");
			ppdef("__pentium4__",   "1");
			break;
		}
		firm_isa           = "ia32";
		pointer_size       = 4;
		modulo_shift       = 32;
		long_double_size   = 12;
		float_int_overflow = ir_overflow_indefinite;
		target.firm_arch   = cpu;
		/* long long and double has a 4 byte alignment inside structs, this odd
		 * mode is everywhere except for windows OSes (they will revert it
		 * below) */
		dialect.long_long_and_double_struct_align = 4;
		dialect.long_double_x87_80bit_float       = true;
	} else if (streq(cpu, "sparc")) {
		ppdefc("sparc",     "1", cond_not_strict);
		ppdef( "__sparc",   "1");
		ppdef( "__sparc__", "1");
		/* we always produce sparc V8 code at the moment */
		ppdef( "__sparc_v8__", "1");
		if (strstr(manufacturer, "leon") != NULL
		 || streq(manufacturer, "invasic")) {
			ppdef("__leon__", "1");
			set_be_option("sparc-cpu=leon");
		}
		firm_isa           = "sparc";
		pointer_size       = 4;
		modulo_shift       = 32;
		long_double_size   = 16;
		float_int_overflow = ir_overflow_min_max;
		target.byte_order_big_endian = true;
	} else if (streq(cpu, "arm")) {
		/* TODO: test, what about
		 * ARM_FEATURE_UNALIGNED, ARMEL, ARM_ARCH_7A, ARM_FEATURE_DSP, ... */
		ppdef("__arm__",   "1");
		if (strstr(os, "eabi") != NULL)
			ppdef("__ARM_EABI__", "1");
		firm_isa           = "arm";
		pointer_size       = 4;
		modulo_shift       = 256;
		long_double_size   = 8;
		float_int_overflow = ir_overflow_min_max;
	} else if (is_amd64_cpu(cpu)) {
		ppdef("__x86_64",   "1");
		ppdef("__x86_64__", "1");
		ppdef("__amd64",    "1");
		ppdef("__amd64__",  "1");
		firm_isa           = "amd64";
		pointer_size       = 8;
		modulo_shift       = 32;
		long_double_size   = 16;
		float_int_overflow = ir_overflow_indefinite;
		dialect.long_double_x87_80bit_float = true;
	} else if (streq(cpu, "mips")) {
		ppdef("__mips__", "1");
		firm_isa           = "mips";
		pointer_size       =  4;
		modulo_shift       = 32;
		long_double_size   =  8;
		float_int_overflow = ir_overflow_indefinite;
		target.byte_order_big_endian = true;
	} else {
		errorf(NULL, "unknown cpu '%s' in target-triple", cpu);
		exit(EXIT_FAILURE);
	}

	target.firm_isa            = firm_isa;
	target.modulo_shift        = modulo_shift;
	target.float_int_overflow  = float_int_overflow;
	dialect.pointer_size       = pointer_size;
	dialect.int_size           = MIN(pointer_size, 4);
	dialect.long_size          = MIN(pointer_size, 8);
	dialect.long_double_size   = long_double_size;
	dialect.wchar_atomic_kind  = ATOMIC_TYPE_INT;
	dialect.pointer_sized_int  = ATOMIC_TYPE_LONG;
	dialect.pointer_sized_uint = ATOMIC_TYPE_ULONG;

	set_compilerlib_name_mangle(compilerlib_name_mangle);

	if (strstr(os, "linux") != NULL) {
		init_generic_elf();
		init_unix();
		ppdef( "__linux",   "1");
		ppdef( "__linux__", "1");
		ppdefc("linux",     "1", cond_not_strict);
		if (strstr(os, "gnu") != NULL)
			ppdef("__gnu_linux__", "1");
	} else if (strstart(os, "freebsd")) {
		ppdef( "__FreeBSD__", "");
		goto bsd;
	} else if (strstr(os, "bsd") != NULL) {
bsd:
		init_generic_elf();
		init_unix();
		set_be_option("ia32-struct_in_reg=yes");
	} else if (streq(os, "elf") || streq(os, "octopos") || streq(os, "irtss")) {
		init_generic_elf();
	} else if (strstart(os, "darwin")) {
		driver_default_exe_output = "a.out";
		set_create_ld_ident(create_name_macho);
		target.user_label_prefix = "_";
		target.object_format = OBJECT_FORMAT_MACH_O;
		target.pic_mode = 2;
		set_be_option("ia32-stackalign=4");
		set_be_option("ia32-struct_in_reg=yes");
		dialect.long_double_size = 16;
		ppdef( "__MACH__",               "1");
		ppdef( "__APPLE__",              "1");
		ppdef( "__APPLE_CC__",           "1");
		ppdef( "__weak",                 "");
		ppdef( "__strong",               "");
		ppdef( "__CONSTANT_CFSTRINGS__", "1");
		ppdef( "__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__", "1050");
		ppdef( "__DYNAMIC__",            "1");
		ppdefc("__LITTLE_ENDIAN__",      "1", cond_is_little_endian);
	} else if (strstart(os, "mingw")) {
		dialect.wchar_atomic_kind = ATOMIC_TYPE_USHORT;
		driver_default_exe_output = "a.exe";
		target.object_format = OBJECT_FORMAT_PE_COFF;
		set_be_option("ia32-struct_in_reg=no");
		dialect.enable_main_collect2_hack = true;
		ppdef("__MINGW32__", "1");
		dialect.long_long_and_double_struct_align = 0;
		ppdef( "__MSVCRT__", "1");
		ppdef( "_WINNT",     "1");
		ppdef( "__WINNT",    "1");
		ppdef( "__WINNT__",  "1");
		ppdefc("WINNT",      "1", cond_not_strict);
		ppdef( "_WIN32",     "1");
		ppdef( "__WIN32",    "1");
		ppdef( "__WIN32__",  "1");
		ppdefc("WIN32",      "1", cond_not_strict);
		if (pointer_size == 8) {
			set_be_option("amd64-x64abi=yes");
			set_create_ld_ident(create_name_win64);
			ppdef( "_WIN64",    "1");
			ppdef( "__WIN64",   "1");
			ppdef( "__WIN64__", "1");
			ppdefc("WIN64",     "1", cond_not_strict);
			ppdef( "__MINGW64__", "1");
			/* to ease porting of old c-code microsoft decided to use 32bits
			 * even for long */
			dialect.long_size = 4;
			dialect.pointer_sized_int  = ATOMIC_TYPE_LONGLONG;
			dialect.pointer_sized_uint = ATOMIC_TYPE_ULONGLONG;
		} else {
			assert(pointer_size == 4);
			set_create_ld_ident(create_name_win32);
			target.user_label_prefix = "_";
			dialect.pointer_sized_int  = ATOMIC_TYPE_INT;
			dialect.pointer_sized_uint = ATOMIC_TYPE_UINT;
			dialect.support_fastcall_stdcall = true;
		}
	} else if (strstart(os, "midipix")) {
		driver_default_exe_output = "a.out";
		target.object_format = OBJECT_FORMAT_PE_COFF;
		set_be_option("ia32-struct_in_reg=no");
		dialect.long_long_and_double_struct_align = 0;
		ppdef("__midipix__", "1");
		if (pointer_size == 8) {
			set_be_option("amd64-x64abi=yes");
			set_create_ld_ident(create_name_win64);
			ppdef("__NT64", "1");
		} else {
			assert(pointer_size == 4);
			set_create_ld_ident(create_name_win32);
			target.user_label_prefix = "_";
			ppdef("__NT32", "1");
		}
	} else {
		errorf(NULL, "unknown operating system '%s' in target-triple", os);
		exit(EXIT_FAILURE);
	}
}

void warn_experimental_target(void)
{
	char const *const experimental = be_get_backend_param()->experimental;
	if (experimental)
		warningf(WARN_EXPERIMENTAL, NULL, "%s", experimental);
}

static bool setup_firm_isa(void)
{
	const char *isa = target.firm_isa;
	char buf[64];
	snprintf(buf, sizeof(buf), "isa=%s", isa);
	if (!be_parse_arg(buf)) {
		errorf(NULL, "Couldn't select firm isa '%s'", isa);
		return false;
	}

	const char *arch = target.firm_arch;
	/* only pass down for ia32 for now */
	if (arch != NULL && streq(isa, "ia32")) {
		snprintf(buf, sizeof(buf), "%s-arch=%s", isa, arch);
		if (!be_parse_arg(buf)) {
			errorf(NULL, "Couldn't select firm arch '%s-arch=%s'", isa, arch);
			return false;
		}
	}

	return true;
}

static bool pass_options_to_firm_be(void)
{
	switch (target.object_format) {
	case OBJECT_FORMAT_ELF:     set_be_option("objectformat=elf");    break;
	case OBJECT_FORMAT_MACH_O:  set_be_option("objectformat=mach-o"); break;
	case OBJECT_FORMAT_PE_COFF: set_be_option("objectformat=coff");   break;
	}

	if (profile_generate) {
		driver_add_flag(&ldflags_obst, "-lfirmprof");
		set_be_option("profilegenerate");
	}
	if (profile_use) {
		set_be_option("profileuse");
	}
	bool res = true;
	const char *pic_option;
	if (target.pic_mode > 0) {
		/* Select correct PIC mode */
		if (target.object_format == OBJECT_FORMAT_MACH_O) {
			pic_option = "pic=mach-o";
		} else {
			pic_option = target.pic_no_plt ? "pic=elf-noplt"
			                               : "pic=elf";
		}
	} else {
		pic_option = "pic=none";
	}
	set_be_option(pic_option);

	/* pass options to firm backend (this happens delayed because we first
	 * had to decide which backend is actually used) */
	for (codegen_option_t *option = codegen_options; option != NULL;
	     option = option->next) {
		char        buf[256];
		const char *opt = option->option;
		/* pass option along to firm backend (except the -m32, -m64 stuff) */
		snprintf(buf, sizeof(buf), "%s-%s", target.firm_isa, opt);
		if (be_parse_arg(buf) == 0) {
			errorf(NULL, "Unknown codegen option '-m%s'", opt);
			res = false;
			continue;
		}

		/* hack to emulate the behaviour of some gcc spec files which filter
		 * flags to pass to cpp/ld/as */
		static char const *const pass_to_cpp_and_ld[] = {
			"soft-float"
		};
		for (size_t i = 0; i < ARRAY_SIZE(pass_to_cpp_and_ld); ++i) {
			if (streq(pass_to_cpp_and_ld[i], option->option)) {
				snprintf(buf, sizeof(buf), "-m%s", option->option);
				driver_add_flag(&cppflags_obst, buf);
				driver_add_flag(&asflags_obst, buf);
				driver_add_flag(&ldflags_obst, buf);
				break;
			}
		}
	}

	/* We can initialize the backend at this point and call be_get_backend_param() */
	if (target.pic_mode > 0 && !be_get_backend_param()->pic_supported) {
		errorf(NULL, "Selected backend '%s' does not support position independent code (PIC)",
			   target.firm_isa);
		res = false;
	}

	return res;
}

static machine_triple_t *get_host_machine_triple(void)
{
#ifdef HOST_TRIPLE
	/* a triple for the host machine was defined in the Makefile
	 * or config.mak */
	return parse_machine_triple(HOST_TRIPLE);
#else
	/* no host triple specified, we do some guessing based on preprocessor
	 * defines (look into predefs.c for inspiration) */
	machine_triple_t *machine = XMALLOC(machine_triple_t);

	machine->cpu_type = xstrdup(
#if defined(__x86_64__)
		"x86_64"
#elif defined(__i686__)
		"i686"
#elif defined(__i386__)
		"i386"
#elif defined(__sparc__)
		"sparc"
#elif defined(__arm__)
		"arm"
#else
	/* Choose a widely used cpu_type; "unknown" would not be useful here. */
		"i386"
#endif
	);

	machine->manufacturer = xstrdup(
#if defined(__leon__)
		"leon"
#else
		"unknown"
#endif
	);

	machine->operating_system = xstrdup(
#if defined(_WIN32) || defined(__CYGWIN__)
		"win32"
#elif defined(__APPLE__)
		"darwin"
#elif defined(__FreeBSD__)
		"freebsd"
#elif defined(__gnu_linux__)
		"linux-gnu"
#elif defined(__linux__)
		"linux"
#elif defined(__midipix__)
		"midipix"
#elif defined(__ELF__)
		"elf"
#else
		"unknown"
#endif
	);

	return machine;
#endif
}

static void determine_target_machine(void)
{
	if (target.machine == NULL)
		target.machine = get_host_machine_triple();
	/* adjust for -m32/-m64 flag */
	const char *cpu = target.machine->cpu_type;
	if (is_ia32_cpu(cpu) && target_size_override == 64) {
		free(target.machine->cpu_type);
		target.machine->cpu_type = xstrdup("x86_64");
	} else if (is_amd64_cpu(cpu) && target_size_override == 32) {
		free(target.machine->cpu_type);
		target.machine->cpu_type = xstrdup("i686");
	}
}

void target_set_defaults(void)
{
	determine_target_machine();
	set_options_for_machine(target.machine);
}

bool target_setup(void)
{
	bool res = setup_firm_isa();
	res &= pass_options_to_firm_be();

	multilib_directory_target_triple = NULL;
	if (target.triple == NULL) {
#ifdef MULTILIB_M32_TRIPLE
		if (dialect.pointer_size == 4)
			multilib_directory_target_triple = MULTILIB_M32_TRIPLE;
#endif
#ifdef MULTILIB_M64_TRIPLE
		if (dialect.pointer_size == 8)
			multilib_directory_target_triple = MULTILIB_M64_TRIPLE;
#endif
	}

	return res;
}
