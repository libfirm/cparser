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

target_t target;

const char *multilib_directory_target_triple;
unsigned target_size_override;
bool set_wchar;
bool short_wchar;
bool unsigned_char;

static atomic_type_kind_t ir_platform_to_ast(ir_platform_type_t type,
                                             bool get_signed)
{
	switch (type) {
	case IR_TYPE_BOOL:
		return ATOMIC_TYPE_BOOL;
	case IR_TYPE_CHAR:
		return get_signed ? ATOMIC_TYPE_SCHAR : ATOMIC_TYPE_UCHAR;
	case IR_TYPE_SHORT:
		return get_signed ? ATOMIC_TYPE_SHORT : ATOMIC_TYPE_USHORT;
	case IR_TYPE_INT:
		return get_signed ? ATOMIC_TYPE_INT : ATOMIC_TYPE_UINT;
	case IR_TYPE_LONG:
		return get_signed ? ATOMIC_TYPE_LONG : ATOMIC_TYPE_ULONG;
	case IR_TYPE_LONG_LONG:
		return get_signed ? ATOMIC_TYPE_LONGLONG : ATOMIC_TYPE_ULONGLONG;
	case IR_TYPE_FLOAT:
		assert(get_signed);
		return ATOMIC_TYPE_FLOAT;
	case IR_TYPE_DOUBLE:
		assert(get_signed);
		return ATOMIC_TYPE_DOUBLE;
	case IR_TYPE_LONG_DOUBLE:
		assert(get_signed);
		return ATOMIC_TYPE_LONG_DOUBLE;
	}
	panic("Invalid basic type");
}

void target_adjust_types_and_dialect(void)
{
	unsigned int_size     = ir_platform_type_size(IR_TYPE_INT);
	unsigned long_size    = ir_platform_type_size(IR_TYPE_LONG);
	unsigned pointer_size = ir_target_pointer_size();
	atomic_type_kind_t pointer_sized_int
		= ir_platform_to_ast(ir_platform_intptr_type(), true);
	atomic_type_kind_t pointer_sized_uint
		= ir_platform_to_ast(ir_platform_intptr_type(), false);
	atomic_type_kind_t wchar_atomic_kind
		= set_wchar ? (short_wchar ? ATOMIC_TYPE_USHORT : ATOMIC_TYPE_INT)
		            : ir_platform_to_ast(ir_platform_wchar_type(),
		                                 ir_platform_wchar_is_signed());
	init_types(int_size, long_size, pointer_size, wchar_atomic_kind,
			   pointer_sized_int, pointer_sized_uint);

	atomic_type_properties_t *const props = atomic_type_properties;

	unsigned const ll_d_struct_align
		= ir_platform_long_long_and_double_struct_align_override();
	if (ll_d_struct_align > 0) {
		props[ATOMIC_TYPE_LONGLONG].struct_alignment  = ll_d_struct_align;
		props[ATOMIC_TYPE_ULONGLONG].struct_alignment = ll_d_struct_align;
		props[ATOMIC_TYPE_DOUBLE].struct_alignment    = ll_d_struct_align;
	}

	props[ATOMIC_TYPE_LONG_DOUBLE].size
		= ir_platform_type_size(IR_TYPE_LONG_DOUBLE);
	unsigned align = ir_platform_type_align(IR_TYPE_LONG_DOUBLE);
	props[ATOMIC_TYPE_LONG_DOUBLE].alignment        = align;
	props[ATOMIC_TYPE_LONG_DOUBLE].struct_alignment = align;

	/* stuff decided after processing operating system specifics and
	 * commandline flags */
	if (unsigned_char) {
		props[ATOMIC_TYPE_CHAR].flags &= ~ATOMIC_TYPE_FLAG_SIGNED;
	} else {
		props[ATOMIC_TYPE_CHAR].flags |= ATOMIC_TYPE_FLAG_SIGNED;
	}

	static bool had_cpp_warning;
	if (dialect.cpp && !had_cpp_warning) {
		warningf(WARN_EXPERIMENTAL, NULL,
		         "C++ support is highly experimental and unfinished");
		had_cpp_warning = true;
	}

	/* Check that we match libfirms view of the world */
	assert(ir_platform_type_size(IR_TYPE_BOOL) == props[ATOMIC_TYPE_BOOL].size);
	assert(ir_platform_type_size(IR_TYPE_CHAR) == props[ATOMIC_TYPE_CHAR].size);
	assert(ir_platform_type_size(IR_TYPE_SHORT)
	       == props[ATOMIC_TYPE_SHORT].size);
	assert(ir_platform_type_size(IR_TYPE_INT) == props[ATOMIC_TYPE_INT].size);
	assert(ir_platform_type_size(IR_TYPE_LONG) == props[ATOMIC_TYPE_LONG].size);
	assert(ir_platform_type_size(IR_TYPE_LONG_LONG)
	       == props[ATOMIC_TYPE_LONGLONG].size);
	assert(ir_platform_type_size(IR_TYPE_FLOAT)
	       == props[ATOMIC_TYPE_FLOAT].size);
	assert(ir_platform_type_size(IR_TYPE_DOUBLE)
	       == props[ATOMIC_TYPE_DOUBLE].size);
	assert(ir_platform_type_size(IR_TYPE_LONG_DOUBLE)
	       == props[ATOMIC_TYPE_LONG_DOUBLE].size);
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

void set_target_option(char const *const arg)
{
	int res = ir_target_option(arg);
	if (!res)
		panic("setting firm backend option '%s' failed (maybe an outdated version of firm is used)", arg);
}

static bool pass_options_to_firm_be(void)
{
#ifdef NO_DEFAULT_VERIFY
    set_target_option("verify=off");
#endif

	if (profile_generate) {
		driver_add_flag(&ldflags_obst, "-lfirmprof");
		set_target_option("profilegenerate");
	}
	if (profile_use) {
		set_target_option("profileuse");
	}
	if (target.set_use_frame_pointer) {
		set_target_option(target.use_frame_pointer ? "omitfp=no" : "omitfp");
	}

	if (target.set_pic) {
		if (target.pic && !ir_target_supports_pic()) {
			errorf(NULL,
			       "Position independent code (PIC) not supported by target");
			exit(EXIT_FAILURE);
		}

		set_target_option(target.pic ? "pic=1" : "pic=0");
	}
	if (target.set_noplt)
		set_target_option(target.pic_noplt ? "noplt=1" : "noplt=0");

	/* pass options to firm backend (this happens delayed because we first
	 * had to decide which backend is actually used) */
	bool res = true;
	for (codegen_option_t *option = codegen_options; option != NULL;
	     option = option->next) {
		/* pass option along to firm backend (except the ones already filtered
		 * out like -m32, -m64) */
		if (!ir_target_option(option->option)) {
			errorf(NULL, "Unknown codegen option '-m%s'", option->option);
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
				char buf[64];
				snprintf(buf, sizeof(buf), "-m%s", option->option);
				driver_add_flag(&cppflags_obst, buf);
				driver_add_flag(&asflags_obst, buf);
				driver_add_flag(&ldflags_obst, buf);
				break;
			}
		}
	}

	return res;
}

static void determine_target_machine(void)
{
	if (target.machine == NULL)
		target.machine = ir_get_host_machine_triple();
	/* adjust for -m32/-m64 flag */
	const char *cpu = ir_triple_get_cpu_type(target.machine);
	if (is_ia32_cpu(cpu) && target_size_override == 64) {
		ir_triple_set_cpu_type(target.machine, "x86_64");
	} else if (is_amd64_cpu(cpu) && target_size_override == 32) {
		ir_triple_set_cpu_type(target.machine, "i686");
	}
}

static void set_options_from_be(void)
{
	if (target.pic && !ir_target_supports_pic()) {
		errorf(NULL, "Position independent code (PIC) not supported by target");
		exit(EXIT_FAILURE);
	}

	target.biggest_alignment     = ir_target_biggest_alignment();
	target.user_label_prefix     = ir_platform_user_label_prefix();
	target.byte_order_big_endian = ir_target_big_endian();
	driver_default_exe_output    = ir_platform_default_exe_name();

	if (strstart(ir_triple_get_operating_system(target.machine), "mingw")) {
		/* TODO: This should be done by libfirm instead of modifying the AST */
		dialect.enable_main_collect2_hack = true;

		if (ir_target_pointer_size() == 4)
			dialect.support_fastcall_stdcall = true;
	}
}

void init_firm_target(void)
{
	ir_init_library();
	determine_target_machine();

	bool initialized = ir_target_set_triple(target.machine);
	if (!initialized) {
		errorf(NULL, "Failed to initialize libfirm code generation\n");
		exit(EXIT_FAILURE);
	}

	if (strstart(ir_triple_get_operating_system(target.machine), "openbsd")) {
		target.pic = true;
		target.set_pic = true;
	}
}

bool target_setup(void)
{
	bool res = pass_options_to_firm_be();
	if (!res)
		return false;

	multilib_directory_target_triple = NULL;
	if (target.triple == NULL) {
#ifdef MULTILIB_M32_TRIPLE
		if (ir_target_pointer_size() == 4)
			multilib_directory_target_triple = MULTILIB_M32_TRIPLE;
#endif
#ifdef MULTILIB_M64_TRIPLE
		if (ir_target_pointer_size() == 8)
			multilib_directory_target_triple = MULTILIB_M64_TRIPLE;
#endif
	}

	ir_target_init();

	set_options_from_be();

	return res;
}
