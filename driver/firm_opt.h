/*
 * This file is part of cparser.
 * Copyright (C) 2012 Michael Beck <mm.beck@gmx.net>
 */
#ifndef FIRM_OPT_H
#define FIRM_OPT_H

#include <stdio.h>
#include <libfirm/firm_types.h>
#include <libfirm/dbginfo.h>

enum rts_names {
	rts_debugbreak,  /**< the name of the __debugbreak() intrinsic */
	rts_abort,       /**< the name of the abort() function */
	rts_abs,         /**< the name of the abs() function */
	rts_alloca,      /**< the name of the alloca() function */
	rts_labs,        /**< the name of the labs() function */
	rts_llabs,       /**< the name of the llabs() function */
	rts_imaxabs,     /**< the name of the imaxabs() function */

	/* double -> double functions */
	rts_fabs,        /**< the name of the fabs() function */
	rts_sqrt,        /**< the name of the sqrt() function */
	rts_cbrt,        /**< the name of the cbrt() function */
	rts_pow,         /**< the name of the pow() function */
	rts_exp,         /**< the name of the exp() function */
	rts_exp2,        /**< the name of the exp2() function */
	rts_exp10,       /**< the name of the exp10() function */
	rts_log,         /**< the name of the log() function */
	rts_log2,        /**< the name of the log2() function */
	rts_log10,       /**< the name of the log10() function */
	rts_sin,         /**< the name of the sin() function */
	rts_cos,         /**< the name of the cos() function */
	rts_tan,         /**< the name of the tan() function */
	rts_asin,        /**< the name of the asin() function */
	rts_acos,        /**< the name of the acos() function */
	rts_atan,        /**< the name of the atan() function */
	rts_sinh,        /**< the name of the sinh() function */
	rts_cosh,        /**< the name of the cosh() function */
	rts_tanh,        /**< the name of the tanh() function */

	/* float -> float functions */
	rts_fabsf,       /**< the name of the fabsf() function */
	rts_sqrtf,       /**< the name of the sqrtf() function */
	rts_cbrtf,       /**< the name of the cbrtf() function */
	rts_powf,        /**< the name of the powf() function */
	rts_expf,        /**< the name of the expf() function */
	rts_exp2f,       /**< the name of the exp2f() function */
	rts_exp10f,      /**< the name of the exp10f() function */
	rts_logf,        /**< the name of the logf() function */
	rts_log2f,       /**< the name of the log2f() function */
	rts_log10f,      /**< the name of the log10f() function */
	rts_sinf,        /**< the name of the sinf() function */
	rts_cosf,        /**< the name of the cosf() function */
	rts_tanf,        /**< the name of the tanf() function */
	rts_asinf,       /**< the name of the asinf() function */
	rts_acosf,       /**< the name of the acosf() function */
	rts_atanf,       /**< the name of the atanf() function */
	rts_sinhf,       /**< the name of the sinhf() function */
	rts_coshf,       /**< the name of the coshf() function */
	rts_tanhf,       /**< the name of the tanhf() function */

	/* long double -> long double functions */
	rts_fabsl,       /**< the name of the fabsl() function */
	rts_sqrtl,       /**< the name of the sqrtl() function */
	rts_cbrtl,       /**< the name of the cbrtl() function */
	rts_powl,        /**< the name of the powl() function */
	rts_expl,        /**< the name of the expl() function */
	rts_exp2l,       /**< the name of the exp2l() function */
	rts_exp10l,      /**< the name of the exp10l() function */
	rts_logl,        /**< the name of the log() function */
	rts_log2l,       /**< the name of the log2() function */
	rts_log10l,      /**< the name of the log10() function */
	rts_sinl,        /**< the name of the sinl() function */
	rts_cosl,        /**< the name of the cosl() function */
	rts_tanl,        /**< the name of the tanl() function */
	rts_asinl,       /**< the name of the asinl() function */
	rts_acosl,       /**< the name of the acosl() function */
	rts_atanl,       /**< the name of the atanl() function */
	rts_sinhl,       /**< the name of the sinhl() function */
	rts_coshl,       /**< the name of the coshl() function */
	rts_tanhl,       /**< the name of the tanhl() function */

	/* string functions */
	rts_strcmp,      /**< the name of the strcmp() function */
	rts_strncmp,     /**< the name of the strncmp() function */
	rts_strcpy,      /**< the name of the strcpy() function */
	rts_strlen,      /**< the name of the strlen() function */
	rts_memcpy,      /**< the name of the memcpy() function */
	rts_mempcpy,     /**< the name of the mempcpy() function */
	rts_memmove,     /**< the name of the memmove() function */
	rts_memset,      /**< the name of the memset() function */
	rts_memcmp,      /**< the name of the memcmp() function */

	rts_max
};

extern ir_entity *rts_entities[rts_max];

/** Initialize for the Firm-generating back end. */
void gen_firm_init(void);

/** free resources hold by firm-generating back end */
void gen_firm_finish(void);

/**
 * Transform, optimize and generate code
 *
 * @param out                a file handle for the output, may be NULL
 * @param input_filename     the name of the (main) source file
 */
void generate_code(FILE *out, const char *input_filename);

/** process optimization commandline option */
int firm_option(const char *opt);

typedef void (*print_option_help_func)(const char *name, const char *description);

void firm_option_help(print_option_help_func func);

/** Choose an optimization level. (Typically used to interpret the -O compiler
 * switches) */
void choose_optimization_pack(int level);

/**
 * Initialize implicit optimization settings in firm. Frontends should call this
 * before starting graph construction
 */
void init_implicit_optimizations(void);

#endif
