/**
 *
 * @file firm_opt.c -- Firm-generating back end optimizations.
 *
 * (C) 2005-2010  Michael Beck   beck@ipd.info.uni-karlsruhe.de
 *
 * $Id$
 */

#include <config.h>

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <libfirm/firm.h>

#include "firm_opt.h"
#include "firm_codegen.h"
#include "firm_cmdline.h"
#include "firm_timing.h"
#include "ast2firm.h"

static ir_timer_t *t_vcg_dump;
static ir_timer_t *t_verify;
static ir_timer_t *t_all_opt;
static bool do_irg_opt(ir_graph *irg, const char *name);

/** dump all the graphs depending on cond */

static void dump_all(const char *suffix)
{
	if (!firm_dump.ir_graph)
		return;

	timer_push(t_vcg_dump);
	dump_all_ir_graphs(suffix);
	timer_pop(t_vcg_dump);
}

/* entities of runtime functions */
ir_entity_ptr rts_entities[rts_max];

/**
 * Map runtime functions.
 */
static void rts_map(void)
{
	static const struct {
		ir_entity_ptr *ent; /**< address of the rts entity */
		i_mapper_func func; /**< mapper function. */
	} mapper[] = {
		/* integer */
		{ &rts_entities[rts_abs],     i_mapper_abs },
		{ &rts_entities[rts_labs],    i_mapper_abs },
		{ &rts_entities[rts_llabs],   i_mapper_abs },
		{ &rts_entities[rts_imaxabs], i_mapper_abs },

		/* double -> double */
		{ &rts_entities[rts_fabs],    i_mapper_abs },
		{ &rts_entities[rts_sqrt],    i_mapper_sqrt },
		{ &rts_entities[rts_cbrt],    i_mapper_cbrt },
		{ &rts_entities[rts_pow],     i_mapper_pow },
		{ &rts_entities[rts_exp],     i_mapper_exp },
		{ &rts_entities[rts_exp2],    i_mapper_exp },
		{ &rts_entities[rts_exp10],   i_mapper_exp },
		{ &rts_entities[rts_log],     i_mapper_log },
		{ &rts_entities[rts_log2],    i_mapper_log2 },
		{ &rts_entities[rts_log10],   i_mapper_log10 },
		{ &rts_entities[rts_sin],     i_mapper_sin },
		{ &rts_entities[rts_cos],     i_mapper_cos },
		{ &rts_entities[rts_tan],     i_mapper_tan },
		{ &rts_entities[rts_asin],    i_mapper_asin },
		{ &rts_entities[rts_acos],    i_mapper_acos },
		{ &rts_entities[rts_atan],    i_mapper_atan },
		{ &rts_entities[rts_sinh],    i_mapper_sinh },
		{ &rts_entities[rts_cosh],    i_mapper_cosh },
		{ &rts_entities[rts_tanh],    i_mapper_tanh },

		/* float -> float */
		{ &rts_entities[rts_fabsf],   i_mapper_abs },
		{ &rts_entities[rts_sqrtf],   i_mapper_sqrt },
		{ &rts_entities[rts_cbrtf],   i_mapper_cbrt },
		{ &rts_entities[rts_powf],    i_mapper_pow },
		{ &rts_entities[rts_expf],    i_mapper_exp },
		{ &rts_entities[rts_exp2f],   i_mapper_exp },
		{ &rts_entities[rts_exp10f],  i_mapper_exp },
		{ &rts_entities[rts_logf],    i_mapper_log },
		{ &rts_entities[rts_log2f],   i_mapper_log2 },
		{ &rts_entities[rts_log10f],  i_mapper_log10 },
		{ &rts_entities[rts_sinf],    i_mapper_sin },
		{ &rts_entities[rts_cosf],    i_mapper_cos },
		{ &rts_entities[rts_tanf],    i_mapper_tan },
		{ &rts_entities[rts_asinf],   i_mapper_asin },
		{ &rts_entities[rts_acosf],   i_mapper_acos },
		{ &rts_entities[rts_atanf],   i_mapper_atan },
		{ &rts_entities[rts_sinhf],   i_mapper_sinh },
		{ &rts_entities[rts_coshf],   i_mapper_cosh },
		{ &rts_entities[rts_tanhf],   i_mapper_tanh },

		/* long double -> long double */
		{ &rts_entities[rts_fabsl],   i_mapper_abs },
		{ &rts_entities[rts_sqrtl],   i_mapper_sqrt },
		{ &rts_entities[rts_cbrtl],   i_mapper_cbrt },
		{ &rts_entities[rts_powl],    i_mapper_pow },
		{ &rts_entities[rts_expl],    i_mapper_exp },
		{ &rts_entities[rts_exp2l],   i_mapper_exp },
		{ &rts_entities[rts_exp10l],  i_mapper_exp },
		{ &rts_entities[rts_logl],    i_mapper_log },
		{ &rts_entities[rts_log2l],   i_mapper_log2 },
		{ &rts_entities[rts_log10l],  i_mapper_log10 },
		{ &rts_entities[rts_sinl],    i_mapper_sin },
		{ &rts_entities[rts_cosl],    i_mapper_cos },
		{ &rts_entities[rts_tanl],    i_mapper_tan },
		{ &rts_entities[rts_asinl],   i_mapper_asin },
		{ &rts_entities[rts_acosl],   i_mapper_acos },
		{ &rts_entities[rts_atanl],   i_mapper_atan },
		{ &rts_entities[rts_sinhl],   i_mapper_sinh },
		{ &rts_entities[rts_coshl],   i_mapper_cosh },
		{ &rts_entities[rts_tanhl],   i_mapper_tanh },

		/* string */
		{ &rts_entities[rts_strcmp],  i_mapper_strcmp },
		{ &rts_entities[rts_strncmp], i_mapper_strncmp },
		{ &rts_entities[rts_strcpy],  i_mapper_strcpy },
		{ &rts_entities[rts_strlen],  i_mapper_strlen },
		{ &rts_entities[rts_memcpy],  i_mapper_memcpy },
		{ &rts_entities[rts_mempcpy], i_mapper_mempcpy },
		{ &rts_entities[rts_memmove], i_mapper_memmove },
		{ &rts_entities[rts_memset],  i_mapper_memset },
		{ &rts_entities[rts_memcmp],  i_mapper_memcmp }
	};
	i_record rec[sizeof(mapper)/sizeof(mapper[0])];
	unsigned i, n_map;

	for (i = n_map = 0; i < sizeof(mapper)/sizeof(mapper[0]); ++i) {
		if (*mapper[i].ent != NULL) {
			rec[n_map].i_call.kind     = INTRINSIC_CALL;
			rec[n_map].i_call.i_ent    = *mapper[i].ent;
			rec[n_map].i_call.i_mapper = mapper[i].func;
			rec[n_map].i_call.ctx      = NULL;
			rec[n_map].i_call.link     = NULL;
			++n_map;
		}
	}

	if (n_map > 0)
		lower_intrinsics(rec, n_map, /* part_block_used=*/0);
}

static int *irg_dump_no;
static int firm_const_exists;

static void do_optimize_funccalls(void)
{
	optimize_funccalls(firm_const_exists, NULL);
}

static void do_gcse(ir_graph *irg)
{
	set_opt_global_cse(1);
	optimize_graph_df(irg);
	place_code(irg);
	set_opt_global_cse(0);
}

static void do_lower_highlevel(ir_graph *irg)
{
	lower_highlevel_graph(irg, firm_opt.lower_bitfields);
}

static void do_stred(ir_graph *irg)
{
	opt_osr(irg, osr_flag_default | osr_flag_keep_reg_pressure | osr_flag_ignore_x86_shift);
}

static void after_inline_opt(ir_graph *irg)
{
	do_irg_opt(irg, "scalar-replace");
	do_irg_opt(irg, "local");
	do_irg_opt(irg, "control-flow");
	do_irg_opt(irg, "combo");
}

static void do_inline(void)
{
	inline_functions(firm_opt.inline_maxsize, firm_opt.inline_threshold,
	                 after_inline_opt);
}

static void do_cloning(void)
{
	proc_cloning((float) firm_opt.clone_threshold);
}

static void do_lower_mux(ir_graph *irg)
{
	lower_mux(irg, NULL);
}

static void do_vrp(ir_graph *irg)
{
	set_vrp_data(irg);
}

typedef enum opt_target {
	OPT_TARGET_IRG, /**< optimization function works on a single graph */
	OPT_TARGET_IRP  /**< optimization function works on the complete program */
} opt_target_t;

typedef enum opt_flags {
	OPT_FLAG_NONE         = 0,
	OPT_FLAG_ENABLED      = 1 << 0, /**< enable the optimization */
	OPT_FLAG_NO_DUMP      = 1 << 1, /**< don't dump after transformation */
	OPT_FLAG_NO_VERIFY    = 1 << 2, /**< don't verify after transformation */
	OPT_FLAG_HIDE_OPTIONS = 1 << 3, /**< do not automatically process
	                                     -foptions for this transformation */
	OPT_FLAG_ESSENTIAL    = 1 << 4, /**< output won't work without this pass
	                                     so we need it even with -O0 */
} opt_flags_t;

typedef void (*transform_irg_func)(ir_graph *irg);
typedef void (*transform_irp_func)(void);

typedef struct {
	opt_target_t  target;
	const char   *name;
	union {
		transform_irg_func transform_irg;
		transform_irp_func transform_irp;
	} u;
	const char   *description;
	opt_flags_t   flags;
} opt_config_t;

static opt_config_t opts[] = {
#define IRG(a, b, c, d) { OPT_TARGET_IRG, a, .u.transform_irg = (transform_irg_func)b, c, d }
#define IRP(a, b, c, d) { OPT_TARGET_IRP, a, .u.transform_irp = b,                     c, d }
	IRG("bool",              opt_bool,                 "bool simplification",                                   OPT_FLAG_NONE),
	IRG("combo",             combo,                    "combined CCE, UCE and GVN",                             OPT_FLAG_NONE),
	IRG("confirm",           construct_confirms,       "confirm optimisation",                                  OPT_FLAG_HIDE_OPTIONS),
	IRG("control-flow",      optimize_cf,              "optimization of control-flow",                          OPT_FLAG_HIDE_OPTIONS),
	IRG("dead",              dead_node_elimination,    "dead node elimination",                                 OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY),
	IRG("deconv",            conv_opt,                 "conv node elimination",                                 OPT_FLAG_NONE),
	IRG("fp-vrp",            fixpoint_vrp,             "fixpoint value range propagation",                      OPT_FLAG_NONE),
	IRG("frame",             opt_frame_irg,            "remove unused frame entities",                          OPT_FLAG_NONE),
	IRG("gcse",              do_gcse,                  "global common subexpression elimination",               OPT_FLAG_NONE),
	IRG("gvn-pre",           do_gvn_pre,               "global value numbering partial redundancy elimination", OPT_FLAG_NONE),
	IRG("if-conversion",     opt_if_conv,              "if-conversion",                                         OPT_FLAG_NONE),
	IRG("invert-loops",      do_loop_inversion,        "loop inversion",                                        OPT_FLAG_NONE),
	IRG("ivopts",            do_stred,                 "induction variable strength reduction",                 OPT_FLAG_NONE),
	IRG("local",             optimize_graph_df,        "local graph optimizations",                             OPT_FLAG_HIDE_OPTIONS),
	IRG("lower",             do_lower_highlevel,       "lowering",                                              OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_ESSENTIAL),
	IRG("lower-mux",         do_lower_mux,             "mux lowering",                                          OPT_FLAG_NONE),
	IRG("opt-load-store",    optimize_load_store,      "load store optimization",                               OPT_FLAG_NONE),
	IRG("opt-tail-rec",      opt_tail_rec_irg,         "tail-recursion eliminiation",                           OPT_FLAG_NONE),
	IRG("parallelize-mem",   opt_parallelize_mem,      "parallelize memory",                                    OPT_FLAG_NONE),
	IRG("place",             place_code,               "code placement",                                        OPT_FLAG_NONE),
	IRG("reassociation",     optimize_reassociation,   "reassociation",                                         OPT_FLAG_NONE),
	IRG("remove-confirms",   remove_confirms,          "confirm removal",                                       OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY),
	IRG("remove-phi-cycles", remove_phi_cycles,        "removal of phi cycles",                                 OPT_FLAG_HIDE_OPTIONS),
	IRG("scalar-replace",    scalar_replacement_opt,   "scalar replacement",                                    OPT_FLAG_NONE),
	IRG("shape-blocks",      shape_blocks,             "block shaping",                                         OPT_FLAG_NONE),
	IRG("thread-jumps",      opt_jumpthreading,        "path-sensitive jumpthreading",                          OPT_FLAG_NONE),
	IRG("unroll-loops",      do_loop_unrolling,        "loop unrolling",                                        OPT_FLAG_NONE),
	IRG("vrp",               do_vrp,                   "value range propagation",                               OPT_FLAG_NONE),
	IRP("inline",            do_inline,                "inlining",                                              OPT_FLAG_NONE),
	IRP("lower-const",       lower_const_code,         "lowering of constant code",                             OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY | OPT_FLAG_ESSENTIAL),
	IRP("target-lowering",   be_lower_for_target,      "lowering necessary for target architecture",            OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_ESSENTIAL),
	IRP("opt-func-call",     do_optimize_funccalls,    "function call optimization",                            OPT_FLAG_NONE),
	IRP("opt-proc-clone",    do_cloning,               "procedure cloning",                                     OPT_FLAG_NONE),
	IRP("remove-unused",     garbage_collect_entities, "removal of unused functions/variables",                 OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY),
	IRP("rts",               rts_map,                  "optimization of known library functions",               OPT_FLAG_HIDE_OPTIONS),
#undef IRP
#undef IRG
};
static const int n_opts = sizeof(opts) / sizeof(opts[0]);
ir_timer_t *timers[sizeof(opts)/sizeof(opts[0])];

static opt_config_t *get_opt(const char *name)
{
	int i;
	for (i = 0; i < n_opts; ++i) {
		opt_config_t *config = &opts[i];
		if (strcmp(config->name, name) == 0)
			return config;
	}

	return NULL;
}

static void set_opt_enabled(const char *name, bool enabled)
{
	opt_config_t *config = get_opt(name);
	config->flags = (config->flags & ~OPT_FLAG_ENABLED)
		| (enabled ? OPT_FLAG_ENABLED : 0);
}

static bool get_opt_enabled(const char *name)
{
	opt_config_t *config = get_opt(name);
	return (config->flags & OPT_FLAG_ENABLED) != 0;
}

/**
 * perform an optimisation on a single graph
 *
 * @return  true if something changed, false otherwise
 */
static bool do_irg_opt(ir_graph *irg, const char *name)
{
	ir_graph     *old_irg;
	opt_config_t *config = get_opt(name);
	size_t        n      = config - opts;
	assert(config != NULL);
	assert(config->target == OPT_TARGET_IRG);
	if (! (config->flags & OPT_FLAG_ENABLED))
		return false;

	old_irg          = current_ir_graph;
	current_ir_graph = irg;

	timer_push(timers[n]);
	config->u.transform_irg(irg);
	timer_pop(timers[n]);

	if (firm_dump.all_phases && firm_dump.ir_graph) {
		dump_ir_graph(irg, name);
	}

	if (firm_opt.check_all) {
		timer_push(t_verify);
		irg_verify(irg, VERIFY_ENFORCE_SSA);
		timer_pop(t_verify);
	}

	current_ir_graph = old_irg;
	return true;
}

static void do_irp_opt(const char *name)
{
	opt_config_t *config = get_opt(name);
	size_t        n      = config - opts;
	assert(config->target == OPT_TARGET_IRP);
	if (! (config->flags & OPT_FLAG_ENABLED))
		return;

	timer_push(timers[n]);
	config->u.transform_irp();
	timer_pop(timers[n]);

	if (firm_dump.ir_graph && firm_dump.all_phases) {
		int i;
		for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
			ir_graph *irg = get_irp_irg(i);
			dump_ir_graph(irg, name);
		}
	}

	if (firm_opt.check_all) {
		int i;
		timer_push(t_verify);
		for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
			irg_verify(get_irp_irg(i), VERIFY_ENFORCE_SSA);
		}
		timer_pop(t_verify);
	}
}

/**
 * Enable transformations which should be always safe (and cheap) to perform
 */
static void enable_safe_defaults(void)
{
	set_opt_enabled("remove-unused", true);
	set_opt_enabled("opt-tail-rec", true);
	set_opt_enabled("opt-func-call", true);
	set_opt_enabled("reassociation", true);
	set_opt_enabled("control-flow", true);
	set_opt_enabled("local", true);
	set_opt_enabled("lower-const", true);
	set_opt_enabled("scalar-replace", true);
	set_opt_enabled("place", true);
	set_opt_enabled("confirm", true);
	set_opt_enabled("opt-load-store", true);
	set_opt_enabled("lower", true);
	set_opt_enabled("deconv", true);
	set_opt_enabled("remove-confirms", true);
	set_opt_enabled("ivopts", true);
	set_opt_enabled("dead", true);
	set_opt_enabled("remove-phi-cycles", true);
	set_opt_enabled("frame", true);
	set_opt_enabled("combo", true);
	set_opt_enabled("invert-loops", true);
	set_opt_enabled("target-lowering", true);
	set_opt_enabled("rts", true);
}

/**
 * run all the Firm optimizations
 *
 * @param input_filename     the name of the (main) source file
 */
static void do_firm_optimizations(const char *input_filename)
{
	size_t   i;
	unsigned aa_opt;

	set_opt_alias_analysis(firm_opt.alias_analysis);

	aa_opt = aa_opt_no_opt;
	if (firm_opt.strict_alias)
		aa_opt |= aa_opt_type_based | aa_opt_byte_type_may_alias;
	if (firm_opt.no_alias)
		aa_opt = aa_opt_no_alias;

	set_irp_memory_disambiguator_options(aa_opt);

	/* parameter passing code should set them directly sometime... */
	set_opt_enabled("gcse", firm_opt.gcse);
	set_opt_enabled("place", !firm_opt.gcse);
	set_opt_enabled("confirm", firm_opt.confirm);
	set_opt_enabled("remove-confirms", firm_opt.confirm);

	/* osr supersedes remove_phi_cycles */
	if (get_opt_enabled("ivopts"))
		set_opt_enabled("remove-phi-cycles", false);

	timer_start(t_all_opt);

	do_irp_opt("rts");

	/* first step: kill dead code */
	for (i = 0; i < get_irp_n_irgs(); i++) {
		ir_graph *irg = get_irp_irg(i);
		do_irg_opt(irg, "combo");
		do_irg_opt(irg, "local");
		do_irg_opt(irg, "control-flow");
	}

	do_irp_opt("remove-unused");
	for (i = 0; i < get_irp_n_irgs(); ++i) {
		ir_graph *irg = get_irp_irg(i);
		do_irg_opt(irg, "opt-tail-rec");
	}
	do_irp_opt("opt-func-call");
	do_irp_opt("lower-const");

	for (i = 0; i < get_irp_n_irgs(); i++) {
		ir_graph *irg = get_irp_irg(i);

		do_irg_opt(irg, "scalar-replace");
		do_irg_opt(irg, "invert-loops");
		do_irg_opt(irg, "unroll-loops");
		do_irg_opt(irg, "local");
		do_irg_opt(irg, "reassociation");
		do_irg_opt(irg, "local");
		do_irg_opt(irg, "gcse");

		if (firm_opt.confirm) {
			/* Confirm construction currently can only handle blocks with only
			   one control flow predecessor. Calling optimize_cf here removes
			   Bad predecessors and help the optimization of switch constructs.
			 */
			do_irg_opt(irg, "control-flow");
			do_irg_opt(irg, "confirm");
			do_irg_opt(irg, "vrp");
			do_irg_opt(irg, "local");
		}

		do_irg_opt(irg, "control-flow");
		do_irg_opt(irg, "opt-load-store");
		do_irg_opt(irg, "fp-vrp");
		do_irg_opt(irg, "lower");
		do_irg_opt(irg, "deconv");
		do_irg_opt(irg, "thread-jumps");
		do_irg_opt(irg, "remove-confirms");
		do_irg_opt(irg, "gvn-pre");
		do_irg_opt(irg, "place");
		do_irg_opt(irg, "control-flow");

		if (do_irg_opt(irg, "if-conversion")) {
			do_irg_opt(irg, "local");
			do_irg_opt(irg, "control-flow");
		}
		/* this doesn't make too much sense but tests the mux destruction... */
		do_irg_opt(irg, "lower-mux");

		do_irg_opt(irg, "bool");
		do_irg_opt(irg, "shape-blocks");
		do_irg_opt(irg, "ivopts");
		do_irg_opt(irg, "local");
		do_irg_opt(irg, "dead");
		do_irg_opt(irg, "frame");
	}

	do_irp_opt("inline");
	do_irp_opt("opt-proc-clone");

	for (i = 0; i < get_irp_n_irgs(); i++) {
		ir_graph *irg = get_irp_irg(i);
		do_irg_opt(irg, "local");
		do_irg_opt(irg, "control-flow");
		do_irg_opt(irg, "thread-jumps");
		do_irg_opt(irg, "local");
		do_irg_opt(irg, "control-flow");

		if( do_irg_opt(irg, "vrp") ) { // if vrp is enabled
			do_irg_opt(irg, "local");
			do_irg_opt(irg, "vrp");
			do_irg_opt(irg, "local");
			do_irg_opt(irg, "vrp");
		}
	}

	if (firm_dump.ir_graph) {
		/* recompute backedges for nicer dumps */
		for (i = 0; i < get_irp_n_irgs(); i++)
			construct_cf_backedges(get_irp_irg(i));
	}

	dump_all("opt");

	if (firm_dump.statistic & STAT_AFTER_OPT)
		stat_dump_snapshot(input_filename, "opt");

	timer_stop(t_all_opt);
}

/**
 * do Firm lowering
 *
 * @param input_filename  the name of the (main) source file
 */
static void do_firm_lowering(const char *input_filename)
{
	int i;

	/* enable architecture dependent optimizations */
	arch_dep_set_opts((arch_dep_opts_t)
			((firm_opt.muls ? arch_dep_mul_to_shift : arch_dep_none) |
			 (firm_opt.divs ? arch_dep_div_by_const : arch_dep_none) |
			 (firm_opt.mods ? arch_dep_mod_by_const : arch_dep_none) ));
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);
		do_irg_opt(irg, "reassociation");
		do_irg_opt(irg, "local");
	}

	do_irp_opt("target-lowering");

	if (firm_dump.statistic & STAT_AFTER_LOWER)
		stat_dump_snapshot(input_filename, "low");

	if (firm_opt.enabled) {
		timer_start(t_all_opt);

		for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
			ir_graph *irg = get_irp_irg(i);

			do_irg_opt(irg, "local");
			do_irg_opt(irg, "gcse");
			do_irg_opt(irg, "control-flow");
			do_irg_opt(irg, "opt-load-store");
			do_irg_opt(irg, "local");
			do_irg_opt(irg, "control-flow");

			if (do_irg_opt(irg, "vrp")) {
				do_irg_opt(irg, "local");
				do_irg_opt(irg, "control-flow");
				do_irg_opt(irg, "vrp");
				do_irg_opt(irg, "local");
				do_irg_opt(irg, "control-flow");
			}

			if (do_irg_opt(irg, "if-conversion")) {
				do_irg_opt(irg, "local");
				do_irg_opt(irg, "control-flow");
			}

			do_irg_opt(irg, "parallelize-mem");
		}
		timer_stop(t_all_opt);

		do_irp_opt("remove-unused");

		dump_all("low-opt");
	}

	if (firm_opt.cc_opt)
		mark_private_methods();

	if (firm_dump.statistic & STAT_FINAL) {
		stat_dump_snapshot(input_filename, "final");
	}
}

/**
 * Initialize for the Firm-generating back end.
 */
void gen_firm_init(void)
{
	unsigned pattern = 0;
	int      i;

	for (i = 0; i < n_opts; ++i) {
		timers[i] = ir_timer_new();
		timer_register(timers[i], opts[i].description);
	}
	t_verify = ir_timer_new();
	timer_register(t_verify, "Firm: verify pass");
	t_vcg_dump = ir_timer_new();
	timer_register(t_vcg_dump, "Firm: vcg dumping");
	t_all_opt = ir_timer_new();
	timer_register(t_all_opt, "Firm: all optimizations");

	if (firm_dump.stat_pattern)
		pattern |= FIRMSTAT_PATTERN_ENABLED;

	if (firm_dump.stat_dag)
		pattern |= FIRMSTAT_COUNT_DAG;

	ir_init(NULL);
	firm_init_stat(firm_dump.statistic == STAT_NONE ?
			0 : FIRMSTAT_ENABLED | FIRMSTAT_COUNT_STRONG_OP
			| FIRMSTAT_COUNT_CONSTS | pattern);

	edges_init_dbg(firm_opt.verify_edges);

	/* Sel node cannot produce NULL pointers */
	set_opt_sel_based_null_check_elim(1);

	/* dynamic dispatch works currently only if whole world scenarios */
	set_opt_dyn_meth_dispatch(0);

	/* do not run architecture dependent optimizations in building phase */
	arch_dep_set_opts(arch_dep_none);

	do_node_verification((firm_verification_t) firm_opt.verify);
	if (firm_dump.filter != NULL)
		ir_set_dump_filter(firm_dump.filter);
	if (firm_dump.extbb)
		ir_add_dump_flags(ir_dump_flag_group_extbb);
	if (firm_dump.no_blocks)
		ir_remove_dump_flags(ir_dump_flag_blocks_as_subgraphs);

	if (firm_opt.enabled) {
		set_optimize(1);
		set_opt_constant_folding(firm_opt.const_folding);
		set_opt_algebraic_simplification(firm_opt.const_folding);
		set_opt_cse(firm_opt.cse);
		set_opt_global_cse(0);
		set_opt_unreachable_code(1);
	} else {
		set_optimize(0);
	}
}

/**
 * Called, after the Firm generation is completed,
 * do all optimizations and backend call here.
 *
 * @param out                a file handle for the output, may be NULL
 * @param input_filename     the name of the (main) source file
 * @param c_mode             non-zero if "C" was compiled
 * @param new_firm_const_exists  non-zero, if the const attribute was used on functions
 */
void gen_firm_finish(FILE *out, const char *input_filename,
                     int new_firm_const_exists)
{
	int i;

#if 0
	if (firm_opt.enable_statev) {
		char buf[1024];
		snprintf(buf, sizeof(buf), "%s.ev", input_filename);
		ir_stat_ev_begin(input_filename, firm_opt.statev_filter);
		ir_stat_ev_compilation_unit(input_filename);
	}
#endif

	firm_const_exists = new_firm_const_exists;

	/* the general for dumping option must be set, or the others will not work*/
	firm_dump.ir_graph
		= (a_byte) (firm_dump.ir_graph | firm_dump.all_phases | firm_dump.extbb);

	ir_add_dump_flags(ir_dump_flag_keepalive_edges
			| ir_dump_flag_consts_local | ir_dump_flag_dominance);
	ir_remove_dump_flags(ir_dump_flag_loops | ir_dump_flag_ld_names);

	/* FIXME: cloning might ADD new graphs. */
	irg_dump_no = calloc(get_irp_last_idx(), sizeof(*irg_dump_no));

	if (firm_dump.all_types) {
		dump_ir_prog_ext(dump_typegraph, "types.vcg");
	}

	/* finalize all graphs */
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);
		irg_finalize_cons(irg);
	}
	dump_all("");

	timer_push(t_verify);
	tr_verify();
	timer_pop(t_verify);

	/* all graphs are finalized, set the irp phase to high */
	set_irp_phase_state(phase_high);

	/* BEWARE: kill unreachable code before doing compound lowering */
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);
		do_irg_opt(irg, "control-flow");
	}

	/* lower copyb nodes */
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);
		lower_CopyB(irg, 128, 4);
	}

	if (firm_dump.statistic & STAT_BEFORE_OPT) {
		stat_dump_snapshot(input_filename, "noopt");
	}

	if (firm_opt.enabled)
		do_firm_optimizations(input_filename);

	if (firm_opt.lower)
		do_firm_lowering(input_filename);

	/* set the phase to low */
	for (i = get_irp_n_irgs() - 1; i >= 0; --i)
		set_irg_phase_state(get_irp_irg(i), phase_low);

	if (firm_dump.statistic & STAT_FINAL_IR)
		stat_dump_snapshot(input_filename, "final-ir");

	/* run the code generator */
	if (firm_be_opt.selection != BE_NONE)
		do_codegen(out, input_filename);

	if (firm_dump.statistic & STAT_FINAL)
		stat_dump_snapshot(input_filename, "final");
}

void disable_all_opts(void)
{
	for (int i = 0; i < n_opts; ++i) {
		opt_config_t *config = &opts[i];
		if (config->flags & OPT_FLAG_ESSENTIAL) {
			config->flags |= OPT_FLAG_ENABLED;
		} else {
			config->flags &= ~OPT_FLAG_ENABLED;
		}
	}
}

int firm_opt_option(const char *opt)
{
	bool enable = true;
	if (strncmp(opt, "no-", 3) == 0) {
		enable = false;
		opt = opt + 3;
	}

	opt_config_t *config = get_opt(opt);
	if (config == NULL || (config->flags & OPT_FLAG_HIDE_OPTIONS))
		return 0;

	config->flags &= ~OPT_FLAG_ENABLED;
	config->flags |= enable ? OPT_FLAG_ENABLED : 0;
	return 1;
}

void firm_opt_option_help(void)
{
	int i;

	for (i = 0; i < n_opts; ++i) {
		char buf[1024];
		char buf2[1024];

		const opt_config_t *config = &opts[i];
		if (config->flags & OPT_FLAG_HIDE_OPTIONS)
			continue;

		snprintf(buf2, sizeof(buf2), "firm: enable %s", config->description);
		print_option_help(config->name, buf2);
		snprintf(buf, sizeof(buf), "no-%s", config->name);
		snprintf(buf2, sizeof(buf2), "firm: disable %s", config->description);
		print_option_help(buf, buf2);
	}
}

/**
 * Do very early initializations
 */
void firm_early_init(void)
{
	/* arg: need this here for command line options */
	be_opt_register();

	enable_safe_defaults();
}
