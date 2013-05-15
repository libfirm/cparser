/*
 * This file is part of cparser.
 * Copyright (C) 2012 Michael Beck <mm.beck@gmx.net>
 */

/**
 * @file
 * @author Michael Beck, Matthias Braun
 * @brief Firm-generating back end optimizations.
 */
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <libfirm/firm.h>

#include "firm_opt.h"
#include "firm_timing.h"
#include "adt/strutil.h"
#include "adt/util.h"

/* optimization settings */
struct a_firm_opt {
	bool     const_folding;   /**< enable constant folding */
	bool     cse;             /**< enable common-subexpression elimination */
	bool     confirm;         /**< enable Confirm optimization */
	bool     muls;            /**< enable architecture dependent mul optimization */
	bool     divs;            /**< enable architecture dependent div optimization */
	bool     mods;            /**< enable architecture dependent mod optimization */
	bool     alias_analysis;  /**< enable Alias Analysis */
	bool     strict_alias;    /**< enable strict Alias Analysis (using type based AA) */
	bool     no_alias;        /**< no aliasing possible. */
	bool     verify;          /**< Firm verifier setting */
	bool     check_all;       /**< enable checking all Firm phases */
	int      clone_threshold; /**< The threshold value for procedure cloning. */
	unsigned inline_maxsize;  /**< Maximum function size for inlining. */
	unsigned inline_threshold;/**< Inlining benefice threshold. */
};

/** statistic options */
typedef enum a_firmstat_selection_tag {
	STAT_NONE        = 0x00000000,
	STAT_BEFORE_OPT  = 0x00000001,
	STAT_AFTER_OPT   = 0x00000002,
	STAT_AFTER_LOWER = 0x00000004,
	STAT_FINAL_IR    = 0x00000008,
	STAT_FINAL       = 0x00000010,
} a_firmstat_selection;

/* dumping options */
struct a_firm_dump {
	bool debug_print;   /**< enable debug print */
	bool all_types;     /**< dump the All_types graph */
	bool ir_graph;      /**< dump all graphs */
	bool all_phases;    /**< dump the IR graph after all phases */
	bool statistic;     /**< Firm statistic setting */
};

struct a_firm_be_opt {
	bool selection;
	bool node_stat;
};

/* optimization settings */
static struct a_firm_opt firm_opt = {
	.const_folding    =  true,
	.cse              =  true,
	.confirm          =  true,
	.muls             =  true,
	.divs             =  true,
	.mods             =  true,
	.alias_analysis   =  true,
	.strict_alias     =  false,
	.no_alias         =  false,
	.verify           =  FIRM_VERIFICATION_ON,
	.check_all        =  true,
	.clone_threshold  =  DEFAULT_CLONE_THRESHOLD,
	.inline_maxsize   =  750,
	.inline_threshold =  0,
};

/* dumping options */
static struct a_firm_dump firm_dump = {
	.debug_print  = false,
	.all_types    = false,
	.ir_graph     = false,
	.all_phases   = false,
	.statistic    = STAT_NONE,
};

#define X(a)  a, sizeof(a)-1

/** Parameter description structure */
static const struct params {
  const char *option;      /**< name of the option */
  size_t     opt_len;      /**< length of the option string */
  bool       *flag;        /**< address of variable to set/reset */
  bool       set;          /**< iff true, variable will be set, else reset */
  const char *description; /**< description of this option */
} firm_options[] = {
  /* firm optimization options */
  { X("no-opt"),                 NULL,                       0, "disable all FIRM optimizations" },
  { X("cse"),                    &firm_opt.cse,              1, "enable common subexpression elimination" },
  { X("no-cse"),                 &firm_opt.cse,              0, "disable common subexpression elimination" },
  { X("const-fold"),             &firm_opt.const_folding,    1, "enable constant folding" },
  { X("no-const-fold"),          &firm_opt.const_folding,    0, "disable constant folding" },
  { X("inline-max-size=<size>"), NULL,                       0, "set maximum size for function inlining" },
  { X("inline-threshold=<size>"),NULL,                       0, "set benefice threshold for function inlining" },
  { X("confirm"),                &firm_opt.confirm,          1, "enable Confirm optimization" },
  { X("no-confirm"),             &firm_opt.confirm,          0, "disable Confirm optimization" },
  { X("opt-mul"),                &firm_opt.muls,             0, "enable multiplication optimization" },
  { X("no-opt-mul"),             &firm_opt.muls,             0, "disable multiplication optimization" },
  { X("opt-div"),                &firm_opt.divs,             0, "enable division optimization" },
  { X("no-opt-div"),             &firm_opt.divs,             0, "disable division optimization" },
  { X("opt-mod"),                &firm_opt.mods,             0, "enable remainder optimization" },
  { X("no-opt-mod"),             &firm_opt.mods,             0, "disable remainder optimization" },
  { X("opt-alias"),              &firm_opt.alias_analysis,   1, "enable alias analysis" },
  { X("no-opt-alias"),           &firm_opt.alias_analysis,   0, "disable alias analysis" },
  { X("alias"),                  &firm_opt.no_alias,         0, "aliasing occurs" },
  { X("no-alias"),               &firm_opt.no_alias,         1, "no aliasing occurs" },
  { X("strict-aliasing"),        &firm_opt.strict_alias,     1, "strict alias rules" },
  { X("no-strict-aliasing"),     &firm_opt.strict_alias,     0, "strict alias rules" },
  { X("clone-threshold=<value>"),NULL,                       0, "set clone threshold to <value>" },

  /* other firm regarding options */
  { X("verify-off"),             &firm_opt.verify,           FIRM_VERIFICATION_OFF,    "disable node verification" },
  { X("verify-on"),              &firm_opt.verify,           FIRM_VERIFICATION_ON,     "enable node verification" },
  { X("verify-report"),          &firm_opt.verify,           FIRM_VERIFICATION_REPORT, "node verification, report only" },

  /* dumping */
  { X("dump-ir"),                &firm_dump.ir_graph,        1, "dump IR graph" },
  { X("dump-all-types"),         &firm_dump.all_types,       1, "dump graph of all types" },
  { X("dump-all-phases"),        &firm_dump.all_phases,      1, "dump graphs for all optimization phases" },
  { X("dump-filter=<string>"),   NULL,                       0, "set dumper filter" },

  /* misc */
  { X("stat-before-opt"),        &firm_dump.statistic,       STAT_BEFORE_OPT,  "Firm statistic output before optimizations" },
  { X("stat-after-opt"),         &firm_dump.statistic,       STAT_AFTER_OPT,   "Firm statistic output after optimizations" },
  { X("stat-after-lower"),       &firm_dump.statistic,       STAT_AFTER_LOWER, "Firm statistic output after lowering" },
  { X("stat-final-ir"),          &firm_dump.statistic,       STAT_FINAL_IR,    "Firm statistic after final optimization" },
  { X("stat-final"),             &firm_dump.statistic,       STAT_FINAL,       "Firm statistic after code generation" },
};

#undef X

static ir_timer_t *t_vcg_dump;
static ir_timer_t *t_verify;
static ir_timer_t *t_all_opt;
static ir_timer_t *t_backend;
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
ir_entity *rts_entities[rts_max];

/**
 * Map runtime functions.
 */
static void rts_map(void)
{
	static const struct {
		ir_entity   **ent; /**< address of the rts entity */
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
	i_record rec[ARRAY_SIZE(mapper)];
	size_t   n_map = 0;

	for (size_t i = 0; i != ARRAY_SIZE(mapper); ++i) {
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
	ir_timer_t   *timer;
} opt_config_t;

static opt_config_t *get_opt(const char *name);

static void do_stred(ir_graph *irg)
{
	opt_osr(irg, osr_flag_default | osr_flag_keep_reg_pressure | osr_flag_ignore_x86_shift);
}

static void after_inline_opt(ir_graph *irg)
{
	opt_config_t *const config = get_opt("inline");
	timer_stop(config->timer);

	do_irg_opt(irg, "scalar-replace");
	do_irg_opt(irg, "local");
	do_irg_opt(irg, "control-flow");
	do_irg_opt(irg, "combo");

	timer_start(config->timer);
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

static void do_gcse(ir_graph *irg)
{
	set_opt_global_cse(1);
	optimize_graph_df(irg);
	set_opt_global_cse(0);
}

static opt_config_t opts[] = {
#define IRG(a, b, c, d) { OPT_TARGET_IRG, a, .u.transform_irg = (transform_irg_func)b, c, d }
#define IRP(a, b, c, d) { OPT_TARGET_IRP, a, .u.transform_irp = b,                     c, d }
	IRG("bool",              opt_bool,                 "bool simplification",                                   OPT_FLAG_NONE),
	IRG("combo",             combo,                    "combined CCE, UCE and GVN",                             OPT_FLAG_NONE),
	IRG("confirm",           construct_confirms,       "confirm optimization",                                  OPT_FLAG_HIDE_OPTIONS),
	IRG("control-flow",      optimize_cf,              "optimization of control-flow",                          OPT_FLAG_HIDE_OPTIONS),
	IRG("dead",              dead_node_elimination,    "dead node elimination",                                 OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY),
	IRG("deconv",            conv_opt,                 "conv node elimination",                                 OPT_FLAG_NONE),
	IRG("fp-vrp",            fixpoint_vrp,             "fixpoint value range propagation",                      OPT_FLAG_NONE),
	IRG("occults",           occult_consts,            "occult constant folding",                               OPT_FLAG_NONE),
	IRG("frame",             opt_frame_irg,            "remove unused frame entities",                          OPT_FLAG_NONE),
	IRG("gvn-pre",           do_gvn_pre,               "global value numbering partial redundancy elimination", OPT_FLAG_NONE),
	IRG("if-conversion",     opt_if_conv,              "if-conversion",                                         OPT_FLAG_NONE),
	IRG("invert-loops",      do_loop_inversion,        "loop inversion",                                        OPT_FLAG_NONE),
	IRG("ivopts",            do_stred,                 "induction variable strength reduction",                 OPT_FLAG_NONE),
	IRG("local",             local_opts,               "local graph optimizations",                             OPT_FLAG_HIDE_OPTIONS),
	IRG("lower",             lower_highlevel_graph,    "lowering",                                              OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_ESSENTIAL),
	IRG("lower-mux",         do_lower_mux,             "mux lowering",                                          OPT_FLAG_NONE),
	IRG("opt-load-store",    optimize_load_store,      "load store optimization",                               OPT_FLAG_NONE),
	IRG("opt-tail-rec",      opt_tail_rec_irg,         "tail-recursion eliminiation",                           OPT_FLAG_NONE),
	IRG("parallelize-mem",   opt_parallelize_mem,      "parallelize memory",                                    OPT_FLAG_NONE),
	IRG("gcse",              do_gcse,                  "global common subexpression eliminiation",              OPT_FLAG_NONE),
	IRG("place",             place_code,               "code placement",                                        OPT_FLAG_NONE),
	IRG("reassociation",     optimize_reassociation,   "reassociation",                                         OPT_FLAG_NONE),
	IRG("remove-confirms",   remove_confirms,          "confirm removal",                                       OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY),
	IRG("remove-phi-cycles", remove_phi_cycles,        "removal of phi cycles",                                 OPT_FLAG_HIDE_OPTIONS),
	IRG("scalar-replace",    scalar_replacement_opt,   "scalar replacement",                                    OPT_FLAG_NONE),
	IRG("shape-blocks",      shape_blocks,             "block shaping",                                         OPT_FLAG_NONE),
	IRG("thread-jumps",      opt_jumpthreading,        "path-sensitive jumpthreading",                          OPT_FLAG_NONE),
	IRG("unroll-loops",      do_loop_unrolling,        "loop unrolling",                                        OPT_FLAG_NONE),
	IRG("vrp",               set_vrp_data,             "value range propagation",                               OPT_FLAG_NONE),
	IRP("inline",            do_inline,                "inlining",                                              OPT_FLAG_NONE),
	IRP("lower-const",       lower_const_code,         "lowering of constant code",                             OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY | OPT_FLAG_ESSENTIAL),
	IRP("local-const",       local_opts_const_code,    "local optimisation of constant initializers",
	                            OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY | OPT_FLAG_ESSENTIAL),
	IRP("target-lowering",   be_lower_for_target,      "lowering necessary for target architecture",            OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_ESSENTIAL),
	IRP("opt-func-call",     optimize_funccalls,       "function call optimization",                            OPT_FLAG_NONE),
	IRP("opt-proc-clone",    do_cloning,               "procedure cloning",                                     OPT_FLAG_NONE),
	IRP("remove-unused",     garbage_collect_entities, "removal of unused functions/variables",                 OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY),
	IRP("rts",               rts_map,                  "optimization of known library functions",               OPT_FLAG_NONE),
	IRP("opt-cc",            mark_private_methods,     "calling conventions optimization",                      OPT_FLAG_NONE),
#undef IRP
#undef IRG
};

#define FOR_EACH_OPT(i) for (opt_config_t *i = opts; i != endof(opts); ++i)

static opt_config_t *get_opt(const char *name)
{
	FOR_EACH_OPT(config) {
		if (streq(config->name, name))
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
 * perform an optimization on a single graph
 *
 * @return  true if something changed, false otherwise
 */
static bool do_irg_opt(ir_graph *irg, const char *name)
{
	opt_config_t *const config = get_opt(name);
	assert(config != NULL);
	assert(config->target == OPT_TARGET_IRG);
	if (! (config->flags & OPT_FLAG_ENABLED))
		return false;

	ir_graph *const old_irg = current_ir_graph;
	current_ir_graph = irg;

	timer_start(config->timer);
	config->u.transform_irg(irg);
	timer_stop(config->timer);

	if (firm_dump.all_phases && firm_dump.ir_graph) {
		dump_ir_graph(irg, name);
	}

	if (firm_opt.verify) {
		timer_push(t_verify);
		irg_verify(irg, VERIFY_ENFORCE_SSA);
		timer_pop(t_verify);
	}

	current_ir_graph = old_irg;
	return true;
}

static void do_irp_opt(const char *name)
{
	opt_config_t *const config = get_opt(name);
	assert(config->target == OPT_TARGET_IRP);
	if (! (config->flags & OPT_FLAG_ENABLED))
		return;

	timer_start(config->timer);
	config->u.transform_irp();
	timer_stop(config->timer);

	if (firm_dump.ir_graph && firm_dump.all_phases) {
		int i;
		for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
			ir_graph *irg = get_irp_irg(i);
			dump_ir_graph(irg, name);
		}
	}

	if (firm_opt.verify) {
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
	set_opt_enabled("local-const", true);
	set_opt_enabled("scalar-replace", true);
	set_opt_enabled("place", true);
	set_opt_enabled("gcse", true);
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
	set_opt_enabled("parallelize-mem", true);
	set_opt_enabled("opt-cc", true);
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
	set_opt_enabled("confirm", firm_opt.confirm);
	set_opt_enabled("remove-confirms", firm_opt.confirm);

	/* osr supersedes remove_phi_cycles */
	if (get_opt_enabled("ivopts"))
		set_opt_enabled("remove-phi-cycles", false);

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
		do_irg_opt(irg, "place");

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
		do_irg_opt(irg, "occults");
		do_irg_opt(irg, "thread-jumps");
		do_irg_opt(irg, "remove-confirms");
		do_irg_opt(irg, "gvn-pre");
		do_irg_opt(irg, "gcse");
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

	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);

		do_irg_opt(irg, "local");
		do_irg_opt(irg, "deconv");
		do_irg_opt(irg, "control-flow");
		do_irg_opt(irg, "opt-load-store");
		do_irg_opt(irg, "gcse");
		do_irg_opt(irg, "place");
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

		add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_NORMALISATION2);
		do_irg_opt(irg, "local");

		do_irg_opt(irg, "parallelize-mem");
		do_irg_opt(irg, "frame");
	}
	/* hack so we get global initializers constant folded even at -O0 */
	set_opt_constant_folding(1);
	set_opt_algebraic_simplification(1);
	do_irp_opt("local-const");
	set_opt_constant_folding(firm_opt.const_folding);
	set_opt_algebraic_simplification(firm_opt.const_folding);
	do_irp_opt("remove-unused");
	do_irp_opt("opt-cc");
	dump_all("low-opt");

	if (firm_dump.statistic & STAT_FINAL) {
		stat_dump_snapshot(input_filename, "final");
	}
}

/**
 * Initialize for the Firm-generating back end.
 */
void gen_firm_init(void)
{
	ir_init();
	enable_safe_defaults();

	FOR_EACH_OPT(i) {
		i->timer = ir_timer_new();
		timer_register(i->timer, i->description);
	}
	t_verify = ir_timer_new();
	timer_register(t_verify, "Firm: verify pass");
	t_vcg_dump = ir_timer_new();
	timer_register(t_vcg_dump, "Firm: vcg dumping");
	t_all_opt = ir_timer_new();
	timer_register(t_all_opt, "Firm: all optimizations");
	t_backend = ir_timer_new();
	timer_register(t_backend, "Firm: backend");
}

/**
 * Called, after the Firm generation is completed,
 * do all optimizations and backend call here.
 *
 * @param out                a file handle for the output, may be NULL
 * @param input_filename     the name of the (main) source file
 */
void generate_code(FILE *out, const char *input_filename)
{
	int i;

	/* initialize implicit opts, just to be sure because really the frontend
	 * should have called it already before starting graph construction */
	init_implicit_optimizations();
	firm_init_stat();

	do_node_verification((firm_verification_t) firm_opt.verify);

	/* the general for dumping option must be set, or the others will not work*/
	firm_dump.ir_graph = (bool) (firm_dump.ir_graph | firm_dump.all_phases);

	ir_add_dump_flags(ir_dump_flag_keepalive_edges
			| ir_dump_flag_consts_local | ir_dump_flag_dominance);
	ir_remove_dump_flags(ir_dump_flag_loops | ir_dump_flag_ld_names);

	/* FIXME: cloning might ADD new graphs. */
	irg_dump_no = calloc(get_irp_last_idx(), sizeof(*irg_dump_no));

	ir_timer_init_parent(t_verify);
	ir_timer_init_parent(t_vcg_dump);
	timer_start(t_all_opt);

	if (firm_dump.all_types) {
		dump_ir_prog_ext(dump_typegraph, "types.vcg");
	}

	dump_all("");

	if (firm_opt.verify) {
		timer_push(t_verify);
		tr_verify();
		timer_pop(t_verify);
	}

	/* BEWARE: kill unreachable code before doing compound lowering */
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);
		do_irg_opt(irg, "control-flow");
	}

	if (firm_dump.statistic & STAT_BEFORE_OPT) {
		stat_dump_snapshot(input_filename, "noopt");
	}

	do_firm_optimizations(input_filename);
	do_firm_lowering(input_filename);

	timer_stop(t_all_opt);

	if (firm_dump.statistic & STAT_FINAL_IR)
		stat_dump_snapshot(input_filename, "final-ir");

	/* run the code generator */
	timer_start(t_backend);
	be_main(out, input_filename);
	timer_stop(t_backend);

	if (firm_dump.statistic & STAT_FINAL)
		stat_dump_snapshot(input_filename, "final");
}

void gen_firm_finish(void)
{
	ir_finish();
}

static void disable_all_opts(void)
{
	firm_opt.cse             = false;
	firm_opt.confirm         = false;
	firm_opt.muls            = false;
	firm_opt.divs            = false;
	firm_opt.mods            = false;
	firm_opt.alias_analysis  = false;
	firm_opt.strict_alias    = false;
	firm_opt.no_alias        = false;
	firm_opt.const_folding   = false;

	FOR_EACH_OPT(config) {
		if (config->flags & OPT_FLAG_ESSENTIAL) {
			config->flags |= OPT_FLAG_ENABLED;
		} else {
			config->flags &= ~OPT_FLAG_ENABLED;
		}
	}
}

static bool firm_opt_option(const char *opt)
{
	char const* const rest   = strstart(opt, "no-");
	bool        const enable = rest ? opt = rest, false : true;

	opt_config_t *config = get_opt(opt);
	if (config == NULL || (config->flags & OPT_FLAG_HIDE_OPTIONS))
		return false;

	config->flags &= ~OPT_FLAG_ENABLED;
	config->flags |= enable ? OPT_FLAG_ENABLED : 0;
	return true;
}

void firm_option_help(print_option_help_func print_option_help)
{
	FOR_EACH_OPT(config) {
		char buf[1024];
		char buf2[1024];

		if (config->flags & OPT_FLAG_HIDE_OPTIONS)
			continue;

		snprintf(buf, sizeof(buf), "-f%s", config->name);
		snprintf(buf2, sizeof(buf2), "enable %s", config->description);
		print_option_help(buf, buf2);
		snprintf(buf, sizeof(buf), "-fno-%s", config->name);
		snprintf(buf2, sizeof(buf2), "disable %s", config->description);
		print_option_help(buf, buf2);
	}

	for (size_t k = 0; k != ARRAY_SIZE(firm_options); ++k) {
		char buf[1024];
		char buf2[1024];
		snprintf(buf, sizeof(buf), "-f%s", firm_options[k].option);
		snprintf(buf2, sizeof(buf2), "%s", firm_options[k].description);
		print_option_help(buf, buf2);
	}
}

int firm_option(const char *const opt)
{
	char const* val;
	if ((val = strstart(opt, "dump-filter="))) {
		ir_set_dump_filter(val);
		return 1;
	} else if ((val = strstart(opt, "clone-threshold="))) {
		sscanf(val, "%d", &firm_opt.clone_threshold);
		return 1;
	} else if ((val = strstart(opt, "inline-max-size="))) {
		sscanf(val, "%u", &firm_opt.inline_maxsize);
		return 1;
	} else if ((val = strstart(opt, "inline-threshold="))) {
		sscanf(val, "%u", &firm_opt.inline_threshold);
		return 1;
	} else if (streq(opt, "no-opt")) {
		disable_all_opts();
		return 1;
	}

	size_t const len = strlen(opt);
	for (size_t i = ARRAY_SIZE(firm_options); i != 0;) {
		struct params const* const o = &firm_options[--i];
		if (len == o->opt_len && memcmp(opt, o->option, len) == 0) {
			/* statistic options do accumulate */
			if (o->flag == &firm_dump.statistic)
				*o->flag = (bool) (*o->flag | o->set);
			else
				*o->flag = o->set;

			return 1;
		}
	}

	/* maybe this enables/disables optimizations */
	if (firm_opt_option(opt))
		return 1;

	return 0;
}

static void set_be_option(const char *arg)
{
	int res = be_parse_arg(arg);
	(void) res;
	assert(res);
}

static void set_option(const char *arg)
{
	int res = firm_option(arg);
	(void) res;
	assert(res);
}

void choose_optimization_pack(int level)
{
	/* apply optimization level */
	switch(level) {
	case 0:
		set_option("no-opt");
		break;
	case 1:
		set_option("no-inline");
		break;
	default:
	case 4:
		/* use_builtins = true; */
		/* fallthrough */
	case 3:
		set_option("thread-jumps");
		set_option("if-conversion");
		/* fallthrough */
	case 2:
		set_option("inline");
		set_option("fp-vrp");
		set_option("occults");
		set_option("deconv");
		set_be_option("omitfp");
		break;
	}
}

void init_implicit_optimizations(void)
{
	set_optimize(1);
	set_opt_constant_folding(firm_opt.const_folding);
	set_opt_algebraic_simplification(firm_opt.const_folding);
	set_opt_cse(firm_opt.cse);
	set_opt_global_cse(0);
}
