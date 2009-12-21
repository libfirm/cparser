/**
 *
 * @file firm_opt.c -- Firm-generating back end optimizations.
 *
 * (C) 2005-2009  Michael Beck   beck@ipd.info.uni-karlsruhe.de
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

#if defined(_DEBUG) || defined(FIRM_DEBUG)
#define DBG(x)  dbg_printf x
#else
#define DBG(x) ((void)0)
#endif /* _DEBUG || FIRM_DEBUG */

static ir_timer_t *t_vcg_dump;
static ir_timer_t *t_verify;
static ir_timer_t *t_all_opt;
static bool do_irg_opt(ir_graph *irg, const char *name);

/** dump all the graphs depending on cond */
#define DUMP_ALL(cond, suffix)                             \
  do {                                                     \
    if (cond) {                                            \
      timer_push(t_vcg_dump);                              \
      if (firm_dump.no_blocks)                             \
        dump_all_ir_graphs(dump_ir_graph, suffix);         \
      else if (firm_dump.extbb)                            \
        dump_all_ir_graphs(dump_ir_extblock_graph, suffix);\
      else                                                 \
        dump_all_ir_graphs(dump_ir_block_graph, suffix);   \
      timer_pop(t_vcg_dump);                               \
    }                                                      \
  } while (0)

/** dump all control flow graphs depending on cond */
#define DUMP_ALL_CFG(cond, suffix)                      \
  do {                                                  \
    if (cond) {                                         \
      timer_push(t_vcg_dump);                           \
        dump_all_ir_graphs(dump_cfg, suffix);           \
      timer_pop(t_vcg_dump);                            \
    }                                                   \
  } while (0)

/** dump graphs irg depending on cond */
#define DUMP_ONE(cond, irg, suffix)                     \
  do {                                                  \
    if (cond) {                                         \
      timer_push(t_vcg_dump);                           \
      if (firm_dump.no_blocks)                          \
        dump_ir_graph(irg, suffix);                     \
      else if (firm_dump.extbb)                         \
        dump_ir_extblock_graph(irg, suffix);            \
      else                                              \
        dump_ir_block_graph(irg, suffix);               \
      timer_pop(t_vcg_dump);                            \
    }                                                   \
  } while (0)

/** dump control flow graph irg depending on cond */
#define DUMP_ONE_CFG(cond, irg, suffix)                 \
  do {                                                  \
    if (cond) {                                         \
      timer_push(t_vcg_dump);                           \
      dump_cfg(irg, suffix);                            \
      timer_pop(t_vcg_dump);                            \
    }                                                   \
  } while (0)

/* set by the backend parameters */
static const ir_settings_arch_dep_t *ad_param              = NULL;
static create_intrinsic_fkt         *arch_create_intrinsic = NULL;
static void                         *create_intrinsic_ctx  = NULL;
static const ir_settings_if_conv_t  *if_conv_info          = NULL;

/* entities of runtime functions */
ir_entity_ptr rts_entities[rts_max];

/**
 * factory for setting architecture dependent parameters
 */
static const ir_settings_arch_dep_t *arch_factory(void)
{
  static const ir_settings_arch_dep_t param = {
      1,   /* also use subs */
      4,   /* maximum shifts */
     31,   /* maximum shift amount */
     NULL, /* use default evaluator */

      1, /* allow Mulhs */
      1, /* allow Mulus */
     32  /* Mulh allowed up to 32 bit */
  };

  return ad_param ? ad_param : &param;
}

/**
 * Map runtime functions.
 */
static void rts_map(void) {
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

  for (i = n_map = 0; i < sizeof(mapper)/sizeof(mapper[0]); ++i)
    if (*mapper[i].ent != NULL) {
      rec[n_map].i_call.kind     = INTRINSIC_CALL;
      rec[n_map].i_call.i_ent    = *mapper[i].ent;
      rec[n_map].i_call.i_mapper = mapper[i].func;
      rec[n_map].i_call.ctx      = NULL;
      rec[n_map].i_call.link     = NULL;
      ++n_map;
  }  /* if */
  if (n_map > 0)
    lower_intrinsics(rec, n_map, /* part_block_used=*/0);
}  /* rts_map */

static int *irg_dump_no;

static void dump_graph_count(ir_graph *const irg, const char *const suffix)
{
  char name[64];
  snprintf(name, sizeof(name), "-%02d_%s", irg_dump_no[get_irg_idx(irg)]++, suffix);
  DUMP_ONE(1, irg, name);
}

static void dump_all_count(const char *const suffix)
{
  const int n_irgs = get_irp_n_irgs();
  int i;

  for (i = 0; i < n_irgs; ++i)
    dump_graph_count(get_irp_irg(i), suffix);
}

#define DUMP_ONE_C(cond, irg, suffix)    \
  do {                                   \
    if (cond) {                          \
      dump_graph_count((irg), (suffix)); \
    }                                    \
  } while (0)

#define DUMP_ALL_C(cond, suffix) \
  do {                           \
    if (cond) {                  \
      dump_all_count((suffix));  \
    }                            \
  } while (0)

static void remove_unused_functions(void)
{
	ir_entity **keep_methods;
	int         arr_len;

	/* Analysis that finds the free methods,
	   i.e. methods that are dereferenced.
	   Optimizes polymorphic calls :-). */
	cgana(&arr_len, &keep_methods);

	/* Remove methods that are never called. */
	gc_irgs(arr_len, keep_methods);
	free(keep_methods);
}

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

static void do_if_conv(ir_graph *irg)
{
	opt_if_conv(irg, if_conv_info);
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

static void do_lower_switch(ir_graph *irg)
{
	lower_switch(irg, firm_opt.spare_size);
}

static void do_lower_dw_ops(void)
{
	lwrdw_param_t init = {
		1,
		1,
		get_atomic_mode(ATOMIC_TYPE_LONGLONG),
		get_atomic_mode(ATOMIC_TYPE_ULONGLONG),
		get_atomic_mode(ATOMIC_TYPE_INT),
		get_atomic_mode(ATOMIC_TYPE_UINT),
		def_create_intrinsic_fkt,
		NULL
	};

	if (arch_create_intrinsic) {
		init.create_intrinsic = arch_create_intrinsic;
		init.ctx              = create_intrinsic_ctx;
	}
	lower_dw_ops(&init);
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
} opt_flags_t;

typedef void (*transform_irg_func)(ir_graph *irg);
typedef void (*transform_irp_func)(void);
typedef void (*func_ptr_t)(void);

typedef struct {
	opt_target_t  target;
	const char   *name;
	func_ptr_t    func;
	const char   *description;
	opt_flags_t   flags;
} opt_config_t;

static opt_config_t opts[] = {
	{ OPT_TARGET_IRP, "rts",             (func_ptr_t) rts_map,                 "optimization of known library functions", OPT_FLAG_HIDE_OPTIONS },
	{ OPT_TARGET_IRG, "combo",           (func_ptr_t) combo,                   "combined CCE, UCE and GVN",               OPT_FLAG_NONE},
	{ OPT_TARGET_IRG, "control-flow",    (func_ptr_t) optimize_cf,             "optimization of control-flow",            OPT_FLAG_HIDE_OPTIONS },
	{ OPT_TARGET_IRG, "local",           (func_ptr_t) optimize_graph_df,       "local graph optimizations",               OPT_FLAG_HIDE_OPTIONS },
	{ OPT_TARGET_IRP, "remove-unused",   (func_ptr_t) remove_unused_functions, "removal of unused functions",             OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY },
	{ OPT_TARGET_IRP, "opt-tail-rec",    (func_ptr_t) opt_tail_recursion,      "tail-recursion eliminiation",             OPT_FLAG_NONE },
	{ OPT_TARGET_IRP, "opt-func-call",   (func_ptr_t) do_optimize_funccalls,   "function call optimization",              OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "lower",           (func_ptr_t) do_lower_highlevel,      "lowering",                                OPT_FLAG_HIDE_OPTIONS },
	{ OPT_TARGET_IRP, "lower-const",     (func_ptr_t) lower_const_code,        "lowering of constant code",               OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY },
	{ OPT_TARGET_IRP, "lower-dw",        (func_ptr_t) do_lower_dw_ops,         "lowering of doubleword operations",       OPT_FLAG_HIDE_OPTIONS },
	{ OPT_TARGET_IRG, "lower-switch",    (func_ptr_t) do_lower_switch,         "switch lowering",                         OPT_FLAG_HIDE_OPTIONS },
	{ OPT_TARGET_IRG, "one-return",      (func_ptr_t) normalize_one_return,    "normalisation to 1 return",               OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY },
	{ OPT_TARGET_IRG, "scalar-replace",  (func_ptr_t) scalar_replacement_opt,  "scalar replacement",                      OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "reassociation",   (func_ptr_t) optimize_reassociation,  "reassociation",                           OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "gcse",            (func_ptr_t) do_gcse,                 "global common subexpression elimination", OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "place",           (func_ptr_t) place_code,              "code placement",                          OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "confirm",         (func_ptr_t) construct_confirms,      "confirm optimisation",                    OPT_FLAG_HIDE_OPTIONS },
	{ OPT_TARGET_IRG, "opt-load-store",  (func_ptr_t) optimize_load_store,     "load store optimization",                 OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "parallelize-mem", (func_ptr_t) opt_parallelize_mem,     "parallelize memory",                      OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "deconv",          (func_ptr_t) conv_opt,                "conv node elimination",                   OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "thread-jumps",    (func_ptr_t) opt_jumpthreading,       "path-sensitive jumpthreading",            OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "remove-confirms", (func_ptr_t) remove_confirms,         "confirm removal",                         OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY },
	{ OPT_TARGET_IRG, "gvn-pre",         (func_ptr_t) do_gvn_pre,              "global value numbering partial redundancy elimination", OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "if-conversion",   (func_ptr_t) do_if_conv,              "if-conversion",                           OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "bool",            (func_ptr_t) opt_bool,                "bool simplification",                     OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "shape-blocks",    (func_ptr_t) shape_blocks,            "block shaping",                           OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "ivopts",          (func_ptr_t) do_stred,                "induction variable strength reduction",   OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "remove-phi-cycles", (func_ptr_t) remove_phi_cycles,     "removal of phi cycles",                   OPT_FLAG_HIDE_OPTIONS },
	{ OPT_TARGET_IRG, "dead",            (func_ptr_t) dead_node_elimination,   "dead node elimination",                   OPT_FLAG_HIDE_OPTIONS | OPT_FLAG_NO_DUMP | OPT_FLAG_NO_VERIFY },
	{ OPT_TARGET_IRP, "inline",          (func_ptr_t) do_inline,               "inlining",                                OPT_FLAG_NONE },
	{ OPT_TARGET_IRP, "opt-proc-clone",  (func_ptr_t) do_cloning,              "procedure cloning",                       OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "invert-loops",    (func_ptr_t) do_loop_inversion,       "loop inversion",                          OPT_FLAG_NONE },
	{ OPT_TARGET_IRG, "peel-loops",      (func_ptr_t) do_loop_peeling,         "loop peeling",                            OPT_FLAG_NONE },
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
	transform_irg_func  func;
	ir_graph           *old_irg;
	opt_config_t       *config = get_opt(name);
	size_t              n      = config - opts;
	assert(config != NULL);
	assert(config->target == OPT_TARGET_IRG);
	if (! (config->flags & OPT_FLAG_ENABLED))
		return false;


	old_irg          = current_ir_graph;
	current_ir_graph = irg;

	func = (transform_irg_func) config->func;

	timer_push(timers[n]);
	func(irg);
	timer_pop(timers[n]);

	DUMP_ONE_C(firm_dump.ir_graph && firm_dump.all_phases, irg, config->name);

	if (firm_opt.check_all) {
		timer_push(t_verify);
		irg_verify(irg, VRFY_ENFORCE_SSA);
		timer_pop(t_verify);
	}

	current_ir_graph = old_irg;
	return true;
}

static void do_irp_opt(const char *name)
{
	transform_irp_func  func;
	opt_config_t       *config = get_opt(name);
	size_t              n      = config - opts;
	assert(config->target == OPT_TARGET_IRP);
	if (! (config->flags & OPT_FLAG_ENABLED))
		return;

	func = (transform_irp_func) config->func;

	timer_push(timers[n]);
	func();
	timer_pop(timers[n]);

	DUMP_ALL_C(firm_dump.ir_graph && firm_dump.all_phases, config->name);

	if (firm_opt.check_all) {
		int i;
		timer_push(t_verify);
		for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
			irg_verify(get_irp_irg(i), VRFY_ENFORCE_SSA);
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
	set_opt_enabled("lower-switch", true);
	set_opt_enabled("remove-phi-cycles", true);
}

/**
 * run all the Firm optimizations
 *
 * @param input_filename     the name of the (main) source file
 */
static void do_firm_optimizations(const char *input_filename)
{
  int      i;
  unsigned aa_opt;

  /* FIXME: cloning might ADD new graphs. */
  irg_dump_no = calloc(get_irp_last_idx(), sizeof(*irg_dump_no));

  set_opt_alias_analysis(firm_opt.alias_analysis);

  aa_opt = aa_opt_no_opt;
  if (firm_opt.strict_alias)
    aa_opt |= aa_opt_type_based | aa_opt_byte_type_may_alias;
  if (firm_opt.no_alias)
    aa_opt = aa_opt_no_alias;

  set_irp_memory_disambiguator_options(aa_opt);

  /* parameter passing code should set them directly sometime... */
  set_opt_enabled("rts", !firm_opt.freestanding);
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
  do_irp_opt("opt-tail-rec");
  do_irp_opt("opt-func-call");
  do_irp_opt("lower-const");

  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_graph *irg = get_irp_irg(i);

    do_irg_opt(irg, "scalar-replace");
    do_irg_opt(irg, "invert-loops");
    do_irg_opt(irg, "local");
    do_irg_opt(irg, "reassociation");
    do_irg_opt(irg, "local");
    do_irg_opt(irg, "gcse");

    if (firm_opt.confirm) {
      /* Confirm construction currently can only handle blocks with only one
      	 control flow predecessor. Calling optimize_cf here removes Bad
      	 predecessors and help the optimization of switch constructs. */
      do_irg_opt(irg, "control-flow");
      do_irg_opt(irg, "confirm");
      do_irg_opt(irg, "local");
    }

    do_irg_opt(irg, "control-flow");
    do_irg_opt(irg, "opt-load-store");
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

    do_irg_opt(irg, "bool");
    do_irg_opt(irg, "shape-blocks");
    do_irg_opt(irg, "lower-switch");
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
  }

  if (firm_dump.ir_graph) {
    /* recompute backedges for nicer dumps */
    for (i = 0; i < get_irp_n_irgs(); i++)
      construct_cf_backedges(get_irp_irg(i));
  }

  do_irp_opt("remove-unused");

  DUMP_ALL(firm_dump.ir_graph, "-opt");

  if (firm_dump.statistic & STAT_AFTER_OPT)
    stat_dump_snapshot(input_filename, "opt");

  timer_stop(t_all_opt);
}

/**
 * compute the size of a type (do implicit lowering)
 *
 * @param ty   a Firm type
 */
static int compute_type_size(ir_type *ty)
{
  optimization_state_t state;
  unsigned             align_all = 1;
  int                  n, size = 0, set = 0;
  int                  i, dims, s;

  if (get_type_state(ty) == layout_fixed) {
    /* do not layout already layouted types again */
    return 1;
  }

  if (is_Method_type(ty) || ty == get_glob_type()) {
    /* no need for size calculation for method types or the global type */
    return 1;
  }

  DBG(("compute type size visiting: %s\n", get_type_name(ty)));

  switch (get_type_tpop_code(ty)) {
  case tpo_class:
  case tpo_struct:
    for (i = 0, n = get_compound_n_members(ty); i < n; ++i) {
      ir_entity *ent  = get_compound_member(ty, i);
      ir_type *ent_ty = get_entity_type(ent);
      unsigned align, misalign;

      /* inner functions do not expand the frame */
      if (is_Method_type(ent_ty) && is_frame_type(ty))
        continue;

      /* compute member types */
      if (! compute_type_size(ent_ty))
        return 0;

      align     = get_type_alignment_bytes(ent_ty);
      align_all = align > align_all ? align : align_all;
      misalign  = (align ? size % align : 0);
      size     += (misalign ? align - misalign : 0);

      set_entity_offset(ent, size);
      size += get_type_size_bytes(ent_ty);

      DBG(("  member %s %s -> (size: %u, align: %u)\n",
            get_type_name(ent_ty), get_entity_name(ent),
            get_type_size_bytes(ent_ty), get_type_alignment_bytes(ent_ty)));
    }
    if (align_all > 0 && size % align_all) {
       DBG(("align of the struct member: %u, type size: %d\n", align_all, size));
       size += align_all - (size % align_all);
       DBG(("correcting type-size to %d\n", size));
    }
    set_type_alignment_bytes(ty, align_all);
    set = 1;
    break;

  case tpo_union:
    for (i = 0, n = get_union_n_members(ty); i < n; ++i) {
      ir_entity *ent = get_union_member(ty, i);

      if (! compute_type_size(get_entity_type(ent)))
        return 0;
      s = get_type_size_bytes(get_entity_type(ent));

      set_entity_offset(ent, 0);
      size = (s > size ? s : size);
    }
    set = 1;
    break;

  case tpo_array:
    dims = get_array_n_dimensions(ty);

    if (! compute_type_size(get_array_element_type(ty)))
      return 0;

    size = 1;

    save_optimization_state(&state);
    set_optimize(1);
    set_opt_constant_folding(1);
	set_opt_algebraic_simplification(1);

    for (i = 0; i < dims; ++i) {
      ir_node *lower   = get_array_lower_bound(ty, i);
      ir_node *upper   = get_array_upper_bound(ty, i);
      ir_graph *rem    = current_ir_graph;
      tarval  *tv_lower, *tv_upper;
      long     val_lower, val_upper;

      current_ir_graph = get_const_code_irg();
      local_optimize_node(lower);
      local_optimize_node(upper);
      current_ir_graph = rem;

      tv_lower = computed_value(lower);
      tv_upper = computed_value(upper);

      if (tv_lower == tarval_bad || tv_upper == tarval_bad) {
        /*
         * we cannot calculate the size of this array yet, it
         * even might be unknown until the end, like argv[]
         */
        restore_optimization_state(&state);
        return 0;
      }

      val_upper = get_tarval_long(tv_upper);
      val_lower = get_tarval_long(tv_lower);
      size     *= val_upper - val_lower;
    }
    restore_optimization_state(&state);

    DBG(("array %s -> (elements: %d, element type size: %d)\n",
          get_type_name(ty),
          size, get_type_size_bytes(get_array_element_type(ty))));
    size *= get_type_size_bytes(get_array_element_type(ty));
    set = 1;
    break;

  default:
    break;
  }

  if (set) {
    set_type_size_bytes(ty, size);
    set_type_state(ty, layout_fixed);
  }

  DBG(("size: %d\n", get_type_size_bytes(ty)));

  return set;
}  /* compute_type_size */

/**
 * layout all non-frame types of the Firm graph
 */
static void compute_type_sizes(void)
{
  int i;
  ir_type *tp;

  /* all non-frame other types */
  for (i = get_irp_n_types() - 1; i >= 0; --i) {
    tp = get_irp_type(i);
    compute_type_size(tp);

    if (is_Method_type(tp)) {
      tp = get_method_value_res_type(tp);

      if (tp) {
        /* we have a value result type for this method, lower */
        compute_type_size(tp);
      }
    }
  }
}  /* compute_type_sizes */

/**
 * layout all frame-types of the Firm graph
 */
static void compute_frame_type_sizes(void)
{
  int i;
  ir_graph *irg;

  /* all frame types */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    irg = get_irp_irg(i);
    /* do not optimize away variables in debug mode */
    if (firm_opt.debug_mode == DBG_MODE_NONE)
      opt_frame_irg(irg);
    compute_type_size(get_irg_frame_type(irg));
  }
}  /* compute_frame_type_sizes */

/**
 * do Firm lowering
 *
 * @param input_filename  the name of the (main) source file
 */
static void do_firm_lowering(const char *input_filename)
{
  int i;

  do_irp_opt("lower-dw");

  if (firm_dump.statistic & STAT_AFTER_LOWER)
    stat_dump_snapshot(input_filename, "low");

  DUMP_ALL(firm_dump.ir_graph, "-low");

  if (firm_opt.enabled) {
    timer_start(t_all_opt);

    /* run reassociation first on all graphs BEFORE the architecture dependent optimizations
       are enabled */
    for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
      ir_graph *irg = get_irp_irg(i);
	  do_irg_opt(irg, "reassociation");
	}

    /* enable architecture dependent optimizations */
    arch_dep_set_opts((arch_dep_opts_t)
                      ((firm_opt.muls ? arch_dep_mul_to_shift : arch_dep_none) |
                      (firm_opt.divs ? arch_dep_div_by_const : arch_dep_none) |
                      (firm_opt.mods ? arch_dep_mod_by_const : arch_dep_none) ));

    for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
      ir_graph *irg = get_irp_irg(i);

      current_ir_graph = irg;

      do_irg_opt(irg, "local");
      do_irg_opt(irg, "gcse");
      do_irg_opt(irg, "opt-load-store");
      do_irg_opt(irg, "local");
      do_irg_opt(irg, "control-flow");

      if (do_irg_opt(irg, "if-conversion")) {
        do_irg_opt(irg, "local");
        do_irg_opt(irg, "control-flow");
      }

      do_irg_opt(irg, "parallelize-mem");
    }
    timer_stop(t_all_opt);

    DUMP_ALL(firm_dump.ir_graph, "-low-opt");
  }

  if (firm_opt.cc_opt)
    mark_private_methods();

  /* set the phase to low */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i)
    set_irg_phase_low(get_irp_irg(i));

  /* all graphs are lowered, set the irp phase to low */
  set_irp_phase_state(phase_low);

  if (firm_dump.statistic & STAT_FINAL) {
    stat_dump_snapshot(input_filename, "final");
  }
}

/**
 * Initialize for the Firm-generating back end.
 */
void gen_firm_init(void)
{
  firm_parameter_t params;
  unsigned         pattern = 0;
  int              i;

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

  memset(&params, 0, sizeof(params));
  params.size                  = sizeof(params);
  params.enable_statistics     = firm_dump.statistic == STAT_NONE ? 0 :
    FIRMSTAT_ENABLED | FIRMSTAT_COUNT_STRONG_OP | FIRMSTAT_COUNT_CONSTS | pattern;
  params.initialize_local_func = uninitialized_local_var;
  params.cc_mask               = 0; /* no regparam, cdecl */
  params.builtin_dbg           = NULL;

  ir_init(&params);

  if (firm_be_opt.selection == BE_FIRM_BE) {
    const backend_params *be_params = be_get_backend_param();

	if (be_params->do_dw_lowering)
		set_opt_enabled("lower-dw", true);

    arch_create_intrinsic   = be_params->arch_create_intrinsic_fkt;
    create_intrinsic_ctx    = be_params->create_intrinsic_ctx;

    ad_param                = be_params->dep_param;
    if_conv_info            = be_params->if_conv_info;
  }

  dbg_init(NULL, NULL, dbg_snprint);
  edges_init_dbg(firm_opt.vrfy_edges);

  /* Sel node cannot produce NULL pointers */
  set_opt_sel_based_null_check_elim(1);

  /* dynamic dispatch works currently only if whole world scenarios */
  set_opt_dyn_meth_dispatch(0);

  arch_dep_init(arch_factory);

  /* do not run architecture dependent optimizations in building phase */
  arch_dep_set_opts(arch_dep_none);

  do_node_verification((firm_verification_t) firm_opt.vrfy);
  if (firm_dump.filter)
    only_dump_method_with_name(new_id_from_str(firm_dump.filter));

  if (firm_opt.enabled) {
    set_optimize(1);
    set_opt_constant_folding(firm_opt.const_folding);
    set_opt_algebraic_simplification(firm_opt.const_folding);
    set_opt_cse(firm_opt.cse);
    set_opt_global_cse(0);
    set_opt_unreachable_code(1);
    set_opt_control_flow(firm_opt.control_flow);
    set_opt_control_flow_weak_simplification(1);
    set_opt_control_flow_strong_simplification(1);
  } else {
    set_optimize(0);
  }

  /* do not dump entity ld names */
  dump_ld_names(0);
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
void gen_firm_finish(FILE *out, const char *input_filename, int c_mode, int new_firm_const_exists)
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

  /* the general for dumping option must be set, or the others will not work */
  firm_dump.ir_graph
      = (a_byte) (firm_dump.ir_graph | firm_dump.all_phases | firm_dump.extbb);

  dump_keepalive_edges(1);
  dump_consts_local(1);
  dump_dominator_information(1);
  dump_loop_information(0);

  if (!firm_dump.edge_labels)
    turn_off_edge_labels();

  if (firm_dump.all_types) {
    dump_all_types("");
    if (! c_mode) {
      dump_class_hierarchy(0, "");
      dump_class_hierarchy(1, "-with-entities");
    }
  }

  /* finalize all graphs */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph *irg = get_irp_irg(i);

    irg_finalize_cons(irg);
    DUMP_ONE(firm_dump.ir_graph, irg, "");
  }

  timer_push(t_verify);
  tr_vrfy();
  timer_pop(t_verify);

  /* all graphs are finalized, set the irp phase to high */
  set_irp_phase_state(phase_high);

  /* BEWARE: kill unreachable code before doing compound lowering */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph *irg = get_irp_irg(i);
	do_irg_opt(irg, "control-flow");
  }

  /* lower all compound call return values */
  lower_compound_params();

  /* computes the sizes of all types that are still not computed */
  compute_type_sizes();

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

  if (firm_dump.gen_firm_asm) {
  	ir_timer_t *timer = ir_timer_new();
  	timer_register(timer, "Firm: Firm assembler");
  	timer_push(timer);
    gen_Firm_assembler(input_filename);
    timer_pop(timer);
    return;
  }

  if (firm_opt.lower)
    do_firm_lowering(input_filename);

  /* computes the sizes of all frame types */
  compute_frame_type_sizes();

  /* set the phase to low */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i)
    set_irg_phase_low(get_irp_irg(i));

  if (firm_dump.statistic & STAT_FINAL_IR)
    stat_dump_snapshot(input_filename, "final-ir");

  /* run the code generator */
  if (firm_be_opt.selection != BE_NONE)
    do_codegen(out, input_filename);

  if (firm_dump.statistic & STAT_FINAL)
    stat_dump_snapshot(input_filename, "final");
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
