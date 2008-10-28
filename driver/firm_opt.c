/**
 *
 * @file firm_opt.c -- Firm-generating back end optimizations.
 *
 * (C) 2005-2007  Michael Beck   beck@ipd.info.uni-karlsruhe.de
 *
 * $Id$
 */
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <libfirm/firm.h>
#include <libfirm/be.h>


#include "firm_opt.h"
#include "firm_codegen.h"
#include "firm_cmdline.h"
#include "firm_timing.h"

#ifdef _WIN32
#define snprintf _snprintf
#endif

#if defined(_DEBUG) || defined(FIRM_DEBUG)
#define DBG(x)  dbg_printf x
#else
#define DBG(x)
#endif /* _DEBUG || FIRM_DEBUG */


/** dump all the graphs depending on cond */
#define DUMP_ALL(cond, suffix)                             \
  do {                                                     \
    if (cond) {                                            \
      timer_push(TV_VCG_DUMP);                             \
      if (firm_dump.no_blocks)                             \
        dump_all_ir_graphs(dump_ir_graph, suffix);         \
      else if (firm_dump.extbb)                            \
        dump_all_ir_graphs(dump_ir_extblock_graph, suffix);\
      else                                                 \
        dump_all_ir_graphs(dump_ir_block_graph, suffix);   \
      timer_pop();                                         \
    }                                                      \
  } while (0)

/** dump all control flow graphs depending on cond */
#define DUMP_ALL_CFG(cond, suffix)                      \
  do {                                                  \
    if (cond) {                                         \
      timer_push(TV_VCG_DUMP);                          \
        dump_all_ir_graphs(dump_cfg, suffix);           \
      timer_pop();                                      \
    }                                                   \
  } while (0)

/** check all graphs depending on cond */
#define CHECK_ALL(cond)                                 \
  do {                                                  \
    if (cond) {                                         \
      int ii;                                           \
      timer_push(TV_VERIFY);                            \
      for (ii = get_irp_n_irgs() - 1; ii >= 0; --ii)    \
        irg_verify(get_irp_irg(ii), VRFY_ENFORCE_SSA);  \
      timer_pop();                                      \
    }                                                   \
  } while (0)



/** dump graphs irg depending on cond */
#define DUMP_ONE(cond, irg, suffix)                     \
  do {                                                  \
    if (cond) {                                         \
      timer_push(TV_VCG_DUMP);                          \
      if (firm_dump.no_blocks)                          \
        dump_ir_graph(irg, suffix);                     \
      else if (firm_dump.extbb)                         \
        dump_ir_extblock_graph(irg, suffix);            \
      else                                              \
        dump_ir_block_graph(irg, suffix);               \
      timer_pop();                                      \
    }                                                   \
  } while (0)

/** dump control flow graph irg depending on cond */
#define DUMP_ONE_CFG(cond, irg, suffix)                 \
  do {                                                  \
    if (cond) {                                         \
      timer_push(TV_VCG_DUMP);                          \
      dump_cfg(irg, suffix);                            \
      timer_pop();                                      \
    }                                                   \
  } while (0)

/** check a graph irg depending on cond */
#define CHECK_ONE(cond, irg)                            \
  do {                                                  \
    if (cond) {                                         \
      timer_push(TV_VERIFY);                            \
        irg_verify(irg, VRFY_ENFORCE_SSA);              \
      timer_pop();                                      \
    }                                                   \
  } while (0)


/* set by the backend parameters */
static const ir_settings_arch_dep_t *ad_param = NULL;
static create_intrinsic_fkt *arch_create_intrinsic = NULL;
static void *create_intrinsic_ctx = NULL;
static const ir_settings_if_conv_t *if_conv_info = NULL;

ir_mode *firm_imm_fp_mode = NULL;

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
    { &rts_entities[rts_memcpy],  i_mapper_memcpy },
    { &rts_entities[rts_memset],  i_mapper_memset },
    { &rts_entities[rts_strcmp],  i_mapper_strcmp },
    { &rts_entities[rts_strncmp], i_mapper_strncmp },
    { &rts_entities[rts_strlen],  i_mapper_strlen }
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

#if 0
static void dump_graph_cfg_count(ir_graph *const irg, const char *const suffix)
{
  char name[64];
  snprintf(name, sizeof(name), "-%02d_%s", irg_dump_no[get_irg_idx(irg)]++, suffix);
  DUMP_ONE_CFG(1, irg, name);
}
#endif

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

#define DUMP_ONE_CFG_C(cond, irg, suffix)    \
  do {                                       \
    if (cond) {                              \
      dump_graph_cfg_count((irg), (suffix)); \
    }                                        \
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

static void do_inline(void)
{
	inline_functions(firm_opt.inline_maxsize, firm_opt.inline_threshold);
}

static void do_cloning(void)
{
	proc_cloning((float) firm_opt.clone_threshold);
}

static void do_lower_switch(ir_graph *irg)
{
	lower_switch(irg, firm_opt.spare_size);
}

typedef enum opt_target {
	OPT_TARGET_IRG,
	OPT_TARGET_IRP
} opt_target;

typedef void (*transform_irg_func)(ir_graph *irg);
typedef void (*transform_irp_func)(void);
typedef void (*func_ptr_t)(void);

typedef struct {
	opt_target  target;
	const char *name;
	func_ptr_t  func;
	bool        enabled;
	bool        dump;
	bool        verify;
	int         timer;
} opt_config_t;

static opt_config_t opts[] = {
	{ OPT_TARGET_IRP, "rts",         (func_ptr_t) rts_map,                 true, true,  true,  -1 },
	{ OPT_TARGET_IRG, "combo",       (func_ptr_t) combo,                   true, true,  true,  TV_COMBO },
	{ OPT_TARGET_IRG, "controlflow", (func_ptr_t) optimize_cf,             true, true,  true,  TV_CF_OPT },
	{ OPT_TARGET_IRG, "local",       (func_ptr_t) optimize_graph_df,       true, true,  true,  TV_LOCAL_OPT },
	{ OPT_TARGET_IRP, "gc_irgs",     (func_ptr_t) remove_unused_functions, true, false, false, TV_CGANA },
	{ OPT_TARGET_IRP, "tailrec",     (func_ptr_t) opt_tail_recursion,      true, true,  true,  TV_TAIL_REC },
	{ OPT_TARGET_IRP, "funccalls",   (func_ptr_t) do_optimize_funccalls,   true, true,  true,  TV_REAL_FUNC_CALL },
	{ OPT_TARGET_IRP, "lowerconst",  (func_ptr_t) lower_const_code,        true, false, false, -1 },
	{ OPT_TARGET_IRG, "onereturn",   (func_ptr_t) normalize_one_return,    true, false, false, -1 },
	{ OPT_TARGET_IRG, "scalar",      (func_ptr_t) scalar_replacement_opt,  true, true,  true,  TV_SCALAR_REPLACE },
	{ OPT_TARGET_IRG, "reassoc",     (func_ptr_t) optimize_reassociation,  true, true,  true,  TV_REASSOCIATION },
	{ OPT_TARGET_IRG, "gcse",        (func_ptr_t) do_gcse,                 true, true,  true,  TV_CODE_PLACE },
	{ OPT_TARGET_IRG, "place",       (func_ptr_t) place_code,              true, true,  true,  TV_CODE_PLACE },
	{ OPT_TARGET_IRG, "confirm",     (func_ptr_t) construct_confirms,      true, true,  true,  TV_CONFIRM_CREATE },
	{ OPT_TARGET_IRG, "ldst",        (func_ptr_t) optimize_load_store,     true, true,  true,  TV_LOAD_STORE },
	{ OPT_TARGET_IRG, "sync",        (func_ptr_t) opt_sync,                true, true,  true,  -1 },
	{ OPT_TARGET_IRG, "lower",       (func_ptr_t) do_lower_highlevel,      true, true,  true,  -1 },
	{ OPT_TARGET_IRG, "deconv",      (func_ptr_t) conv_opt,                true, true,  true,  TV_DECONV },
	{ OPT_TARGET_IRG, "condeval",    (func_ptr_t) opt_cond_eval,           true, true,  true,  TV_COND_EVAL },
	{ OPT_TARGET_IRG, "remove_confirms", (func_ptr_t) remove_confirms,     true, false, false, TV_CONFIRM_CREATE },
	{ OPT_TARGET_IRG, "gvnpre",      (func_ptr_t) do_gvn_pre,              true, true,  true,  -1 },
	{ OPT_TARGET_IRG, "ifconv",      (func_ptr_t) do_if_conv,              true, true,  true,  TV_IF_CONV },
	{ OPT_TARGET_IRG, "bool",        (func_ptr_t) opt_bool,                true, true,  true,  -1 },
	{ OPT_TARGET_IRG, "end-melt",    (func_ptr_t) melt_end_blocks,         true, true,  true,  TV_END_MELT },
	{ OPT_TARGET_IRG, "stred",       (func_ptr_t) do_stred,                true, true,  true,  TV_OSR },
	{ OPT_TARGET_IRG, "dead",        (func_ptr_t) dead_node_elimination,   true, false, true,  TV_DEAD_NODE },
	{ OPT_TARGET_IRP, "inline",      (func_ptr_t) do_inline,               true, true,  true,  -1 },
	{ OPT_TARGET_IRP, "clone",       (func_ptr_t) do_cloning,              true, true,  true,  -1 },
	{ OPT_TARGET_IRG, "lower_switch", (func_ptr_t) do_lower_switch,        true, true,  true,  -1 },
};
static const int n_opts = sizeof(opts) / sizeof(opts[0]);

static opt_config_t *get_opt(const char *name)
{
	int i;
	for (i = 0; i < n_opts; ++i) {
		opt_config_t *config = &opts[i];
		if (strcmp(config->name, name) == 0)
			return config;
	}

	assert(0);
	return NULL;
}

static void set_opt_enabled(const char *name, bool enabled)
{
	opt_config_t *config = get_opt(name);
	config->enabled = enabled;
}

static void do_irg_opt(ir_graph *irg, const char *name)
{
	transform_irg_func  func;
	ir_graph           *old_irg;
	opt_config_t       *config = get_opt(name);
	assert(config->target == OPT_TARGET_IRG);
	if (!config->enabled)
		return;

	if (config->timer != -1)
		timer_push(config->timer);

	old_irg          = current_ir_graph;
	current_ir_graph = irg;

	func = (transform_irg_func) config->func;
	func(irg);

	if (config->timer != -1)
		timer_pop();

	DUMP_ONE_C(firm_dump.ir_graph && firm_dump.all_phases, irg, config->name);
	CHECK_ONE(firm_opt.check_all, irg);

	current_ir_graph = old_irg;
}

static void do_irp_opt(const char *name)
{
	transform_irp_func  func;
	opt_config_t       *config = get_opt(name);
	assert(config->target == OPT_TARGET_IRP);
	if (!config->enabled)
		return;

	if (config->timer != -1)
		timer_push(config->timer);

	func = (transform_irp_func) config->func;
	func();

	DUMP_ALL_C(firm_dump.ir_graph && firm_dump.all_phases, config->name);
	CHECK_ALL(firm_opt.check_all);

	if (config->timer != -1)
		timer_pop();
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

  set_opt_strength_red(firm_opt.strength_red);
  set_opt_scalar_replacement(firm_opt.scalar_replace);
  set_opt_auto_create_sync(firm_opt.auto_sync);
  set_opt_alias_analysis(firm_opt.alias_analysis);
  set_opt_combo(firm_opt.combo);

  aa_opt = aa_opt_no_opt;
  if (firm_opt.strict_alias)
    aa_opt |= aa_opt_type_based | aa_opt_byte_type_may_alias;
  if (firm_opt.no_alias)
    aa_opt = aa_opt_no_alias;

  set_irp_memory_disambiguator_options(aa_opt);

  /* parameter passing code should set them directly sometime... */
  set_opt_enabled("rts", firm_opt.freestanding);
  set_opt_enabled("gc_irgs", firm_opt.remove_unused);
  set_opt_enabled("tailrec", firm_opt.tail_rec);
  set_opt_enabled("funccalls", firm_opt.func_calls);
  set_opt_enabled("gcse", firm_opt.gcse);
  set_opt_enabled("place", !firm_opt.gcse);
  set_opt_enabled("confirm", firm_opt.confirm);
  set_opt_enabled("remove_confirms", firm_opt.confirm);
  set_opt_enabled("sync", firm_opt.sync);
  set_opt_enabled("ldst", firm_opt.load_store);
  set_opt_enabled("deconv", firm_opt.deconv);
  set_opt_enabled("condeval", firm_opt.cond_eval);
  set_opt_enabled("gvnpre", firm_opt.gvn_pre);
  set_opt_enabled("ifconv", firm_opt.if_conversion);
  set_opt_enabled("bool", firm_opt.bool_opt);
  set_opt_enabled("end-melt", firm_opt.end_melt);
  set_opt_enabled("inline", firm_opt.do_inline);
  set_opt_enabled("clone", firm_opt.cloning);
  set_opt_enabled("combo", firm_opt.combo);

  timer_start(TV_ALL_OPT);

  do_irp_opt("rts");

  /* first step: kill dead code */
  if (firm_opt.combo) {
    for (i = 0; i < get_irp_n_irgs(); i++) {
      ir_graph *irg = get_irp_irg(i);
      do_irg_opt(irg, "combo");
    }
  } else {
    for (i = 0; i < get_irp_n_irgs(); i++) {
      ir_graph *irg = get_irp_irg(i);
      do_irg_opt(irg, "local");
      do_irg_opt(irg, "controlflow");
    }
  }

  do_irp_opt("gc_irgs");
  do_irp_opt("tailrec");
  do_irp_opt("funccalls");
  do_irp_opt("lowerconst");

  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_graph *irg = get_irp_irg(i);

#ifdef FIRM_EXT_GRS
    /* If SIMD optimization is on, make sure we have only 1 return */
    if (firm_ext_grs.create_pattern || firm_ext_grs.simd_opt)
      do_irg_opt("onereturn");
#endif

    do_irg_opt(irg, "scalar");
    do_irg_opt(irg, "local");
    do_irg_opt(irg, "reassoc");
    do_irg_opt(irg, "local");
    do_irg_opt(irg, "gcse");

    if (firm_opt.confirm) {
      /* Confirm construction currently can only handle blocks with only one control
         flow predecessor. Calling optimize_cf here removes Bad predecessors and help
         the optimization of switch constructs. */
      do_irg_opt(irg, "controlflow");
      do_irg_opt(irg, "confirm");
      do_irg_opt(irg, "local");
    }

    do_irg_opt(irg, "controlflow");
    do_irg_opt(irg, "ldst");
    do_irg_opt(irg, "lower");
    do_irg_opt(irg, "deconv");
    do_irg_opt(irg, "condeval");
    do_irg_opt(irg, "lower_switch");
    do_irg_opt(irg, "remove_confirms");
    do_irg_opt(irg, "gvnpre");
    do_irg_opt(irg, "place");
    do_irg_opt(irg, "controlflow");

    if (firm_opt.if_conversion) {
      do_irg_opt(irg, "ifconv");
      do_irg_opt(irg, "local");
      do_irg_opt(irg, "controlflow");
    }

    do_irg_opt(irg, "bool");
    do_irg_opt(irg, "end-melt");
    do_irg_opt(irg, "stred");
    do_irg_opt(irg, "local");
    do_irg_opt(irg, "dead");
  }

  do_irp_opt("inline");
  do_irp_opt("clone");

  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_graph *irg = get_irp_irg(i);
    do_irg_opt(irg, "local");
    do_irg_opt(irg, "controlflow");
    do_irg_opt(irg, "condeval");
    do_irg_opt(irg, "local");
    do_irg_opt(irg, "controlflow");
  }

  if (firm_dump.ir_graph) {
    /* recompute backedges for nicer dumps */
    for (i = 0; i < get_irp_n_irgs(); i++)
      construct_cf_backedges(get_irp_irg(i));
  }

  do_irp_opt("gc_irgs");

  DUMP_ALL(firm_dump.ir_graph, "-opt");
  /* verify optimized graphs */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph *irg = get_irp_irg(i);
    irg_verify(irg, VRFY_ENFORCE_SSA);
  }

  if (firm_dump.statistic & STAT_AFTER_OPT)
    stat_dump_snapshot(input_filename, "opt");

  timer_stop(TV_ALL_OPT);
}  /* do_firm_optimizations */

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

  /* do class lowering and vtbl creation */
//  lower_classes_to_struct("vtbl", "m");

#if 0
  timer_push(TV_LOWER);
  lower_highlevel();
  timer_pop();
#endif

  if (firm_opt.lower_ll) {
    lwrdw_param_t init = {
      1,
      1,
      mode_Ls, mode_Lu,
      mode_Is, mode_Iu,
      def_create_intrinsic_fkt,
      NULL
    };


    if (arch_create_intrinsic) {
      init.create_intrinsic = arch_create_intrinsic;
      init.ctx              = create_intrinsic_ctx;
    }
    timer_push(TV_DW_LOWER);
      lower_dw_ops(&init);
    timer_pop();
  }

  if (firm_dump.statistic & STAT_AFTER_LOWER)
    stat_dump_snapshot(input_filename, "low");

  /* verify lowered graphs */
  timer_push(TV_VERIFY);
  for (i = get_irp_n_irgs() - 1; i >= 0; --i)
    irg_verify(get_irp_irg(i), VRFY_ENFORCE_SSA);
  timer_pop();

  DUMP_ALL(firm_dump.ir_graph, "-low");

  if (firm_opt.enabled) {
    timer_start(TV_ALL_OPT);

    /* run reassociation first on all graphs BEFORE the architecture dependent optimizations
       are enabled */
    for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
      ir_graph *irg = get_irp_irg(i);
	  do_irg_opt(irg, "reassoc");
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
	  do_irg_opt(irg, "ldst");
	  do_irg_opt(irg, "local");
	  do_irg_opt(irg, "controlflow");

      if (firm_opt.if_conversion) {
		  do_irg_opt(irg, "ifconv");
		  do_irg_opt(irg, "local");
		  do_irg_opt(irg, "controlflow");
	  }

	  do_irg_opt(current_ir_graph, "sync");
    }
    timer_stop(TV_ALL_OPT);

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
}  /* do_firm_lowering */

/**
 * Initialize for the Firm-generating back end.
 */
void gen_firm_init(void)
{
  firm_parameter_t params;
  unsigned         pattern = 0;

  /* the automatic state is only set if inlining is enabled */
  firm_opt.auto_inline = firm_opt.do_inline;

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

  #ifdef FIRM_EXT_GRS
  /* Activate Graph rewriting if SIMD optimization is turned on */
  /* This has to be done before init_firm() is called! */
  if (firm_ext_grs.simd_opt)
    ext_grs_activate();
#endif

  init_firm(&params);

  if (firm_be_opt.selection == BE_FIRM_BE) {
    const backend_params *be_params = be_get_backend_param();

    firm_opt.lower_ll       = (a_byte) be_params->do_dw_lowering;
    params.arch_op_settings = be_params->arch_op_settings;

    arch_create_intrinsic   = be_params->arch_create_intrinsic_fkt;
    create_intrinsic_ctx    = be_params->create_intrinsic_ctx;

    ad_param                = be_params->dep_param;
    if_conv_info            = be_params->if_conv_info;

    if (be_params->has_imm_fp_mode)
      firm_imm_fp_mode = be_params->imm_fp_mode;
  }
  /* OS option must be set to the backend */
  switch (firm_opt.os_support) {
  case OS_SUPPORT_MINGW:
    firm_be_option("ia32-gasmode=mingw");
    break;
  case OS_SUPPORT_MACHO:
    firm_be_option("ia32-gasmode=macho");
    break;
  case OS_SUPPORT_LINUX:
  default:
    firm_be_option("ia32-gasmode=linux");
    break;
  }

  dbg_init(NULL, NULL, dbg_snprint);
  edges_init_dbg(firm_opt.vrfy_edges);
  //cbackend_set_debug_retrieve(dbg_retrieve);

  set_opt_precise_exc_context(firm_opt.precise_exc);
  set_opt_fragile_ops(firm_opt.fragile_ops);

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
}  /* gen_firm_init */

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

    /* verify the graph */
    timer_push(TV_VERIFY);
      irg_verify(irg, VRFY_ENFORCE_SSA);
    timer_pop();
  }

  timer_push(TV_VERIFY);
    tr_vrfy();
  timer_pop();

  /* all graphs are finalized, set the irp phase to high */
  set_irp_phase_state(phase_high);

  /* BEWARE: kill unreachable code before doing compound lowering */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph *irg = get_irp_irg(i);
    optimize_cf(irg);
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
    timer_push(TV_FIRM_ASM);
      gen_Firm_assembler(input_filename);
    timer_pop();
    return;
  }

  if (firm_opt.lower)
    do_firm_lowering(input_filename);

  /* computes the sizes of all frame types */
  compute_frame_type_sizes();

  /* set the phase to low */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i)
    set_irg_phase_low(get_irp_irg(i));


#ifdef FIRM_EXT_GRS
  /** SIMD Optimization  Extensions **/

  /* Pattern creation step. No code has to be generated, so
     exit after pattern creation */
  if (firm_ext_grs.create_pattern) {
    ext_grs_create_pattern();
    exit(0);
  }

  /* SIMD optimization step. Uses graph patterns to find
     rich instructions and rewrite */
  if (firm_ext_grs.simd_opt)
    ext_grs_simd_opt();
#endif

  if (firm_dump.statistic & STAT_FINAL_IR)
    stat_dump_snapshot(input_filename, "final-ir");

  /* run the code generator */
  if (firm_be_opt.selection != BE_NONE)
    do_codegen(out, input_filename);

  if (firm_dump.statistic & STAT_FINAL)
    stat_dump_snapshot(input_filename, "final");

#if 0
  if (firm_opt.ycomp_dbg)
    firm_finish_ycomp_debugger();
#endif
}  /* gen_firm_finish */

/**
 * Do very early initializations
 */
void firm_early_init(void) {
  /* arg: need this here for command line options */
  be_opt_register();
  firm_init_options(NULL, 0, NULL);
}  /* firm_early_init */
