/**
 * @file firm_cmdline.c -- Additional Firm generating backend parameters
 *
 * Compile when BACK_END_IS_CP_FIRM_BE is defined
 *
 * (C) 2005  Michael Beck  beck@ipd.info.uni-karlsruhe.de
 *
 * $Id$
 */
#include <string.h>
#include "firm_cmdline.h"
#include <libfirm/firm.h>
#include <libfirm/be.h>

#ifdef _WIN32
#define DEFAULT_OS OS_SUPPORT_MINGW
#elif defined(__APPLE__)
#define DEFAULT_OS OS_SUPPORT_MACHO
#else
#define DEFAULT_OS OS_SUPPORT_LINUX
#endif

/* optimization settings */
struct a_firm_opt firm_opt = {
  /* enabled         = */ TRUE,
  /* debug_mode      = */ DBG_MODE_NONE,
  /* const_folding   = */ TRUE,
  /* reassoc         = */ TRUE,
  /* cse             = */ TRUE,
  /* control_flow    = */ TRUE,
  /* combo           = */ TRUE,
  /* gcse            = */ TRUE,
  /* gvn_pre         = */ FALSE,
  /* cond_eval       = */ FALSE,
  /* if_conversion   = */ FALSE,
  /* func_calls      = */ TRUE,
  /* do_inline       = */ FALSE,
  /* auto_inline     = */ TRUE,
  /* tail_rec        = */ TRUE,
  /* strength_red    = */ TRUE,
  /* scalar_replace  = */ TRUE,
  /* confirm         = */ TRUE,
  /* muls            = */ TRUE,
  /* divs            = */ TRUE,
  /* mods            = */ TRUE,
  /* fragile_ops     = */ TRUE,
  /* load_store      = */ TRUE,
  /* modes           = */ FALSE,
  /* precise_exc     = */ FALSE,	/* never needed for C */
  /* use_DivMod      = */ FALSE,
  /* remove_unused   = */ TRUE,
  /* cloning         = */ FALSE,
  /* auto_sync       = */ TRUE,
  /* alias_analysis  = */ TRUE,
  /* strict_alias    = */ FALSE,
  /* no_alias        = */ FALSE,
  /* sync            = */ TRUE,
  /* deconv          = */ FALSE,
  /* cc_opt          = */ TRUE,
  /* bool_opt        = */ FALSE,
  /* end_melt        = */ FALSE,
  /* freestanding;   = */ FALSE,
  /* fp_model        = */ fp_model_precise,
  /* lower_ll        = */ FALSE,
  /* vrfy            = */ FIRM_VERIFICATION_ON,
  /* check_all       = */ FALSE,
  /* lower           = */ TRUE,
  /* os_support      = */ DEFAULT_OS,
  /* honor_restrict  = */ TRUE,
  /* lower_bitfields = */ TRUE,
  /* pic             = */ FALSE,
  /* ycomp_dbg       = */ FALSE,
  /* ycomp_host      = */ FIRM_YCOMP_DEFAULT_HOST,
  /* ycomp_port      = */ FIRM_YCOMP_DEFAULT_PORT,
  /* clone_threshold = */ DEFAULT_CLONE_THRESHOLD,
  /* inline_maxsize  = */ 750,
  /* inline_threshold= */ 0,
  /* vrfy_edges      = */ FALSE,
  /* grs_simd_opt    = */ 0,
  /* grs_create_pattern = */ 0,
  /* spare_size      = */ 128,
};

/* dumping options */
struct a_firm_dump firm_dump = {
  /* debug_print  = */ FALSE,
  /* all_types    = */ FALSE,
  /* no_blocks    = */ FALSE,
  /* extbb        = */ FALSE,
  /* ir_graph     = */ FALSE,
  /* all_phases   = */ FALSE,
  /* edge_labels  = */ FALSE,
  /* statistic    = */ STAT_NONE,
  /* stat_pattern = */ 0,
  /* stat_dag     = */ 0,
  /* gen_firm_asm = */ FALSE,
  /* filter       = */ NULL
};

#ifdef FIRM_EXT_GRS
struct a_firm_ext_grs firm_ext_grs = {
  /* simd_opt       = */ FALSE,
  /* create_pattern = */ FALSE
};
#endif

struct a_firm_be_opt firm_be_opt = {
  /* selection = */ BE_FIRM_BE,
  /* node_stat = */ 0,
};

#define X(a)	a, sizeof(a)-1

/** Parameter description structure */
static const struct params {
  const char *option;      /**< name of the option */
  int        opt_len;      /**< length of the option string */
  a_byte     *flag;        /**< address of variable to set/reset */
  a_byte     set;          /**< iff true, variable will be set, else reset */
  const char *description; /**< description of this option */
} firm_options[] = {
  /* this must be first */
  { X("help"),                   NULL,                       0, "print FCC related help options" },

  /* firm optimization options */
  { X("pic"),                    &firm_opt.pic,              1, "firm: generate position independent code" },
  { X("g0"),                     &firm_opt.debug_mode,       DBG_MODE_BACKSTORE, "firm: Debug Mode: use back stores" },
  { X("g1"),                     &firm_opt.debug_mode,       DBG_MODE_FULL,      "firm: Debug Mode: no register variables" },
  { X("no-opt"),                 NULL,                       0, "firm: disable all FIRM optimizations" },
  { X("cse"),                    &firm_opt.cse,              1, "firm: enable common subexpression elimination" },
  { X("no-cse"),                 &firm_opt.cse,              0, "firm: disable common subexpression elimination" },
  { X("const-fold"),             &firm_opt.const_folding,    1, "firm: enable constant folding" },
  { X("no-const-fold"),          &firm_opt.const_folding,    0, "firm: disable constant folding" },
  { X("control_flow"),           &firm_opt.control_flow,     1, "firm: enable control flow optimization" },
  { X("no-control-flow"),        &firm_opt.control_flow,     0, "firm: disable control flow optimization" },
  { X("combo"),                  &firm_opt.combo,            1, "firm: enable combined CCE, UCE and GVN" },
  { X("no-combo"),               &firm_opt.combo,            0, "firm: disable combined CCE, UCE and GVN" },
  { X("gcse"),                   &firm_opt.gcse,             1, "firm: enable global common subexpression elimination" },
  { X("no-gcse"),                &firm_opt.gcse,             0, "firm: disable global common subexpression elimination" },
  { X("gvn-pre"),                &firm_opt.gvn_pre,          1, "firm: enable GVN partial redundancy elimination" },
  { X("no-gvn-pre"),             &firm_opt.gvn_pre,          0, "firm: disable GVN partial redundancy elimination" },
  { X("cond-eval"),              &firm_opt.cond_eval,        1, "firm: enable partial condition evaluation optimization" },
  { X("no-cond-eval"),           &firm_opt.cond_eval,        0, "firm: disable partial condition evaluation optimization" },
  { X("if-conv"),                &firm_opt.if_conversion,    1, "firm: enable if-conversion optimization" },
  { X("no-if-conv"),             &firm_opt.if_conversion,    0, "firm: disable if-conversion optimization" },
  { X("opt-func-call"),          &firm_opt.func_calls,       1, "firm: enable function call optimization" },
  { X("no-opt-func-call"),       &firm_opt.func_calls,       0, "firm: disable function call optimization" },
  { X("reassociation"),          &firm_opt.reassoc,          1, "firm: enable reassociation" },
  { X("no-reassociation"),       &firm_opt.reassoc,          0, "firm: disable reassociation" },
  { X("inline"),                 &firm_opt.do_inline,        1, "firm: enable FIRM inlining" },
  { X("no-inline"),              &firm_opt.do_inline,        0, "firm: disable FIRM inlining" },
  { X("inline-max-size=<size>"), NULL,                       0, "firm: set maximum size for function inlining" },
  { X("inline-threshold=<size>"),NULL,                       0, "firm: set benefice threshold for function inlining" },
  { X("tail-rec"),               &firm_opt.tail_rec,         1, "firm: enable tail-recursion optimization" },
  { X("no-tail-rec"),            &firm_opt.tail_rec,         0, "firm: disable tail-recursion optimization" },
  { X("strength-red"),           &firm_opt.strength_red,     1, "firm: enable strength reduction for loops" },
  { X("no-strength-red"),        &firm_opt.strength_red,     0, "firm: disable strength reduction for loops" },
  { X("scalar-replace"),         &firm_opt.scalar_replace,   1, "firm: enable scalar replacement" },
  { X("no-scalar-replace"),      &firm_opt.scalar_replace,   0, "firm: disable scalar replacement" },
  { X("confirm"),                &firm_opt.confirm,          1, "firm: enable Confirm optimization" },
  { X("no-confirm"),             &firm_opt.confirm,          0, "firm: disable Confirm optimization" },
  { X("opt-mul"),                &firm_opt.muls,             1, "firm: enable multiplication optimization" },
  { X("no-opt-mul"),             &firm_opt.muls,             0, "firm: disable multiplication optimization" },
  { X("opt-div"),                &firm_opt.divs,             1, "firm: enable division optimization" },
  { X("no-opt-div"),             &firm_opt.divs,             0, "firm: disable division optimization" },
  { X("opt-mod"),                &firm_opt.mods,             1, "firm: enable remainder optimization" },
  { X("no-opt-mod"),             &firm_opt.mods,             0, "firm: disable remainder optimization" },
  { X("opt-fragile-ops"),        &firm_opt.fragile_ops,      1, "firm: enable fragile ops optimization" },
  { X("no-opt-fragile-ops"),     &firm_opt.fragile_ops,      0, "firm: disable fragile ops optimization" },
  { X("opt-load-store"),         &firm_opt.load_store,       1, "firm: enable load store optimization" },
  { X("no-opt-load-store"),      &firm_opt.load_store,       0, "firm: disable load store optimization" },
  { X("opt-modes"),              &firm_opt.modes,            1, "firm: optimize integer modes" },
  { X("no-opt-modes"),           &firm_opt.modes,            0, "firm: disable integer modes optimization" },
  { X("sync"),                   &firm_opt.auto_sync,        1, "firm: automatically create Sync nodes" },
  { X("no-sync"),                &firm_opt.auto_sync,        0, "firm: do not create Sync nodes" },
  { X("opt-alias"),              &firm_opt.alias_analysis,   1, "firm: enable alias analysis" },
  { X("no-opt-alias"),           &firm_opt.alias_analysis,   0, "firm: disable alias analysis" },
  { X("alias"),                  &firm_opt.no_alias,         0, "firm: aliasing occurs" },
  { X("no-alias"),               &firm_opt.no_alias,         1, "firm: no aliasing occurs" },
  { X("strict-aliasing"),        &firm_opt.strict_alias,     1, "firm: strict alias rules" },
  { X("no-strict-aliasing"),     &firm_opt.strict_alias,     0, "firm: strict alias rules" },
  { X("opt-proc-clone"),         &firm_opt.cloning,          1, "firm: enable procedure cloning" },
  { X("no-opt-proc-clone"),      &firm_opt.cloning,          0, "firm: disable procedure cloning" },
  { X("clone-threshold=<value>"),NULL,                       0, "firm: set clone threshold to <value>" },
  { X("DivMod"),                 &firm_opt.use_DivMod,       1, "firm: use DivMod nodes" },
  { X("no-DivMod"),              &firm_opt.use_DivMod,       0, "firm: don't use DivMod nodes" },
  { X("precise-except"),         &firm_opt.precise_exc,      1, "firm: precise exception context" },
  { X("no-precise-except"),      &firm_opt.precise_exc,      0, "firm: no precise exception context" },
  { X("remove-unused"),          &firm_opt.remove_unused,    1, "firm: remove unused functions" },
  { X("no-remove-unused"),       &firm_opt.remove_unused,    0, "firm: dont't remove unused functions" },
  { X("fp-precise"),             &firm_opt.fp_model,         fp_model_precise, "firm: precise fp model" },
  { X("fp-fast"),                &firm_opt.fp_model,         fp_model_fast,    "firm: fast fp model" },
  { X("fp-strict"),              &firm_opt.fp_model,         fp_model_strict,  "firm: strict fp model" },
  { X("sync"),                   &firm_opt.sync,             1, "firm: use Syncs to remove unnecessary memory dependencies" },
  { X("no-sync"),                &firm_opt.sync,             0, "firm: do not use Syncs to remove unnecessary memory dependencies" },
  { X("deconv"),                 &firm_opt.deconv,           1, "firm: enable the conv node optimization" },
  { X("no-deconv"),              &firm_opt.deconv,           0, "firm: disable the conv node optimization" },
  { X("opt-cc"),                 &firm_opt.cc_opt,           1, "firm: enable calling conventions optimization" },
  { X("no-opt-cc"),              &firm_opt.cc_opt,           0, "firm: disable calling conventions optimization" },
  { X("bool"),                   &firm_opt.bool_opt,         1, "firm: enable bool simplification optimization" },
  { X("no-bool"),                &firm_opt.bool_opt,         0, "firm: disable bool simplification optimization" },
  { X("end-melt"),               &firm_opt.end_melt,         1, "firm: enable end block melting" },
  { X("no-end-melt"),            &firm_opt.end_melt,         0, "firm: disable end block melting" },
  { X("freestanding"),           &firm_opt.freestanding,     1, "firm: freestanding environment" },
  { X("hosted"),                 &firm_opt.freestanding,     0, "firm: hosted environment" },

  /* other firm regarding options */
  { X("restrict"),               &firm_opt.honor_restrict,   1, "firm: honor restrict keyword" },
  { X("no-restrict"),            &firm_opt.honor_restrict,   1, "firm: restrict keyword is meaningless" },
  { X("no-lower"),               &firm_opt.lower,            0, "firm: disable lowering" },
  { X("vrfy-off"),               &firm_opt.vrfy,             FIRM_VERIFICATION_OFF, "firm: disable node verification" },
  { X("vrfy-on"),                &firm_opt.vrfy,             FIRM_VERIFICATION_ON, "firm: enable node verification" },
  { X("vrfy-report"),            &firm_opt.vrfy,             FIRM_VERIFICATION_REPORT, "firm: node verification, report only" },
  { X("check-all"),              &firm_opt.check_all,        1, "firm: enable checking all Firm phases" },
  { X("no-check-all"),           &firm_opt.check_all,        0, "firm: disable checking all Firm phases" },
  { X("vrfy-edges-on"),          &firm_opt.vrfy_edges,       1, "firm: enable out edge verification" },
  { X("vrfy-edges-off"),         &firm_opt.vrfy_edges,       0, "firm: disable out edge verification" },

  /* dumping */
#if defined(_DEBUG) || defined(FIRM_DEBUG)
  { X("debug"),                  &firm_dump.debug_print,     1, "firm: enable debug output" },
#endif

  { X("dump-ir"),                &firm_dump.ir_graph,        1, "firm: dump IR graph" },
  { X("dump-all-types"),         &firm_dump.all_types,       1, "firm: dump graph of all types" },
  { X("dump-no-blocks"),         &firm_dump.no_blocks,       1, "firm: dump non-blocked graph" },
  { X("dump-extbb"),             &firm_dump.extbb,           1, "firm: dump extended basic blocks" },
  { X("dump-all-phases"),        &firm_dump.all_phases,      1, "firm: dump graphs for all optimization phases" },
  { X("dump-edge-labels"),       &firm_dump.edge_labels,     1, "firm: dump edge labels" },

  /* code generation */
  { X("no-codegen"),             &firm_be_opt.selection,     BE_NONE, "cg: disable code generator" },

#ifdef FIRM_EXT_GRS
  { X("grs-simd-opt"),           &firm_ext_grs.simd_opt,		1, "firm: do simd optimization" },
  { X("grs-create-pattern"),     &firm_ext_grs.create_pattern,	1, "firm: create patterns for simd optimization" },
  { X("no-grs-simd-opt"),        &firm_ext_grs.simd_opt,		0, "firm: do simd optimization" },
  { X("no-grs-create-pattern"),  &firm_ext_grs.create_pattern,	0, "firm: create patterns for simd optimization" },
#endif

#ifdef FIRM_BACKEND
  { X("be-firm"),                &firm_be_opt.selection,     BE_FIRM_BE, "backend: firm backend facility" },
#endif /* FIRM_BACKEND */
#ifdef FIRM2C_BACKEND
  { X("be-firm2c"),              &firm_be_opt.selection,     BE_FIRM2C, "backend: firm2C" },
#endif /* FIRM2C_BACKEND */

  /* misc */
  { X("stat-before-opt"),        &firm_dump.statistic,       STAT_BEFORE_OPT,  "misc: Firm statistic output before optimizations" },
  { X("stat-after-opt"),         &firm_dump.statistic,       STAT_AFTER_OPT,   "misc: Firm statistic output after optimizations" },
  { X("stat-after-lower"),       &firm_dump.statistic,       STAT_AFTER_LOWER, "misc: Firm statistic output after lowering" },
  { X("stat-final-ir"),          &firm_dump.statistic,       STAT_FINAL_IR,    "misc: Firm statistic after final optimization" },
  { X("stat-final"),             &firm_dump.statistic,       STAT_FINAL,       "misc: Firm statistic after code generation" },
  { X("stat-pattern"),           &firm_dump.stat_pattern,    1, "misc: Firm statistic calculates most used pattern" },
  { X("stat-dag"),               &firm_dump.stat_dag,        1, "misc: Firm calculates DAG statistics" },
  { X("firm-asm"),               &firm_dump.gen_firm_asm,    1, "misc: output Firm assembler" },
  { X("win32"),                  &firm_opt.os_support,       OS_SUPPORT_MINGW, "misc: generate MinGW Win32 code" },
  { X("mac"),                    &firm_opt.os_support,       OS_SUPPORT_MACHO, "misc: generate MacOS code" },
  { X("linux"),                  &firm_opt.os_support,       OS_SUPPORT_LINUX, "misc: generate Linux-ELF code" },
  { X("ycomp"),                  &firm_opt.ycomp_dbg,        1, "misc: enable yComp debugger extension" },
  { X("ycomp-host=<hostname>"),  NULL,                       0, "misc: yComp host" },
  { X("ycomp-port=<port>"),      NULL,                       0, "misc: yComp port" },

  /* string options */
  { X("dump-filter=<string>"),   NULL,                       0, "misc: set dumper filter" },
};

#undef X

/** A strdup replacement */
static char *StrDup(const char *s) {
  int   l = strlen(s);
  char *r = malloc(l+1);

  if (r != NULL)
    memcpy(r, s, l+1);
  return r;
}

/**
 * Set a dumper filter.
 */
static void set_dump_filter(const char *filter)
{
  firm_dump.filter = StrDup(filter);
}  /* set_dump_filter */

/**
 * Set ycomp host
 */
static void set_ycomp_host(const char *host)
{
  firm_opt.ycomp_host = StrDup(host);
}  /* set_ycomp_host */

/** Disable all optimizations. */
static void disable_opts(void) {
  /* firm_opt.const_folding */
  firm_opt.reassoc         = FALSE;
  firm_opt.cse             = FALSE;
  /* firm_opt.control_flow */
  firm_opt.gcse            = FALSE;
  firm_opt.gvn_pre         = FALSE;
  firm_opt.cond_eval       = FALSE;
  firm_opt.if_conversion   = FALSE;
  firm_opt.func_calls      = FALSE;
  firm_opt.do_inline       = FALSE;
  firm_opt.auto_inline     = FALSE;
  firm_opt.tail_rec        = FALSE;
  firm_opt.strength_red    = FALSE;
  firm_opt.scalar_replace  = FALSE;
  firm_opt.confirm         = FALSE;
  firm_opt.muls            = FALSE;
  firm_opt.divs            = FALSE;
  firm_opt.mods            = FALSE;
  firm_opt.fragile_ops     = FALSE;
  firm_opt.load_store      = FALSE;
  firm_opt.remove_unused   = FALSE;
  /* firm_opt.jmp_tbls */
  firm_opt.cloning         = FALSE;
  /* firm_opt.auto_sync */
  firm_opt.alias_analysis  = FALSE;
  firm_opt.strict_alias    = FALSE;
  firm_opt.no_alias        = FALSE;
  firm_opt.sync            = FALSE;
  firm_opt.deconv          = FALSE;
  firm_opt.cc_opt          = FALSE;
  firm_opt.bool_opt        = FALSE;
  firm_opt.end_melt        = FALSE;
  firm_opt.freestanding    = TRUE;
}  /* disable_opts */

/**
 * Handles a firm option.
 */
int firm_option(const char *opt)
{
  int i, len    = strlen(opt);
  const char *p = opt;
  int res;

  if (strncmp("dump-filter=", opt, 12) == 0) {
    opt = &opt[12];
    set_dump_filter(opt);
    return 1;
  }
  else if (strncmp("clone-threshold=", opt, 16) == 0) {
    sscanf(&opt[16], "%d", &firm_opt.clone_threshold);
    firm_opt.cloning = TRUE;
    return 1;
  }
  else if (strncmp("inline-max-size=", opt, 16) == 0) {
    sscanf(&opt[16], "%u", &firm_opt.inline_maxsize);
    return 1;
  }
  else if (strncmp("inline-threshold=", opt, 17) == 0) {
    sscanf(&opt[17], "%u", &firm_opt.inline_threshold);
    return 1;
  }
  else if (strncmp("ycomp-host=", opt, 11) == 0) {
    opt = &opt[11];
    set_ycomp_host(opt);
    return 1;
  }
  else if (strncmp("ycomp-port=", opt, 11) == 0) {
    sscanf(&opt[11], "%d", &firm_opt.ycomp_port);
    return 1;
  }
  else if (strcmp("no-opt", opt) == 0) {
    disable_opts();
    return 1;
  }

  for (i = sizeof(firm_options) / sizeof(firm_options[0]) - 1; i >= 0; --i) {
    if (len == firm_options[i].opt_len && strncmp(p, firm_options[i].option, len) == 0) {
      if (!firm_options[i].flag) {
        /* help option */
        for (i = 0; i < (int) (sizeof(firm_options)/sizeof(firm_options[0])); ++i) {
          printf("-f %-20s %s\n", firm_options[i].option,
                 firm_options[i].description);
        }
        return -1;
      }
      /* statistic options do accumulate */
      if (firm_options[i].flag == &firm_dump.statistic)
        *firm_options[i].flag = (a_byte) (*firm_options[i].flag | firm_options[i].set);
      else
        *firm_options[i].flag = firm_options[i].set;

      if (firm_options[i].flag == &firm_opt.debug_mode && firm_opt.debug_mode > DBG_MODE_NONE) {
        if (firm_opt.debug_mode == DBG_MODE_FULL)
          disable_opts();
        res = 1;
        res &= firm_be_option("omitfp=0");
        res &= firm_be_option("stabs");
        return res;
      }
      break;
    }
  }

  if (i < 0)
    return 0;
  return 1;
}  /* firm_option */

/**
 * Handles a firm backend option.
 *
 * The options are here only checked for validity and later transmitted
 * to the firm backend (in the hope they do not fail ...)
 */
int firm_be_option(const char *opt) {
#ifdef FIRM_BACKEND
  return be_parse_arg(opt);
#else
  return 0;
#endif /* FIRM_BACKEND */
}  /* firm_be_option */

/**
 * prints the firm version number
 */
void print_firm_version(FILE *f) {
  firm_version_t version;

  firm_get_version(&version);

  fprintf(f, "Firm C-Compiler using libFirm (%u.%u", version.major, version.minor);
  if (version.revision[0] != 0) {
  	fputc(' ', f);
    fputs(version.revision, f);
  }
   if(version.build[0] != 0) {
  	fputc(' ', f);
    fputs(version.build, f);
  }
  fprintf(f, "}\n"
  		     "(C) 2005-2008 Michael Beck\n"
             "(C) 1995-2008 University of Karlsruhe\n"
             "Using ");
}  /* print_firm_version */
