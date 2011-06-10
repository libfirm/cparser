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
#include "../adt/strutil.h"
#include "../adt/util.h"
#include "firm_cmdline.h"
#include "firm_opt.h"
#include <libfirm/firm.h>
#include <libfirm/be.h>

/* optimization settings */
struct a_firm_opt firm_opt = {
  /* const_folding   = */ true,
  /* cse             = */ true,
  /* confirm         = */ true,
  /* muls            = */ true,
  /* divs            = */ true,
  /* mods            = */ true,
  /* alias_analysis  = */ true,
  /* strict_alias    = */ false,
  /* no_alias        = */ false,
  /* verify          = */ FIRM_VERIFICATION_ON,
  /* check_all       = */ true,
  /* clone_threshold = */ DEFAULT_CLONE_THRESHOLD,
  /* inline_maxsize  = */ 750,
  /* inline_threshold= */ 0,
  /* verify_edges    = */ false,
};

/* dumping options */
struct a_firm_dump firm_dump = {
  /* debug_print  = */ false,
  /* all_types    = */ false,
  /* no_blocks    = */ false,
  /* extbb        = */ false,
  /* ir_graph     = */ false,
  /* all_phases   = */ false,
  /* statistic    = */ STAT_NONE,
  /* stat_pattern = */ 0,
  /* stat_dag     = */ 0,
  /* filter       = */ NULL
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
  /* this must be first */
  { X("help"),                   NULL,                       0, "print FCC related help options" },

  /* firm optimization options */
  { X("no-opt"),                 NULL,                       0, "firm: disable all FIRM optimizations" },
  { X("cse"),                    &firm_opt.cse,              1, "firm: enable common subexpression elimination" },
  { X("no-cse"),                 &firm_opt.cse,              0, "firm: disable common subexpression elimination" },
  { X("const-fold"),             &firm_opt.const_folding,    1, "firm: enable constant folding" },
  { X("no-const-fold"),          &firm_opt.const_folding,    0, "firm: disable constant folding" },
  { X("inline-max-size=<size>"), NULL,                       0, "firm: set maximum size for function inlining" },
  { X("inline-threshold=<size>"),NULL,                       0, "firm: set benefice threshold for function inlining" },
  { X("confirm"),                &firm_opt.confirm,          1, "firm: enable Confirm optimization" },
  { X("no-confirm"),             &firm_opt.confirm,          0, "firm: disable Confirm optimization" },
  { X("opt-mul"),                &firm_opt.muls,             0, "firm: enable multiplication optimization" },
  { X("no-opt-mul"),             &firm_opt.muls,             0, "firm: disable multiplication optimization" },
  { X("opt-div"),                &firm_opt.divs,             0, "firm: enable division optimization" },
  { X("no-opt-div"),             &firm_opt.divs,             0, "firm: disable division optimization" },
  { X("opt-mod"),                &firm_opt.mods,             0, "firm: enable remainder optimization" },
  { X("no-opt-mod"),             &firm_opt.mods,             0, "firm: disable remainder optimization" },
  { X("opt-alias"),              &firm_opt.alias_analysis,   1, "firm: enable alias analysis" },
  { X("no-opt-alias"),           &firm_opt.alias_analysis,   0, "firm: disable alias analysis" },
  { X("alias"),                  &firm_opt.no_alias,         0, "firm: aliasing occurs" },
  { X("no-alias"),               &firm_opt.no_alias,         1, "firm: no aliasing occurs" },
  { X("strict-aliasing"),        &firm_opt.strict_alias,     1, "firm: strict alias rules" },
  { X("no-strict-aliasing"),     &firm_opt.strict_alias,     0, "firm: strict alias rules" },
  { X("clone-threshold=<value>"),NULL,                       0, "firm: set clone threshold to <value>" },

  /* other firm regarding options */
  { X("verify-off"),             &firm_opt.verify,           FIRM_VERIFICATION_OFF, "firm: disable node verification" },
  { X("verify-on"),              &firm_opt.verify,           FIRM_VERIFICATION_ON, "firm: enable node verification" },
  { X("verify-report"),          &firm_opt.verify,           FIRM_VERIFICATION_REPORT, "firm: node verification, report only" },
  { X("check-all"),              &firm_opt.check_all,        1, "firm: enable checking all Firm phases" },
  { X("no-check-all"),           &firm_opt.check_all,        0, "firm: disable checking all Firm phases" },
  { X("verify-edges-on"),        &firm_opt.verify_edges,     1, "firm: enable out edge verification" },
  { X("verify-edges-off"),       &firm_opt.verify_edges,     0, "firm: disable out edge verification" },

  /* dumping */
  { X("dump-ir"),                &firm_dump.ir_graph,        1, "firm: dump IR graph" },
  { X("dump-all-types"),         &firm_dump.all_types,       1, "firm: dump graph of all types" },
  { X("dump-no-blocks"),         &firm_dump.no_blocks,       1, "firm: dump non-blocked graph" },
  { X("dump-extbb"),             &firm_dump.extbb,           1, "firm: dump extended basic blocks" },
  { X("dump-all-phases"),        &firm_dump.all_phases,      1, "firm: dump graphs for all optimization phases" },

  /* misc */
  { X("stat-before-opt"),        &firm_dump.statistic,       STAT_BEFORE_OPT,  "misc: Firm statistic output before optimizations" },
  { X("stat-after-opt"),         &firm_dump.statistic,       STAT_AFTER_OPT,   "misc: Firm statistic output after optimizations" },
  { X("stat-after-lower"),       &firm_dump.statistic,       STAT_AFTER_LOWER, "misc: Firm statistic output after lowering" },
  { X("stat-final-ir"),          &firm_dump.statistic,       STAT_FINAL_IR,    "misc: Firm statistic after final optimization" },
  { X("stat-final"),             &firm_dump.statistic,       STAT_FINAL,       "misc: Firm statistic after code generation" },
  { X("stat-pattern"),           &firm_dump.stat_pattern,    1, "misc: Firm statistic calculates most used pattern" },
  { X("stat-dag"),               &firm_dump.stat_dag,        1, "misc: Firm calculates DAG statistics" },

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

/** Disable all optimizations. */
static void disable_opts(void) {
  firm_opt.cse             = false;
  firm_opt.confirm         = false;
  firm_opt.muls            = false;
  firm_opt.divs            = false;
  firm_opt.mods            = false;
  firm_opt.alias_analysis  = false;
  firm_opt.strict_alias    = false;
  firm_opt.no_alias        = false;
  disable_all_opts();
}  /* disable_opts */

void print_option_help(const char *name, const char *description)
{
	printf("-f %-20s %s\n", name, description);
}

/**
 * Handles a firm option.
 */
int firm_option(char const *const opt)
{
	char const* val;
  if ((val = strstart(opt, "dump-filter="))) {
    set_dump_filter(val);
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
    disable_opts();
    return 1;
  }

  size_t const len = strlen(opt);
  for (size_t i = lengthof(firm_options); i != 0;) {
    struct params const* const o = &firm_options[--i];
    if (len == o->opt_len && strncmp(opt, o->option, len) == 0) {
      if (!o->flag) {
        /* help option */
        print_option_help(firm_options[0].option, firm_options[0].description);
        firm_opt_option_help();
        for (size_t k = 1; k != lengthof(firm_options); ++k) {
          print_option_help(firm_options[k].option, firm_options[k].description);
        }
        return -1;
      }

      /* statistic options do accumulate */
      if (o->flag == &firm_dump.statistic)
        *o->flag = (bool) (*o->flag | o->set);
      else
        *o->flag = o->set;

      return 1;
    }
  }

  /* maybe this enables/disables optimisations */
  if (firm_opt_option(opt))
    return 1;

  return 0;
}
