/**
 * @file firm_cmdline.h -- Additional Firm generating backend parameters
 *
 * Generates Firm fro the IL.  It works with both C++ and C programs.
 */
#ifndef FIRM_CMDLINE_H
#define FIRM_CMDLINE_H

#include <stdbool.h>

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
  bool     fp_model;        /**< fp model */
  bool     verify;          /**< Firm verifier setting */
  bool     check_all;       /**< enable checking all Firm phases */
  int      clone_threshold; /**< The threshold value for procedure cloning. */
  unsigned inline_maxsize;  /**< Maximum function size for inlining. */
  unsigned inline_threshold;/**< Inlining benefice threshold. */
  bool     verify_edges;    /**< verify edges */
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
  bool no_blocks;     /**< dump non-blocked graph */
  bool extbb;         /**< dumps extended basic blocks */
  bool ir_graph;      /**< dump all graphs */
  bool all_phases;    /**< dump the IR graph after all phases */
  bool statistic;     /**< Firm statistic setting */
  bool stat_pattern;  /**< enable Firm statistic pattern */
  bool stat_dag;      /**< enable Firm DAG statistic */
  char *filter;       /**< the dump filter */
};

struct a_firm_be_opt {
  bool selection;
  bool node_stat;
};

extern struct a_firm_be_opt firm_be_opt;
extern struct a_firm_opt firm_opt;
extern struct a_firm_dump firm_dump;
extern struct a_firm_ext_grs firm_ext_grs;

void print_option_help(const char *name, const char *description);

/**
 * called by the generic command line parser
 * to handle the --firm= or -f options
 */
int firm_option(const char *opt);

#endif
