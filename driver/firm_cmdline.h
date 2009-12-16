/**
 * @file firm_cmdline.h -- Additional Firm generating backend parameters
 *
 * Generates Firm fro the IL.  It works with both C++ and C programs.
 *
 * Compile with STANDALONE_CP_FIRM_BE defined and BACK_END_IS_CP_FIRM_BE
 * defined as 1 to get a main program back end.  Otherwise, a version to be
 * called in the same program as the front end is produced (if needed).
 */
#ifndef FIRM_CMDLINE_H
#define FIRM_CMDLINE_H

#include "fe_common.h"

enum an_os_support {
  OS_SUPPORT_LINUX,         /**< create code for Linux OS */
  OS_SUPPORT_MINGW,         /**< create code for MinGW WIN32 */
  OS_SUPPORT_MACHO          /**< create code for MacOS Mach-O */
} an_os_support;

enum a_debug_mode {
  DBG_MODE_NONE      = 0,   /**< no special debug support */
  DBG_MODE_BACKSTORE = 1,   /**< backstores are created */
  DBG_MODE_FULL      = 2,   /**< no register valiables */
};

/* optimization settings */
struct a_firm_opt {
  a_byte   enabled;         /**< enable all optimizations */
  a_byte   debug_mode;      /**< debug mode: store all local variables */
  a_byte   const_folding;   /**< enable constant folding */
  a_byte   cse;             /**< enable common-subexpression elimination */
  a_byte   control_flow;    /**< enable control flow optimizations */
  a_byte   gcse;            /**< enable global common-subexpression elimination */
  a_byte   confirm;         /**< enable Confirm optimization */
  a_byte   muls;            /**< enable architecture dependent mul optimization */
  a_byte   divs;            /**< enable architecture dependent div optimization */
  a_byte   mods;            /**< enable architecture dependent mod optimization */
  a_byte   alias_analysis;  /**< enable Alias Analysis */
  a_byte   strict_alias;    /**< enable strict Alias Analysis (using type based AA) */
  a_byte   no_alias;        /**< no aliasing possible. */
  a_byte   cc_opt;          /**< optimize calling conventions */
  a_byte   freestanding;    /**< if set, freestanding mode is enabled */
  a_byte   fp_model;        /**< fp model */
  a_byte   lower_ll;        /**< lower double word access */
  a_byte   vrfy;            /**< Firm verifier setting */
  a_byte   check_all;       /**< enable checking all Firm phases */
  a_byte   lower;           /**< enable Firm lowering */
  a_byte   os_support;      /**< current os support */
  a_byte   honor_restrict;  /**< enable restrict keyword */
  a_byte   lower_bitfields; /**< lower bitfield access */
  a_byte   pic;             /**< generate position independent code */
  int      clone_threshold; /**< The threshold value for procedure cloning. */
  unsigned inline_maxsize;  /**< Maximum function size for inlining. */
  unsigned inline_threshold;/**< Inlining benefice threshold. */
  a_byte   vrfy_edges;      /**< verify edges */
  a_byte   grs_simd_opt;
  a_byte   grs_create_pattern;
  unsigned spare_size;      /**< allowed spare size for table switches in machine words. */
  a_byte   enable_statev;   /**< enable statev output */
  char     *statev_filter;  /**< statev filter */
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

/* backend selection */
typedef enum a_backend_selection_tag {
  BE_NONE      = 0,       /**< no backend */
  BE_FIRM_BE   = 1,       /**< Use Firm internal backend facility. */
  BE_FIRM2C    = 2        /**< Use generic Firm2C backend */
} a_backend_selection;

/* dumping options */
struct a_firm_dump {
  a_byte debug_print;   /**< enable debug print */
  a_byte all_types;     /**< dump the All_types graph */
  a_byte no_blocks;     /**< dump non-blocked graph */
  a_byte extbb;         /**< dumps extended basic blocks */
  a_byte ir_graph;      /**< dump all graphs */
  a_byte all_phases;    /**< dump the IR graph after all phases */
  a_byte edge_labels;   /**< use edge labels when dumping IR graphs */
  a_byte statistic;     /**< Firm statistic setting */
  a_byte stat_pattern;  /**< enable Firm statistic pattern */
  a_byte stat_dag;      /**< enable Firm DAG statistic */
  a_byte gen_firm_asm;  /**< generate Firm assembler and exit */
  char   *filter;       /**< the dump filter */
};

struct a_firm_be_opt {
  a_byte selection;
  a_byte node_stat;
};

#ifdef FIRM_EXT_GRS
struct a_firm_ext_grs {
  a_byte simd_opt;          /**< enable graph based SIMD optimization */
  a_byte create_pattern;    /**< enable pattern creation for SIMD opts */
};
#endif


extern struct a_firm_be_opt firm_be_opt;
extern struct a_firm_opt firm_opt;
extern struct a_firm_dump firm_dump;
extern struct a_firm_ext_grs firm_ext_grs;

void print_option_help(const char *name, const char *description);

/**
 * prints the firm version number
 */
void print_firm_version(FILE *f);

/**
 * called by the generic command line parser
 * to handle the --firm= or -f options
 */
int firm_option(const char *opt);

/**
 * called by the generic command line parser
 * to handle the --backend= or -b options
 */
int firm_be_option(const char *opt);

#endif /* FIRM_CMDLINE_H */
