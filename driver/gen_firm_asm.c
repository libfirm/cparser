/*
 * Generate Firm assembler from Firm graphs.
 *
 * (C) 2005-2006  Michael Beck   beck@ipd.info.uni-karlsruhe.de
 */
#include <stdio.h>
#include <string.h>
#include <libfirm/firm.h>
#include "firm_opt.h"

/**
 * The variable context. Contains information
 * about needed variables.
 */
typedef struct var_ctx {
  FILE *f;              /**< File handle for output. */
  int max_arity;        /**< Maximum arity of in array. */
  int need_tv;          /**< If set, need a tv variable. */
  int need_unknown_X;   /**< If set, need an unknown_X variable. */
  int need_unknown;     /**< If set, need an unknown variable. */
} var_ctx;

/**
 * A context for backedge fixes.
 */
typedef struct fix_ctx {
  FILE *f;              /**< File handle for output. */
  ir_node *start_block; /**< The start block of the current graph. */
  int has_backedges;    /**< If set the current graph contains backedges. */
} fix_ctx;

/**
 * Print the name of an IR-node as <prefix><op>_<idx><suffix>.
 *
 * @param f       the output file handle
 * @param prefix  a prefix to print before the node name
 * @param n       the IR-node
 * @param suffix  a suffix to print after the node name
 */
static void name(FILE *f, const char *prefix, ir_node *n, const char *suffix)
{
  const char *op_name = get_op_name(get_irn_op(n));
  unsigned    index   = get_irn_idx(n);
  if(prefix == NULL)
    prefix = "";
  if(suffix == NULL)
    suffix = "";

  fprintf(f, "%s%s_%u%s", prefix, op_name, index, suffix);
}

/**
 * Generate the header of the function.
 *
 * @param f    the output file handle
 * @param irg  the graph
 */
static void generate_header(FILE *f, ir_graph *irg)
{
  int i, n;
  ir_entity *ent = get_irg_entity(irg);
  ir_type *tp = get_entity_type(ent);
  const char *s = get_entity_name(ent);
  fprintf(f,
    "/**\n"
    " * Generates the Firm code for\n"
    " * ");

  if (get_method_n_ress(tp) == 0)
    fprintf(f, "void");
  else
    fprintf(f, get_type_name(get_method_res_type(tp, 0)));
  fprintf(f, " %s(", s);
  n = get_method_n_params(tp);
  if (n == 0)
    fprintf(f, get_method_variadicity(tp) == variadicity_variadic ? "" : "void");
  else {
    for (i = 0; i < n; ++i) {
      if (i > 0)
        fprintf(f, ", ");
      fprintf(f, get_type_name(get_method_param_type(tp, i)));
    }
    if (get_method_variadicity(tp) == variadicity_variadic)
      fprintf(f, ", ...");
  }
  fprintf(f, ")\n");

  fprintf(f,
    " *\n"
    " * @param func  the entity for the generated graph\n"
    " */\n"
    "void gen_firm_%s(ir_entity *func)\n"
    "{\n", s);
  fprintf(f,
    "  ir_graph *rem = current_ir_graph;\n");
}

/**
 * Generate the function prolog.
 *
 * @param env   a variable context
 * @param irg   the graph
 */
static void generate_prolog(var_ctx *env, ir_graph *irg)
{
  FILE *f = env->f;
  (void) irg;
  fprintf(f,
    "  irg = new_ir_graph(func, 0);\n"
    "  /* kill the current block */\n"
    "  tmp = get_cur_block();\n"
    "  mature_immBlock(tmp);\n"
    "  set_Block_dead(tmp);\n");
  if (env->need_unknown_X)
    fprintf(f, "  unknownX = new_r_Unknown(irg, mode_X);\n");
}

/**
 * Walker: Fix Block and Phi nodes containing backedges.
 */
static void fix_block_and_phi(ir_node *n, void *ctx)
{
  fix_ctx *env = ctx;
  int i, arity;
  FILE *f;

  if (is_Block(n) || is_Phi(n)) {
    arity = get_irn_arity(n);

    /* ignore the start block */
    if (n == env->start_block)
      return;

    f = env->f;
    for (i = 0; i < arity; ++i) {
      if (is_backedge(n, i)) {
        name(f, "  set_irn_n(", n, NULL);
        fprintf(f, ", %d, ", i);
        name(f, NULL, get_irn_n(n, i), ");\n");
      }
    }
  }
}

/**
 * Generate the function epilog.
 *
 * @param env   the backedge fix context
 * @param irg   the graph
 */
static void generate_epilog(fix_ctx *env, ir_graph *irg)
{
  FILE *f = env->f;
  ir_node *end;
  int i, n;

  /* Fix backedges if there was any */
  if (env->has_backedges) {
    fprintf(f, "  /* fix Blocks and Phis */\n");
    env->start_block = get_irg_start_block(irg);
    irg_walk_graph(irg, NULL, fix_block_and_phi, env);
  }

  /* create the keep-alives */
  end = get_irg_end(irg);
  n = get_End_n_keepalives(end);
  for (i = 0; i < n; ++i) {
    name(f, "  add_End_keepalive(", end, NULL);
    name(f, ", ", get_End_keepalive(end, i), ");\n");
  }
  name(f, "  mature_immBlock(", get_irg_end_block(irg), ");\n");
  fprintf(f, "  current_ir_graph = rem;\n");
  fprintf(f, "}  /* gen_firm_%s */\n\n", get_entity_name(get_irg_entity(irg)));
}

/**
 * Walker: Create a variable declaration for a given node.
 * The variable name is <op>_<node idx>, see name().
 *
 * @param n    the current IR-node
 * @param ctx  a variable context
 */
static void dump_var_def(ir_node *n, void *ctx)
{
  var_ctx *env = ctx;
  int arity = get_irn_arity(n);

  name(env->f, "  ir_node *", n, " = NULL;\n");
  if (arity > env->max_arity)
     env->max_arity = arity;
  if (get_irn_op(n) == op_Const)
    env->need_tv = 1;
  if (has_backedges(n)) {
    if (is_Block(n))
      env->need_unknown_X = 1;
    else
      env->need_unknown = 1;
  }
}

/**
 * Generate all needed variable declarations for a graph.
 *
 * @param env  a variable context
 * @param irg  the graph
 */
static void generate_var_decls(var_ctx *env, ir_graph *irg)
{
  FILE *f = env->f;

  irg_walk_graph(irg, NULL, dump_var_def, env);
  fprintf(f, "  ir_node *in[%d];\n", env->max_arity);
  if (env->need_tv)
    fprintf(f, "  tarval *tv = NULL;\n");
  fprintf(f, "  const char *s = NULL;\n");
  fprintf(f, "  ir_entity *ent = NULL;\n");
  fprintf(f, "  ir_mode *mode = NULL;\n");
  fprintf(f, "  ir_graph *irg = NULL;\n");
  fprintf(f, "  ir_node *tmp = NULL;\n");
  fprintf(f, "  symconst_symbol sym;\n");
  if (env->need_unknown_X)
    fprintf(f, "  ir_node *unknownX = NULL;\n");
  if (env->need_unknown)
    fprintf(f, "  ir_node *unknown = NULL;\n");
  fprintf(f, "\n");
}

/**
 * Generate code for a predefined node.
 *
 * @param f   file handle for output
 * @param n   the node
 *
 * @return non-zero if n was a predefined node, 0 else
 */
static int generate_predef_node(FILE *f, ir_node *n)
{
  ir_graph *irg = current_ir_graph;

#define X(code)                         \
  else if (n == code)                   \
    name(f, "  ", n, " = " #code ";\n");

  if (0);
  X(get_irg_start(irg))
  X(get_irg_start_block(irg))
  X(get_irg_end(irg))
  X(get_irg_end_block(irg))
  X(get_irg_frame(irg))
  X(get_irg_globals(irg))
  X(get_irg_tls(irg))
  X(get_irg_value_param_base(irg))
  X(get_irg_initial_mem(irg))
  X(get_irg_no_mem(irg))
  else
    return 0;
  return 1;

#undef X
}

/**
 * Generate code for a Binop node.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Binop(FILE *f, ir_node *n)
{
  ir_op *op = get_irn_op(n);
  ir_node *right = get_binop_right(n);

  name(f, "  ", n, " = ");
  fprintf(f, "new_r_%s(irg, ", get_op_name(op));
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, right, ", ");
  name(f, NULL, get_binop_left(n), ", get_irn_mode(");
  name(f, NULL, right, "));\n");
}

/**
 * Generate code for a Divop node.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Divop(FILE *f, ir_node *n)
{
  ir_op *op = get_irn_op(n);
  ir_node *right = get_binop_right(n);
  ir_mode *mode = get_divop_resmod(n);

  name(f, "  ", n, " = ");
  fprintf(f, "new_r_%s(irg, ", get_op_name(op));
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_fragile_op_mem(n), ", ");
  name(f, NULL, get_binop_left(n), ", ");
  name(f, NULL, right, ", ");

  /* avoid to print the mode name if possible */
  if (mode == get_irn_mode(right)) {
    name(f, "get_irn_mode(", right, ");\n");
  } else {
    fprintf(f, "mode_%s);\n", get_mode_name(mode));
  }
}

/**
 * Generate code for an Unop node.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Unop(FILE *f, ir_node *n)
{
  ir_op *op = get_irn_op(n);
  ir_node *irn = get_unop_op(n);

  name(f, "  ", n, " = ");
  fprintf(f, "new_r_%s(irg, ", get_op_name(op));
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, irn, ", get_irn_mode(");
  name(f, NULL, irn, "));\n");
}

/**
 * Generate code for a Load.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Load(FILE *f, ir_node *n)
{
  ir_node       *ptr = get_Load_ptr(n);
  ir_op         *op = get_irn_op(ptr);
  ir_volatility vol;

  /* try to detect the mode */
  if (op == op_Sel) {
    name(f, "  ent = get_Sel_entity(", ptr, ");\n");
    fprintf(f, "  mode = get_type_mode(get_entity_type(ent));\n");
  }
  else if (op == op_SymConst && get_SymConst_kind(ptr) == symconst_addr_ent) {
    name(f, "  ent = get_SymConst_entity(", ptr, ");\n");
    fprintf(f, "  mode = get_type_mode(get_entity_type(ent));\n");
  }
  else
    fprintf(f, "  mode = mode_%s;\n", get_mode_name(get_Load_mode(n)));

  name(f, "  ", n, " = ");
  fprintf(f, "new_r_Load(irg, ");
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_Load_mem(n), ", ");
  name(f, NULL, ptr, ", mode);\n");
  vol = get_Load_volatility(n);
  if (vol != volatility_non_volatile) {
    name(f, "  set_Load_volatility(", n, ", ");
    fprintf(f, "%s);\n", get_volatility_name(vol));
  }
}

/**
 * Generate code for a Store.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Store(FILE *f, ir_node *n)
{
  ir_volatility vol;

  name(f, "  ", n, " = ");
  fprintf(f, "new_r_Store(irg, ");
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_Store_mem(n), ", ");
  name(f, NULL, get_Store_ptr(n), ", ");
  name(f, NULL, get_Store_value(n), ")\n");
  vol = get_Store_volatility(n);
  if (vol != volatility_non_volatile) {
    name(f, "  set_Store_volatility(", n, ", ");
    fprintf(f, "%s);\n", get_volatility_name(vol));
  }
}

/**
 * Generate code for a Return.
 */
static void generate_code_Return(FILE *f, ir_node *n)
{
  int i, n_res = get_Return_n_ress(n);

  if (n_res > 1)
    for (i = 0; i < n_res; ++i) {
      fprintf(f, "  in[%d] = ", i);
      name(f, NULL, get_Return_res(n, i), ";\n");
    }
  name(f, "  ", n, " = ");
  fprintf(f, "new_r_Return(irg, ");
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_Return_mem(n), ", ");
  if (n_res > 1)
    fprintf(f, "%d, in);\n", n_res);
  else if (n_res == 1)
    name(f, "1, &", get_Return_res(n, 0), ");\n");
  else
    fprintf(f, "0, NULL);\n");
}

/**
 * Generate code for a Raise.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Raise(FILE *f, ir_node *n)
{
  name(f, "  ", n, " = new_r_Return(irg, ");
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_Raise_mem(n), ", ");
  name(f, NULL, get_Raise_exo_ptr(n), ");\n");
}

/**
 * Generate code for a Bound.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Bound(FILE *f, ir_node *n)
{
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_Bound_mem(n), ", ");
  name(f, NULL, get_Bound_index(n), ", ");
  name(f, NULL, get_Bound_lower(n), ", ");
  name(f, NULL, get_Bound_upper(n), ");\n");
}

/**
 * Generate code for an Unknown.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Unknown(FILE *f, ir_node *n)
{
  name(f, "  ", n, " = new_r_Unknown(irg, ");
  fprintf(f, "mode_%s);\n", get_mode_name(get_irn_mode(n)));
}

/**
 * Generate code for a Bad.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Bad(FILE *f, ir_node *n)
{
  name(f, "  ", n, " = new_r_Bad(irg);\n");
}

/**
 * Generate code for a Const.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Const(FILE *f, ir_node *n)
{
  ir_type *tp = get_Const_type(n);
  tarval *tv = get_Const_tarval(n);
  ir_mode *mode = get_irn_mode(n);
  char buf[256];

  if (tp == get_unknown_type()) {
    name(f, "  ", n, " = new_r_Const(irg, ");
  }
  else {
    name(f, "  ", n, " = new_r_Const_type(irg, ");
  }
  name(f, NULL, get_nodes_block(n), ", ");
  fprintf(f, "mode_%s, ", get_mode_name(mode));

  if (! mode_is_reference(mode)) {
    if (tv == get_tarval_null(mode))
      fprintf(f, "get_tarval_null(mode)");
    else if (tv == get_tarval_one(mode))
      fprintf(f, "get_tarval_one(mode)");
    else if (tv == get_tarval_b_false())
      fprintf(f, "get_tarval_b_false()");
    else if (tv == get_tarval_b_true())
      fprintf(f, "get_tarval_b_true()");
	else if (tv == get_tarval_minus_one(mode))
	  fprintf(f, "get_tarval_minus_one(mode)");
    else if (mode_is_float(mode)) {
		if (tv == get_tarval_nan(mode))
		  fprintf(f, "get_tarval_nan(mode)");
		else if (tv == get_tarval_minus_inf(mode))
		  fprintf(f, "get_tarval_minus_inf(mode)");
		else if (tv == get_tarval_plus_inf(mode))
		  fprintf(f, "get_tarval_plus_inf(mode)");
		else
		  goto def_mode;
	}
    else
      goto def_mode;
  }
  else {
    if (tv == get_tarval_null(mode))
      fprintf(f, "get_tarval_null(mode)");
    else {
def_mode:
      tarval_snprintf(buf, sizeof(buf), tv);
      fprintf(f, "new_tarval_from_str(\"%s\", %u, ", buf, strlen(buf));
      fprintf(f, "mode_%s)", get_mode_name(mode));
    }
  }

  if (tp != get_unknown_type())
    fprintf(f, ", %s", get_type_name(tp));
  fprintf(f, ");\n");
}

/**
 * Generate code for a SymConst.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_SymConst(FILE *f, ir_node *n)
{
  ident *id;
  ir_type *tp;
  ir_entity *ent;
  ir_label_t label;
  symconst_kind kind = get_SymConst_kind(n);
  const char *k_name = "NULL";
  const char *str;

  switch (kind) {
  case symconst_addr_ent:
    fprintf(f, "  sym.entity_p = ENTITY(%s);\n", get_entity_name(get_SymConst_entity(n)));
    k_name = "symconst_addr_ent";
    break;
  case symconst_addr_name:
    id = get_SymConst_name(n);
    str = get_id_str(id);
    fprintf(f, "  sym.ident_p = new_id_from_chars(\"%s\", %d);\n", str, get_id_strlen(id));
    k_name = "symconst_addr_name";
    break;
  case symconst_type_size:
    tp = get_SymConst_type(n);
    fprintf(f, "  sym.type_p = %s;\n", get_type_name(tp));
    k_name = "symconst_type_size";
    break;
  case symconst_type_align:
    tp = get_SymConst_type(n);
    fprintf(f, "  sym.type_p = %s;\n", get_type_name(tp));
    k_name = "symconst_type_align";
    break;
  case symconst_type_tag:
    tp = get_SymConst_type(n);
    fprintf(f, "  sym.type_p = %s;\n", get_type_name(tp));
    k_name = "symconst_type_tag";
    break;
  case symconst_ofs_ent:
    ent = get_SymConst_entity(n);
    fprintf(f, "  sym.entity_p = %s;\n", get_entity_name(ent));
    k_name = "symconst_ofs_ent";
    break;
  case symconst_enum_const:
    id = get_SymConst_name(n);
    str = get_id_str(id);
    fprintf(f, "  sym.ident_p = new_id_from_chars(\"%s\", %d);\n", str, get_id_strlen(id));
    k_name = "symconst_enum_const";
    break;
  case symconst_label:
    label = get_SymConst_label(n);
    fprintf(f, "  sym.label = %lu;\n", label);
    k_name = "symconst_label";
    break;
  }
  name(f, "  ", n, " = new_r_SymConst(irg, ");
  name(f, NULL, get_nodes_block(n), ", sym, ");
  fprintf(f, "%s);\n", k_name);
}

/**
 * Generate code for a Sel.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Sel(FILE *f, ir_node *n)
{
  int n_index = get_Sel_n_indexs(n);
  int i;
  ir_op *op;
  ir_node *ptr = get_Sel_ptr(n);
  ir_entity *ent = get_Sel_entity(n);
  int have_ent = 0;

  if (n_index > 0) {
    ir_type *tp;
    ir_entity *up_ent;

    /* try to detect the array entity */
    op = get_irn_op(ptr);
    if (op == op_SymConst && get_SymConst_kind(ptr) == symconst_addr_ent) {
      up_ent = get_SymConst_entity(ptr);
      tp = get_entity_type(up_ent);
      if (is_Array_type(tp) && get_array_element_entity(tp) == ent) {
        name(f, "  ent = get_SymConst_entity(", ptr, ");\n");
        fprintf(f, "  ent = get_array_element_entity(get_entity_type(ent));\n");
        have_ent = 1;
      }
    }
    else if (op == op_Sel) {
      up_ent = get_Sel_entity(ptr);
      tp = get_entity_type(up_ent);
      if (is_Array_type(tp) && get_array_element_entity(tp) == ent) {
        name(f, "  ent = get_Sel_entity(", ptr, ");\n");
        fprintf(f, "  ent = get_array_element_entity(get_entity_type(ent));\n");
        have_ent = 1;
      }
    }
  }
  if (n_index > 1)
    for (i = 0; i < n_index; ++i) {
      fprintf(f, "  in[%d] = ", i);
      name(f, NULL, get_Sel_index(n, i), ";\n");
    }

  if (n_index == 0)
    name(f, "  ", n, " = new_r_simpleSel(irg, ");
  else
    name(f, "  ", n, " = new_r_Sel(irg, ");
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_Sel_mem(n), ", ");
  name(f, NULL, ptr, ", ");
  if (n_index == 1)
    name(f, "1, &", get_Sel_index(n, 0), ", ");
  else if (n_index > 0)
    fprintf(f, "%d, in, ", n_index);

  ent = get_Sel_entity(n);
  if (have_ent)
    fprintf(f, "ent);\n");
  else
    fprintf(f, "ENTITY(%s));\n", get_entity_name(get_Sel_entity(n)));
}

/**
 * Generate code for a Block. Break loops here.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Block(fix_ctx *env, ir_node *n)
{
  FILE *f = env->f;
  int i, arity = get_irn_arity(n);
  for (i = 0; i < arity; ++i) {
    fprintf(f, "  in[%d] = ", i);
    if (is_backedge(n, i)) {
      fprintf(f, "unknownX;\n");
      env->has_backedges = 1;
    }
    else
      name(f, NULL, get_irn_n(n, i), ";\n");
  }

  name(f, "  ", n, " = new_r_Block(irg, ");
  fprintf(f, "%d, in);\n", arity);
}

/**
 * Generate code for a Phi. Break loops here.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Phi(fix_ctx *env, ir_node *n)
{
  FILE *f = env->f;
  int i, arity = get_irn_arity(n);
  ir_mode *mode = get_irn_mode(n);

  if (has_backedges(n)) {
    fprintf(f, "  unknown = new_r_Unknown(irg, mode_%s);\n", get_mode_name(mode));
    env->has_backedges = 1;
  }
  for (i = 0; i < arity; ++i) {
    fprintf(f, "  in[%d] = ", i);
    if (is_backedge(n, i))
      fprintf(f, "unknown;\n");
    else
      name(f, NULL, get_irn_n(n, i), ";\n");
  }

  name(f, "  ", n, " = new_r_Phi(irg, ");
  fprintf(f, "%d, in, mode_%s);\n", arity, get_mode_name(mode));
}

/**
 * Generate code for a Proj(Load).
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Proj_Load(FILE *f, ir_node *n)
{
  ir_node *pred = get_Proj_pred(n);
  ir_node *block = get_nodes_block(n);
  long proj_nr = get_Proj_proj(n);
  const char *mode = "NULL";
  const char *proj= "-1";

  switch(proj_nr) {
  case pn_Load_M:
    mode = "mode_M";
    proj = "pn_Load_M";
    break;
  case pn_Load_X_regular:
    mode = "mode_X";
    proj = "pn_Load_X_regular";
    break;
  case pn_Load_X_except:
    mode = "mode_X";
    proj = "pn_Load_X_except";
    break;
  case pn_Load_res:
    mode = "mode";
    proj = "pn_Load_res";
    name(f, "  mode = get_Load_mode(", pred, ");\n");
    break;
  }
  name(f, "  ", n, " = new_r_Proj(irg, ");
  name(f, NULL, block, ", ");
  name(f, NULL, pred, ", ");
  fprintf(f, "%s, %s);\n", mode, proj);
}

/**
 * Generate code for a Proj(Store).
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Proj_Store(FILE *f, ir_node *n)
{
  ir_node *pred = get_Proj_pred(n);
  ir_node *block = get_nodes_block(n);
  long proj_nr = get_Proj_proj(n);
  const char *mode = "NULL";
  const char *proj= "-1";

  switch(proj_nr) {
  case pn_Store_M:
    mode = "mode_M";
    proj = "pn_Store_M";
    break;
  case pn_Store_X_regular:
    mode = "mode_X";
    proj = "pn_Store_X_regular";
    break;
  case pn_Store_X_except:
    mode = "mode_X";
    proj = "pn_Store_X_except";
    break;
  }
  name(f, "  ", n, " = new_r_Proj(irg, ");
  name(f, NULL, block, ", ");
  name(f, NULL, pred, ", ");
  fprintf(f, "%s, %s);\n", mode, proj);
}

/**
 * Generate code for a Proj(Div).
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Proj_Div(FILE *f, ir_node *n)
{
  ir_node *pred = get_Proj_pred(n);
  ir_node *block = get_nodes_block(n);
  long proj_nr = get_Proj_proj(n);
  const char *mode = "NULL";
  const char *proj= "-1";

  switch(proj_nr) {
  case pn_Div_M:
    mode = "mode_M";
    proj = "pn_Div_M";
    break;
  case pn_Div_X_regular:
    mode = "mode_X";
    proj = "pn_Div_X_regular";
    break;
  case pn_Div_X_except:
    mode = "mode_X";
    proj = "pn_Div_X_except";
    break;
  case pn_Div_res:
    mode = "mode";
    proj = "pn_Div_res";
    name(f, "  mode = get_Div_resmode(", pred, ");\n");
    break;
  }
  name(f, "  ", n, " = new_r_Proj(irg, ");
  name(f, NULL, block, ", ");
  name(f, NULL, pred, ", ");
  fprintf(f, "%s, %s);\n", mode, proj);
}

/**
 * Generate code for a Proj(Mod).
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Proj_Mod(FILE *f, ir_node *n)
{
  ir_node *pred = get_Proj_pred(n);
  ir_node *block = get_nodes_block(n);
  long proj_nr = get_Proj_proj(n);
  const char *mode = "NULL";
  const char *proj= "-1";

  switch(proj_nr) {
  case pn_Mod_M:
    mode = "mode_M";
    proj = "pn_Mod_M";
    break;
  case pn_Mod_X_regular:
    mode = "mode_X";
    proj = "pn_Mod_X_regular";
    break;
  case pn_Mod_X_except:
    mode = "mode_X";
    proj = "pn_Mod_X_except";
    break;
  case pn_Mod_res:
    mode = "mode";
    proj = "pn_Mod_res";
    name(f, "  mode = get_Mod_resmode(", pred, ");\n");
    break;
  }
  name(f, "  ", n, " = new_r_Proj(irg, ");
  name(f, NULL, block, ", ");
  name(f, NULL, pred, ", ");
  fprintf(f, "%s, %s);\n", mode, proj);
}

/**
 * Generate code for a Proj(DivMod).
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Proj_DivMod(FILE *f, ir_node *n)
{
  ir_node *pred = get_Proj_pred(n);
  ir_node *block = get_nodes_block(n);
  long proj_nr = get_Proj_proj(n);
  const char *mode = "NULL";
  const char *proj= "-1";

  switch(proj_nr) {
  case pn_DivMod_M:
    mode = "mode_M";
    proj = "pn_DivMod_M";
    break;
  case pn_DivMod_X_except:
    mode = "mode_X";
    proj = "pn_DivMod_X_except";
    break;
  case pn_DivMod_res_div:
    mode = "mode";
    proj = "pn_DivMod_res_div";
    name(f, "  mode = get_DivMod_resmode(", pred, ");\n");
    break;
  case pn_DivMod_res_mod:
    mode = "mode";
    proj = "pn_DivMod_res_mod";
    name(f, "  mode = get_DivMod_resmode(", pred, ");\n");
    break;
  }
  name(f, "  ", n, " = new_r_Proj(irg, ");
  name(f, NULL, block, ", ");
  name(f, NULL, pred, ", ");
  fprintf(f, "%s, %s);\n", mode, proj);
}

/**
 * Generate code for a Proj(Cmp).
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Proj_Cmp(FILE *f, ir_node *n)
{
  long proj_nr = get_Proj_proj(n);

  name(f, "  ", n, " = new_r_Proj(irg, ");
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_Proj_pred(n), ", ");
  fprintf(f, "mode_b, %s);\n", get_pnc_string(proj_nr));
}

/**
 * Generate code for a Proj(Cond).
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Proj_Cond(FILE *f, ir_node *n)
{
  ir_node *cond = get_Proj_pred(n);
  ir_node *sel = get_Cond_selector(cond);
  ir_node *block = get_nodes_block(n);
  long proj_nr = get_Proj_proj(n);

  if (get_irn_mode(sel) == mode_b) {
    name(f, "  ", n, " = new_r_Proj(irg, ");
    name(f, NULL, block, ", ");
    name(f, NULL, cond, ", ");
    fprintf(f, "mode_b, %s);\n", proj_nr ? "pn_Cond_true" : "pn_Cond_false");
  }
  else {
    if (proj_nr == get_Cond_defaultProj(cond))
      name(f, "  ", n, " = new_r_defaultProj(irg, ");
    else
      name(f, "  ", n, " = new_r_Proj(irg, ");

    name(f, NULL, block, ", ");
    name(f, NULL, cond, ", ");
    fprintf(f, "mode_%s, %ld);\n", get_mode_name(get_irn_mode(n)), proj_nr);
  }
}

/**
 * Generate code for a Proj.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Proj(FILE *f, ir_node *n)
{
  ir_node *pred = get_Proj_pred(n);

  switch (get_irn_opcode(pred)) {
  case iro_Load:
    generate_code_Proj_Load(f, n);
    break;
  case iro_Store:
    generate_code_Proj_Store(f, n);
    break;
  case iro_Div:
    generate_code_Proj_Div(f, n);
    break;
  case iro_Mod:
    generate_code_Proj_Mod(f, n);
    break;
  case iro_DivMod:
    generate_code_Proj_DivMod(f, n);
    break;
  case iro_Cmp:
    generate_code_Proj_Cmp(f, n);
    break;
  case iro_Cond:
    generate_code_Proj_Cond(f, n);
    break;
  default: {
    const char *mode_name = get_mode_name(get_irn_mode(n));
    name(f, "  ", n, " = new_r_Proj(irg, ");
    name(f, NULL, get_nodes_block(n), ", ");
    name(f, NULL, get_Proj_pred(n), ", ");
    fprintf(f, "mode_%s, %ld);\n", mode_name, get_Proj_proj(n));
  }
  }
}

/**
 * Generate code for a Jmp.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Jmp(FILE *f, ir_node *n)
{
  name(f, "  ", n, " = new_r_Jmp(irg, ");
  name(f, NULL, get_nodes_block(n), ");\n");
}

/**
 * Generate code for a Cond.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Cond(FILE *f, ir_node *n)
{
  name(f, "  ", n, " = new_r_Cond(irg, ");
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_Cond_selector(n), ");\n");
}

/**
 * Generate code for a Call.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Call(FILE *f, ir_node *n)
{
  int i, n_param = get_Call_n_params(n);
  ir_node *ptr = get_Call_ptr(n);
  ir_op *op = get_irn_op(ptr);
  ir_entity *ent;
  ir_type *tp = get_Call_type(n);
  int have_tp = 0;

  /* try to detect the type */
  if (op == op_Sel) {
    ent = get_Sel_entity(ptr);
    if (tp == get_entity_type(ent)) {
      name(f, "  ent = get_Sel_entity(", ptr, ");\n");
      fprintf(f, "  tp = get_entity_type(ent);\n");
      have_tp = 1;
    }
  }
  else if (op == op_SymConst && get_SymConst_kind(ptr) == symconst_addr_ent) {
    ent = get_SymConst_entity(ptr);
    if (tp == get_entity_type(ent)) {
      name(f, "  ent = get_SymConst_entity(", ptr, ");\n");
      fprintf(f, "  tp = get_entity_type(ent);\n");
      have_tp = 1;
    }
  }
  if (! have_tp)
    fprintf(f, "  tp = TYPE(%s);\n", get_type_name(tp));

  for (i = 0; i < n_param; ++i) {
    fprintf(f, "  in[%d] = ", i);
    name(f, NULL, get_Call_param(n, i), ";\n");
  }
  name(f, "  ", n, " = ");
  fprintf(f, "new_r_Call(irg, ");
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_Call_mem(n), ", ");
  if (n_param > 0)
    fprintf(f, "%d, in, tp);\n", n_param);
  else
    fprintf(f, "0, NULL, tp);\n");
}

/**
 * Create code for the given Alloc node
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Alloc(FILE *f, ir_node *n)
{
  name(f, "  ", n, " = new_r_Alloc(irg, ");
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_Alloc_mem(n), ", ");
  name(f, NULL, get_Alloc_size(n), ", tp, ");
  fprintf(f, get_Alloc_where(n) == stack_alloc ? "stack_alloc);\n" : "heap_alloc);\n");
}

/**
 * Create code for the given Free node
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_code_Free(FILE *f, ir_node *n)
{
  fprintf(f, "  tp = TYPE(%s);\n", get_type_name(get_Free_type(n)));
  name(f, "  ", n, " = new_r_Alloc(irg, ");
  name(f, NULL, get_nodes_block(n), ", ");
  name(f, NULL, get_Free_ptr(n), ", ");
  name(f, NULL, get_Free_size(n), ", tp, ");
  fprintf(f, get_Free_where(n) == stack_alloc ? "stack_alloc);\n" : "heap_alloc);\n");
}

/**
 * Create code for the given node constructor.
 *
 * @param f   file handle for output
 * @param n   the node
 */
static void generate_node(fix_ctx *env, ir_node *n)
{
  FILE *f = env->f;
  ir_op *op = get_irn_op(n);

  if (op == op_Div || op == op_Mod || op == op_DivMod || op == op_Quot)
    generate_code_Divop(f, n);
  else if (op == op_Load)
    generate_code_Load(f, n);
  else if (op == op_Store)
    generate_code_Store(f, n);
  else if (op == op_Return)
    generate_code_Return(f, n);
  else if (op == op_Raise)
    generate_code_Raise(f, n);
  else if (op == op_Bound)
    generate_code_Bound(f, n);
  else if (op == op_Unknown)
    generate_code_Unknown(f, n);
  else if (op == op_Bad)
    generate_code_Bad(f, n);
  else if (op == op_Const)
    generate_code_Const(f, n);
  else if (op == op_SymConst)
    generate_code_SymConst(f, n);
  else if (op == op_Sel)
    generate_code_Sel(f, n);
  else if (op == op_Block)
    generate_code_Block(env, n);
  else if (op == op_Phi)
    generate_code_Phi(env, n);
  else if (op == op_Proj)
    generate_code_Proj(f, n);
  else if (op == op_Jmp)
    generate_code_Jmp(f, n);
  else if (op == op_Cond)
    generate_code_Cond(f, n);
  else if (op == op_Call)
    generate_code_Call(f, n);
  else if (op == op_Alloc)
    generate_code_Alloc(f, n);
  else if (op == op_Free)
    generate_code_Free(f, n);
  else if (is_binop(n))
    generate_code_Binop(f, n);
  else if (is_unop(n))
    generate_code_Unop(f, n);
  else
    name(f, "  ", n, " = NULL;\n");
}

/**
 * Walker: Create code for the given node.
 */
static void dump_code(ir_node *n, void *ctx)
{
  fix_ctx *env = ctx;

  if (! generate_predef_node(env->f, n))
    generate_node(env, n);
}

/**
 * Generate Firm code.
 *
 * @param env  a fix environment
 * @param irg  the graph
 */
static void generate_code(fix_ctx *env, ir_graph *irg)
{
  irg_walk_graph(irg, NULL, dump_code, env);
}

/**
 * Generate Firm assembler for a given graph
 *
 * @param f    the output file handle
 * @param irg  the graph
 */
static void gen_Firm_assembler_irg(FILE *f, ir_graph *irg)
{
  fix_ctx fenv;
  var_ctx venv;

  venv.f              = f;
  venv.max_arity      = 0;
  venv.need_tv        = 0;
  venv.need_unknown_X = 0;
  venv.need_unknown   = 0;

  fenv.f              = f;
  fenv.start_block    = get_irg_start_block(irg);
  fenv.has_backedges  = 0;

  /* needed to detect loops */
  construct_backedges(irg);

  generate_header(f, irg);
  generate_var_decls(&venv, irg);
  generate_prolog(&venv, irg);
  generate_code(&fenv, irg);
  generate_epilog(&fenv, irg);
}

void gen_Firm_assembler(const char *input_filename)
{
  FILE     *f;
  ir_graph *irg;
  int      i, n;
  (void) input_filename;

  f = fopen("firm_output.c", "w");
  if (f) {
    for (i = 0, n = get_irp_n_irgs(); i < n; ++i) {
      irg = get_irp_irg(i);
      gen_Firm_assembler_irg(f, irg);
    }
  }
  else {
    /* report some error here */
  }
  fclose(f);
}
