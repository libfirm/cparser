/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "ast2firm.h"

#include <assert.h>
#include <libfirm/adt/obst.h>
#include <libfirm/be.h>
#include <libfirm/firm.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>

#include "adt/array.h"
#include "adt/error.h"
#include "adt/strutil.h"
#include "adt/unicode.h"
#include "adt/util.h"
#include "ast/ast_t.h"
#include "ast/constfold.h"
#include "ast/constfoldbits.h"
#include "ast/entity_t.h"
#include "ast/position.h"
#include "ast/printer.h"
#include "ast/symbol_t.h"
#include "ast/symbol_table.h"
#include "ast/type_t.h"
#include "ast/types.h"
#include "ast/walk.h"
#include "driver/diagnostic.h"
#include "driver/lang_features.h"
#include "driver/warning.h"
#include "entitymap_t.h"
#include "firm/firm_opt.h"
#include "jump_target.h"
#include "mangle.h"
#include "parser/parser.h"

typedef struct trampoline_region trampoline_region;
struct trampoline_region {
	ir_entity *function; /**< The function that is called by this trampoline */
	ir_entity *region;   /**< created region for the trampoline */
};

typedef struct complex_value {
	ir_node *real;
	ir_node *imag;
} complex_value;

static int         next_value_number_function;
static jump_target continue_target;
static jump_target break_target;
static ir_node    *current_switch;
static bool        saw_default_label;
static entity_t  **inner_functions;
static jump_target ijmp_target;
static ir_node   **ijmp_ops;
static ir_node   **ijmp_blocks;

#define PUSH_BREAK(val) \
	jump_target const old_break_target = break_target; \
	(init_jump_target(&break_target, (val)))
#define POP_BREAK() \
	((void)(break_target = old_break_target))

#define PUSH_CONTINUE(val) \
	jump_target const old_continue_target = continue_target; \
	(init_jump_target(&continue_target, (val)))
#define POP_CONTINUE() \
	((void)(continue_target = old_continue_target))

#define PUSH_IRG(val) \
	ir_graph *const old_irg = current_ir_graph; \
	ir_graph *const new_irg = (val); \
	((void)(current_ir_graph = new_irg))

#define POP_IRG() \
	(assert(current_ir_graph == new_irg), (void)(current_ir_graph = old_irg))

static const entity_t     *current_function_entity;
static ir_node            *current_function_name;
static ir_node            *current_funcsig;
static ir_graph           *current_function;
static translation_unit_t *current_translation_unit;
static trampoline_region  *current_trampolines;
static ir_type            *current_outer_frame;
static ir_node            *current_static_link;
static ir_entity          *current_vararg_entity;
static entitymap_t         entitymap;

static struct obstack asm_obst;

typedef enum declaration_kind_t {
	DECLARATION_KIND_UNKNOWN,
	DECLARATION_KIND_VARIABLE_LENGTH_ARRAY,
	DECLARATION_KIND_GLOBAL_VARIABLE,
	DECLARATION_KIND_LOCAL_VARIABLE,
	DECLARATION_KIND_LOCAL_VARIABLE_ENTITY,
	DECLARATION_KIND_PARAMETER,
	DECLARATION_KIND_PARAMETER_ENTITY,
	DECLARATION_KIND_FUNCTION,
	DECLARATION_KIND_COMPOUND_MEMBER,
	DECLARATION_KIND_INNER_FUNCTION
} declaration_kind_t;

static void enqueue_inner_function(entity_t *entity)
{
	if (inner_functions == NULL)
		inner_functions = NEW_ARR_F(entity_t *, 0);
	ARR_APP1(entity_t*, inner_functions, entity);
}

static ir_node *uninitialized_local_var(ir_graph *irg, ir_mode *mode, int pos)
{
	const entity_t *entity = get_irg_loc_description(irg, pos);
	if (entity)
		warningf(WARN_UNINITIALIZED, &entity->base.pos, "'%N' might be used uninitialized", entity);
	return new_r_Unknown(irg, mode);
}

static src_loc_t dbg_retrieve(const dbg_info *dbg)
{
	position_t const *const pos = (position_t const*)dbg;
	if (pos) {
		return (src_loc_t){ pos->input_name, pos->lineno, pos->colno };
	} else {
		return (src_loc_t){ NULL, 0, 0 };
	}
}

static dbg_info *get_dbg_info(const position_t *pos)
{
	return (dbg_info*) pos;
}

static void dbg_print_type_dbg_info(char *buffer, size_t buffer_size,
                                    const type_dbg_info *dbg)
{
	assert(dbg != NULL);
	print_to_buffer(buffer, buffer_size);
	const type_t *type = (const type_t*) dbg;
	print_type(type);
	finish_print_to_buffer();
}

static type_dbg_info *get_type_dbg_info_(const type_t *type)
{
	return (type_dbg_info*) type;
}

/* is the current block a reachable one? */
static bool currently_reachable(void)
{
	ir_node *const block = get_cur_block();
	return block != NULL && !is_Bad(block);
}

static void set_unreachable_now(void)
{
	set_cur_block(NULL);
}

static ir_node *expression_to_control_flow(expression_t const *expr, jump_target *true_target, jump_target *false_target);
static ir_node *expression_to_value(expression_t const *expr);
static complex_value expression_to_complex(const expression_t *expression);

static ir_node *get_vla_size(array_type_t *const type)
{
	ir_node *size_node = type->size_node;
	if (size_node == NULL) {
		size_node = expression_to_value(type->size_expression);
		type->size_node = size_node;
	}
	return size_node;
}

static unsigned count_parameters(const function_type_t *function_type)
{
	unsigned count = 0;

	function_parameter_t *parameter = function_type->parameters;
	for ( ; parameter != NULL; parameter = parameter->next) {
		++count;
	}

	return count;
}

static ir_type *create_primitive_irtype(type_t const *const type, ir_mode *const mode)
{
	type_dbg_info *const dbgi   = get_type_dbg_info_(type);
	ir_type       *const irtype = new_type_primitive(mode);
	set_type_dbg_info(irtype, dbgi);

	unsigned const align = get_type_alignment(type);
	set_type_alignment_bytes(irtype, align);

	unsigned const size = get_type_size(type);
	set_type_size_bytes(irtype, size);

	return irtype;
}

/**
 * Creates a Firm type for an atomic type
 */
static ir_type *create_atomic_type(type_t const *const type)
{
	return create_primitive_irtype(type, atomic_modes[type->atomic.akind]);
}

static ir_type *get_ir_type(type_t *type);

/**
 * Creates a Firm type for a complex type
 */
static ir_type *create_complex_type(type_t const *const type)
{
	type_dbg_info *const dbgi    = get_type_dbg_info_(type);
	type_t        *const etype   = make_atomic_type(type->atomic.akind, TYPE_QUALIFIER_NONE);
	ir_type       *const iretype = get_ir_type(etype);
	ir_type       *const irtype  = new_type_array(1, iretype);
	set_type_dbg_info(irtype, dbgi);

	unsigned const align = get_type_alignment(type);
	set_type_alignment_bytes(irtype, align);

	unsigned const size = get_type_size(type);
	set_type_size_bytes(irtype, size);

	set_array_bounds_int(irtype, 0, 0, 2);
	set_type_state(irtype, layout_fixed);

	return irtype;
}

/**
 * return type of a parameter (and take transparent union gnu extension into
 * account)
 */
static type_t *get_parameter_type(type_t *orig_type)
{
	type_t *type = skip_typeref(orig_type);
	if (is_type_union(type)
			&& get_type_modifiers(orig_type) & DM_TRANSPARENT_UNION) {
		compound_t *compound = type->compound.compound;
		type                 = compound->members.entities->declaration.type;
	}

	return type;
}

static ir_type *create_method_type(const function_type_t *function_type,
                                   bool for_closure)
{
	type_t        *return_type  = skip_typeref(function_type->return_type);

	int            n_parameters = count_parameters(function_type)
	                               + (for_closure ? 1 : 0);
	int            n_results    = is_type_void(return_type) ? 0 : 1;
	type_dbg_info *dbgi         = get_type_dbg_info_((const type_t*) function_type);
	ir_type       *irtype       = new_type_method(n_parameters, n_results);
	set_type_dbg_info(irtype, dbgi);

	if (!is_type_void(return_type)) {
		ir_type *restype = get_ir_type(return_type);
		set_method_res_type(irtype, 0, restype);
	}

	function_parameter_t *parameter = function_type->parameters;
	int                   n         = 0;
	if (for_closure) {
		ir_type *p_irtype = get_ir_type(type_void_ptr);
		set_method_param_type(irtype, n, p_irtype);
		++n;
	}
	for ( ; parameter != NULL; parameter = parameter->next) {
		type_t  *type     = get_parameter_type(parameter->type);
		ir_type *p_irtype = get_ir_type(type);
		set_method_param_type(irtype, n, p_irtype);
		++n;
	}

	bool is_variadic = function_type->variadic;
	if (is_variadic)
		set_method_variadicity(irtype, variadicity_variadic);

	unsigned cc = get_method_calling_convention(irtype);
	switch (function_type->calling_convention) {
	case CC_DEFAULT: /* unspecified calling convention, equal to one of the other, typically cdecl */
	case CC_CDECL:
is_cdecl:
		set_method_calling_convention(irtype, SET_CDECL(cc));
		break;

	case CC_STDCALL:
		if (is_variadic)
			goto is_cdecl;

		/* only non-variadic function can use stdcall, else use cdecl */
		set_method_calling_convention(irtype, SET_STDCALL(cc));
		break;

	case CC_FASTCALL:
		if (is_variadic)
			goto is_cdecl;
		/* only non-variadic function can use fastcall, else use cdecl */
		set_method_calling_convention(irtype, SET_FASTCALL(cc));
		break;

	case CC_THISCALL:
		/* Hmm, leave default, not accepted by the parser yet. */
		break;
	}

	if (for_closure)
		set_method_calling_convention(irtype, get_method_calling_convention(irtype) | cc_this_call);

	const decl_modifiers_t modifiers = function_type->modifiers;
	if (modifiers & DM_CONST)
		add_method_additional_properties(irtype, mtp_property_const);
	if (modifiers & DM_PURE)
		add_method_additional_properties(irtype, mtp_property_pure);
	if (modifiers & DM_RETURNS_TWICE)
		add_method_additional_properties(irtype, mtp_property_returns_twice);
	if (modifiers & DM_NORETURN)
		add_method_additional_properties(irtype, mtp_property_noreturn);
	if (modifiers & DM_NOTHROW)
		add_method_additional_properties(irtype, mtp_property_nothrow);
	if (modifiers & DM_MALLOC)
		add_method_additional_properties(irtype, mtp_property_malloc);

	return irtype;
}

static ir_type *create_pointer_type(type_t const *const type, type_t *const points_to)
{
	ir_type *const ir_points_to = get_ir_type(points_to);
	/* Break a potential cycle: Getting a struct type might in turn trigger
	 * creating this pointer type. */
	if (type->base.firm_type)
		return type->base.firm_type;

	type_dbg_info *const dbgi = get_type_dbg_info_(type);
	ir_type *res = new_type_pointer(ir_points_to);
	set_type_dbg_info(res, dbgi);
	return res;
}

static ir_type *create_array_type(type_t const *const type)
{
	type_dbg_info *const dbgi    = get_type_dbg_info_(type);
	ir_type       *const iretype = get_ir_type(type->array.element_type);
	ir_type       *const irtype  = new_type_array(1, iretype);
	set_type_dbg_info(irtype, dbgi);

	unsigned const align = get_type_alignment(type);
	set_type_alignment_bytes(irtype, align);

	if (type->array.size_constant) {
		set_array_bounds_int(irtype, 0, 0, type->array.size);

		unsigned const size = get_type_size(type);
		set_type_size_bytes(irtype, size);
	} else {
		set_array_lower_bound_int(irtype, 0, 0);
	}
	set_type_state(irtype, layout_fixed);

	return irtype;
}

/**
 * Construct firm type from ast struct type.
 */
static ir_type *create_compound_type(type_t *const type)
{
	bool        const is_union = type->base.kind == TYPE_COMPOUND_UNION;
	compound_t *const compound = type->compound.compound;

	symbol_t *type_symbol = compound->base.symbol;
	ident    *id;
	if (type_symbol != NULL) {
		id = new_id_from_str(type_symbol->string);
	} else {
		if (is_union) {
			id = id_unique("__anonymous_union.%u");
		} else {
			id = id_unique("__anonymous_struct.%u");
		}
	}

	type_dbg_info *const tdbgi  = get_type_dbg_info_(type);
	ir_type       *const irtype
		= is_union ? new_type_union(id) : new_type_struct(id);
	set_type_dbg_info(irtype, tdbgi);

	/* Set firm type right away, to break potential cycles. */
	type->base.firm_type = irtype;

	entity_t *entry = compound->members.entities;
	for ( ; entry != NULL; entry = entry->base.next) {
		if (entry->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		symbol_t *symbol     = entry->base.symbol;
		type_t   *entry_type = entry->declaration.type;
		ident    *member_id;
		if (symbol == NULL) {
			/* anonymous bitfield member, skip */
			if (entry->compound_member.bitfield)
				continue;
			assert(is_type_compound(entry_type));
			member_id = id_unique("anon.%u");
		} else {
			member_id = new_id_from_str(symbol->string);
		}

		ir_type   *entry_irtype = get_ir_type(entry_type);
		dbg_info  *dbgi   = get_dbg_info(&entry->base.pos);
		ir_entity *entity = new_entity(irtype, member_id, entry_irtype);
		set_entity_dbg_info(entity, dbgi);

		set_entity_offset(entity, entry->compound_member.offset);
		if (entry->compound_member.bitfield) {
			set_entity_bitfield_offset(entity,
			                           entry->compound_member.bit_offset);
			set_entity_bitfield_size(entity, entry->compound_member.bit_size);
		}

		assert(entry->declaration.kind == DECLARATION_KIND_UNKNOWN);
		entry->declaration.kind       = DECLARATION_KIND_COMPOUND_MEMBER;
		entry->compound_member.entity = entity;
	}

	set_type_alignment_bytes(irtype, compound->alignment);
	set_type_size_bytes(irtype, compound->size);
	set_type_state(irtype, layout_fixed);

	return irtype;
}

static ir_type *create_ir_type(type_t *const type)
{
	switch (type->kind) {
	case TYPE_ARRAY:           return create_array_type(type);
	case TYPE_ATOMIC:
	case TYPE_ENUM:
	case TYPE_IMAGINARY:       return create_atomic_type(type);
	case TYPE_COMPLEX:         return create_complex_type(type);
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:  return create_compound_type(type);
	case TYPE_FUNCTION:        return create_method_type(&type->function, false);
	case TYPE_POINTER:         return create_pointer_type(type, type->pointer.points_to);
	case TYPE_REFERENCE:       return create_pointer_type(type, type->reference.refers_to);
	case TYPE_VOID:            return create_primitive_irtype(type, mode_ANY);

	case TYPE_ERROR:
	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
	case TYPE_BUILTIN_TEMPLATE:
		break;
	}
	panic("invalid type");
}

static ir_type *get_ir_type(type_t *type)
{
	type = skip_typeref(type);

	ir_type **const irtype = &type->base.firm_type;
	if (!*irtype) {
		type_t   *const utype   = get_unqualified_type(type);
		ir_type **const irutype = &utype->base.firm_type;
		if (!*irutype)
			*irutype = create_ir_type(utype);
		*irtype = *irutype;
	}
	return *irtype;
}

/**
 * Return a node representing the size of a type.
 */
static ir_node *get_type_size_node(type_t *type)
{
	ir_mode *const mode = get_ir_mode_storage(type_size_t);
	type = skip_typeref(type);

	if (is_type_array(type) && type->array.is_vla) {
		ir_node *size_node = get_vla_size(&type->array);
		ir_node *elem_size = get_type_size_node(type->array.element_type);
		ir_node *real_size = new_d_Mul(NULL, size_node, elem_size, mode);
		return real_size;
	}

	unsigned const size = get_type_size(type);
	return new_Const_long(mode, size);
}

/** Names of the runtime functions. */
static const struct {
	int        id;           /**< the rts id */
	int        n_res;        /**< number of return values */
	const char *name;        /**< the name of the rts function */
	int        n_params;     /**< number of parameters */
	unsigned   flags;        /**< language flags */
} rts_data[] = {
	{ rts_debugbreak, 0, "__debugbreak", 0, _MS },
	{ rts_abort,      0, "abort",        0, _C89 },
	{ rts_alloca,     1, "alloca",       1, _ALL },
	{ rts_abs,        1, "abs",          1, _C89 },
	{ rts_labs,       1, "labs",         1, _C89 },
	{ rts_llabs,      1, "llabs",        1, _C99 },
	{ rts_imaxabs,    1, "imaxabs",      1, _C99 },

	{ rts_fabs,       1, "fabs",         1, _C89 },
	{ rts_sqrt,       1, "sqrt",         1, _C89 },
	{ rts_cbrt,       1, "cbrt",         1, _C99 },
	{ rts_exp,        1, "exp",          1, _C89 },
	{ rts_exp2,       1, "exp2",         1, _C89 },
	{ rts_exp10,      1, "exp10",        1, _GNUC },
	{ rts_log,        1, "log",          1, _C89 },
	{ rts_log2,       1, "log2",         1, _C89 },
	{ rts_log10,      1, "log10",        1, _C89 },
	{ rts_pow,        1, "pow",          2, _C89 },
	{ rts_sin,        1, "sin",          1, _C89 },
	{ rts_cos,        1, "cos",          1, _C89 },
	{ rts_tan,        1, "tan",          1, _C89 },
	{ rts_asin,       1, "asin",         1, _C89 },
	{ rts_acos,       1, "acos",         1, _C89 },
	{ rts_atan,       1, "atan",         1, _C89 },
	{ rts_sinh,       1, "sinh",         1, _C89 },
	{ rts_cosh,       1, "cosh",         1, _C89 },
	{ rts_tanh,       1, "tanh",         1, _C89 },

	{ rts_fabsf,      1, "fabsf",        1, _C99 },
	{ rts_sqrtf,      1, "sqrtf",        1, _C99 },
	{ rts_cbrtf,      1, "cbrtf",        1, _C99 },
	{ rts_expf,       1, "expf",         1, _C99 },
	{ rts_exp2f,      1, "exp2f",        1, _C99 },
	{ rts_exp10f,     1, "exp10f",       1, _GNUC },
	{ rts_logf,       1, "logf",         1, _C99 },
	{ rts_log2f,      1, "log2f",        1, _C99 },
	{ rts_log10f,     1, "log10f",       1, _C99 },
	{ rts_powf,       1, "powf",         2, _C99 },
	{ rts_sinf,       1, "sinf",         1, _C99 },
	{ rts_cosf,       1, "cosf",         1, _C99 },
	{ rts_tanf,       1, "tanf",         1, _C99 },
	{ rts_asinf,      1, "asinf",        1, _C99 },
	{ rts_acosf,      1, "acosf",        1, _C99 },
	{ rts_atanf,      1, "atanf",        1, _C99 },
	{ rts_sinhf,      1, "sinhf",        1, _C99 },
	{ rts_coshf,      1, "coshf",        1, _C99 },
	{ rts_tanhf,      1, "tanhf",        1, _C99 },

	{ rts_fabsl,      1, "fabsl",        1, _C99 },
	{ rts_sqrtl,      1, "sqrtl",        1, _C99 },
	{ rts_cbrtl,      1, "cbrtl",        1, _C99 },
	{ rts_expl,       1, "expl",         1, _C99 },
	{ rts_exp2l,      1, "exp2l",        1, _C99 },
	{ rts_exp10l,     1, "exp10l",       1, _GNUC },
	{ rts_logl,       1, "logl",         1, _C99 },
	{ rts_log2l,      1, "log2l",        1, _C99 },
	{ rts_log10l,     1, "log10l",       1, _C99 },
	{ rts_powl,       1, "powl",         2, _C99 },
	{ rts_sinl,       1, "sinl",         1, _C99 },
	{ rts_cosl,       1, "cosl",         1, _C99 },
	{ rts_tanl,       1, "tanl",         1, _C99 },
	{ rts_asinl,      1, "asinl",        1, _C99 },
	{ rts_acosl,      1, "acosl",        1, _C99 },
	{ rts_atanl,      1, "atanl",        1, _C99 },
	{ rts_sinhl,      1, "sinhl",        1, _C99 },
	{ rts_coshl,      1, "coshl",        1, _C99 },
	{ rts_tanhl,      1, "tanhl",        1, _C99 },

	{ rts_strcmp,     1, "strcmp",       2, _C89 },
	{ rts_strncmp,    1, "strncmp",      3, _C89 },
	{ rts_strcpy,     1, "strcpy",       2, _C89 },
	{ rts_strlen,     1, "strlen",       1, _C89 },
	{ rts_memcpy,     1, "memcpy",       3, _C89 },
	{ rts_mempcpy,    1, "mempcpy",      3, _GNUC },
	{ rts_memmove,    1, "memmove",      3, _C89 },
	{ rts_memset,     1, "memset",       3, _C89 },
	{ rts_memcmp,     1, "memcmp",       3, _C89 },
};

static ident *rts_idents[ARRAY_SIZE(rts_data)];

static create_ld_ident_func create_ld_ident = create_name_linux_elf;

void set_create_ld_ident(ident *(*func)(entity_t*))
{
	create_ld_ident = func;
}

static bool declaration_is_definition(const entity_t *entity)
{
	switch (entity->kind) {
	case ENTITY_VARIABLE:
		return entity->declaration.storage_class != STORAGE_CLASS_EXTERN;
	case ENTITY_FUNCTION:
		return entity->function.body != NULL
		    || entity->function.alias.entity != NULL;
	case ENTITY_PARAMETER:
	case ENTITY_COMPOUND_MEMBER:
		return false;
	case ENTITY_ASM_ARGUMENT:
	case ENTITY_CLASS:
	case ENTITY_ENUM:
	case ENTITY_ENUM_VALUE:
	case ENTITY_LABEL:
	case ENTITY_LOCAL_LABEL:
	case ENTITY_NAMESPACE:
	case ENTITY_STRUCT:
	case ENTITY_TYPEDEF:
	case ENTITY_UNION:
		break;
	}
	panic("entity is not a declaration");
}

/**
 * Handle GNU attributes for entities
 *
 * @param ent   the entity
 * @param decl  the routine declaration
 */
static void handle_decl_modifiers(ir_entity *irentity, entity_t *entity)
{
	assert(is_declaration(entity));
	decl_modifiers_t modifiers = entity->declaration.modifiers;

	if (is_method_entity(irentity)) {
		if (modifiers & DM_PURE)
			add_entity_additional_properties(irentity, mtp_property_pure);
		if (modifiers & DM_CONST)
			add_entity_additional_properties(irentity, mtp_property_const);
		if (modifiers & DM_NOINLINE)
			add_entity_additional_properties(irentity, mtp_property_noinline);
		if (modifiers & DM_FORCEINLINE)
			add_entity_additional_properties(irentity, mtp_property_always_inline);
		if (modifiers & DM_NAKED)
			add_entity_additional_properties(irentity, mtp_property_naked);
		if (entity->kind == ENTITY_FUNCTION && entity->function.is_inline)
			add_entity_additional_properties(irentity,
											 mtp_property_inline_recommended);
	}
	if ((modifiers & DM_USED) && declaration_is_definition(entity)) {
		add_entity_linkage(irentity, IR_LINKAGE_HIDDEN_USER);
	}
	if ((modifiers & DM_WEAK) && declaration_is_definition(entity)
	    && entity->declaration.storage_class != STORAGE_CLASS_EXTERN) {
		add_entity_linkage(irentity, IR_LINKAGE_WEAK);
	}
}

static bool is_main(entity_t *entity)
{
	static symbol_t *sym_main = NULL;
	if (sym_main == NULL) {
		sym_main = symbol_table_insert("main");
	}

	if (entity->base.symbol != sym_main)
		return false;
	/* must be in outermost scope */
	if (entity->base.parent_scope != &current_translation_unit->scope)
		return false;

	return true;
}

/**
 * Creates an entity representing a function.
 *
 * @param entity       the function declaration/definition
 * @param owner_type   the owner type of this function, NULL
 *                     for global functions
 */
static ir_entity *get_function_entity(entity_t *entity, ir_type *owner_type)
{
	assert(entity->kind == ENTITY_FUNCTION);
	if (entity->function.irentity != NULL)
		return entity->function.irentity;

	switch (entity->function.btk) {
	case BUILTIN_NONE:
	case BUILTIN_LIBC:
	case BUILTIN_LIBC_CHECK:
		break;
	default:
		return NULL;
	}

	symbol_t *symbol = entity->base.symbol;
	ident    *id     = new_id_from_str(symbol->string);

	/* already an entity defined? */
	ir_entity *irentity = entitymap_get(&entitymap, symbol);
	bool const has_body = entity->function.body != NULL;
	if (irentity != NULL) {
		goto entity_created;
	}

	ir_type *ir_type_method;
	if (entity->function.need_closure)
		ir_type_method = create_method_type(&entity->declaration.type->function, true);
	else
		ir_type_method = get_ir_type(entity->declaration.type);

	bool nested_function = false;
	if (owner_type == NULL)
		owner_type = get_glob_type();
	else
		nested_function = true;

	if (entity->function.alias.entity != NULL) {
		/* create alias entity but do not resolve alias just yet, phase 2 of
		 * the main loop will do so. */
		irentity = new_alias_entity(owner_type, id, NULL, ir_type_method);
	} else {
		irentity = new_entity(owner_type, id, ir_type_method);
	}
	dbg_info *const dbgi = get_dbg_info(&entity->base.pos);
	set_entity_dbg_info(irentity, dbgi);

	ident *ld_id;
	if (nested_function)
		ld_id = id_unique("inner.%u");
	else
		ld_id = create_ld_ident(entity);
	set_entity_ld_ident(irentity, ld_id);

	handle_decl_modifiers(irentity, entity);

	if (!nested_function) {
		storage_class_t const storage_class = entity->declaration.storage_class;
		if (storage_class == STORAGE_CLASS_STATIC) {
		    set_entity_visibility(irentity, ir_visibility_local);
		} else {
		    set_entity_visibility(irentity, ir_visibility_external);
		}

		bool const is_inline = entity->function.is_inline;
		if (is_inline && has_body) {
			if ((dialect.c99 && storage_class == STORAGE_CLASS_NONE)
			    || (!dialect.c99 && storage_class == STORAGE_CLASS_EXTERN)) {
				add_entity_linkage(irentity, IR_LINKAGE_NO_CODEGEN);
			}
		}
	} else {
		/* nested functions are always local */
		set_entity_visibility(irentity, ir_visibility_local);
	}

	/* We should check for file scope here, but as long as we compile C only
	   this is not needed. */
	if (!dialect.freestanding && !has_body) {
		/* check for a known runtime function */
		for (size_t i = 0; i < ARRAY_SIZE(rts_data); ++i) {
			if (id != rts_idents[i])
				continue;

			function_type_t *function_type
				= &entity->declaration.type->function;
			/* rts_entities code can't handle a "wrong" number of parameters */
			if (function_type->unspecified_parameters)
				continue;

			/* check number of parameters */
			int n_params = count_parameters(function_type);
			if (n_params != rts_data[i].n_params)
				continue;

			type_t *return_type = skip_typeref(function_type->return_type);
			int     n_res       = is_type_void(return_type) ? 0 : 1;
			if (n_res != rts_data[i].n_res)
				continue;

			/* ignore those rts functions not necessary needed for current mode */
			if ((dialect.features & rts_data[i].flags) == 0)
				continue;
			assert(rts_entities[rts_data[i].id] == NULL);
			rts_entities[rts_data[i].id] = irentity;
		}
	}

	entitymap_insert(&entitymap, symbol, irentity);

entity_created:
	entity->declaration.kind  = DECLARATION_KIND_FUNCTION;
	entity->function.irentity = irentity;

	return irentity;
}

static ir_node *create_conv(dbg_info *dbgi, ir_node *value, ir_mode *dest_mode)
{
	ir_mode *value_mode = get_irn_mode(value);

	if (value_mode == dest_mode)
		return value;

	return new_d_Conv(dbgi, value, dest_mode);
}

static ir_node *conv_to_storage_type(dbg_info *const dbgi, ir_node *const val, type_t *const type)
{
	ir_mode *const mode = get_ir_mode_storage(type);
	return create_conv(dbgi, val, mode);
}

/**
 * Creates a node representing a string constant.
 *
 * @param src_pos    the source position of the string constant
 * @param id_prefix  a prefix for the name of the generated string constant
 * @param value      the value of the string constant
 */
static ir_node *string_to_firm(position_t const *const src_pos, char const *const id_prefix, string_t const *const value)
{
	size_t            const slen        = get_string_len(value) + 1;
	ir_initializer_t *const initializer = create_initializer_compound(slen);
	ir_type          *      elem_type;
	switch (value->encoding) {
	case STRING_ENCODING_CHAR:
	case STRING_ENCODING_UTF8: {
		elem_type = get_ir_type(type_char);

		ir_mode *const mode = get_type_mode(elem_type);
		char const    *p    = value->begin;
		for (size_t i = 0; i < slen; ++i) {
			ir_tarval        *tv  = new_tarval_from_long(*p++, mode);
			ir_initializer_t *val = create_initializer_tarval(tv);
			set_initializer_compound_value(initializer, i, val);
		}
		goto finish;
	}

	{
		type_t *type;
	case STRING_ENCODING_CHAR16: type = type_char16_t; goto init_wide;
	case STRING_ENCODING_CHAR32: type = type_char32_t; goto init_wide;
	case STRING_ENCODING_WIDE:   type = type_wchar_t;  goto init_wide;
init_wide:;
		elem_type = get_ir_type(type);

		ir_mode *const mode = get_type_mode(elem_type);
		char const    *p    = value->begin;
		for (size_t i = 0; i < slen; ++i) {
			assert(p <= value->begin + value->size);
			utf32             v   = read_utf8_char(&p);
			ir_tarval        *tv  = new_tarval_from_long(v, mode);
			ir_initializer_t *val = create_initializer_tarval(tv);
			set_initializer_compound_value(initializer, i, val);
		}
		goto finish;
	}
	}
	panic("invalid string encoding");

finish:;
	ir_type *const type = new_type_array(1, elem_type);
	set_array_bounds_int(type, 0, 0, slen);
	set_type_size_bytes( type, slen * get_type_size_bytes(elem_type));
	set_type_state(      type, layout_fixed);

	ir_type   *const global_type = get_glob_type();
	ident     *const id          = id_unique(id_prefix);
	dbg_info  *const dbgi        = get_dbg_info(src_pos);
	ir_entity *const entity      = new_entity(global_type, id, type);
	set_entity_dbg_info(   entity, dbgi);
	set_entity_ld_ident(   entity, id);
	set_entity_visibility( entity, ir_visibility_private);
	add_entity_linkage(    entity, IR_LINKAGE_CONSTANT);
	set_entity_initializer(entity, initializer);

	return new_d_Address(dbgi, entity);
}

static bool try_create_integer(literal_expression_t *literal, type_t *type)
{
	assert(type->kind == TYPE_ATOMIC || type->kind == TYPE_COMPLEX);
	atomic_type_kind_t akind = type->atomic.akind;

	ir_mode    *const mode = atomic_modes[akind];
	char const *const str  = literal->value.begin;
	ir_tarval  *const tv   = new_tarval_from_str(str, literal->suffix - str, mode);
	if (tv == tarval_bad)
		return false;

	literal->base.type    = type;
	literal->target_value = tv;
	return true;
}

void determine_literal_type(literal_expression_t *const literal)
{
	assert(literal->base.kind == EXPR_LITERAL_INTEGER);

	/* -1: signed only, 0: any, 1: unsigned only */
	int const sign =
		!is_type_signed(literal->base.type) ? 1 :
		literal->value.begin[0] == '0'      ? 0 :
		-1; /* Decimal literals only try signed types. */

	tarval_int_overflow_mode_t old_mode = tarval_get_integer_overflow_mode();
	tarval_set_integer_overflow_mode(TV_OVERFLOW_BAD);

	if (try_create_integer(literal, literal->base.type))
		goto finished;

	/* now try if the constant is small enough for some types */
	if (sign >= 0 && try_create_integer(literal, type_unsigned_int))
		goto finished;
	if (sign <= 0 && try_create_integer(literal, type_long))
		goto finished;
	if (sign >= 0 && try_create_integer(literal, type_unsigned_long))
		goto finished;
	/* last try? then we should not report tarval_bad */
	if (sign < 0)
		tarval_set_integer_overflow_mode(TV_OVERFLOW_WRAP);
	if (sign <= 0 && try_create_integer(literal, type_long_long))
		goto finished;

	/* last try */
	assert(sign >= 0);
	tarval_set_integer_overflow_mode(TV_OVERFLOW_WRAP);
	bool res = try_create_integer(literal, type_unsigned_long_long);
	if (!res)
		panic("internal error when parsing number literal");

finished:
	tarval_set_integer_overflow_mode(old_mode);
}

/*
 * Allocate an area of size bytes aligned at alignment
 * at a frame type.
 */
static ir_entity *alloc_trampoline(ir_type *frame_type, int size, unsigned alignment)
{
	static unsigned area_cnt = 0;
	char buf[32];

	ir_type *ir_type_char = get_ir_type(type_char);
	ir_type *tp           = new_type_array(1, ir_type_char);
	set_array_bounds_int(tp, 0, 0, size);
	set_type_alignment_bytes(tp, alignment);

	snprintf(buf, sizeof(buf), "trampolin%u", area_cnt++);
	ident *name = new_id_from_str(buf);
	ir_entity *area = new_entity(frame_type, name, tp);

	/* mark this entity as compiler generated */
	set_entity_compiler_generated(area, 1);
	return area;
}

/**
 * Return a node representing a trampoline region
 * for a given function entity.
 *
 * @param dbgi    debug info
 * @param entity  the function entity
 */
static ir_node *get_trampoline_region(dbg_info *dbgi, ir_entity *entity)
{
	ir_entity *region = NULL;
	int        i;

	if (current_trampolines != NULL) {
		for (i = ARR_LEN(current_trampolines) - 1; i >= 0; --i) {
			if (current_trampolines[i].function == entity) {
				region = current_trampolines[i].region;
				break;
			}
		}
	} else {
		current_trampolines = NEW_ARR_F(trampoline_region, 0);
	}
	ir_graph *irg = current_ir_graph;
	if (region == NULL) {
		/* create a new region */
		const backend_params *be_params = be_get_backend_param();
		ir_type              *frame_tp  = get_irg_frame_type(irg);

		trampoline_region reg;
		reg.function = entity;
		reg.region   = alloc_trampoline(frame_tp,
		                                be_params->trampoline_size,
		                                be_params->trampoline_align);
		ARR_APP1(trampoline_region, current_trampolines, reg);
		region = reg.region;
	}
	return new_d_simpleSel(dbgi, get_irg_no_mem(irg), get_irg_frame(irg),
	                       region);
}

/**
 * Creates a trampoline for a function represented by an entity.
 *
 * @param dbgi    debug info
 * @param mode    the (reference) mode for the function address
 * @param entity  the function entity
 */
static ir_node *create_trampoline(dbg_info *dbgi, ir_mode *mode,
                                  ir_entity *entity)
{
	assert(entity != NULL);
	ir_node *in[3];
	in[0] = get_trampoline_region(dbgi, entity);
	in[1] = new_d_Address(dbgi, entity);
	in[2] = get_irg_frame(current_ir_graph);

	ir_node *irn = new_d_Builtin(dbgi, get_store(), 3, in, ir_bk_inner_trampoline, get_unknown_type());
	set_store(new_Proj(irn, mode_M, pn_Builtin_M));
	return new_Proj(irn, mode, pn_Builtin_max+1);
}

/**
 * Dereference an address.
 *
 * @param dbgi  debug info
 * @param type  the type of the dereferenced result (the points_to type)
 * @param addr  the address to dereference
 */
static ir_node *deref_address(dbg_info *const dbgi, type_t *const type,
		                      ir_node *const addr)
{
	type_t *const skipped = skip_typeref(type);
	if (is_type_compound(skipped) ||
	    is_type_function(skipped) ||
	    is_type_array(skipped)    ||
	    /* Exotic case of an unused reference expression of an extern void
	     * variable. */
	    is_type_void(skipped)) {
		return addr;
	}

	ir_cons_flags  flags    = skipped->base.qualifiers & TYPE_QUALIFIER_VOLATILE
	                          ? cons_volatile : cons_none;
	ir_type *const irtype   = get_ir_type(skipped);
	ir_mode *const mode     = get_type_mode(irtype);
	ir_node *const memory   = get_store();
	ir_node *const load     = new_d_Load(dbgi, memory, addr, mode, flags);
	ir_node *const load_mem = new_d_Proj(dbgi, load, mode_M, pn_Load_M);
	ir_node *const load_res = new_d_Proj(dbgi, load, mode,   pn_Load_res);

	set_store(load_mem);
	return load_res;
}

/**
 * Returns the correct base address depending on whether it is a parameter or a
 * normal local variable.
 */
static ir_node *get_local_frame(ir_entity *const ent)
{
	ir_graph      *const irg   = current_ir_graph;
	const ir_type *const owner = get_entity_owner(ent);
	if (owner == current_outer_frame) {
		assert(current_static_link != NULL);
		return current_static_link;
	} else {
		return get_irg_frame(irg);
	}
}

/**
 * Keep the current block and memory.
 * This is necessary for all loops, because they could become infinite.
 */
static void keep_loop(void)
{
	keep_alive(get_cur_block());
	keep_alive(get_store());
}

static ir_node *reference_addr(const reference_expression_t *ref)
{
	dbg_info *dbgi   = get_dbg_info(&ref->base.pos);
	entity_t *entity = ref->entity;
	assert(is_declaration(entity));

	if (entity->kind == ENTITY_FUNCTION
	    && entity->function.btk != BUILTIN_NONE) {
		ir_entity *irentity = get_function_entity(entity, NULL);
		/* for gcc compatibility we have to produce (dummy) addresses for some
		 * builtins which don't have entities */
		if (irentity == NULL) {
			position_t const *const pos = &ref->base.pos;
			warningf(WARN_OTHER, pos, "taking address of builtin '%N'", ref->entity);

			/* simply create a NULL pointer */
			ir_mode *const mode = get_ir_mode_storage(type_void_ptr);
			return new_Const(get_mode_null(mode));
		}
	}

	switch ((declaration_kind_t) entity->declaration.kind) {
	case DECLARATION_KIND_UNKNOWN:
		break;
	case DECLARATION_KIND_PARAMETER:
	case DECLARATION_KIND_LOCAL_VARIABLE:
		/* you can store to a local variable (so we don't panic but return NULL
		 * as an indicator for no real address) */
		return NULL;
	case DECLARATION_KIND_GLOBAL_VARIABLE: {
		ir_node *const addr = new_d_Address(dbgi, entity->variable.v.entity);
		return addr;
	}

	case DECLARATION_KIND_LOCAL_VARIABLE_ENTITY:
	case DECLARATION_KIND_PARAMETER_ENTITY: {
		ir_entity *irentity = entity->variable.v.entity;
		ir_node   *frame    = get_local_frame(irentity);
		ir_node   *sel = new_d_simpleSel(dbgi, new_NoMem(), frame, irentity);
		return sel;
	}

	case DECLARATION_KIND_VARIABLE_LENGTH_ARRAY:
		return entity->variable.v.vla_base;

	case DECLARATION_KIND_FUNCTION: {
		return new_d_Address(dbgi, entity->function.irentity);
	}

	case DECLARATION_KIND_INNER_FUNCTION: {
		type_t  *const type = skip_typeref(entity->declaration.type);
		ir_mode *const mode = get_ir_mode_storage(type);
		if (!entity->function.goto_to_outer && !entity->function.need_closure) {
			/* inner function not using the closure */
			return new_d_Address(dbgi, entity->function.irentity);
		} else {
			/* need trampoline here */
			return create_trampoline(dbgi, mode, entity->function.irentity);
		}
	}

	case DECLARATION_KIND_COMPOUND_MEMBER:
		panic("not implemented reference type");
	}

	panic("reference to declaration with unknown type");
}

static ir_node *reference_expression_to_firm(const reference_expression_t *ref)
{
	dbg_info *const dbgi   = get_dbg_info(&ref->base.pos);
	entity_t *const entity = ref->entity;
	assert(is_declaration(entity));

	switch ((declaration_kind_t)entity->declaration.kind) {
	case DECLARATION_KIND_LOCAL_VARIABLE:
	case DECLARATION_KIND_PARAMETER: {
		type_t  *const type  = skip_typeref(entity->declaration.type);
		ir_mode *const mode  = get_ir_mode_storage(type);
		return get_value(entity->variable.v.value_number, mode);
	}

	default: {
		ir_node *const addr = reference_addr(ref);
		return deref_address(dbgi, entity->declaration.type, addr);
	}
	}
}

/**
 * Transform calls to builtin functions.
 */
static ir_node *process_builtin_call(const call_expression_t *call)
{
	dbg_info *dbgi = get_dbg_info(&call->base.pos);

	assert(call->function->kind == EXPR_REFERENCE);
	reference_expression_t *builtin = &call->function->reference;

	type_t *expr_type = skip_typeref(builtin->base.type);
	assert(is_type_pointer(expr_type));

	type_t *function_type = skip_typeref(expr_type->pointer.points_to);

	switch (builtin->entity->function.btk) {
	case BUILTIN_NONE:
		break;
	case BUILTIN_ALLOCA: {
		expression_t *argument = call->arguments->expression;
		ir_node      *size     = expression_to_value(argument);

		ir_node *store   = get_store();
		ir_node *allocan = new_d_Alloc(dbgi, store, size, 1);
		ir_node *proj_m  = new_Proj(allocan, mode_M, pn_Alloc_M);
		set_store(proj_m);
		ir_node *res     = new_Proj(allocan, mode_P_data, pn_Alloc_res);

		return res;
	}
	case BUILTIN_INF: {
		ir_tarval *tv = fold_builtin_inf(call, function_type);
		return new_d_Const(dbgi, tv);
	}
	case BUILTIN_NAN: {
		ir_tarval *tv = fold_builtin_nan(call, function_type);
		return new_d_Const(dbgi, tv);
	}
	case BUILTIN_EXPECT: {
		expression_t *argument = call->arguments->expression;
		return expression_to_value(argument);
	}
	case BUILTIN_VA_END:
		/* evaluate the argument of va_end for its side effects */
		expression_to_value(call->arguments->expression);
		return NULL;
	case BUILTIN_OBJECT_SIZE: {
		/* determine value of "type" */
		expression_t *type_expression = call->arguments->next->expression;
		long          type_val        = fold_expression_to_int(type_expression);
		type_t       *type            = function_type->function.return_type;
		ir_mode      *mode            = get_ir_mode_storage(type);
		/* just produce a "I don't know" result */
		ir_tarval    *result          = type_val & 2 ? get_mode_null(mode) :
		                                get_mode_minus_one(mode);

		return new_d_Const(dbgi, result);
	}
	case BUILTIN_ROTR:
	case BUILTIN_ROTL: {
		/* firm matches shl/shr patterns for rotl */
		ir_node *val  = expression_to_value(call->arguments->expression);
		ir_node *shf  = expression_to_value(call->arguments->next->expression);
		ir_mode *mode = get_irn_mode(val);
		ir_mode *mode_uint = atomic_modes[ATOMIC_TYPE_UINT];
		ir_node *c    = new_Const_long(mode_uint, get_mode_size_bits(mode));
		ir_node *sub  = new_d_Sub(dbgi, c, create_conv(dbgi, shf, mode_uint), mode_uint);
		ir_node *shlop = builtin->entity->function.btk == BUILTIN_ROTL ? val : sub;
		ir_node *shrop = builtin->entity->function.btk == BUILTIN_ROTR ? val : sub;
		ir_node *shl = new_d_Shl(dbgi, shlop, shf, mode);
		ir_node *shr = new_d_Shr(dbgi, shrop, shf, mode);
		return new_d_Or(dbgi, shl, shr, mode);
	}
	case BUILTIN_FIRM:
		break;
	case BUILTIN_LIBC:
	case BUILTIN_LIBC_CHECK:
		panic("builtin did not produce an entity");
	}
	panic("invalid builtin");
}

static ir_node *complex_to_memory(dbg_info *dbgi, type_t *type,
                                  complex_value value);

/**
 * Transform a call expression.
 * Handles some special cases, like alloca() calls, which must be resolved
 * BEFORE the inlines runs. Inlining routines calling alloca() is dangerous,
 * 176.gcc for instance might allocate 2GB instead of 256 MB if alloca is not
 * handled right...
 */
static ir_node *call_expression_to_firm(const call_expression_t *const call)
{
	dbg_info *const dbgi = get_dbg_info(&call->base.pos);
	assert(currently_reachable());

	expression_t   *function = call->function;
	ir_node        *callee   = NULL;
	bool            firm_builtin = false;
	ir_builtin_kind firm_builtin_kind = ir_bk_trap;
	if (function->kind == EXPR_REFERENCE) {
		const reference_expression_t *ref    = &function->reference;
		entity_t                     *entity = ref->entity;

		if (entity->kind == ENTITY_FUNCTION) {
			builtin_kind_t builtin = entity->function.btk;
			if (builtin == BUILTIN_FIRM) {
				firm_builtin = true;
				firm_builtin_kind = entity->function.b.firm_builtin_kind;
			} else if (builtin != BUILTIN_NONE && builtin != BUILTIN_LIBC
			           && builtin != BUILTIN_LIBC_CHECK) {
				return process_builtin_call(call);
			}
		}
	}
	if (!firm_builtin)
		callee = expression_to_value(function);

	function_type_t *function_type = call->concrete_type;
	if (function_type == NULL) {
		type_t *type = skip_typeref(function->base.type);
		assert(is_type_pointer(type));
		pointer_type_t *pointer_type = &type->pointer;
		type_t         *points_to    = skip_typeref(pointer_type->points_to);
		assert(is_type_function(points_to));
		function_type = &points_to->function;
	}

	int      n_parameters    = 0;
	ir_type *ir_method_type  = get_ir_type((type_t*) function_type);
	ir_type *new_method_type = NULL;
	if (function_type->variadic || function_type->unspecified_parameters) {
		const call_argument_t *argument = call->arguments;
		for ( ; argument != NULL; argument = argument->next) {
			++n_parameters;
		}

		/* we need to construct a new method type matching the call
		 * arguments... */
		type_dbg_info *tdbgi = get_type_dbg_info_((const type_t*) function_type);
		int n_res       = get_method_n_ress(ir_method_type);
		new_method_type = new_type_method(n_parameters, n_res);
		set_type_dbg_info(new_method_type, tdbgi);
		set_method_calling_convention(new_method_type,
		               get_method_calling_convention(ir_method_type));
		set_method_additional_properties(new_method_type,
		               get_method_additional_properties(ir_method_type));
		set_method_variadicity(new_method_type, variadicity_variadic);

		for (int i = 0; i < n_res; ++i) {
			set_method_res_type(new_method_type, i,
			                    get_method_res_type(ir_method_type, i));
		}
		argument = call->arguments;
		for (int i = 0; i < n_parameters; ++i, argument = argument->next) {
			expression_t *expression = argument->expression;
			ir_type      *irtype     = get_ir_type(expression->base.type);
			set_method_param_type(new_method_type, i, irtype);
		}
		ir_method_type = new_method_type;
	} else {
		n_parameters = get_method_n_params(ir_method_type);
	}

	/* variable length array must have length > 0 */
	ir_node *in[n_parameters==0?1:n_parameters];

	const call_argument_t *argument               = call->arguments;
	bool                   has_compound_parameter = false;
	for (int n = 0; n < n_parameters; ++n) {
		expression_t *expression = argument->expression;
		type_t *const arg_type = skip_typeref(expression->base.type);
		if (is_type_complex(arg_type)) {
			complex_value value = expression_to_complex(expression);
			in[n] = complex_to_memory(dbgi, arg_type, value);
			has_compound_parameter = true;
		} else {
			if (is_type_compound(arg_type))
				has_compound_parameter = true;
			in[n] = conv_to_storage_type(dbgi, expression_to_value(expression),
			                             arg_type);
		}

		argument = argument->next;
	}

	/* Const function don't read memory and need no memory input. However
	 * we cannot model compound types as direct values in firm yet and
	 * resort to memory in these cases. */
	ir_node *store;
	if ((function_type->modifiers & DM_CONST) && !has_compound_parameter) {
		store = get_irg_no_mem(current_ir_graph);
	} else {
		store = get_store();
	}

	ir_node *node;
	type_t  *return_type = skip_typeref(function_type->return_type);
	ir_node *result      = NULL;
	if (firm_builtin) {
		node = new_d_Builtin(dbgi, store, n_parameters, in, firm_builtin_kind,
		                     ir_method_type);
		if (!(function_type->modifiers & DM_CONST)) {
			ir_node *mem = new_Proj(node, mode_M, pn_Builtin_M);
			set_store(mem);
		}

		if (!is_type_void(return_type)) {
			assert(is_type_scalar(return_type));
			ir_mode *mode = get_ir_mode_storage(return_type);
			result = new_Proj(node, mode, pn_Builtin_max+1);
		}
	} else {
		node = new_d_Call(dbgi, store, callee, n_parameters, in, ir_method_type);
		if (!(function_type->modifiers & DM_CONST)) {
			ir_node *mem = new_Proj(node, mode_M, pn_Call_M);
			set_store(mem);
		}

		if (!is_type_void(return_type)) {
			ir_node *const resproj = new_Proj(node, mode_T, pn_Call_T_result);
			ir_mode *const mode    = get_ir_mode_storage(return_type);
			result                 = new_Proj(resproj, mode, 0);
		}
	}

	if (function_type->modifiers & DM_NORETURN) {
		/* A dead end:  Keep the Call and the Block.  Also place all further
		 * nodes into a new and unreachable block. */
		keep_alive(node);
		keep_alive(get_cur_block());
		ir_node *block = new_Block(0, NULL);
		set_cur_block(block);
	}

	return result;
}

static ir_node *statement_to_firm(statement_t *statement);
static ir_node *compound_statement_to_firm(compound_statement_t *compound);
static ir_node *expression_to_addr(const expression_t *expression);

static void assign_value(dbg_info *dbgi, ir_node *addr, type_t *type,
                         ir_node *value)
{
	value = conv_to_storage_type(dbgi, value, type);

	ir_node *memory = get_store();
	ir_cons_flags flags = type->base.qualifiers & TYPE_QUALIFIER_VOLATILE
		? cons_volatile : cons_none;

	if (is_type_scalar(type) && !is_type_complex(type)) {
		ir_node  *store     = new_d_Store(dbgi, memory, addr, value, flags);
		ir_node  *store_mem = new_d_Proj(dbgi, store, mode_M, pn_Store_M);
		set_store(store_mem);
	} else {
		ir_type *irtype = get_ir_type(type);
		ir_node *copyb  = new_d_CopyB(dbgi, memory, addr, value, irtype, flags);
		set_store(copyb);
	}
}

static ir_tarval *create_bitfield_mask(ir_mode *mode, int offset, int size)
{
	ir_tarval *all_one   = get_mode_all_one(mode);
	int        mode_size = get_mode_size_bits(mode);
	ir_mode   *mode_uint = atomic_modes[ATOMIC_TYPE_UINT];

	assert(offset >= 0);
	assert(size   >= 0);
	assert(offset + size <= mode_size);
	if (size == mode_size) {
		return all_one;
	}

	long       shiftr    = get_mode_size_bits(mode) - size;
	long       shiftl    = offset;
	ir_tarval *tv_shiftr = new_tarval_from_long(shiftr, mode_uint);
	ir_tarval *tv_shiftl = new_tarval_from_long(shiftl, mode_uint);
	ir_tarval *mask0     = tarval_shr(all_one, tv_shiftr);
	ir_tarval *mask1     = tarval_shl(mask0, tv_shiftl);

	return mask1;
}

static ir_node *bitfield_store_to_firm(dbg_info *dbgi,
		ir_entity *entity, ir_node *addr, ir_node *value, bool set_volatile,
		bool need_return)
{
	ir_type *base_type   = get_entity_type(entity);
	ir_mode *mode        = get_type_mode(base_type);
	ir_mode *mode_uint   = atomic_modes[ATOMIC_TYPE_UINT];

	value = create_conv(dbgi, value, mode);

	/* kill upper bits of value and shift to right position */
	unsigned  bitoffset  = get_entity_bitfield_offset(entity);
	unsigned  bitsize    = get_entity_bitfield_size(entity);
	unsigned  base_bits  = get_mode_size_bits(mode);
	unsigned  shiftwidth = base_bits - bitsize;

	ir_node  *shiftcount = new_Const_long(mode_uint, shiftwidth);
	ir_node  *shiftl     = new_d_Shl(dbgi, value, shiftcount, mode);

	unsigned  shrwidth   = base_bits - bitsize - bitoffset;
	ir_node  *shrconst   = new_Const_long(mode_uint, shrwidth);
	ir_node  *shiftr     = new_d_Shr(dbgi, shiftl, shrconst, mode);

	/* load current value */
	ir_node   *mem             = get_store();
	ir_node   *load            = new_d_Load(dbgi, mem, addr, mode,
	                                  set_volatile ? cons_volatile : cons_none);
	ir_node   *load_mem        = new_d_Proj(dbgi, load, mode_M, pn_Load_M);
	ir_node   *load_res        = new_d_Proj(dbgi, load, mode, pn_Load_res);
	ir_tarval *shift_mask      = create_bitfield_mask(mode, bitoffset, bitsize);
	ir_tarval *inv_mask        = tarval_not(shift_mask);
	ir_node   *inv_mask_node   = new_d_Const(dbgi, inv_mask);
	ir_node   *load_res_masked = new_d_And(dbgi, load_res, inv_mask_node, mode);

	/* construct new value and store */
	ir_node *new_val   = new_d_Or(dbgi, load_res_masked, shiftr, mode);
	ir_node *store     = new_d_Store(dbgi, load_mem, addr, new_val,
	                                 set_volatile ? cons_volatile : cons_none);
	ir_node *store_mem = new_d_Proj(dbgi, store, mode_M, pn_Store_M);
	set_store(store_mem);

	if (!need_return)
		return NULL;

	ir_node *res_shr;
	ir_node *count_res_shr = new_Const_long(mode_uint, base_bits - bitsize);
	if (mode_is_signed(mode)) {
		res_shr = new_d_Shrs(dbgi, shiftl, count_res_shr, mode);
	} else {
		res_shr = new_d_Shr(dbgi, shiftl, count_res_shr, mode);
	}
	return res_shr;
}

static ir_node *bitfield_extract_to_firm(const select_expression_t *expression,
                                         ir_node *addr)
{
	dbg_info *dbgi      = get_dbg_info(&expression->base.pos);
	entity_t *entity    = expression->compound_entry;
	type_t   *base_type = entity->declaration.type;
	ir_mode  *mode      = get_ir_mode_storage(base_type);
	ir_node  *mem       = get_store();
	ir_node  *load      = new_d_Load(dbgi, mem, addr, mode, cons_none);
	ir_node  *load_mem  = new_d_Proj(dbgi, load, mode_M, pn_Load_M);
	ir_node  *load_res  = new_d_Proj(dbgi, load, mode, pn_Load_res);
	ir_mode  *mode_uint = atomic_modes[ATOMIC_TYPE_UINT];

	ir_mode  *amode     = mode;
	/* optimisation, since shifting in modes < machine_size is usually
	 * less efficient */
	if (get_mode_size_bits(amode) < get_mode_size_bits(mode_uint)) {
		amode = mode_uint;
	}
	unsigned amode_size = get_mode_size_bits(amode);
	load_res = create_conv(dbgi, load_res, amode);

	set_store(load_mem);

	/* kill upper bits */
	assert(expression->compound_entry->kind == ENTITY_COMPOUND_MEMBER);
	unsigned   bitoffset   = entity->compound_member.bit_offset;
	unsigned   bitsize     = entity->compound_member.bit_size;
	unsigned   shift_bitsl = amode_size - bitoffset - bitsize;
	ir_tarval *tvl         = new_tarval_from_long((long)shift_bitsl, mode_uint);
	ir_node   *countl      = new_d_Const(dbgi, tvl);
	ir_node   *shiftl      = new_d_Shl(dbgi, load_res, countl, amode);

	unsigned   shift_bitsr = bitoffset + shift_bitsl;
	assert(shift_bitsr <= amode_size);
	ir_tarval *tvr         = new_tarval_from_long((long)shift_bitsr, mode_uint);
	ir_node   *countr      = new_d_Const(dbgi, tvr);
	ir_node   *shiftr;
	if (mode_is_signed(mode)) {
		shiftr = new_d_Shrs(dbgi, shiftl, countr, amode);
	} else {
		shiftr = new_d_Shr(dbgi, shiftl, countr, amode);
	}

	return conv_to_storage_type(dbgi, shiftr, expression->base.type);
}

/* make sure the selected compound type is constructed */
static void construct_select_compound(const select_expression_t *expression)
{
	type_t *type = skip_typeref(expression->compound->base.type);
	if (is_type_pointer(type)) {
		type = type->pointer.points_to;
	}
	(void) get_ir_type(type);
}

static ir_node *set_value_for_expression_addr(const expression_t *expression,
                                              ir_node *value, ir_node *addr)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.pos);
	type_t   *type = skip_typeref(expression->base.type);
	value = conv_to_storage_type(dbgi, value, type);

	if (expression->kind == EXPR_REFERENCE) {
		const reference_expression_t *ref = &expression->reference;

		entity_t *entity = ref->entity;
		assert(is_declaration(entity));
		assert(entity->declaration.kind != DECLARATION_KIND_UNKNOWN);
		if (entity->declaration.kind == DECLARATION_KIND_LOCAL_VARIABLE ||
		    entity->declaration.kind == DECLARATION_KIND_PARAMETER) {
			set_value(entity->variable.v.value_number, value);
			return value;
		}
	}

	if (addr == NULL)
		addr = expression_to_addr(expression);
	assert(addr != NULL);

	if (expression->kind == EXPR_SELECT) {
		const select_expression_t *selecte = &expression->select;

		construct_select_compound(selecte);

		entity_t *entity = selecte->compound_entry;
		assert(entity->kind == ENTITY_COMPOUND_MEMBER);
		if (entity->compound_member.bitfield) {
			ir_entity *irentity = entity->compound_member.entity;
			bool       set_volatile
				= selecte->base.type->base.qualifiers & TYPE_QUALIFIER_VOLATILE;
			value = bitfield_store_to_firm(dbgi, irentity, addr, value,
			                               set_volatile, true);
			return value;
		}
	}

	assign_value(dbgi, addr, type, value);
	return value;
}

static ir_node *get_value_from_lvalue(const expression_t *expression,
                                      ir_node *addr)
{
	if (expression->kind == EXPR_REFERENCE) {
		const reference_expression_t *ref = &expression->reference;

		entity_t *entity = ref->entity;
		assert(entity->kind == ENTITY_VARIABLE
				|| entity->kind == ENTITY_PARAMETER);
		assert(entity->declaration.kind != DECLARATION_KIND_UNKNOWN);
		int value_number;
		if (entity->declaration.kind == DECLARATION_KIND_LOCAL_VARIABLE ||
		    entity->declaration.kind == DECLARATION_KIND_PARAMETER) {
			value_number = entity->variable.v.value_number;
			assert(addr == NULL);
			type_t  *type = skip_typeref(expression->base.type);
			ir_mode *mode = get_ir_mode_storage(type);
			return get_value(value_number, mode);
		}
	}

	assert(addr != NULL);
	dbg_info *dbgi = get_dbg_info(&expression->base.pos);

	ir_node *value;
	if (expression->kind == EXPR_SELECT &&
	    expression->select.compound_entry->compound_member.bitfield) {
	    construct_select_compound(&expression->select);
		value = bitfield_extract_to_firm(&expression->select, addr);
	} else {
		value = deref_address(dbgi, expression->base.type, addr);
	}

	return value;
}

static ir_node *incdec_to_firm(unary_expression_t const *const expr, bool const inc, bool const pre)
{
	type_t  *const type = skip_typeref(expr->base.type);
	ir_mode *const mode = get_ir_mode_arithmetic(type);

	ir_node *offset;
	if (is_type_pointer(type)) {
		offset = get_type_size_node(type->pointer.points_to);
	} else {
		assert(is_type_arithmetic(type));
		offset = new_Const(get_mode_one(mode));
	}

	dbg_info           *const dbgi        = get_dbg_info(&expr->base.pos);
	expression_t const *const value_expr  = expr->value;
	ir_node            *const addr        = expression_to_addr(value_expr);
	ir_node            *const value       = get_value_from_lvalue(value_expr, addr);
	ir_node            *const value_arith = create_conv(dbgi, value, mode);
	ir_node            *const new_value   = inc
		? new_d_Add(dbgi, value_arith, offset, mode)
		: new_d_Sub(dbgi, value_arith, offset, mode);

	ir_node *const store_value = set_value_for_expression_addr(value_expr, new_value, addr);
	return pre ? store_value : value;
}

static bool is_local_variable(expression_t *expression)
{
	if (expression->kind != EXPR_REFERENCE)
		return false;
	reference_expression_t *ref_expr = &expression->reference;
	entity_t               *entity   = ref_expr->entity;
	if (entity->kind != ENTITY_VARIABLE)
		return false;
	assert(entity->declaration.kind != DECLARATION_KIND_UNKNOWN);
	return entity->declaration.kind == DECLARATION_KIND_LOCAL_VARIABLE;
}

/**
 * Handle the assume optimizer hint: check if a Confirm
 * node can be created.
 *
 * @param dbi    debug info
 * @param expr   the IL assume expression
 *
 * we support here only some simple cases:
 *  - var rel const
 *  - const rel val
 *  - var rel var
 */
static ir_node *handle_assume_compare(dbg_info *dbi,
                                      const binary_expression_t *expression)
{
	expression_t *op1 = expression->left;
	expression_t *op2 = expression->right;
	entity_t     *var2, *var = NULL;
	ir_node      *res      = NULL;
	ir_relation   relation = get_relation(expression->base.kind);

	if (is_local_variable(op1) && is_local_variable(op2)) {
		var  = op1->reference.entity;
	    var2 = op2->reference.entity;

		type_t  *const type = skip_typeref(var->declaration.type);
		ir_mode *const mode = get_ir_mode_storage(type);

		ir_node *const irn1 = get_value(var->variable.v.value_number, mode);
		ir_node *const irn2 = get_value(var2->variable.v.value_number, mode);

		res = new_d_Confirm(dbi, irn2, irn1, get_inversed_relation(relation));
		set_value(var2->variable.v.value_number, res);

		res = new_d_Confirm(dbi, irn1, irn2, relation);
		set_value(var->variable.v.value_number, res);

		return res;
	}

	expression_t *con = NULL;
	if (is_local_variable(op1) && is_constant_expression(op2) != EXPR_CLASS_VARIABLE) {
		var = op1->reference.entity;
		con = op2;
	} else if (is_constant_expression(op1) != EXPR_CLASS_VARIABLE && is_local_variable(op2)) {
		relation = get_inversed_relation(relation);
		var = op2->reference.entity;
		con = op1;
	}

	if (var != NULL) {
		type_t  *const type = skip_typeref(var->declaration.type);
		ir_mode *const mode = get_ir_mode_storage(type);

		res = get_value(var->variable.v.value_number, mode);
		res = new_d_Confirm(dbi, res, expression_to_value(con), relation);
		set_value(var->variable.v.value_number, res);
	}
	return res;
}

/**
 * Handle the assume optimizer hint.
 *
 * @param dbi    debug info
 * @param expr   the IL assume expression
 */
static ir_node *handle_assume(expression_t const *const expr)
{
	switch (expr->kind) {
	case EXPR_BINARY_EQUAL:
	case EXPR_BINARY_NOTEQUAL:
	case EXPR_BINARY_LESS:
	case EXPR_BINARY_LESSEQUAL:
	case EXPR_BINARY_GREATER:
	case EXPR_BINARY_GREATEREQUAL: {
		dbg_info *const dbgi = get_dbg_info(&expr->base.pos);
		return handle_assume_compare(dbgi, &expr->binary);
	}

	default:
		return NULL;
	}
}

static ir_node *create_cast(unary_expression_t const *const expr)
{
	type_t  *const from_type = skip_typeref(expr->value->base.type);
	ir_node       *value     = is_type_complex(from_type)
		? expression_to_complex(expr->value).real
		: expression_to_value(expr->value);

	type_t *const type = skip_typeref(expr->base.type);
	if (is_type_void(type))
		return NULL;

	dbg_info *const dbgi = get_dbg_info(&expr->base.pos);
	ir_mode  *const mode = get_ir_mode_storage(type);
	/* check for conversion from / to __based types */
	if (is_type_pointer(type) && is_type_pointer(from_type)) {
		const variable_t *from_var = from_type->pointer.base_variable;
		const variable_t *to_var   = type->pointer.base_variable;
		if (from_var != to_var) {
			if (from_var != NULL) {
				ir_node *const addr = new_d_Address(dbgi, from_var->v.entity);
				ir_node *const base = deref_address(dbgi, from_var->base.type, addr);
				value = new_d_Add(dbgi, value, base, mode);
			}
			if (to_var != NULL) {
				ir_node *const addr = new_d_Address(dbgi, to_var->v.entity);
				ir_node *const base = deref_address(dbgi, to_var->base.type, addr);
				value = new_d_Sub(dbgi, value, base, mode);
			}
		}
	}

	return create_conv(dbgi, value, mode);
}

static ir_node *complement_to_firm(unary_expression_t const *const expr)
{
	dbg_info *const dbgi  = get_dbg_info(&expr->base.pos);
	type_t   *const type  = skip_typeref(expr->base.type);
	ir_mode  *const mode  = get_ir_mode_arithmetic(type);
	ir_node  *const value = create_conv(dbgi, expression_to_value(expr->value), mode);
	return new_d_Not(dbgi, value, mode);
}

static ir_node *dereference_to_firm(unary_expression_t const *const expr)
{
	dbg_info *const dbgi       = get_dbg_info(&expr->base.pos);
	ir_node        *value      = expression_to_value(expr->value);
	type_t   *const value_type = skip_typeref(expr->value->base.type);
	assert(is_type_pointer(value_type));

	/* check for __based */
	variable_t const *const base_var = value_type->pointer.base_variable;
	if (base_var) {
		ir_node *const addr = new_d_Address(dbgi, base_var->v.entity);
		ir_node *const base = deref_address(dbgi, base_var->base.type, addr);
		value = new_d_Add(dbgi, value, base, get_ir_mode_storage(value_type));
	}
	type_t *const points_to = value_type->pointer.points_to;
	return deref_address(dbgi, points_to, value);
}

static ir_node *negate_to_firm(unary_expression_t const *const expr)
{
	dbg_info *const dbgi  = get_dbg_info(&expr->base.pos);
	type_t   *const type  = skip_typeref(expr->base.type);
	ir_mode  *const mode  = get_ir_mode_arithmetic(type);
	ir_node  *const value = create_conv(dbgi, expression_to_value(expr->value), mode);
	return new_d_Minus(dbgi, value, mode);
}

static ir_node *adjust_for_pointer_arithmetic(dbg_info *dbgi,
		ir_node *value, type_t *type)
{
	ir_mode        *const mode         = get_ir_mode_storage(type_ptrdiff_t);
	assert(is_type_pointer(type));
	pointer_type_t *const pointer_type = &type->pointer;
	type_t         *const points_to    = skip_typeref(pointer_type->points_to);
	ir_node        *      elem_size    = get_type_size_node(points_to);
	elem_size                          = create_conv(dbgi, elem_size, mode);
	value                              = create_conv(dbgi, value,     mode);
	ir_node        *const mul          = new_d_Mul(dbgi, value, elem_size, mode);
	return mul;
}

static ir_node *create_div(dbg_info *dbgi, ir_node *left, ir_node *right,
                           ir_mode *mode)
{
	ir_node *pin = new_Pin(new_NoMem());
	ir_node *op  = new_d_Div(dbgi, pin, left, right, mode,
	                         op_pin_state_floats);
	return new_d_Proj(dbgi, op, mode, pn_Div_res);
}

static ir_node *create_op(binary_expression_t const *const expr, ir_node *left, ir_node *right)
{
	ir_mode                *mode;
	dbg_info         *const dbgi       = get_dbg_info(&expr->base.pos);
	type_t           *const type_left  = skip_typeref(expr->left->base.type);
	type_t           *const type_right = skip_typeref(expr->right->base.type);
	expression_kind_t const kind       = expr->base.kind;
	switch (kind) {
	case EXPR_BINARY_SHIFTLEFT:
	case EXPR_BINARY_SHIFTRIGHT:
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:
		mode  = get_ir_mode_arithmetic(expr->base.type);
		left  = create_conv(dbgi, left,  mode);
		right = create_conv(dbgi, right, atomic_modes[ATOMIC_TYPE_UINT]);
		break;

	case EXPR_BINARY_SUB:
		if (is_type_pointer(type_left) && is_type_pointer(type_right)) {
			const pointer_type_t *const ptr_type = &type_left->pointer;

			mode = get_ir_mode_storage(expr->base.type);
			ir_node *const elem_size = get_type_size_node(ptr_type->points_to);
			ir_node *const conv_size = new_d_Conv(dbgi, elem_size, mode);
			ir_node *const sub       = new_d_Sub(dbgi, left, right, mode);
			ir_node *const no_mem    = new_NoMem();
			ir_node *const divn      = new_d_DivRL(dbgi, no_mem, sub, conv_size,
												   mode, op_pin_state_floats);
			return new_d_Proj(dbgi, divn, mode, pn_Div_res);
		}
		/* fallthrough */
	case EXPR_BINARY_SUB_ASSIGN:
		if (is_type_pointer(type_left)) {
			right = adjust_for_pointer_arithmetic(dbgi, right, type_left);
			mode  = get_ir_mode_storage(type_left);
			break;
		}
		goto normal_node;

	case EXPR_BINARY_ADD:
	case EXPR_BINARY_ADD_ASSIGN:
		if (is_type_pointer(type_left)) {
			right = adjust_for_pointer_arithmetic(dbgi, right, type_left);
			mode  = get_ir_mode_storage(type_left);
			break;
		} else if (is_type_pointer(type_right)) {
			left  = adjust_for_pointer_arithmetic(dbgi, left, type_right);
			mode  = get_ir_mode_storage(type_right);
			break;
		}
		goto normal_node;

	default:
normal_node:
		mode  = get_ir_mode_arithmetic(type_right);
		left  = create_conv(dbgi, left,  mode);
		right = create_conv(dbgi, right, mode);
		break;
	}

	switch (kind) {
	case EXPR_BINARY_ADD_ASSIGN:
	case EXPR_BINARY_ADD:
		return new_d_Add(dbgi, left, right, mode);
	case EXPR_BINARY_SUB_ASSIGN:
	case EXPR_BINARY_SUB:
		return new_d_Sub(dbgi, left, right, mode);
	case EXPR_BINARY_MUL_ASSIGN:
	case EXPR_BINARY_MUL:
		return new_d_Mul(dbgi, left, right, mode);
	case EXPR_BINARY_DIV:
	case EXPR_BINARY_DIV_ASSIGN:
		return create_div(dbgi, left, right, mode);
	case EXPR_BINARY_BITWISE_AND:
	case EXPR_BINARY_BITWISE_AND_ASSIGN:
		return new_d_And(dbgi, left, right, mode);
	case EXPR_BINARY_BITWISE_OR:
	case EXPR_BINARY_BITWISE_OR_ASSIGN:
		return new_d_Or(dbgi, left, right, mode);
	case EXPR_BINARY_BITWISE_XOR:
	case EXPR_BINARY_BITWISE_XOR_ASSIGN:
		return new_d_Eor(dbgi, left, right, mode);
	case EXPR_BINARY_SHIFTLEFT:
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:
		return new_d_Shl(dbgi, left, right, mode);
	case EXPR_BINARY_SHIFTRIGHT:
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:
		if (mode_is_signed(mode)) {
			return new_d_Shrs(dbgi, left, right, mode);
		} else {
			return new_d_Shr(dbgi, left, right, mode);
		}
	case EXPR_BINARY_MOD:
	case EXPR_BINARY_MOD_ASSIGN: {
		ir_node *pin = new_Pin(new_NoMem());
		ir_node *op  = new_d_Mod(dbgi, pin, left, right, mode,
		                         op_pin_state_floats);
		ir_node *res = new_d_Proj(dbgi, op, mode, pn_Mod_res);
		return res;
	}
	default:
		panic("unexpected expression kind");
	}
}

static ir_node *binop_to_firm(binary_expression_t const *const expr)
{
	ir_node *const left  = expression_to_value(expr->left);
	ir_node *const right = expression_to_value(expr->right);
	return create_op(expr, left, right);
}

/**
 * Check if a given expression is a GNU __builtin_expect() call.
 */
static bool is_builtin_expect(const expression_t *expression)
{
	if (expression->kind != EXPR_CALL)
		return false;

	expression_t *function = expression->call.function;
	if (function->kind != EXPR_REFERENCE)
		return false;
	reference_expression_t *ref = &function->reference;
	if (ref->entity->kind         != ENTITY_FUNCTION ||
	    ref->entity->function.btk != BUILTIN_EXPECT)
		return false;

	return true;
}

static void compare_to_control_flow(expression_t const *const expr, ir_node *const left, ir_node *const right, ir_relation const relation, jump_target *const true_target, jump_target *const false_target)
{
	dbg_info *const dbgi = get_dbg_info(&expr->base.pos);
	ir_node  *const cmp  = new_d_Cmp(dbgi, left, right, relation);
	if (is_Const(cmp)) {
		if (tarval_is_null(get_Const_tarval(cmp))) {
			jump_to_target(false_target);
		} else {
			jump_to_target(true_target);
		}
	} else {
		ir_node *const cond       = new_d_Cond(dbgi, cmp);
		ir_node *const true_proj  = new_d_Proj(dbgi, cond, mode_X, pn_Cond_true);
		ir_node *const false_proj = new_d_Proj(dbgi, cond, mode_X, pn_Cond_false);

		/* set branch prediction info based on __builtin_expect */
		if (is_builtin_expect(expr) && is_Cond(cond)) {
			call_argument_t *const argument = expr->call.arguments->next;
			if (is_constant_expression(argument->expression) != EXPR_CLASS_VARIABLE) {
				bool               const cnst = fold_expression_to_bool(argument->expression);
				cond_jmp_predicate const pred = cnst ? COND_JMP_PRED_TRUE : COND_JMP_PRED_FALSE;
				set_Cond_jmp_pred(cond, pred);
			}
		}

		add_pred_to_jump_target(true_target,  true_proj);
		add_pred_to_jump_target(false_target, false_proj);
	}
	set_unreachable_now();
}

static ir_node *control_flow_to_1_0(expression_t const *const expr, jump_target *const true_target, jump_target *const false_target)
{
	ir_node        *val  = NULL;
	dbg_info *const dbgi = get_dbg_info(&expr->base.pos);
	ir_mode  *const mode = get_ir_mode_storage(expr->base.type);
	jump_target     exit_target;
	init_jump_target(&exit_target, NULL);

	if (enter_jump_target(true_target)) {
		jump_to_target(&exit_target);
		val = new_d_Const(dbgi, get_mode_one(mode));
	}

	if (enter_jump_target(false_target)) {
		jump_to_target(&exit_target);
		ir_node *const zero = new_d_Const(dbgi, get_mode_null(mode));
		if (val) {
			ir_node *const in[] = { val, zero };
			val = new_rd_Phi(dbgi, exit_target.block, ARRAY_SIZE(in), in, mode);
		} else {
			val = zero;
		}
	}

	if (!enter_jump_target(&exit_target)) {
		set_cur_block(new_Block(0, NULL));
		val = new_d_Bad(dbgi, mode);
	}
	return val;
}

static ir_node *binop_assign_to_firm(binary_expression_t const *const expr)
{
	ir_node            *const right     = expression_to_value(expr->right);
	expression_t const *const left_expr = expr->left;
	ir_node            *const addr      = expression_to_addr(left_expr);
	ir_node            *const left      = get_value_from_lvalue(left_expr, addr);
	ir_node                  *result    = create_op(expr, left, right);

	type_t *const type = skip_typeref(expr->base.type);
	if (is_type_atomic(type, ATOMIC_TYPE_BOOL)) {
		jump_target true_target;
		jump_target false_target;
		init_jump_target(&true_target,  NULL);
		init_jump_target(&false_target, NULL);
		ir_mode *const mode = get_irn_mode(result);
		ir_node *const zero = new_Const(get_mode_null(mode));
		compare_to_control_flow((expression_t const*)expr, result, zero, ir_relation_unordered_less_greater, &true_target, &false_target);
		result = control_flow_to_1_0((expression_t const*)expr, &true_target, &false_target);
	}

	return set_value_for_expression_addr(left_expr, result, addr);
}

static ir_node *assign_expression_to_firm(binary_expression_t const *const expr)
{
	ir_node *const addr  = expression_to_addr(expr->left);
	ir_node *const right = expression_to_value(expr->right);
	return set_value_for_expression_addr(expr->left, right, addr);
}

/** evaluate an expression and discard the result, but still produce the
 * side-effects. */
static void evaluate_expression_discard_result(const expression_t *expression)
{
	type_t *type = skip_typeref(expression->base.type);
	if (is_type_complex(type)) {
		expression_to_complex(expression);
	} else {
		expression_to_value(expression);
	}
}

static ir_node *comma_expression_to_firm(binary_expression_t const *const expr)
{
	evaluate_expression_discard_result(expr->left);
	return expression_to_value(expr->right);
}

static ir_node *array_access_addr(const array_access_expression_t *expression)
{
	dbg_info *dbgi        = get_dbg_info(&expression->base.pos);
	ir_node  *base_addr   = expression_to_value(expression->array_ref);
	ir_node  *offset      = expression_to_value(expression->index);
	type_t   *ref_type    = skip_typeref(expression->array_ref->base.type);
	ir_node  *real_offset = adjust_for_pointer_arithmetic(dbgi, offset, ref_type);
	ir_node  *result      = new_d_Add(dbgi, base_addr, real_offset, mode_P_data);

	return result;
}

static ir_node *array_access_to_firm(
		const array_access_expression_t *expression)
{
	dbg_info *dbgi   = get_dbg_info(&expression->base.pos);
	ir_node  *addr   = array_access_addr(expression);
	type_t   *type   = revert_automatic_type_conversion(
			(const expression_t*) expression);
	type             = skip_typeref(type);

	return deref_address(dbgi, type, addr);
}

static void create_local_initializer(initializer_t *initializer, dbg_info *dbgi,
                                     ir_entity *entity, type_t *type);
static ir_initializer_t *create_ir_initializer(
		const initializer_t *initializer, type_t *type);

static ir_entity *create_initializer_entity(dbg_info *dbgi,
                                            initializer_t *initializer,
                                            type_t *type)
{
	/* create the ir_initializer */
	PUSH_IRG(get_const_code_irg());
	ir_initializer_t *irinitializer = create_ir_initializer(initializer, type);
	POP_IRG();

	ident     *const id          = id_unique("initializer.%u");
	ir_type   *const irtype      = get_ir_type(type);
	ir_type   *const global_type = get_glob_type();
	ir_entity *const entity      = new_entity(global_type, id, irtype);
	set_entity_dbg_info(entity, dbgi);
	set_entity_ld_ident(entity, id);
	set_entity_visibility(entity, ir_visibility_private);
	add_entity_linkage(entity, IR_LINKAGE_CONSTANT);
	set_entity_initializer(entity, irinitializer);
	return entity;
}

static ir_node *compound_literal_addr(compound_literal_expression_t const *const expression)
{
	dbg_info      *dbgi        = get_dbg_info(&expression->base.pos);
	type_t        *type        = expression->type;
	type_t        *skipped     = skip_typeref(type);
	initializer_t *initializer = expression->initializer;

	if (expression->global_scope || (
	      skipped->base.qualifiers & TYPE_QUALIFIER_CONST &&
	      is_constant_initializer(initializer) != EXPR_CLASS_VARIABLE
	    )) {
		ir_entity *entity = create_initializer_entity(dbgi, initializer, type);
		return new_d_Address(dbgi, entity);
	} else {
		/* create an entity on the stack */
		ident   *const id     = id_unique("CompLit.%u");
		ir_type *const irtype = get_ir_type(type);
		ir_type *frame_type   = get_irg_frame_type(current_ir_graph);

		ir_entity *const entity = new_entity(frame_type, id, irtype);
		set_entity_dbg_info(entity, dbgi);
		set_entity_ld_ident(entity, id);

		/* create initialisation code */
		create_local_initializer(initializer, dbgi, entity, type);

		/* create a sel for the compound literal address */
		ir_node *frame = get_irg_frame(current_ir_graph);
		ir_node *sel   = new_d_simpleSel(dbgi, new_NoMem(), frame, entity);
		return sel;
	}
}

static ir_node *compound_literal_to_firm(
		compound_literal_expression_t const* const expr)
{
	dbg_info *const dbgi = get_dbg_info(&expr->base.pos);
	type_t   *const type = skip_typeref(expr->type);
	if (is_type_array(type) || is_type_compound(type)) {
		ir_node  *const addr = compound_literal_addr(expr);
		return deref_address(dbgi, type, addr);
	} else {
		assert(is_type_scalar(type));
		const initializer_t *initializer = expr->initializer;
		switch (initializer->kind) {
		case INITIALIZER_VALUE:
			return expression_to_value(initializer->value.value);
		case INITIALIZER_STRING:
			return string_to_firm(&expr->base.pos, "str.%u",
			                      &(get_init_string(initializer)->value));
		case INITIALIZER_LIST:
		case INITIALIZER_DESIGNATOR: {
			/* shouldn't really happen in valid programs, return 0 for
			 * invalid ones... */
			ir_mode *mode = get_ir_mode_arithmetic(type);
			return new_d_Const(dbgi, get_mode_null(mode));
		}
		}
		panic("invalid initializer");
	}
}

/**
 * Transform a sizeof expression into Firm code.
 */
static ir_node *sizeof_to_firm(const typeprop_expression_t *expression)
{
	type_t *const type = skip_typeref(expression->type);
	/* §6.5.3.4:2 if the type is a VLA, evaluate the expression. */
	if (is_type_array(type) && type->array.is_vla
			&& expression->tp_expression != NULL) {
		expression_to_value(expression->tp_expression);
	}

	return get_type_size_node(type);
}

static ir_node *conditional_to_firm(const conditional_expression_t *expression)
{
	jump_target true_target;
	jump_target false_target;
	init_jump_target(&true_target,  NULL);
	init_jump_target(&false_target, NULL);
	ir_node *const cond_expr = expression_to_control_flow(expression->condition, &true_target, &false_target);

	ir_node        *val  = NULL;
	dbg_info *const dbgi = get_dbg_info(&expression->base.pos);
	type_t   *const type = skip_typeref(expression->base.type);
	ir_mode  *const mode = is_type_void(type) ? NULL : get_ir_mode_arithmetic(type);
	jump_target exit_target;
	init_jump_target(&exit_target, NULL);

	if (enter_jump_target(&true_target)) {
		if (expression->true_expression) {
			val = expression_to_value(expression->true_expression);
		} else if (cond_expr) {
			val = cond_expr;
		} else {
			/* Condition ended with a short circuit (&&, ||, !) operation or a
			 * comparison.  Generate a "1" as value for the true branch. */
			val = new_Const(get_mode_one(mode));
		}
		if (val)
			val = create_conv(dbgi, val, mode);
		jump_to_target(&exit_target);
	}

	if (enter_jump_target(&false_target)) {
		ir_node *false_val = expression_to_value(expression->false_expression);
		if (false_val)
			false_val = create_conv(dbgi, false_val, mode);
		jump_to_target(&exit_target);
		if (val) {
			ir_node *const in[] = { val, false_val };
			val = new_rd_Phi(dbgi, exit_target.block, ARRAY_SIZE(in), in, get_irn_mode(val));
		} else {
			val = false_val;
		}
	}

	if (!enter_jump_target(&exit_target)) {
		set_cur_block(new_Block(0, NULL));
		if (!is_type_void(type))
			val = new_Bad(mode);
	}
	return val;
}

/**
 * Returns an IR-node representing the address of a field.
 */
static ir_node *select_addr(const select_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.pos);

	construct_select_compound(expression);

	ir_node *compound_addr = expression_to_value(expression->compound);

	entity_t *entry = expression->compound_entry;
	assert(entry->kind == ENTITY_COMPOUND_MEMBER);
	assert(entry->declaration.kind == DECLARATION_KIND_COMPOUND_MEMBER);

	ir_entity *irentity = entry->compound_member.entity;
	assert(irentity != NULL);
	return new_d_simpleSel(dbgi, new_NoMem(), compound_addr, irentity);
}

static ir_node *select_to_firm(const select_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.pos);
	ir_node  *addr = select_addr(expression);
	type_t   *type = revert_automatic_type_conversion(
			(const expression_t*) expression);
	type           = skip_typeref(type);

	entity_t *entry = expression->compound_entry;
	assert(entry->kind == ENTITY_COMPOUND_MEMBER);

	if (entry->compound_member.bitfield) {
		return bitfield_extract_to_firm(expression, addr);
	}

	return deref_address(dbgi, type, addr);
}

static ir_node *function_name_to_firm(
		const funcname_expression_t *const expr)
{
	switch (expr->kind) {
	case FUNCNAME_FUNCTION:
	case FUNCNAME_PRETTY_FUNCTION:
	case FUNCNAME_FUNCDNAME:
		if (current_function_name == NULL) {
			position_t const *const src_pos = &expr->base.pos;
			char       const *const name    = current_function_entity->base.symbol->string;
			string_t          const string  = { name, strlen(name), STRING_ENCODING_CHAR };
			current_function_name = string_to_firm(src_pos, "__func__.%u", &string);
		}
		return current_function_name;
	case FUNCNAME_FUNCSIG:
		if (current_funcsig == NULL) {
			position_t const *const src_pos = &expr->base.pos;
			ir_entity        *const ent     = get_irg_entity(current_ir_graph);
			char       const *const name    = get_entity_ld_name(ent);
			string_t          const string  = { name, strlen(name), STRING_ENCODING_CHAR };
			current_funcsig = string_to_firm(src_pos, "__FUNCSIG__.%u", &string);
		}
		return current_funcsig;
	}
	panic("Unsupported function name");
}

static ir_node *statement_expression_to_firm(const statement_expression_t *expr)
{
	statement_t *statement = expr->statement;

	assert(statement->kind == STATEMENT_COMPOUND);
	return compound_statement_to_firm(&statement->compound);
}

static ir_node *va_start_expression_to_firm(
	const va_start_expression_t *const expr)
{
	ir_entity *param_ent = current_vararg_entity;
	if (param_ent == NULL) {
		size_t   const n           = IR_VA_START_PARAMETER_NUMBER;
		ir_type *const frame_type  = get_irg_frame_type(current_ir_graph);
		ir_type *const param_type  = get_unknown_type();
		param_ent = new_parameter_entity(frame_type, n, param_type);
		current_vararg_entity = param_ent;
	}

	ir_node  *const frame   = get_irg_frame(current_ir_graph);
	dbg_info *const dbgi    = get_dbg_info(&expr->base.pos);
	ir_node  *const no_mem  = new_NoMem();
	ir_node  *const arg_sel = new_d_simpleSel(dbgi, no_mem, frame, param_ent);

	set_value_for_expression_addr(expr->ap, arg_sel, NULL);

	return NULL;
}

static ir_node *va_arg_expression_to_firm(const va_arg_expression_t *const expr)
{
	const backend_params *be_params = be_get_backend_param();
	unsigned stack_param_align = be_params->stack_param_align;

	type_t       *const type    = expr->base.type;
	expression_t *const ap_expr = expr->ap;
	ir_node      *const ap_addr = expression_to_addr(ap_expr);
	ir_node      *const ap      = get_value_from_lvalue(ap_expr, ap_addr);
	dbg_info     *const dbgi    = get_dbg_info(&expr->base.pos);
	ir_node      *const res     = deref_address(dbgi, type, ap);

	ir_node      *const cnst    = get_type_size_node(expr->base.type);
	ir_mode      *const mode    = get_irn_mode(cnst);
	ir_node      *const c1      = new_Const_long(mode, stack_param_align - 1);
	ir_node      *const c2      = new_d_Add(dbgi, cnst, c1, mode);
	ir_node      *const c3      = new_Const_long(mode, -(long)stack_param_align);
	ir_node      *const c4      = new_d_And(dbgi, c2, c3, mode);
	ir_node      *const add     = new_d_Add(dbgi, ap, c4, mode_P_data);

	set_value_for_expression_addr(ap_expr, add, ap_addr);

	return res;
}

/**
 * Generate Firm for a va_copy expression.
 */
static ir_node *va_copy_expression_to_firm(const va_copy_expression_t *const expr)
{
	ir_node *const src = expression_to_value(expr->src);
	set_value_for_expression_addr(expr->dst, src, NULL);
	return NULL;
}

static ir_node *dereference_addr(const unary_expression_t *const expression)
{
	assert(expression->base.kind == EXPR_UNARY_DEREFERENCE);
	return expression_to_value(expression->value);
}

/**
 * Returns a IR-node representing an lvalue of the given expression.
 */
static ir_node *expression_to_addr(const expression_t *expression)
{
	switch (expression->kind) {
	case EXPR_ARRAY_ACCESS:
		return array_access_addr(&expression->array_access);
	case EXPR_COMPOUND_LITERAL:
		return compound_literal_addr(&expression->compound_literal);
	case EXPR_REFERENCE:
		return reference_addr(&expression->reference);
	case EXPR_SELECT:
		return select_addr(&expression->select);
	case EXPR_UNARY_DEREFERENCE:
		return dereference_addr(&expression->unary);
	default:
		break;
	}
	panic("trying to get address of non-lvalue");
}

static void prepare_label_target(label_t *const label)
{
	if (label->address_taken && !label->indirect_block) {
		ir_node *const iblock = new_immBlock();
		label->indirect_block = iblock;
		ARR_APP1(ir_node*, ijmp_blocks, iblock);
		jump_from_block_to_target(&label->target, iblock);
	}
}

/**
 * Pointer to a label.  This is used for the
 * GNU address-of-label extension.
 */
static ir_node *label_address_to_firm(const label_address_expression_t *label)
{
	/* Beware: Might be called from create initializer with current_ir_graph
	 * set to const_code_irg. */
	PUSH_IRG(current_function);
	prepare_label_target(label->label);
	POP_IRG();

	dbg_info  *const dbgi   = get_dbg_info(&label->base.pos);
	ir_entity *const entity = create_Block_entity(label->label->indirect_block);
	return new_d_Address(dbgi, entity);
}

static ir_node *expression_to_value(expression_t const *const expr)
{
#ifndef NDEBUG
	assert(!expr->base.transformed);
	((expression_t*)expr)->base.transformed = true;
	assert(!is_type_complex(skip_typeref(expr->base.type)));
#endif

	switch (expr->kind) {
	case EXPR_UNARY_CAST:
		if (!is_type_atomic(skip_typeref(expr->base.type), ATOMIC_TYPE_BOOL))
			return create_cast(&expr->unary);
		/* FALLTHROUGH */
	case EXPR_BINARY_EQUAL:
	case EXPR_BINARY_GREATER:
	case EXPR_BINARY_GREATEREQUAL:
	case EXPR_BINARY_ISGREATER:
	case EXPR_BINARY_ISGREATEREQUAL:
	case EXPR_BINARY_ISLESS:
	case EXPR_BINARY_ISLESSEQUAL:
	case EXPR_BINARY_ISLESSGREATER:
	case EXPR_BINARY_ISUNORDERED:
	case EXPR_BINARY_LESS:
	case EXPR_BINARY_LESSEQUAL:
	case EXPR_BINARY_LOGICAL_AND:
	case EXPR_BINARY_LOGICAL_OR:
	case EXPR_BINARY_NOTEQUAL:
	case EXPR_UNARY_NOT: {
		jump_target true_target;
		jump_target false_target;
		init_jump_target(&true_target,  NULL);
		init_jump_target(&false_target, NULL);
		expression_to_control_flow(expr, &true_target, &false_target);
		return control_flow_to_1_0(expr, &true_target, &false_target);
	}

	case EXPR_BINARY_ADD:
	case EXPR_BINARY_BITWISE_AND:
	case EXPR_BINARY_BITWISE_OR:
	case EXPR_BINARY_BITWISE_XOR:
	case EXPR_BINARY_DIV:
	case EXPR_BINARY_MOD:
	case EXPR_BINARY_MUL:
	case EXPR_BINARY_SHIFTLEFT:
	case EXPR_BINARY_SHIFTRIGHT:
	case EXPR_BINARY_SUB:
		return binop_to_firm(&expr->binary);

	case EXPR_BINARY_ADD_ASSIGN:
	case EXPR_BINARY_BITWISE_AND_ASSIGN:
	case EXPR_BINARY_BITWISE_OR_ASSIGN:
	case EXPR_BINARY_BITWISE_XOR_ASSIGN:
	case EXPR_BINARY_DIV_ASSIGN:
	case EXPR_BINARY_MOD_ASSIGN:
	case EXPR_BINARY_MUL_ASSIGN:
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:
	case EXPR_BINARY_SUB_ASSIGN:
		return binop_assign_to_firm(&expr->binary);

	{
		bool inc;
		bool pre;
	case EXPR_UNARY_POSTFIX_DECREMENT: inc = false; pre = false; goto incdec;
	case EXPR_UNARY_POSTFIX_INCREMENT: inc = true;  pre = false; goto incdec;
	case EXPR_UNARY_PREFIX_DECREMENT:  inc = false; pre = true;  goto incdec;
	case EXPR_UNARY_PREFIX_INCREMENT:  inc = true;  pre = true;  goto incdec;
incdec:
		return incdec_to_firm(&expr->unary, inc, pre);
	}

	case EXPR_UNARY_IMAG: {
		complex_value irvalue = expression_to_complex(expr->unary.value);
		return irvalue.imag;
	}
	case EXPR_UNARY_REAL: {
		complex_value irvalue = expression_to_complex(expr->unary.value);
		return irvalue.real;
	}

	case EXPR_ARRAY_ACCESS:               return array_access_to_firm(            &expr->array_access);
	case EXPR_BINARY_ASSIGN:              return assign_expression_to_firm(       &expr->binary);
	case EXPR_BINARY_COMMA:               return comma_expression_to_firm(        &expr->binary);
	case EXPR_CALL:                       return call_expression_to_firm(         &expr->call);
	case EXPR_COMPOUND_LITERAL:           return compound_literal_to_firm(        &expr->compound_literal);
	case EXPR_CONDITIONAL:                return conditional_to_firm(             &expr->conditional);
	case EXPR_FUNCNAME:                   return function_name_to_firm(           &expr->funcname);
	case EXPR_LABEL_ADDRESS:              return label_address_to_firm(           &expr->label_address);
	case EXPR_REFERENCE:                  return reference_expression_to_firm(    &expr->reference);
	case EXPR_SELECT:                     return select_to_firm(                  &expr->select);
	case EXPR_SIZEOF:                     return sizeof_to_firm(                  &expr->typeprop);
	case EXPR_STATEMENT:                  return statement_expression_to_firm(    &expr->statement);
	case EXPR_STRING_LITERAL:             return string_to_firm(                  &expr->base.pos, "str.%u", &expr->string_literal.value);
	case EXPR_UNARY_ASSUME:               return handle_assume(                    expr->unary.value);
	case EXPR_UNARY_COMPLEMENT:           return complement_to_firm(              &expr->unary);
	case EXPR_UNARY_DEREFERENCE:          return dereference_to_firm(             &expr->unary);
	case EXPR_UNARY_NEGATE:               return negate_to_firm(                  &expr->unary);
	case EXPR_UNARY_PLUS:                 return expression_to_value(              expr->unary.value);
	case EXPR_UNARY_TAKE_ADDRESS:         return expression_to_addr(               expr->unary.value);
	case EXPR_VA_ARG:                     return va_arg_expression_to_firm(       &expr->va_arge);
	case EXPR_VA_COPY:                    return va_copy_expression_to_firm(      &expr->va_copye);
	case EXPR_VA_START:                   return va_start_expression_to_firm(     &expr->va_starte);

	case EXPR_UNARY_DELETE:
	case EXPR_UNARY_DELETE_ARRAY:
	case EXPR_UNARY_THROW:
		panic("expression not implemented");

	case EXPR_ALIGNOF:
	case EXPR_BUILTIN_CONSTANT_P:
	case EXPR_BUILTIN_TYPES_COMPATIBLE_P:
	case EXPR_CLASSIFY_TYPE:
	case EXPR_ENUM_CONSTANT:
	case EXPR_LITERAL_CASES:
	case EXPR_LITERAL_CHARACTER:
	case EXPR_OFFSETOF: {
		dbg_info  *const dbgi = get_dbg_info(&expr->base.pos);
		ir_tarval *const tv   = fold_expression(expr);
		return new_d_Const(dbgi, tv);
	}

	case EXPR_ERROR: break;
	}
	panic("invalid expression");
}

static void complex_equality_evaluation(const binary_expression_t *binexpr,
	jump_target *const true_target, jump_target *const false_target,
	ir_relation relation);

static complex_value complex_to_control_flow(const expression_t *expression,
                                             jump_target *true_target,
                                             jump_target *false_target);

/**
 * create a short-circuit expression evaluation that tries to construct
 * efficient control flow structures for &&, || and ! expressions
 */
static ir_node *expression_to_control_flow(expression_t const *const expr, jump_target *const true_target, jump_target *const false_target)
{
	switch (expr->kind) {
	case EXPR_UNARY_NOT:
		expression_to_control_flow(expr->unary.value, false_target, true_target);
		return NULL;

	case EXPR_BINARY_LOGICAL_AND: {
		jump_target extra_target;
		init_jump_target(&extra_target, NULL);
		expression_to_control_flow(expr->binary.left, &extra_target, false_target);
		if (enter_jump_target(&extra_target))
			expression_to_control_flow(expr->binary.right, true_target, false_target);
		return NULL;
	}

	case EXPR_BINARY_LOGICAL_OR: {
		jump_target extra_target;
		init_jump_target(&extra_target, NULL);
		expression_to_control_flow(expr->binary.left, true_target, &extra_target);
		if (enter_jump_target(&extra_target))
			expression_to_control_flow(expr->binary.right, true_target, false_target);
		return NULL;
	}

	case EXPR_BINARY_COMMA:
		evaluate_expression_discard_result(expr->binary.left);
		return expression_to_control_flow(expr->binary.right, true_target, false_target);

	case EXPR_BINARY_EQUAL:
	case EXPR_BINARY_GREATER:
	case EXPR_BINARY_GREATEREQUAL:
	case EXPR_BINARY_ISGREATER:
	case EXPR_BINARY_ISGREATEREQUAL:
	case EXPR_BINARY_ISLESS:
	case EXPR_BINARY_ISLESSEQUAL:
	case EXPR_BINARY_ISLESSGREATER:
	case EXPR_BINARY_ISUNORDERED:
	case EXPR_BINARY_LESS:
	case EXPR_BINARY_LESSEQUAL:
	case EXPR_BINARY_NOTEQUAL: {
		type_t     *const type     = skip_typeref(expr->binary.left->base.type);
		ir_relation const relation = get_relation(expr->kind);
		if (is_type_complex(type)) {
			complex_equality_evaluation(&expr->binary, true_target,
			                            false_target, relation);
			return NULL;
		}

		dbg_info *const dbgi  = get_dbg_info(&expr->base.pos);
		ir_mode  *const mode  = get_ir_mode_arithmetic(type);
		ir_node  *const left  = create_conv(dbgi, expression_to_value(expr->binary.left),  mode);
		ir_node  *const right = create_conv(dbgi, expression_to_value(expr->binary.right), mode);
		compare_to_control_flow(expr, left, right, relation, true_target, false_target);
		return NULL;
	}

	case EXPR_UNARY_CAST:
		if (is_type_atomic(skip_typeref(expr->base.type), ATOMIC_TYPE_BOOL)) {
			expression_to_control_flow(expr->unary.value, true_target, false_target);
			return NULL;
		}
		/* FALLTHROUGH */
	default: {
		type_t *const type = skip_typeref(expr->base.type);
		if (is_type_complex(type)) {
			complex_to_control_flow(expr, true_target, false_target);
			return NULL;
		}

		dbg_info   *const dbgi  = get_dbg_info(&expr->base.pos);
		ir_mode    *const mode  = get_ir_mode_arithmetic(type);
		ir_node    *const val   = create_conv(dbgi, expression_to_value(expr), mode);
		ir_node    *const left  = val;
		ir_node    *const right = new_Const(get_mode_null(get_irn_mode(val)));
		ir_relation const relation = ir_relation_unordered_less_greater;
		compare_to_control_flow(expr, left, right, relation, true_target, false_target);
		return val;
	}
	}
}

static complex_value complex_conv(dbg_info *dbgi, complex_value value,
                                  ir_mode *mode)
{
	return (complex_value) {
		create_conv(dbgi, value.real, mode),
		create_conv(dbgi, value.imag, mode)
	};
}

static complex_value complex_conv_to_storage(dbg_info *const dbgi,
	complex_value const value, type_t *const type)
{
	ir_mode *const mode = get_complex_mode_storage(type);
	return complex_conv(dbgi, value, mode);
}

static void store_complex(dbg_info *dbgi, ir_node *addr, type_t *type,
                          complex_value value)
{
	value = complex_conv_to_storage(dbgi, value, type);
	ir_graph  *const irg    = current_ir_graph;
	ir_type   *const irtype = get_ir_type(type);
	ir_node   *const mem    = get_store();
	ir_node   *const nomem  = get_irg_no_mem(irg);
	ir_mode   *const mode   = get_complex_mode_storage(type);
	ir_node   *const real   = create_conv(dbgi, value.real, mode);
	ir_node   *const imag   = create_conv(dbgi, value.imag, mode);
	ir_node   *const storer = new_d_Store(dbgi, mem, addr, real, cons_floats);
	ir_node   *const memr   = new_Proj(storer, mode_M, pn_Store_M);
	ir_mode   *const muint  = atomic_modes[ATOMIC_TYPE_UINT];
	ir_node   *const one    = new_Const(get_mode_one(muint));
	ir_node   *const in[1]  = { one };
	ir_entity *const arrent = get_array_element_entity(irtype);
	ir_node   *const addri  = new_d_Sel(dbgi, nomem, addr, 1, in, arrent);
	ir_node   *const storei = new_d_Store(dbgi, memr, addri, imag, cons_floats);
	ir_node   *const memi   = new_Proj(storei, mode_M, pn_Store_M);
	set_store(memi);
}

static ir_node *complex_to_memory(dbg_info *dbgi, type_t *type,
                                  complex_value value)
{
	ir_graph  *const irg         = current_ir_graph;
	ir_type   *const frame_type  = get_irg_frame_type(irg);
	ident     *const id          = id_unique("cmplex_tmp.%u");
	ir_type   *const irtype      = get_ir_type(type);
	ir_entity *const tmp_storage = new_entity(frame_type, id, irtype);
	ir_node   *const frame       = get_irg_frame(irg);
	ir_node   *const nomem       = get_irg_no_mem(irg);
	ir_node   *const addr        = new_simpleSel(nomem, frame, tmp_storage);
	set_entity_compiler_generated(tmp_storage, 1);
	store_complex(dbgi, addr, type, value);
	return addr;
}

static complex_value read_localvar_complex(dbg_info *dbgi, entity_t *const entity)
{
	assert(entity->declaration.kind == DECLARATION_KIND_LOCAL_VARIABLE
	    || entity->declaration.kind == DECLARATION_KIND_PARAMETER);
	type_t  *const type = skip_typeref(entity->declaration.type);
	ir_mode *const mode = get_complex_mode_storage(type);
	ir_node *const real = get_value(entity->variable.v.value_number, mode);
	ir_node *const imag = get_value(entity->variable.v.value_number+1, mode);
	ir_mode *const mode_arithmetic = get_complex_mode_arithmetic(type);
	return (complex_value) {
		create_conv(dbgi, real, mode_arithmetic),
		create_conv(dbgi, imag, mode_arithmetic)
	};
}

static complex_value complex_deref_address(dbg_info *const dbgi,
                                           type_t *type, ir_node *const addr,
                                           ir_cons_flags flags)
{
	type = skip_typeref(type);
	assert(is_type_complex(type));

	if (type->base.qualifiers & TYPE_QUALIFIER_VOLATILE)
		flags |= cons_volatile;
	ir_mode   *const mode      = get_complex_mode_storage(type);
	ir_node   *const memory    = get_store();
	ir_node   *const load      = new_d_Load(dbgi, memory, addr, mode, flags);
	ir_node   *const load_mem  = new_Proj(load, mode_M, pn_Load_M);
	ir_node   *const load_res  = new_Proj(load, mode,   pn_Load_res);

	ir_type   *const irtype    = get_ir_type(type);
	ir_mode   *const mode_uint = atomic_modes[ATOMIC_TYPE_UINT];
	ir_node   *const in[1]     = { new_Const(get_mode_one(mode_uint)) };
	ir_entity *const entity    = get_array_element_entity(irtype);
	ir_node   *const nomem     = get_irg_no_mem(current_ir_graph);
	ir_node   *const addr2     = new_Sel(nomem, addr, 1, in, entity);
	ir_node   *const load2     = new_d_Load(dbgi, load_mem, addr2, mode, flags);
	ir_node   *const load_mem2 = new_Proj(load2, mode_M, pn_Load_M);
	ir_node   *const load_res2 = new_Proj(load2, mode, pn_Load_res);
	set_store(load_mem2);

	return (complex_value) { load_res, load_res2 };
}

static complex_value complex_reference_to_firm(const reference_expression_t *ref)
{
	dbg_info *const dbgi   = get_dbg_info(&ref->base.pos);
	entity_t *const entity = ref->entity;
	assert(is_declaration(entity));

	switch ((declaration_kind_t)entity->declaration.kind) {
	case DECLARATION_KIND_LOCAL_VARIABLE:
	case DECLARATION_KIND_PARAMETER:
		return read_localvar_complex(dbgi, entity);
	default: {
		ir_node *const addr = reference_addr(ref);
		return complex_deref_address(dbgi, entity->declaration.type, addr, cons_none);
	}
	}
}

static complex_value complex_select_to_firm(const select_expression_t *select)
{
	dbg_info *const dbgi = get_dbg_info(&select->base.pos);
	ir_node  *const addr = select_addr(select);
	type_t   *const type = skip_typeref(select->base.type);
	return complex_deref_address(dbgi, type, addr, cons_none);
}

static complex_value complex_array_access_to_firm(
	const array_access_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.pos);
	ir_node  *addr = array_access_addr(expression);
	type_t   *type = skip_typeref(expression->base.type);
	assert(is_type_complex(type));
	return complex_deref_address(dbgi, type, addr, cons_none);
}

static complex_value get_complex_from_lvalue(const expression_t *expression,
                                             ir_node *addr)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.pos);

	if (expression->kind == EXPR_REFERENCE) {
		const reference_expression_t *ref = &expression->reference;

		entity_t *entity = ref->entity;
		assert(entity->kind == ENTITY_VARIABLE
		    || entity->kind == ENTITY_PARAMETER);
		assert(entity->declaration.kind != DECLARATION_KIND_UNKNOWN);
		if (entity->declaration.kind == DECLARATION_KIND_LOCAL_VARIABLE ||
		    entity->declaration.kind == DECLARATION_KIND_PARAMETER) {
		    return read_localvar_complex(dbgi, entity);
		}
	}

	assert(addr != NULL);
	return complex_deref_address(dbgi, expression->base.type, addr, cons_none);
}

static complex_value complex_cast_to_firm(const unary_expression_t *expression)
{
	const expression_t *const value     = expression->value;
	dbg_info           *const dbgi      = get_dbg_info(&expression->base.pos);
	type_t             *const from_type = skip_typeref(value->base.type);
	type_t             *const to_type   = skip_typeref(expression->base.type);
	ir_mode            *const mode      = get_complex_mode_storage(to_type);

	if (is_type_complex(from_type)) {
		complex_value cvalue = expression_to_complex(value);
		return complex_conv(dbgi, cvalue, mode);
	} else {
		ir_node *const value_node = expression_to_value(value);
		ir_node *const zero       = new_Const(get_mode_null(mode));
		ir_node *const casted     = create_conv(dbgi, value_node, mode);
		return (complex_value) { casted, zero };
	}
}

typedef complex_value (*new_complex_binop)(dbg_info *dbgi, complex_value left,
                                           complex_value right, ir_mode *mode);

static complex_value new_complex_add(dbg_info *dbgi, complex_value left,
                                     complex_value right, ir_mode *mode)
{
	return (complex_value) {
		new_d_Add(dbgi, left.real, right.real, mode),
		new_d_Add(dbgi, left.imag, right.imag, mode)
	};
}

static complex_value new_complex_sub(dbg_info *dbgi, complex_value left,
                                     complex_value right, ir_mode *mode)
{
	return (complex_value) {
		new_d_Sub(dbgi, left.real, right.real, mode),
		new_d_Sub(dbgi, left.imag, right.imag, mode)
	};
}

static complex_value new_complex_mul(dbg_info *dbgi, complex_value left,
                                     complex_value right, ir_mode *mode)
{
	ir_node *const op1 = new_d_Mul(dbgi, left.real, right.real, mode);
	ir_node *const op2 = new_d_Mul(dbgi, left.imag, right.imag, mode);
	ir_node *const op3 = new_d_Mul(dbgi, left.real, right.imag, mode);
	ir_node *const op4 = new_d_Mul(dbgi, left.imag, right.real, mode);
	return (complex_value) {
		new_d_Sub(dbgi, op1, op2, mode),
		new_d_Add(dbgi, op3, op4, mode)
	};
}

static complex_value new_complex_div(dbg_info *dbgi, complex_value left,
                                     complex_value right, ir_mode *mode)
{
	ir_node *const op1 = new_d_Mul(dbgi, left.real, right.real, mode);
	ir_node *const op2 = new_d_Mul(dbgi, left.imag, right.imag, mode);
	ir_node *const op3 = new_d_Mul(dbgi, left.imag, right.real, mode);
	ir_node *const op4 = new_d_Mul(dbgi, left.real, right.imag, mode);
	ir_node *const op5 = new_d_Mul(dbgi, right.real, right.real, mode);
	ir_node *const op6 = new_d_Mul(dbgi, right.imag, right.imag, mode);
	ir_node *const real_dividend = new_d_Add(dbgi, op1, op2, mode);
	ir_node *const real_divisor  = new_d_Add(dbgi, op5, op6, mode);
	ir_node *const imag_dividend = new_d_Sub(dbgi, op3, op4, mode);
	ir_node *const imag_divisor  = new_d_Add(dbgi, op5, op6, mode);
	return (complex_value) {
		create_div(dbgi, real_dividend, real_divisor, mode),
		create_div(dbgi, imag_dividend, imag_divisor, mode)
	};
}

typedef complex_value (*new_complex_unop)(dbg_info *dbgi, complex_value value,
                                          ir_mode *mode);

static complex_value new_complex_increment(dbg_info *dbgi, complex_value value,
                                           ir_mode *mode)
{
	ir_node *one = new_Const(get_mode_one(mode));
	return (complex_value) {
		new_d_Add(dbgi, value.real, one, mode),
		value.imag
	};
}

static complex_value new_complex_decrement(dbg_info *dbgi, complex_value value,
                                           ir_mode *mode)
{
	ir_node *one = new_Const(get_mode_one(mode));
	return (complex_value) {
		new_d_Sub(dbgi, value.real, one, mode),
		value.imag
	};
}

static void set_complex_value_for_expression(dbg_info *dbgi,
											 const expression_t *expression,
                                             complex_value value,
                                             ir_node *addr)
{
	type_t  *const type = skip_typeref(expression->base.type);
	ir_mode *const mode = get_complex_mode_storage(type);
	ir_node *const real = create_conv(dbgi, value.real, mode);
	ir_node *const imag = create_conv(dbgi, value.imag, mode);

	if (expression->kind == EXPR_REFERENCE) {
		const reference_expression_t *ref = &expression->reference;

		entity_t *entity = ref->entity;
		assert(is_declaration(entity));
		assert(entity->declaration.kind != DECLARATION_KIND_UNKNOWN);
		if (entity->declaration.kind == DECLARATION_KIND_LOCAL_VARIABLE ||
		    entity->declaration.kind == DECLARATION_KIND_PARAMETER) {
			set_value(entity->variable.v.value_number, real);
			set_value(entity->variable.v.value_number+1, imag);
			return;
		}
	}

	if (addr == NULL)
		addr = expression_to_addr(expression);
	assert(addr != NULL);
	store_complex(dbgi, addr, type, value);
}

static complex_value create_complex_assign_unop(const unary_expression_t *unop,
                                                new_complex_unop constructor,
                                                bool return_old)
{
	dbg_info *const     dbgi       = get_dbg_info(&unop->base.pos);
	const expression_t *value_expr = unop->value;
	ir_node            *addr       = expression_to_addr(value_expr);
	complex_value       value      = get_complex_from_lvalue(value_expr, addr);
	type_t             *type       = skip_typeref(unop->base.type);
	ir_mode            *mode       = get_complex_mode_arithmetic(type);
	value = complex_conv(dbgi, value, mode);
	complex_value       new_value  = constructor(dbgi, value, mode);
	set_complex_value_for_expression(dbgi, value_expr, new_value, addr);
	return return_old ? value : new_value;
}

static complex_value complex_negate_to_firm(const unary_expression_t *expr)
{
	complex_value cvalue = expression_to_complex(expr->value);
	dbg_info     *dbgi   = get_dbg_info(&expr->base.pos);
	ir_mode      *mode   = get_complex_mode_arithmetic(expr->base.type);
	cvalue = complex_conv(dbgi, cvalue, mode);
	return (complex_value) {
		new_d_Minus(dbgi, cvalue.real, mode),
		new_d_Minus(dbgi, cvalue.imag, mode)
	};
}

static complex_value complex_complement_to_firm(const unary_expression_t *expr)
{
	complex_value cvalue = expression_to_complex(expr->value);
	dbg_info     *dbgi   = get_dbg_info(&expr->base.pos);
	ir_mode      *mode   = get_complex_mode_arithmetic(expr->base.type);
	cvalue = complex_conv(dbgi, cvalue, mode);
	return (complex_value) {
		cvalue.real,
		new_d_Minus(dbgi, cvalue.imag, mode)
	};
}

static complex_value create_complex_binop(const binary_expression_t *binexpr,
                                          new_complex_binop constructor)
{
	dbg_info     *dbgi  = get_dbg_info(&binexpr->base.pos);
	ir_mode      *mode  = get_complex_mode_arithmetic(binexpr->base.type);
	complex_value left  = expression_to_complex(binexpr->left);
	complex_value right = expression_to_complex(binexpr->right);
	left  = complex_conv(dbgi, left, mode);
	right = complex_conv(dbgi, right, mode);
	return constructor(dbgi, left, right, mode);
}

static complex_value create_complex_assign_binop(const binary_expression_t *binexpr,
                                                 new_complex_binop constructor)
{
	dbg_info      *dbgi   = get_dbg_info(&binexpr->base.pos);
	expression_t  *lefte  = binexpr->left;
	expression_t  *righte = binexpr->right;
	ir_mode       *mode   = get_complex_mode_arithmetic(righte->base.type);
	ir_node       *addr   = expression_to_addr(lefte);
	complex_value  left   = get_complex_from_lvalue(lefte, addr);
	complex_value  right  = expression_to_complex(righte);
	left  = complex_conv(dbgi, left, mode);
	right = complex_conv(dbgi, right, mode);
	complex_value  new_value = constructor(dbgi, left, right, mode);
	type_t        *res_type  = skip_typeref(binexpr->base.type);
	set_complex_value_for_expression(dbgi, lefte, new_value, addr);
	return complex_conv_to_storage(dbgi, new_value, res_type);
}

static complex_value complex_call_to_firm(const call_expression_t *call)
{
	ir_node         *result        = call_expression_to_firm(call);
	expression_t    *function      = call->function;
	type_t          *type          = skip_typeref(function->base.type);
	assert(is_type_pointer(type));
	pointer_type_t  *pointer_type  = &type->pointer;
	type_t          *points_to     = skip_typeref(pointer_type->points_to);
	assert(is_type_function(points_to));
	function_type_t *function_type = &points_to->function;
	type_t          *return_type   = skip_typeref(function_type->return_type);
	assert(is_type_complex(return_type));
	dbg_info        *dbgi          = get_dbg_info(&call->base.pos);
	return complex_deref_address(dbgi, return_type, result, cons_floats);
}

static void complex_equality_evaluation(const binary_expression_t *binexpr,
	jump_target *const true_target, jump_target *const false_target,
	ir_relation relation)
{
	jump_target extra_target;
	init_jump_target(&extra_target, NULL);

	complex_value left  = expression_to_complex(binexpr->left);
	complex_value right = expression_to_complex(binexpr->right);
	dbg_info     *dbgi  = get_dbg_info(&binexpr->base.pos);
	ir_mode      *mode  = get_complex_mode_arithmetic(binexpr->left->base.type);
	left  = complex_conv(dbgi, left, mode);
	right = complex_conv(dbgi, right, mode);

	ir_node  *cmp_real   = new_d_Cmp(dbgi, left.real, right.real, relation);
	ir_node  *cond       = new_d_Cond(dbgi, cmp_real);
	ir_node  *true_proj  = new_Proj(cond, mode_X, pn_Cond_true);
	ir_node  *false_proj = new_Proj(cond, mode_X, pn_Cond_false);
	add_pred_to_jump_target(&extra_target, true_proj);
	add_pred_to_jump_target(false_target, false_proj);
	if (!enter_jump_target(&extra_target))
		return;

	ir_node *cmp_imag     = new_d_Cmp(dbgi, left.imag, right.imag, relation);
	ir_node *condi        = new_d_Cond(dbgi, cmp_imag);
	ir_node *true_proj_i  = new_Proj(condi, mode_X, pn_Cond_true);
	ir_node *false_proj_i = new_Proj(condi, mode_X, pn_Cond_false);
	add_pred_to_jump_target(true_target, true_proj_i);
	add_pred_to_jump_target(false_target, false_proj_i);
	set_unreachable_now();
}

static complex_value complex_to_control_flow(
	const expression_t *const expression, jump_target *const true_target,
	jump_target *const false_target)
{
	jump_target extra_target;
	init_jump_target(&extra_target, NULL);
	complex_value       value      = expression_to_complex(expression);
	if (is_Const(value.real) && is_Const(value.imag)) {
		ir_tarval *tv_real = get_Const_tarval(value.real);
		ir_tarval *tv_imag = get_Const_tarval(value.imag);
		if (tarval_is_null(tv_real) && tarval_is_null(tv_imag)) {
			jump_to_target(false_target);
		} else {
			jump_to_target(true_target);
		}
		set_unreachable_now();
		return value;
	}

	dbg_info     *const dbgi       = get_dbg_info(&expression->base.pos);
	type_t       *const type       = expression->base.type;
	ir_mode      *const mode       = get_complex_mode_arithmetic(type);
	value = complex_conv(dbgi, value, mode);
	ir_node      *const zero       = new_Const(get_mode_null(mode));
	ir_node      *const cmp_real   =
		new_d_Cmp(dbgi, value.real, zero, ir_relation_unordered_less_greater);
	ir_node      *const cond_real  = new_d_Cond(dbgi, cmp_real);
	ir_node      *const true_real  = new_Proj(cond_real, mode_X, pn_Cond_true);
	ir_node      *const false_real = new_Proj(cond_real, mode_X, pn_Cond_false);
	add_pred_to_jump_target(true_target, true_real);
	add_pred_to_jump_target(&extra_target, false_real);
	if (!enter_jump_target(&extra_target))
		return value;

	ir_node      *const cmp_imag   =
		new_d_Cmp(dbgi, value.imag, zero, ir_relation_unordered_less_greater);
	ir_node      *const cond_imag  = new_d_Cond(dbgi, cmp_imag);
	ir_node      *const true_imag  = new_Proj(cond_imag, mode_X, pn_Cond_true);
	ir_node      *const false_imag = new_Proj(cond_imag, mode_X, pn_Cond_false);
	add_pred_to_jump_target(true_target, true_imag);
	add_pred_to_jump_target(false_target, false_imag);
	set_unreachable_now();

	return value;
}

static complex_value complex_conditional_to_firm(
	const conditional_expression_t *const expression)
{
	jump_target true_target;
	jump_target false_target;
	init_jump_target(&true_target,  NULL);
	init_jump_target(&false_target, NULL);
	complex_value cond_val;
	memset(&cond_val, 0, sizeof(cond_val));
	if (expression->true_expression == NULL) {
		assert(is_type_complex(skip_typeref(expression->condition->base.type)));
		cond_val = complex_to_control_flow(expression->condition,
		                                   &true_target, &false_target);
	} else {
		expression_to_control_flow(expression->condition, &true_target, &false_target);
	}

	complex_value  val;
	memset(&val, 0, sizeof(val));
	jump_target    exit_target;
	init_jump_target(&exit_target, NULL);
	type_t   *const type = skip_typeref(expression->base.type);
	ir_mode  *const mode = get_complex_mode_arithmetic(type);
	dbg_info *const dbgi = get_dbg_info(&expression->base.pos);

	if (enter_jump_target(&true_target)) {
		if (expression->true_expression) {
			val = expression_to_complex(expression->true_expression);
		} else {
			assert(cond_val.real != NULL);
			val = cond_val;
		}
		val = complex_conv(dbgi, val, mode);
		jump_to_target(&exit_target);
	}

	if (enter_jump_target(&false_target)) {
		complex_value false_val
			= expression_to_complex(expression->false_expression);
		false_val = complex_conv(dbgi, false_val, mode);
		jump_to_target(&exit_target);
		if (val.real != NULL) {
			ir_node  *const inr[] = { val.real, false_val.real };
			ir_node  *const ini[] = { val.imag, false_val.imag };
			ir_node  *const block = exit_target.block;
			val.real = new_rd_Phi(dbgi, block, ARRAY_SIZE(inr), inr, mode);
			val.imag = new_rd_Phi(dbgi, block, ARRAY_SIZE(ini), ini, mode);
		} else {
			val = false_val;
		}
	}

	if (!enter_jump_target(&exit_target)) {
		set_cur_block(new_Block(0, NULL));
		assert(!is_type_void(type));
		val.real = val.imag = new_Bad(mode);
	}
	return val;
}

static void create_local_declarations(entity_t*);

static complex_value compound_statement_to_firm_complex(
	const compound_statement_t *compound)
{
	create_local_declarations(compound->scope.entities);

	complex_value result    = { NULL, NULL };
	statement_t  *statement = compound->statements;
	statement_t  *next;
	for ( ; statement != NULL; statement = next) {
		next = statement->base.next;
		/* last statement is the return value */
		if (next == NULL) {
			/* it must be an expression, otherwise we wouldn't be in the
			 * complex variant of compound_statement_to_firm */
			if (statement->kind != STATEMENT_EXPRESSION)
				panic("last member of complex statement expression not an expression statement");
			expression_t *expression = statement->expression.expression;
			assert(is_type_complex(skip_typeref(expression->base.type)));
			result = expression_to_complex(expression);
		} else {
			statement_to_firm(statement);
		}
	}

	return result;
}

static complex_value complex_assign_to_firm(const binary_expression_t *expr)
{
	dbg_info     *const dbgi  = get_dbg_info(&expr->base.pos);
	complex_value const value = expression_to_complex(expr->right);
	ir_node      *const addr  = expression_to_addr(expr->left);
	set_complex_value_for_expression(dbgi, expr->left, value, addr);
	return value;
}

static complex_value complex_statement_expression_to_firm(
	const statement_expression_t *const expr)
{
	const statement_t *const statement = expr->statement;
	assert(statement->kind == STATEMENT_COMPOUND);

	return compound_statement_to_firm_complex(&statement->compound);
}

static complex_value complex_dereference_to_firm(
	const unary_expression_t *const expr)
{
	ir_node  *const addr = expression_to_value(expr->value);
	dbg_info *const dbgi = get_dbg_info(&expr->base.pos);
	type_t   *const type = skip_typeref(expr->base.type);
	return complex_deref_address(dbgi, type, addr, cons_none);
}

static complex_value expression_to_complex(const expression_t *expression)
{
	switch (expression->kind) {
	case EXPR_REFERENCE:
		return complex_reference_to_firm(&expression->reference);
	case EXPR_SELECT:
		return complex_select_to_firm(&expression->select);
	case EXPR_ARRAY_ACCESS:
		return complex_array_access_to_firm(&expression->array_access);
	case EXPR_UNARY_CAST:
		return complex_cast_to_firm(&expression->unary);
	case EXPR_BINARY_COMMA:
		evaluate_expression_discard_result(expression->binary.left);
		return expression_to_complex(expression->binary.right);
	case EXPR_BINARY_ADD:
		return create_complex_binop(&expression->binary, new_complex_add);
	case EXPR_BINARY_ADD_ASSIGN:
		return create_complex_assign_binop(&expression->binary, new_complex_add);
	case EXPR_BINARY_SUB:
		return create_complex_binop(&expression->binary, new_complex_sub);
	case EXPR_BINARY_SUB_ASSIGN:
		return create_complex_assign_binop(&expression->binary, new_complex_sub);
	case EXPR_BINARY_MUL:
		return create_complex_binop(&expression->binary, new_complex_mul);
	case EXPR_BINARY_MUL_ASSIGN:
		return create_complex_assign_binop(&expression->binary, new_complex_mul);
	case EXPR_BINARY_DIV:
		return create_complex_binop(&expression->binary, new_complex_div);
	case EXPR_BINARY_DIV_ASSIGN:
		return create_complex_assign_binop(&expression->binary, new_complex_div);
	case EXPR_UNARY_PLUS:
		return expression_to_complex(expression->unary.value);
	case EXPR_UNARY_PREFIX_INCREMENT:
		return create_complex_assign_unop(&expression->unary,
		                                  new_complex_increment, false);
	case EXPR_UNARY_PREFIX_DECREMENT:
		return create_complex_assign_unop(&expression->unary,
		                                  new_complex_decrement, false);
	case EXPR_UNARY_POSTFIX_INCREMENT:
		return create_complex_assign_unop(&expression->unary,
		                                  new_complex_increment, true);
	case EXPR_UNARY_POSTFIX_DECREMENT:
		return create_complex_assign_unop(&expression->unary,
		                                  new_complex_decrement, true);
	case EXPR_UNARY_NEGATE:
		return complex_negate_to_firm(&expression->unary);
	case EXPR_UNARY_COMPLEMENT:
		return complex_complement_to_firm(&expression->unary);
	case EXPR_UNARY_DEREFERENCE:
		return complex_dereference_to_firm(&expression->unary);
	case EXPR_BINARY_ASSIGN:
		return complex_assign_to_firm(&expression->binary);
	case EXPR_LITERAL_INTEGER:
	case EXPR_LITERAL_FLOATINGPOINT: {
		complex_constant cnst = fold_complex_literal(&expression->literal);
		dbg_info        *dbgi = get_dbg_info(&expression->base.pos);
		return (complex_value) {
			new_d_Const(dbgi, cnst.real),
			new_d_Const(dbgi, cnst.imag)
		};
	}
	case EXPR_CALL:
		return complex_call_to_firm(&expression->call);
	case EXPR_CONDITIONAL:
		return complex_conditional_to_firm(&expression->conditional);
	case EXPR_STATEMENT:
		return complex_statement_expression_to_firm(&expression->statement);
	case NEVER_COMPLEX_CASES:
		break;
	}
	panic("unexpected complex expression");
}

static void create_variable_entity(entity_t *variable,
                                   declaration_kind_t declaration_kind,
                                   ir_type *parent_type)
{
	assert(variable->kind == ENTITY_VARIABLE);
	type_t    *type = skip_typeref(variable->declaration.type);

	ident     *const id        = new_id_from_str(variable->base.symbol->string);
	ir_type   *const irtype    = get_ir_type(type);
	dbg_info  *const dbgi      = get_dbg_info(&variable->base.pos);
	unsigned         alignment = variable->declaration.alignment;

	ir_entity *irentity;
	if (variable->variable.alias.entity != NULL) {
		/* create alias entity but do not set aliased yet as we can't resolved
		 * it at this point yet. */
		irentity = new_alias_entity(parent_type, id, NULL, irtype);
	} else {
		irentity = new_entity(parent_type, id, irtype);
	}
	set_entity_dbg_info(irentity, dbgi);
	set_entity_alignment(irentity, alignment);

	handle_decl_modifiers(irentity, variable);

	variable->declaration.kind  = (unsigned char) declaration_kind;
	variable->variable.v.entity = irentity;
	set_entity_ld_ident(irentity, create_ld_ident(variable));

	if (type->base.qualifiers & TYPE_QUALIFIER_VOLATILE) {
		set_entity_volatility(irentity, volatility_is_volatile);
	}
}


typedef struct type_path_entry_t type_path_entry_t;
struct type_path_entry_t {
	type_t           *type;
	ir_initializer_t *initializer;
	size_t            index;
	entity_t         *compound_entry;
};

typedef struct type_path_t type_path_t;
struct type_path_t {
	type_path_entry_t *path;
	type_t            *top_type;
	bool               invalid;
};

static __attribute__((unused)) void debug_print_type_path(const type_path_t *path)
{
	size_t len = ARR_LEN(path->path);

	for (size_t i = 0; i < len; ++i) {
		const type_path_entry_t *entry = & path->path[i];

		type_t *type = skip_typeref(entry->type);
		if (is_type_compound(type)) {
			fprintf(stderr, ".%s", entry->compound_entry->base.symbol->string);
		} else if (is_type_array(type)) {
			fprintf(stderr, "[%u]", (unsigned) entry->index);
		} else {
			fprintf(stderr, "-INVALID-");
		}
	}
	fprintf(stderr, "  (");
	print_type(path->top_type);
	fprintf(stderr, ")");
}

static type_path_entry_t *get_type_path_top(const type_path_t *path)
{
	size_t len = ARR_LEN(path->path);
	assert(len > 0);
	return & path->path[len-1];
}

static type_path_entry_t *append_to_type_path(type_path_t *path)
{
	size_t len = ARR_LEN(path->path);
	ARR_RESIZE(type_path_entry_t, path->path, len+1);

	type_path_entry_t *result = & path->path[len];
	memset(result, 0, sizeof(result[0]));
	return result;
}

static size_t get_compound_member_count(const compound_type_t *type)
{
	compound_t *compound  = type->compound;
	size_t      n_members = 0;
	entity_t   *member    = compound->members.entities;
	for ( ; member != NULL; member = member->base.next) {
		/* skip anonymous bitfield members */
		if (member->compound_member.bitfield && member->base.symbol == NULL)
		    continue;
		++n_members;
	}

	return n_members;
}

static ir_initializer_t *get_initializer_entry(type_path_t *path)
{
#ifndef NDEBUG
	type_t *orig_top_type = path->top_type;
	type_t *top_type      = skip_typeref(orig_top_type);

	assert(is_type_compound(top_type) || is_type_array(top_type));
#endif

	if (ARR_LEN(path->path) == 0) {
		return NULL;
	} else {
		type_path_entry_t *top         = get_type_path_top(path);
		ir_initializer_t  *initializer = top->initializer;
		return get_initializer_compound_value(initializer, top->index);
	}
}

static void descend_into_subtype(type_path_t *path)
{
	type_t *orig_top_type = path->top_type;
	type_t *top_type      = skip_typeref(orig_top_type);

	assert(is_type_compound(top_type) || is_type_array(top_type));

	ir_initializer_t *initializer = get_initializer_entry(path);

	type_path_entry_t *top = append_to_type_path(path);
	top->type              = top_type;

	size_t len;

	if (is_type_compound(top_type)) {
		compound_t *const compound = top_type->compound.compound;
		entity_t   *const entry    = skip_unnamed_bitfields(compound->members.entities);

		top->compound_entry = entry;
		top->index          = 0;
		len                 = get_compound_member_count(&top_type->compound);
		if (entry != NULL) {
			assert(entry->kind == ENTITY_COMPOUND_MEMBER);
			path->top_type = entry->declaration.type;
		}
	} else {
		assert(is_type_array(top_type));
		assert(top_type->array.size > 0);

		top->index     = 0;
		path->top_type = top_type->array.element_type;
		len            = top_type->array.size;
	}
	if (initializer == NULL
	    || get_initializer_kind(initializer) == IR_INITIALIZER_NULL) {
		initializer = create_initializer_compound(len);
		/* we have to set the entry at the 2nd latest path entry... */
		size_t path_len = ARR_LEN(path->path);
		assert(path_len >= 1);
		if (path_len > 1) {
			type_path_entry_t *entry        = & path->path[path_len-2];
			ir_initializer_t  *tinitializer = entry->initializer;
			set_initializer_compound_value(tinitializer, entry->index,
			                               initializer);
		}
	}
	top->initializer = initializer;
}

static void ascend_from_subtype(type_path_t *path)
{
	type_path_entry_t *top = get_type_path_top(path);

	path->top_type = top->type;

	size_t len = ARR_LEN(path->path);
	ARR_RESIZE(type_path_entry_t, path->path, len-1);
}

static void walk_designator(type_path_t *path, const designator_t *designator)
{
	/* designators start at current object type */
	ARR_RESIZE(type_path_entry_t, path->path, 1);

	for ( ; designator != NULL; designator = designator->next) {
		type_path_entry_t *top         = get_type_path_top(path);
		type_t            *orig_type   = top->type;
		type_t            *type        = skip_typeref(orig_type);

		if (designator->symbol != NULL) {
			assert(is_type_compound(type));
			size_t    index_int = 0;
			symbol_t *symbol    = designator->symbol;

			compound_t *compound = type->compound.compound;
			entity_t   *iter     = compound->members.entities;
			for (; iter->base.symbol != symbol; iter = iter->base.next, ++index_int) {}
			assert(iter->kind == ENTITY_COMPOUND_MEMBER);

			/* revert previous initialisations of other union elements */
			if (type->kind == TYPE_COMPOUND_UNION) {
				ir_initializer_t *initializer = top->initializer;
				if (initializer != NULL
					&& get_initializer_kind(initializer) == IR_INITIALIZER_COMPOUND) {
					/* are we writing to a new element? */
					ir_initializer_t *oldi
						= get_initializer_compound_value(initializer, index_int);
					if (get_initializer_kind(oldi) == IR_INITIALIZER_NULL) {
						/* clear initializer */
						size_t len
							= get_initializer_compound_n_entries(initializer);
						ir_initializer_t *nulli = get_initializer_null();
						for (size_t i = 0; i < len; ++i) {
							set_initializer_compound_value(initializer, i,
							                               nulli);
						}
					}
				}
			}

			top->type           = orig_type;
			top->compound_entry = iter;
			top->index          = index_int;
			orig_type           = iter->declaration.type;
		} else {
			expression_t *array_index = designator->array_index;
			assert(is_type_array(type));

			long index_long = fold_expression_to_int(array_index);
			assert(0 <= index_long && (!type->array.size_constant || (size_t)index_long < type->array.size));

			top->type  = orig_type;
			top->index = (size_t) index_long;
			orig_type  = type->array.element_type;
		}
		path->top_type = orig_type;

		if (designator->next != NULL) {
			descend_into_subtype(path);
		}
	}

	path->invalid  = false;
}

static void advance_current_object(type_path_t *path)
{
	if (path->invalid) {
		/* TODO: handle this... */
		panic("invalid initializer (excessive elements)");
	}

	type_path_entry_t *top = get_type_path_top(path);

	type_t *type = skip_typeref(top->type);
	if (is_type_union(type)) {
		/* only the first element is initialized in unions */
		top->compound_entry = NULL;
	} else if (is_type_struct(type)) {
		entity_t *entry = top->compound_entry;

		top->index++;
		entry               = skip_unnamed_bitfields(entry->base.next);
		top->compound_entry = entry;
		if (entry != NULL) {
			assert(entry->kind == ENTITY_COMPOUND_MEMBER);
			path->top_type = entry->declaration.type;
			return;
		}
	} else {
		assert(is_type_array(type));

		top->index++;
		if (!type->array.size_constant || top->index < type->array.size) {
			return;
		}
	}

	/* we're past the last member of the current sub-aggregate, try if we
	 * can ascend in the type hierarchy and continue with another subobject */
	size_t len = ARR_LEN(path->path);

	if (len > 1) {
		ascend_from_subtype(path);
		advance_current_object(path);
	} else {
		path->invalid = true;
	}
}


static ir_initializer_t *create_ir_initializer_value(
		const initializer_value_t *initializer)
{
	expression_t *expr = initializer->value;
	type_t       *type = skip_typeref(expr->base.type);

	if (is_type_compound(type)) {
		if (expr->kind == EXPR_UNARY_CAST) {
			expr = expr->unary.value;
			type = skip_typeref(expr->base.type);
		}
		/* must be a compound literal... */
		if (expr->kind == EXPR_COMPOUND_LITERAL) {
			return create_ir_initializer(expr->compound_literal.initializer,
			                             type);
		}
	} else if (is_type_complex(type)) {
		complex_value     const value     = expression_to_complex(expr);
		ir_mode          *const mode      = get_complex_mode_storage(type);
		ir_node          *const real      = create_conv(NULL, value.real, mode);
		ir_node          *const imag      = create_conv(NULL, value.imag, mode);
		ir_initializer_t *const res       = create_initializer_compound(2);
		ir_initializer_t *const init_real = create_initializer_const(real);
		ir_initializer_t *const init_imag = create_initializer_const(imag);
		set_initializer_compound_value(res, 0, init_real);
		set_initializer_compound_value(res, 1, init_imag);
		return res;
	}

	if (is_constant_expression(expr) >= EXPR_CLASS_CONSTANT) {
		ir_tarval *      tv   = fold_expression(expr);
		ir_mode   *const mode = get_ir_mode_storage(type);
		tv = tarval_convert_to(tv, mode);
		return create_initializer_tarval(tv);
	} else {
		ir_node *value = expression_to_value(expr);
		value = conv_to_storage_type(NULL, value, type);
		return create_initializer_const(value);
	}
}

/** Tests whether type can be initialized by a string constant */
static bool is_string_type(type_t *type)
{
	if (!is_type_array(type))
		return false;

	type_t *const inner = skip_typeref(type->array.element_type);
	return is_type_integer(inner);
}

static ir_initializer_t *create_ir_initializer_list(
		const initializer_list_t *initializer, type_t *type)
{
	type_path_t path;
	memset(&path, 0, sizeof(path));
	path.top_type = type;
	path.path     = NEW_ARR_F(type_path_entry_t, 0);

	descend_into_subtype(&path);

	for (size_t i = 0; i < initializer->len; ++i) {
		const initializer_t *sub_initializer = initializer->initializers[i];

		if (sub_initializer->kind == INITIALIZER_DESIGNATOR) {
			walk_designator(&path, sub_initializer->designator.designator);
			continue;
		}

		if (sub_initializer->kind == INITIALIZER_VALUE) {
			const expression_t *expr      = sub_initializer->value.value;
			const type_t       *expr_type = skip_typeref(expr->base.type);
			/* we might have to descend into types until the types match */
			while (true) {
				type_t *orig_top_type = path.top_type;
				type_t *top_type      = skip_typeref(orig_top_type);

				if (types_compatible(top_type, expr_type))
					break;
				descend_into_subtype(&path);
			}
		} else if (sub_initializer->kind == INITIALIZER_STRING) {
			/* we might have to descend into types until we're at a scalar
			 * type */
			while (true) {
				type_t *orig_top_type = path.top_type;
				type_t *top_type      = skip_typeref(orig_top_type);

				if (is_string_type(top_type))
					break;
				descend_into_subtype(&path);
			}
		}

		ir_initializer_t *sub_irinitializer
			= create_ir_initializer(sub_initializer, path.top_type);

		size_t path_len = ARR_LEN(path.path);
		assert(path_len >= 1);
		type_path_entry_t *entry        = & path.path[path_len-1];
		ir_initializer_t  *tinitializer = entry->initializer;
		set_initializer_compound_value(tinitializer, entry->index,
		                               sub_irinitializer);

		advance_current_object(&path);
	}

	assert(ARR_LEN(path.path) >= 1);
	ir_initializer_t *result = path.path[0].initializer;
	DEL_ARR_F(path.path);

	return result;
}

static ir_initializer_t *create_ir_initializer_string(initializer_t const *const init, type_t *type)
{
	type = skip_typeref(type);

	assert(type->kind == TYPE_ARRAY);
	assert(type->array.size_constant);
	string_literal_expression_t const *const str = get_init_string(init);
	size_t            const str_len = str->value.size;
	size_t            const arr_len = type->array.size;
	ir_initializer_t *const irinit  = create_initializer_compound(arr_len);
	ir_mode          *const mode    = get_ir_mode_storage(type->array.element_type);
	char const       *      p       = str->value.begin;
	switch (str->value.encoding) {
	case STRING_ENCODING_CHAR:
	case STRING_ENCODING_UTF8:
		for (size_t i = 0; i != arr_len; ++i) {
			char              const c      = i < str_len ? *p++ : 0;
			ir_tarval        *const tv     = new_tarval_from_long(c, mode);
			ir_initializer_t *const tvinit = create_initializer_tarval(tv);
			set_initializer_compound_value(irinit, i, tvinit);
		}
		break;

	case STRING_ENCODING_CHAR16:
	case STRING_ENCODING_CHAR32:
	case STRING_ENCODING_WIDE:
		for (size_t i = 0; i != arr_len; ++i) {
			utf32             const c      = i < str_len ? read_utf8_char(&p) : 0;
			ir_tarval        *const tv     = new_tarval_from_long(c, mode);
			ir_initializer_t *const tvinit = create_initializer_tarval(tv);
			set_initializer_compound_value(irinit, i, tvinit);
		}
		break;
	}

	return irinit;
}

static ir_initializer_t *create_ir_initializer(
		const initializer_t *initializer, type_t *type)
{
	switch (initializer->kind) {
	case INITIALIZER_STRING:
		return create_ir_initializer_string(initializer, type);

	case INITIALIZER_LIST:
		return create_ir_initializer_list(&initializer->list, type);

	case INITIALIZER_VALUE:
		return create_ir_initializer_value(&initializer->value);

	case INITIALIZER_DESIGNATOR:
		panic("unexpected designator initializer");
	}
	panic("unknown initializer");
}

/** ANSI C §6.7.8:21: If there are fewer initializers [..] than there
 *  are elements [...] the remainder of the aggregate shall be initialized
 *  implicitly the same as objects that have static storage duration. */
static void create_dynamic_null_initializer(ir_entity *entity, dbg_info *dbgi,
		ir_node *base_addr)
{
	/* for unions we must NOT do anything for null initializers */
	ir_type *owner = get_entity_owner(entity);
	if (is_Union_type(owner)) {
		return;
	}

	ir_type *ent_type = get_entity_type(entity);
	/* create sub-initializers for a compound type */
	if (is_compound_type(ent_type)) {
		unsigned n_members = get_compound_n_members(ent_type);
		for (unsigned n = 0; n < n_members; ++n) {
			ir_entity *member = get_compound_member(ent_type, n);
			ir_node   *addr   = new_d_simpleSel(dbgi, new_NoMem(), base_addr,
				                                member);
			create_dynamic_null_initializer(member, dbgi, addr);
		}
		return;
	}
	if (is_Array_type(ent_type)) {
		assert(has_array_upper_bound(ent_type, 0));
		long n = get_array_upper_bound_int(ent_type, 0);
		for (long i = 0; i < n; ++i) {
			ir_mode   *mode_uint = atomic_modes[ATOMIC_TYPE_UINT];
			ir_tarval *index_tv = new_tarval_from_long(i, mode_uint);
			ir_node   *cnst     = new_d_Const(dbgi, index_tv);
			ir_node   *in[1]    = { cnst };
			ir_entity *arrent   = get_array_element_entity(ent_type);
			ir_node   *addr     = new_d_Sel(dbgi, new_NoMem(), base_addr, 1, in,
			                                arrent);
			create_dynamic_null_initializer(arrent, dbgi, addr);
		}
		return;
	}

	ir_mode *value_mode = get_type_mode(ent_type);
	ir_node *node       = new_Const(get_mode_null(value_mode));

	/* is it a bitfield type? */
	if (is_compound_type(get_entity_owner(entity))
	    && get_entity_bitfield_size(entity) > 0) {
		bitfield_store_to_firm(dbgi, entity, base_addr, node, false, false);
		return;
	}

	ir_node *mem    = get_store();
	ir_node *store  = new_d_Store(dbgi, mem, base_addr, node, cons_none);
	ir_node *proj_m = new_Proj(store, mode_M, pn_Store_M);
	set_store(proj_m);
}

static void create_dynamic_initializer_sub(ir_initializer_t *initializer,
		ir_entity *entity, ir_type *type, dbg_info *dbgi, ir_node *base_addr)
{
	switch (get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		create_dynamic_null_initializer(entity, dbgi, base_addr);
		return;
	case IR_INITIALIZER_CONST: {
		ir_node *node     = get_initializer_const_value(initializer);
		ir_type *ent_type = get_entity_type(entity);

		/* is it a bitfield type? */
		if (is_compound_type(get_entity_owner(entity))
		    && get_entity_bitfield_size(entity) > 0) {
			bitfield_store_to_firm(dbgi, entity, base_addr, node, false, false);
			return;
		}

		ir_node *mem = get_store();
		ir_node *new_mem;
		if (is_compound_type(ent_type)) {
			new_mem = new_d_CopyB(dbgi, mem, base_addr, node, ent_type, cons_none);
		} else {
			assert(get_type_mode(type) == get_irn_mode(node));
			ir_node *store = new_d_Store(dbgi, mem, base_addr, node, cons_none);
			new_mem = new_Proj(store, mode_M, pn_Store_M);
		}
		set_store(new_mem);
		return;
	}
	case IR_INITIALIZER_TARVAL: {
		ir_tarval *tv   = get_initializer_tarval_value(initializer);
		ir_node   *cnst = new_d_Const(dbgi, tv);

		/* is it a bitfield type? */
		if (is_compound_type(get_entity_owner(entity))
		    && get_entity_bitfield_size(entity) > 0) {
			bitfield_store_to_firm(dbgi, entity, base_addr, cnst, false, false);
			return;
		}

		assert(get_type_mode(type) == get_tarval_mode(tv));
		ir_node *mem    = get_store();
		ir_node *store  = new_d_Store(dbgi, mem, base_addr, cnst, cons_none);
		ir_node *proj_m = new_Proj(store, mode_M, pn_Store_M);
		set_store(proj_m);
		return;
	}
	case IR_INITIALIZER_COMPOUND: {
		assert(is_compound_type(type) || is_Array_type(type));
		int n_members;
		if (is_Array_type(type)) {
			assert(has_array_upper_bound(type, 0));
			n_members = get_array_upper_bound_int(type, 0);
		} else {
			n_members = get_compound_n_members(type);
		}

		if (get_initializer_compound_n_entries(initializer)
				!= (unsigned) n_members)
			panic("initializer doesn't match compound type");

		for (int i = 0; i < n_members; ++i) {
			ir_node   *addr;
			ir_type   *irtype;
			ir_entity *sub_entity;
			if (is_Array_type(type)) {
				ir_mode   *mode_uint = atomic_modes[ATOMIC_TYPE_UINT];
				ir_tarval *index_tv = new_tarval_from_long(i, mode_uint);
				ir_node   *cnst     = new_d_Const(dbgi, index_tv);
				ir_node   *in[1]    = { cnst };
				irtype     = get_array_element_type(type);
				sub_entity = get_array_element_entity(type);
				addr       = new_d_Sel(dbgi, new_NoMem(), base_addr, 1, in,
				                       sub_entity);
			} else {
				sub_entity = get_compound_member(type, i);
				irtype     = get_entity_type(sub_entity);
				addr       = new_d_simpleSel(dbgi, new_NoMem(), base_addr,
				                             sub_entity);
			}

			ir_initializer_t *sub_init
				= get_initializer_compound_value(initializer, i);

			create_dynamic_initializer_sub(sub_init, sub_entity, irtype, dbgi,
			                               addr);
		}
		return;
	}
	}

	panic("invalid ir_initializer");
}

static void create_dynamic_initializer(ir_initializer_t *initializer,
		dbg_info *dbgi, ir_entity *entity)
{
	ir_node *frame     = get_irg_frame(current_ir_graph);
	ir_node *base_addr = new_d_simpleSel(dbgi, new_NoMem(), frame, entity);
	ir_type *type      = get_entity_type(entity);

	create_dynamic_initializer_sub(initializer, entity, type, dbgi, base_addr);
}

static void create_local_initializer(initializer_t *initializer, dbg_info *dbgi,
                                     ir_entity *entity, type_t *type)
{
	ir_node *memory = get_store();
	ir_node *nomem  = new_NoMem();
	ir_node *frame  = get_irg_frame(current_ir_graph);
	ir_node *addr   = new_d_simpleSel(dbgi, nomem, frame, entity);

	if (initializer->kind == INITIALIZER_VALUE) {
		initializer_value_t *initializer_value = &initializer->value;

		ir_node *value = expression_to_value(initializer_value->value);
		type = skip_typeref(type);
		assign_value(dbgi, addr, type, value);
		return;
	}

	if (is_constant_initializer(initializer) == EXPR_CLASS_VARIABLE) {
		ir_initializer_t *irinitializer
			= create_ir_initializer(initializer, type);

		create_dynamic_initializer(irinitializer, dbgi, entity);
		return;
	}

	/* create a "template" entity which is copied to the entity on the stack */
	ir_entity *const init_entity
		= create_initializer_entity(dbgi, initializer, type);
	ir_node *const src_addr = new_d_Address(dbgi, init_entity);
	ir_type *const irtype   = get_ir_type(type);
	ir_cons_flags flags     = get_entity_volatility(entity) == volatility_is_volatile ?
	                          cons_volatile : cons_none;
	ir_node *const copyb    = new_d_CopyB(dbgi, memory, addr, src_addr, irtype, flags);
	set_store(copyb);
}

static void create_initializer_local_variable_entity(entity_t *entity)
{
	assert(entity->kind == ENTITY_VARIABLE);
	initializer_t *initializer = entity->variable.initializer;
	dbg_info      *dbgi        = get_dbg_info(&entity->base.pos);
	ir_entity     *irentity    = entity->variable.v.entity;
	type_t        *type        = entity->declaration.type;

	create_local_initializer(initializer, dbgi, irentity, type);
}

static void create_variable_initializer(entity_t *entity)
{
	assert(entity->kind == ENTITY_VARIABLE);
	initializer_t *initializer = entity->variable.initializer;
	if (initializer == NULL)
		return;

	declaration_kind_t declaration_kind
		= (declaration_kind_t) entity->declaration.kind;
	if (declaration_kind == DECLARATION_KIND_LOCAL_VARIABLE_ENTITY) {
		create_initializer_local_variable_entity(entity);
		return;
	}

	/* make sure values are folded even for -O0 */
	optimization_state_t state;
	save_optimization_state(&state);
	set_optimize(1);
	set_opt_constant_folding(1);

	type_t            *type = entity->declaration.type;
	type_qualifiers_t  tq   = get_type_qualifier(type, true);

	if (initializer->kind == INITIALIZER_VALUE) {
		expression_t *      value     = initializer->value.value;
		type_t       *const init_type = skip_typeref(value->base.type);

		if (is_type_complex(init_type)) {
			complex_value nodes = expression_to_complex(value);
			dbg_info     *dbgi  = get_dbg_info(&entity->base.pos);
			ir_mode      *mode  = get_complex_mode_storage(init_type);
			ir_node      *real  = create_conv(dbgi, nodes.real, mode);
			ir_node      *imag  = create_conv(dbgi, nodes.imag, mode);
			if (declaration_kind == DECLARATION_KIND_LOCAL_VARIABLE) {
				set_value(entity->variable.v.value_number, real);
				set_value(entity->variable.v.value_number+1, imag);
			} else {
				assert(declaration_kind == DECLARATION_KIND_GLOBAL_VARIABLE);
				ir_entity *irentity = entity->variable.v.entity;
				if (tq & TYPE_QUALIFIER_CONST
						&& get_entity_owner(irentity) != get_tls_type()) {
					add_entity_linkage(irentity, IR_LINKAGE_CONSTANT);
				}
				ir_initializer_t *complex_init = create_initializer_compound(2);
				ir_initializer_t *reali = create_initializer_const(real);
				set_initializer_compound_value(complex_init, 0, reali);
				ir_initializer_t *imagi = create_initializer_const(imag);
				set_initializer_compound_value(complex_init, 1, imagi);
				set_entity_initializer(irentity, complex_init);
			}
			restore_optimization_state(&state);
			return;
		} else if (!is_type_scalar(init_type)) {
			if (value->kind != EXPR_COMPOUND_LITERAL)
				panic("expected non-scalar initializer to be a compound literal");
			initializer = value->compound_literal.initializer;
			goto have_initializer;
		}

		ir_node  *      node = expression_to_value(value);
		dbg_info *const dbgi = get_dbg_info(&entity->base.pos);
		node = conv_to_storage_type(dbgi, node, init_type);

		if (declaration_kind == DECLARATION_KIND_LOCAL_VARIABLE) {
			set_value(entity->variable.v.value_number, node);
		} else {
			assert(declaration_kind == DECLARATION_KIND_GLOBAL_VARIABLE);

			ir_entity *irentity = entity->variable.v.entity;

			if (tq & TYPE_QUALIFIER_CONST
					&& get_entity_owner(irentity) != get_tls_type()) {
				add_entity_linkage(irentity, IR_LINKAGE_CONSTANT);
			}
			set_atomic_ent_value(irentity, node);
		}
	} else {
have_initializer:
		assert(declaration_kind == DECLARATION_KIND_LOCAL_VARIABLE_ENTITY ||
		       declaration_kind == DECLARATION_KIND_GLOBAL_VARIABLE);

		ir_entity        *irentity        = entity->variable.v.entity;
		ir_initializer_t *irinitializer
			= create_ir_initializer(initializer, type);

		if (tq & TYPE_QUALIFIER_CONST) {
			add_entity_linkage(irentity, IR_LINKAGE_CONSTANT);
		}
		set_entity_initializer(irentity, irinitializer);
	}

	restore_optimization_state(&state);
}

static void create_variable_length_array(entity_t *entity)
{
	assert(entity->kind == ENTITY_VARIABLE);
	assert(entity->variable.initializer == NULL);

	entity->declaration.kind    = DECLARATION_KIND_VARIABLE_LENGTH_ARRAY;
	entity->variable.v.vla_base = NULL;

	/* TODO: record VLA somewhere so we create the free node when we leave
	 * it's scope */
}

static void allocate_variable_length_array(entity_t *entity)
{
	assert(entity->kind == ENTITY_VARIABLE);
	assert(entity->variable.initializer == NULL);
	assert(currently_reachable());

	dbg_info *dbgi    = get_dbg_info(&entity->base.pos);
	type_t   *type    = entity->declaration.type;
	type_t   *el_type = type->array.element_type;

	/* make sure size_node is calculated */
	ir_node  *size  = get_type_size_node(type);
	ir_node  *mem   = get_store();
	unsigned  align = get_type_alignment(el_type);
	ir_node  *alloc = new_d_Alloc(dbgi, mem, size, align);

	ir_node  *proj_m = new_d_Proj(dbgi, alloc, mode_M, pn_Alloc_M);
	ir_node  *addr   = new_d_Proj(dbgi, alloc, mode_P_data, pn_Alloc_res);
	set_store(proj_m);

	assert(entity->declaration.kind == DECLARATION_KIND_VARIABLE_LENGTH_ARRAY);
	entity->variable.v.vla_base = addr;
}

static bool var_needs_entity(variable_t const *const var)
{
	if (var->address_taken)
		return true;
	type_t *const type = skip_typeref(var->base.type);
	return (!is_type_scalar(type) && !is_type_complex(type))
	     || type->base.qualifiers & TYPE_QUALIFIER_VOLATILE;
}

/**
 * Creates a Firm local variable from a declaration.
 */
static void create_local_variable(entity_t *entity)
{
	assert(entity->kind == ENTITY_VARIABLE);
	assert(entity->declaration.kind == DECLARATION_KIND_UNKNOWN);

	if (!var_needs_entity(&entity->variable)) {
		entity->declaration.kind        = DECLARATION_KIND_LOCAL_VARIABLE;
		entity->variable.v.value_number = next_value_number_function;
		set_irg_loc_description(current_ir_graph, next_value_number_function, entity);
		++next_value_number_function;
		if (is_type_complex(skip_typeref(entity->declaration.type)))
			++next_value_number_function;
		return;
	}

	/* is it a variable length array? */
	type_t *const type = skip_typeref(entity->declaration.type);
	if (is_type_array(type) && !type->array.size_constant) {
		create_variable_length_array(entity);
		return;
	}

	ir_type *const frame_type = get_irg_frame_type(current_ir_graph);
	create_variable_entity(entity, DECLARATION_KIND_LOCAL_VARIABLE_ENTITY, frame_type);
}

static void create_local_static_variable(entity_t *entity)
{
	assert(entity->kind == ENTITY_VARIABLE);
	assert(entity->declaration.kind == DECLARATION_KIND_UNKNOWN);

	type_t   *type           = skip_typeref(entity->declaration.type);
	ir_type  *const var_type = entity->variable.thread_local ?
		get_tls_type() : get_glob_type();
	ir_type  *const irtype   = get_ir_type(type);
	dbg_info *const dbgi     = get_dbg_info(&entity->base.pos);

	size_t l = strlen(entity->base.symbol->string);
	char   buf[l + sizeof(".%u")];
	snprintf(buf, sizeof(buf), "%s.%%u", entity->base.symbol->string);
	ident     *const id       = id_unique(buf);
	ir_entity *const irentity = new_entity(var_type, id, irtype);
	set_entity_dbg_info(irentity, dbgi);

	if (type->base.qualifiers & TYPE_QUALIFIER_VOLATILE) {
		set_entity_volatility(irentity, volatility_is_volatile);
	}

	entity->declaration.kind  = DECLARATION_KIND_GLOBAL_VARIABLE;
	entity->variable.v.entity = irentity;

	set_entity_ld_ident(irentity, id);
	set_entity_visibility(irentity, ir_visibility_local);

	if (entity->variable.initializer == NULL) {
		ir_initializer_t *null_init = get_initializer_null();
		set_entity_initializer(irentity, null_init);
	}

	PUSH_IRG(get_const_code_irg());
	create_variable_initializer(entity);
	POP_IRG();
}

static ir_node *return_statement_to_firm(return_statement_t *statement)
{
	if (!currently_reachable())
		return NULL;

	dbg_info *const dbgi = get_dbg_info(&statement->base.pos);
	type_t   *const type = skip_typeref(current_function_entity->declaration.type->function.return_type);

	ir_node *in[1];
	int in_len;
	if (is_type_void(type)) {
		/* just create the side effects, don't return anything */
		if (statement->value)
			evaluate_expression_discard_result(statement->value);
		in[0]  = NULL;
		in_len = 0;
	} else if (is_type_complex(type)) {
		if (statement->value) {
			complex_value value = expression_to_complex(statement->value);
			in[0] = complex_to_memory(dbgi, type, value);
		} else {
			in[0] = new_Unknown(mode_P_data);
		}
		in_len = 1;
	} else {
		ir_mode *const mode = get_ir_mode_storage(type);
		if (statement->value) {
			ir_node *value = expression_to_value(statement->value);
			value = conv_to_storage_type(dbgi, value, type);
			in[0] = create_conv(dbgi, value, mode);
		} else {
			in[0] = new_Unknown(mode);
		}
		in_len = 1;
	}

	ir_node *const store = get_store();
	ir_node *const ret   = new_d_Return(dbgi, store, in_len, in);

	ir_node *end_block = get_irg_end_block(current_ir_graph);
	add_immBlock_pred(end_block, ret);

	set_unreachable_now();
	return NULL;
}

static ir_node *expression_statement_to_firm(expression_statement_t *statement)
{
	if (!currently_reachable())
		return NULL;

	expression_t *expression = statement->expression;
	type_t       *type       = skip_typeref(expression->base.type);
	if (is_type_complex(type)) {
		expression_to_complex(expression);
		return NULL;
	} else {
		return expression_to_value(statement->expression);
	}
}

static ir_node *compound_statement_to_firm(compound_statement_t *compound)
{
	create_local_declarations(compound->scope.entities);

	ir_node     *result    = NULL;
	statement_t *statement = compound->statements;
	for ( ; statement != NULL; statement = statement->base.next) {
		result = statement_to_firm(statement);
	}

	return result;
}

static void create_global_variable(entity_t *entity)
{
	ir_linkage       linkage    = IR_LINKAGE_DEFAULT;
	ir_visibility    visibility = ir_visibility_external;
	storage_class_t  storage    = entity->declaration.storage_class;
	decl_modifiers_t modifiers  = entity->declaration.modifiers;
	assert(entity->kind == ENTITY_VARIABLE);

	switch (storage) {
	case STORAGE_CLASS_EXTERN: visibility = ir_visibility_external; break;
	case STORAGE_CLASS_STATIC: visibility = ir_visibility_local;    break;
	case STORAGE_CLASS_NONE:   visibility = ir_visibility_external; break;
	case STORAGE_CLASS_TYPEDEF:
	case STORAGE_CLASS_AUTO:
	case STORAGE_CLASS_REGISTER:
		panic("invalid storage class for global var");
	}

	/* "common" symbols */
	if (storage == STORAGE_CLASS_NONE
	    && entity->variable.initializer == NULL
	    && !entity->variable.thread_local
	    && (modifiers & DM_WEAK) == 0) {
		linkage |= IR_LINKAGE_MERGE;
	}

	ir_type *var_type = get_glob_type();
	if (entity->variable.thread_local) {
		var_type = get_tls_type();
	}
	create_variable_entity(entity, DECLARATION_KIND_GLOBAL_VARIABLE, var_type);
	ir_entity *irentity = entity->variable.v.entity;
	add_entity_linkage(irentity, linkage);
	set_entity_visibility(irentity, visibility);
	if (entity->variable.initializer == NULL
	    && storage != STORAGE_CLASS_EXTERN) {
		ir_initializer_t *null_init = get_initializer_null();
		set_entity_initializer(irentity, null_init);
	}
}

static void create_local_declaration(entity_t *entity)
{
	assert(is_declaration(entity));

	/* construct type */
	(void) get_ir_type(entity->declaration.type);
	if (entity->base.symbol == NULL) {
		return;
	}

	switch (entity->declaration.storage_class) {
	case STORAGE_CLASS_STATIC:
		if (entity->kind == ENTITY_FUNCTION) {
			(void)get_function_entity(entity, NULL);
		} else {
			create_local_static_variable(entity);
		}
		return;
	case STORAGE_CLASS_EXTERN:
		if (entity->kind == ENTITY_FUNCTION) {
			assert(entity->function.body == NULL);
			(void)get_function_entity(entity, NULL);
		} else {
			create_global_variable(entity);
			create_variable_initializer(entity);
		}
		return;
	case STORAGE_CLASS_NONE:
	case STORAGE_CLASS_AUTO:
	case STORAGE_CLASS_REGISTER:
		if (entity->kind == ENTITY_FUNCTION) {
			if (entity->function.body != NULL) {
				ir_type *owner = get_irg_frame_type(current_ir_graph);
				(void)get_function_entity(entity, owner);
				entity->declaration.kind = DECLARATION_KIND_INNER_FUNCTION;
				enqueue_inner_function(entity);
			} else {
				(void)get_function_entity(entity, NULL);
			}
		} else {
			create_local_variable(entity);
		}
		return;
	case STORAGE_CLASS_TYPEDEF:
		break;
	}
	panic("invalid storage class");
}

static void create_local_declarations(entity_t *e)
{
	for (; e; e = e->base.next) {
		if (is_declaration(e))
			create_local_declaration(e);
	}
}

static void initialize_local_declaration(entity_t *entity)
{
	if (entity->base.symbol == NULL)
		return;

	// no need to emit code in dead blocks
	if (entity->declaration.storage_class != STORAGE_CLASS_STATIC
			&& !currently_reachable())
		return;

	switch ((declaration_kind_t) entity->declaration.kind) {
	case DECLARATION_KIND_LOCAL_VARIABLE:
	case DECLARATION_KIND_LOCAL_VARIABLE_ENTITY:
		create_variable_initializer(entity);
		return;

	case DECLARATION_KIND_VARIABLE_LENGTH_ARRAY:
		allocate_variable_length_array(entity);
		return;

	case DECLARATION_KIND_COMPOUND_MEMBER:
	case DECLARATION_KIND_GLOBAL_VARIABLE:
	case DECLARATION_KIND_FUNCTION:
	case DECLARATION_KIND_INNER_FUNCTION:
		return;

	case DECLARATION_KIND_PARAMETER:
	case DECLARATION_KIND_PARAMETER_ENTITY:
		panic("can't initialize parameters");

	case DECLARATION_KIND_UNKNOWN:
		panic("can't initialize unknown declaration");
	}
	panic("invalid declaration kind");
}

static ir_node *declaration_statement_to_firm(declaration_statement_t *statement)
{
	entity_t *entity = statement->declarations_begin;
	if (entity == NULL)
		return NULL;

	entity_t *const last = statement->declarations_end;
	for ( ;; entity = entity->base.next) {
		if (is_declaration(entity)) {
			initialize_local_declaration(entity);
		} else if (entity->kind == ENTITY_TYPEDEF) {
			/* §6.7.7:3  Any array size expressions associated with variable length
			 * array declarators are evaluated each time the declaration of the
			 * typedef name is reached in the order of execution. */
			type_t *const type = skip_typeref(entity->typedefe.type);
			if (is_type_array(type) && type->array.is_vla)
				get_vla_size(&type->array);
		}
		if (entity == last)
			break;
	}

	return NULL;
}

static ir_node *if_statement_to_firm(if_statement_t *statement)
{
	create_local_declarations(statement->scope.entities);

	/* Create the condition. */
	jump_target true_target;
	jump_target false_target;
	init_jump_target(&true_target,  NULL);
	init_jump_target(&false_target, NULL);
	if (currently_reachable())
		expression_to_control_flow(statement->condition, &true_target, &false_target);

	jump_target exit_target;
	init_jump_target(&exit_target, NULL);

	/* Create the true statement. */
	enter_jump_target(&true_target);
	statement_to_firm(statement->true_statement);
	jump_to_target(&exit_target);

	/* Create the false statement. */
	enter_jump_target(&false_target);
	if (statement->false_statement)
		statement_to_firm(statement->false_statement);
	jump_to_target(&exit_target);

	enter_jump_target(&exit_target);
	return NULL;
}

static ir_node *do_while_statement_to_firm(do_while_statement_t *statement)
{
	create_local_declarations(statement->scope.entities);

	PUSH_BREAK(NULL);
	PUSH_CONTINUE(NULL);

	expression_t *const cond = statement->condition;
	/* Avoid an explicit body block in case of do ... while (0);. */
	if (is_constant_expression(cond) != EXPR_CLASS_VARIABLE && !fold_expression_to_bool(cond)) {
		/* do ... while (0);. */
		statement_to_firm(statement->body);
		jump_to_target(&continue_target);
		enter_jump_target(&continue_target);
		jump_to_target(&break_target);
	} else {
		jump_target body_target;
		init_jump_target(&body_target, NULL);
		jump_to_target(&body_target);
		enter_immature_jump_target(&body_target);
		keep_loop();
		statement_to_firm(statement->body);
		jump_to_target(&continue_target);
		if (enter_jump_target(&continue_target))
			expression_to_control_flow(statement->condition, &body_target, &break_target);
		enter_jump_target(&body_target);
	}
	enter_jump_target(&break_target);

	POP_CONTINUE();
	POP_BREAK();
	return NULL;
}

static ir_node *for_statement_to_firm(for_statement_t *statement)
{
	create_local_declarations(statement->scope.entities);

	if (currently_reachable()) {
		entity_t *entity = statement->scope.entities;
		for ( ; entity != NULL; entity = entity->base.next) {
			if (!is_declaration(entity))
				continue;

			initialize_local_declaration(entity);
		}

		if (statement->initialisation != NULL) {
			expression_to_value(statement->initialisation);
		}
	}

	/* Create the header block */
	jump_target header_target;
	init_jump_target(&header_target, NULL);
	jump_to_target(&header_target);
	enter_immature_jump_target(&header_target);
	keep_loop();

	expression_t *const step = statement->step;
	PUSH_BREAK(NULL);
	PUSH_CONTINUE(step ? NULL : header_target.block);

	/* Create the condition. */
	expression_t *const cond = statement->condition;
	if (cond && (is_constant_expression(cond) == EXPR_CLASS_VARIABLE || !fold_expression_to_bool(cond))) {
		jump_target body_target;
		init_jump_target(&body_target, NULL);
		expression_to_control_flow(cond, &body_target, &break_target);
		enter_jump_target(&body_target);
	}

	/* Create the loop body. */
	statement_to_firm(statement->body);
	jump_to_target(&continue_target);

	/* Create the step code. */
	if (step && enter_jump_target(&continue_target)) {
		expression_to_value(step);
		jump_to_target(&header_target);
	}

	enter_jump_target(&header_target);
	enter_jump_target(&break_target);

	POP_CONTINUE();
	POP_BREAK();
	return NULL;
}

static ir_switch_table *create_switch_table(const switch_statement_t *statement)
{
	/* determine number of cases */
	size_t n_cases = 0;
	for (case_label_statement_t *l = statement->first_case; l != NULL;
	     l = l->next) {
		/* default case */
		if (l->expression == NULL)
			continue;
		if (l->is_empty_range)
			continue;
		++n_cases;
	}

	ir_switch_table *res = ir_new_switch_table(current_ir_graph, n_cases);
	size_t           i   = 0;
	for (case_label_statement_t *l = statement->first_case; l != NULL;
	     l = l->next) {
	    if (l->expression == NULL) {
			l->pn = pn_Switch_default;
			continue;
		}
		if (l->is_empty_range)
			continue;
		ir_tarval *min = l->first_case;
		ir_tarval *max = l->last_case;
		long       pn  = (long) i+1;
		ir_switch_table_set(res, i++, min, max, pn);
		l->pn = pn;
	}
	return res;
}

static ir_node *switch_statement_to_firm(switch_statement_t *statement)
{
	dbg_info *dbgi        = get_dbg_info(&statement->base.pos);
	ir_node  *switch_node = NULL;

	if (currently_reachable()) {
		ir_node *expression = expression_to_value(statement->expression);
		ir_switch_table *table = create_switch_table(statement);
		unsigned n_outs = (unsigned)ir_switch_table_get_n_entries(table) + 1;

		switch_node = new_d_Switch(dbgi, expression, n_outs, table);
	}

	set_unreachable_now();

	PUSH_BREAK(NULL);
	ir_node *const old_switch            = current_switch;
	const bool     old_saw_default_label = saw_default_label;
	saw_default_label                    = false;
	current_switch                       = switch_node;

	statement_to_firm(statement->body);
	jump_to_target(&break_target);

	if (!saw_default_label && switch_node) {
		ir_node *proj = new_d_Proj(dbgi, switch_node, mode_X, pn_Switch_default);
		add_pred_to_jump_target(&break_target, proj);
	}

	enter_jump_target(&break_target);

	assert(current_switch == switch_node);
	current_switch    = old_switch;
	saw_default_label = old_saw_default_label;
	POP_BREAK();
	return NULL;
}

static ir_node *case_label_to_firm(const case_label_statement_t *statement)
{
	if (current_switch != NULL && !statement->is_empty_range) {
		jump_target case_target;
		init_jump_target(&case_target, NULL);

		/* Fallthrough from previous case */
		jump_to_target(&case_target);

		ir_node *const proj = new_Proj(current_switch, mode_X, statement->pn);
		add_pred_to_jump_target(&case_target, proj);
		if (statement->expression == NULL)
			saw_default_label = true;

		enter_jump_target(&case_target);
	}

	return statement_to_firm(statement->statement);
}

static ir_node *label_to_firm(const label_statement_t *statement)
{
	label_t *const label = statement->label;
	prepare_label_target(label);
	jump_to_target(&label->target);
	if (--label->n_users == 0) {
		enter_jump_target(&label->target);
	} else {
		enter_immature_jump_target(&label->target);
		keep_loop();
	}

	return statement_to_firm(statement->statement);
}

static ir_node *goto_statement_to_firm(goto_statement_t *const stmt)
{
	label_t *const label = stmt->label;
	prepare_label_target(label);
	jump_to_target(&label->target);
	if (--label->n_users == 0)
		enter_jump_target(&label->target);
	set_unreachable_now();
	return NULL;
}

static ir_node *computed_goto_to_firm(computed_goto_statement_t const *const statement)
{
	if (currently_reachable()) {
		ir_node *const op = expression_to_value(statement->expression);
		ARR_APP1(ir_node*, ijmp_ops, op);
		jump_to_target(&ijmp_target);
		set_unreachable_now();
	}
	return NULL;
}

static ir_node *asm_statement_to_firm(const asm_statement_t *statement)
{
	bool           needs_memory = statement->is_volatile;
	size_t         n_clobbers   = 0;
	asm_clobber_t *clobber      = statement->clobbers;
	for ( ; clobber != NULL; clobber = clobber->next) {
		const char *clobber_str = clobber->clobber.begin;

		if (streq(clobber_str, "memory")) {
			needs_memory = true;
			continue;
		}

		ident *id = new_id_from_str(clobber_str);
		obstack_ptr_grow(&asm_obst, id);
		++n_clobbers;
	}
	assert(obstack_object_size(&asm_obst) == n_clobbers * sizeof(ident*));
	ident **clobbers = NULL;
	if (n_clobbers > 0) {
		clobbers = obstack_finish(&asm_obst);
	}

	size_t n_inputs  = 0;
	for (const entity_t *argument = statement->inputs; argument != NULL;
	     argument = argument->base.next) {
		n_inputs++;
	}
	size_t n_outputs = 0;
	for (const entity_t *argument = statement->outputs; argument != NULL;
	     argument = argument->base.next) {
		n_outputs++;
	}

	unsigned next_pos = 0;

	ir_node *ins[n_inputs + n_outputs + 1];
	size_t   in_size = 0;

	ir_asm_constraint tmp_in_constraints[n_outputs];

	const expression_t *out_exprs[n_outputs];
	ir_node            *out_addrs[n_outputs];
	size_t              out_size = 0;

	for (const entity_t *entity = statement->outputs; entity != NULL;
	     entity = entity->base.next) {
	    const asm_argument_t *argument = &entity->asm_argument;
		unsigned pos         = next_pos++;
		ident   *constraints = new_id_from_str(argument->constraints.begin);
		if (argument->direct_write) {
			expression_t *expr   = argument->expression;
			ir_node      *addr   = expression_to_addr(expr);
			/* in+output, construct an artifical same_as constraint on the
			 * input */
			if (argument->direct_read) {
				ir_node *value = get_value_from_lvalue(expr, addr);

				char buf[64];
				snprintf(buf, sizeof(buf), "%u", (unsigned) out_size);

				ir_asm_constraint constraint;
				constraint.pos        = pos;
				constraint.constraint = new_id_from_str(buf);
				constraint.mode       = get_ir_mode_storage(expr->base.type);
				tmp_in_constraints[in_size] = constraint;
				ins[in_size] = value;

				++in_size;
			}

			out_exprs[out_size] = expr;
			out_addrs[out_size] = addr;
			++out_size;

			ir_asm_constraint constraint;
			constraint.pos        = pos;
			constraint.constraint = constraints;
			constraint.mode       = get_ir_mode_storage(expr->base.type);

			obstack_grow(&asm_obst, &constraint, sizeof(constraint));
		} else {
			/* the only case where an argument doesn't "write" is when it is
			 * a memor operand (so we really write to the pointed memory instead
			 * of the operand itself). So for the firm ASM node the operand
			 * (or rather its address) becomes an input. */
			assert(argument->indirect_write);
			needs_memory = true;

			ir_asm_constraint constraint;
			constraint.pos              = pos;
			constraint.constraint       = constraints;
			constraint.mode             = mode_M;
			tmp_in_constraints[in_size] = constraint;

			ins[in_size] = expression_to_addr(argument->expression);
			++in_size;
		}
	}
	assert(obstack_object_size(&asm_obst)
			== out_size * sizeof(ir_asm_constraint));
	ir_asm_constraint *output_constraints = obstack_finish(&asm_obst);

	obstack_grow(&asm_obst, tmp_in_constraints,
	             in_size * sizeof(tmp_in_constraints[0]));
	for (const entity_t *entity = statement->inputs; entity != NULL;
	     entity = entity->base.next) {
		const asm_argument_t *argument = &entity->asm_argument;
		ir_node *input;
		if (argument->direct_read) {
			/* we can treat this as "normal" input */
			input = expression_to_value(argument->expression);
		} else {
			assert(argument->indirect_read);
			needs_memory = true;
			input        = expression_to_addr(argument->expression);
		}

		ir_asm_constraint constraint;
		constraint.pos        = next_pos++;
		constraint.constraint = new_id_from_str(argument->constraints.begin);
		constraint.mode       = get_irn_mode(input);

		obstack_grow(&asm_obst, &constraint, sizeof(constraint));
		ins[in_size++] = input;
	}

	ir_node *mem = needs_memory ? get_store() : new_NoMem();
	assert(obstack_object_size(&asm_obst)
			== in_size * sizeof(ir_asm_constraint));
	ir_asm_constraint *input_constraints = obstack_finish(&asm_obst);

	/* create asm node */
	dbg_info *dbgi     = get_dbg_info(&statement->base.pos);
	ident    *asm_text = new_id_from_str(statement->normalized_text.begin);
	ir_node  *node     = new_d_ASM(dbgi, mem, in_size, ins, input_constraints,
	                               out_size, output_constraints,
	                               n_clobbers, clobbers, asm_text);

	if (statement->is_volatile) {
		set_irn_pinned(node, op_pin_state_pinned);
	} else {
		set_irn_pinned(node, op_pin_state_floats);
	}

	/* create output projs & connect them */
	if (needs_memory) {
		ir_node *projm = new_Proj(node, mode_M, out_size);
		set_store(projm);
	}

	for (size_t i = 0; i < out_size; ++i) {
		const expression_t *out_expr = out_exprs[i];
		long                pn       = i;
		ir_mode            *mode     = get_ir_mode_storage(out_expr->base.type);
		ir_node            *proj     = new_Proj(node, mode, pn);
		ir_node            *addr     = out_addrs[i];

		set_value_for_expression_addr(out_expr, proj, addr);
	}

	return NULL;
}

static ir_node *ms_try_statement_to_firm(ms_try_statement_t *statement)
{
	statement_to_firm(statement->try_statement);
	position_t const *const pos = &statement->base.pos;
	warningf(WARN_OTHER, pos, "structured exception handling ignored");
	return NULL;
}

static ir_node *leave_statement_to_firm(leave_statement_t *statement)
{
	errorf(&statement->base.pos, "__leave not supported yet");
	return NULL;
}

/**
 * Transform a statement.
 */
static ir_node *statement_to_firm(statement_t *const stmt)
{
#ifndef NDEBUG
	assert(!stmt->base.transformed);
	stmt->base.transformed = true;
#endif

	switch (stmt->kind) {
	case STATEMENT_ASM:           return asm_statement_to_firm(        &stmt->asms);
	case STATEMENT_CASE_LABEL:    return case_label_to_firm(           &stmt->case_label);
	case STATEMENT_COMPOUND:      return compound_statement_to_firm(   &stmt->compound);
	case STATEMENT_COMPUTED_GOTO: return computed_goto_to_firm(        &stmt->computed_goto);
	case STATEMENT_DECLARATION:   return declaration_statement_to_firm(&stmt->declaration);
	case STATEMENT_DO_WHILE:      return do_while_statement_to_firm(   &stmt->do_while);
	case STATEMENT_EMPTY:         return NULL; /* nothing */
	case STATEMENT_EXPRESSION:    return expression_statement_to_firm( &stmt->expression);
	case STATEMENT_FOR:           return for_statement_to_firm(        &stmt->fors);
	case STATEMENT_GOTO:          return goto_statement_to_firm(       &stmt->gotos);
	case STATEMENT_IF:            return if_statement_to_firm(         &stmt->ifs);
	case STATEMENT_LABEL:         return label_to_firm(                &stmt->label);
	case STATEMENT_LEAVE:         return leave_statement_to_firm(      &stmt->leave);
	case STATEMENT_MS_TRY:        return ms_try_statement_to_firm(     &stmt->ms_try);
	case STATEMENT_RETURN:        return return_statement_to_firm(     &stmt->returns);
	case STATEMENT_SWITCH:        return switch_statement_to_firm(     &stmt->switchs);

	{
		jump_target *tgt;
	case STATEMENT_BREAK:    tgt = &break_target;    goto jump;
	case STATEMENT_CONTINUE: tgt = &continue_target; goto jump;
jump:
		jump_to_target(tgt);
		set_unreachable_now();
		return NULL;
	}

	case STATEMENT_ERROR: panic("error statement");
	}
	panic("statement not implemented");
}

static int count_local_variables(const entity_t *entity,
                                 const entity_t *const last)
{
	int count = 0;
	entity_t const *const end = last != NULL ? last->base.next : NULL;
	for (; entity != end; entity = entity->base.next) {
		if ((entity->kind == ENTITY_VARIABLE || entity->kind == ENTITY_PARAMETER) &&
		    !var_needs_entity(&entity->variable)) {
		    type_t *type = skip_typeref(entity->declaration.type);
			count += is_type_complex(type) ? 2 : 1;
		}
	}
	return count;
}

static void count_local_variables_in_stmt(statement_t *stmt, void *const env)
{
	int *const count = env;

	switch (stmt->kind) {
	case STATEMENT_DECLARATION: {
		const declaration_statement_t *const decl_stmt = &stmt->declaration;
		*count += count_local_variables(decl_stmt->declarations_begin,
				decl_stmt->declarations_end);
		break;
	}

	case STATEMENT_FOR:
		*count += count_local_variables(stmt->fors.scope.entities, NULL);
		break;

	default:
		break;
	}
}

/**
 * Return the number of local (alias free) variables used by a function.
 */
static int get_function_n_local_vars(entity_t *entity)
{
	const function_t *function = &entity->function;
	int count = 0;

	/* count parameters */
	count += count_local_variables(function->parameters.entities, NULL);

	/* count local variables declared in body */
	walk_statements(function->body, count_local_variables_in_stmt, &count);
	return count;
}

/**
 * Build Firm code for the parameters of a function.
 */
static void initialize_function_parameters(entity_t *entity)
{
	assert(entity->kind == ENTITY_FUNCTION);
	ir_graph *irg             = current_ir_graph;
	ir_node  *args            = get_irg_args(irg);
	int       n               = 0;
	ir_type  *function_irtype;

	if (entity->function.need_closure) {
		/* add an extra parameter for the static link */
		entity->function.static_link = new_r_Proj(args, mode_P_data, 0);
		++n;

		/* Matze: IMO this is wrong, nested functions should have an own
		 * type and not rely on strange parameters... */
		function_irtype = create_method_type(&entity->declaration.type->function, true);
	} else {
		function_irtype = get_ir_type(entity->declaration.type);
	}

	entity_t *parameter = entity->function.parameters.entities;
	for ( ; parameter != NULL; parameter = parameter->base.next, ++n) {
		if (parameter->kind != ENTITY_PARAMETER)
			continue;

		assert(parameter->declaration.kind == DECLARATION_KIND_UNKNOWN);
		type_t *type = skip_typeref(parameter->declaration.type);

		dbg_info *const dbgi         = get_dbg_info(&parameter->base.pos);
		ir_type  *const param_irtype = get_method_param_type(function_irtype, n);
		if (var_needs_entity(&parameter->variable)) {
			ir_type   *frame_type = get_irg_frame_type(irg);
			ir_entity *param
				= new_parameter_entity(frame_type, n, param_irtype);
			set_entity_dbg_info(param, dbgi);
			parameter->declaration.kind  = DECLARATION_KIND_PARAMETER_ENTITY;
			parameter->variable.v.entity = param;
		} else if (is_type_complex(type)) {
			ir_type   *frame_type = get_irg_frame_type(irg);
			ir_entity *param
				= new_parameter_entity(frame_type, n, param_irtype);
			set_entity_dbg_info(param, dbgi);
			ir_node   *nomem = get_irg_no_mem(irg);
			ir_node   *frame = get_irg_frame(irg);
			ir_node   *addr  = new_simpleSel(nomem, frame, param);
			complex_value value = complex_deref_address(NULL, type, addr, cons_floats);

			parameter->declaration.kind        = DECLARATION_KIND_PARAMETER;
			parameter->variable.v.value_number = next_value_number_function;
			set_irg_loc_description(irg, next_value_number_function,
									parameter);
			set_irg_loc_description(irg, next_value_number_function+1,
									parameter);
			set_value(next_value_number_function, value.real);
			set_value(next_value_number_function+1, value.imag);
			next_value_number_function += 2;
		} else {
			ir_mode *param_mode = get_type_mode(param_irtype);
			long     pn         = n;
			ir_node *value      = new_rd_Proj(dbgi, args, param_mode, pn);
			value = conv_to_storage_type(dbgi, value, type);

			parameter->declaration.kind        = DECLARATION_KIND_PARAMETER;
			parameter->variable.v.value_number = next_value_number_function;
			set_irg_loc_description(irg, next_value_number_function,
									parameter);
			++next_value_number_function;

			set_value(parameter->variable.v.value_number, value);
		}
	}
}

static void add_function_pointer(ir_type *segment, ir_entity *method,
                                 const char *unique_template)
{
	ir_type   *method_type  = get_entity_type(method);
	ir_type   *ptr_type     = new_type_pointer(method_type);

	/* these entities don't really have a name but firm only allows
	 * "" in ld_ident.
	 * Note that we mustn't give these entities a name since for example
	 * Mach-O doesn't allow them. */
	ident     *ide          = id_unique(unique_template);
	ir_entity *ptr          = new_entity(segment, ide, ptr_type);
	ir_graph  *irg          = get_const_code_irg();
	ir_node   *val          = new_r_Address(irg, method);

	set_entity_ld_ident(ptr, new_id_from_chars("", 0));
	set_entity_compiler_generated(ptr, 1);
	set_entity_visibility(ptr, ir_visibility_private);
	add_entity_linkage(ptr, IR_LINKAGE_CONSTANT|IR_LINKAGE_HIDDEN_USER);
	set_atomic_ent_value(ptr, val);
}

/**
 * Create code for a function and all inner functions.
 *
 * @param entity  the function entity
 */
static void create_function(entity_t *entity)
{
	assert(entity->kind == ENTITY_FUNCTION);
	ir_entity *function_entity = get_function_entity(entity, current_outer_frame);

	if (entity->function.body == NULL)
		return;

	inner_functions     = NULL;
	current_trampolines = NULL;

	if (entity->declaration.modifiers & DM_CONSTRUCTOR) {
		ir_type *segment = get_segment_type(IR_SEGMENT_CONSTRUCTORS);
		add_function_pointer(segment, function_entity, "constructor_ptr.%u");
	}
	if (entity->declaration.modifiers & DM_DESTRUCTOR) {
		ir_type *segment = get_segment_type(IR_SEGMENT_DESTRUCTORS);
		add_function_pointer(segment, function_entity, "destructor_ptr.%u");
	}

	current_function_entity = entity;
	current_function_name   = NULL;
	current_funcsig         = NULL;

	assert(!ijmp_ops);
	assert(!ijmp_blocks);
	init_jump_target(&ijmp_target, NULL);
	ijmp_ops    = NEW_ARR_F(ir_node*, 0);
	ijmp_blocks = NEW_ARR_F(ir_node*, 0);

	int       n_local_vars = get_function_n_local_vars(entity);
	ir_graph *irg          = new_ir_graph(function_entity, n_local_vars);
	current_ir_graph = irg;

	ir_graph *old_current_function = current_function;
	current_function = irg;

	ir_entity *const old_current_vararg_entity = current_vararg_entity;
	current_vararg_entity = NULL;

	set_irn_dbg_info(get_irg_start_block(irg),
	                 get_entity_dbg_info(function_entity));

	next_value_number_function = 0;
	initialize_function_parameters(entity);
	current_static_link = entity->function.static_link;

	statement_to_firm(entity->function.body);

	ir_node *end_block = get_irg_end_block(irg);

	/* do we have a return statement yet? */
	if (currently_reachable()) {
		type_t *type = skip_typeref(entity->declaration.type);
		assert(is_type_function(type));
		type_t *const return_type = skip_typeref(type->function.return_type);

		ir_node *ret;
		if (is_type_void(return_type)) {
			ret = new_Return(get_store(), 0, NULL);
		} else {
			ir_mode *const mode = get_ir_mode_storage(return_type);

			ir_node *in[1];
			/* §5.1.2.2.3 main implicitly returns 0 */
			if (is_main(entity)) {
				in[0] = new_Const(get_mode_null(mode));
			} else {
				in[0] = new_Unknown(mode);
			}
			ret = new_Return(get_store(), 1, in);
		}
		add_immBlock_pred(end_block, ret);
	}

	if (enter_jump_target(&ijmp_target)) {
		keep_loop();
		size_t   const n    = ARR_LEN(ijmp_ops);
		ir_node *const op   = n == 1 ? ijmp_ops[0] : new_Phi(n, ijmp_ops, get_irn_mode(ijmp_ops[0]));
		ir_node *const ijmp = new_IJmp(op);
		for (size_t i = ARR_LEN(ijmp_blocks); i-- != 0;) {
			ir_node *const block = ijmp_blocks[i];
			add_immBlock_pred(block, ijmp);
			mature_immBlock(block);
		}
	}

	DEL_ARR_F(ijmp_ops);
	DEL_ARR_F(ijmp_blocks);
	ijmp_ops    = NULL;
	ijmp_blocks = NULL;

	irg_finalize_cons(irg);

	irg_assert_verify(irg);
	current_vararg_entity = old_current_vararg_entity;
	current_function      = old_current_function;

	if (current_trampolines != NULL) {
		DEL_ARR_F(current_trampolines);
		current_trampolines = NULL;
	}

	/* create inner functions if any */
	entity_t **inner = inner_functions;
	if (inner != NULL) {
		ir_type *rem_outer_frame      = current_outer_frame;
		current_outer_frame           = get_irg_frame_type(current_ir_graph);
		for (int i = ARR_LEN(inner) - 1; i >= 0; --i) {
			create_function(inner[i]);
		}
		DEL_ARR_F(inner);

		current_outer_frame      = rem_outer_frame;
	}
}

static ir_entity *get_irentity(entity_t *entity)
{
	switch (entity->kind) {
	case ENTITY_PARAMETER:
	case ENTITY_VARIABLE:        return entity->variable.v.entity;
	case ENTITY_FUNCTION:        return entity->function.irentity;
	case ENTITY_COMPOUND_MEMBER: return entity->compound_member.entity;
	case ENTITY_ASM_ARGUMENT:
	case ENTITY_CLASS:
	case ENTITY_ENUM:
	case ENTITY_ENUM_VALUE:
	case ENTITY_LABEL:
	case ENTITY_LOCAL_LABEL:
	case ENTITY_NAMESPACE:
	case ENTITY_STRUCT:
	case ENTITY_TYPEDEF:
	case ENTITY_UNION:
		return NULL;
	}
	panic("invalid entity kind");
}

static void scope_to_firm(scope_t *scope)
{
	/* first pass: create declarations */
	entity_t *entity = scope->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		if (entity->base.symbol == NULL)
			continue;

		switch (entity->kind) {
		case ENTITY_FUNCTION:
			if (entity->function.btk != BUILTIN_NONE) {
				/* builtins have no representation */
				continue;
			}
			(void)get_function_entity(entity, NULL);
			break;
		case ENTITY_VARIABLE:
			create_global_variable(entity);
			break;
		case ENTITY_NAMESPACE:
			scope_to_firm(&entity->namespacee.members);
			break;
		default:
			break;
		}
	}

	/* second pass: create code/initializers */
	entity = scope->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		if (entity->base.symbol == NULL)
			continue;

		switch (entity->kind) {
		case ENTITY_FUNCTION: {
			if (entity->function.btk != BUILTIN_NONE) {
				/* builtins have no representation */
				continue;
			}

			entity_t *alias = entity->function.alias.entity;
			if (alias != NULL) {
				ir_entity *aliased = get_irentity(alias);
				set_entity_alias(entity->function.irentity, aliased);
			} else {
				create_function(entity);
			}
			break;
		}
		case ENTITY_VARIABLE: {
			assert(entity->declaration.kind
					== DECLARATION_KIND_GLOBAL_VARIABLE);
			entity_t *alias = entity->variable.alias.entity;
			if (alias != NULL) {
				ir_entity *aliased = get_irentity(alias);
				set_entity_alias(entity->variable.v.entity, aliased);
			} else {
				current_ir_graph = get_const_code_irg();
				create_variable_initializer(entity);
			}
			break;
		}
		default:
			break;
		}
	}
}

void init_ast2firm(void)
{
	obstack_init(&asm_obst);

	ir_set_debug_retrieve(dbg_retrieve);
	ir_set_type_debug_retrieve(dbg_print_type_dbg_info);

	/* create idents for all known runtime functions */
	for (size_t i = 0; i < ARRAY_SIZE(rts_data); ++i) {
		rts_idents[i] = new_id_from_str(rts_data[i].name);
	}

	entitymap_init(&entitymap);
}

void exit_ast2firm(void)
{
	entitymap_destroy(&entitymap);
	obstack_free(&asm_obst, NULL);
}

static void global_asm_to_firm(statement_t *s)
{
	for (; s != NULL; s = s->base.next) {
		assert(s->kind == STATEMENT_ASM);

		char const *const text = s->asms.asm_text.begin;
		size_t      const size = s->asms.asm_text.size;
		ident      *const id   = new_id_from_chars(text, size);
		add_irp_asm(id);
	}
}

static const char *get_cwd(void)
{
	static char buf[1024];
	if (buf[0] == '\0') {
		return getcwd(buf, sizeof(buf));
	}
	return buf;
}

void translation_unit_to_firm(translation_unit_t *unit)
{
	if (dialect.cpp) {
		be_dwarf_set_source_language(DW_LANG_C_plus_plus);
	} else if (dialect.c99) {
		be_dwarf_set_source_language(DW_LANG_C99);
	} else if (dialect.c89) {
		be_dwarf_set_source_language(DW_LANG_C89);
	} else {
		be_dwarf_set_source_language(DW_LANG_C);
	}
	const char *cwd = get_cwd();
	if (cwd != NULL) {
		be_dwarf_set_compilation_directory(cwd);
	}

	/* initialize firm arithmetic */
	ir_set_uninitialized_local_variable_func(uninitialized_local_var);

	/* just to be sure */
	init_jump_target(&break_target,    NULL);
	init_jump_target(&continue_target, NULL);
	current_switch           = NULL;
	current_translation_unit = unit;

	scope_to_firm(&unit->scope);
	global_asm_to_firm(unit->global_asm);

	current_ir_graph         = NULL;
	current_translation_unit = NULL;
}
