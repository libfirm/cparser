/*
 * This file is part of cparser.
 * Copyright (C) 2007-2009 Matthias Braun <matze@braunis.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
#include <config.h>

#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <limits.h>

#include <libfirm/firm.h>
#include <libfirm/adt/obst.h>
#include <libfirm/be.h>

#include "ast2firm.h"

#include "adt/error.h"
#include "adt/array.h"
#include "adt/strutil.h"
#include "adt/util.h"
#include "symbol_t.h"
#include "token_t.h"
#include "type_t.h"
#include "ast_t.h"
#include "entity_t.h"
#include "parser.h"
#include "diagnostic.h"
#include "lang_features.h"
#include "types.h"
#include "type_hash.h"
#include "mangle.h"
#include "walk.h"
#include "warning.h"
#include "printer.h"
#include "entitymap_t.h"
#include "driver/firm_opt.h"

typedef struct trampoline_region trampoline_region;
struct trampoline_region {
	ir_entity        *function;    /**< The function that is called by this trampoline */
	ir_entity        *region;      /**< created region for the trampoline */
};

fp_model_t firm_fp_model = fp_model_precise;

static const backend_params *be_params;

static ir_type *ir_type_char;
static ir_type *ir_type_wchar_t;

/* architecture specific floating point arithmetic mode (if any) */
static ir_mode *mode_float_arithmetic;

/* alignment of stack parameters */
static unsigned stack_param_align;

static int        next_value_number_function;
static ir_node   *continue_label;
static ir_node   *break_label;
static ir_node   *current_switch;
static bool       saw_default_label;
static label_t  **all_labels;
static entity_t **inner_functions;
static ir_node   *ijmp_list;
static bool       constant_folding;

static const entity_t     *current_function_entity;
static ir_node            *current_function_name;
static ir_node            *current_funcsig;
static ir_graph           *current_function;
static translation_unit_t *current_translation_unit;
static trampoline_region  *current_trampolines;
static ir_type            *current_outer_frame;
static ir_node            *current_static_link;
static ir_entity          *current_vararg_entity;

static entitymap_t  entitymap;

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

static ir_type *get_ir_type_incomplete(type_t *type);

static void enqueue_inner_function(entity_t *entity)
{
	if (inner_functions == NULL)
		inner_functions = NEW_ARR_F(entity_t *, 0);
	ARR_APP1(entity_t*, inner_functions, entity);
}

static ir_node *uninitialized_local_var(ir_graph *irg, ir_mode *mode, int pos)
{
	const entity_t *entity = get_irg_loc_description(irg, pos);

	if (entity != NULL) {
		source_position_t const *const pos = &entity->base.source_position;
		warningf(WARN_UNINITIALIZED, pos, "'%N' might be used uninitialized", entity);
	}
	return new_r_Unknown(irg, mode);
}

static src_loc_t dbg_retrieve(const dbg_info *dbg)
{
	source_position_t const *const pos = (source_position_t const*)dbg;
	if (pos) {
		return (src_loc_t){ pos->input_name, pos->lineno, pos->colno };
	} else {
		return (src_loc_t){ NULL, 0, 0 };
	}
}

static dbg_info *get_dbg_info(const source_position_t *pos)
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

ir_mode *atomic_modes[ATOMIC_TYPE_LAST+1];

static ir_node *_expression_to_firm(const expression_t *expression);
static ir_node *expression_to_firm(const expression_t *expression);

static unsigned decide_modulo_shift(unsigned type_size)
{
	if (architecture_modulo_shift == 0)
		return 0;
	if (type_size < architecture_modulo_shift)
		return architecture_modulo_shift;
	return type_size;
}

static ir_mode *init_atomic_ir_mode(atomic_type_kind_t kind)
{
	unsigned flags = get_atomic_type_flags(kind);
	unsigned size  = get_atomic_type_size(kind);
	if ((flags & ATOMIC_TYPE_FLAG_FLOAT)
	    && !(flags & ATOMIC_TYPE_FLAG_COMPLEX)) {
		switch (size) {
		case 4:  return get_modeF();
		case 8:  return get_modeD();
		default: panic("unexpected kind");
		}
	} else if (flags & ATOMIC_TYPE_FLAG_INTEGER) {
		char            name[64];
		unsigned        bit_size     = size * 8;
		bool            is_signed    = (flags & ATOMIC_TYPE_FLAG_SIGNED) != 0;
		unsigned        modulo_shift = decide_modulo_shift(bit_size);

		snprintf(name, sizeof(name), "%s%u", is_signed ? "I" : "U", bit_size);
		return new_int_mode(name, irma_twos_complement, bit_size, is_signed,
		                    modulo_shift);
	}

	return NULL;
}

/**
 * Initialises the atomic modes depending on the machine size.
 */
static void init_atomic_modes(void)
{
	atomic_modes[ATOMIC_TYPE_VOID] = mode_ANY;
	for (int i = 0; i <= ATOMIC_TYPE_LAST; ++i) {
		if (atomic_modes[i] != NULL)
			continue;
		atomic_modes[i] = init_atomic_ir_mode((atomic_type_kind_t) i);
	}
}

ir_mode *get_atomic_mode(atomic_type_kind_t kind)
{
	assert(kind <= ATOMIC_TYPE_LAST);
	return atomic_modes[kind];
}

static ir_node *get_vla_size(array_type_t *const type)
{
	ir_node *size_node = type->size_node;
	if (size_node == NULL) {
		size_node = expression_to_firm(type->size_expression);
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

/**
 * Creates a Firm type for an atomic type
 */
static ir_type *create_atomic_type(atomic_type_kind_t akind, const type_t *type)
{
	ir_mode        *mode      = atomic_modes[akind];
	type_dbg_info  *dbgi      = get_type_dbg_info_(type);
	ir_type        *irtype    = new_d_type_primitive(mode, dbgi);
	il_alignment_t  alignment = get_atomic_type_alignment(akind);

	set_type_size_bytes(irtype, get_atomic_type_size(akind));
	set_type_alignment_bytes(irtype, alignment);

	return irtype;
}

/**
 * Creates a Firm type for a complex type
 */
static ir_type *create_complex_type(const atomic_type_t *type)
{
	atomic_type_kind_t  kind = type->akind;
	ir_mode            *mode = atomic_modes[kind];
	ident              *id   = get_mode_ident(mode);

	(void) id;

	/* FIXME: finish the array */
	return NULL;
}

/**
 * Creates a Firm type for an imaginary type
 */
static ir_type *create_imaginary_type(const atomic_type_t *type)
{
	return create_atomic_type(type->akind, (const type_t*)type);
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

static ir_type *create_method_type(const function_type_t *function_type, bool for_closure)
{
	type_t        *return_type  = skip_typeref(function_type->return_type);

	int            n_parameters = count_parameters(function_type)
	                               + (for_closure ? 1 : 0);
	int            n_results    = is_type_void(return_type) ? 0 : 1;
	type_dbg_info *dbgi         = get_type_dbg_info_((const type_t*) function_type);
	ir_type       *irtype       = new_d_type_method(n_parameters, n_results, dbgi);

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

static ir_type *create_pointer_type(pointer_type_t *type)
{
	type_dbg_info *dbgi         = get_type_dbg_info_((const type_t*) type);
	type_t        *points_to    = type->points_to;
	ir_type       *ir_points_to = get_ir_type_incomplete(points_to);
	ir_type       *irtype       = new_d_type_pointer(ir_points_to, dbgi);

	return irtype;
}

static ir_type *create_reference_type(reference_type_t *type)
{
	type_dbg_info *dbgi         = get_type_dbg_info_((const type_t*) type);
	type_t        *refers_to    = type->refers_to;
	ir_type       *ir_refers_to = get_ir_type_incomplete(refers_to);
	ir_type       *irtype       = new_d_type_pointer(ir_refers_to, dbgi);

	return irtype;
}

static ir_type *create_array_type(array_type_t *type)
{
	type_dbg_info *dbgi            = get_type_dbg_info_((const type_t*) type);
	type_t        *element_type    = type->element_type;
	ir_type       *ir_element_type = get_ir_type(element_type);
	ir_type       *irtype          = new_d_type_array(1, ir_element_type, dbgi);

	const int align = get_type_alignment_bytes(ir_element_type);
	set_type_alignment_bytes(irtype, align);

	if (type->size_constant) {
		int n_elements = type->size;

		set_array_bounds_int(irtype, 0, 0, n_elements);

		size_t elemsize = get_type_size_bytes(ir_element_type);
		if (elemsize % align > 0) {
			elemsize += align - (elemsize % align);
		}
		set_type_size_bytes(irtype, n_elements * elemsize);
	} else {
		set_array_lower_bound_int(irtype, 0, 0);
	}
	set_type_state(irtype, layout_fixed);

	return irtype;
}

/**
 * Return the signed integer type of size bits.
 *
 * @param size   the size
 */
static ir_type *get_signed_int_type_for_bit_size(ir_type *base_tp,
                                                 unsigned size,
												 const type_t *type)
{
	static ir_mode *s_modes[64 + 1] = {NULL, };
	ir_type *res;
	ir_mode *mode;

	if (size <= 0 || size > 64)
		return NULL;

	mode = s_modes[size];
	if (mode == NULL) {
		char name[32];

		snprintf(name, sizeof(name), "bf_I%u", size);
		mode = new_int_mode(name, irma_twos_complement, size, 1, 0);
		s_modes[size] = mode;
	}

	type_dbg_info *dbgi = get_type_dbg_info_(type);
	res                 = new_d_type_primitive(mode, dbgi);
	set_primitive_base_type(res, base_tp);

	return res;
}

/**
 * Return the unsigned integer type of size bits.
 *
 * @param size   the size
 */
static ir_type *get_unsigned_int_type_for_bit_size(ir_type *base_tp,
                                                   unsigned size,
												   const type_t *type)
{
	static ir_mode *u_modes[64 + 1] = {NULL, };
	ir_type *res;
	ir_mode *mode;

	if (size <= 0 || size > 64)
		return NULL;

	mode = u_modes[size];
	if (mode == NULL) {
		char name[32];

		snprintf(name, sizeof(name), "bf_U%u", size);
		mode = new_int_mode(name, irma_twos_complement, size, 0, 0);
		u_modes[size] = mode;
	}

	type_dbg_info *dbgi = get_type_dbg_info_(type);
	res = new_d_type_primitive(mode, dbgi);
	set_primitive_base_type(res, base_tp);

	return res;
}

static ir_type *create_bitfield_type(const entity_t *entity)
{
	assert(entity->kind == ENTITY_COMPOUND_MEMBER);
	type_t *base = skip_typeref(entity->declaration.type);
	assert(is_type_integer(base));
	ir_type *irbase = get_ir_type(base);

	unsigned bit_size = entity->compound_member.bit_size;

	if (is_type_signed(base)) {
		return get_signed_int_type_for_bit_size(irbase, bit_size, base);
	} else {
		return get_unsigned_int_type_for_bit_size(irbase, bit_size, base);
	}
}

/**
 * Construct firm type from ast struct type.
 */
static ir_type *create_compound_type(compound_type_t *const type, bool const incomplete)
{
	compound_t *compound = type->compound;

	if (compound->irtype != NULL && (compound->irtype_complete || incomplete)) {
		return compound->irtype;
	}

	bool const is_union = type->base.kind == TYPE_COMPOUND_UNION;

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

	ir_type *irtype;
	if (is_union) {
		irtype = new_type_union(id);
	} else {
		irtype = new_type_struct(id);
	}

	compound->irtype_complete = false;
	compound->irtype          = irtype;

	if (incomplete)
		return irtype;

	if (is_union) {
		layout_union_type(type);
	} else {
		layout_struct_type(type);
	}

	compound->irtype_complete = true;

	entity_t *entry = compound->members.entities;
	for ( ; entry != NULL; entry = entry->base.next) {
		if (entry->kind != ENTITY_COMPOUND_MEMBER)
			continue;

		symbol_t *symbol     = entry->base.symbol;
		type_t   *entry_type = entry->declaration.type;
		ident    *ident;
		if (symbol == NULL) {
			/* anonymous bitfield member, skip */
			if (entry->compound_member.bitfield)
				continue;
			assert(is_type_compound(entry_type));
			ident = id_unique("anon.%u");
		} else {
			ident = new_id_from_str(symbol->string);
		}

		dbg_info *dbgi       = get_dbg_info(&entry->base.source_position);

		ir_type *entry_irtype;
		if (entry->compound_member.bitfield) {
			entry_irtype = create_bitfield_type(entry);
		} else {
			entry_irtype = get_ir_type(entry_type);
		}
		ir_entity *entity = new_d_entity(irtype, ident, entry_irtype, dbgi);

		set_entity_offset(entity, entry->compound_member.offset);
		set_entity_offset_bits_remainder(entity,
		                                 entry->compound_member.bit_offset);

		assert(entry->declaration.kind == DECLARATION_KIND_UNKNOWN);
		entry->declaration.kind       = DECLARATION_KIND_COMPOUND_MEMBER;
		entry->compound_member.entity = entity;
	}

	set_type_alignment_bytes(irtype, compound->alignment);
	set_type_size_bytes(irtype, compound->size);
	set_type_state(irtype, layout_fixed);

	return irtype;
}

static ir_tarval *fold_constant_to_tarval(expression_t const *);

static void determine_enum_values(enum_type_t *const type)
{
	ir_mode   *const mode    = atomic_modes[type->base.akind];
	ir_tarval *const one     = get_mode_one(mode);
	ir_tarval *      tv_next = get_mode_null(mode);

	enum_t   *enume = type->enume;
	entity_t *entry = enume->base.next;
	for (; entry != NULL; entry = entry->base.next) {
		if (entry->kind != ENTITY_ENUM_VALUE)
			break;

		expression_t *const init = entry->enum_value.value;
		if (init != NULL) {
			tv_next = fold_constant_to_tarval(init);
		}
		assert(entry->enum_value.tv == NULL || entry->enum_value.tv == tv_next);
		entry->enum_value.tv = tv_next;
		tv_next = tarval_add(tv_next, one);
	}
}

static ir_type *create_enum_type(enum_type_t *const type)
{
	return create_atomic_type(type->base.akind, (const type_t*) type);
}

static ir_type *get_ir_type_incomplete(type_t *type)
{
	type = skip_typeref(type);

	if (type->base.firm_type != NULL) {
		return type->base.firm_type;
	}

	if (is_type_compound(type)) {
		return create_compound_type(&type->compound, true);
	} else {
		return get_ir_type(type);
	}
}

ir_type *get_ir_type(type_t *type)
{
	type = skip_typeref(type);

	if (type->base.firm_type != NULL) {
		return type->base.firm_type;
	}

	ir_type *firm_type = NULL;
	switch (type->kind) {
	case TYPE_ATOMIC:
		firm_type = create_atomic_type(type->atomic.akind, type);
		break;
	case TYPE_COMPLEX:
		firm_type = create_complex_type(&type->atomic);
		break;
	case TYPE_IMAGINARY:
		firm_type = create_imaginary_type(&type->atomic);
		break;
	case TYPE_FUNCTION:
		firm_type = create_method_type(&type->function, false);
		break;
	case TYPE_POINTER:
		firm_type = create_pointer_type(&type->pointer);
		break;
	case TYPE_REFERENCE:
		firm_type = create_reference_type(&type->reference);
		break;
	case TYPE_ARRAY:
		firm_type = create_array_type(&type->array);
		break;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		firm_type = create_compound_type(&type->compound, false);
		break;
	case TYPE_ENUM:
		firm_type = create_enum_type(&type->enumt);
		break;

	case TYPE_ERROR:
	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
		break;
	}
	if (firm_type == NULL)
		panic("unknown type found");

	type->base.firm_type = firm_type;
	return firm_type;
}

static ir_mode *get_ir_mode_storage(type_t *type)
{
	type = skip_typeref(type);

	/* Firm doesn't report a mode for arrays and structs/unions. */
	if (!is_type_scalar(type)) {
		return mode_P_data;
	}

	ir_type *const irtype = get_ir_type(type);
	ir_mode *const mode   = get_type_mode(irtype);
	assert(mode != NULL);
	return mode;
}

/*
 * get arithmetic mode for a type. This is different from get_ir_mode_storage,
 * int that it returns bigger modes for floating point on some platforms
 * (x87 internally does arithemtic with 80bits)
 */
static ir_mode *get_ir_mode_arithmetic(type_t *type)
{
	ir_mode *mode = get_ir_mode_storage(type);
	if (mode_is_float(mode) && mode_float_arithmetic != NULL) {
		return mode_float_arithmetic;
	}

	return mode;
}

/**
 * Return a node representing the size of a type.
 */
static ir_node *get_type_size_node(type_t *type)
{
	unsigned size;
	ir_mode *mode = get_ir_mode_arithmetic(type_size_t);
	type = skip_typeref(type);

	if (is_type_array(type) && type->array.is_vla) {
		ir_node *size_node = get_vla_size(&type->array);
		ir_node *elem_size = get_type_size_node(type->array.element_type);
		ir_node *real_size = new_d_Mul(NULL, size_node, elem_size, mode);
		return real_size;
	}

	size = get_type_size(type);
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

static ident *rts_idents[lengthof(rts_data)];

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
		return entity->function.statement != NULL;
	case ENTITY_PARAMETER:
	case ENTITY_COMPOUND_MEMBER:
		return false;
	case ENTITY_TYPEDEF:
	case ENTITY_ENUM:
	case ENTITY_ENUM_VALUE:
	case ENTITY_NAMESPACE:
	case ENTITY_LABEL:
	case ENTITY_LOCAL_LABEL:
		break;
	}
	panic("declaration_is_definition called on non-declaration");
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
		if (modifiers & DM_PURE) {
			set_entity_additional_properties(irentity, mtp_property_pure);
		}
		if (modifiers & DM_CONST) {
			add_entity_additional_properties(irentity, mtp_property_const);
		}
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
	bool const has_body = entity->function.statement != NULL;
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

	dbg_info *const dbgi = get_dbg_info(&entity->base.source_position);
	irentity = new_d_entity(owner_type, id, ir_type_method, dbgi);

	ident *ld_id;
	if (nested_function)
		ld_id = id_unique("inner.%u");
	else
		ld_id = create_ld_ident(entity);
	set_entity_ld_ident(irentity, ld_id);

	handle_decl_modifiers(irentity, entity);

	if (! nested_function) {
		storage_class_tag_t const storage_class
			= (storage_class_tag_t) entity->declaration.storage_class;
		if (storage_class == STORAGE_CLASS_STATIC) {
		    set_entity_visibility(irentity, ir_visibility_local);
		} else {
		    set_entity_visibility(irentity, ir_visibility_external);
		}

		bool const is_inline = entity->function.is_inline;
		if (is_inline && has_body) {
			if (((c_mode & _C99) && storage_class == STORAGE_CLASS_NONE)
			    || ((c_mode & _C99) == 0
			        && storage_class == STORAGE_CLASS_EXTERN)) {
				add_entity_linkage(irentity, IR_LINKAGE_NO_CODEGEN);
			}
		}
	} else {
		/* nested functions are always local */
		set_entity_visibility(irentity, ir_visibility_local);
	}

	/* We should check for file scope here, but as long as we compile C only
	   this is not needed. */
	if (!freestanding && !has_body) {
		/* check for a known runtime function */
		for (size_t i = 0; i < lengthof(rts_data); ++i) {
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
			if ((c_mode & rts_data[i].flags) == 0)
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

/**
 * Creates a SymConst for a given entity.
 *
 * @param dbgi    debug info
 * @param entity  the entity
 */
static ir_node *create_symconst(dbg_info *dbgi, ir_entity *entity)
{
	assert(entity != NULL);
	union symconst_symbol sym;
	sym.entity_p = entity;
	return new_d_SymConst(dbgi, mode_P, sym, symconst_addr_ent);
}

static ir_node *create_Const_from_bool(ir_mode *const mode, bool const v)
{
	return new_Const((v ? get_mode_one : get_mode_null)(mode));
}

static ir_node *create_conv_from_b(dbg_info *dbgi, ir_node *value,
                                   ir_mode *dest_mode)
{
	if (is_Const(value)) {
		return create_Const_from_bool(dest_mode, !is_Const_null(value));
	}

	ir_node *cond       = new_d_Cond(dbgi, value);
	ir_node *proj_true  = new_Proj(cond, mode_X, pn_Cond_true);
	ir_node *proj_false = new_Proj(cond, mode_X, pn_Cond_false);
	ir_node *tblock     = new_Block(1, &proj_true);
	ir_node *fblock     = new_Block(1, &proj_false);
	set_cur_block(tblock);
	ir_node *const1 = new_Const(get_mode_one(dest_mode));
	ir_node *tjump  = new_Jmp();
	set_cur_block(fblock);
	ir_node *const0 = new_Const(get_mode_null(dest_mode));
	ir_node *fjump  = new_Jmp();

	ir_node *in[2]      = { tjump, fjump };
	ir_node *mergeblock = new_Block(2, in);
	set_cur_block(mergeblock);
	ir_node *phi_in[2]  = { const1, const0 };
	ir_node *phi        = new_Phi(2, phi_in, dest_mode);
	return phi;
}

static ir_node *create_conv(dbg_info *dbgi, ir_node *value, ir_mode *dest_mode)
{
	ir_mode *value_mode = get_irn_mode(value);

	if (value_mode == dest_mode)
		return value;

	if (dest_mode == mode_b) {
		ir_node *zero = new_Const(get_mode_null(value_mode));
		ir_node *cmp  = new_d_Cmp(dbgi, value, zero, ir_relation_unordered_less_greater);
		return cmp;
	} else if (value_mode == mode_b) {
		return create_conv_from_b(dbgi, value, dest_mode);
	}

	return new_d_Conv(dbgi, value, dest_mode);
}

/**
 * Creates a SymConst node representing a string constant.
 *
 * @param src_pos    the source position of the string constant
 * @param id_prefix  a prefix for the name of the generated string constant
 * @param value      the value of the string constant
 */
static ir_node *string_to_firm(source_position_t const *const src_pos, char const *const id_prefix, string_t const *const value)
{
	size_t            const slen        = get_string_len(value) + 1;
	ir_initializer_t *const initializer = create_initializer_compound(slen);
	ir_type          *      elem_type;
	switch (value->encoding) {
	case STRING_ENCODING_CHAR: {
		elem_type = ir_type_char;

		ir_mode *const mode = get_type_mode(elem_type);
		char const    *p    = value->begin;
		for (size_t i = 0; i < slen; ++i) {
			ir_tarval        *tv  = new_tarval_from_long(*p++, mode);
			ir_initializer_t *val = create_initializer_tarval(tv);
			set_initializer_compound_value(initializer, i, val);
		}
		goto finish;
	}

	case STRING_ENCODING_WIDE: {
		elem_type = ir_type_wchar_t;

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
	ir_entity *const entity      = new_d_entity(global_type, id, type, dbgi);
	set_entity_ld_ident(   entity, id);
	set_entity_visibility( entity, ir_visibility_private);
	add_entity_linkage(    entity, IR_LINKAGE_CONSTANT);
	set_entity_initializer(entity, initializer);

	return create_symconst(dbgi, entity);
}

static bool try_create_integer(literal_expression_t *literal, type_t *type)
{
	assert(type->kind == TYPE_ATOMIC);
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

/**
 * Creates a Const node representing a constant.
 */
static ir_node *literal_to_firm(const literal_expression_t *literal)
{
	type_t     *type   = skip_typeref(literal->base.type);
	ir_mode    *mode   = get_ir_mode_storage(type);
	const char *string = literal->value.begin;
	size_t      size   = literal->value.size;
	ir_tarval  *tv;

	switch (literal->base.kind) {
	case EXPR_LITERAL_INTEGER:
		assert(literal->target_value != NULL);
		tv = literal->target_value;
		break;

	case EXPR_LITERAL_FLOATINGPOINT:
		tv = new_tarval_from_str(string, size, mode);
		break;

	case EXPR_LITERAL_BOOLEAN:
		if (string[0] == 't') {
			tv = get_mode_one(mode);
		} else {
			assert(string[0] == 'f');
	case EXPR_LITERAL_MS_NOOP:
			tv = get_mode_null(mode);
		}
		break;

	default:
		panic("Invalid literal kind found");
	}

	dbg_info *dbgi       = get_dbg_info(&literal->base.source_position);
	ir_node  *res        = new_d_Const(dbgi, tv);
	ir_mode  *mode_arith = get_ir_mode_arithmetic(type);
	return create_conv(dbgi, res, mode_arith);
}

/**
 * Creates a Const node representing a character constant.
 */
static ir_node *char_literal_to_firm(string_literal_expression_t const *literal)
{
	type_t     *type   = skip_typeref(literal->base.type);
	ir_mode    *mode   = get_ir_mode_storage(type);
	const char *string = literal->value.begin;
	size_t      size   = literal->value.size;
	ir_tarval  *tv;

	switch (literal->value.encoding) {
	case STRING_ENCODING_WIDE: {
		utf32  v = read_utf8_char(&string);
		char   buf[128];
		size_t len = snprintf(buf, sizeof(buf), UTF32_PRINTF_FORMAT, v);

		tv = new_tarval_from_str(buf, len, mode);
		break;
	}

	case STRING_ENCODING_CHAR: {
		long long int v;
		bool char_is_signed
			= get_atomic_type_flags(ATOMIC_TYPE_CHAR) & ATOMIC_TYPE_FLAG_SIGNED;
		if (size == 1 && char_is_signed) {
			v = (signed char)string[0];
		} else {
			v = 0;
			for (size_t i = 0; i < size; ++i) {
				v = (v << 8) | ((unsigned char)string[i]);
			}
		}
		char   buf[128];
		size_t len = snprintf(buf, sizeof(buf), "%lld", v);

		tv = new_tarval_from_str(buf, len, mode);
		break;
	}

	default:
		panic("Invalid literal kind found");
	}

	dbg_info *dbgi       = get_dbg_info(&literal->base.source_position);
	ir_node  *res        = new_d_Const(dbgi, tv);
	ir_mode  *mode_arith = get_ir_mode_arithmetic(type);
	return create_conv(dbgi, res, mode_arith);
}

/*
 * Allocate an area of size bytes aligned at alignment
 * at a frame type.
 */
static ir_entity *alloc_trampoline(ir_type *frame_type, int size, unsigned alignment)
{
	static unsigned area_cnt = 0;
	char buf[32];

	ir_type *tp = new_type_array(1, ir_type_char);
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
		ir_type           *frame_tp = get_irg_frame_type(irg);
		trampoline_region  reg;
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
	in[1] = create_symconst(dbgi, entity);
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
	type_t *skipped = skip_typeref(type);
	if (is_type_incomplete(skipped))
		return addr;

	ir_type *irtype = get_ir_type(skipped);
	if (is_compound_type(irtype)
	    || is_Method_type(irtype)
	    || is_Array_type(irtype)) {
		return addr;
	}

	ir_cons_flags  flags    = skipped->base.qualifiers & TYPE_QUALIFIER_VOLATILE
	                          ? cons_volatile : cons_none;
	ir_mode *const mode     = get_type_mode(irtype);
	ir_node *const memory   = get_store();
	ir_node *const load     = new_d_Load(dbgi, memory, addr, mode, flags);
	ir_node *const load_mem = new_d_Proj(dbgi, load, mode_M, pn_Load_M);
	ir_node *const load_res = new_d_Proj(dbgi, load, mode,   pn_Load_res);

	set_store(load_mem);

	ir_mode *const mode_arithmetic = get_ir_mode_arithmetic(skipped);
	return create_conv(dbgi, load_res, mode_arithmetic);
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
 * Keep all memory edges of the given block.
 */
static void keep_all_memory(ir_node *block)
{
	ir_node *old = get_cur_block();

	set_cur_block(block);
	keep_alive(get_store());
	/* TODO: keep all memory edges from restricted pointers */
	set_cur_block(old);
}

static ir_node *enum_constant_to_firm(reference_expression_t const *const ref)
{
	entity_t *entity = ref->entity;
	if (entity->enum_value.tv == NULL) {
		type_t *type = skip_typeref(entity->enum_value.enum_type);
		assert(type->kind == TYPE_ENUM);
		determine_enum_values(&type->enumt);
	}

	return new_Const(entity->enum_value.tv);
}

static ir_node *reference_addr(const reference_expression_t *ref)
{
	dbg_info *dbgi   = get_dbg_info(&ref->base.source_position);
	entity_t *entity = ref->entity;
	assert(is_declaration(entity));

	if (entity->kind == ENTITY_FUNCTION
	    && entity->function.btk != BUILTIN_NONE) {
		ir_entity *irentity = get_function_entity(entity, NULL);
		/* for gcc compatibility we have to produce (dummy) addresses for some
		 * builtins which don't have entities */
		if (irentity == NULL) {
			source_position_t const *const pos = &ref->base.source_position;
			warningf(WARN_OTHER, pos, "taking address of builtin '%N'", ref->entity);

			/* simply create a NULL pointer */
			ir_mode  *mode = get_ir_mode_arithmetic(type_void_ptr);
			ir_node  *res  = new_Const(get_mode_null(mode));

			return res;
		}
	}

	switch((declaration_kind_t) entity->declaration.kind) {
	case DECLARATION_KIND_UNKNOWN:
		break;
	case DECLARATION_KIND_PARAMETER:
	case DECLARATION_KIND_LOCAL_VARIABLE:
		/* you can store to a local variable (so we don't panic but return NULL
		 * as an indicator for no real address) */
		return NULL;
	case DECLARATION_KIND_GLOBAL_VARIABLE: {
		ir_node *const addr = create_symconst(dbgi, entity->variable.v.entity);
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
		return create_symconst(dbgi, entity->function.irentity);
	}

	case DECLARATION_KIND_INNER_FUNCTION: {
		type_t  *const type = skip_typeref(entity->declaration.type);
		ir_mode *const mode = get_ir_mode_storage(type);
		if (!entity->function.goto_to_outer && !entity->function.need_closure) {
			/* inner function not using the closure */
			return create_symconst(dbgi, entity->function.irentity);
		} else {
			/* need trampoline here */
			return create_trampoline(dbgi, mode, entity->function.irentity);
		}
	}

	case DECLARATION_KIND_COMPOUND_MEMBER:
		panic("not implemented reference type");
	}

	panic("reference to declaration with unknown type found");
}

static ir_node *reference_expression_to_firm(const reference_expression_t *ref)
{
	dbg_info *const dbgi   = get_dbg_info(&ref->base.source_position);
	entity_t *const entity = ref->entity;
	assert(is_declaration(entity));

	switch ((declaration_kind_t)entity->declaration.kind) {
	case DECLARATION_KIND_LOCAL_VARIABLE:
	case DECLARATION_KIND_PARAMETER: {
		type_t  *const type  = skip_typeref(entity->declaration.type);
		ir_mode *const mode  = get_ir_mode_storage(type);
		ir_node *const value = get_value(entity->variable.v.value_number, mode);
		return create_conv(dbgi, value, get_ir_mode_arithmetic(type));
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
	dbg_info *dbgi = get_dbg_info(&call->base.source_position);

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
		ir_node      *size     = expression_to_firm(argument);

		ir_node *store  = get_store();
		ir_node *alloca = new_d_Alloc(dbgi, store, size, get_unknown_type(),
		                              stack_alloc);
		ir_node *proj_m = new_Proj(alloca, mode_M, pn_Alloc_M);
		set_store(proj_m);
		ir_node *res    = new_Proj(alloca, mode_P_data, pn_Alloc_res);

		return res;
	}
	case BUILTIN_INF: {
		type_t    *type = function_type->function.return_type;
		ir_mode   *mode = get_ir_mode_arithmetic(type);
		ir_tarval *tv   = get_mode_infinite(mode);
		ir_node   *res  = new_d_Const(dbgi, tv);
		return res;
	}
	case BUILTIN_NAN: {
		/* Ignore string for now... */
		assert(is_type_function(function_type));
		type_t    *type = function_type->function.return_type;
		ir_mode   *mode = get_ir_mode_arithmetic(type);
		ir_tarval *tv   = get_mode_NAN(mode);
		ir_node   *res  = new_d_Const(dbgi, tv);
		return res;
	}
	case BUILTIN_EXPECT: {
		expression_t *argument = call->arguments->expression;
		return _expression_to_firm(argument);
	}
	case BUILTIN_VA_END:
		/* evaluate the argument of va_end for its side effects */
		_expression_to_firm(call->arguments->expression);
		return NULL;
	case BUILTIN_OBJECT_SIZE: {
		/* determine value of "type" */
		expression_t *type_expression = call->arguments->next->expression;
		long          type_val        = fold_constant_to_int(type_expression);
		type_t       *type            = function_type->function.return_type;
		ir_mode      *mode            = get_ir_mode_arithmetic(type);
		/* just produce a "I don't know" result */
		ir_tarval    *result          = type_val & 2 ? get_mode_null(mode) :
		                                get_mode_minus_one(mode);

		return new_d_Const(dbgi, result);
	}
	case BUILTIN_ROTL: {
		ir_node *val  = expression_to_firm(call->arguments->expression);
		ir_node *shf  = expression_to_firm(call->arguments->next->expression);
		ir_mode *mode = get_irn_mode(val);
		ir_mode *mode_uint = atomic_modes[ATOMIC_TYPE_UINT];
		return new_d_Rotl(dbgi, val, create_conv(dbgi, shf, mode_uint), mode);
	}
	case BUILTIN_ROTR: {
		ir_node *val  = expression_to_firm(call->arguments->expression);
		ir_node *shf  = expression_to_firm(call->arguments->next->expression);
		ir_mode *mode = get_irn_mode(val);
		ir_mode *mode_uint = atomic_modes[ATOMIC_TYPE_UINT];
		ir_node *c    = new_Const_long(mode_uint, get_mode_size_bits(mode));
		ir_node *sub  = new_d_Sub(dbgi, c, create_conv(dbgi, shf, mode_uint), mode_uint);
		return new_d_Rotl(dbgi, val, sub, mode);
	}
	case BUILTIN_FIRM:
		break;
	case BUILTIN_LIBC:
	case BUILTIN_LIBC_CHECK:
		panic("builtin did not produce an entity");
	}
	panic("invalid builtin found");
}

/**
 * Transform a call expression.
 * Handles some special cases, like alloca() calls, which must be resolved
 * BEFORE the inlines runs. Inlining routines calling alloca() is dangerous,
 * 176.gcc for instance might allocate 2GB instead of 256 MB if alloca is not
 * handled right...
 */
static ir_node *call_expression_to_firm(const call_expression_t *const call)
{
	dbg_info *const dbgi = get_dbg_info(&call->base.source_position);
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
		callee = expression_to_firm(function);

	type_t *type = skip_typeref(function->base.type);
	assert(is_type_pointer(type));
	pointer_type_t *pointer_type = &type->pointer;
	type_t         *points_to    = skip_typeref(pointer_type->points_to);
	assert(is_type_function(points_to));
	function_type_t *function_type = &points_to->function;

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
		new_method_type = new_d_type_method(n_parameters, n_res, tdbgi);
		set_method_calling_convention(new_method_type,
		               get_method_calling_convention(ir_method_type));
		set_method_additional_properties(new_method_type,
		               get_method_additional_properties(ir_method_type));
		set_method_variadicity(new_method_type,
		                       get_method_variadicity(ir_method_type));

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

	ir_node *in[n_parameters];

	const call_argument_t *argument = call->arguments;
	for (int n = 0; n < n_parameters; ++n) {
		expression_t *expression = argument->expression;
		ir_node      *arg_node   = expression_to_firm(expression);

		type_t *arg_type = skip_typeref(expression->base.type);
		if (!is_type_compound(arg_type)) {
			ir_mode *const mode = get_ir_mode_storage(arg_type);
			arg_node = create_conv(dbgi, arg_node, mode);
		}

		in[n] = arg_node;

		argument = argument->next;
	}

	ir_node *store;
	if (function_type->modifiers & DM_CONST) {
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
		if (! (function_type->modifiers & DM_CONST)) {
			ir_node *mem = new_Proj(node, mode_M, pn_Builtin_M);
			set_store(mem);
		}

		if (!is_type_void(return_type)) {
			assert(is_type_scalar(return_type));
			ir_mode *mode = get_ir_mode_storage(return_type);
			result = new_Proj(node, mode, pn_Builtin_max+1);
			ir_mode *mode_arith = get_ir_mode_arithmetic(return_type);
			result              = create_conv(NULL, result, mode_arith);
		}
	} else {
		node = new_d_Call(dbgi, store, callee, n_parameters, in, ir_method_type);
		if (! (function_type->modifiers & DM_CONST)) {
			ir_node *mem = new_Proj(node, mode_M, pn_Call_M);
			set_store(mem);
		}

		if (!is_type_void(return_type)) {
			ir_node *const resproj    = new_Proj(node, mode_T, pn_Call_T_result);
			ir_mode *const mode       = get_ir_mode_storage(return_type);
			result                    = new_Proj(resproj, mode, 0);
			ir_mode *const mode_arith = get_ir_mode_arithmetic(return_type);
			result                    = create_conv(NULL, result, mode_arith);
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
static ir_node *create_condition_evaluation(const expression_t *expression,
                                            ir_node *true_block,
                                            ir_node *false_block);

static void assign_value(dbg_info *dbgi, ir_node *addr, type_t *type,
                         ir_node *value)
{
	if (!is_type_compound(type)) {
		ir_mode *mode = get_ir_mode_storage(type);
		value         = create_conv(dbgi, value, mode);
	}

	ir_node *memory = get_store();

	if (is_type_scalar(type)) {
		ir_cons_flags flags = type->base.qualifiers & TYPE_QUALIFIER_VOLATILE
		                      ? cons_volatile : cons_none;
		ir_node  *store     = new_d_Store(dbgi, memory, addr, value, flags);
		ir_node  *store_mem = new_d_Proj(dbgi, store, mode_M, pn_Store_M);
		set_store(store_mem);
	} else {
		ir_type *irtype    = get_ir_type(type);
		ir_node *copyb     = new_d_CopyB(dbgi, memory, addr, value, irtype);
		ir_node *copyb_mem = new_Proj(copyb, mode_M, pn_CopyB_M);
		set_store(copyb_mem);
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
	ir_type *entity_type = get_entity_type(entity);
	ir_type *base_type   = get_primitive_base_type(entity_type);
	ir_mode *mode        = get_type_mode(base_type);
	ir_mode *mode_uint   = atomic_modes[ATOMIC_TYPE_UINT];

	value = create_conv(dbgi, value, mode);

	/* kill upper bits of value and shift to right position */
	unsigned  bitoffset  = get_entity_offset_bits_remainder(entity);
	unsigned  bitsize    = get_mode_size_bits(get_type_mode(entity_type));
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
	dbg_info *dbgi      = get_dbg_info(&expression->base.source_position);
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

	type_t  *type    = expression->base.type;
	ir_mode *resmode = get_ir_mode_arithmetic(type);
	return create_conv(dbgi, shiftr, resmode);
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
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);
	type_t   *type = skip_typeref(expression->base.type);

	if (!is_type_compound(type)) {
		ir_mode  *mode = get_ir_mode_storage(type);
		value          = create_conv(dbgi, value, mode);
	}

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
		const select_expression_t *select = &expression->select;

		construct_select_compound(select);

		entity_t *entity = select->compound_entry;
		assert(entity->kind == ENTITY_COMPOUND_MEMBER);
		if (entity->compound_member.bitfield) {
			ir_entity *irentity = entity->compound_member.entity;
			bool       set_volatile
				= select->base.type->base.qualifiers & TYPE_QUALIFIER_VOLATILE;
			value = bitfield_store_to_firm(dbgi, irentity, addr, value,
			                               set_volatile, true);
			return value;
		}
	}

	assign_value(dbgi, addr, type, value);
	return value;
}

static void set_value_for_expression(const expression_t *expression,
                                     ir_node *value)
{
	set_value_for_expression_addr(expression, value, NULL);
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
			ir_node *res  = get_value(value_number, mode);
			return create_conv(NULL, res, get_ir_mode_arithmetic(type));
		}
	}

	assert(addr != NULL);
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);

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


static ir_node *create_incdec(const unary_expression_t *expression)
{
	dbg_info *const     dbgi = get_dbg_info(&expression->base.source_position);
	const expression_t *value_expr = expression->value;
	ir_node            *addr       = expression_to_addr(value_expr);
	ir_node            *value      = get_value_from_lvalue(value_expr, addr);

	type_t  *type = skip_typeref(expression->base.type);
	ir_mode *mode = get_ir_mode_arithmetic(expression->base.type);

	ir_node *offset;
	if (is_type_pointer(type)) {
		pointer_type_t *pointer_type = &type->pointer;
		offset = get_type_size_node(pointer_type->points_to);
	} else {
		assert(is_type_arithmetic(type));
		offset = new_Const(get_mode_one(mode));
	}

	ir_node *result;
	ir_node *store_value;
	switch(expression->base.kind) {
	case EXPR_UNARY_POSTFIX_INCREMENT:
		result      = value;
		store_value = new_d_Add(dbgi, value, offset, mode);
		break;
	case EXPR_UNARY_POSTFIX_DECREMENT:
		result      = value;
		store_value = new_d_Sub(dbgi, value, offset, mode);
		break;
	case EXPR_UNARY_PREFIX_INCREMENT:
		result      = new_d_Add(dbgi, value, offset, mode);
		store_value = result;
		break;
	case EXPR_UNARY_PREFIX_DECREMENT:
		result      = new_d_Sub(dbgi, value, offset, mode);
		store_value = result;
		break;
	default:
		panic("no incdec expr in create_incdec");
	}

	set_value_for_expression_addr(value_expr, store_value, addr);

	return result;
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

static ir_relation get_relation(const expression_kind_t kind)
{
	switch(kind) {
	case EXPR_BINARY_EQUAL:         return ir_relation_equal;
	case EXPR_BINARY_ISLESSGREATER: return ir_relation_less_greater;
	case EXPR_BINARY_NOTEQUAL:      return ir_relation_unordered_less_greater;
	case EXPR_BINARY_ISLESS:
	case EXPR_BINARY_LESS:          return ir_relation_less;
	case EXPR_BINARY_ISLESSEQUAL:
	case EXPR_BINARY_LESSEQUAL:     return ir_relation_less_equal;
	case EXPR_BINARY_ISGREATER:
	case EXPR_BINARY_GREATER:       return ir_relation_greater;
	case EXPR_BINARY_ISGREATEREQUAL:
	case EXPR_BINARY_GREATEREQUAL:  return ir_relation_greater_equal;
	case EXPR_BINARY_ISUNORDERED:   return ir_relation_unordered;

	default:
		break;
	}
	panic("trying to get ir_relation from non-comparison binexpr type");
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
	if (is_local_variable(op1) && is_constant_expression(op2) == EXPR_CLASS_CONSTANT) {
		var = op1->reference.entity;
		con = op2;
	} else if (is_constant_expression(op1) == EXPR_CLASS_CONSTANT && is_local_variable(op2)) {
		relation = get_inversed_relation(relation);
		var = op2->reference.entity;
		con = op1;
	}

	if (var != NULL) {
		type_t  *const type = skip_typeref(var->declaration.type);
		ir_mode *const mode = get_ir_mode_storage(type);

		res = get_value(var->variable.v.value_number, mode);
		res = new_d_Confirm(dbi, res, expression_to_firm(con), relation);
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
static ir_node *handle_assume(dbg_info *dbi, const expression_t *expression)
{
	switch(expression->kind) {
	case EXPR_BINARY_EQUAL:
	case EXPR_BINARY_NOTEQUAL:
	case EXPR_BINARY_LESS:
	case EXPR_BINARY_LESSEQUAL:
	case EXPR_BINARY_GREATER:
	case EXPR_BINARY_GREATEREQUAL:
		return handle_assume_compare(dbi, &expression->binary);
	default:
		return NULL;
	}
}

static ir_node *create_cast(dbg_info *dbgi, ir_node *value_node,
                            type_t *from_type, type_t *type)
{
	type = skip_typeref(type);
	if (is_type_void(type)) {
		/* make sure firm type is constructed */
		(void) get_ir_type(type);
		return NULL;
	}
	if (!is_type_scalar(type)) {
		/* make sure firm type is constructed */
		(void) get_ir_type(type);
		return value_node;
	}

	from_type     = skip_typeref(from_type);
	ir_mode *mode = get_ir_mode_storage(type);
	/* check for conversion from / to __based types */
	if (is_type_pointer(type) && is_type_pointer(from_type)) {
		const variable_t *from_var = from_type->pointer.base_variable;
		const variable_t *to_var   = type->pointer.base_variable;
		if (from_var != to_var) {
			if (from_var != NULL) {
				ir_node *const addr = create_symconst(dbgi, from_var->v.entity);
				ir_node *const base = deref_address(dbgi, from_var->base.type, addr);
				value_node = new_d_Add(dbgi, value_node, base, mode);
			}
			if (to_var != NULL) {
				ir_node *const addr = create_symconst(dbgi, to_var->v.entity);
				ir_node *const base = deref_address(dbgi, to_var->base.type, addr);
				value_node = new_d_Sub(dbgi, value_node, base, mode);
			}
		}
	}

	if (is_type_atomic(type, ATOMIC_TYPE_BOOL)) {
		/* bool adjustments (we save a mode_Bu, but have to temporarily
		 * convert to mode_b so we only get a 0/1 value */
		value_node = create_conv(dbgi, value_node, mode_b);
	}

	ir_mode *mode_arith = get_ir_mode_arithmetic(type);
	ir_node *node       = create_conv(dbgi, value_node, mode);
	node                = create_conv(dbgi, node, mode_arith);

	return node;
}

static ir_node *unary_expression_to_firm(const unary_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);
	type_t   *type = skip_typeref(expression->base.type);

	const expression_t *value = expression->value;

	switch(expression->base.kind) {
	case EXPR_UNARY_TAKE_ADDRESS:
		return expression_to_addr(value);

	case EXPR_UNARY_NEGATE: {
		ir_node *value_node = expression_to_firm(value);
		ir_mode *mode       = get_ir_mode_arithmetic(type);
		return new_d_Minus(dbgi, value_node, mode);
	}
	case EXPR_UNARY_PLUS:
		return expression_to_firm(value);
	case EXPR_UNARY_BITWISE_NEGATE: {
		ir_node *value_node = expression_to_firm(value);
		ir_mode *mode       = get_ir_mode_arithmetic(type);
		return new_d_Not(dbgi, value_node, mode);
	}
	case EXPR_UNARY_NOT: {
		ir_node *value_node = _expression_to_firm(value);
		value_node          = create_conv(dbgi, value_node, mode_b);
		ir_node *res        = new_d_Not(dbgi, value_node, mode_b);
		return res;
	}
	case EXPR_UNARY_DEREFERENCE: {
		ir_node *value_node = expression_to_firm(value);
		type_t  *value_type = skip_typeref(value->base.type);
		assert(is_type_pointer(value_type));

		/* check for __based */
		const variable_t *const base_var = value_type->pointer.base_variable;
		if (base_var != NULL) {
			ir_node *const addr = create_symconst(dbgi, base_var->v.entity);
			ir_node *const base = deref_address(dbgi, base_var->base.type, addr);
			value_node = new_d_Add(dbgi, value_node, base, get_ir_mode_storage(value_type));
		}
		type_t  *points_to  = value_type->pointer.points_to;
		return deref_address(dbgi, points_to, value_node);
	}
	case EXPR_UNARY_POSTFIX_INCREMENT:
	case EXPR_UNARY_POSTFIX_DECREMENT:
	case EXPR_UNARY_PREFIX_INCREMENT:
	case EXPR_UNARY_PREFIX_DECREMENT:
		return create_incdec(expression);
	case EXPR_UNARY_CAST: {
		ir_node *value_node = expression_to_firm(value);
		type_t  *from_type  = value->base.type;
		return create_cast(dbgi, value_node, from_type, type);
	}
	case EXPR_UNARY_ASSUME:
		return handle_assume(dbgi, value);

	default:
		break;
	}
	panic("invalid UNEXPR type found");
}

/**
 * produces a 0/1 depending of the value of a mode_b node
 */
static ir_node *produce_condition_result(const expression_t *expression,
                                         ir_mode *mode, dbg_info *dbgi)
{
	ir_node *const one_block  = new_immBlock();
	ir_node *const zero_block = new_immBlock();
	create_condition_evaluation(expression, one_block, zero_block);
	mature_immBlock(one_block);
	mature_immBlock(zero_block);

	ir_node *const jmp_one  = new_rd_Jmp(dbgi, one_block);
	ir_node *const jmp_zero = new_rd_Jmp(dbgi, zero_block);
	ir_node *const in_cf[2] = { jmp_one, jmp_zero };
	ir_node *const block    = new_Block(lengthof(in_cf), in_cf);
	set_cur_block(block);

	ir_node *const one   = new_Const(get_mode_one(mode));
	ir_node *const zero  = new_Const(get_mode_null(mode));
	ir_node *const in[2] = { one, zero };
	ir_node *const val   = new_d_Phi(dbgi, lengthof(in), in, mode);

	return val;
}

static ir_node *adjust_for_pointer_arithmetic(dbg_info *dbgi,
		ir_node *value, type_t *type)
{
	ir_mode        *const mode         = get_ir_mode_arithmetic(type_ptrdiff_t);
	assert(is_type_pointer(type));
	pointer_type_t *const pointer_type = &type->pointer;
	type_t         *const points_to    = skip_typeref(pointer_type->points_to);
	ir_node        *      elem_size    = get_type_size_node(points_to);
	elem_size                          = create_conv(dbgi, elem_size, mode);
	value                              = create_conv(dbgi, value,     mode);
	ir_node        *const mul          = new_d_Mul(dbgi, value, elem_size, mode);
	return mul;
}

static ir_node *create_op(dbg_info *dbgi, const binary_expression_t *expression,
                          ir_node *left, ir_node *right)
{
	ir_mode  *mode;
	type_t   *type_left  = skip_typeref(expression->left->base.type);
	type_t   *type_right = skip_typeref(expression->right->base.type);

	expression_kind_t kind = expression->base.kind;

	switch (kind) {
	case EXPR_BINARY_SHIFTLEFT:
	case EXPR_BINARY_SHIFTRIGHT:
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:
		mode  = get_ir_mode_arithmetic(expression->base.type);
		right = create_conv(dbgi, right, atomic_modes[ATOMIC_TYPE_UINT]);
		break;

	case EXPR_BINARY_SUB:
		if (is_type_pointer(type_left) && is_type_pointer(type_right)) {
			const pointer_type_t *const ptr_type = &type_left->pointer;

			mode = get_ir_mode_arithmetic(expression->base.type);
			ir_node *const elem_size = get_type_size_node(ptr_type->points_to);
			ir_node *const conv_size = new_d_Conv(dbgi, elem_size, mode);
			ir_node *const sub       = new_d_Sub(dbgi, left, right, mode);
			ir_node *const no_mem    = new_NoMem();
			ir_node *const div       = new_d_DivRL(dbgi, no_mem, sub, conv_size,
												   mode, op_pin_state_floats);
			return new_d_Proj(dbgi, div, mode, pn_Div_res);
		}
		/* fallthrough */
	case EXPR_BINARY_SUB_ASSIGN:
		if (is_type_pointer(type_left)) {
			right = adjust_for_pointer_arithmetic(dbgi, right, type_left);
			mode  = get_ir_mode_arithmetic(type_left);
			break;
		}
		goto normal_node;

	case EXPR_BINARY_ADD:
	case EXPR_BINARY_ADD_ASSIGN:
		if (is_type_pointer(type_left)) {
			right = adjust_for_pointer_arithmetic(dbgi, right, type_left);
			mode  = get_ir_mode_arithmetic(type_left);
			break;
		} else if (is_type_pointer(type_right)) {
			left  = adjust_for_pointer_arithmetic(dbgi, left, type_right);
			mode  = get_ir_mode_arithmetic(type_right);
			break;
		}
		goto normal_node;

	default:
normal_node:
		mode = get_ir_mode_arithmetic(type_right);
		left = create_conv(dbgi, left, mode);
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
	case EXPR_BINARY_DIV:
	case EXPR_BINARY_DIV_ASSIGN: {
		ir_node *pin = new_Pin(new_NoMem());
		ir_node *op  = new_d_Div(dbgi, pin, left, right, mode,
		                         op_pin_state_floats);
		ir_node *res = new_d_Proj(dbgi, op, mode, pn_Div_res);
		return res;
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

static ir_node *create_lazy_op(const binary_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);
	type_t   *type = skip_typeref(expression->base.type);
	ir_mode  *mode = get_ir_mode_arithmetic(type);

	if (is_constant_expression(expression->left) == EXPR_CLASS_CONSTANT) {
		bool val = fold_constant_to_bool(expression->left);
		expression_kind_t ekind = expression->base.kind;
		assert(ekind == EXPR_BINARY_LOGICAL_AND || ekind == EXPR_BINARY_LOGICAL_OR);
		if (ekind == EXPR_BINARY_LOGICAL_AND) {
			if (!val) {
				return new_Const(get_mode_null(mode));
			}
		} else {
			if (val) {
				return new_Const(get_mode_one(mode));
			}
		}

		if (is_constant_expression(expression->right) == EXPR_CLASS_CONSTANT) {
			bool valr = fold_constant_to_bool(expression->right);
			return create_Const_from_bool(mode, valr);
		}

		return produce_condition_result(expression->right, mode, dbgi);
	}

	return produce_condition_result((const expression_t*) expression, mode,
	                                dbgi);
}

typedef ir_node * (*create_arithmetic_func)(dbg_info *dbgi, ir_node *left,
                                            ir_node *right, ir_mode *mode);

static ir_node *create_assign_binop(const binary_expression_t *expression)
{
	dbg_info *const     dbgi = get_dbg_info(&expression->base.source_position);
	const expression_t *left_expr = expression->left;
	type_t             *type      = skip_typeref(left_expr->base.type);
	ir_node            *right     = expression_to_firm(expression->right);
	ir_node            *left_addr = expression_to_addr(left_expr);
	ir_node            *left      = get_value_from_lvalue(left_expr, left_addr);
	ir_node            *result    = create_op(dbgi, expression, left, right);

	result = create_cast(dbgi, result, expression->right->base.type, type);

	result = set_value_for_expression_addr(left_expr, result, left_addr);

	if (!is_type_compound(type)) {
		ir_mode *mode_arithmetic = get_ir_mode_arithmetic(type);
		result = create_conv(dbgi, result, mode_arithmetic);
	}
	return result;
}

static ir_node *binary_expression_to_firm(const binary_expression_t *expression)
{
	expression_kind_t kind = expression->base.kind;

	switch(kind) {
	case EXPR_BINARY_EQUAL:
	case EXPR_BINARY_NOTEQUAL:
	case EXPR_BINARY_LESS:
	case EXPR_BINARY_LESSEQUAL:
	case EXPR_BINARY_GREATER:
	case EXPR_BINARY_GREATEREQUAL:
	case EXPR_BINARY_ISGREATER:
	case EXPR_BINARY_ISGREATEREQUAL:
	case EXPR_BINARY_ISLESS:
	case EXPR_BINARY_ISLESSEQUAL:
	case EXPR_BINARY_ISLESSGREATER:
	case EXPR_BINARY_ISUNORDERED: {
		dbg_info   *dbgi     = get_dbg_info(&expression->base.source_position);
		ir_node    *left     = expression_to_firm(expression->left);
		ir_node    *right    = expression_to_firm(expression->right);
		ir_relation relation = get_relation(kind);
		ir_node    *cmp      = new_d_Cmp(dbgi, left, right, relation);
		return cmp;
	}
	case EXPR_BINARY_ASSIGN: {
		ir_node *addr  = expression_to_addr(expression->left);
		ir_node *right = expression_to_firm(expression->right);
		ir_node *res
			= set_value_for_expression_addr(expression->left, right, addr);

		type_t  *type            = skip_typeref(expression->base.type);
		if (!is_type_compound(type)) {
			ir_mode *mode_arithmetic = get_ir_mode_arithmetic(type);
			res                      = create_conv(NULL, res, mode_arithmetic);
		}
		return res;
	}
	case EXPR_BINARY_ADD:
	case EXPR_BINARY_SUB:
	case EXPR_BINARY_MUL:
	case EXPR_BINARY_DIV:
	case EXPR_BINARY_MOD:
	case EXPR_BINARY_BITWISE_AND:
	case EXPR_BINARY_BITWISE_OR:
	case EXPR_BINARY_BITWISE_XOR:
	case EXPR_BINARY_SHIFTLEFT:
	case EXPR_BINARY_SHIFTRIGHT:
	{
		dbg_info *dbgi  = get_dbg_info(&expression->base.source_position);
		ir_node  *left  = expression_to_firm(expression->left);
		ir_node  *right = expression_to_firm(expression->right);
		return create_op(dbgi, expression, left, right);
	}
	case EXPR_BINARY_LOGICAL_AND:
	case EXPR_BINARY_LOGICAL_OR:
		return create_lazy_op(expression);
	case EXPR_BINARY_COMMA:
		/* create side effects of left side */
		(void) expression_to_firm(expression->left);
		return _expression_to_firm(expression->right);

	case EXPR_BINARY_ADD_ASSIGN:
	case EXPR_BINARY_SUB_ASSIGN:
	case EXPR_BINARY_MUL_ASSIGN:
	case EXPR_BINARY_MOD_ASSIGN:
	case EXPR_BINARY_DIV_ASSIGN:
	case EXPR_BINARY_BITWISE_AND_ASSIGN:
	case EXPR_BINARY_BITWISE_OR_ASSIGN:
	case EXPR_BINARY_BITWISE_XOR_ASSIGN:
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:
		return create_assign_binop(expression);
	default:
		panic("invalid binexpr type");
	}
}

static ir_node *array_access_addr(const array_access_expression_t *expression)
{
	dbg_info *dbgi        = get_dbg_info(&expression->base.source_position);
	ir_node  *base_addr   = expression_to_firm(expression->array_ref);
	ir_node  *offset      = expression_to_firm(expression->index);
	type_t   *ref_type    = skip_typeref(expression->array_ref->base.type);
	ir_node  *real_offset = adjust_for_pointer_arithmetic(dbgi, offset, ref_type);
	ir_node  *result      = new_d_Add(dbgi, base_addr, real_offset, mode_P_data);

	return result;
}

static ir_node *array_access_to_firm(
		const array_access_expression_t *expression)
{
	dbg_info *dbgi   = get_dbg_info(&expression->base.source_position);
	ir_node  *addr   = array_access_addr(expression);
	type_t   *type   = revert_automatic_type_conversion(
			(const expression_t*) expression);
	type             = skip_typeref(type);

	return deref_address(dbgi, type, addr);
}

static long get_offsetof_offset(const offsetof_expression_t *expression)
{
	type_t *orig_type = expression->type;
	long    offset    = 0;

	designator_t *designator = expression->designator;
	for ( ; designator != NULL; designator = designator->next) {
		type_t *type = skip_typeref(orig_type);
		/* be sure the type is constructed */
		(void) get_ir_type(type);

		if (designator->symbol != NULL) {
			assert(is_type_compound(type));
			symbol_t *symbol = designator->symbol;

			compound_t *compound = type->compound.compound;
			entity_t   *iter     = compound->members.entities;
			for (; iter->base.symbol != symbol; iter = iter->base.next) {}

			assert(iter->kind == ENTITY_COMPOUND_MEMBER);
			assert(iter->declaration.kind == DECLARATION_KIND_COMPOUND_MEMBER);
			offset += get_entity_offset(iter->compound_member.entity);

			orig_type = iter->declaration.type;
		} else {
			expression_t *array_index = designator->array_index;
			assert(designator->array_index != NULL);
			assert(is_type_array(type));

			long index         = fold_constant_to_int(array_index);
			ir_type *arr_type  = get_ir_type(type);
			ir_type *elem_type = get_array_element_type(arr_type);
			long     elem_size = get_type_size_bytes(elem_type);

			offset += index * elem_size;

			orig_type = type->array.element_type;
		}
	}

	return offset;
}

static ir_node *offsetof_to_firm(const offsetof_expression_t *expression)
{
	ir_mode   *mode   = get_ir_mode_arithmetic(expression->base.type);
	long       offset = get_offsetof_offset(expression);
	ir_tarval *tv     = new_tarval_from_long(offset, mode);
	dbg_info  *dbgi   = get_dbg_info(&expression->base.source_position);

	return new_d_Const(dbgi, tv);
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
	ir_graph *const old_current_ir_graph = current_ir_graph;
	current_ir_graph = get_const_code_irg();

	ir_initializer_t *irinitializer = create_ir_initializer(initializer, type);

	assert(current_ir_graph == get_const_code_irg());
	current_ir_graph = old_current_ir_graph;

	ident     *const id          = id_unique("initializer.%u");
	ir_type   *const irtype      = get_ir_type(type);
	ir_type   *const global_type = get_glob_type();
	ir_entity *const entity      = new_d_entity(global_type, id, irtype, dbgi);
	set_entity_ld_ident(entity, id);
	set_entity_visibility(entity, ir_visibility_private);
	add_entity_linkage(entity, IR_LINKAGE_CONSTANT);
	set_entity_initializer(entity, irinitializer);
	return entity;
}

static ir_node *compound_literal_addr(compound_literal_expression_t const *const expression)
{
	dbg_info      *dbgi        = get_dbg_info(&expression->base.source_position);
	type_t        *type        = expression->type;
	initializer_t *initializer = expression->initializer;

	if (is_constant_initializer(initializer) == EXPR_CLASS_CONSTANT) {
		ir_entity *entity = create_initializer_entity(dbgi, initializer, type);
		return create_symconst(dbgi, entity);
	} else {
		/* create an entity on the stack */
		ident   *const id     = id_unique("CompLit.%u");
		ir_type *const irtype = get_ir_type(type);
		ir_type *frame_type   = get_irg_frame_type(current_ir_graph);

		ir_entity *const entity = new_d_entity(frame_type, id, irtype, dbgi);
		set_entity_ld_ident(entity, id);

		/* create initialisation code */
		create_local_initializer(initializer, dbgi, entity, type);

		/* create a sel for the compound literal address */
		ir_node *frame = get_irg_frame(current_ir_graph);
		ir_node *sel   = new_d_simpleSel(dbgi, new_NoMem(), frame, entity);
		return sel;
	}
}

static ir_node *compound_literal_to_firm(compound_literal_expression_t const* const expr)
{
	dbg_info *const dbgi = get_dbg_info(&expr->base.source_position);
	type_t   *const type = expr->type;
	ir_node  *const addr = compound_literal_addr(expr);
	return deref_address(dbgi, type, addr);
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
		expression_to_firm(expression->tp_expression);
	}

	return get_type_size_node(type);
}

static entity_t *get_expression_entity(const expression_t *expression)
{
	if (expression->kind != EXPR_REFERENCE)
		return NULL;

	return expression->reference.entity;
}

static unsigned get_cparser_entity_alignment(const entity_t *entity)
{
	switch(entity->kind) {
	case DECLARATION_KIND_CASES:
		return entity->declaration.alignment;
	case ENTITY_STRUCT:
	case ENTITY_UNION:
		return entity->compound.alignment;
	case ENTITY_TYPEDEF:
		return entity->typedefe.alignment;
	default:
		break;
	}
	return 0;
}

/**
 * Transform an alignof expression into Firm code.
 */
static ir_node *alignof_to_firm(const typeprop_expression_t *expression)
{
	unsigned alignment = 0;

	const expression_t *tp_expression = expression->tp_expression;
	if (tp_expression != NULL) {
		entity_t *entity = get_expression_entity(tp_expression);
		if (entity != NULL) {
			alignment = get_cparser_entity_alignment(entity);
		}
	}

	if (alignment == 0) {
		type_t *type = expression->type;
		alignment = get_type_alignment(type);
	}

	dbg_info  *dbgi = get_dbg_info(&expression->base.source_position);
	ir_mode   *mode = get_ir_mode_arithmetic(expression->base.type);
	ir_tarval *tv   = new_tarval_from_long(alignment, mode);
	return new_d_Const(dbgi, tv);
}

static void init_ir_types(void);

static ir_tarval *fold_constant_to_tarval(const expression_t *expression)
{
	assert(is_constant_expression(expression) == EXPR_CLASS_CONSTANT);

	bool constant_folding_old = constant_folding;
	constant_folding = true;
	int old_optimize         = get_optimize();
	int old_constant_folding = get_opt_constant_folding();
	set_optimize(1);
	set_opt_constant_folding(1);

	init_ir_types();

	ir_graph *old_current_ir_graph = current_ir_graph;
	current_ir_graph = get_const_code_irg();

	ir_node *const cnst = _expression_to_firm(expression);

	current_ir_graph = old_current_ir_graph;
	set_optimize(old_optimize);
	set_opt_constant_folding(old_constant_folding);

	if (!is_Const(cnst)) {
		panic("couldn't fold constant");
	}

	constant_folding = constant_folding_old;

	ir_tarval *const tv   = get_Const_tarval(cnst);
	ir_mode   *const mode = get_ir_mode_arithmetic(skip_typeref(expression->base.type));
	return tarval_convert_to(tv, mode);
}

/* this function is only used in parser.c, but it relies on libfirm functionality */
bool constant_is_negative(const expression_t *expression)
{
	ir_tarval *tv = fold_constant_to_tarval(expression);
	return tarval_is_negative(tv);
}

long fold_constant_to_int(const expression_t *expression)
{
	ir_tarval *tv = fold_constant_to_tarval(expression);
	if (!tarval_is_long(tv)) {
		panic("result of constant folding is not integer");
	}

	return get_tarval_long(tv);
}

bool fold_constant_to_bool(const expression_t *expression)
{
	ir_tarval *tv = fold_constant_to_tarval(expression);
	return !tarval_is_null(tv);
}

static ir_node *conditional_to_firm(const conditional_expression_t *expression)
{
	dbg_info *const dbgi = get_dbg_info(&expression->base.source_position);

	/* first try to fold a constant condition */
	if (is_constant_expression(expression->condition) == EXPR_CLASS_CONSTANT) {
		bool val = fold_constant_to_bool(expression->condition);
		if (val) {
			expression_t *true_expression = expression->true_expression;
			if (true_expression == NULL)
				true_expression = expression->condition;
			return expression_to_firm(true_expression);
		} else {
			return expression_to_firm(expression->false_expression);
		}
	}

	ir_node *const true_block  = new_immBlock();
	ir_node *const false_block = new_immBlock();
	ir_node *const cond_expr   = create_condition_evaluation(expression->condition, true_block, false_block);
	mature_immBlock(true_block);
	mature_immBlock(false_block);

	set_cur_block(true_block);
	ir_node *true_val;
	if (expression->true_expression != NULL) {
		true_val = expression_to_firm(expression->true_expression);
	} else if (cond_expr != NULL && get_irn_mode(cond_expr) != mode_b) {
		true_val = cond_expr;
	} else {
		/* Condition ended with a short circuit (&&, ||, !) operation or a
		 * comparison.  Generate a "1" as value for the true branch. */
		true_val = new_Const(get_mode_one(mode_Is));
	}
	ir_node *const true_jmp = new_d_Jmp(dbgi);

	set_cur_block(false_block);
	ir_node *const false_val = expression_to_firm(expression->false_expression);
	ir_node *const false_jmp = new_d_Jmp(dbgi);

	/* create the common block */
	ir_node *const in_cf[2] = { true_jmp, false_jmp };
	ir_node *const block    = new_Block(lengthof(in_cf), in_cf);
	set_cur_block(block);

	/* TODO improve static semantics, so either both or no values are NULL */
	if (true_val == NULL || false_val == NULL)
		return NULL;

	ir_node *const in[2] = { true_val, false_val };
	type_t  *const type  = skip_typeref(expression->base.type);
	ir_mode *const mode  = get_ir_mode_arithmetic(type);
	ir_node *const val   = new_d_Phi(dbgi, lengthof(in), in, mode);

	return val;
}

/**
 * Returns an IR-node representing the address of a field.
 */
static ir_node *select_addr(const select_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);

	construct_select_compound(expression);

	ir_node *compound_addr = expression_to_firm(expression->compound);

	entity_t *entry = expression->compound_entry;
	assert(entry->kind == ENTITY_COMPOUND_MEMBER);
	assert(entry->declaration.kind == DECLARATION_KIND_COMPOUND_MEMBER);

	if (constant_folding) {
		ir_mode *mode      = get_irn_mode(compound_addr);
		ir_mode *mode_uint = get_reference_mode_unsigned_eq(mode);
		ir_node *ofs       = new_Const_long(mode_uint, entry->compound_member.offset);
		return new_d_Add(dbgi, compound_addr, ofs, mode);
	} else {
		ir_entity *irentity = entry->compound_member.entity;
		assert(irentity != NULL);
		return new_d_simpleSel(dbgi, new_NoMem(), compound_addr, irentity);
	}
}

static ir_node *select_to_firm(const select_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);
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

/* Values returned by __builtin_classify_type. */
typedef enum gcc_type_class
{
	no_type_class = -1,
	void_type_class,
	integer_type_class,
	char_type_class,
	enumeral_type_class,
	boolean_type_class,
	pointer_type_class,
	reference_type_class,
	offset_type_class,
	real_type_class,
	complex_type_class,
	function_type_class,
	method_type_class,
	record_type_class,
	union_type_class,
	array_type_class,
	string_type_class,
	set_type_class,
	file_type_class,
	lang_type_class
} gcc_type_class;

static ir_node *classify_type_to_firm(const classify_type_expression_t *const expr)
{
	type_t *type = expr->type_expression->base.type;

	/* FIXME gcc returns different values depending on whether compiling C or C++
	 * e.g. int x[10] is pointer_type_class in C, but array_type_class in C++ */
	gcc_type_class tc;
	for (;;) {
		type = skip_typeref(type);
		switch (type->kind) {
			case TYPE_ATOMIC: {
				const atomic_type_t *const atomic_type = &type->atomic;
				switch (atomic_type->akind) {
					/* gcc cannot do that */
					case ATOMIC_TYPE_VOID:
						tc = void_type_class;
						goto make_const;

					case ATOMIC_TYPE_WCHAR_T:   /* gcc handles this as integer */
					case ATOMIC_TYPE_CHAR:      /* gcc handles this as integer */
					case ATOMIC_TYPE_SCHAR:     /* gcc handles this as integer */
					case ATOMIC_TYPE_UCHAR:     /* gcc handles this as integer */
					case ATOMIC_TYPE_SHORT:
					case ATOMIC_TYPE_USHORT:
					case ATOMIC_TYPE_INT:
					case ATOMIC_TYPE_UINT:
					case ATOMIC_TYPE_LONG:
					case ATOMIC_TYPE_ULONG:
					case ATOMIC_TYPE_LONGLONG:
					case ATOMIC_TYPE_ULONGLONG:
					case ATOMIC_TYPE_BOOL:      /* gcc handles this as integer */
						tc = integer_type_class;
						goto make_const;

					case ATOMIC_TYPE_FLOAT:
					case ATOMIC_TYPE_DOUBLE:
					case ATOMIC_TYPE_LONG_DOUBLE:
						tc = real_type_class;
						goto make_const;
				}
				panic("Unexpected atomic type in classify_type_to_firm().");
			}

			case TYPE_COMPLEX:         tc = complex_type_class; goto make_const;
			case TYPE_IMAGINARY:       tc = complex_type_class; goto make_const;
			case TYPE_ARRAY:           /* gcc handles this as pointer */
			case TYPE_FUNCTION:        /* gcc handles this as pointer */
			case TYPE_POINTER:         tc = pointer_type_class; goto make_const;
			case TYPE_COMPOUND_STRUCT: tc = record_type_class;  goto make_const;
			case TYPE_COMPOUND_UNION:  tc = union_type_class;   goto make_const;

			/* gcc handles this as integer */
			case TYPE_ENUM:            tc = integer_type_class; goto make_const;

			/* gcc classifies the referenced type */
			case TYPE_REFERENCE: type = type->reference.refers_to; continue;

			/* typedef/typeof should be skipped already */
			case TYPE_TYPEDEF:
			case TYPE_TYPEOF:
			case TYPE_ERROR:
				break;
		}
		panic("unexpected TYPE classify_type_to_firm().");
	}

make_const:;
	dbg_info  *const dbgi = get_dbg_info(&expr->base.source_position);
	ir_mode   *const mode = atomic_modes[ATOMIC_TYPE_INT];
	ir_tarval *const tv   = new_tarval_from_long(tc, mode);
	return new_d_Const(dbgi, tv);
}

static ir_node *function_name_to_firm(
		const funcname_expression_t *const expr)
{
	switch(expr->kind) {
	case FUNCNAME_FUNCTION:
	case FUNCNAME_PRETTY_FUNCTION:
	case FUNCNAME_FUNCDNAME:
		if (current_function_name == NULL) {
			source_position_t const *const src_pos = &expr->base.source_position;
			char              const *const name    = current_function_entity->base.symbol->string;
			string_t                 const string  = { name, strlen(name), STRING_ENCODING_CHAR };
			current_function_name = string_to_firm(src_pos, "__func__.%u", &string);
		}
		return current_function_name;
	case FUNCNAME_FUNCSIG:
		if (current_funcsig == NULL) {
			source_position_t const *const src_pos = &expr->base.source_position;
			ir_entity               *const ent     = get_irg_entity(current_ir_graph);
			char              const *const name    = get_entity_ld_name(ent);
			string_t                 const string  = { name, strlen(name), STRING_ENCODING_CHAR };
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
	dbg_info *const dbgi    = get_dbg_info(&expr->base.source_position);
	ir_node  *const no_mem  = new_NoMem();
	ir_node  *const arg_sel = new_d_simpleSel(dbgi, no_mem, frame, param_ent);

	set_value_for_expression(expr->ap, arg_sel);

	return NULL;
}

static ir_node *va_arg_expression_to_firm(const va_arg_expression_t *const expr)
{
	type_t       *const type    = expr->base.type;
	expression_t *const ap_expr = expr->ap;
	ir_node      *const ap_addr = expression_to_addr(ap_expr);
	ir_node      *const ap      = get_value_from_lvalue(ap_expr, ap_addr);
	dbg_info     *const dbgi    = get_dbg_info(&expr->base.source_position);
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
	ir_node *const src = expression_to_firm(expr->src);
	set_value_for_expression(expr->dst, src);
	return NULL;
}

static ir_node *dereference_addr(const unary_expression_t *const expression)
{
	assert(expression->base.kind == EXPR_UNARY_DEREFERENCE);
	return expression_to_firm(expression->value);
}

/**
 * Returns a IR-node representing an lvalue of the given expression.
 */
static ir_node *expression_to_addr(const expression_t *expression)
{
	switch(expression->kind) {
	case EXPR_ARRAY_ACCESS:
		return array_access_addr(&expression->array_access);
	case EXPR_CALL:
		return call_expression_to_firm(&expression->call);
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

static ir_node *builtin_constant_to_firm(
		const builtin_constant_expression_t *expression)
{
	ir_mode *const mode = get_ir_mode_arithmetic(expression->base.type);
	bool     const v    = is_constant_expression(expression->value) == EXPR_CLASS_CONSTANT;
	return create_Const_from_bool(mode, v);
}

static ir_node *builtin_types_compatible_to_firm(
		const builtin_types_compatible_expression_t *expression)
{
	type_t  *const left  = get_unqualified_type(skip_typeref(expression->left));
	type_t  *const right = get_unqualified_type(skip_typeref(expression->right));
	bool     const value = types_compatible(left, right);
	ir_mode *const mode  = get_ir_mode_arithmetic(expression->base.type);
	return create_Const_from_bool(mode, value);
}

static ir_node *get_label_block(label_t *label)
{
	if (label->block != NULL)
		return label->block;

	/* beware: might be called from create initializer with current_ir_graph
	 * set to const_code_irg. */
	ir_graph *rem    = current_ir_graph;
	current_ir_graph = current_function;

	ir_node *block = new_immBlock();

	label->block = block;

	ARR_APP1(label_t *, all_labels, label);

	current_ir_graph = rem;
	return block;
}

/**
 * Pointer to a label.  This is used for the
 * GNU address-of-label extension.
 */
static ir_node *label_address_to_firm(const label_address_expression_t *label)
{
	dbg_info  *dbgi   = get_dbg_info(&label->base.source_position);
	ir_node   *block  = get_label_block(label->label);
	ir_entity *entity = create_Block_entity(block);

	symconst_symbol value;
	value.entity_p = entity;
	return new_d_SymConst(dbgi, mode_P_code, value, symconst_addr_ent);
}

/**
 * creates firm nodes for an expression. The difference between this function
 * and expression_to_firm is, that this version might produce mode_b nodes
 * instead of mode_Is.
 */
static ir_node *_expression_to_firm(expression_t const *const expr)
{
#ifndef NDEBUG
	if (!constant_folding) {
		assert(!expr->base.transformed);
		((expression_t*)expr)->base.transformed = true;
	}
#endif

	switch (expr->kind) {
	case EXPR_ALIGNOF:                    return alignof_to_firm(                 &expr->typeprop);
	case EXPR_ARRAY_ACCESS:               return array_access_to_firm(            &expr->array_access);
	case EXPR_BINARY_CASES:               return binary_expression_to_firm(       &expr->binary);
	case EXPR_BUILTIN_CONSTANT_P:         return builtin_constant_to_firm(        &expr->builtin_constant);
	case EXPR_BUILTIN_TYPES_COMPATIBLE_P: return builtin_types_compatible_to_firm(&expr->builtin_types_compatible);
	case EXPR_CALL:                       return call_expression_to_firm(         &expr->call);
	case EXPR_CLASSIFY_TYPE:              return classify_type_to_firm(           &expr->classify_type);
	case EXPR_COMPOUND_LITERAL:           return compound_literal_to_firm(        &expr->compound_literal);
	case EXPR_CONDITIONAL:                return conditional_to_firm(             &expr->conditional);
	case EXPR_FUNCNAME:                   return function_name_to_firm(           &expr->funcname);
	case EXPR_LABEL_ADDRESS:              return label_address_to_firm(           &expr->label_address);
	case EXPR_LITERAL_CASES:              return literal_to_firm(                 &expr->literal);
	case EXPR_LITERAL_CHARACTER:          return char_literal_to_firm(            &expr->string_literal);
	case EXPR_OFFSETOF:                   return offsetof_to_firm(                &expr->offsetofe);
	case EXPR_REFERENCE:                  return reference_expression_to_firm(    &expr->reference);
	case EXPR_ENUM_CONSTANT:              return enum_constant_to_firm(           &expr->reference);
	case EXPR_SELECT:                     return select_to_firm(                  &expr->select);
	case EXPR_SIZEOF:                     return sizeof_to_firm(                  &expr->typeprop);
	case EXPR_STATEMENT:                  return statement_expression_to_firm(    &expr->statement);
	case EXPR_UNARY_CASES:                return unary_expression_to_firm(        &expr->unary);
	case EXPR_VA_ARG:                     return va_arg_expression_to_firm(       &expr->va_arge);
	case EXPR_VA_COPY:                    return va_copy_expression_to_firm(      &expr->va_copye);
	case EXPR_VA_START:                   return va_start_expression_to_firm(     &expr->va_starte);

	case EXPR_STRING_LITERAL: return string_to_firm(&expr->base.source_position, "str.%u", &expr->string_literal.value);

	case EXPR_ERROR: break;
	}
	panic("invalid expression found");
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

static bool produces_mode_b(const expression_t *expression)
{
	switch (expression->kind) {
	case EXPR_BINARY_EQUAL:
	case EXPR_BINARY_NOTEQUAL:
	case EXPR_BINARY_LESS:
	case EXPR_BINARY_LESSEQUAL:
	case EXPR_BINARY_GREATER:
	case EXPR_BINARY_GREATEREQUAL:
	case EXPR_BINARY_ISGREATER:
	case EXPR_BINARY_ISGREATEREQUAL:
	case EXPR_BINARY_ISLESS:
	case EXPR_BINARY_ISLESSEQUAL:
	case EXPR_BINARY_ISLESSGREATER:
	case EXPR_BINARY_ISUNORDERED:
	case EXPR_UNARY_NOT:
		return true;

	case EXPR_CALL:
		if (is_builtin_expect(expression)) {
			expression_t *argument = expression->call.arguments->expression;
			return produces_mode_b(argument);
		}
		return false;
	case EXPR_BINARY_COMMA:
		return produces_mode_b(expression->binary.right);

	default:
		return false;
	}
}

static ir_node *expression_to_firm(const expression_t *expression)
{
	if (!produces_mode_b(expression)) {
		ir_node *res = _expression_to_firm(expression);
		assert(res == NULL || get_irn_mode(res) != mode_b);
		return res;
	}

	if (is_constant_expression(expression) == EXPR_CLASS_CONSTANT) {
		return new_Const(fold_constant_to_tarval(expression));
	}

	/* we have to produce a 0/1 from the mode_b expression */
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);
	ir_mode  *mode = get_ir_mode_arithmetic(expression->base.type);
	return produce_condition_result(expression, mode, dbgi);
}

/**
 * create a short-circuit expression evaluation that tries to construct
 * efficient control flow structures for &&, || and ! expressions
 */
static ir_node *create_condition_evaluation(const expression_t *expression,
                                            ir_node *true_block,
                                            ir_node *false_block)
{
	switch(expression->kind) {
	case EXPR_UNARY_NOT: {
		const unary_expression_t *unary_expression = &expression->unary;
		create_condition_evaluation(unary_expression->value, false_block,
		                            true_block);
		return NULL;
	}
	case EXPR_BINARY_LOGICAL_AND: {
		const binary_expression_t *binary_expression = &expression->binary;

		ir_node *extra_block = new_immBlock();
		create_condition_evaluation(binary_expression->left, extra_block,
		                            false_block);
		mature_immBlock(extra_block);
		set_cur_block(extra_block);
		create_condition_evaluation(binary_expression->right, true_block,
		                            false_block);
		return NULL;
	}
	case EXPR_BINARY_LOGICAL_OR: {
		const binary_expression_t *binary_expression = &expression->binary;

		ir_node *extra_block = new_immBlock();
		create_condition_evaluation(binary_expression->left, true_block,
		                            extra_block);
		mature_immBlock(extra_block);
		set_cur_block(extra_block);
		create_condition_evaluation(binary_expression->right, true_block,
		                            false_block);
		return NULL;
	}
	default:
		break;
	}

	dbg_info *dbgi       = get_dbg_info(&expression->base.source_position);
	ir_node  *cond_expr  = _expression_to_firm(expression);
	ir_node  *condition  = create_conv(dbgi, cond_expr, mode_b);
	ir_node  *cond       = new_d_Cond(dbgi, condition);
	ir_node  *true_proj  = new_d_Proj(dbgi, cond, mode_X, pn_Cond_true);
	ir_node  *false_proj = new_d_Proj(dbgi, cond, mode_X, pn_Cond_false);

	/* set branch prediction info based on __builtin_expect */
	if (is_builtin_expect(expression) && is_Cond(cond)) {
		call_argument_t *argument = expression->call.arguments->next;
		if (is_constant_expression(argument->expression) == EXPR_CLASS_CONSTANT) {
			bool               const cnst = fold_constant_to_bool(argument->expression);
			cond_jmp_predicate const pred = cnst ? COND_JMP_PRED_TRUE : COND_JMP_PRED_FALSE;
			set_Cond_jmp_pred(cond, pred);
		}
	}

	add_immBlock_pred(true_block, true_proj);
	add_immBlock_pred(false_block, false_proj);

	set_unreachable_now();
	return cond_expr;
}

static void create_variable_entity(entity_t *variable,
                                   declaration_kind_t declaration_kind,
                                   ir_type *parent_type)
{
	assert(variable->kind == ENTITY_VARIABLE);
	type_t    *type = skip_typeref(variable->declaration.type);

	ident     *const id        = new_id_from_str(variable->base.symbol->string);
	ir_type   *const irtype    = get_ir_type(type);
	dbg_info  *const dbgi      = get_dbg_info(&variable->base.source_position);
	ir_entity *const irentity  = new_d_entity(parent_type, id, irtype, dbgi);
	unsigned         alignment = variable->declaration.alignment;

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
		++n_members;
	}

	return n_members;
}

static ir_initializer_t *get_initializer_entry(type_path_t *path)
{
	type_t *orig_top_type = path->top_type;
	type_t *top_type      = skip_typeref(orig_top_type);

	assert(is_type_compound(top_type) || is_type_array(top_type));

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
			size_t    index  = 0;
			symbol_t *symbol = designator->symbol;

			compound_t *compound = type->compound.compound;
			entity_t   *iter     = compound->members.entities;
			for (; iter->base.symbol != symbol; iter = iter->base.next, ++index) {}
			assert(iter->kind == ENTITY_COMPOUND_MEMBER);

			/* revert previous initialisations of other union elements */
			if (type->kind == TYPE_COMPOUND_UNION) {
				ir_initializer_t *initializer = top->initializer;
				if (initializer != NULL
					&& get_initializer_kind(initializer) == IR_INITIALIZER_COMPOUND) {
					/* are we writing to a new element? */
					ir_initializer_t *oldi
						= get_initializer_compound_value(initializer, index);
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
			top->index          = index;
			orig_type           = iter->declaration.type;
		} else {
			expression_t *array_index = designator->array_index;
			assert(is_type_array(type));

			long index = fold_constant_to_int(array_index);
			assert(0 <= index && (!type->array.size_constant || (size_t)index < type->array.size));

			top->type  = orig_type;
			top->index = (size_t) index;
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
		panic("invalid initializer in ast2firm (excessive elements)");
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
	if (is_type_compound(initializer->value->base.type)) {
		panic("initializer creation for compounds not implemented yet");
	}
	type_t       *type = initializer->value->base.type;
	expression_t *expr = initializer->value;
	ir_node *value = expression_to_firm(expr);
	ir_mode *mode  = get_ir_mode_storage(type);
	value          = create_conv(NULL, value, mode);
	return create_initializer_const(value);
}

/** test wether type can be initialized by a string constant */
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
			/* we might have to descend into types until we're at a scalar
			 * type */
			while(true) {
				type_t *orig_top_type = path.top_type;
				type_t *top_type      = skip_typeref(orig_top_type);

				if (is_type_scalar(top_type))
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
		for (size_t i = 0; i != arr_len; ++i) {
			char              const c      = i < str_len ? *p++ : 0;
			ir_tarval        *const tv     = new_tarval_from_long(c, mode);
			ir_initializer_t *const tvinit = create_initializer_tarval(tv);
			set_initializer_compound_value(irinit, i, tvinit);
		}
		break;

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
	switch(initializer->kind) {
		case INITIALIZER_STRING:
			return create_ir_initializer_string(initializer, type);

		case INITIALIZER_LIST:
			return create_ir_initializer_list(&initializer->list, type);

		case INITIALIZER_VALUE:
			return create_ir_initializer_value(&initializer->value);

		case INITIALIZER_DESIGNATOR:
			panic("unexpected designator initializer found");
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
	if (is_Primitive_type(ent_type) &&
			get_primitive_base_type(ent_type) != NULL) {
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
	switch(get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		create_dynamic_null_initializer(entity, dbgi, base_addr);
		return;
	case IR_INITIALIZER_CONST: {
		ir_node *node     = get_initializer_const_value(initializer);
		ir_type *ent_type = get_entity_type(entity);

		/* is it a bitfield type? */
		if (is_Primitive_type(ent_type) &&
				get_primitive_base_type(ent_type) != NULL) {
			bitfield_store_to_firm(dbgi, entity, base_addr, node, false, false);
			return;
		}

		assert(get_type_mode(type) == get_irn_mode(node));
		ir_node *mem    = get_store();
		ir_node *store  = new_d_Store(dbgi, mem, base_addr, node, cons_none);
		ir_node *proj_m = new_Proj(store, mode_M, pn_Store_M);
		set_store(proj_m);
		return;
	}
	case IR_INITIALIZER_TARVAL: {
		ir_tarval *tv       = get_initializer_tarval_value(initializer);
		ir_node   *cnst     = new_d_Const(dbgi, tv);
		ir_type   *ent_type = get_entity_type(entity);

		/* is it a bitfield type? */
		if (is_Primitive_type(ent_type) &&
				get_primitive_base_type(ent_type) != NULL) {
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

	panic("invalid IR_INITIALIZER found");
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

		ir_node *value = expression_to_firm(initializer_value->value);
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
	ir_node *const src_addr = create_symconst(dbgi, init_entity);
	ir_type *const irtype   = get_ir_type(type);
	ir_node *const copyb    = new_d_CopyB(dbgi, memory, addr, src_addr, irtype);

	ir_node *const copyb_mem = new_Proj(copyb, mode_M, pn_CopyB_M);
	set_store(copyb_mem);
}

static void create_initializer_local_variable_entity(entity_t *entity)
{
	assert(entity->kind == ENTITY_VARIABLE);
	initializer_t *initializer = entity->variable.initializer;
	dbg_info      *dbgi        = get_dbg_info(&entity->base.source_position);
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

	type_t            *type = entity->declaration.type;
	type_qualifiers_t  tq   = get_type_qualifier(type, true);

	if (initializer->kind == INITIALIZER_VALUE) {
		expression_t *      value     = initializer->value.value;
		type_t       *const init_type = skip_typeref(value->base.type);

		if (!is_type_scalar(init_type)) {
			/* skip convs */
			while (value->kind == EXPR_UNARY_CAST)
				value = value->unary.value;

			if (value->kind != EXPR_COMPOUND_LITERAL)
				panic("expected non-scalar initializer to be a compound literal");
			initializer = value->compound_literal.initializer;
			goto have_initializer;
		}

		ir_node  *      node = expression_to_firm(value);
		dbg_info *const dbgi = get_dbg_info(&entity->base.source_position);
		ir_mode  *const mode = get_ir_mode_storage(init_type);
		node = create_conv(dbgi, node, mode);

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

	dbg_info *dbgi      = get_dbg_info(&entity->base.source_position);
	type_t   *type      = entity->declaration.type;
	ir_type  *el_type   = get_ir_type(type->array.element_type);

	/* make sure size_node is calculated */
	get_type_size_node(type);
	ir_node  *elems = type->array.size_node;
	ir_node  *mem   = get_store();
	ir_node  *alloc = new_d_Alloc(dbgi, mem, elems, el_type, stack_alloc);

	ir_node  *proj_m = new_d_Proj(dbgi, alloc, mode_M, pn_Alloc_M);
	ir_node  *addr   = new_d_Proj(dbgi, alloc, mode_P_data, pn_Alloc_res);
	set_store(proj_m);

	assert(entity->declaration.kind == DECLARATION_KIND_VARIABLE_LENGTH_ARRAY);
	entity->variable.v.vla_base = addr;
}

/**
 * Creates a Firm local variable from a declaration.
 */
static void create_local_variable(entity_t *entity)
{
	assert(entity->kind == ENTITY_VARIABLE);
	assert(entity->declaration.kind == DECLARATION_KIND_UNKNOWN);

	bool needs_entity = entity->variable.address_taken;
	type_t *type = skip_typeref(entity->declaration.type);

	/* is it a variable length array? */
	if (is_type_array(type) && !type->array.size_constant) {
		create_variable_length_array(entity);
		return;
	} else if (is_type_array(type) || is_type_compound(type)) {
		needs_entity = true;
	} else if (type->base.qualifiers & TYPE_QUALIFIER_VOLATILE) {
		needs_entity = true;
	}

	if (needs_entity) {
		ir_type *frame_type = get_irg_frame_type(current_ir_graph);
		create_variable_entity(entity,
		                       DECLARATION_KIND_LOCAL_VARIABLE_ENTITY,
		                       frame_type);
	} else {
		entity->declaration.kind        = DECLARATION_KIND_LOCAL_VARIABLE;
		entity->variable.v.value_number = next_value_number_function;
		set_irg_loc_description(current_ir_graph, next_value_number_function,
		                        entity);
		++next_value_number_function;
	}
}

static void create_local_static_variable(entity_t *entity)
{
	assert(entity->kind == ENTITY_VARIABLE);
	assert(entity->declaration.kind == DECLARATION_KIND_UNKNOWN);

	type_t   *type           = skip_typeref(entity->declaration.type);
	ir_type  *const var_type = entity->variable.thread_local ?
		get_tls_type() : get_glob_type();
	ir_type  *const irtype   = get_ir_type(type);
	dbg_info *const dbgi     = get_dbg_info(&entity->base.source_position);

	size_t l = strlen(entity->base.symbol->string);
	char   buf[l + sizeof(".%u")];
	snprintf(buf, sizeof(buf), "%s.%%u", entity->base.symbol->string);
	ident     *const id       = id_unique(buf);
	ir_entity *const irentity = new_d_entity(var_type, id, irtype, dbgi);

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

	ir_graph *const old_current_ir_graph = current_ir_graph;
	current_ir_graph = get_const_code_irg();

	create_variable_initializer(entity);

	assert(current_ir_graph == get_const_code_irg());
	current_ir_graph = old_current_ir_graph;
}



static ir_node *return_statement_to_firm(return_statement_t *statement)
{
	if (!currently_reachable())
		return NULL;

	dbg_info *const dbgi = get_dbg_info(&statement->base.source_position);
	type_t   *const type = skip_typeref(current_function_entity->declaration.type->function.return_type);
	ir_node  *      res  = statement->value ? expression_to_firm(statement->value) : NULL;

	int in_len;
	if (!is_type_void(type)) {
		ir_mode *const mode = get_ir_mode_storage(type);
		if (res) {
			res = create_conv(dbgi, res, mode);
		} else {
			res = new_Unknown(mode);
		}
		in_len = 1;
	} else {
		in_len = 0;
	}

	ir_node *const in[1] = { res };
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

	return expression_to_firm(statement->expression);
}

static void create_local_declarations(entity_t*);

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
	ir_linkage          linkage    = IR_LINKAGE_DEFAULT;
	ir_visibility       visibility = ir_visibility_external;
	storage_class_tag_t storage
		= (storage_class_tag_t)entity->declaration.storage_class;
	decl_modifiers_t    modifiers  = entity->declaration.modifiers;
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

	switch ((storage_class_tag_t) entity->declaration.storage_class) {
	case STORAGE_CLASS_STATIC:
		if (entity->kind == ENTITY_FUNCTION) {
			(void)get_function_entity(entity, NULL);
		} else {
			create_local_static_variable(entity);
		}
		return;
	case STORAGE_CLASS_EXTERN:
		if (entity->kind == ENTITY_FUNCTION) {
			assert(entity->function.statement == NULL);
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
			if (entity->function.statement != NULL) {
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
	panic("invalid storage class found");
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
	ir_node *true_block  = NULL;
	ir_node *false_block = NULL;
	if (currently_reachable()) {
		true_block  = new_immBlock();
		false_block = new_immBlock();
		create_condition_evaluation(statement->condition, true_block, false_block);
		mature_immBlock(true_block);
		mature_immBlock(false_block);
	}

	/* Create the true statement. */
	set_cur_block(true_block);
	statement_to_firm(statement->true_statement);
	ir_node *fallthrough_block = get_cur_block();

	/* Create the false statement. */
	set_cur_block(false_block);
	if (statement->false_statement != NULL) {
		statement_to_firm(statement->false_statement);
	}

	/* Handle the block after the if-statement.  Minor simplification and
	 * optimisation: Reuse the false/true block as fallthrough block, if the
	 * true/false statement does not pass control to the fallthrough block, e.g.
	 * in the typical if (x) return; pattern. */
	if (fallthrough_block) {
		if (currently_reachable()) {
			ir_node *const t_jump = new_r_Jmp(fallthrough_block);
			ir_node *const f_jump = new_Jmp();
			ir_node *const in[]   = { t_jump, f_jump };
			fallthrough_block = new_Block(2, in);
		}
		set_cur_block(fallthrough_block);
	}

	return NULL;
}

/**
 * Add an unconditional jump to the target block.  If the source block is not
 * reachable, then a Bad predecessor is created to prevent Phi-less unreachable
 * loops.  This is necessary if the jump potentially enters a loop.
 */
static void jump_to(ir_node *const target_block)
{
	ir_node *const pred = currently_reachable() ? new_Jmp() : new_Bad(mode_X);
	add_immBlock_pred(target_block, pred);
}

/**
 * Add an unconditional jump to the target block, if the current block is
 * reachable and do nothing otherwise.  This is only valid if the jump does not
 * enter a loop (a back edge is ok).
 */
static void jump_if_reachable(ir_node *const target_block)
{
	if (currently_reachable())
		add_immBlock_pred(target_block, new_Jmp());
}

static ir_node *while_statement_to_firm(while_statement_t *statement)
{
	create_local_declarations(statement->scope.entities);

	/* Create the header block */
	ir_node *const header_block = new_immBlock();
	jump_to(header_block);

	/* Create the condition. */
	ir_node      *      body_block;
	ir_node      *      false_block;
	expression_t *const cond = statement->condition;
	if (is_constant_expression(cond) == EXPR_CLASS_CONSTANT &&
			fold_constant_to_bool(cond)) {
		/* Shortcut for while (true). */
		body_block  = header_block;
		false_block = NULL;

		keep_alive(header_block);
		keep_all_memory(header_block);
	} else {
		body_block  = new_immBlock();
		false_block = new_immBlock();

		set_cur_block(header_block);
		create_condition_evaluation(cond, body_block, false_block);
		mature_immBlock(body_block);
	}

	ir_node *const old_continue_label = continue_label;
	ir_node *const old_break_label    = break_label;
	continue_label = header_block;
	break_label    = false_block;

	/* Create the loop body. */
	set_cur_block(body_block);
	statement_to_firm(statement->body);
	jump_if_reachable(header_block);

	mature_immBlock(header_block);
	assert(false_block == NULL || false_block == break_label);
	false_block = break_label;
	if (false_block != NULL) {
		mature_immBlock(false_block);
	}
	set_cur_block(false_block);

	assert(continue_label == header_block);
	continue_label = old_continue_label;
	break_label    = old_break_label;
	return NULL;
}

static ir_node *get_break_label(void)
{
	if (break_label == NULL) {
		break_label = new_immBlock();
	}
	return break_label;
}

static ir_node *do_while_statement_to_firm(do_while_statement_t *statement)
{
	create_local_declarations(statement->scope.entities);

	/* create the header block */
	ir_node *header_block = new_immBlock();

	/* the loop body */
	ir_node *body_block = new_immBlock();
	jump_to(body_block);

	ir_node *old_continue_label = continue_label;
	ir_node *old_break_label    = break_label;
	continue_label              = header_block;
	break_label                 = NULL;

	set_cur_block(body_block);
	statement_to_firm(statement->body);
	ir_node *const false_block = get_break_label();

	assert(continue_label == header_block);
	continue_label = old_continue_label;
	break_label    = old_break_label;

	jump_if_reachable(header_block);

	/* create the condition */
	mature_immBlock(header_block);
	set_cur_block(header_block);

	create_condition_evaluation(statement->condition, body_block, false_block);
	mature_immBlock(body_block);
	mature_immBlock(false_block);

	set_cur_block(false_block);
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
			expression_to_firm(statement->initialisation);
		}
	}

	/* Create the header block */
	ir_node *const header_block = new_immBlock();
	jump_to(header_block);

	/* Create the condition. */
	ir_node *body_block;
	ir_node *false_block;
	if (statement->condition != NULL) {
		body_block  = new_immBlock();
		false_block = new_immBlock();

		set_cur_block(header_block);
		create_condition_evaluation(statement->condition, body_block, false_block);
		mature_immBlock(body_block);
	} else {
		/* for-ever. */
		body_block  = header_block;
		false_block = NULL;

		keep_alive(header_block);
		keep_all_memory(header_block);
	}

	/* Create the step block, if necessary. */
	ir_node      *      step_block = header_block;
	expression_t *const step       = statement->step;
	if (step != NULL) {
		step_block = new_immBlock();
	}

	ir_node *const old_continue_label = continue_label;
	ir_node *const old_break_label    = break_label;
	continue_label = step_block;
	break_label    = false_block;

	/* Create the loop body. */
	set_cur_block(body_block);
	statement_to_firm(statement->body);
	jump_if_reachable(step_block);

	/* Create the step code. */
	if (step != NULL) {
		mature_immBlock(step_block);
		set_cur_block(step_block);
		expression_to_firm(step);
		jump_if_reachable(header_block);
	}

	mature_immBlock(header_block);
	assert(false_block == NULL || false_block == break_label);
	false_block = break_label;
	if (false_block != NULL) {
		mature_immBlock(false_block);
	}
	set_cur_block(false_block);

	assert(continue_label == step_block);
	continue_label = old_continue_label;
	break_label    = old_break_label;
	return NULL;
}

static ir_node *create_jump_statement(const statement_t *statement, ir_node *target_block)
{
	if (!currently_reachable())
		return NULL;

	dbg_info *dbgi = get_dbg_info(&statement->base.source_position);
	ir_node  *jump = new_d_Jmp(dbgi);
	add_immBlock_pred(target_block, jump);

	set_unreachable_now();
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
		ir_tarval *min = fold_constant_to_tarval(l->expression);
		ir_tarval *max = min;
		long       pn  = (long) i+1;
		if (l->end_range != NULL)
			max = fold_constant_to_tarval(l->end_range);
		ir_switch_table_set(res, i++, min, max, pn);
		l->pn = pn;
	}
	return res;
}

static ir_node *switch_statement_to_firm(switch_statement_t *statement)
{
	dbg_info *dbgi        = get_dbg_info(&statement->base.source_position);
	ir_node  *switch_node = NULL;

	if (currently_reachable()) {
		ir_node *expression = expression_to_firm(statement->expression);
		ir_switch_table *table = create_switch_table(statement);
		unsigned n_outs = (unsigned)ir_switch_table_get_n_entries(table) + 1;

		switch_node = new_d_Switch(dbgi, expression, n_outs, table);
	}

	set_unreachable_now();

	ir_node *const old_switch            = current_switch;
	ir_node *const old_break_label       = break_label;
	const bool     old_saw_default_label = saw_default_label;
	saw_default_label                    = false;
	current_switch                       = switch_node;
	break_label                          = NULL;

	statement_to_firm(statement->body);

	if (currently_reachable()) {
		add_immBlock_pred(get_break_label(), new_Jmp());
	}

	if (!saw_default_label && switch_node) {
		ir_node *proj = new_d_Proj(dbgi, switch_node, mode_X, pn_Switch_default);
		add_immBlock_pred(get_break_label(), proj);
	}

	if (break_label != NULL) {
		mature_immBlock(break_label);
	}
	set_cur_block(break_label);

	assert(current_switch == switch_node);
	current_switch    = old_switch;
	break_label       = old_break_label;
	saw_default_label = old_saw_default_label;
	return NULL;
}

static ir_node *case_label_to_firm(const case_label_statement_t *statement)
{
	if (statement->is_empty_range)
		return NULL;

	if (current_switch != NULL) {
		ir_node *block = new_immBlock();
		/* Fallthrough from previous case */
		jump_if_reachable(block);

		ir_node  *const proj = new_Proj(current_switch, mode_X, statement->pn);
		add_immBlock_pred(block, proj);
		if (statement->expression == NULL)
			saw_default_label = true;

		mature_immBlock(block);
		set_cur_block(block);
	}

	return statement_to_firm(statement->statement);
}

static ir_node *label_to_firm(const label_statement_t *statement)
{
	ir_node *block = get_label_block(statement->label);
	jump_to(block);

	set_cur_block(block);
	keep_alive(block);
	keep_all_memory(block);

	return statement_to_firm(statement->statement);
}

static ir_node *computed_goto_to_firm(computed_goto_statement_t const *const statement)
{
	if (!currently_reachable())
		return NULL;

	ir_node  *const irn  = expression_to_firm(statement->expression);
	dbg_info *const dbgi = get_dbg_info(&statement->base.source_position);
	ir_node  *const ijmp = new_d_IJmp(dbgi, irn);

	set_irn_link(ijmp, ijmp_list);
	ijmp_list = ijmp;

	set_unreachable_now();
	return NULL;
}

static ir_node *asm_statement_to_firm(const asm_statement_t *statement)
{
	bool           needs_memory = statement->is_volatile;
	size_t         n_clobbers   = 0;
	asm_clobber_t *clobber      = statement->clobbers;
	for ( ; clobber != NULL; clobber = clobber->next) {
		const char *clobber_str = clobber->clobber.begin;

		if (!be_is_valid_clobber(clobber_str)) {
			errorf(&statement->base.source_position,
				   "invalid clobber '%s' specified", clobber->clobber);
			continue;
		}

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
	asm_argument_t *argument = statement->inputs;
	for ( ; argument != NULL; argument = argument->next)
		n_inputs++;
	size_t n_outputs = 0;
	argument = statement->outputs;
	for ( ; argument != NULL; argument = argument->next)
		n_outputs++;

	unsigned next_pos = 0;

	ir_node *ins[n_inputs + n_outputs + 1];
	size_t   in_size = 0;

	ir_asm_constraint tmp_in_constraints[n_outputs];

	const expression_t *out_exprs[n_outputs];
	ir_node            *out_addrs[n_outputs];
	size_t              out_size = 0;

	argument = statement->outputs;
	for ( ; argument != NULL; argument = argument->next) {
		const char *constraints = argument->constraints.begin;
		asm_constraint_flags_t asm_flags
			= be_parse_asm_constraints(constraints);

		{
			source_position_t const *const pos = &statement->base.source_position;
			if (asm_flags & ASM_CONSTRAINT_FLAG_NO_SUPPORT) {
				warningf(WARN_OTHER, pos, "some constraints in '%s' are not supported", constraints);
			}
			if (asm_flags & ASM_CONSTRAINT_FLAG_INVALID) {
				errorf(pos, "some constraints in '%s' are invalid", constraints);
				continue;
			}
			if (! (asm_flags & ASM_CONSTRAINT_FLAG_MODIFIER_WRITE)) {
				errorf(pos, "no write flag specified for output constraints '%s'", constraints);
				continue;
			}
		}

		unsigned pos = next_pos++;
		if ( (asm_flags & ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE)
				|| (asm_flags & ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER) ) {
			expression_t *expr = argument->expression;
			ir_node      *addr = expression_to_addr(expr);
			/* in+output, construct an artifical same_as constraint on the
			 * input */
			if (asm_flags & ASM_CONSTRAINT_FLAG_MODIFIER_READ) {
				char     buf[64];
				ir_node *value = get_value_from_lvalue(expr, addr);

				snprintf(buf, sizeof(buf), "%u", (unsigned) out_size);

				ir_asm_constraint constraint;
				constraint.pos              = pos;
				constraint.constraint       = new_id_from_str(buf);
				constraint.mode             = get_ir_mode_storage(expr->base.type);
				tmp_in_constraints[in_size] = constraint;
				ins[in_size] = value;

				++in_size;
			}

			out_exprs[out_size] = expr;
			out_addrs[out_size] = addr;
			++out_size;
		} else if (asm_flags & ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP) {
			/* pure memory ops need no input (but we have to make sure we
			 * attach to the memory) */
			assert(! (asm_flags &
						(ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE
						 | ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER)));
			needs_memory = true;

			/* we need to attach the address to the inputs */
			expression_t *expr = argument->expression;

			ir_asm_constraint constraint;
			constraint.pos              = pos;
			constraint.constraint       = new_id_from_str(constraints);
			constraint.mode             = mode_M;
			tmp_in_constraints[in_size] = constraint;

			ins[in_size] = expression_to_addr(expr);
			++in_size;
			continue;
		} else {
			errorf(&statement->base.source_position,
			       "only modifiers but no place set in constraints '%s'",
			       constraints);
			continue;
		}

		ir_asm_constraint constraint;
		constraint.pos        = pos;
		constraint.constraint = new_id_from_str(constraints);
		constraint.mode       = get_ir_mode_storage(argument->expression->base.type);

		obstack_grow(&asm_obst, &constraint, sizeof(constraint));
	}
	assert(obstack_object_size(&asm_obst)
			== out_size * sizeof(ir_asm_constraint));
	ir_asm_constraint *output_constraints = obstack_finish(&asm_obst);


	obstack_grow(&asm_obst, tmp_in_constraints,
	             in_size * sizeof(tmp_in_constraints[0]));
	/* find and count input and output arguments */
	argument = statement->inputs;
	for ( ; argument != NULL; argument = argument->next) {
		const char *constraints = argument->constraints.begin;
		asm_constraint_flags_t asm_flags
			= be_parse_asm_constraints(constraints);

		if (asm_flags & ASM_CONSTRAINT_FLAG_NO_SUPPORT) {
			errorf(&statement->base.source_position,
			       "some constraints in '%s' are not supported", constraints);
			continue;
		}
		if (asm_flags & ASM_CONSTRAINT_FLAG_INVALID) {
			errorf(&statement->base.source_position,
			       "some constraints in '%s' are invalid", constraints);
			continue;
		}
		if (asm_flags & ASM_CONSTRAINT_FLAG_MODIFIER_WRITE) {
			errorf(&statement->base.source_position,
			       "write flag specified for input constraints '%s'",
			       constraints);
			continue;
		}

		ir_node *input;
		if ( (asm_flags & ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE)
				|| (asm_flags & ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER) ) {
			/* we can treat this as "normal" input */
			input = expression_to_firm(argument->expression);
		} else if (asm_flags & ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP) {
			/* pure memory ops need no input (but we have to make sure we
			 * attach to the memory) */
			assert(! (asm_flags &
						(ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE
						 | ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER)));
			needs_memory = true;
			input = expression_to_addr(argument->expression);
		} else {
			errorf(&statement->base.source_position,
			       "only modifiers but no place set in constraints '%s'",
			       constraints);
			continue;
		}

		ir_asm_constraint constraint;
		constraint.pos        = next_pos++;
		constraint.constraint = new_id_from_str(constraints);
		constraint.mode       = get_irn_mode(input);

		obstack_grow(&asm_obst, &constraint, sizeof(constraint));
		ins[in_size++] = input;
	}

	ir_node *mem = needs_memory ? get_store() : new_NoMem();
	assert(obstack_object_size(&asm_obst)
			== in_size * sizeof(ir_asm_constraint));
	ir_asm_constraint *input_constraints = obstack_finish(&asm_obst);

	/* create asm node */
	dbg_info *dbgi = get_dbg_info(&statement->base.source_position);

	ident *asm_text = new_id_from_str(statement->asm_text.begin);

	ir_node *node = new_d_ASM(dbgi, mem, in_size, ins, input_constraints,
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

	size_t i;
	for (i = 0; i < out_size; ++i) {
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
	source_position_t const *const pos = &statement->base.source_position;
	warningf(WARN_OTHER, pos, "structured exception handling ignored");
	return NULL;
}

static ir_node *leave_statement_to_firm(leave_statement_t *statement)
{
	errorf(&statement->base.source_position, "__leave not supported yet");
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
	case STATEMENT_IF:            return if_statement_to_firm(         &stmt->ifs);
	case STATEMENT_LABEL:         return label_to_firm(                &stmt->label);
	case STATEMENT_LEAVE:         return leave_statement_to_firm(      &stmt->leave);
	case STATEMENT_MS_TRY:        return ms_try_statement_to_firm(     &stmt->ms_try);
	case STATEMENT_RETURN:        return return_statement_to_firm(     &stmt->returns);
	case STATEMENT_SWITCH:        return switch_statement_to_firm(     &stmt->switchs);
	case STATEMENT_WHILE:         return while_statement_to_firm(      &stmt->whiles);

	case STATEMENT_BREAK:         return create_jump_statement(stmt, get_break_label());
	case STATEMENT_CONTINUE:      return create_jump_statement(stmt, continue_label);
	case STATEMENT_GOTO:          return create_jump_statement(stmt, get_label_block(stmt->gotos.label));

	case STATEMENT_ERROR: panic("error statement found");
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
		    !entity->variable.address_taken                                       &&
		    is_type_scalar(skip_typeref(entity->declaration.type)))
			++count;
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
	walk_statements(function->statement, count_local_variables_in_stmt, &count);
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

		assert(!is_type_array(type));
		bool const needs_entity = parameter->variable.address_taken || is_type_compound(type);

		ir_type *param_irtype = get_method_param_type(function_irtype, n);
		if (needs_entity) {
			ir_type   *frame_type = get_irg_frame_type(irg);
			ir_entity *param
				= new_parameter_entity(frame_type, n, param_irtype);
			parameter->declaration.kind  = DECLARATION_KIND_PARAMETER_ENTITY;
			parameter->variable.v.entity = param;
			continue;
		}

		ir_mode *param_mode = get_type_mode(param_irtype);
		long     pn         = n;
		ir_node *value      = new_r_Proj(args, param_mode, pn);

		ir_mode *mode = get_ir_mode_storage(type);
		value = create_conv(NULL, value, mode);

		parameter->declaration.kind        = DECLARATION_KIND_PARAMETER;
		parameter->variable.v.value_number = next_value_number_function;
		set_irg_loc_description(current_ir_graph, next_value_number_function,
		                        parameter);
		++next_value_number_function;

		set_value(parameter->variable.v.value_number, value);
	}
}

/**
 * Handle additional decl modifiers for IR-graphs
 *
 * @param irg            the IR-graph
 * @param dec_modifiers  additional modifiers
 */
static void handle_decl_modifier_irg(ir_graph *irg,
                                     decl_modifiers_t decl_modifiers)
{
	if (decl_modifiers & DM_NAKED) {
		/* TRUE if the declaration includes the Microsoft
		   __declspec(naked) specifier. */
		add_irg_additional_properties(irg, mtp_property_naked);
	}
	if (decl_modifiers & DM_FORCEINLINE) {
		/* TRUE if the declaration includes the
		   Microsoft __forceinline specifier. */
		set_irg_inline_property(irg, irg_inline_forced);
	}
	if (decl_modifiers & DM_NOINLINE) {
		/* TRUE if the declaration includes the Microsoft
		   __declspec(noinline) specifier. */
		set_irg_inline_property(irg, irg_inline_forbidden);
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
	ir_node   *val          = new_rd_SymConst_addr_ent(NULL, irg, mode_P_code,
	                                                   method);

	set_entity_ld_ident(ptr, new_id_from_chars("", 0));
	set_entity_compiler_generated(ptr, 1);
	set_entity_visibility(ptr, ir_visibility_private);
	add_entity_linkage(ptr, IR_LINKAGE_CONSTANT|IR_LINKAGE_HIDDEN_USER);
	set_atomic_ent_value(ptr, val);
}

/**
 * Generate possible IJmp branches to a given label block.
 */
static void gen_ijmp_branches(ir_node *block)
{
	ir_node *ijmp;
	for (ijmp = ijmp_list; ijmp != NULL; ijmp = get_irn_link(ijmp)) {
		add_immBlock_pred(block, ijmp);
	}
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

	if (entity->function.statement == NULL)
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

	assert(all_labels == NULL);
	all_labels = NEW_ARR_F(label_t *, 0);
	ijmp_list  = NULL;

	int       n_local_vars = get_function_n_local_vars(entity);
	ir_graph *irg          = new_ir_graph(function_entity, n_local_vars);
	current_ir_graph = irg;

	ir_graph *old_current_function = current_function;
	current_function = irg;

	ir_entity *const old_current_vararg_entity = current_vararg_entity;
	current_vararg_entity = NULL;

	set_irg_fp_model(irg, firm_fp_model);
	tarval_enable_fp_ops(1);
	set_irn_dbg_info(get_irg_start_block(irg),
	                 get_entity_dbg_info(function_entity));

	/* set inline flags */
	if (entity->function.is_inline)
		set_irg_inline_property(irg, irg_inline_recomended);
	handle_decl_modifier_irg(irg, entity->declaration.modifiers);

	next_value_number_function = 0;
	initialize_function_parameters(entity);
	current_static_link = entity->function.static_link;

	statement_to_firm(entity->function.statement);

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

	for (int i = ARR_LEN(all_labels) - 1; i >= 0; --i) {
		label_t *label = all_labels[i];
		if (label->address_taken) {
			gen_ijmp_branches(label->block);
		}
		mature_immBlock(label->block);
	}

	DEL_ARR_F(all_labels);
	all_labels = NULL;

	irg_finalize_cons(irg);

	/* finalize the frame type */
	ir_type *frame_type = get_irg_frame_type(irg);
	int      n          = get_compound_n_members(frame_type);
	int      align_all  = 4;
	int      offset     = 0;
	for (int i = 0; i < n; ++i) {
		ir_entity *member      = get_compound_member(frame_type, i);
		ir_type   *entity_type = get_entity_type(member);

		int align = get_type_alignment_bytes(entity_type);
		if (align > align_all)
			align_all = align;
		int misalign = 0;
		if (align > 0) {
			misalign  = offset % align;
			if (misalign > 0) {
				offset += align - misalign;
			}
		}

		set_entity_offset(member, offset);
		offset += get_type_size_bytes(entity_type);
	}
	set_type_size_bytes(frame_type, offset);
	set_type_alignment_bytes(frame_type, align_all);

	irg_verify(irg, VERIFY_ENFORCE_SSA);
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

static void scope_to_firm(scope_t *scope)
{
	/* first pass: create declarations */
	entity_t *entity = scope->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		if (entity->base.symbol == NULL)
			continue;

		if (entity->kind == ENTITY_FUNCTION) {
			if (entity->function.btk != BUILTIN_NONE) {
				/* builtins have no representation */
				continue;
			}
			(void)get_function_entity(entity, NULL);
		} else if (entity->kind == ENTITY_VARIABLE) {
			create_global_variable(entity);
		} else if (entity->kind == ENTITY_NAMESPACE) {
			scope_to_firm(&entity->namespacee.members);
		}
	}

	/* second pass: create code/initializers */
	entity = scope->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		if (entity->base.symbol == NULL)
			continue;

		if (entity->kind == ENTITY_FUNCTION) {
			if (entity->function.btk != BUILTIN_NONE) {
				/* builtins have no representation */
				continue;
			}
			create_function(entity);
		} else if (entity->kind == ENTITY_VARIABLE) {
			assert(entity->declaration.kind
					== DECLARATION_KIND_GLOBAL_VARIABLE);
			current_ir_graph = get_const_code_irg();
			create_variable_initializer(entity);
		}
	}
}

void init_ast2firm(void)
{
	obstack_init(&asm_obst);
	init_atomic_modes();

	ir_set_debug_retrieve(dbg_retrieve);
	ir_set_type_debug_retrieve(dbg_print_type_dbg_info);

	/* create idents for all known runtime functions */
	for (size_t i = 0; i < lengthof(rts_data); ++i) {
		rts_idents[i] = new_id_from_str(rts_data[i].name);
	}

	entitymap_init(&entitymap);
}

static void init_ir_types(void)
{
	static int ir_types_initialized = 0;
	if (ir_types_initialized)
		return;
	ir_types_initialized = 1;

	ir_type_char    = get_ir_type(type_char);
	ir_type_wchar_t = get_ir_type(type_wchar_t);

	be_params             = be_get_backend_param();
	mode_float_arithmetic = be_params->mode_float_arithmetic;

	stack_param_align     = be_params->stack_param_align;
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
	if (c_mode & _CXX) {
		be_dwarf_set_source_language(DW_LANG_C_plus_plus);
	} else if (c_mode & _C99) {
		be_dwarf_set_source_language(DW_LANG_C99);
	} else if (c_mode & _C89) {
		be_dwarf_set_source_language(DW_LANG_C89);
	} else {
		be_dwarf_set_source_language(DW_LANG_C);
	}
	const char *cwd = get_cwd();
	if (cwd != NULL) {
		be_dwarf_set_compilation_directory(cwd);
	}

	/* initialize firm arithmetic */
	tarval_set_integer_overflow_mode(TV_OVERFLOW_WRAP);
	ir_set_uninitialized_local_variable_func(uninitialized_local_var);

	/* just to be sure */
	continue_label           = NULL;
	break_label              = NULL;
	current_switch           = NULL;
	current_translation_unit = unit;

	init_ir_types();

	scope_to_firm(&unit->scope);
	global_asm_to_firm(unit->global_asm);

	current_ir_graph         = NULL;
	current_translation_unit = NULL;
}
