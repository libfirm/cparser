/*
 * This file is part of cparser.
 * Copyright (C) 2007-2008 Matthias Braun <matze@braunis.de>
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

#include <libfirm/firm.h>
#include <libfirm/adt/obst.h>
#include <libfirm/be.h>

#include "ast2firm.h"

#include "adt/error.h"
#include "adt/array.h"
#include "symbol_t.h"
#include "token_t.h"
#include "type_t.h"
#include "ast_t.h"
#include "parser.h"
#include "diagnostic.h"
#include "lang_features.h"
#include "types.h"
#include "warning.h"
#include "entitymap_t.h"
#include "driver/firm_opt.h"
#include "driver/firm_cmdline.h"

/* some idents needed for name mangling */
static ident *id_underscore;
static ident *id_imp;

static ir_type *ir_type_const_char;
static ir_type *ir_type_wchar_t;
static ir_type *ir_type_void;
static ir_type *ir_type_int;

static type_t *type_const_char;

static int       next_value_number_function;
static ir_node  *continue_label;
static ir_node  *break_label;
static ir_node  *current_switch_cond;
static bool      saw_default_label;
static ir_node **imature_blocks;
static bool constant_folding;

static const declaration_t *current_function_decl;
static ir_node             *current_function_name;
static ir_node             *current_funcsig;
static switch_statement_t  *current_switch;

static entitymap_t  entitymap;

static struct obstack asm_obst;

typedef enum declaration_kind_t {
	DECLARATION_KIND_UNKNOWN,
	DECLARATION_KIND_FUNCTION,
	DECLARATION_KIND_VARIABLE_LENGTH_ARRAY,
	DECLARATION_KIND_GLOBAL_VARIABLE,
	DECLARATION_KIND_LOCAL_VARIABLE,
	DECLARATION_KIND_LOCAL_VARIABLE_ENTITY,
	DECLARATION_KIND_COMPOUND_MEMBER,
	DECLARATION_KIND_LABEL_BLOCK,
	DECLARATION_KIND_ENUM_ENTRY,
	DECLARATION_KIND_COMPOUND_TYPE_INCOMPLETE,
	DECLARATION_KIND_COMPOUND_TYPE_COMPLETE,
	DECLARATION_KIND_TYPE
} declaration_kind_t;

static ir_type *get_ir_type(type_t *type);
static ir_type *get_ir_type_incomplete(type_t *type);
static int count_decls_in_stmts(const statement_t *stmt);

ir_node *uninitialized_local_var(ir_graph *irg, ir_mode *mode, int pos)
{
	const declaration_t *declaration = get_irg_loc_description(irg, pos);

	if (declaration != NULL) {
		warningf(&declaration->source_position,
		         "variable '%#T' might be used uninitialized",
		         declaration->type, declaration->symbol);
	}
	return new_r_Unknown(irg, mode);
}

unsigned dbg_snprint(char *buf, unsigned len, const dbg_info *dbg)
{
	const source_position_t *pos = (const source_position_t*) dbg;
	if (pos == NULL)
		return 0;
	return (unsigned) snprintf(buf, len, "%s:%u", pos->input_name,
	                           pos->linenr);
}

const char *dbg_retrieve(const dbg_info *dbg, unsigned *line)
{
	const source_position_t *pos = (const source_position_t*) dbg;
	if (pos == NULL)
		return NULL;
	if (line != NULL)
		*line = pos->linenr;
	return pos->input_name;
}

static dbg_info *get_dbg_info(const source_position_t *pos)
{
	return (dbg_info*) pos;
}

static ir_mode *_atomic_modes[ATOMIC_TYPE_LAST+1];

static ir_mode *mode_int, *mode_uint;

static ir_node *_expression_to_firm(const expression_t *expression);
static ir_node *expression_to_firm(const expression_t *expression);
static inline ir_mode *get_ir_mode(type_t *type);
static void create_local_declaration(declaration_t *declaration);

static ir_mode *init_atomic_ir_mode(atomic_type_kind_t kind)
{
	unsigned flags = get_atomic_type_flags(kind);
	unsigned size  = get_atomic_type_size(kind);
	if ( (flags & (ATOMIC_TYPE_FLAG_INTEGER | ATOMIC_TYPE_FLAG_FLOAT))
			&& !(flags & ATOMIC_TYPE_FLAG_COMPLEX)) {
		char            name[64];
		ir_mode_sort    sort;
		unsigned        bit_size     = size * 8;
		bool            is_signed    = (flags & ATOMIC_TYPE_FLAG_SIGNED) != 0;
		ir_mode_arithmetic arithmetic;
		unsigned        modulo_shift;

		if (flags & ATOMIC_TYPE_FLAG_INTEGER) {
			assert(! (flags & ATOMIC_TYPE_FLAG_FLOAT));
			snprintf(name, sizeof(name), "i%s%d", is_signed?"":"u", bit_size);
			sort         = irms_int_number;
			arithmetic   = irma_twos_complement;
			modulo_shift = bit_size < machine_size ? machine_size : bit_size;
		} else {
			assert(flags & ATOMIC_TYPE_FLAG_FLOAT);
			snprintf(name, sizeof(name), "f%d", bit_size);
			sort         = irms_float_number;
			arithmetic   = irma_ieee754;
			modulo_shift = 0;
		}
		return new_ir_mode(name, sort, bit_size, is_signed, arithmetic,
		                   modulo_shift);
	}

	return NULL;
}

/**
 * Initialises the atomic modes depending on the machine size.
 */
static void init_atomic_modes(void)
{
	for(int i = 0; i <= ATOMIC_TYPE_LAST; ++i) {
		_atomic_modes[i] = init_atomic_ir_mode((atomic_type_kind_t) i);
	}
	mode_int  = _atomic_modes[ATOMIC_TYPE_INT];
	mode_uint = _atomic_modes[ATOMIC_TYPE_UINT];

	/* there's no real void type in firm */
	_atomic_modes[ATOMIC_TYPE_VOID] = mode_int;

	/* initialize pointer modes */
	char            name[64];
	ir_mode_sort    sort         = irms_reference;
	unsigned        bit_size     = machine_size;
	bool            is_signed    = 0;
	ir_mode_arithmetic arithmetic   = irma_twos_complement;
	unsigned        modulo_shift
		= bit_size < machine_size ? machine_size : bit_size;

	snprintf(name, sizeof(name), "p%d", machine_size);
	ir_mode *ptr_mode = new_ir_mode(name, sort, bit_size, is_signed, arithmetic,
	                                modulo_shift);

	set_reference_mode_signed_eq(ptr_mode, _atomic_modes[get_intptr_kind()]);
	set_reference_mode_unsigned_eq(ptr_mode, _atomic_modes[get_uintptr_kind()]);

	/* Hmm, pointers should be machine size */
	set_modeP_data(ptr_mode);
	set_modeP_code(ptr_mode);
}

static unsigned get_compound_type_size(compound_type_t *type)
{
	ir_type *irtype = get_ir_type((type_t*) type);
	return get_type_size_bytes(irtype);
}

static unsigned get_array_type_size(array_type_t *type)
{
	assert(!type->is_vla);
	ir_type *irtype = get_ir_type((type_t*) type);
	return get_type_size_bytes(irtype);
}


static unsigned get_type_size_const(type_t *type)
{
	switch(type->kind) {
	case TYPE_ERROR:
		panic("error type occurred");
	case TYPE_ATOMIC:
		return get_atomic_type_size(type->atomic.akind);
	case TYPE_COMPLEX:
		return 2 * get_atomic_type_size(type->complex.akind);
	case TYPE_IMAGINARY:
		return get_atomic_type_size(type->imaginary.akind);
	case TYPE_ENUM:
		return get_mode_size_bytes(mode_int);
	case TYPE_COMPOUND_UNION:
	case TYPE_COMPOUND_STRUCT:
		return get_compound_type_size(&type->compound);
	case TYPE_FUNCTION:
		/* just a pointer to the function */
		return get_mode_size_bytes(mode_P_code);
	case TYPE_POINTER:
		return get_mode_size_bytes(mode_P_data);
	case TYPE_ARRAY:
		return get_array_type_size(&type->array);
	case TYPE_BUILTIN:
		return get_type_size_const(type->builtin.real_type);
	case TYPE_BITFIELD:
		panic("type size of bitfield request");
	case TYPE_TYPEDEF:
	case TYPE_TYPEOF:
	case TYPE_INVALID:
		break;
	}
	panic("Trying to determine size of invalid type");
}

static ir_node *get_type_size(type_t *type)
{
	type = skip_typeref(type);

	if (is_type_array(type) && type->array.is_vla) {
		ir_node *size_node = type->array.size_node;
		if (size_node == NULL) {
			size_node = expression_to_firm(type->array.size_expression);
			assert(!is_Const(size_node));
			type->array.size_node = size_node;
		}

		ir_node *elem_size = get_type_size(type->array.element_type);
		ir_mode *mode      = get_irn_mode(size_node);
		ir_node *real_size = new_d_Mul(NULL, size_node, elem_size, mode);
		return real_size;
	}

	ir_mode *mode = get_ir_mode(type_size_t);
	symconst_symbol sym;
	sym.type_p = get_ir_type(type);
	return new_SymConst(mode, sym, symconst_type_size);
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
static ir_type *create_atomic_type(const atomic_type_t *type)
{
	dbg_info           *dbgi      = get_dbg_info(&type->base.source_position);
	atomic_type_kind_t  kind      = type->akind;
	ir_mode            *mode      = _atomic_modes[kind];
	ident              *id        = get_mode_ident(mode);
	ir_type            *irtype    = new_d_type_primitive(id, mode, dbgi);

	set_type_alignment_bytes(irtype, type->base.alignment);

	return irtype;
}

/**
 * Creates a Firm type for a complex type
 */
static ir_type *create_complex_type(const complex_type_t *type)
{
	dbg_info           *dbgi      = get_dbg_info(&type->base.source_position);
	atomic_type_kind_t  kind      = type->akind;
	ir_mode            *mode      = _atomic_modes[kind];
	ident              *id        = get_mode_ident(mode);

	(void) id;
	(void) dbgi;

	/* FIXME: finish the array */
	return NULL;
}

/**
 * Creates a Firm type for an imaginary type
 */
static ir_type *create_imaginary_type(const imaginary_type_t *type)
{
	dbg_info           *dbgi      = get_dbg_info(&type->base.source_position);
	atomic_type_kind_t  kind      = type->akind;
	ir_mode            *mode      = _atomic_modes[kind];
	ident              *id        = get_mode_ident(mode);
	ir_type            *irtype    = new_d_type_primitive(id, mode, dbgi);

	set_type_alignment_bytes(irtype, type->base.alignment);

	return irtype;
}

/**
 * return type of a parameter (and take transparent union gnu extension into
 * account)
 */
static type_t *get_parameter_type(type_t *type)
{
	type = skip_typeref(type);
	if (type->base.modifiers & TYPE_MODIFIER_TRANSPARENT_UNION) {
		declaration_t *decl = type->compound.declaration;
		type                = decl->scope.declarations->type;
	}

	return type;
}

static ir_type *create_method_type(const function_type_t *function_type)
{
	type_t  *return_type  = function_type->return_type;

	ident   *id           = id_unique("functiontype.%u");
	int      n_parameters = count_parameters(function_type);
	int      n_results    = return_type == type_void ? 0 : 1;
	dbg_info *dbgi        = get_dbg_info(&function_type->base.source_position);
	ir_type *irtype       = new_d_type_method(id, n_parameters, n_results, dbgi);

	if (return_type != type_void) {
		ir_type *restype = get_ir_type(return_type);
		set_method_res_type(irtype, 0, restype);
	}

	function_parameter_t *parameter = function_type->parameters;
	int                   n         = 0;
	for ( ; parameter != NULL; parameter = parameter->next) {
		type_t  *type     = get_parameter_type(parameter->type);
		ir_type *p_irtype = get_ir_type(type);
		set_method_param_type(irtype, n, p_irtype);
		++n;
	}

	if (function_type->variadic || function_type->unspecified_parameters) {
		set_method_variadicity(irtype, variadicity_variadic);
	}

	unsigned cc = get_method_calling_convention(irtype);
	switch (function_type->calling_convention) {
	case CC_DEFAULT: /* unspecified calling convention, equal to one of the other, typically cdecl */
	case CC_CDECL:
is_cdecl:
		set_method_calling_convention(irtype, SET_CDECL(cc));
		break;

	case CC_STDCALL:
		if (function_type->variadic || function_type->unspecified_parameters)
			goto is_cdecl;

		/* only non-variadic function can use stdcall, else use cdecl */
		set_method_calling_convention(irtype, SET_STDCALL(cc));
		break;

	case CC_FASTCALL:
		if (function_type->variadic || function_type->unspecified_parameters)
			goto is_cdecl;
		/* only non-variadic function can use fastcall, else use cdecl */
		set_method_calling_convention(irtype, SET_FASTCALL(cc));
		break;

	case CC_THISCALL:
		/* Hmm, leave default, not accepted by the parser yet. */
		warningf(&function_type->base.source_position, "THISCALL calling convention not supported yet");
		break;
	}
	return irtype;
}

static ir_type *create_pointer_type(pointer_type_t *type)
{
	type_t   *points_to    = type->points_to;
	ir_type  *ir_points_to = get_ir_type_incomplete(points_to);
	dbg_info *dbgi         = get_dbg_info(&type->base.source_position);
	ir_type  *ir_type      = new_d_type_pointer(id_unique("pointer.%u"),
	                                            ir_points_to, mode_P_data, dbgi);

	return ir_type;
}

static ir_type *create_array_type(array_type_t *type)
{
	type_t  *element_type    = type->element_type;
	ir_type *ir_element_type = get_ir_type(element_type);

	ident    *id      = id_unique("array.%u");
	dbg_info *dbgi    = get_dbg_info(&type->base.source_position);
	ir_type  *ir_type = new_d_type_array(id, 1, ir_element_type, dbgi);

	const int align = get_type_alignment_bytes(ir_element_type);
	set_type_alignment_bytes(ir_type, align);

	if (type->size_constant) {
		int n_elements = type->size;

		set_array_bounds_int(ir_type, 0, 0, n_elements);

		size_t elemsize = get_type_size_bytes(ir_element_type);
		if (elemsize % align > 0) {
			elemsize += align - (elemsize % align);
		}
		set_type_size_bytes(ir_type, n_elements * elemsize);
	} else {
		set_array_lower_bound_int(ir_type, 0, 0);
	}
	set_type_state(ir_type, layout_fixed);

	return ir_type;
}

/**
 * Return the signed integer type of size bits.
 *
 * @param size   the size
 */
static ir_type *get_signed_int_type_for_bit_size(ir_type *base_tp,
                                                 unsigned size)
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
		mode = new_ir_mode(name, irms_int_number, size, 1, irma_twos_complement,
		                   size <= 32 ? 32 : size );
		s_modes[size] = mode;
	}

	char name[32];
	snprintf(name, sizeof(name), "I%u", size);
	ident *id = new_id_from_str(name);
	dbg_info *dbgi = get_dbg_info(&builtin_source_position);
	res = new_d_type_primitive(mangle_u(get_type_ident(base_tp), id), mode, dbgi);
	set_primitive_base_type(res, base_tp);

	return res;
}

/**
 * Return the unsigned integer type of size bits.
 *
 * @param size   the size
 */
static ir_type *get_unsigned_int_type_for_bit_size(ir_type *base_tp,
                                                   unsigned size)
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
		mode = new_ir_mode(name, irms_int_number, size, 0, irma_twos_complement,
		                   size <= 32 ? 32 : size );
		u_modes[size] = mode;
	}

	char name[32];

	snprintf(name, sizeof(name), "U%u", size);
	ident *id = new_id_from_str(name);
	dbg_info *dbgi = get_dbg_info(&builtin_source_position);
	res = new_d_type_primitive(mangle_u(get_type_ident(base_tp), id), mode, dbgi);
	set_primitive_base_type(res, base_tp);

	return res;
}

static ir_type *create_bitfield_type(bitfield_type_t *const type)
{
	type_t *base = skip_typeref(type->base_type);
	assert(base->kind == TYPE_ATOMIC || base->kind == TYPE_ENUM);
	ir_type *irbase = get_ir_type(base);

	unsigned size = fold_constant(type->size);

	assert(!is_type_float(base));
	if (is_type_signed(base)) {
		return get_signed_int_type_for_bit_size(irbase, size);
	} else {
		return get_unsigned_int_type_for_bit_size(irbase, size);
	}
}

#define INVALID_TYPE ((ir_type_ptr)-1)

enum {
	COMPOUND_IS_STRUCT = false,
	COMPOUND_IS_UNION  = true
};

/**
 * Construct firm type from ast struct type.
 *
 * As anonymous inner structs get flattened to a single firm type, we might get
 * irtype, outer_offset and out_align passed (they represent the position of
 * the anonymous inner struct inside the resulting firm struct)
 */
static ir_type *create_compound_type(compound_type_t *type, ir_type *irtype,
                                     size_t *outer_offset, size_t *outer_align,
                                     bool incomplete, bool is_union)
{
	declaration_t      *declaration = type->declaration;
	declaration_kind_t  kind        = (declaration_kind_t)declaration->declaration_kind;

	if (kind == DECLARATION_KIND_COMPOUND_TYPE_COMPLETE
			|| (kind == DECLARATION_KIND_COMPOUND_TYPE_INCOMPLETE
				&& incomplete))
		return declaration->v.irtype;

	size_t align_all  = 1;
	size_t offset     = 0;
	size_t bit_offset = 0;
	size_t size       = 0;

	if (irtype == NULL) {
		symbol_t *symbol = declaration->symbol;
		ident    *id;
		if (symbol != NULL) {
			id = new_id_from_str(symbol->string);
		} else {
			if (is_union) {
				id = id_unique("__anonymous_union.%u");
			} else {
				id = id_unique("__anonymous_struct.%u");
			}
		}
		dbg_info *dbgi  = get_dbg_info(&type->base.source_position);

		if (is_union) {
			irtype = new_d_type_union(id, dbgi);
		} else {
			irtype = new_d_type_struct(id, dbgi);
		}

		declaration->declaration_kind
			= DECLARATION_KIND_COMPOUND_TYPE_INCOMPLETE;
		declaration->v.irtype         = irtype;
		//type->base.firm_type          = irtype;
	} else {
		offset    = *outer_offset;
		align_all = *outer_align;
	}

	if (incomplete)
		return irtype;

	declaration->declaration_kind = DECLARATION_KIND_COMPOUND_TYPE_COMPLETE;

	declaration_t *entry = declaration->scope.declarations;
	for( ; entry != NULL; entry = entry->next) {
		if (entry->namespc != NAMESPACE_NORMAL)
			continue;

		size_t prev_offset = offset;

		symbol_t *symbol     = entry->symbol;
		type_t   *entry_type = skip_typeref(entry->type);
		dbg_info *dbgi       = get_dbg_info(&entry->source_position);

		ident    *ident;
		if (symbol != NULL) {
			ident = new_id_from_str(symbol->string);
		} else {
			if (entry_type->kind == TYPE_COMPOUND_STRUCT) {
				create_compound_type(&entry_type->compound, irtype, &offset,
		 		                     &align_all, false, COMPOUND_IS_STRUCT);
		 		goto finished_member;
			} else if (entry_type->kind == TYPE_COMPOUND_UNION) {
				create_compound_type(&entry_type->compound, irtype, &offset,
				                     &align_all, false, COMPOUND_IS_UNION);
				goto finished_member;
			} else {
				assert(entry_type->kind == TYPE_BITFIELD);
			}
			ident = id_unique("anon.%u");
		}

		ir_type *base_irtype;
		if (entry_type->kind == TYPE_BITFIELD) {
			base_irtype = get_ir_type(entry_type->bitfield.base_type);
		} else {
			base_irtype = get_ir_type(entry_type);
		}

		size_t entry_alignment = get_type_alignment_bytes(base_irtype);
		size_t misalign        = offset % entry_alignment;

		ir_type   *entry_irtype = get_ir_type(entry_type);
		ir_entity *entity = new_d_entity(irtype, ident, entry_irtype, dbgi);

		size_t base;
		size_t bits_remainder;
		if (entry_type->kind == TYPE_BITFIELD) {
			size_t size_bits      = fold_constant(entry_type->bitfield.size);
			size_t rest_size_bits = (entry_alignment - misalign)*8 - bit_offset;

			if (size_bits > rest_size_bits) {
				/* start a new bucket */
				offset     += entry_alignment - misalign;
				bit_offset  = 0;

				base           = offset;
				bits_remainder = 0;
			} else {
				/* put into current bucket */
				base           = offset - misalign;
				bits_remainder = misalign * 8 + bit_offset;
			}

			offset     += size_bits / 8;
			bit_offset  = bit_offset + (size_bits % 8);
		} else {
			size_t entry_size = get_type_size_bytes(base_irtype);
			if (misalign > 0 || bit_offset > 0)
				offset += entry_alignment - misalign;

			base           = offset;
			bits_remainder = 0;
			offset        += entry_size;
			bit_offset     = 0;
		}

		if (entry_alignment > align_all) {
			if (entry_alignment % align_all != 0) {
				panic("uneven alignments not supported yet");
			}
			align_all = entry_alignment;
		}

		set_entity_offset(entity, base);
		set_entity_offset_bits_remainder(entity,
		                                 (unsigned char) bits_remainder);
		//add_struct_member(irtype, entity);
		entry->declaration_kind = DECLARATION_KIND_COMPOUND_MEMBER;
		assert(entry->v.entity == NULL);
		entry->v.entity         = entity;

finished_member:
		if (is_union) {
			size_t entry_size = offset - prev_offset;
			if (entry_size > size) {
				size = entry_size;
			}
			offset     = 0;
			bit_offset = 0;
		}
	}

	if (!is_union) {
		size = offset;
	}

	size_t misalign = offset % align_all;
	if (misalign > 0 || bit_offset > 0) {
		size += align_all - misalign;
	}

	if (outer_offset != NULL) {
		if (!is_union) {
			*outer_offset = offset;
		} else {
			*outer_offset += size;
		}

		if (align_all > *outer_align) {
			if (align_all % *outer_align != 0) {
				panic("uneven alignments not supported yet");
			}
			*outer_align = align_all;
		}
	} else {
		set_type_alignment_bytes(irtype, align_all);
		set_type_size_bytes(irtype, size);
		set_type_state(irtype, layout_fixed);
	}

	return irtype;
}

static ir_type *create_enum_type(enum_type_t *const type)
{
	type->base.firm_type = ir_type_int;

	ir_mode *const mode    = mode_int;
	tarval  *const one     = get_mode_one(mode);
	tarval  *      tv_next = get_tarval_null(mode);

	bool constant_folding_old = constant_folding;
	constant_folding = true;

	declaration_t *declaration = type->declaration->next;
	for (; declaration != NULL; declaration = declaration->next) {
		if (declaration->storage_class != STORAGE_CLASS_ENUM_ENTRY)
			break;

		declaration->declaration_kind = DECLARATION_KIND_ENUM_ENTRY;

		expression_t *const init = declaration->init.enum_value;
		if (init != NULL) {
			ir_node *const cnst = expression_to_firm(init);
			if (!is_Const(cnst)) {
				panic("couldn't fold constant");
			}
			tv_next = get_Const_tarval(cnst);
		}
		declaration->v.enum_val = tv_next;
		tv_next = tarval_add(tv_next, one);
	}

	constant_folding = constant_folding_old;

	return ir_type_int;
}

static ir_type *get_ir_type_incomplete(type_t *type)
{
	assert(type != NULL);
	type = skip_typeref(type);

	if (type->base.firm_type != NULL) {
		assert(type->base.firm_type != INVALID_TYPE);
		return type->base.firm_type;
	}

	switch (type->kind) {
	case TYPE_COMPOUND_STRUCT:
		return create_compound_type(&type->compound, NULL, NULL, NULL,
		                            true, COMPOUND_IS_STRUCT);
	case TYPE_COMPOUND_UNION:
		return create_compound_type(&type->compound, NULL, NULL, NULL,
		                            true, COMPOUND_IS_UNION);
	default:
		return get_ir_type(type);
	}
}

static ir_type *get_ir_type(type_t *type)
{
	assert(type != NULL);

	type = skip_typeref(type);

	if (type->base.firm_type != NULL) {
		assert(type->base.firm_type != INVALID_TYPE);
		return type->base.firm_type;
	}

	ir_type *firm_type = NULL;
	switch (type->kind) {
	case TYPE_ERROR:
		panic("error type occurred");
	case TYPE_ATOMIC:
		firm_type = create_atomic_type(&type->atomic);
		break;
	case TYPE_COMPLEX:
		firm_type = create_complex_type(&type->complex);
		break;
	case TYPE_IMAGINARY:
		firm_type = create_imaginary_type(&type->imaginary);
		break;
	case TYPE_FUNCTION:
		firm_type = create_method_type(&type->function);
		break;
	case TYPE_POINTER:
		firm_type = create_pointer_type(&type->pointer);
		break;
	case TYPE_ARRAY:
		firm_type = create_array_type(&type->array);
		break;
	case TYPE_COMPOUND_STRUCT:
		firm_type = create_compound_type(&type->compound, NULL, NULL, NULL,
		                                 false, COMPOUND_IS_STRUCT);
		break;
	case TYPE_COMPOUND_UNION:
		firm_type = create_compound_type(&type->compound, NULL, NULL, NULL,
		                                 false, COMPOUND_IS_UNION);
		break;
	case TYPE_ENUM:
		firm_type = create_enum_type(&type->enumt);
		break;
	case TYPE_BUILTIN:
		firm_type = get_ir_type(type->builtin.real_type);
		break;
	case TYPE_BITFIELD:
		firm_type = create_bitfield_type(&type->bitfield);
		break;

	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
	case TYPE_INVALID:
		break;
	}
	if (firm_type == NULL)
		panic("unknown type found");

	type->base.firm_type = firm_type;
	return firm_type;
}

static inline ir_mode *get_ir_mode(type_t *type)
{
	ir_type *irtype = get_ir_type(type);

	/* firm doesn't report a mode for arrays somehow... */
	if (is_Array_type(irtype)) {
		return mode_P_data;
	}

	ir_mode *mode = get_type_mode(irtype);
	assert(mode != NULL);
	return mode;
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

	{ rts_memcpy,     1, "memcpy",       3, _C89 },  /* HMM, man say its C99 */
	{ rts_memset,     1, "memset",       3, _C89 },  /* HMM, man say its C99 */
	{ rts_strcmp,     1, "strcmp",       2, _C89 },
	{ rts_strncmp,    1, "strncmp",      3, _C89 }
};

static ident *rts_idents[sizeof(rts_data) / sizeof(rts_data[0])];

/**
 * Mangles an entity linker (ld) name for win32 usage.
 *
 * @param ent          the entity to be mangled
 * @param declaration  the declaration
 */
static ident *create_ld_ident_win32(ir_entity *ent, declaration_t *declaration)
{
	ident *id;

	if (is_Method_type(get_entity_type(ent)))
		id = decorate_win32_c_fkt(ent, get_entity_ident(ent));
	else {
		/* always add an underscore in win32 */
		id = mangle(id_underscore, get_entity_ident(ent));
	}

	decl_modifiers_t decl_modifiers = declaration->modifiers;
	if (decl_modifiers & DM_DLLIMPORT) {
		/* add prefix for imported symbols */
		id = mangle(id_imp, id);
	}
	return id;
}

/**
 * Mangles an entity linker (ld) name for Linux ELF usage.
 *
 * @param ent          the entity to be mangled
 * @param declaration  the declaration
 */
static ident *create_ld_ident_linux_elf(ir_entity *entity,
                                        declaration_t *declaration)
{
	(void) declaration;
	return get_entity_ident(entity);
}

/**
 * Mangles an entity linker (ld) name for Mach-O usage.
 *
 * @param ent          the entity to be mangled
 * @param declaration  the declaration
 */
static ident *create_ld_ident_macho(ir_entity *ent, declaration_t *declaration)
{
	(void) declaration;
	ident *id = mangle(id_underscore, get_entity_ident(ent));
	return id;
}

typedef ident* (*create_ld_ident_func)(ir_entity *entity,
                                       declaration_t *declaration);
create_ld_ident_func  create_ld_ident = create_ld_ident_linux_elf;

/**
 * Handle GNU attributes for entities
 *
 * @param ent   the entity
 * @param decl  the routine declaration
 */
static void handle_gnu_attributes_ent(ir_entity *ent, declaration_t *decl)
{
	if (decl->modifiers & DM_PURE) {
		/* TRUE if the declaration includes the GNU
		   __attribute__((pure)) specifier. */
		set_entity_additional_property(ent, mtp_property_pure);
	}
	if (decl->modifiers & DM_USED) {
		/* TRUE if the declaration includes the GNU
		   __attribute__((used)) specifier. */
		set_entity_stickyness(ent, stickyness_sticky);
	}
}

/**
 * Creates an entity representing a function.
 *
 * @param declaration  the function declaration
 */
static ir_entity *get_function_entity(declaration_t *declaration)
{
	if (declaration->declaration_kind == DECLARATION_KIND_FUNCTION)
		return declaration->v.entity;
	assert(declaration->declaration_kind == DECLARATION_KIND_UNKNOWN);

	symbol_t *symbol = declaration->symbol;
	ident    *id     = new_id_from_str(symbol->string);

	ir_type  *global_type    = get_glob_type();
	ir_type  *ir_type_method = get_ir_type(declaration->type);
	assert(is_Method_type(ir_type_method));

	/* already an entity defined? */
	ir_entity *entity = entitymap_get(&entitymap, symbol);
	if (entity != NULL) {
		if (get_entity_visibility(entity) == visibility_external_allocated
				&& declaration->init.statement != NULL) {
			set_entity_visibility(entity, visibility_external_visible);
		}
		goto entity_created;
	}

	dbg_info *const dbgi = get_dbg_info(&declaration->source_position);
	entity               = new_d_entity(global_type, id, ir_type_method, dbgi);
	set_entity_ld_ident(entity, create_ld_ident(entity, declaration));

	handle_gnu_attributes_ent(entity, declaration);

	/* static inline             => local
	 * extern inline             => local
	 * inline without definition => local
	 * inline with definition    => external_visible */
	storage_class_tag_t const storage_class = declaration->storage_class;
	bool                const is_inline     = declaration->is_inline;
	bool                const has_body      = declaration->init.statement != NULL;
	if (is_inline && storage_class == STORAGE_CLASS_NONE && has_body) {
		set_entity_visibility(entity, visibility_external_visible);
	} else if (storage_class == STORAGE_CLASS_STATIC ||
	           (is_inline && has_body)) {
		if (!has_body) {
			/* this entity was declared, but is defined nowhere */
			set_entity_peculiarity(entity, peculiarity_description);
		}
		set_entity_visibility(entity, visibility_local);
	} else if (has_body) {
		set_entity_visibility(entity, visibility_external_visible);
	} else {
		set_entity_visibility(entity, visibility_external_allocated);
	}
	set_entity_allocation(entity, allocation_static);

	/* We should check for file scope here, but as long as we compile C only
	   this is not needed. */
	if (! firm_opt.freestanding) {
		/* check for a known runtime function */
		for (size_t i = 0; i < sizeof(rts_data) / sizeof(rts_data[0]); ++i) {
			if (id != rts_idents[i])
				continue;

			/* ignore those rts functions not necessary needed for current mode */
			if ((c_mode & rts_data[i].flags) == 0)
				continue;
			assert(rts_entities[rts_data[i].id] == NULL);
			rts_entities[rts_data[i].id] = entity;
		}
	}

	entitymap_insert(&entitymap, symbol, entity);

entity_created:
	declaration->declaration_kind = DECLARATION_KIND_FUNCTION;
	declaration->v.entity         = entity;

	return entity;
}

/**
 * Creates a Const node representing a constant.
 */
static ir_node *const_to_firm(const const_expression_t *cnst)
{
	dbg_info *dbgi = get_dbg_info(&cnst->base.source_position);
	ir_mode  *mode = get_ir_mode(cnst->base.type);

	char    buf[128];
	tarval *tv;
	size_t  len;
	if (mode_is_float(mode)) {
		tv = new_tarval_from_double(cnst->v.float_value, mode);
	} else {
		if (mode_is_signed(mode)) {
			len = snprintf(buf, sizeof(buf), "%lld", cnst->v.int_value);
		} else {
			len = snprintf(buf, sizeof(buf), "%llu",
			               (unsigned long long) cnst->v.int_value);
		}
		tv = new_tarval_from_str(buf, len, mode);
	}

	return new_d_Const(dbgi, mode, tv);
}

/**
 * Creates a Const node representing a character constant.
 */
static ir_node *character_constant_to_firm(const const_expression_t *cnst)
{
	dbg_info *dbgi = get_dbg_info(&cnst->base.source_position);
	ir_mode  *mode = get_ir_mode(cnst->base.type);

	long long int v = 0;
	for (size_t i = 0; i < cnst->v.character.size; ++i) {
		if (char_is_signed) {
			v = (v << 8) | ((signed char)cnst->v.character.begin[i]);
		} else {
			v = (v << 8) | ((unsigned char)cnst->v.character.begin[i]);
		}
	}
	char    buf[128];
	size_t  len = snprintf(buf, sizeof(buf), "%lld", v);
	tarval *tv = new_tarval_from_str(buf, len, mode);

	return new_d_Const(dbgi, mode, tv);
}

/**
 * Creates a Const node representing a wide character constant.
 */
static ir_node *wide_character_constant_to_firm(const const_expression_t *cnst)
{
	dbg_info *dbgi = get_dbg_info(&cnst->base.source_position);
	ir_mode  *mode = get_ir_mode(cnst->base.type);

	long long int v = cnst->v.wide_character.begin[0];

	char    buf[128];
	size_t  len = snprintf(buf, sizeof(buf), "%lld", v);
	tarval *tv = new_tarval_from_str(buf, len, mode);

	return new_d_Const(dbgi, mode, tv);
}

/**
 * Creates a SymConst for a given entity.
 *
 * @param dbgi    debug info
 * @param mode    the (reference) mode for the SymConst
 * @param entity  the entity
 */
static ir_node *create_symconst(dbg_info *dbgi, ir_mode *mode,
                              ir_entity *entity)
{
	assert(entity != NULL);
	union symconst_symbol sym;
	sym.entity_p = entity;
	return new_d_SymConst(dbgi, mode, sym, symconst_addr_ent);
}

/**
 * Creates a SymConst node representing a string constant.
 *
 * @param src_pos    the source position of the string constant
 * @param id_prefix  a prefix for the name of the generated string constant
 * @param value      the value of the string constant
 */
static ir_node *string_to_firm(const source_position_t *const src_pos,
                               const char *const id_prefix,
                               const string_t *const value)
{
	ir_type  *const global_type = get_glob_type();
	dbg_info *const dbgi        = get_dbg_info(src_pos);
	ir_type  *const type        = new_d_type_array(id_unique("strtype.%u"), 1,
	                                               ir_type_const_char, dbgi);

	ident     *const id     = id_unique(id_prefix);
	ir_entity *const entity = new_d_entity(global_type, id, type, dbgi);
	set_entity_ld_ident(entity, id);
	set_entity_variability(entity, variability_constant);
	set_entity_allocation(entity, allocation_static);

	ir_type *const elem_type = ir_type_const_char;
	ir_mode *const mode      = get_type_mode(elem_type);

	const char* const string = value->begin;
	const size_t      slen   = value->size;

	set_array_lower_bound_int(type, 0, 0);
	set_array_upper_bound_int(type, 0, slen);
	set_type_size_bytes(type, slen);
	set_type_state(type, layout_fixed);

	tarval **const tvs = xmalloc(slen * sizeof(tvs[0]));
	for(size_t i = 0; i < slen; ++i) {
		tvs[i] = new_tarval_from_long(string[i], mode);
	}

	set_array_entity_values(entity, tvs, slen);
	free(tvs);

	return create_symconst(dbgi, mode_P_data, entity);
}

/**
 * Creates a SymConst node representing a string literal.
 *
 * @param literal   the string literal
 */
static ir_node *string_literal_to_firm(
		const string_literal_expression_t* literal)
{
	return string_to_firm(&literal->base.source_position, "Lstr.%u",
	                      &literal->value);
}

/**
 * Creates a SymConst node representing a wide string literal.
 *
 * @param literal   the wide string literal
 */
static ir_node *wide_string_literal_to_firm(
	const wide_string_literal_expression_t* const literal)
{
	ir_type *const global_type = get_glob_type();
	ir_type *const elem_type   = ir_type_wchar_t;
	dbg_info *const dbgi       = get_dbg_info(&literal->base.source_position);
	ir_type *const type        = new_d_type_array(id_unique("strtype.%u"), 1,
	                                            elem_type, dbgi);

	ident     *const id     = id_unique("Lstr.%u");
	ir_entity *const entity = new_d_entity(global_type, id, type, dbgi);
	set_entity_ld_ident(entity, id);
	set_entity_variability(entity, variability_constant);
	set_entity_allocation(entity, allocation_static);

	ir_mode *const mode      = get_type_mode(elem_type);

	const wchar_rep_t *const string = literal->value.begin;
	const size_t             slen   = literal->value.size;

	set_array_lower_bound_int(type, 0, 0);
	set_array_upper_bound_int(type, 0, slen);
	set_type_size_bytes(type, slen);
	set_type_state(type, layout_fixed);

	tarval **const tvs = xmalloc(slen * sizeof(tvs[0]));
	for(size_t i = 0; i < slen; ++i) {
		tvs[i] = new_tarval_from_long(string[i], mode);
	}

	set_array_entity_values(entity, tvs, slen);
	free(tvs);

	return create_symconst(dbgi, mode_P_data, entity);
}

static ir_node *deref_address(dbg_info *const dbgi, type_t *const type,
		                      ir_node *const addr)
{
	ir_type *irtype = get_ir_type(type);
	if (is_compound_type(irtype)
			|| is_Method_type(irtype)
			|| is_Array_type(irtype)) {
		return addr;
	}

	ir_mode *const mode     = get_type_mode(irtype);
	ir_node *const memory   = get_store();
	ir_node *const load     = new_d_Load(dbgi, memory, addr, mode);
	ir_node *const load_mem = new_d_Proj(dbgi, load, mode_M, pn_Load_M);
	ir_node *const load_res = new_d_Proj(dbgi, load, mode,   pn_Load_res);

	if (type->base.qualifiers & TYPE_QUALIFIER_VOLATILE) {
		set_Load_volatility(load, volatility_is_volatile);
	}

	set_store(load_mem);
	return load_res;
}

/**
 * Creates a strict Conv if neccessary.
 */
static ir_node *do_strict_conv(dbg_info *dbgi, ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);

	if (!(get_irg_fp_model(current_ir_graph) & fp_explicit_rounding))
		return node;
	if (!mode_is_float(mode))
		return node;

	/* check if there is already a Conv */
	if (is_Conv(node)) {
		/* convert it into a strict Conv */
		set_Conv_strict(node, 1);
		return node;
	}

	/* otherwise create a new one */
	return new_d_strictConv(dbgi, node, mode);
}

static ir_node *get_global_var_address(dbg_info *const dbgi,
                                       const declaration_t *const decl)
{
	assert(decl->declaration_kind == DECLARATION_KIND_GLOBAL_VARIABLE);

	ir_entity *const entity = decl->v.entity;
	switch ((storage_class_tag_t)decl->storage_class) {
		case STORAGE_CLASS_THREAD:
		case STORAGE_CLASS_THREAD_EXTERN:
		case STORAGE_CLASS_THREAD_STATIC: {
			ir_node *const no_mem = new_NoMem();
			ir_node *const tls    = get_irg_tls(current_ir_graph);
			return new_d_simpleSel(dbgi, no_mem, tls, entity);
		}

		default:
			return create_symconst(dbgi, mode_P_data, entity);
	}
}

/**
 * Returns the correct base address depending on whether it is a parameter or a
 * normal local variable.
 */
static ir_node *get_local_frame(ir_entity *const ent)
{
	ir_graph      *const irg   = current_ir_graph;
	const ir_type *const owner = get_entity_owner(ent);
	if (owner == get_irg_frame_type(irg)) {
		return get_irg_frame(irg);
	} else {
		assert(owner == get_method_value_param_type(get_entity_type(get_irg_entity(irg))));
		return get_irg_value_param_base(irg);
	}
}

static ir_node *create_conv(dbg_info *dbgi, ir_node *value, ir_mode *dest_mode)
{
	ir_mode *value_mode = get_irn_mode(value);

	if (value_mode == dest_mode || is_Bad(value))
		return value;

	if (dest_mode == mode_b) {
		ir_node *zero = new_Const(value_mode, get_mode_null(value_mode));
		ir_node *cmp  = new_d_Cmp(dbgi, value, zero);
		ir_node *proj = new_d_Proj(dbgi, cmp, mode_b, pn_Cmp_Lg);
		return proj;
	}

	return new_d_Conv(dbgi, value, dest_mode);
}

static ir_node *reference_expression_to_firm(const reference_expression_t *ref)
{
	dbg_info      *dbgi        = get_dbg_info(&ref->base.source_position);
	declaration_t *declaration = ref->declaration;
	type_t        *type        = skip_typeref(declaration->type);

	/* make sure the type is constructed */
	(void) get_ir_type(type);

	switch((declaration_kind_t) declaration->declaration_kind) {
	case DECLARATION_KIND_TYPE:
	case DECLARATION_KIND_UNKNOWN:
		break;

	case DECLARATION_KIND_ENUM_ENTRY: {
		ir_mode *const mode = get_ir_mode(type);
		return new_Const(mode, declaration->v.enum_val);
	}

	case DECLARATION_KIND_LOCAL_VARIABLE: {
		ir_mode *const mode = get_ir_mode(type);
		return get_value(declaration->v.value_number, mode);
	}
	case DECLARATION_KIND_FUNCTION: {
		ir_mode *const mode = get_ir_mode(type);
		return create_symconst(dbgi, mode, declaration->v.entity);
	}
	case DECLARATION_KIND_GLOBAL_VARIABLE: {
		ir_node *const addr   = get_global_var_address(dbgi, declaration);
		return deref_address(dbgi, declaration->type, addr);
	}

	case DECLARATION_KIND_LOCAL_VARIABLE_ENTITY: {
		ir_entity *entity = declaration->v.entity;
		ir_node   *frame  = get_local_frame(entity);
		ir_node   *sel    = new_d_simpleSel(dbgi, new_NoMem(), frame, entity);
		return deref_address(dbgi, declaration->type, sel);
	}

	case DECLARATION_KIND_VARIABLE_LENGTH_ARRAY:
		return declaration->v.vla_base;

	case DECLARATION_KIND_COMPOUND_TYPE_INCOMPLETE:
	case DECLARATION_KIND_COMPOUND_TYPE_COMPLETE:
	case DECLARATION_KIND_COMPOUND_MEMBER:
	case DECLARATION_KIND_LABEL_BLOCK:
		panic("not implemented reference type");
	}

	panic("reference to declaration with unknown type found");
}

static ir_node *reference_addr(const reference_expression_t *ref)
{
	dbg_info      *dbgi        = get_dbg_info(&ref->base.source_position);
	declaration_t *declaration = ref->declaration;

	switch((declaration_kind_t) declaration->declaration_kind) {
	case DECLARATION_KIND_TYPE:
	case DECLARATION_KIND_UNKNOWN:
		break;
	case DECLARATION_KIND_LOCAL_VARIABLE:
		/* you can store to a local variable (so we don't panic but return NULL
		 * as an indicator for no real address) */
		return NULL;
	case DECLARATION_KIND_FUNCTION: {
		type_t *const  type = skip_typeref(ref->base.type);
		ir_mode *const mode = get_ir_mode(type);
		return create_symconst(dbgi, mode, declaration->v.entity);
	}
	case DECLARATION_KIND_GLOBAL_VARIABLE: {
		ir_node *const addr = get_global_var_address(dbgi, declaration);
		return addr;
	}
	case DECLARATION_KIND_LOCAL_VARIABLE_ENTITY: {
		ir_entity *entity = declaration->v.entity;
		ir_node   *frame  = get_local_frame(entity);
		ir_node   *sel    = new_d_simpleSel(dbgi, new_NoMem(), frame, entity);

		return sel;
	}

	case DECLARATION_KIND_VARIABLE_LENGTH_ARRAY:
		return declaration->v.vla_base;

	case DECLARATION_KIND_ENUM_ENTRY:
		panic("trying to reference enum entry");

	case DECLARATION_KIND_COMPOUND_TYPE_INCOMPLETE:
	case DECLARATION_KIND_COMPOUND_TYPE_COMPLETE:
	case DECLARATION_KIND_COMPOUND_MEMBER:
	case DECLARATION_KIND_LABEL_BLOCK:
		panic("not implemented reference type");
	}

	panic("reference to declaration with unknown type found");
}

/**
 * Transform calls to builtin functions.
 */
static ir_node *process_builtin_call(const call_expression_t *call)
{
	dbg_info *dbgi = get_dbg_info(&call->base.source_position);

	assert(call->function->kind == EXPR_BUILTIN_SYMBOL);
	builtin_symbol_expression_t *builtin = &call->function->builtin_symbol;

	type_t *type = skip_typeref(builtin->base.type);
	assert(is_type_pointer(type));

	type_t   *function_type = skip_typeref(type->pointer.points_to);
	symbol_t *symbol        = builtin->symbol;

	switch(symbol->ID) {
	case T___builtin_alloca: {
		if (call->arguments == NULL || call->arguments->next != NULL) {
			panic("invalid number of parameters on __builtin_alloca");
		}
		expression_t *argument = call->arguments->expression;
		ir_node      *size     = expression_to_firm(argument);

		ir_node *store  = get_store();
		ir_node *alloca = new_d_Alloc(dbgi, store, size, firm_unknown_type,
		                              stack_alloc);
		ir_node *proj_m = new_Proj(alloca, mode_M, pn_Alloc_M);
		set_store(proj_m);
		ir_node *res    = new_Proj(alloca, mode_P_data, pn_Alloc_res);

		return res;
	}
	case T___builtin_huge_val: {
		ir_mode *mode = get_ir_mode(function_type->function.return_type);
		tarval  *tv   = get_mode_infinite(mode);
		ir_node *res  = new_d_Const(dbgi, mode, tv);
		return   res;
	}
	case T___builtin_nan:
	case T___builtin_nanf:
	case T___builtin_nand: {
		/* Ignore string for now... */
		assert(is_type_function(function_type));
		ir_mode *mode = get_ir_mode(function_type->function.return_type);
		tarval  *tv   = get_mode_NAN(mode);
		ir_node *res  = new_d_Const(dbgi, mode, tv);
		return res;
	}
	case T___builtin_va_end:
		return NULL;
	default:
		panic("Unsupported builtin found\n");
	}
}

/**
 * Transform a call expression.
 * Handles some special cases, like alloca() calls, which must be resolved
 * BEFORE the inlines runs. Inlining routines calling alloca() is dangerous,
 * 176.gcc for instance might allocate 2GB instead of 256 MB if alloca is not
 * handled right...
 */
static ir_node *call_expression_to_firm(const call_expression_t *call)
{
	dbg_info *dbgi  = get_dbg_info(&call->base.source_position);
	assert(get_cur_block() != NULL);

	expression_t *function = call->function;
	if (function->kind == EXPR_BUILTIN_SYMBOL) {
		return process_builtin_call(call);
	}
	if (function->kind == EXPR_REFERENCE) {
		const reference_expression_t *ref = &function->reference;
		declaration_t *declaration = ref->declaration;

		if ((declaration_kind_t)declaration->declaration_kind == DECLARATION_KIND_FUNCTION) {
			if (declaration->v.entity == rts_entities[rts_alloca]) {
				/* handle alloca() call */
				expression_t *argument = call->arguments->expression;
			   	ir_node      *size     = expression_to_firm(argument);

			   	size = create_conv(dbgi, size, get_ir_mode(type_size_t));

	   			ir_node *store  = get_store();
				dbg_info *dbgi  = get_dbg_info(&call->base.source_position);
				ir_node *alloca = new_d_Alloc(dbgi, store, size, firm_unknown_type,
                                              stack_alloc);
				ir_node *proj_m = new_Proj(alloca, mode_M, pn_Alloc_M);
				set_store(proj_m);
				ir_node *res    = new_Proj(alloca, mode_P_data, pn_Alloc_res);

				return res;
			}
		}
	}
	ir_node *callee = expression_to_firm(function);

	type_t *type = skip_typeref(function->base.type);
	assert(is_type_pointer(type));
	pointer_type_t *pointer_type = &type->pointer;
	type_t         *points_to    = skip_typeref(pointer_type->points_to);
	assert(is_type_function(points_to));
	function_type_t *function_type = &points_to->function;

	int      n_parameters = 0;
	ir_type *ir_method_type  = get_ir_type((type_t*) function_type);
	ir_type *new_method_type = NULL;
	if (function_type->variadic || function_type->unspecified_parameters) {
		const call_argument_t *argument = call->arguments;
		for( ; argument != NULL; argument = argument->next) {
			++n_parameters;
		}

		/* we need to construct a new method type matching the call
		 * arguments... */
		int n_res       = get_method_n_ress(ir_method_type);
		dbg_info *dbgi  = get_dbg_info(&call->base.source_position);
		new_method_type = new_d_type_method(id_unique("calltype.%u"),
		                                    n_parameters, n_res, dbgi);
		set_method_calling_convention(new_method_type,
		               get_method_calling_convention(ir_method_type));
		set_method_additional_properties(new_method_type,
		               get_method_additional_properties(ir_method_type));
		set_method_variadicity(new_method_type,
		                       get_method_variadicity(ir_method_type));

		for(int i = 0; i < n_res; ++i) {
			set_method_res_type(new_method_type, i,
			                    get_method_res_type(ir_method_type, i));
		}
		argument = call->arguments;
		for(int i = 0; i < n_parameters; ++i, argument = argument->next) {
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
	for(int n = 0; n < n_parameters; ++n) {
		expression_t *expression = argument->expression;
		ir_node      *arg_node   = expression_to_firm(expression);

		arg_node = do_strict_conv(dbgi, arg_node);

		in[n] = arg_node;

		argument = argument->next;
	}

	ir_node  *store = get_store();
	ir_node  *node  = new_d_Call(dbgi, store, callee, n_parameters, in,
	                             ir_method_type);
	ir_node  *mem   = new_d_Proj(dbgi, node, mode_M, pn_Call_M_regular);
	set_store(mem);

	type_t  *return_type = skip_typeref(function_type->return_type);
	ir_node *result      = NULL;

	if (!is_type_atomic(return_type, ATOMIC_TYPE_VOID)) {
		ir_mode *mode;
		if (is_type_scalar(return_type)) {
			mode = get_ir_mode(return_type);
		} else {
			mode = mode_P_data;
		}
		ir_node *resproj = new_d_Proj(dbgi, node, mode_T, pn_Call_T_result);
		result           = new_d_Proj(dbgi, resproj, mode, 0);
	}

	return result;
}

static void statement_to_firm(statement_t *statement);
static ir_node *compound_statement_to_firm(compound_statement_t *compound);

static ir_node *expression_to_addr(const expression_t *expression);
static void create_condition_evaluation(const expression_t *expression,
                                        ir_node *true_block,
                                        ir_node *false_block);

static void assign_value(dbg_info *dbgi, ir_node *addr, type_t *type,
                         ir_node *value)
{
	value = do_strict_conv(dbgi, value);

	ir_node *memory = get_store();

	if (is_type_scalar(type)) {
		ir_node  *store     = new_d_Store(dbgi, memory, addr, value);
		ir_node  *store_mem = new_d_Proj(dbgi, store, mode_M, pn_Store_M);
		if (type->base.qualifiers & TYPE_QUALIFIER_VOLATILE)
			set_Store_volatility(store, volatility_is_volatile);
		set_store(store_mem);
	} else {
		ir_type *irtype    = get_ir_type(type);
		ir_node *copyb     = new_d_CopyB(dbgi, memory, addr, value, irtype);
		ir_node *copyb_mem = new_Proj(copyb, mode_M, pn_CopyB_M_regular);
		set_store(copyb_mem);
	}
}

static tarval *create_bitfield_mask(ir_mode *mode, int offset, int size)
{
	tarval *all_one   = get_mode_all_one(mode);
	int     mode_size = get_mode_size_bits(mode);

	assert(offset >= 0);
	assert(size   >= 0);
	assert(offset + size <= mode_size);
	if (size == mode_size) {
		return all_one;
	}

	long    shiftr    = get_mode_size_bits(mode) - size;
	long    shiftl    = offset;
	tarval *tv_shiftr = new_tarval_from_long(shiftr, mode_uint);
	tarval *tv_shiftl = new_tarval_from_long(shiftl, mode_uint);
	tarval *mask0     = tarval_shr(all_one, tv_shiftr);
	tarval *mask1     = tarval_shl(mask0, tv_shiftl);

	return mask1;
}

static void bitfield_store_to_firm(const select_expression_t *expression,
                                   ir_node *addr, ir_node *value)
{
	type_t       *type   = expression->base.type;
	ir_mode      *mode   = get_ir_mode(type);

	assert(get_irn_mode(value) == mode || is_Bad(value));

	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);

	/* kill upper bits of value and shift to right position */
	ir_entity *entity       = expression->compound_entry->v.entity;
	int        bitoffset    = get_entity_offset_bits_remainder(entity);
	ir_type   *entity_type  = get_entity_type(entity);
	int        bitsize      = get_mode_size_bits(get_type_mode(entity_type));

	tarval  *mask            = create_bitfield_mask(mode, 0, bitsize);
	ir_node *mask_node       = new_d_Const(dbgi, mode, mask);
	ir_node *value_masked    = new_d_And(dbgi, value, mask_node, mode);
	tarval  *shiftl          = new_tarval_from_long(bitoffset, mode_uint);
	ir_node *shiftcount      = new_d_Const(dbgi, mode_uint, shiftl);
	ir_node *value_maskshift = new_d_Shl(dbgi, value_masked, shiftcount, mode);

	/* load current value */
	ir_node  *mem             = get_store();
	ir_node  *load            = new_d_Load(dbgi, mem, addr, mode);
	ir_node  *load_mem        = new_d_Proj(dbgi, load, mode_M, pn_Load_M);
	ir_node  *load_res        = new_d_Proj(dbgi, load, mode, pn_Load_res);
	tarval   *shift_mask      = create_bitfield_mask(mode, bitoffset, bitsize);
	tarval   *inv_mask        = tarval_not(shift_mask);
	ir_node  *inv_mask_node   = new_d_Const(dbgi, mode, inv_mask);
	ir_node  *load_res_masked = new_d_And(dbgi, load_res, inv_mask_node, mode);

	/* construct new value and store */
	ir_node *new_val   = new_d_Or(dbgi, load_res_masked, value_maskshift, mode);
	ir_node *store     = new_d_Store(dbgi, load_mem, addr, new_val);
	ir_node *store_mem = new_d_Proj(dbgi, store, mode_M, pn_Store_M);
	set_store(store_mem);

	if (type->base.qualifiers & TYPE_QUALIFIER_VOLATILE) {
		set_Load_volatility(load, volatility_is_volatile);
		set_Store_volatility(store, volatility_is_volatile);
	}
}

static ir_node *bitfield_extract_to_firm(const select_expression_t *expression,
		ir_node *addr)
{
	dbg_info *dbgi     = get_dbg_info(&expression->base.source_position);
	type_t   *type     = expression->base.type;
	ir_mode  *mode     = get_ir_mode(type);
	ir_node  *mem      = get_store();
	ir_node  *load     = new_d_Load(dbgi, mem, addr, mode);
	ir_node  *load_mem = new_d_Proj(dbgi, load, mode_M, pn_Load_M);
	ir_node  *load_res = new_d_Proj(dbgi, load, mode, pn_Load_res);

	load_res           = create_conv(dbgi, load_res, mode_int);

	set_store(load_mem);

	/* kill upper bits */
	ir_entity *entity       = expression->compound_entry->v.entity;
	int        bitoffset    = get_entity_offset_bits_remainder(entity);
	ir_type   *entity_type  = get_entity_type(entity);
	int        bitsize      = get_mode_size_bits(get_type_mode(entity_type));
	long       shift_bitsl  = machine_size - bitoffset - bitsize;
	assert(shift_bitsl >= 0);
	tarval    *tvl          = new_tarval_from_long(shift_bitsl, mode_uint);
	ir_node   *countl       = new_d_Const(dbgi, mode_uint, tvl);
	ir_node   *shiftl       = new_d_Shl(dbgi, load_res, countl, mode_int);

	long       shift_bitsr  = bitoffset + shift_bitsl;
	assert(shift_bitsr <= (long) machine_size);
	tarval    *tvr          = new_tarval_from_long(shift_bitsr, mode_uint);
	ir_node   *countr       = new_d_Const(dbgi, mode_uint, tvr);
	ir_node   *shiftr;
	if (mode_is_signed(mode)) {
		shiftr = new_d_Shrs(dbgi, shiftl, countr, mode_int);
	} else {
		shiftr = new_d_Shr(dbgi, shiftl, countr, mode_int);
	}

	return create_conv(dbgi, shiftr, mode);
}

static void set_value_for_expression_addr(const expression_t *expression,
                                          ir_node *value, ir_node *addr)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);
	value          = do_strict_conv(dbgi, value);

	if (expression->kind == EXPR_REFERENCE) {
		const reference_expression_t *ref = &expression->reference;

		declaration_t *declaration = ref->declaration;
		assert(declaration->declaration_kind != DECLARATION_KIND_UNKNOWN);
		if (declaration->declaration_kind == DECLARATION_KIND_LOCAL_VARIABLE) {
			set_value(declaration->v.value_number, value);
			return;
		}
	}

	if (addr == NULL)
		addr = expression_to_addr(expression);

	type_t *type = skip_typeref(expression->base.type);

	if (expression->kind == EXPR_SELECT) {
		const select_expression_t *select = &expression->select;

		declaration_t *declaration = select->compound_entry;
		if (declaration->type->kind == TYPE_BITFIELD) {
			bitfield_store_to_firm(select, addr, value);
			return;
		}
	}

	assign_value(dbgi, addr, type, value);
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

		declaration_t *declaration = ref->declaration;
		assert(declaration->declaration_kind != DECLARATION_KIND_UNKNOWN);
		if (declaration->declaration_kind == DECLARATION_KIND_LOCAL_VARIABLE) {
			assert(addr == NULL);
			ir_mode *mode = get_ir_mode(expression->base.type);
			return get_value(declaration->v.value_number, mode);
		}
	}

	assert(addr != NULL);
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);

	ir_node *value;
	if (expression->kind == EXPR_SELECT &&
	    expression->select.compound_entry->type->kind == TYPE_BITFIELD){
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
	ir_mode *mode = get_ir_mode(expression->base.type);

	ir_node *offset;
	if (is_type_pointer(type)) {
		pointer_type_t *pointer_type = &type->pointer;
		offset                       = get_type_size(pointer_type->points_to);
	} else {
		assert(is_type_arithmetic(type));
		offset = new_Const(mode, get_mode_one(mode));
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
	reference_expression_t *ref_expr    = &expression->reference;
	declaration_t          *declaration = ref_expr->declaration;
	return declaration->declaration_kind == DECLARATION_KIND_LOCAL_VARIABLE;
}

static pn_Cmp get_pnc(const expression_kind_t kind, type_t *const type)
{
	switch(kind) {
	case EXPR_BINARY_EQUAL:         return pn_Cmp_Eq;
	case EXPR_BINARY_ISLESSGREATER: return pn_Cmp_Lg;
	case EXPR_BINARY_NOTEQUAL:
		return is_type_float(skip_typeref(type)) ? pn_Cmp_Ne : pn_Cmp_Lg;
	case EXPR_BINARY_ISLESS:
	case EXPR_BINARY_LESS:          return pn_Cmp_Lt;
	case EXPR_BINARY_ISLESSEQUAL:
	case EXPR_BINARY_LESSEQUAL:     return pn_Cmp_Le;
	case EXPR_BINARY_ISGREATER:
	case EXPR_BINARY_GREATER:       return pn_Cmp_Gt;
	case EXPR_BINARY_ISGREATEREQUAL:
	case EXPR_BINARY_GREATEREQUAL:  return pn_Cmp_Ge;
	case EXPR_BINARY_ISUNORDERED:   return pn_Cmp_Uo;

	default:
		break;
	}
	panic("trying to get pn_Cmp from non-comparison binexpr type");
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
	expression_t  *op1 = expression->left;
	expression_t  *op2 = expression->right;
	declaration_t *var2, *var = NULL;
	ir_node       *res = NULL;
	pn_Cmp         cmp_val;

	cmp_val = get_pnc(expression->base.kind, op1->base.type);

	if (is_local_variable(op1) && is_local_variable(op2)) {
    	var  = op1->reference.declaration;
	    var2 = op2->reference.declaration;

		type_t  *const type = skip_typeref(var->type);
		ir_mode *const mode = get_ir_mode(type);

		ir_node *const irn1 = get_value(var->v.value_number, mode);
		ir_node *const irn2 = get_value(var2->v.value_number, mode);

		res = new_d_Confirm(dbi, irn2, irn1, get_inversed_pnc(cmp_val));
		set_value(var2->v.value_number, res);

		res = new_d_Confirm(dbi, irn1, irn2, cmp_val);
		set_value(var->v.value_number, res);

		return res;
	}

	expression_t *con;
	if (is_local_variable(op1) && is_constant_expression(op2)) {
		var = op1->reference.declaration;
		con = op2;
	} else if (is_constant_expression(op1) && is_local_variable(op2)) {
		cmp_val = get_inversed_pnc(cmp_val);
		var = op2->reference.declaration;
		con = op1;
	}

	if (var != NULL) {
		type_t  *const type = skip_typeref(var->type);
		ir_mode *const mode = get_ir_mode(type);

		res = get_value(var->v.value_number, mode);
		res = new_d_Confirm(dbi, res, expression_to_firm(con), cmp_val);
		set_value(var->v.value_number, res);
	}
	return res;
}

/**
 * Handle the assume optimizer hint.
 *
 * @param dbi    debug info
 * @param expr   the IL assume expression
 */
static ir_node *handle_assume(dbg_info *dbi, const expression_t *expression) {
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

static ir_node *unary_expression_to_firm(const unary_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);
	type_t   *type = skip_typeref(expression->base.type);

	if (expression->base.kind == EXPR_UNARY_TAKE_ADDRESS)
		return expression_to_addr(expression->value);

	const expression_t *value = expression->value;

	switch(expression->base.kind) {
	case EXPR_UNARY_NEGATE: {
		ir_node *value_node = expression_to_firm(value);
		ir_mode *mode = get_ir_mode(type);
		return new_d_Minus(dbgi, value_node, mode);
	}
	case EXPR_UNARY_PLUS:
		return expression_to_firm(value);
	case EXPR_UNARY_BITWISE_NEGATE: {
		ir_node *value_node = expression_to_firm(value);
		ir_mode *mode = get_ir_mode(type);
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
		if (is_type_scalar(type)) {
			ir_mode *mode = get_ir_mode(type);
			ir_node *node = create_conv(dbgi, value_node, mode);
			node = do_strict_conv(dbgi, node);
			return node;
		} else {
			/* make sure firm type is constructed */
			(void) get_ir_type(type);
			return value_node;
		}
	}
	case EXPR_UNARY_CAST_IMPLICIT: {
		ir_node *value_node = expression_to_firm(value);
		if (is_type_scalar(type)) {
			ir_mode *mode = get_ir_mode(type);
			return create_conv(dbgi, value_node, mode);
		} else {
			return value_node;
		}
	}
	case EXPR_UNARY_ASSUME:
		if (firm_opt.confirm)
			return handle_assume(dbgi, value);
		else
			return NULL;

	default:
		break;
	}
	panic("invalid UNEXPR type found");
}

/**
 * produces a 0/1 depending of the value of a mode_b node
 */
static ir_node *produce_condition_result(const expression_t *expression,
                                         dbg_info *dbgi)
{
	ir_mode *mode      = get_ir_mode(expression->base.type);
	ir_node *cur_block = get_cur_block();

	ir_node *one_block = new_immBlock();
	ir_node *one       = new_Const(mode, get_mode_one(mode));
	ir_node *jmp_one   = new_d_Jmp(dbgi);

	ir_node *zero_block = new_immBlock();
	ir_node *zero       = new_Const(mode, get_mode_null(mode));
	ir_node *jmp_zero   = new_d_Jmp(dbgi);

	set_cur_block(cur_block);
	create_condition_evaluation(expression, one_block, zero_block);
	mature_immBlock(one_block);
	mature_immBlock(zero_block);

	ir_node *common_block = new_immBlock();
	add_immBlock_pred(common_block, jmp_one);
	add_immBlock_pred(common_block, jmp_zero);
	mature_immBlock(common_block);

	ir_node *in[2] = { one, zero };
	ir_node *val   = new_d_Phi(dbgi, 2, in, mode);

	return val;
}

static ir_node *adjust_for_pointer_arithmetic(dbg_info *dbgi,
		ir_node *value, type_t *type)
{
	pointer_type_t *const pointer_type = &type->pointer;
	type_t         *const points_to    = skip_typeref(pointer_type->points_to);
	unsigned              elem_size    = get_type_size_const(points_to);

	/* gcc extension */
	if (elem_size == 0 && is_type_atomic(points_to, ATOMIC_TYPE_VOID)) {
		elem_size = 1;
	}

	assert(elem_size >= 1);
	if (elem_size == 1)
		return value;

	value = create_conv(dbgi, value, mode_int);
	ir_node *const cnst = new_Const_long(mode_int, (long)elem_size);
	ir_node *const mul  = new_d_Mul(dbgi, value, cnst, mode_int);
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
		mode  = get_irn_mode(left);
		right = create_conv(dbgi, right, mode_uint);
		break;

	case EXPR_BINARY_SUB:
		if (is_type_pointer(type_left) && is_type_pointer(type_right)) {
			const pointer_type_t *const ptr_type = &type_left->pointer;

			mode = get_ir_mode(expression->base.type);
			ir_node *const elem_size = get_type_size(ptr_type->points_to);
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
			mode  = get_ir_mode(type_left);
			break;
		}
		goto normal_node;

	case EXPR_BINARY_ADD:
	case EXPR_BINARY_ADD_ASSIGN:
		if (is_type_pointer(type_left)) {
			right = adjust_for_pointer_arithmetic(dbgi, right, type_left);
			mode  = get_ir_mode(type_left);
			break;
		} else if (is_type_pointer(type_right)) {
			left  = adjust_for_pointer_arithmetic(dbgi, left, type_right);
			mode  = get_ir_mode(type_right);
			break;
		}
		goto normal_node;

	default:
normal_node:
		mode = get_irn_mode(right);
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
		ir_node *op;
		ir_node *res;
		if (mode_is_float(mode)) {
			op  = new_d_Quot(dbgi, pin, left, right, mode, op_pin_state_floats);
			res = new_d_Proj(dbgi, op, mode, pn_Quot_res);
		} else {
			op  = new_d_Div(dbgi, pin, left, right, mode, op_pin_state_floats);
			res = new_d_Proj(dbgi, op, mode, pn_Div_res);
		}
		return res;
	}
	case EXPR_BINARY_MOD:
	case EXPR_BINARY_MOD_ASSIGN: {
		ir_node *pin = new_Pin(new_NoMem());
		assert(!mode_is_float(mode));
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
	type_t   *type = expression->base.type;
	ir_mode  *mode = get_ir_mode(type);

	if (is_constant_expression(expression->left)) {
		long val = fold_constant(expression->left);
		expression_kind_t ekind = expression->base.kind;
		assert(ekind == EXPR_BINARY_LOGICAL_AND || ekind == EXPR_BINARY_LOGICAL_OR);
		if ((ekind == EXPR_BINARY_LOGICAL_AND && val != 0) ||
		    (ekind == EXPR_BINARY_LOGICAL_OR  && val == 0)) {
			return expression_to_firm(expression->right);
		} else {
			return new_Const(mode, get_mode_one(mode));
		}
	}

	return produce_condition_result((const expression_t*) expression, dbgi);
}

typedef ir_node * (*create_arithmetic_func)(dbg_info *dbgi, ir_node *left,
                                            ir_node *right, ir_mode *mode);

static ir_node *create_assign_binop(const binary_expression_t *expression)
{
	dbg_info *const     dbgi = get_dbg_info(&expression->base.source_position);
	const expression_t *left_expr = expression->left;
	ir_mode            *left_mode = get_ir_mode(left_expr->base.type);
	ir_node            *left_addr = expression_to_addr(left_expr);
	ir_node            *left      = get_value_from_lvalue(left_expr, left_addr);
	ir_node            *right     = expression_to_firm(expression->right);
	ir_node            *result    = create_op(dbgi, expression, left, right);

	result = create_conv(dbgi, result, left_mode);
	result = do_strict_conv(dbgi, result);

	set_value_for_expression_addr(left_expr, result, left_addr);

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
		dbg_info *dbgi = get_dbg_info(&expression->base.source_position);
		ir_node *left  = expression_to_firm(expression->left);
		ir_node *right = expression_to_firm(expression->right);
		ir_node *cmp   = new_d_Cmp(dbgi, left, right);
		long     pnc   = get_pnc(kind, expression->left->base.type);
		ir_node *proj  = new_d_Proj(dbgi, cmp, mode_b, pnc);
		return proj;
	}
	case EXPR_BINARY_ASSIGN: {
		ir_node *addr  = expression_to_addr(expression->left);
		ir_node *right = expression_to_firm(expression->right);
		set_value_for_expression_addr(expression->left, right, addr);

		return right;
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
	case EXPR_BINARY_BUILTIN_EXPECT:
		return _expression_to_firm(expression->left);
	default:
		panic("TODO binexpr type");
	}
}

static ir_node *array_access_addr(const array_access_expression_t *expression)
{
	dbg_info *dbgi      = get_dbg_info(&expression->base.source_position);
	ir_node  *base_addr = expression_to_firm(expression->array_ref);
	ir_node  *offset    = expression_to_firm(expression->index);

	type_t  *offset_type = skip_typeref(expression->index->base.type);
	ir_mode *mode;
	if (is_type_signed(offset_type)) {
		mode = get_ir_mode(type_ssize_t);
	} else {
		mode = get_ir_mode(type_size_t);
	}
	offset = create_conv(dbgi, offset, mode);

	type_t *ref_type = skip_typeref(expression->array_ref->base.type);
	assert(is_type_pointer(ref_type));
	pointer_type_t *pointer_type = &ref_type->pointer;

	ir_node *elem_size_const = get_type_size(pointer_type->points_to);
	elem_size_const          = create_conv(dbgi, elem_size_const, mode);
	ir_node *real_offset     = new_d_Mul(dbgi, offset, elem_size_const,
	                                     mode);
	ir_node *result          = new_d_Add(dbgi, base_addr, real_offset, mode_P_data);

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
	for( ; designator != NULL; designator = designator->next) {
		type_t *type = skip_typeref(orig_type);
		/* be sure the type is constructed */
		(void) get_ir_type(type);

		if (designator->symbol != NULL) {
			assert(is_type_compound(type));
			symbol_t *symbol = designator->symbol;

			declaration_t *declaration = type->compound.declaration;
			declaration_t *iter        = declaration->scope.declarations;
			for( ; iter != NULL; iter = iter->next) {
				if (iter->symbol == symbol) {
					break;
				}
			}
			assert(iter != NULL);

			assert(iter->declaration_kind == DECLARATION_KIND_COMPOUND_MEMBER);
			offset += get_entity_offset(iter->v.entity);

			orig_type = iter->type;
		} else {
			expression_t *array_index = designator->array_index;
			assert(designator->array_index != NULL);
			assert(is_type_array(type));
			assert(is_type_valid(array_index->base.type));

			long index         = fold_constant(array_index);
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
	ir_mode  *mode   = get_ir_mode(expression->base.type);
	long      offset = get_offsetof_offset(expression);
	tarval   *tv     = new_tarval_from_long(offset, mode);
	dbg_info *dbgi   = get_dbg_info(&expression->base.source_position);

	return new_d_Const(dbgi, mode, tv);
}

static void create_local_initializer(initializer_t *initializer, dbg_info *dbgi,
                                     ir_entity *entity, type_t *type);

static ir_node *compound_literal_to_firm(
		const compound_literal_expression_t *expression)
{
	type_t *type = expression->type;

	/* create an entity on the stack */
	ir_type *frame_type = get_irg_frame_type(current_ir_graph);

	ident     *const id     = id_unique("CompLit.%u");
	ir_type   *const irtype = get_ir_type(type);
	dbg_info  *const dbgi   = get_dbg_info(&expression->base.source_position);
	ir_entity *const entity = new_d_entity(frame_type, id, irtype, dbgi);
	set_entity_ld_ident(entity, id);

	set_entity_variability(entity, variability_uninitialized);

	/* create initialisation code */
	initializer_t *initializer = expression->initializer;
	create_local_initializer(initializer, dbgi, entity, type);

	/* create a sel for the compound literal address */
	ir_node *frame = get_local_frame(entity);
	ir_node *sel   = new_d_simpleSel(dbgi, new_NoMem(), frame, entity);
	return sel;
}

/**
 * Transform a sizeof expression into Firm code.
 */
static ir_node *sizeof_to_firm(const typeprop_expression_t *expression)
{
	type_t *type = expression->type;
	if (type == NULL) {
		type = expression->tp_expression->base.type;
		assert(type != NULL);
	}

	type = skip_typeref(type);
	/* § 6.5.3.4 (2) if the type is a VLA, evaluate the expression. */
	if (is_type_array(type) && type->array.is_vla
			&& expression->tp_expression != NULL) {
		expression_to_firm(expression->tp_expression);
	}

	return get_type_size(type);
}

/**
 * Transform an alignof expression into Firm code.
 */
static ir_node *alignof_to_firm(const typeprop_expression_t *expression)
{
	type_t *type = expression->type;
	if (type == NULL) {
		/* beware: if expression is a variable reference, return the
		   alignment of the variable. */
		const expression_t *tp_expression = expression->tp_expression;
		const declaration_t *declaration = expr_is_variable(tp_expression);
		if (declaration != NULL) {
			/* TODO: get the alignment of this variable. */
		}
		type = tp_expression->base.type;
		assert(type != NULL);
	}

	ir_mode *const mode = get_ir_mode(expression->base.type);
	symconst_symbol sym;
	sym.type_p = get_ir_type(type);
	return new_SymConst(mode, sym, symconst_type_align);
}

static void init_ir_types(void);
long fold_constant(const expression_t *expression)
{
	bool constant_folding_old = constant_folding;
	constant_folding = true;

	init_ir_types();

	assert(is_constant_expression(expression));

	ir_graph *old_current_ir_graph = current_ir_graph;
	if (current_ir_graph == NULL) {
		current_ir_graph = get_const_code_irg();
	}

	ir_node *cnst = expression_to_firm(expression);
	current_ir_graph = old_current_ir_graph;

	if (!is_Const(cnst)) {
		panic("couldn't fold constant\n");
	}

	tarval *tv = get_Const_tarval(cnst);
	if (!tarval_is_long(tv)) {
		panic("result of constant folding is not integer\n");
	}

	constant_folding = constant_folding_old;

	return get_tarval_long(tv);
}

static ir_node *conditional_to_firm(const conditional_expression_t *expression)
{
	dbg_info *const dbgi = get_dbg_info(&expression->base.source_position);

	/* first try to fold a constant condition */
	if (is_constant_expression(expression->condition)) {
		long val = fold_constant(expression->condition);
		if (val) {
			return expression_to_firm(expression->true_expression);
		} else {
			return expression_to_firm(expression->false_expression);
		}
	}

	ir_node *cur_block   = get_cur_block();

	/* create the true block */
	ir_node *true_block  = new_immBlock();

	ir_node *true_val = expression_to_firm(expression->true_expression);
	ir_node *true_jmp = new_Jmp();

	/* create the false block */
	ir_node *false_block = new_immBlock();

	ir_node *false_val = expression_to_firm(expression->false_expression);
	ir_node *false_jmp = new_Jmp();

	/* create the condition evaluation */
	set_cur_block(cur_block);
	create_condition_evaluation(expression->condition, true_block, false_block);
	mature_immBlock(true_block);
	mature_immBlock(false_block);

	/* create the common block */
	ir_node *common_block = new_immBlock();
	add_immBlock_pred(common_block, true_jmp);
	add_immBlock_pred(common_block, false_jmp);
	mature_immBlock(common_block);

	/* TODO improve static semantics, so either both or no values are NULL */
	if (true_val == NULL || false_val == NULL)
		return NULL;

	ir_node *in[2] = { true_val, false_val };
	ir_mode *mode  = get_irn_mode(true_val);
	assert(get_irn_mode(false_val) == mode);
	ir_node *val   = new_d_Phi(dbgi, 2, in, mode);

	return val;
}

/**
 * Returns an IR-node representing the address of a field.
 */
static ir_node *select_addr(const select_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);

	ir_node *compound_addr = expression_to_firm(expression->compound);

	/* make sure the type is constructed */
	type_t *type = skip_typeref(expression->compound->base.type);
	if (is_type_pointer(type)) {
		type = type->pointer.points_to;
	}
	(void) get_ir_type(type);

	declaration_t *entry = expression->compound_entry;
	assert(entry->declaration_kind == DECLARATION_KIND_COMPOUND_MEMBER);
	ir_entity     *entity = entry->v.entity;

	assert(entity != NULL);

	ir_node *sel = new_d_simpleSel(dbgi, new_NoMem(), compound_addr, entity);

	return sel;
}

static ir_node *select_to_firm(const select_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);
	ir_node  *addr = select_addr(expression);
	type_t   *type = revert_automatic_type_conversion(
			(const expression_t*) expression);
	type           = skip_typeref(type);

	declaration_t *entry      = expression->compound_entry;
	type_t        *entry_type = skip_typeref(entry->type);

	if (entry_type->kind == TYPE_BITFIELD) {
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
	const type_t *const type = skip_typeref(expr->type_expression->base.type);

	gcc_type_class tc;
	switch (type->kind)
	{
		case TYPE_ATOMIC: {
			const atomic_type_t *const atomic_type = &type->atomic;
			switch (atomic_type->akind) {
				/* should not be reached */
				case ATOMIC_TYPE_INVALID:
					tc = no_type_class;
					goto make_const;

				/* gcc cannot do that */
				case ATOMIC_TYPE_VOID:
					tc = void_type_class;
					goto make_const;

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
		case TYPE_BITFIELD:        tc = integer_type_class; goto make_const;
		case TYPE_ARRAY:           /* gcc handles this as pointer */
		case TYPE_FUNCTION:        /* gcc handles this as pointer */
		case TYPE_POINTER:         tc = pointer_type_class; goto make_const;
		case TYPE_COMPOUND_STRUCT: tc = record_type_class;  goto make_const;
		case TYPE_COMPOUND_UNION:  tc = union_type_class;   goto make_const;

		/* gcc handles this as integer */
		case TYPE_ENUM:            tc = integer_type_class; goto make_const;

		case TYPE_BUILTIN:
		/* typedef/typeof should be skipped already */
		case TYPE_TYPEDEF:
		case TYPE_TYPEOF:
		case TYPE_INVALID:
		case TYPE_ERROR:
			break;
	}
	panic("unexpected TYPE classify_type_to_firm().");

make_const: ;
	dbg_info *const dbgi = get_dbg_info(&expr->base.source_position);
	ir_mode  *const mode = mode_int;
	tarval   *const tv   = new_tarval_from_long(tc, mode);
	return new_d_Const(dbgi, mode, tv);
}

static ir_node *function_name_to_firm(
		const funcname_expression_t *const expr)
{
	switch(expr->kind) {
	case FUNCNAME_FUNCTION:
	case FUNCNAME_PRETTY_FUNCTION:
	case FUNCNAME_FUNCDNAME:
		if (current_function_name == NULL) {
			const source_position_t *const src_pos = &expr->base.source_position;
			const char *const name = current_function_decl->symbol->string;
			const string_t string = { name, strlen(name) + 1 };
			current_function_name = string_to_firm(src_pos, "__func__.%u", &string);
		}
		return current_function_name;
	case FUNCNAME_FUNCSIG:
		if (current_funcsig == NULL) {
			const source_position_t *const src_pos = &expr->base.source_position;
			ir_entity *ent = get_irg_entity(current_ir_graph);
			const char *const name = get_entity_ld_name(ent);
			const string_t string = { name, strlen(name) + 1 };
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
	ir_type   *const method_type = get_ir_type(current_function_decl->type);
	int        const n           = get_method_n_params(method_type) - 1;
	ir_entity *const parm_ent    = get_method_value_param_ent(method_type, n);
	ir_node   *const arg_base    = get_irg_value_param_base(current_ir_graph);
	dbg_info  *const dbgi        = get_dbg_info(&expr->base.source_position);
	ir_node   *const no_mem      = new_NoMem();
	ir_node   *const arg_sel     =
		new_d_simpleSel(dbgi, no_mem, arg_base, parm_ent);

	ir_node   *const cnst        = get_type_size(expr->parameter->type);
	ir_node   *const add         = new_d_Add(dbgi, arg_sel, cnst, mode_P_data);
	set_value_for_expression(expr->ap, add);

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

	ir_node      *const cnst    = get_type_size(expr->base.type);
	ir_node      *const add     = new_d_Add(dbgi, ap, cnst, mode_P_data);

	set_value_for_expression_addr(ap_expr, add, ap_addr);

	return res;
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
	case EXPR_REFERENCE:
		return reference_addr(&expression->reference);
	case EXPR_ARRAY_ACCESS:
		return array_access_addr(&expression->array_access);
	case EXPR_SELECT:
		return select_addr(&expression->select);
	case EXPR_CALL:
		return call_expression_to_firm(&expression->call);
	case EXPR_UNARY_DEREFERENCE: {
		return dereference_addr(&expression->unary);
	}
	default:
		break;
	}
	panic("trying to get address of non-lvalue");
}

static ir_node *builtin_constant_to_firm(
		const builtin_constant_expression_t *expression)
{
	ir_mode *mode = get_ir_mode(expression->base.type);
	long     v;

	if (is_constant_expression(expression->value)) {
		v = 1;
	} else {
		v = 0;
	}
	return new_Const_long(mode, v);
}

static ir_node *builtin_prefetch_to_firm(
		const builtin_prefetch_expression_t *expression)
{
	ir_node *adr = expression_to_firm(expression->adr);
	/* no Firm support for prefetch yet */
	(void) adr;
	return NULL;
}

/**
 * creates firm nodes for an expression. The difference between this function
 * and expression_to_firm is, that this version might produce mode_b nodes
 * instead of mode_Is.
 */
static ir_node *_expression_to_firm(const expression_t *expression)
{
#ifndef NDEBUG
	if (!constant_folding) {
		assert(!expression->base.transformed);
		((expression_t*) expression)->base.transformed = true;
	}
#endif

	switch(expression->kind) {
	case EXPR_CHARACTER_CONSTANT:
		return character_constant_to_firm(&expression->conste);
	case EXPR_WIDE_CHARACTER_CONSTANT:
		return wide_character_constant_to_firm(&expression->conste);
	case EXPR_CONST:
		return const_to_firm(&expression->conste);
	case EXPR_STRING_LITERAL:
		return string_literal_to_firm(&expression->string);
	case EXPR_WIDE_STRING_LITERAL:
		return wide_string_literal_to_firm(&expression->wide_string);
	case EXPR_REFERENCE:
		return reference_expression_to_firm(&expression->reference);
	case EXPR_CALL:
		return call_expression_to_firm(&expression->call);
	EXPR_UNARY_CASES
		return unary_expression_to_firm(&expression->unary);
	EXPR_BINARY_CASES
		return binary_expression_to_firm(&expression->binary);
	case EXPR_ARRAY_ACCESS:
		return array_access_to_firm(&expression->array_access);
	case EXPR_SIZEOF:
		return sizeof_to_firm(&expression->typeprop);
	case EXPR_ALIGNOF:
		return alignof_to_firm(&expression->typeprop);
	case EXPR_CONDITIONAL:
		return conditional_to_firm(&expression->conditional);
	case EXPR_SELECT:
		return select_to_firm(&expression->select);
	case EXPR_CLASSIFY_TYPE:
		return classify_type_to_firm(&expression->classify_type);
	case EXPR_FUNCNAME:
		return function_name_to_firm(&expression->funcname);
	case EXPR_STATEMENT:
		return statement_expression_to_firm(&expression->statement);
	case EXPR_VA_START:
		return va_start_expression_to_firm(&expression->va_starte);
	case EXPR_VA_ARG:
		return va_arg_expression_to_firm(&expression->va_arge);
	case EXPR_BUILTIN_SYMBOL:
		panic("unimplemented expression found");
	case EXPR_BUILTIN_CONSTANT_P:
		return builtin_constant_to_firm(&expression->builtin_constant);
	case EXPR_BUILTIN_PREFETCH:
		return builtin_prefetch_to_firm(&expression->builtin_prefetch);
	case EXPR_OFFSETOF:
		return offsetof_to_firm(&expression->offsetofe);
	case EXPR_COMPOUND_LITERAL:
		return compound_literal_to_firm(&expression->compound_literal);

	case EXPR_UNKNOWN:
	case EXPR_INVALID:
		break;
	}
	panic("invalid expression found");
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

	case EXPR_BINARY_BUILTIN_EXPECT:
		return produces_mode_b(expression->binary.left);
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

	if (is_constant_expression(expression)) {
		ir_node *res  = _expression_to_firm(expression);
		ir_mode *mode = get_ir_mode(expression->base.type);
		assert(is_Const(res));
		if (is_Const_null(res)) {
			return new_Const_long(mode, 0);
		} else {
			return new_Const_long(mode, 1);
		}
	}

	/* we have to produce a 0/1 from the mode_b expression */
	dbg_info *dbgi = get_dbg_info(&expression->base.source_position);
	return produce_condition_result(expression, dbgi);
}

static ir_node *expression_to_modeb(const expression_t *expression)
{
	ir_node *res = _expression_to_firm(expression);
	res          = create_conv(NULL, res, mode_b);

	return res;
}

/**
 * create a short-circuit expression evaluation that tries to construct
 * efficient control flow structures for &&, || and ! expressions
 */
static void create_condition_evaluation(const expression_t *expression,
                                        ir_node *true_block,
                                        ir_node *false_block)
{
	switch(expression->kind) {
	case EXPR_UNARY_NOT: {
		const unary_expression_t *unary_expression = &expression->unary;
		create_condition_evaluation(unary_expression->value, false_block,
		                            true_block);
		return;
	}
	case EXPR_BINARY_LOGICAL_AND: {
		const binary_expression_t *binary_expression = &expression->binary;

		ir_node *cur_block   = get_cur_block();
		ir_node *extra_block = new_immBlock();
		set_cur_block(cur_block);
		create_condition_evaluation(binary_expression->left, extra_block,
		                            false_block);
		mature_immBlock(extra_block);
		set_cur_block(extra_block);
		create_condition_evaluation(binary_expression->right, true_block,
		                            false_block);
		return;
	}
	case EXPR_BINARY_LOGICAL_OR: {
		const binary_expression_t *binary_expression = &expression->binary;

		ir_node *cur_block   = get_cur_block();
		ir_node *extra_block = new_immBlock();
		set_cur_block(cur_block);
		create_condition_evaluation(binary_expression->left, true_block,
		                            extra_block);
		mature_immBlock(extra_block);
		set_cur_block(extra_block);
		create_condition_evaluation(binary_expression->right, true_block,
		                            false_block);
		return;
	}
	default:
		break;
	}

	dbg_info *dbgi       = get_dbg_info(&expression->base.source_position);
	ir_node  *condition  = expression_to_modeb(expression);
	ir_node  *cond       = new_d_Cond(dbgi, condition);
	ir_node  *true_proj  = new_d_Proj(dbgi, cond, mode_X, pn_Cond_true);
	ir_node  *false_proj = new_d_Proj(dbgi, cond, mode_X, pn_Cond_false);

	/* set branch prediction info based on __builtin_expect */
	if (expression->kind == EXPR_BINARY_BUILTIN_EXPECT) {
		long               cnst = fold_constant(expression->binary.right);
		cond_jmp_predicate pred;

		if (cnst == 0) {
			pred = COND_JMP_PRED_FALSE;
		} else {
			pred = COND_JMP_PRED_TRUE;
		}
		set_Cond_jmp_pred(cond, pred);
	}

	add_immBlock_pred(true_block, true_proj);
	if (false_block != NULL) {
		add_immBlock_pred(false_block, false_proj);
	}

	set_cur_block(NULL);
}



static void create_declaration_entity(declaration_t *declaration,
                                      declaration_kind_t declaration_kind,
                                      ir_type *parent_type)
{
	type_t    *const type   = skip_typeref(declaration->type);
	if (is_type_function(type)) {
		(void) get_function_entity(declaration);
		return;
	}

	ident     *const id     = new_id_from_str(declaration->symbol->string);
	ir_type   *const irtype = get_ir_type(type);
	dbg_info  *const dbgi   = get_dbg_info(&declaration->source_position);
	ir_entity *const entity = new_d_entity(parent_type, id, irtype, dbgi);

	declaration->declaration_kind = (unsigned char) declaration_kind;
	declaration->v.entity         = entity;
	set_entity_variability(entity, variability_uninitialized);
	set_entity_ld_ident(entity, create_ld_ident(entity, declaration));
	if (parent_type == get_tls_type())
		set_entity_allocation(entity, allocation_automatic);
	else if (declaration_kind == DECLARATION_KIND_GLOBAL_VARIABLE)
		set_entity_allocation(entity, allocation_static);

	if (type->base.qualifiers & TYPE_QUALIFIER_VOLATILE) {
		set_entity_volatility(entity, volatility_is_volatile);
	}
}


typedef struct type_path_entry_t type_path_entry_t;
struct type_path_entry_t {
	type_t           *type;
	ir_initializer_t *initializer;
	size_t            index;
	declaration_t    *compound_entry;
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

	for(size_t i = 0; i < len; ++i) {
		const type_path_entry_t *entry = & path->path[i];

		type_t *type = skip_typeref(entry->type);
		if (is_type_compound(type)) {
			fprintf(stderr, ".%s", entry->compound_entry->symbol->string);
		} else if (is_type_array(type)) {
			fprintf(stderr, "[%zd]", entry->index);
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

static size_t get_compound_size(const compound_type_t *type)
{
	declaration_t *declaration = type->declaration;
	declaration_t *member      = declaration->scope.declarations;
	size_t         size        = 0;
	for( ; member != NULL; member = member->next) {
		++size;
	}
	/* TODO: cache results? */

	return size;
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
		declaration_t *declaration = top_type->compound.declaration;
		declaration_t *entry       = declaration->scope.declarations;

		top->compound_entry = entry;
		top->index          = 0;
		len                 = get_compound_size(&top_type->compound);
		if (entry != NULL)
			path->top_type = entry->type;
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

	for( ; designator != NULL; designator = designator->next) {
		type_path_entry_t *top         = get_type_path_top(path);
		type_t            *orig_type   = top->type;
		type_t            *type        = skip_typeref(orig_type);

		if (designator->symbol != NULL) {
			assert(is_type_compound(type));
			size_t    index  = 0;
			symbol_t *symbol = designator->symbol;

			declaration_t *declaration = type->compound.declaration;
			declaration_t *iter        = declaration->scope.declarations;
			for( ; iter != NULL; iter = iter->next, ++index) {
				if (iter->symbol == symbol) {
					break;
				}
			}
			assert(iter != NULL);

			top->type           = orig_type;
			top->compound_entry = iter;
			top->index          = index;
			orig_type           = iter->type;
		} else {
			expression_t *array_index = designator->array_index;
			assert(designator->array_index != NULL);
			assert(is_type_array(type));
			assert(is_type_valid(array_index->base.type));

			long index = fold_constant(array_index);
			assert(index >= 0);
#ifndef NDEBUG
			if (type->array.size_constant == 1) {
				long array_size = type->array.size;
				assert(index < array_size);
			}
#endif

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
		return;
	}

	type_path_entry_t *top = get_type_path_top(path);

	type_t *type = skip_typeref(top->type);
	if (is_type_union(type)) {
		top->compound_entry = NULL;
	} else if (is_type_struct(type)) {
		declaration_t *entry = top->compound_entry;

		top->index++;
		entry               = entry->next;
		top->compound_entry = entry;
		if (entry != NULL) {
			path->top_type = entry->type;
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


static ir_initializer_t *create_ir_initializer(
		const initializer_t *initializer, type_t *type);

static ir_initializer_t *create_ir_initializer_value(
		const initializer_value_t *initializer)
{
	if (is_type_compound(initializer->value->base.type)) {
		panic("initializer creation for compounds not implemented yet");
	}
	ir_node *value = expression_to_firm(initializer->value);
	return create_initializer_const(value);
}

static ir_initializer_t *create_ir_initializer_list(
		const initializer_list_t *initializer, type_t *type)
{
	type_path_t path;
	memset(&path, 0, sizeof(path));
	path.top_type = type;
	path.path     = NEW_ARR_F(type_path_entry_t, 0);

	descend_into_subtype(&path);

	for(size_t i = 0; i < initializer->len; ++i) {
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

static ir_initializer_t *create_ir_initializer_string(
		const initializer_string_t *initializer, type_t *type)
{
	type = skip_typeref(type);

	size_t            string_len    = initializer->string.size;
	assert(type->kind == TYPE_ARRAY);
	assert(type->array.size_constant);
	size_t            len           = type->array.size;
	ir_initializer_t *irinitializer = create_initializer_compound(len);

	const char *string = initializer->string.begin;
	ir_mode    *mode   = get_ir_mode(type->array.element_type);

	for(size_t i = 0; i < len; ++i) {
		char c = 0;
		if (i < string_len)
			c = string[i];

		tarval           *tv = new_tarval_from_long(c, mode);
		ir_initializer_t *char_initializer = create_initializer_tarval(tv);

		set_initializer_compound_value(irinitializer, i, char_initializer);
	}

	return irinitializer;
}

static ir_initializer_t *create_ir_initializer_wide_string(
		const initializer_wide_string_t *initializer, type_t *type)
{
	size_t            string_len    = initializer->string.size;
	assert(type->kind == TYPE_ARRAY);
	assert(type->array.size_constant);
	size_t            len           = type->array.size;
	ir_initializer_t *irinitializer = create_initializer_compound(len);

	const wchar_rep_t *string = initializer->string.begin;
	ir_mode           *mode   = get_type_mode(ir_type_wchar_t);

	for(size_t i = 0; i < len; ++i) {
		wchar_rep_t c = 0;
		if (i < string_len) {
			c = string[i];
		}
		tarval *tv = new_tarval_from_long(c, mode);
		ir_initializer_t *char_initializer = create_initializer_tarval(tv);

		set_initializer_compound_value(irinitializer, i, char_initializer);
	}

	return irinitializer;
}

static ir_initializer_t *create_ir_initializer(
		const initializer_t *initializer, type_t *type)
{
	switch(initializer->kind) {
		case INITIALIZER_STRING:
			return create_ir_initializer_string(&initializer->string, type);

		case INITIALIZER_WIDE_STRING:
			return create_ir_initializer_wide_string(&initializer->wide_string,
			                                         type);

		case INITIALIZER_LIST:
			return create_ir_initializer_list(&initializer->list, type);

		case INITIALIZER_VALUE:
			return create_ir_initializer_value(&initializer->value);

		case INITIALIZER_DESIGNATOR:
			panic("unexpected designator initializer found");
	}
	panic("unknown initializer");
}

static void create_dynamic_null_initializer(ir_type *type, dbg_info *dbgi,
		ir_node *base_addr)
{
	if (is_atomic_type(type)) {
		ir_mode *mode = get_type_mode(type);
		tarval  *zero = get_mode_null(mode);
		ir_node *cnst = new_d_Const(dbgi, mode, zero);

		/* TODO: bitfields */
		ir_node *mem    = get_store();
		ir_node *store  = new_d_Store(dbgi, mem, base_addr, cnst);
		ir_node *proj_m = new_Proj(store, mode_M, pn_Store_M);
		set_store(proj_m);
	} else {
		assert(is_compound_type(type));

		int n_members;
		if (is_Array_type(type)) {
			assert(has_array_upper_bound(type, 0));
			n_members = get_array_upper_bound_int(type, 0);
		} else {
			n_members = get_compound_n_members(type);
		}

		for(int i = 0; i < n_members; ++i) {
			ir_node *addr;
			ir_type *irtype;
			if (is_Array_type(type)) {
				ir_entity *entity   = get_array_element_entity(type);
				tarval    *index_tv = new_tarval_from_long(i, mode_uint);
				ir_node   *cnst     = new_d_Const(dbgi, mode_uint, index_tv);
				ir_node   *in[1]    = { cnst };
				irtype = get_array_element_type(type);
				addr   = new_d_Sel(dbgi, new_NoMem(), base_addr, 1, in, entity);
			} else {
				ir_entity *member = get_compound_member(type, i);

				irtype = get_entity_type(member);
				addr   = new_d_simpleSel(dbgi, new_NoMem(), base_addr, member);
			}

			create_dynamic_null_initializer(irtype, dbgi, addr);
		}
	}
}

static void create_dynamic_initializer_sub(ir_initializer_t *initializer,
		ir_type *type, dbg_info *dbgi, ir_node *base_addr)
{
	switch(get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL: {
		create_dynamic_null_initializer(type, dbgi, base_addr);
		return;
	}
	case IR_INITIALIZER_CONST: {
		ir_node *node = get_initializer_const_value(initializer);
		ir_mode *mode = get_irn_mode(node);
		assert(get_type_mode(type) == mode);

		/* TODO: bitfields... */
		ir_node *mem    = get_store();
		ir_node *store  = new_d_Store(dbgi, mem, base_addr, node);
		ir_node *proj_m = new_Proj(store, mode_M, pn_Store_M);
		set_store(proj_m);
		return;
	}
	case IR_INITIALIZER_TARVAL: {
		tarval  *tv   = get_initializer_tarval_value(initializer);
		ir_mode *mode = get_tarval_mode(tv);
		ir_node *cnst = new_d_Const(dbgi, mode, tv);
		assert(get_type_mode(type) == mode);

		/* TODO: bitfields... */
		ir_node *mem    = get_store();
		ir_node *store  = new_d_Store(dbgi, mem, base_addr, cnst);
		ir_node *proj_m = new_Proj(store, mode_M, pn_Store_M);
		set_store(proj_m);
		return;
	}
	case IR_INITIALIZER_COMPOUND: {
		assert(is_compound_type(type));
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

		for(int i = 0; i < n_members; ++i) {
			ir_node *addr;
			ir_type *irtype;
			if (is_Array_type(type)) {
				ir_entity *entity   = get_array_element_entity(type);
				tarval    *index_tv = new_tarval_from_long(i, mode_uint);
				ir_node   *cnst     = new_d_Const(dbgi, mode_uint, index_tv);
				ir_node   *in[1]    = { cnst };
				irtype = get_array_element_type(type);
				addr   = new_d_Sel(dbgi, new_NoMem(), base_addr, 1, in, entity);
			} else {
				ir_entity *member = get_compound_member(type, i);

				irtype = get_entity_type(member);
				addr   = new_d_simpleSel(dbgi, new_NoMem(), base_addr, member);
			}

			ir_initializer_t *sub_init
				= get_initializer_compound_value(initializer, i);

			create_dynamic_initializer_sub(sub_init, irtype, dbgi, addr);
		}
		return;
	}
	}

	panic("invalid IR_INITIALIZER found");
}

static void create_dynamic_initializer(ir_initializer_t *initializer,
		dbg_info *dbgi, ir_entity *entity)
{
	ir_node *frame     = get_local_frame(entity);
	ir_node *base_addr = new_d_simpleSel(dbgi, new_NoMem(), frame, entity);
	ir_type *type      = get_entity_type(entity);

	create_dynamic_initializer_sub(initializer, type, dbgi, base_addr);
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

	if (!is_constant_initializer(initializer)) {
		ir_initializer_t *irinitializer
			= create_ir_initializer(initializer, type);

		create_dynamic_initializer(irinitializer, dbgi, entity);
		return;
	}

	/* create the ir_initializer */
	ir_graph *const old_current_ir_graph = current_ir_graph;
	current_ir_graph = get_const_code_irg();

	ir_initializer_t *irinitializer = create_ir_initializer(initializer, type);

	assert(current_ir_graph == get_const_code_irg());
	current_ir_graph = old_current_ir_graph;

	/* create a "template" entity which is copied to the entity on the stack */
	ident     *const id          = id_unique("initializer.%u");
	ir_type   *const irtype      = get_ir_type(type);
	ir_type   *const global_type = get_glob_type();
	ir_entity *const init_entity = new_d_entity(global_type, id, irtype, dbgi);
	set_entity_ld_ident(init_entity, id);

	set_entity_variability(init_entity, variability_initialized);
	set_entity_visibility(init_entity, visibility_local);
	set_entity_allocation(init_entity, allocation_static);

	set_entity_initializer(init_entity, irinitializer);

	ir_node *const src_addr = create_symconst(dbgi, mode_P_data, init_entity);
	ir_node *const copyb    = new_d_CopyB(dbgi, memory, addr, src_addr, irtype);

	ir_node *const copyb_mem = new_Proj(copyb, mode_M, pn_CopyB_M_regular);
	set_store(copyb_mem);
}

static void create_initializer_local_variable_entity(declaration_t *declaration)
{
	initializer_t *initializer = declaration->init.initializer;
	dbg_info      *dbgi        = get_dbg_info(&declaration->source_position);
	ir_entity     *entity      = declaration->v.entity;
	type_t        *type        = declaration->type;
	create_local_initializer(initializer, dbgi, entity, type);
}

static void create_declaration_initializer(declaration_t *declaration)
{
	initializer_t *initializer = declaration->init.initializer;
	if (initializer == NULL)
		return;

	declaration_kind_t declaration_kind
		= (declaration_kind_t) declaration->declaration_kind;
	if (declaration_kind == DECLARATION_KIND_LOCAL_VARIABLE_ENTITY) {
		create_initializer_local_variable_entity(declaration);
		return;
	}

	type_t *type = skip_typeref(declaration->type);

	if (initializer->kind == INITIALIZER_VALUE) {
		initializer_value_t *initializer_value = &initializer->value;
		dbg_info            *dbgi
			= get_dbg_info(&declaration->source_position);

		ir_node *value = expression_to_firm(initializer_value->value);
		value = do_strict_conv(dbgi, value);

		if (declaration_kind == DECLARATION_KIND_LOCAL_VARIABLE) {
			set_value(declaration->v.value_number, value);
		} else {
			assert(declaration_kind == DECLARATION_KIND_GLOBAL_VARIABLE);

			ir_entity *entity = declaration->v.entity;

			if (type->base.qualifiers & TYPE_QUALIFIER_CONST) {
				set_entity_variability(entity, variability_constant);
			} else {
				set_entity_variability(entity, variability_initialized);
			}
			set_atomic_ent_value(entity, value);
		}
	} else {
		assert(declaration_kind == DECLARATION_KIND_LOCAL_VARIABLE_ENTITY
				|| declaration_kind == DECLARATION_KIND_GLOBAL_VARIABLE);

		ir_entity        *entity        = declaration->v.entity;
		ir_initializer_t *irinitializer
			= create_ir_initializer(initializer, type);

		if (type->base.qualifiers & TYPE_QUALIFIER_CONST) {
			set_entity_variability(entity, variability_constant);
		} else {
			set_entity_variability(entity, variability_initialized);
		}
		set_entity_initializer(entity, irinitializer);
	}
}

static void create_variable_length_array(declaration_t *declaration)
{
	/* initializers are not allowed for VLAs */
	assert(declaration->init.initializer == NULL);

	declaration->declaration_kind = DECLARATION_KIND_VARIABLE_LENGTH_ARRAY;
	declaration->v.vla_base       = NULL;

	/* TODO: record VLA somewhere so we create the free node when we leave
	 * it's scope */
}

static void allocate_variable_length_array(declaration_t *declaration)
{
	/* initializers are not allowed for VLAs */
	assert(declaration->init.initializer == NULL);
	assert(get_cur_block() != NULL);

	dbg_info *dbgi      = get_dbg_info(&declaration->source_position);
	type_t   *type      = declaration->type;
	ir_type  *el_type   = get_ir_type(type->array.element_type);

	/* make sure size_node is calculated */
	get_type_size(type);
	ir_node  *elems = type->array.size_node;
	ir_node  *mem   = get_store();
	ir_node  *alloc = new_d_Alloc(dbgi, mem, elems, el_type, stack_alloc);

	ir_node  *proj_m = new_d_Proj(dbgi, alloc, mode_M, pn_Alloc_M);
	ir_node  *addr   = new_d_Proj(dbgi, alloc, mode_P_data, pn_Alloc_res);
	set_store(proj_m);

	assert(declaration->declaration_kind
			== DECLARATION_KIND_VARIABLE_LENGTH_ARRAY);
	declaration->v.vla_base       = addr;
}

/**
 * Creates a Firm local variable from a declaration.
 */
static void create_local_variable(declaration_t *declaration)
{
	assert(declaration->declaration_kind == DECLARATION_KIND_UNKNOWN);

	bool needs_entity = declaration->address_taken;
	type_t *type = skip_typeref(declaration->type);

	/* is it a variable length array? */
	if (is_type_array(type) && !type->array.size_constant) {
		create_variable_length_array(declaration);
		return;
	} else if (is_type_array(type) || is_type_compound(type)) {
		needs_entity = true;
	} else if (type->base.qualifiers & TYPE_QUALIFIER_VOLATILE) {
		needs_entity = true;
	}

	if (needs_entity) {
		ir_type *frame_type = get_irg_frame_type(current_ir_graph);
		create_declaration_entity(declaration,
		                          DECLARATION_KIND_LOCAL_VARIABLE_ENTITY,
		                          frame_type);
	} else {
		declaration->declaration_kind = DECLARATION_KIND_LOCAL_VARIABLE;
		declaration->v.value_number   = next_value_number_function;
		set_irg_loc_description(current_ir_graph, next_value_number_function, declaration);
		++next_value_number_function;
	}
}

static void create_local_static_variable(declaration_t *declaration)
{
	assert(declaration->declaration_kind == DECLARATION_KIND_UNKNOWN);

	type_t    *const type        = skip_typeref(declaration->type);
	ir_type   *const global_type = get_glob_type();
	ir_type   *const irtype      = get_ir_type(type);
	dbg_info  *const dbgi        = get_dbg_info(&declaration->source_position);

	size_t l = strlen(declaration->symbol->string);
	char   buf[l + sizeof(".%u")];
	snprintf(buf, sizeof(buf), "%s.%%u", declaration->symbol->string);
	ident     *const id = id_unique(buf);

	ir_entity *const entity      = new_d_entity(global_type, id, irtype, dbgi);

	if (type->base.qualifiers & TYPE_QUALIFIER_VOLATILE) {
		set_entity_volatility(entity, volatility_is_volatile);
	}

	declaration->declaration_kind = DECLARATION_KIND_GLOBAL_VARIABLE;
	declaration->v.entity         = entity;
	set_entity_ld_ident(entity, create_ld_ident(entity, declaration));
	set_entity_variability(entity, variability_uninitialized);
	set_entity_visibility(entity, visibility_local);
	set_entity_allocation(entity, allocation_static);

	ir_graph *const old_current_ir_graph = current_ir_graph;
	current_ir_graph = get_const_code_irg();

	create_declaration_initializer(declaration);

	assert(current_ir_graph == get_const_code_irg());
	current_ir_graph = old_current_ir_graph;
}



static void return_statement_to_firm(return_statement_t *statement)
{
	if (get_cur_block() == NULL)
		return;

	dbg_info *dbgi        = get_dbg_info(&statement->base.source_position);
	ir_type  *func_irtype = get_ir_type(current_function_decl->type);


	ir_node *in[1];
	int      in_len;
	if (get_method_n_ress(func_irtype) > 0) {
		ir_type *res_type = get_method_res_type(func_irtype, 0);

		if (statement->value != NULL) {
			ir_node *node = expression_to_firm(statement->value);
			node  = do_strict_conv(dbgi, node);
			in[0] = node;
		} else {
			ir_mode *mode;
			if (is_compound_type(res_type)) {
				mode = mode_P_data;
			} else {
				mode = get_type_mode(res_type);
			}
			in[0] = new_Unknown(mode);
		}
		in_len = 1;
	} else {
		/* build return_value for its side effects */
		if (statement->value != NULL) {
			expression_to_firm(statement->value);
		}
		in_len = 0;
	}

	ir_node  *store = get_store();
	ir_node  *ret   = new_d_Return(dbgi, store, in_len, in);

	ir_node *end_block = get_irg_end_block(current_ir_graph);
	add_immBlock_pred(end_block, ret);

	set_cur_block(NULL);
}

static ir_node *expression_statement_to_firm(expression_statement_t *statement)
{
	if (get_cur_block() == NULL)
		return NULL;

	return expression_to_firm(statement->expression);
}

static ir_node *compound_statement_to_firm(compound_statement_t *compound)
{
	declaration_t *declaration = compound->scope.declarations;
	for( ; declaration != NULL; declaration = declaration->next) {
		create_local_declaration(declaration);
	}

	ir_node     *result    = NULL;
	statement_t *statement = compound->statements;
	for( ; statement != NULL; statement = statement->base.next) {
		if (statement->base.next == NULL
				&& statement->kind == STATEMENT_EXPRESSION) {
			result = expression_statement_to_firm(
					&statement->expression);
			break;
		}
		statement_to_firm(statement);
	}

	return result;
}

static void create_global_variable(declaration_t *declaration)
{
	ir_visibility  vis;
	ir_type       *var_type;
	switch ((storage_class_tag_t)declaration->storage_class) {
		case STORAGE_CLASS_STATIC:
			vis = visibility_local;
			goto global_var;

		case STORAGE_CLASS_EXTERN:
			vis = visibility_external_allocated;
			goto global_var;

		case STORAGE_CLASS_NONE:
			vis = visibility_external_visible;
			goto global_var;

		case STORAGE_CLASS_THREAD:
			vis = visibility_external_visible;
			goto tls_var;

		case STORAGE_CLASS_THREAD_EXTERN:
			vis = visibility_external_allocated;
			goto tls_var;

		case STORAGE_CLASS_THREAD_STATIC:
			vis = visibility_local;
			goto tls_var;

tls_var:
			var_type = get_tls_type();
			goto create_var;

global_var:
			var_type = get_glob_type();
			goto create_var;

create_var:
			create_declaration_entity(declaration,
			                          DECLARATION_KIND_GLOBAL_VARIABLE,
			                          var_type);
			if (!is_type_function(skip_typeref(declaration->type))) {
				set_entity_visibility(declaration->v.entity, vis);
			}

			return;

		case STORAGE_CLASS_TYPEDEF:
		case STORAGE_CLASS_AUTO:
		case STORAGE_CLASS_REGISTER:
		case STORAGE_CLASS_ENUM_ENTRY:
			break;
	}
	panic("Invalid storage class for global variable");
}

static void create_local_declaration(declaration_t *declaration)
{
	if (declaration->namespc != NAMESPACE_NORMAL)
		return;
	/* construct type */
	(void) get_ir_type(declaration->type);
	if (declaration->symbol == NULL) {
		return;
	}

	type_t *type = skip_typeref(declaration->type);

	switch ((storage_class_tag_t) declaration->storage_class) {
	case STORAGE_CLASS_STATIC:
		create_local_static_variable(declaration);
		return;
	case STORAGE_CLASS_EXTERN:
		create_global_variable(declaration);
		create_declaration_initializer(declaration);
		return;
	case STORAGE_CLASS_NONE:
	case STORAGE_CLASS_AUTO:
	case STORAGE_CLASS_REGISTER:
		if (is_type_function(type)) {
			if (declaration->init.statement != NULL) {
				panic("nested functions not supported yet");
			} else {
				get_function_entity(declaration);
			}
		} else {
			create_local_variable(declaration);
		}
		return;
	case STORAGE_CLASS_ENUM_ENTRY:
		/* should already be handled */
		assert(declaration->declaration_kind == DECLARATION_KIND_ENUM_ENTRY);
		return;
	case STORAGE_CLASS_TYPEDEF:
		declaration->declaration_kind = DECLARATION_KIND_TYPE;
		return;
	case STORAGE_CLASS_THREAD:
	case STORAGE_CLASS_THREAD_EXTERN:
	case STORAGE_CLASS_THREAD_STATIC:
		break;
	}
	panic("invalid storage class found");
}

static void initialize_local_declaration(declaration_t *declaration)
{
	if (declaration->symbol == NULL || declaration->namespc != NAMESPACE_NORMAL)
		return;

	switch ((declaration_kind_t) declaration->declaration_kind) {
	case DECLARATION_KIND_LOCAL_VARIABLE:
	case DECLARATION_KIND_LOCAL_VARIABLE_ENTITY:
		create_declaration_initializer(declaration);
		return;

	case DECLARATION_KIND_VARIABLE_LENGTH_ARRAY:
		allocate_variable_length_array(declaration);
		return;

	case DECLARATION_KIND_LABEL_BLOCK:
	case DECLARATION_KIND_COMPOUND_MEMBER:
	case DECLARATION_KIND_GLOBAL_VARIABLE:
	case DECLARATION_KIND_COMPOUND_TYPE_INCOMPLETE:
	case DECLARATION_KIND_COMPOUND_TYPE_COMPLETE:
	case DECLARATION_KIND_FUNCTION:
	case DECLARATION_KIND_TYPE:
	case DECLARATION_KIND_ENUM_ENTRY:
		return;

	case DECLARATION_KIND_UNKNOWN:
		panic("can't initialize unknwon declaration");
	}
	panic("invalid declaration kind");
}

static void declaration_statement_to_firm(declaration_statement_t *statement)
{
	declaration_t *declaration = statement->declarations_begin;
	declaration_t *end         = statement->declarations_end->next;
	for( ; declaration != end; declaration = declaration->next) {
		if (declaration->namespc != NAMESPACE_NORMAL)
			continue;
		initialize_local_declaration(declaration);
	}
}

static void if_statement_to_firm(if_statement_t *statement)
{
	ir_node *cur_block = get_cur_block();

	ir_node *fallthrough_block = NULL;

	/* the true (blocks) */
	ir_node *true_block = NULL;
	if (statement->true_statement != NULL) {
		true_block = new_immBlock();
		statement_to_firm(statement->true_statement);
		if (get_cur_block() != NULL) {
			ir_node *jmp = new_Jmp();
			if (fallthrough_block == NULL)
				fallthrough_block = new_immBlock();
			add_immBlock_pred(fallthrough_block, jmp);
		}
	}

	/* the false (blocks) */
	ir_node *false_block = NULL;
	if (statement->false_statement != NULL) {
		false_block = new_immBlock();

		statement_to_firm(statement->false_statement);
		if (get_cur_block() != NULL) {
			ir_node *jmp = new_Jmp();
			if (fallthrough_block == NULL)
				fallthrough_block = new_immBlock();
			add_immBlock_pred(fallthrough_block, jmp);
		}
	}

	/* create the condition */
	if (cur_block != NULL) {
		if (true_block == NULL || false_block == NULL) {
			if (fallthrough_block == NULL)
				fallthrough_block = new_immBlock();
			if (true_block == NULL)
				true_block = fallthrough_block;
			if (false_block == NULL)
				false_block = fallthrough_block;
		}

		set_cur_block(cur_block);
		create_condition_evaluation(statement->condition, true_block,
		                            false_block);
	}

	mature_immBlock(true_block);
	if (false_block != fallthrough_block) {
		mature_immBlock(false_block);
	}
	if (fallthrough_block != NULL) {
		mature_immBlock(fallthrough_block);
	}

	set_cur_block(fallthrough_block);
}

static void while_statement_to_firm(while_statement_t *statement)
{
	ir_node *jmp = NULL;
	if (get_cur_block() != NULL) {
		jmp = new_Jmp();
	}

	/* create the header block */
	ir_node *header_block = new_immBlock();
	if (jmp != NULL) {
		add_immBlock_pred(header_block, jmp);
	}

	/* the loop body */
	ir_node *old_continue_label = continue_label;
	ir_node *old_break_label    = break_label;
	continue_label              = header_block;
	break_label                 = NULL;

	ir_node *body_block = new_immBlock();
	statement_to_firm(statement->body);
	ir_node *false_block = break_label;

	assert(continue_label == header_block);
	continue_label = old_continue_label;
	break_label    = old_break_label;

	if (get_cur_block() != NULL) {
		jmp = new_Jmp();
		add_immBlock_pred(header_block, jmp);
	}

	/* shortcut for while(true) */
	if (is_constant_expression(statement->condition)
			&& fold_constant(statement->condition) != 0) {
		set_cur_block(header_block);
		ir_node *header_jmp = new_Jmp();
		add_immBlock_pred(body_block, header_jmp);

		keep_alive(body_block);
	} else {
		if (false_block == NULL) {
			false_block = new_immBlock();
		}

		/* create the condition */
		set_cur_block(header_block);

		create_condition_evaluation(statement->condition, body_block,
		                            false_block);
	}

	mature_immBlock(body_block);
	mature_immBlock(header_block);
	if (false_block != NULL) {
		mature_immBlock(false_block);
	}

	set_cur_block(false_block);
}

static void do_while_statement_to_firm(do_while_statement_t *statement)
{
	ir_node *jmp = NULL;
	if (get_cur_block() != NULL) {
		jmp = new_Jmp();
	}

	/* create the header block */
	ir_node *header_block = new_immBlock();

	/* the loop body */
	ir_node *body_block = new_immBlock();
	if (jmp != NULL) {
		add_immBlock_pred(body_block, jmp);
	}

	ir_node *old_continue_label = continue_label;
	ir_node *old_break_label    = break_label;
	continue_label              = header_block;
	break_label                 = NULL;

	statement_to_firm(statement->body);
	ir_node *false_block = break_label;

	assert(continue_label == header_block);
	continue_label = old_continue_label;
	break_label    = old_break_label;

	if (get_cur_block() != NULL) {
		ir_node *body_jmp = new_Jmp();
		add_immBlock_pred(header_block, body_jmp);
		mature_immBlock(header_block);
	}

	if (false_block == NULL) {
		false_block = new_immBlock();
	}

	/* create the condition */
	set_cur_block(header_block);

	create_condition_evaluation(statement->condition, body_block, false_block);
	mature_immBlock(body_block);
	mature_immBlock(header_block);
	if (false_block != NULL) {
		mature_immBlock(false_block);
	}

	set_cur_block(false_block);
}

static void for_statement_to_firm(for_statement_t *statement)
{
	ir_node *jmp = NULL;

	/* create declarations */
	declaration_t *declaration = statement->scope.declarations;
	for( ; declaration != NULL; declaration = declaration->next) {
		create_local_declaration(declaration);
	}
	declaration = statement->scope.declarations;
	for( ; declaration != NULL; declaration = declaration->next) {
		initialize_local_declaration(declaration);
	}

	if (get_cur_block() != NULL) {
		if (statement->initialisation != NULL) {
			expression_to_firm(statement->initialisation);
		}

		jmp = new_Jmp();
	}


	/* create the step block */
	ir_node *const step_block = new_immBlock();
	if (statement->step != NULL) {
		expression_to_firm(statement->step);
	}
	ir_node *const step_jmp = new_Jmp();

	/* create the header block */
	ir_node *const header_block = new_immBlock();
	if (jmp != NULL) {
		add_immBlock_pred(header_block, jmp);
	}
	add_immBlock_pred(header_block, step_jmp);

	/* the false block */
	ir_node *const false_block = new_immBlock();

	/* the loop body */
	ir_node * body_block;
	if (statement->body != NULL) {
		ir_node *const old_continue_label = continue_label;
		ir_node *const old_break_label    = break_label;
		continue_label = step_block;
		break_label    = false_block;

		body_block = new_immBlock();
		statement_to_firm(statement->body);

		assert(continue_label == step_block);
		assert(break_label    == false_block);
		continue_label = old_continue_label;
		break_label    = old_break_label;

		if (get_cur_block() != NULL) {
			jmp = new_Jmp();
			add_immBlock_pred(step_block, jmp);
		}
	} else {
		body_block = step_block;
	}

	/* create the condition */
	set_cur_block(header_block);
	if (statement->condition != NULL) {
		create_condition_evaluation(statement->condition, body_block,
		                            false_block);
	} else {
		keep_alive(header_block);
		jmp = new_Jmp();
		add_immBlock_pred(body_block, jmp);
	}

	mature_immBlock(body_block);
	mature_immBlock(false_block);
	mature_immBlock(step_block);
	mature_immBlock(header_block);
	mature_immBlock(false_block);

	set_cur_block(false_block);
}

static void create_jump_statement(const statement_t *statement,
                                  ir_node *target_block)
{
	if (get_cur_block() == NULL)
		return;

	dbg_info *dbgi = get_dbg_info(&statement->base.source_position);
	ir_node  *jump = new_d_Jmp(dbgi);
	add_immBlock_pred(target_block, jump);

	set_cur_block(NULL);
}

static ir_node *get_break_label(void)
{
	if (break_label == NULL) {
		ir_node *cur_block = get_cur_block();
		break_label = new_immBlock();
		set_cur_block(cur_block);
	}
	return break_label;
}

static void switch_statement_to_firm(switch_statement_t *statement)
{
	dbg_info *dbgi = get_dbg_info(&statement->base.source_position);

	ir_node *expression  = expression_to_firm(statement->expression);
	ir_node *cond        = new_d_Cond(dbgi, expression);

	set_cur_block(NULL);

	ir_node *const old_switch_cond       = current_switch_cond;
	ir_node *const old_break_label       = break_label;
	const bool     old_saw_default_label = saw_default_label;
	saw_default_label                    = false;
	current_switch_cond                  = cond;
	break_label                          = NULL;
	switch_statement_t *const old_switch = current_switch;
	current_switch                       = statement;

	/* determine a free number for the default label */
	unsigned long num_cases = 0;
	long def_nr = 0;
	for (case_label_statement_t *l = statement->first_case; l != NULL; l = l->next) {
		if (l->expression == NULL) {
			/* default case */
			continue;
		}
		for (long cns = l->first_case; cns <= l->last_case; ++cns)
			++num_cases;
		if (l->last_case > def_nr)
			def_nr = l->last_case;
	}

	if (def_nr + 1 < 0) {
		/* Bad: an overflow occurred, we cannot be sure that the
    	 * maximum + 1 is a free number. Scan the values a second
    	 * time to find a free number.
    	 */
		unsigned char *bits = xmalloc((num_cases + 7) >> 3);
		unsigned long i;

 		memset(bits, 0, (num_cases + 7) >> 3);
		for (case_label_statement_t *l = statement->first_case; l != NULL; l = l->next) {
			if (l->expression == NULL) {
				/* default case */
				continue;
			}
			for (long cns = l->first_case; cns <= l->last_case; ++cns) {
				if (cns >= 0 && (unsigned long)cns < num_cases)
					bits[cns >> 3] |= (1 << (cns & 7));
			}
		}
		/* We look at the first num_cases constants:
		 * Either they are densed, so we took the last (num_cases)
		 * one, or they are non densed, so we will find one free
		 * there...
		 */
		for (i = 0; i < num_cases; ++i)
			if ((bits[i >> 3] & (i & 7)) == 0)
				break;

		free(bits);
		def_nr = i;
	} else {
		++def_nr;
	}
	statement->def_proj_nr = def_nr;

	if (statement->body != NULL) {
		statement_to_firm(statement->body);
	}

	if (get_cur_block() != NULL) {
		ir_node *jmp = new_Jmp();
		add_immBlock_pred(get_break_label(), jmp);
	}

	if (!saw_default_label) {
		set_cur_block(get_nodes_block(cond));
		ir_node *const proj = new_d_defaultProj(dbgi, cond,
		                                        statement->def_proj_nr);
		add_immBlock_pred(get_break_label(), proj);
	}

	if (break_label != NULL) {
		mature_immBlock(break_label);
	}
	set_cur_block(break_label);

	assert(current_switch_cond == cond);
	current_switch      = old_switch;
	current_switch_cond = old_switch_cond;
	break_label         = old_break_label;
	saw_default_label   = old_saw_default_label;
}

static void case_label_to_firm(const case_label_statement_t *statement)
{
	if (statement->is_empty)
		return;

	dbg_info *dbgi = get_dbg_info(&statement->base.source_position);

	ir_node *const fallthrough = (get_cur_block() == NULL ? NULL : new_Jmp());

	ir_node *proj;
	ir_node *old_block = get_nodes_block(current_switch_cond);
	ir_node *block     = new_immBlock();

	set_cur_block(old_block);
	if (statement->expression != NULL) {
		long start_pn = statement->first_case;
		long end_pn   = statement->last_case;
		assert(start_pn <= end_pn);
		/* create jumps for all cases in the given range */
		for (long pn = start_pn; pn <= end_pn; ++pn) {
			proj = new_d_Proj(dbgi, current_switch_cond, mode_X, pn);
			add_immBlock_pred(block, proj);
		}
	} else {
		saw_default_label = true;
		proj = new_d_defaultProj(dbgi, current_switch_cond,
		                         current_switch->def_proj_nr);

		add_immBlock_pred(block, proj);
	}

	if (fallthrough != NULL) {
		add_immBlock_pred(block, fallthrough);
	}
	mature_immBlock(block);
	set_cur_block(block);

	if (statement->statement != NULL) {
		statement_to_firm(statement->statement);
	}
}

static ir_node *get_label_block(declaration_t *label)
{
	assert(label->namespc == NAMESPACE_LABEL);

	if (label->declaration_kind == DECLARATION_KIND_LABEL_BLOCK) {
		return label->v.block;
	}
	assert(label->declaration_kind == DECLARATION_KIND_UNKNOWN);

	ir_node *old_cur_block = get_cur_block();
	ir_node *block         = new_immBlock();
	set_cur_block(old_cur_block);

	label->declaration_kind = DECLARATION_KIND_LABEL_BLOCK;
	label->v.block          = block;

	ARR_APP1(ir_node *, imature_blocks, block);

	return block;
}

static void label_to_firm(const label_statement_t *statement)
{
	ir_node *block = get_label_block(statement->label);

	if (get_cur_block() != NULL) {
		ir_node *jmp = new_Jmp();
		add_immBlock_pred(block, jmp);
	}

	set_cur_block(block);
	keep_alive(block);

	if (statement->statement != NULL) {
		statement_to_firm(statement->statement);
	}
}

static void goto_to_firm(const goto_statement_t *statement)
{
	if (get_cur_block() == NULL)
		return;

	ir_node *block = get_label_block(statement->label);
	ir_node *jmp   = new_Jmp();
	add_immBlock_pred(block, jmp);

	set_cur_block(NULL);
}

static void asm_statement_to_firm(const asm_statement_t *statement)
{
	bool needs_memory = false;

	if (statement->is_volatile) {
		needs_memory = true;
	}

	size_t         n_clobbers = 0;
	asm_clobber_t *clobber    = statement->clobbers;
	for( ; clobber != NULL; clobber = clobber->next) {
		const char *clobber_str = clobber->clobber.begin;

		if (!be_is_valid_clobber(clobber_str)) {
			errorf(&statement->base.source_position,
				   "invalid clobber '%s' specified", clobber->clobber);
			continue;
		}

		if (strcmp(clobber_str, "memory") == 0) {
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
		if (! (asm_flags & ASM_CONSTRAINT_FLAG_MODIFIER_WRITE)) {
			errorf(&statement->base.source_position,
			       "no write flag specified for output constraints '%s'",
			       constraints);
			continue;
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

				snprintf(buf, sizeof(buf), "%d", pos);

				ir_asm_constraint constraint;
				constraint.pos              = pos;
				constraint.constraint       = new_id_from_str(buf);
				constraint.mode             = get_ir_mode(expr->base.type);
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
			constraint.mode             = NULL;
			tmp_in_constraints[in_size] = constraint;

			ins[in_size]          = expression_to_addr(expr);
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
		constraint.mode       = get_ir_mode(argument->expression->base.type);

		obstack_grow(&asm_obst, &constraint, sizeof(constraint));
	}
	assert(obstack_object_size(&asm_obst)
			== out_size * sizeof(ir_asm_constraint));
	ir_asm_constraint *output_constraints = obstack_finish(&asm_obst);


	obstack_grow(&asm_obst, tmp_in_constraints,
	             in_size * sizeof(tmp_in_constraints[0]));
	/* find and count input and output arguments */
	argument = statement->inputs;
	for( ; argument != NULL; argument = argument->next) {
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

	if (needs_memory) {
		ir_asm_constraint constraint;
		constraint.pos        = next_pos++;
		constraint.constraint = new_id_from_str("");
		constraint.mode       = mode_M;

		obstack_grow(&asm_obst, &constraint, sizeof(constraint));
		ins[in_size++] = get_store();
	}

	assert(obstack_object_size(&asm_obst)
			== in_size * sizeof(ir_asm_constraint));
	ir_asm_constraint *input_constraints = obstack_finish(&asm_obst);

	/* create asm node */
	dbg_info *dbgi = get_dbg_info(&statement->base.source_position);

	ident *asm_text = new_id_from_str(statement->asm_text.begin);

	ir_node *node = new_d_ASM(dbgi, in_size, ins, input_constraints,
	                          out_size, output_constraints,
	                          n_clobbers, clobbers, asm_text);

	if (statement->is_volatile) {
		set_irn_pinned(node, op_pin_state_pinned);
	} else {
		set_irn_pinned(node, op_pin_state_floats);
	}

	/* create output projs & connect them */
	if (needs_memory) {
		ir_node *projm = new_Proj(node, mode_M, out_size+1);
		set_store(projm);
	}

	size_t i;
	for (i = 0; i < out_size; ++i) {
		const expression_t *out_expr = out_exprs[i];
		long                pn       = i;
		ir_mode            *mode     = get_ir_mode(out_expr->base.type);
		ir_node            *proj     = new_Proj(node, mode, pn);
		ir_node            *addr     = out_addrs[i];

		set_value_for_expression_addr(out_expr, proj, addr);
	}
}

static void	ms_try_statement_to_firm(ms_try_statement_t *statement) {
	statement_to_firm(statement->try_statement);
	warningf(&statement->base.source_position, "structured exception handling ignored");
}

static void	leave_statement_to_firm(leave_statement_t *statement) {
	errorf(&statement->base.source_position, "__leave not supported yet");
}

/**
 * Transform a statement.
 */
static void statement_to_firm(statement_t *statement)
{
#ifndef NDEBUG
	assert(!statement->base.transformed);
	statement->base.transformed = true;
#endif

	switch(statement->kind) {
	case STATEMENT_INVALID:
		panic("invalid statement found");
		return;
	case STATEMENT_EMPTY:
		/* nothing */
		return;
	case STATEMENT_COMPOUND:
		compound_statement_to_firm(&statement->compound);
		return;
	case STATEMENT_RETURN:
		return_statement_to_firm(&statement->returns);
		return;
	case STATEMENT_EXPRESSION:
		expression_statement_to_firm(&statement->expression);
		return;
	case STATEMENT_IF:
		if_statement_to_firm(&statement->ifs);
		return;
	case STATEMENT_WHILE:
		while_statement_to_firm(&statement->whiles);
		return;
	case STATEMENT_DO_WHILE:
		do_while_statement_to_firm(&statement->do_while);
		return;
	case STATEMENT_DECLARATION:
		declaration_statement_to_firm(&statement->declaration);
		return;
	case STATEMENT_BREAK:
		create_jump_statement(statement, get_break_label());
		return;
	case STATEMENT_CONTINUE:
		create_jump_statement(statement, continue_label);
		return;
	case STATEMENT_SWITCH:
		switch_statement_to_firm(&statement->switchs);
		return;
	case STATEMENT_CASE_LABEL:
		case_label_to_firm(&statement->case_label);
		return;
	case STATEMENT_FOR:
		for_statement_to_firm(&statement->fors);
		return;
	case STATEMENT_LABEL:
		label_to_firm(&statement->label);
		return;
	case STATEMENT_GOTO:
		goto_to_firm(&statement->gotos);
		return;
	case STATEMENT_ASM:
		asm_statement_to_firm(&statement->asms);
		return;
	case STATEMENT_MS_TRY:
		ms_try_statement_to_firm(&statement->ms_try);
		return;
	case STATEMENT_LEAVE:
		leave_statement_to_firm(&statement->leave);
		return;
	}
	panic("Statement not implemented\n");
}

static int count_decls_in_expression(const expression_t *expression);

static int count_local_declarations(const declaration_t *      decl,
                                    const declaration_t *const end)
{
	int count = 0;
	for (; decl != end; decl = decl->next) {
		if (decl->namespc != NAMESPACE_NORMAL)
			continue;
		type_t *type = skip_typeref(decl->type);

		if (!decl->address_taken && is_type_scalar(type))
			++count;
		const initializer_t *initializer = decl->init.initializer;
		/* FIXME: should walk initializer hierarchies... */
		if (initializer != NULL && initializer->kind == INITIALIZER_VALUE) {
			count += count_decls_in_expression(initializer->value.value);
		}
	}
	return count;
}

static int count_decls_in_expression(const expression_t *expression) {
	int count = 0;

	if (expression == NULL)
		return 0;

	switch((expression_kind_t) expression->base.kind) {
	case EXPR_STATEMENT:
		return count_decls_in_stmts(expression->statement.statement);
	EXPR_BINARY_CASES {
		int count_left  = count_decls_in_expression(expression->binary.left);
		int count_right = count_decls_in_expression(expression->binary.right);
		return count_left + count_right;
	}
	EXPR_UNARY_CASES
		return count_decls_in_expression(expression->unary.value);
	case EXPR_CALL:	{
		call_argument_t *argument = expression->call.arguments;
		for( ; argument != NULL; argument = argument->next) {
			count += count_decls_in_expression(argument->expression);
		}
		return count;
	}

	case EXPR_UNKNOWN:
	case EXPR_INVALID:
		panic("unexpected expression kind");

	case EXPR_COMPOUND_LITERAL:
		/* TODO... */
		break;

	case EXPR_CONDITIONAL:
		count += count_decls_in_expression(expression->conditional.condition);
		count += count_decls_in_expression(expression->conditional.true_expression);
		count += count_decls_in_expression(expression->conditional.false_expression);
		return count;

	case EXPR_BUILTIN_PREFETCH:
		count += count_decls_in_expression(expression->builtin_prefetch.adr);
		count += count_decls_in_expression(expression->builtin_prefetch.rw);
		count += count_decls_in_expression(expression->builtin_prefetch.locality);
		return count;

	case EXPR_BUILTIN_CONSTANT_P:
		count += count_decls_in_expression(expression->builtin_constant.value);
		return count;

	case EXPR_SELECT:
		count += count_decls_in_expression(expression->select.compound);
		return count;

	case EXPR_ARRAY_ACCESS:
		count += count_decls_in_expression(expression->array_access.array_ref);
		count += count_decls_in_expression(expression->array_access.index);
		return count;

	case EXPR_CLASSIFY_TYPE:
		count += count_decls_in_expression(expression->classify_type.type_expression);
		return count;

	case EXPR_SIZEOF:
	case EXPR_ALIGNOF: {
		expression_t *tp_expression = expression->typeprop.tp_expression;
		if (tp_expression != NULL) {
			count += count_decls_in_expression(tp_expression);
		}
		return count;
	}

	case EXPR_OFFSETOF:
	case EXPR_REFERENCE:
	case EXPR_CONST:
	case EXPR_CHARACTER_CONSTANT:
	case EXPR_WIDE_CHARACTER_CONSTANT:
	case EXPR_STRING_LITERAL:
	case EXPR_WIDE_STRING_LITERAL:
	case EXPR_FUNCNAME:
	case EXPR_BUILTIN_SYMBOL:
	case EXPR_VA_START:
	case EXPR_VA_ARG:
		break;
	}

	/* TODO FIXME: finish/fix that firm patch that allows dynamic value numbers
	 * (or implement all the missing expressions here/implement a walker)
	 */

	return 0;
}

static int count_decls_in_stmts(const statement_t *stmt)
{
	int count = 0;
	for (; stmt != NULL; stmt = stmt->base.next) {
		switch (stmt->kind) {
			case STATEMENT_EMPTY:
				break;

			case STATEMENT_DECLARATION: {
				const declaration_statement_t *const decl_stmt = &stmt->declaration;
				count += count_local_declarations(decl_stmt->declarations_begin,
				                                  decl_stmt->declarations_end->next);
				break;
			}

			case STATEMENT_COMPOUND: {
				const compound_statement_t *const comp =
					&stmt->compound;
				count += count_decls_in_stmts(comp->statements);
				break;
			}

			case STATEMENT_IF: {
				const if_statement_t *const if_stmt = &stmt->ifs;
				count += count_decls_in_expression(if_stmt->condition);
				count += count_decls_in_stmts(if_stmt->true_statement);
				count += count_decls_in_stmts(if_stmt->false_statement);
				break;
			}

			case STATEMENT_SWITCH: {
				const switch_statement_t *const switch_stmt = &stmt->switchs;
				count += count_decls_in_expression(switch_stmt->expression);
				count += count_decls_in_stmts(switch_stmt->body);
				break;
			}

			case STATEMENT_LABEL: {
				const label_statement_t *const label_stmt = &stmt->label;
				if (label_stmt->statement != NULL) {
					count += count_decls_in_stmts(label_stmt->statement);
				}
				break;
			}

			case STATEMENT_WHILE: {
				const while_statement_t *const while_stmt = &stmt->whiles;
				count += count_decls_in_expression(while_stmt->condition);
				count += count_decls_in_stmts(while_stmt->body);
				break;
			}

			case STATEMENT_DO_WHILE: {
				const do_while_statement_t *const do_while_stmt = &stmt->do_while;
				count += count_decls_in_expression(do_while_stmt->condition);
				count += count_decls_in_stmts(do_while_stmt->body);
				break;
			}

			case STATEMENT_FOR: {
				const for_statement_t *const for_stmt = &stmt->fors;
				count += count_local_declarations(for_stmt->scope.declarations, NULL);
				count += count_decls_in_expression(for_stmt->initialisation);
				count += count_decls_in_expression(for_stmt->condition);
				count += count_decls_in_expression(for_stmt->step);
				count += count_decls_in_stmts(for_stmt->body);
				break;
			}

			case STATEMENT_CASE_LABEL: {
				const case_label_statement_t *label = &stmt->case_label;
				count += count_decls_in_expression(label->expression);
				if (label->statement != NULL) {
					count += count_decls_in_stmts(label->statement);
				}
				break;
			}

			case STATEMENT_ASM:
			case STATEMENT_BREAK:
			case STATEMENT_CONTINUE:
				break;

			case STATEMENT_EXPRESSION: {
				const expression_statement_t *expr_stmt = &stmt->expression;
				count += count_decls_in_expression(expr_stmt->expression);
				break;
			}

			case STATEMENT_GOTO:
			case STATEMENT_LEAVE:
			case STATEMENT_INVALID:
				break;

			case STATEMENT_RETURN: {
				const return_statement_t *ret_stmt = &stmt->returns;
				count += count_decls_in_expression(ret_stmt->value);
				break;
			}

			case STATEMENT_MS_TRY: {
				const ms_try_statement_t *const try_stmt = &stmt->ms_try;
				count += count_decls_in_stmts(try_stmt->try_statement);
				if (try_stmt->except_expression != NULL)
					count += count_decls_in_expression(try_stmt->except_expression);
				count += count_decls_in_stmts(try_stmt->final_statement);
				break;
			}
		}
	}
	return count;
}

static int get_function_n_local_vars(declaration_t *declaration)
{
	int count = 0;

	/* count parameters */
	count += count_local_declarations(declaration->scope.declarations, NULL);

	/* count local variables declared in body */
	count += count_decls_in_stmts(declaration->init.statement);

	return count;
}

static void initialize_function_parameters(declaration_t *declaration)
{
	ir_graph        *irg             = current_ir_graph;
	ir_node         *args            = get_irg_args(irg);
	ir_node         *start_block     = get_irg_start_block(irg);
	ir_type         *function_irtype = get_ir_type(declaration->type);

	int            n         = 0;
	declaration_t *parameter = declaration->scope.declarations;
	for( ; parameter != NULL; parameter = parameter->next, ++n) {
		assert(parameter->declaration_kind == DECLARATION_KIND_UNKNOWN);
		type_t *type = skip_typeref(parameter->type);

		bool needs_entity = parameter->address_taken;
		assert(!is_type_array(type));
		if (is_type_compound(type)) {
			needs_entity = true;
		}

		if (needs_entity) {
			ir_entity *entity = get_method_value_param_ent(function_irtype, n);
			ident     *id     = new_id_from_str(parameter->symbol->string);
			set_entity_ident(entity, id);

			parameter->declaration_kind
				= DECLARATION_KIND_LOCAL_VARIABLE_ENTITY;
			parameter->v.entity = entity;
			continue;
		}

		ir_type *param_irtype = get_method_param_type(function_irtype, n);
		ir_mode *param_mode   = get_type_mode(param_irtype);

		long     pn    = n;
		ir_node *value = new_r_Proj(irg, start_block, args, param_mode, pn);

		ir_mode *mode = get_ir_mode(type);
		value = create_conv(NULL, value, mode);
		value = do_strict_conv(NULL, value);

		parameter->declaration_kind = DECLARATION_KIND_LOCAL_VARIABLE;
		parameter->v.value_number   = next_value_number_function;
		set_irg_loc_description(current_ir_graph, next_value_number_function, parameter);
		++next_value_number_function;

		set_value(parameter->v.value_number, value);
	}
}

/**
 * Handle additional decl modifiers for IR-graphs
 *
 * @param irg            the IR-graph
 * @param dec_modifiers  additional modifiers
 */
static void handle_decl_modifier_irg(ir_graph_ptr irg, decl_modifiers_t decl_modifiers)
{
	if (decl_modifiers & DM_NORETURN) {
		/* TRUE if the declaration includes the Microsoft
		   __declspec(noreturn) specifier. */
		set_irg_additional_property(irg, mtp_property_noreturn);
	}
	if (decl_modifiers & DM_NOTHROW) {
		/* TRUE if the declaration includes the Microsoft
		   __declspec(nothrow) specifier. */
		set_irg_additional_property(irg, mtp_property_nothrow);
	}
	if (decl_modifiers & DM_NAKED) {
		/* TRUE if the declaration includes the Microsoft
		   __declspec(naked) specifier. */
		set_irg_additional_property(irg, mtp_property_naked);
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
	ident     *id           = id_unique(unique_template);
	ir_type   *ptr_type     = new_type_pointer(id, method_type, mode_P_code);

	ident     *ide          = id_unique(unique_template);
	ir_entity *ptr          = new_entity(segment, ide, ptr_type);
	ir_graph  *irg          = get_const_code_irg();
	ir_node   *val          = new_rd_SymConst_addr_ent(NULL, irg, mode_P_code,
	                                                   method, NULL);

	set_entity_compiler_generated(ptr, 1);
	set_entity_variability(ptr, variability_constant);
	set_atomic_ent_value(ptr, val);
}

/**
 * Create code for a function.
 */
static void create_function(declaration_t *declaration)
{
	ir_entity *function_entity = get_function_entity(declaration);

	if (declaration->init.statement == NULL)
		return;

	if (declaration->modifiers & DM_CONSTRUCTOR) {
		ir_type *segment = get_segment_type(IR_SEGMENT_CONSTRUCTORS);
		add_function_pointer(segment, function_entity, "constructor_ptr.%u");
	}
	if (declaration->modifiers & DM_DESTRUCTOR) {
		ir_type *segment = get_segment_type(IR_SEGMENT_DESTRUCTORS);
		add_function_pointer(segment, function_entity, "destructor_ptr.%u");
	}

	current_function_decl = declaration;
	current_function_name = NULL;
	current_funcsig       = NULL;

	assert(imature_blocks == NULL);
	imature_blocks = NEW_ARR_F(ir_node*, 0);

	int       n_local_vars = get_function_n_local_vars(declaration);
	ir_graph *irg          = new_ir_graph(function_entity, n_local_vars);

	set_irg_fp_model(irg, firm_opt.fp_model);
	tarval_enable_fp_ops((firm_opt.fp_model & fp_strict_algebraic) == 0);
	set_irn_dbg_info(get_irg_start_block(irg), get_entity_dbg_info(function_entity));

	ir_node *first_block = get_cur_block();

	/* set inline flags */
	if (declaration->is_inline)
		set_irg_inline_property(irg, irg_inline_recomended);
	handle_decl_modifier_irg(irg, declaration->modifiers);

	next_value_number_function = 0;
	initialize_function_parameters(declaration);

	statement_to_firm(declaration->init.statement);

	ir_node *end_block = get_irg_end_block(irg);

	/* do we have a return statement yet? */
	if (get_cur_block() != NULL) {
		type_t *type = skip_typeref(declaration->type);
		assert(is_type_function(type));
		const function_type_t *func_type   = &type->function;
		const type_t          *return_type
			= skip_typeref(func_type->return_type);

		ir_node *ret;
		if (is_type_atomic(return_type, ATOMIC_TYPE_VOID)) {
			ret = new_Return(get_store(), 0, NULL);
		} else {
			ir_mode *mode;
			if (is_type_scalar(return_type)) {
				mode = get_ir_mode(func_type->return_type);
			} else {
				mode = mode_P_data;
			}

			ir_node *in[1];
			/* §5.1.2.2.3 main implicitly returns 0 */
			if (strcmp(declaration->symbol->string, "main") == 0) {
				in[0] = new_Const(mode, get_mode_null(mode));
			} else {
				in[0] = new_Unknown(mode);
			}
			ret = new_Return(get_store(), 1, in);
		}
		add_immBlock_pred(end_block, ret);
	}

	for(int i = 0; i < ARR_LEN(imature_blocks); ++i) {
		mature_immBlock(imature_blocks[i]);
	}
	DEL_ARR_F(imature_blocks);
	imature_blocks = NULL;

	mature_immBlock(first_block);
	mature_immBlock(end_block);

	irg_finalize_cons(irg);

	/* finalize the frame type */
	ir_type *frame_type = get_irg_frame_type(irg);
	int      n          = get_compound_n_members(frame_type);
	int      align_all  = 4;
	int      offset     = 0;
	for(int i = 0; i < n; ++i) {
		ir_entity *entity      = get_compound_member(frame_type, i);
		ir_type   *entity_type = get_entity_type(entity);

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

		set_entity_offset(entity, offset);
		offset += get_type_size_bytes(entity_type);
	}
	set_type_size_bytes(frame_type, offset);
	set_type_alignment_bytes(frame_type, align_all);

	irg_vrfy(irg);
}

static void scope_to_firm(scope_t *scope)
{
	/* first pass: create declarations */
	declaration_t *declaration = scope->declarations;
	for( ; declaration != NULL; declaration = declaration->next) {
		if (declaration->namespc != NAMESPACE_NORMAL)
			continue;
		if (declaration->storage_class == STORAGE_CLASS_ENUM_ENTRY
				|| declaration->storage_class == STORAGE_CLASS_TYPEDEF)
			continue;
		if (declaration->symbol == NULL)
			continue;

		type_t *type = skip_typeref(declaration->type);
		if (is_type_function(type)) {
			get_function_entity(declaration);
		} else {
			create_global_variable(declaration);
		}
	}

	/* second pass: create code/initializers */
	declaration = scope->declarations;
	for( ; declaration != NULL; declaration = declaration->next) {
		if (declaration->namespc != NAMESPACE_NORMAL)
			continue;
		if (declaration->storage_class == STORAGE_CLASS_ENUM_ENTRY
				|| declaration->storage_class == STORAGE_CLASS_TYPEDEF)
			continue;
		if (declaration->symbol == NULL)
			continue;

		type_t *type = declaration->type;
		if (type->kind == TYPE_FUNCTION) {
			create_function(declaration);
		} else {
			assert(declaration->declaration_kind
					== DECLARATION_KIND_GLOBAL_VARIABLE);
			current_ir_graph = get_const_code_irg();
			create_declaration_initializer(declaration);
		}
	}
}

void init_ast2firm(void)
{
	obstack_init(&asm_obst);
	init_atomic_modes();

	id_underscore = new_id_from_chars("_", 1);
	id_imp        = new_id_from_chars("__imp_", 6);

	/* OS option must be set to the backend */
	const char *s = "ia32-gasmode=linux";
	switch (firm_opt.os_support) {
	case OS_SUPPORT_MINGW:
		create_ld_ident = create_ld_ident_win32;
		s = "ia32-gasmode=mingw";
		break;
	case OS_SUPPORT_LINUX:
		create_ld_ident = create_ld_ident_linux_elf;
		s = "ia32-gasmode=elf";
		break;
	case OS_SUPPORT_MACHO:
		create_ld_ident = create_ld_ident_macho;
		s = "ia32-gasmode=macho";
		break;
	}
	int res = firm_be_option(s);
	(void) res;
	assert(res);

	/* create idents for all known runtime functions */
	for (size_t i = 0; i < sizeof(rts_data) / sizeof(rts_data[0]); ++i) {
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

	type_const_char = make_atomic_type(ATOMIC_TYPE_CHAR, TYPE_QUALIFIER_CONST);
	type_void       = make_atomic_type(ATOMIC_TYPE_VOID, TYPE_QUALIFIER_NONE);
	type_int        = make_atomic_type(ATOMIC_TYPE_INT,  TYPE_QUALIFIER_NONE);

	ir_type_int        = get_ir_type(type_int);
	ir_type_const_char = get_ir_type(type_const_char);
	ir_type_wchar_t    = get_ir_type(type_wchar_t);
	ir_type_void       = get_ir_type(type_void);
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
		size_t            size = s->asms.asm_text.size;

		/* skip the last \0 */
		if (text[size - 1] == '\0')
			--size;

		ident *const id = new_id_from_chars(text, size);
		add_irp_asm(id);
	}
}

void translation_unit_to_firm(translation_unit_t *unit)
{
	/* just to be sure */
	continue_label      = NULL;
	break_label         = NULL;
	current_switch_cond = NULL;

	init_ir_types();

	scope_to_firm(&unit->scope);
	global_asm_to_firm(unit->global_asm);
}
