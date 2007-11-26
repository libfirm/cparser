#include <config.h>

#define _GNU_SOURCE

#include <assert.h>
#include <string.h>
#include <stdbool.h>

#include <libfirm/firm.h>
#include <libfirm/adt/obst.h>

#include "ast2firm.h"

#include "adt/error.h"
#include "adt/array.h"
#include "token_t.h"
#include "type_t.h"
#include "ast_t.h"
#include "parser.h"

#define MAGIC_DEFAULT_PN_NUMBER	    (long) -314159265

static ir_type *ir_type_const_char;
static ir_type *ir_type_void;
static ir_type *ir_type_int;
static ir_type *ir_type_void_ptr;

static type_t *type_const_char;
static type_t *type_void;
static type_t *type_int;

static symbol_t *symbol_alloca;

static int       next_value_number_function;
static ir_node  *continue_label;
static ir_node  *break_label;
static ir_node  *current_switch_cond;
static bool      saw_default_label;
static ir_node **imature_blocks;

static const declaration_t *current_function_decl;
static ir_node             *current_function_name;

typedef enum declaration_type_t {
	DECLARATION_TYPE_UNKNOWN,
	DECLARATION_TYPE_FUNCTION,
	DECLARATION_TYPE_GLOBAL_VARIABLE,
	DECLARATION_TYPE_LOCAL_VARIABLE,
	DECLARATION_TYPE_LOCAL_VARIABLE_ENTITY,
	DECLARATION_TYPE_COMPOUND_MEMBER,
	DECLARATION_TYPE_LABEL_BLOCK,
	DECLARATION_TYPE_ENUM_ENTRY
} declaration_type_t;

static ir_type *get_ir_type(type_t *type);

ir_node *uninitialized_local_var(ir_graph *irg, ir_mode *mode, int pos)
{
	(void) pos;
#if 0
	const declaration_t *declaration = & value_numbers[pos]->declaration;

	print_warning_prefix(declaration->source_position);
	fprintf(stderr, "variable '%s' might be used uninitialized\n",
			declaration->symbol->string);
#endif
	fprintf(stderr, "Some variable might be used uninitialized\n");
	return new_r_Unknown(irg, mode);
}

unsigned dbg_snprint(char *buf, unsigned len, const dbg_info *dbg)
{
	const source_position_t *pos = (const source_position_t*) dbg;
	if(pos == NULL)
		return 0;
	return (unsigned) snprintf(buf, len, "%s:%u", pos->input_name,
	                           pos->linenr);
}

const char *retrieve_dbg(const dbg_info *dbg, unsigned *line)
{
	const source_position_t *pos = (const source_position_t*) dbg;
	if(pos == NULL)
		return NULL;
	if(line != NULL)
		*line = pos->linenr;
	return pos->input_name;
}

void init_ast2firm(void)
{
	type_const_char = make_atomic_type(ATOMIC_TYPE_CHAR, TYPE_QUALIFIER_CONST);
	type_void       = make_atomic_type(ATOMIC_TYPE_VOID, TYPE_QUALIFIER_NONE);
	type_int        = make_atomic_type(ATOMIC_TYPE_INT,  TYPE_QUALIFIER_NONE);

	ir_type_int        = get_ir_type(type_int);
	ir_type_const_char = get_ir_type(type_const_char);
	ir_type_void       = get_ir_type(type_int); /* we don't have a real void
	                                               type in firm */
	ir_type_void_ptr   = new_type_pointer(new_id_from_str("void_ptr"),
	                                      ir_type_void, mode_P_data);

	type_void->base.firm_type = ir_type_void;

	symbol_alloca = symbol_table_insert("__builtin_alloca");
}

void exit_ast2firm(void)
{
}

static unsigned unique_id = 0;

static ident *unique_ident(const char *tag)
{
	char buf[256];

	snprintf(buf, sizeof(buf), "%s.%u", tag, unique_id);
	unique_id++;
	return new_id_from_str(buf);
}

static ir_mode *get_atomic_mode(const atomic_type_t* atomic_type)
{
	switch(atomic_type->atype) {
	case ATOMIC_TYPE_SCHAR:
	case ATOMIC_TYPE_CHAR:
		return mode_Bs;
	case ATOMIC_TYPE_UCHAR:
		return mode_Bu;
	case ATOMIC_TYPE_SHORT:
		return mode_Hs;
	case ATOMIC_TYPE_USHORT:
		return mode_Hu;
	case ATOMIC_TYPE_LONG:
	case ATOMIC_TYPE_INT:
		return mode_Is;
	case ATOMIC_TYPE_ULONG:
	case ATOMIC_TYPE_UINT:
		return mode_Iu;
	case ATOMIC_TYPE_LONGLONG:
		return mode_Ls;
	case ATOMIC_TYPE_ULONGLONG:
		return mode_Lu;
	case ATOMIC_TYPE_FLOAT:
		return mode_F;
	case ATOMIC_TYPE_DOUBLE:
		return mode_D;
	case ATOMIC_TYPE_LONG_DOUBLE:
		return mode_E;
	case ATOMIC_TYPE_BOOL:
		return mode_b;
#ifdef PROVIDE_COMPLEX
	case ATOMIC_TYPE_FLOAT_COMPLEX:
	case ATOMIC_TYPE_DOUBLE_COMPLEX:
	case ATOMIC_TYPE_LONG_DOUBLE_COMPLEX:
		panic("complex lowering not implemented yet");
		break;
	case ATOMIC_TYPE_FLOAT_IMAGINARY:
	case ATOMIC_TYPE_DOUBLE_IMAGINARY:
	case ATOMIC_TYPE_LONG_DOUBLE_IMAGINARY:
		panic("imaginary lowering not implemented yet");
		break;
#endif
	case ATOMIC_TYPE_VOID:
		/* firm has no real void... */
		return mode_Is;
	case ATOMIC_TYPE_INVALID:
		break;
	}
	panic("Encountered unknown atomic type");
}


static unsigned get_type_size(type_t *type);

static unsigned get_atomic_type_size(const atomic_type_t *type)
{
	switch(type->atype) {
	case ATOMIC_TYPE_CHAR:
	case ATOMIC_TYPE_SCHAR:
	case ATOMIC_TYPE_UCHAR:
		return 1;

	case ATOMIC_TYPE_SHORT:
	case ATOMIC_TYPE_USHORT:
		return 2;

	case ATOMIC_TYPE_BOOL:
	case ATOMIC_TYPE_INT:
	case ATOMIC_TYPE_UINT:
	case ATOMIC_TYPE_LONG:
	case ATOMIC_TYPE_ULONG:
	case ATOMIC_TYPE_FLOAT:
		return 4;

	case ATOMIC_TYPE_LONGLONG:
	case ATOMIC_TYPE_ULONGLONG:
	case ATOMIC_TYPE_DOUBLE:
		return 8;

	case ATOMIC_TYPE_LONG_DOUBLE:
		return 12;

	case ATOMIC_TYPE_VOID:
		return 1;

	case ATOMIC_TYPE_INVALID:
		break;
	}
	panic("Trying to determine size of invalid atomic type");
}

static unsigned get_compound_type_size(compound_type_t *type)
{
	ir_type *irtype = get_ir_type((type_t*) type);
	return get_type_size_bytes(irtype);
}

static unsigned get_array_type_size(array_type_t *type)
{
	ir_type *irtype = get_ir_type((type_t*) type);
	return get_type_size_bytes(irtype);
}

static unsigned get_type_size(type_t *type)
{
	type = skip_typeref(type);

	switch(type->type) {
	case TYPE_ATOMIC:
		return get_atomic_type_size((const atomic_type_t*) type);
	case TYPE_ENUM:
		return get_mode_size_bytes(mode_Is);
	case TYPE_COMPOUND_UNION:
	case TYPE_COMPOUND_STRUCT:
		return get_compound_type_size((compound_type_t*) type);
	case TYPE_FUNCTION:
		/* just a pointer to the function */
		return get_mode_size_bytes(mode_P_code);
	case TYPE_POINTER:
		return get_mode_size_bytes(mode_P_data);
	case TYPE_ARRAY:
		return get_array_type_size((array_type_t*) type);
	case TYPE_BUILTIN:
	case TYPE_TYPEDEF:
	case TYPE_TYPEOF:
	case TYPE_INVALID:
		break;
	}
	panic("Trying to determine size of invalid type");
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




static long fold_constant(const expression_t *expression);

static ir_type *create_atomic_type(const atomic_type_t *type)
{
	ir_mode *mode   = get_atomic_mode(type);
	ident   *id     = get_mode_ident(mode);
	ir_type *irtype = new_type_primitive(id, mode);

	return irtype;
}

static ir_type *create_method_type(const function_type_t *function_type)
{
	type_t  *result_type  = function_type->result_type;

	ident   *id           = unique_ident("functiontype");
	int      n_parameters = count_parameters(function_type);
	int      n_results    = result_type == type_void ? 0 : 1;
	ir_type *irtype       = new_type_method(id, n_parameters, n_results);

	if(result_type != type_void) {
		ir_type *restype = get_ir_type(result_type);
		set_method_res_type(irtype, 0, restype);
	}

	function_parameter_t *parameter = function_type->parameters;
	int                   n         = 0;
	for( ; parameter != NULL; parameter = parameter->next) {
		ir_type *p_irtype = get_ir_type(parameter->type);
		set_method_param_type(irtype, n, p_irtype);
		++n;
	}

	if(function_type->variadic || function_type->unspecified_parameters) {
		set_method_variadicity(irtype, variadicity_variadic);
	}

	return irtype;
}

static ir_type *create_pointer_type(pointer_type_t *type)
{
	type_t  *points_to = type->points_to;
	ir_type *ir_points_to;
	/* Avoid endless recursion if the points_to type contains this poiner type
	 * again (might be a struct). We therefore first create a void* pointer
	 * and then set the real points_to type
	 */
	ir_type *ir_type = new_type_pointer(unique_ident("pointer"),
	                                    ir_type_void, mode_P_data);
	type->type.firm_type  = ir_type;

	ir_points_to = get_ir_type(points_to);
	set_pointer_points_to_type(ir_type, ir_points_to);

	return ir_type;
}

static ir_type *create_array_type(array_type_t *type)
{
	type_t  *element_type    = type->element_type;
	ir_type *ir_element_type = get_ir_type(element_type);

	ident   *id      = unique_ident("array");
	ir_type *ir_type = new_type_array(id, 1, ir_element_type);

	if(type->size != NULL) {
		int n_elements = fold_constant(type->size);

		set_array_bounds_int(ir_type, 0, 0, n_elements);

		size_t elemsize = get_type_size_bytes(ir_element_type);
		int align = get_type_alignment_bytes(ir_element_type);
		if(elemsize % align > 0) {
			elemsize += align - (elemsize % align);
		}
		set_type_size_bytes(ir_type, n_elements * elemsize);
		set_type_alignment_bytes(ir_type, align);
		set_type_state(ir_type, layout_fixed);
	}

	return ir_type;
}

#define INVALID_TYPE ((ir_type_ptr)-1)

static ir_type *create_struct_type(compound_type_t *type)
{
	symbol_t *symbol = type->declaration->symbol;
	ident    *id;
	if(symbol != NULL) {
		id = unique_ident(symbol->string);
	} else {
		id = unique_ident("__anonymous_struct");
	}
	ir_type *ir_type = new_type_struct(id);

	type->type.firm_type = ir_type;

	int align_all = 1;
	int offset    = 0;
	declaration_t *entry = type->declaration->context.declarations;
	for( ; entry != NULL; entry = entry->next) {
		if(entry->namespc != NAMESPACE_NORMAL)
			continue;

		ident       *ident         = new_id_from_str(entry->symbol->string);
		ir_type_ptr  entry_ir_type = get_ir_type(entry->type);

		int entry_size      = get_type_size_bytes(entry_ir_type);
		int entry_alignment = get_type_alignment_bytes(entry_ir_type);
		int misalign        = offset % entry_alignment;
		if (misalign != 0)
			offset += entry_alignment - misalign;

		ir_entity *entity = new_entity(ir_type, ident, entry_ir_type);
		set_entity_offset(entity, offset);
		add_struct_member(ir_type, entity);
		entry->declaration_type = DECLARATION_TYPE_COMPOUND_MEMBER;
		entry->v.entity         = entity;

		offset += entry_size;
		if(entry_alignment > align_all) {
			if(entry_alignment % align_all != 0) {
				panic("Uneven alignments not supported yet");
			}
			align_all = entry_alignment;
		}
	}

	int misalign = offset % align_all;
	offset += misalign;
	set_type_alignment_bytes(ir_type, align_all);
	set_type_size_bytes(ir_type, offset);
	set_type_state(ir_type, layout_fixed);

	return ir_type;
}

static ir_type *create_union_type(compound_type_t *type)
{
	declaration_t *declaration = type->declaration;
	symbol_t      *symbol      = declaration->symbol;
	ident         *id;
	if(symbol != NULL) {
		id = unique_ident(symbol->string);
	} else {
		id = unique_ident("__anonymous_union");
	}
	ir_type  *ir_type = new_type_union(id);

	type->type.firm_type = ir_type;

	int align_all = 1;
	int size      = 0;
	declaration_t *entry = declaration->context.declarations;
	for( ; entry != NULL; entry = entry->next) {
		if(entry->namespc != NAMESPACE_NORMAL)
			continue;

		ident       *ident         = new_id_from_str(entry->symbol->string);
		ir_type_ptr  entry_ir_type = get_ir_type(entry->type);

		int entry_size      = get_type_size_bytes(entry_ir_type);
		int entry_alignment = get_type_alignment_bytes(entry_ir_type);

		ir_entity *entity = new_entity(ir_type, ident, entry_ir_type);
		add_union_member(ir_type, entity);
		set_entity_offset(entity, 0);
		entry->declaration_type = DECLARATION_TYPE_COMPOUND_MEMBER;
		entry->v.entity         = entity;

		if(entry_size > size) {
			size = entry_size;
		}
		if(entry_alignment > align_all) {
			if(entry_alignment % align_all != 0) {
				panic("Uneven alignments not supported yet");
			}
			align_all = entry_alignment;
		}
	}

	set_type_alignment_bytes(ir_type, align_all);
	set_type_size_bytes(ir_type, size);
	set_type_state(ir_type, layout_fixed);

	return ir_type;
}

static ir_node *expression_to_firm(const expression_t *expression);
static inline ir_mode *get_ir_mode(type_t *type);

static ir_type *create_enum_type(enum_type_t *const type)
{
	type->type.firm_type = ir_type_int;

	ir_mode *const mode    = get_ir_mode((type_t*) type);
	tarval  *const one     = get_mode_one(mode);
	tarval  *      tv_next = get_tarval_null(mode);

	declaration_t *declaration = type->declaration->next;
	for (; declaration != NULL; declaration = declaration->next) {
		if (declaration->storage_class != STORAGE_CLASS_ENUM_ENTRY)
			break;

		declaration->declaration_type = DECLARATION_TYPE_ENUM_ENTRY;

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

	return ir_type_int;
}

static ir_type *get_ir_type(type_t *type)
{
	assert(type != NULL);

	type = skip_typeref(type);

	if(type->base.firm_type != NULL) {
		assert(type->base.firm_type != INVALID_TYPE);
		return type->base.firm_type;
	}

	ir_type *firm_type = NULL;
	switch(type->type) {
	case TYPE_ATOMIC:
		firm_type = create_atomic_type((atomic_type_t*) type);
		break;
	case TYPE_FUNCTION:
		firm_type = create_method_type((function_type_t*) type);
		break;
	case TYPE_POINTER:
		firm_type = create_pointer_type((pointer_type_t*) type);
		break;
	case TYPE_ARRAY:
		firm_type = create_array_type((array_type_t*) type);
		break;
	case TYPE_COMPOUND_STRUCT:
		firm_type = create_struct_type((compound_type_t*) type);
		break;
	case TYPE_COMPOUND_UNION:
		firm_type = create_union_type((compound_type_t*) type);
		break;
	case TYPE_ENUM:
		firm_type = create_enum_type((enum_type_t*) type);
		break;
	case TYPE_BUILTIN:
	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
	case TYPE_INVALID:
		break;
	}
	if(firm_type == NULL)
		panic("unknown type found");

	type->base.firm_type = firm_type;
	return firm_type;
}

static inline ir_mode *get_ir_mode(type_t *type)
{
	ir_type *irtype = get_ir_type(type);

	/* firm doesn't report a mode for arrays somehow... */
	if(is_Array_type(irtype)) {
		return mode_P;
	}

	ir_mode *mode = get_type_mode(irtype);
	assert(mode != NULL);
	return mode;
}

static ir_entity* get_function_entity(declaration_t *declaration)
{
	if(declaration->declaration_type == DECLARATION_TYPE_FUNCTION)
		return declaration->v.entity;
	assert(declaration->declaration_type == DECLARATION_TYPE_UNKNOWN);

	symbol_t *symbol = declaration->symbol;
	ident    *id     = new_id_from_str(symbol->string);

	ir_type  *global_type    = get_glob_type();
	ir_type  *ir_type_method = get_ir_type(declaration->type);
	assert(is_Method_type(ir_type_method));

	ir_entity *entity = new_entity(global_type, id, ir_type_method);
	set_entity_ld_ident(entity, id);
	if(declaration->storage_class == STORAGE_CLASS_STATIC
			|| declaration->is_inline) {
		set_entity_visibility(entity, visibility_local);
	} else if(declaration->init.statement != NULL) {
		set_entity_visibility(entity, visibility_external_visible);
	} else {
		set_entity_visibility(entity, visibility_external_allocated);
	}

	declaration->declaration_type = DECLARATION_TYPE_FUNCTION;
	declaration->v.entity         = entity;

	return entity;
}

static dbg_info *get_dbg_info(const source_position_t *pos)
{
	return (dbg_info*) pos;
}

static ir_node *const_to_firm(const const_t *cnst)
{
	dbg_info *dbgi = get_dbg_info(&cnst->expression.source_position);
	ir_mode  *mode = get_ir_mode(cnst->expression.datatype);

	char    buf[128];
	tarval *tv;
	size_t  len;
	if(mode_is_float(mode)) {
		tv = new_tarval_from_double(cnst->v.float_value, mode);
	} else {
		if(mode_is_signed(mode)) {
			len = snprintf(buf, sizeof(buf), "%lld", cnst->v.int_value);
		} else {
			len = snprintf(buf, sizeof(buf), "%llu", cnst->v.int_value);
		}
		tv = new_tarval_from_str(buf, len, mode);
	}

	return new_d_Const(dbgi, mode, tv);
}

static ir_node *create_symconst(dbg_info *dbgi, ir_entity *entity)
{
	assert(entity != NULL);
	union symconst_symbol sym;
	sym.entity_p = entity;
	return new_d_SymConst(dbgi, sym, symconst_addr_ent);
}

static ir_node *string_to_firm(const source_position_t *const src_pos,
                               const char *const id_prefix,
                               const char *const string)
{
	ir_type *const global_type = get_glob_type();
	ir_type *const type        = new_type_array(unique_ident("strtype"), 1,
	                                            ir_type_const_char);

	ident     *const id     = unique_ident(id_prefix);
	ir_entity *const entity = new_entity(global_type, id, type);
	set_entity_ld_ident(entity, id);
	set_entity_variability(entity, variability_constant);

	ir_type *const elem_type = ir_type_const_char;
	ir_mode *const mode      = get_type_mode(elem_type);

	const size_t slen = strlen(string) + 1;

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

	dbg_info *const dbgi = get_dbg_info(src_pos);

	return create_symconst(dbgi, entity);
}

static ir_node *string_literal_to_firm(const string_literal_t* literal)
{
	return string_to_firm(&literal->expression.source_position, "Lstr",
	                      literal->value);
}

static ir_node *deref_address(ir_type *const irtype, ir_node *const addr,
                              dbg_info *const dbgi)
{
	if(is_compound_type(irtype) || is_Array_type(irtype)) {
		return addr;
	}

	ir_mode *const mode     = get_type_mode(irtype);
	ir_node *const memory   = get_store();
	ir_node *const load     = new_d_Load(dbgi, memory, addr, mode);
	ir_node *const load_mem = new_d_Proj(dbgi, load, mode_M, pn_Load_M);
	ir_node *const load_res = new_d_Proj(dbgi, load, mode,   pn_Load_res);
	set_store(load_mem);
	return load_res;
}

static ir_node *reference_expression_to_firm(const reference_expression_t *ref)
{
	dbg_info      *dbgi        = get_dbg_info(&ref->expression.source_position);
	declaration_t *declaration = ref->declaration;
	type_t        *type        = skip_typeref(declaration->type);

	switch((declaration_type_t) declaration->declaration_type) {
	case DECLARATION_TYPE_UNKNOWN:
		if (declaration->storage_class != STORAGE_CLASS_ENUM_ENTRY) {
			break;
		}
		get_ir_type(type);
		/* FALLTHROUGH */

	case DECLARATION_TYPE_ENUM_ENTRY: {
		ir_mode *const mode = get_ir_mode(type);
		return new_Const(mode, declaration->v.enum_val);
	}

	case DECLARATION_TYPE_LOCAL_VARIABLE: {
		ir_mode *mode = get_ir_mode(type);
		return get_value(declaration->v.value_number, mode);
	}
	case DECLARATION_TYPE_FUNCTION: {
		return create_symconst(dbgi, declaration->v.entity);
	}
	case DECLARATION_TYPE_GLOBAL_VARIABLE: {
		ir_entity *entity   = declaration->v.entity;
		ir_node   *symconst = create_symconst(dbgi, entity);
		ir_type   *irtype   = get_entity_type(entity);
		return deref_address(irtype, symconst, dbgi);
	}
	case DECLARATION_TYPE_LOCAL_VARIABLE_ENTITY: {
		ir_entity *entity = declaration->v.entity;
		ir_node   *frame  = get_irg_frame(current_ir_graph);
		ir_node   *sel    = new_d_simpleSel(dbgi, new_NoMem(), frame, entity);
		ir_type   *irtype = get_entity_type(entity);
		return deref_address(irtype, sel, dbgi);
	}

	case DECLARATION_TYPE_COMPOUND_MEMBER:
	case DECLARATION_TYPE_LABEL_BLOCK:
		panic("not implemented reference type");
	}

	panic("reference to declaration with unknown type found");
}

static ir_node *reference_addr(const reference_expression_t *ref)
{
	dbg_info      *dbgi        = get_dbg_info(&ref->expression.source_position);
	declaration_t *declaration = ref->declaration;

	switch((declaration_type_t) declaration->declaration_type) {
	case DECLARATION_TYPE_UNKNOWN:
		break;
	case DECLARATION_TYPE_LOCAL_VARIABLE:
		panic("local variable without entity has no address");
	case DECLARATION_TYPE_FUNCTION: {
		return create_symconst(dbgi, declaration->v.entity);
	}
	case DECLARATION_TYPE_GLOBAL_VARIABLE: {
		ir_entity *entity   = declaration->v.entity;
		ir_node   *symconst = create_symconst(dbgi, entity);
		return symconst;
	}
	case DECLARATION_TYPE_LOCAL_VARIABLE_ENTITY: {
		ir_entity *entity = declaration->v.entity;
		ir_node   *frame  = get_irg_frame(current_ir_graph);
		ir_node   *sel    = new_d_simpleSel(dbgi, new_NoMem(), frame, entity);

		return sel;
	}

	case DECLARATION_TYPE_ENUM_ENTRY:
		panic("trying to reference enum entry");

	case DECLARATION_TYPE_COMPOUND_MEMBER:
	case DECLARATION_TYPE_LABEL_BLOCK:
		panic("not implemented reference type");
	}

	panic("reference to declaration with unknown type found");
}

static ir_node *process_builtin_call(const call_expression_t *call)
{
	dbg_info *dbgi = get_dbg_info(&call->expression.source_position);

	assert(call->function->type == EXPR_BUILTIN_SYMBOL);
	builtin_symbol_expression_t *builtin
		= (builtin_symbol_expression_t*) call->function;
	symbol_t *symbol = builtin->symbol;

	if(symbol == symbol_alloca) {
		if(call->arguments == NULL || call->arguments->next != NULL) {
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
	} else {
		panic("Unsupported builtin found\n");
	}
}

static ir_node *call_expression_to_firm(const call_expression_t *call)
{
	assert(get_cur_block() != NULL);

	expression_t  *function = call->function;
	if(function->type == EXPR_BUILTIN_SYMBOL) {
		return process_builtin_call(call);
	}
	ir_node       *callee   = expression_to_firm(function);

	function_type_t *function_type;
	if (function->datatype->type == TYPE_POINTER) {
		pointer_type_t *const ptr_type = (pointer_type_t*)function->datatype;
		assert(ptr_type->points_to->type == TYPE_FUNCTION);
		function_type = (function_type_t*)ptr_type->points_to;
	} else {
		assert(function->datatype->type == TYPE_FUNCTION);
		function_type = (function_type_t*)function->datatype;
	}

	int              n_parameters = 0;
	call_argument_t *argument     = call->arguments;
	for( ; argument != NULL; argument = argument->next) {
		++n_parameters;
	}

	ir_type *ir_method_type  = get_ir_type((type_t*) function_type);
	ir_type *new_method_type = NULL;
	if(function_type->variadic || function_type->unspecified_parameters) {
		/* we need to construct a new method type matching the call
		 * arguments... */
		int n_res       = get_method_n_ress(ir_method_type);
		new_method_type = new_type_method(unique_ident("calltype"),
		                                  n_parameters, n_res);
		set_method_calling_convention(new_method_type,
		               get_method_calling_convention(ir_method_type));
		set_method_additional_properties(new_method_type,
		               get_method_additional_properties(ir_method_type));

		for(int i = 0; i < n_res; ++i) {
			set_method_res_type(new_method_type, i,
			                    get_method_res_type(ir_method_type, i));
		}
	}
	ir_node *in[n_parameters];

	argument = call->arguments;
	int n = 0;
	for( ; argument != NULL; argument = argument->next) {
		expression_t *expression = argument->expression;
		ir_node      *arg_node   = expression_to_firm(expression);

		in[n] = arg_node;
		if(new_method_type != NULL) {
			ir_type *irtype = get_ir_type(expression->datatype);
			set_method_param_type(new_method_type, n, irtype);
		}

		n++;
	}
	assert(n == n_parameters);

	if(new_method_type != NULL)
		ir_method_type = new_method_type;

	dbg_info *dbgi  = get_dbg_info(&call->expression.source_position);
	ir_node  *store = get_store();
	ir_node  *node  = new_d_Call(dbgi, store, callee, n_parameters, in,
	                             ir_method_type);
	ir_node  *mem   = new_d_Proj(dbgi, node, mode_M, pn_Call_M_regular);
	set_store(mem);

	type_t  *result_type = function_type->result_type;
	ir_node *result      = NULL;
	if(result_type != type_void) {
		ir_mode *mode    = get_ir_mode(result_type);
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

static void set_value_for_expression(const expression_t *expression,
                                     ir_node *value)
{
	if(expression->type == EXPR_REFERENCE) {
		reference_expression_t *ref = (reference_expression_t*) expression;

		declaration_t *declaration = ref->declaration;
		assert(declaration->declaration_type != DECLARATION_TYPE_UNKNOWN);
		if(declaration->declaration_type == DECLARATION_TYPE_LOCAL_VARIABLE) {
			set_value(declaration->v.value_number, value);
			return;
		}
	}

	dbg_info *dbgi      = get_dbg_info(&expression->source_position);
	ir_node  *addr      = expression_to_addr(expression);
	assert(get_irn_mode(value) == get_ir_mode(expression->datatype));
	ir_node  *memory    = get_store();
	ir_node  *store     = new_d_Store(dbgi, memory, addr, value);
	ir_node  *store_mem = new_d_Proj(dbgi, store, mode_M, pn_Store_M);
	set_store(store_mem);
}

static ir_node *create_conv(dbg_info *dbgi, ir_node *value, ir_mode *dest_mode)
{
	ir_mode *value_mode = get_irn_mode(value);

	if (value_mode == dest_mode || is_Bad(value))
		return value;

	if(dest_mode == mode_b) {
		ir_node *zero = new_Const(value_mode, get_mode_null(value_mode));
		ir_node *cmp  = new_d_Cmp(dbgi, value, zero);
		ir_node *proj = new_d_Proj(dbgi, cmp, mode_b, pn_Cmp_Lg);
		return proj;
	}

	return new_d_Conv(dbgi, value, dest_mode);
}

static ir_node *create_incdec(const unary_expression_t *expression)
{
	dbg_info     *dbgi  = get_dbg_info(&expression->expression.source_position);
	type_t       *type  = expression->expression.datatype;
	ir_mode      *mode  = get_ir_mode(type);
	expression_t *value = expression->value;

	ir_node *value_node = expression_to_firm(value);

	ir_node *offset;
	if(type->type == TYPE_POINTER) {
		pointer_type_t *pointer_type = (pointer_type_t*) type;
		unsigned        elem_size    = get_type_size(pointer_type->points_to);
		offset = new_Const_long(mode_Is, elem_size);
	} else {
		assert(is_type_arithmetic(type));
		offset = new_Const(mode, get_mode_one(mode));
	}

	switch(expression->type) {
	case UNEXPR_POSTFIX_INCREMENT: {
		ir_node *new_value = new_d_Add(dbgi, value_node, offset, mode);
		set_value_for_expression(value, new_value);
		return value_node;
	}
	case UNEXPR_POSTFIX_DECREMENT: {
		ir_node *new_value = new_d_Sub(dbgi, value_node, offset, mode);
		set_value_for_expression(value, new_value);
		return value_node;
	}
	case UNEXPR_PREFIX_INCREMENT: {
		ir_node *new_value = new_d_Add(dbgi, value_node, offset, mode);
		set_value_for_expression(value, new_value);
		return new_value;
	}
	case UNEXPR_PREFIX_DECREMENT: {
		ir_node *new_value = new_d_Sub(dbgi, value_node, offset, mode);
		set_value_for_expression(value, new_value);
		return new_value;
	}
	default:
		panic("no incdec expr in create_incdec");
		return NULL;
	}
}

static ir_node *unary_expression_to_firm(const unary_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->expression.source_position);
	type_t   *type = skip_typeref(expression->expression.datatype);

	if(expression->type == UNEXPR_TAKE_ADDRESS)
		return expression_to_addr(expression->value);

	const expression_t *value      = expression->value;
	ir_node            *value_node = expression_to_firm(value);

	switch(expression->type) {
	case UNEXPR_NEGATE:
		return new_d_Minus(dbgi, value_node, get_ir_mode(type));
	case UNEXPR_PLUS:
		return value_node;
	case UNEXPR_BITWISE_NEGATE:
		return new_d_Not(dbgi, value_node, get_ir_mode(type));
	case UNEXPR_NOT: {
		if(get_irn_mode(value_node) != mode_b) {
			value_node = create_conv(dbgi, value_node, mode_b);
		}
		value_node = new_d_Not(dbgi, value_node, mode_b);
		ir_mode *const mode = get_ir_mode(type);
		if(mode != mode_b) {
			value_node = create_conv(dbgi, value_node, mode);
		}
		return value_node;
	}
	case UNEXPR_DEREFERENCE: {
		ir_type *irtype = get_ir_type(type);
		return deref_address(irtype, value_node, dbgi);
	}
	case UNEXPR_POSTFIX_INCREMENT:
	case UNEXPR_POSTFIX_DECREMENT:
	case UNEXPR_PREFIX_INCREMENT:
	case UNEXPR_PREFIX_DECREMENT:
		return create_incdec(expression);
	case UNEXPR_CAST:
		return create_conv(dbgi, value_node, get_ir_mode(type));

	case UNEXPR_TAKE_ADDRESS:
	case UNEXPR_INVALID:
		break;
	}
	panic("invalid UNEXPR type found");
}

static long get_pnc(binary_expression_type_t type)
{
	switch(type) {
	case BINEXPR_EQUAL:        return pn_Cmp_Eq;
	case BINEXPR_NOTEQUAL:     return pn_Cmp_Lg;
	case BINEXPR_LESS:         return pn_Cmp_Lt;
	case BINEXPR_LESSEQUAL:    return pn_Cmp_Le;
	case BINEXPR_GREATER:      return pn_Cmp_Gt;
	case BINEXPR_GREATEREQUAL: return pn_Cmp_Ge;
	default:
		break;
	}
	panic("trying to get pn_Cmp from non-comparison binexpr type");
}

static ir_node *create_lazy_op(const binary_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->expression.source_position);
	type_t   *type = expression->expression.datatype;
	ir_mode  *mode = get_ir_mode(type);

	ir_node *cur_block = get_cur_block();

	ir_node *one_block = new_immBlock();
	ir_node *one       = new_Const(mode, get_mode_one(mode));
	ir_node *jmp_one   = new_d_Jmp(dbgi);

	ir_node *zero_block = new_immBlock();
	ir_node *zero       = new_Const(mode, get_mode_null(mode));
	ir_node *jmp_zero   = new_d_Jmp(dbgi);

	set_cur_block(cur_block);
	create_condition_evaluation((const expression_t*) expression,
	                            one_block, zero_block);
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

typedef ir_node * (*create_arithmetic_func)(dbg_info *dbgi, ir_node *left,
                                            ir_node *right, ir_mode *mode);

static ir_node *create_arithmetic_binop(const binary_expression_t *expression,
                                        create_arithmetic_func func)
{
	dbg_info *dbgi  = get_dbg_info(&expression->expression.source_position);
	ir_node  *left  = expression_to_firm(expression->left);
	ir_node  *right = expression_to_firm(expression->right);
	type_t   *type  = expression->right->datatype;
	/* be careful with the modes, because in arithmetic assign nodes only
	 * the right operand has the mode of the arithmetic already */
	ir_mode  *mode  = get_ir_mode(type);
	left            = create_conv(dbgi, left, mode);
	ir_node  *res   = func(dbgi, left, right, mode);

	return res;
}

static ir_node *pointer_arithmetic(ir_node  *const pointer,
                                   ir_node  *      integer,
                                   type_t   *const type,
                                   dbg_info *const dbgi,
                                   const create_arithmetic_func func)
{
	pointer_type_t *const pointer_type = (pointer_type_t*)type;
	type_t         *const points_to    = pointer_type->points_to;
	const unsigned        elem_size    = get_type_size(points_to);

	assert(elem_size >= 1);
	if (elem_size > 1) {
		integer             = create_conv(dbgi, integer, mode_Is);
		ir_node *const cnst = new_Const_long(mode_Is, (long)elem_size);
		ir_node *const mul  = new_d_Mul(dbgi, integer, cnst, mode_Is);
		integer = mul;
	}

	ir_mode *const mode = get_ir_mode(type);
	return func(dbgi, pointer, integer, mode);
}

static ir_node *create_arithmetic_assign_binop(
		const binary_expression_t *expression, create_arithmetic_func func)
{
	dbg_info *const dbgi = get_dbg_info(&expression->expression.source_position);
	type_t   *const type = expression->expression.datatype;
	ir_node  *value;

	if (type->type == TYPE_POINTER) {
		ir_node        *const pointer = expression_to_firm(expression->left);
		ir_node        *      integer = expression_to_firm(expression->right);
		value = pointer_arithmetic(pointer, integer, type, dbgi, func);
	} else {
		value = create_arithmetic_binop(expression, func);
	}

	ir_mode  *const mode = get_ir_mode(type);
	value = create_conv(dbgi, value, mode);
	set_value_for_expression(expression->left, value);

	return value;
}

static ir_node *create_add(const binary_expression_t *expression)
{
	dbg_info *dbgi  = get_dbg_info(&expression->expression.source_position);
	ir_node  *left  = expression_to_firm(expression->left);
	ir_node  *right = expression_to_firm(expression->right);
	type_t   *type  = expression->expression.datatype;

	expression_t *expr_left  = expression->left;
	expression_t *expr_right = expression->right;
	type_t       *type_left  = skip_typeref(expr_left->datatype);
	type_t       *type_right = skip_typeref(expr_right->datatype);

	if(is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		ir_mode *const mode = get_ir_mode(type);
		return new_d_Add(dbgi, left, right, mode);
	}

	if (type_left->type == TYPE_POINTER || type_left->type == TYPE_ARRAY) {
		return pointer_arithmetic(left, right, type, dbgi, new_d_Add);
	} else {
		assert(type_right->type == TYPE_POINTER || type_right->type == TYPE_ARRAY);
		return pointer_arithmetic(right, left, type, dbgi, new_d_Add);
	}
}

static ir_node *create_sub(const binary_expression_t *expression)
{
	dbg_info *const dbgi  = get_dbg_info(&expression->expression.source_position);
	expression_t *const expr_left  = expression->left;
	expression_t *const expr_right = expression->right;
	ir_node      *const left       = expression_to_firm(expr_left);
	ir_node      *const right      = expression_to_firm(expr_right);
	type_t       *const type       = expression->expression.datatype;
	type_t       *const type_left  = skip_typeref(expr_left->datatype);
	type_t       *const type_right = skip_typeref(expr_right->datatype);

	if (is_type_arithmetic(type_left) && is_type_arithmetic(type_right)) {
		ir_mode *const mode = get_ir_mode(type);
		return new_d_Sub(dbgi, left, right, mode);
	} else if (type_left->type == TYPE_POINTER && type_right->type == TYPE_POINTER) {
		const pointer_type_t *const ptr_type = (const pointer_type_t*)type_left;
		const unsigned elem_size             = get_type_size(ptr_type->points_to);
		ir_mode *const mode   = get_ir_mode(type);
		ir_node *const sub    = new_d_Sub(dbgi, left, right, mode);
		ir_node *const cnst   = new_Const_long(mode_Is, (long)elem_size);
		ir_node *const no_mem = new_NoMem();
		ir_node *const div    = new_d_Div(dbgi, no_mem, sub, cnst, mode,
		                                  op_pin_state_floats);
		return new_d_Proj(dbgi, div, mode, pn_Div_res);
	}

	assert(type_left->type == TYPE_POINTER);
	return pointer_arithmetic(left, right, type_left, dbgi, new_d_Sub);
}

static ir_node *create_shift(const binary_expression_t *expression)
{
	dbg_info *dbgi  = get_dbg_info(&expression->expression.source_position);
	ir_node  *left  = expression_to_firm(expression->left);
	ir_node  *right = expression_to_firm(expression->right);
	type_t   *type  = expression->expression.datatype;
	ir_mode  *mode  = get_ir_mode(type);

	/* firm always wants the shift count to be unsigned */
	right = create_conv(dbgi, right, mode_Iu);

	ir_node *res;

	switch(expression->type) {
	case BINEXPR_SHIFTLEFT_ASSIGN:
	case BINEXPR_SHIFTLEFT:
		res = new_d_Shl(dbgi, left, right, mode);
		break;
	case BINEXPR_SHIFTRIGHT_ASSIGN:
	case BINEXPR_SHIFTRIGHT: {
	 	 expression_t *expr_left = expression->left;
		 type_t       *type_left = skip_typeref(expr_left->datatype);

		 if(is_type_signed(type_left)) {
			res = new_d_Shrs(dbgi, left, right, mode);
		 } else {
		 	 res = new_d_Shr(dbgi, left, right, mode);
		 }
		 break;
	}
	default:
		panic("create shift op called for non-shift op");
	}

	return res;
}


static ir_node *create_divmod(const binary_expression_t *expression)
{
	dbg_info *dbgi  = get_dbg_info(&expression->expression.source_position);
	ir_node  *left  = expression_to_firm(expression->left);
	ir_node  *right = expression_to_firm(expression->right);
	ir_node  *pin   = new_Pin(new_NoMem());
	/* be careful with the modes, because in arithmetic assign nodes only
	 * the right operand has the mode of the arithmetic already */
	type_t   *type  = expression->right->datatype;
	ir_mode  *mode  = get_ir_mode(type);
	left            = create_conv(dbgi, left, mode);
	ir_node  *op;
	ir_node  *res;

	switch (expression->type)  {
		case BINEXPR_DIV:
		case BINEXPR_DIV_ASSIGN:
			if(mode_is_float(mode)) {
				op  = new_d_Quot(dbgi, pin, left, right, mode, op_pin_state_floats);
				res = new_d_Proj(dbgi, op, mode, pn_Quot_res);
			} else {
				op  = new_d_Div(dbgi, pin, left, right, mode, op_pin_state_floats);
				res = new_d_Proj(dbgi, op, mode, pn_Div_res);
			}
			break;

		case BINEXPR_MOD:
		case BINEXPR_MOD_ASSIGN:
			assert(!mode_is_float(mode));
			op  = new_d_Mod(dbgi, pin, left, right, mode, op_pin_state_floats);
			res = new_d_Proj(dbgi, op, mode, pn_Mod_res);
			break;

		default: panic("unexpected binary expression type in create_divmod()");
	}

	return res;
}

static ir_node *create_arithmetic_assign_divmod(
		const binary_expression_t *expression)
{
	ir_node  *      value = create_divmod(expression);
	dbg_info *const dbgi  = get_dbg_info(&expression->expression.source_position);
	type_t   *const type  = expression->expression.datatype;
	ir_mode  *const mode  = get_ir_mode(type);

	assert(type->type != TYPE_POINTER);

	value = create_conv(dbgi, value, mode);
	set_value_for_expression(expression->left, value);

	return value;
}

static ir_node *create_arithmetic_assign_shift(
		const binary_expression_t *expression)
{
	ir_node  *      value = create_shift(expression);
	dbg_info *const dbgi  = get_dbg_info(&expression->expression.source_position);
	type_t   *const type  = expression->expression.datatype;
	ir_mode  *const mode  = get_ir_mode(type);

	value = create_conv(dbgi, value, mode);
	set_value_for_expression(expression->left, value);

	return value;
}

static ir_node *binary_expression_to_firm(const binary_expression_t *expression)
{
	binary_expression_type_t type = expression->type;
	switch(type) {
	case BINEXPR_EQUAL:
	case BINEXPR_NOTEQUAL:
	case BINEXPR_LESS:
	case BINEXPR_LESSEQUAL:
	case BINEXPR_GREATER:
	case BINEXPR_GREATEREQUAL: {
		dbg_info *dbgi = get_dbg_info(&expression->expression.source_position);
		ir_node *left  = expression_to_firm(expression->left);
		ir_node *right = expression_to_firm(expression->right);
		ir_node *cmp   = new_d_Cmp(dbgi, left, right);
		long     pnc   = get_pnc(type);
		ir_node *proj  = new_d_Proj(dbgi, cmp, mode_b, pnc);
		return proj;
	}
	case BINEXPR_ASSIGN: {
		ir_node *right = expression_to_firm(expression->right);
		set_value_for_expression(expression->left, right);
		return right;
	}
	case BINEXPR_ADD:
		return create_add(expression);
	case BINEXPR_SUB:
		return create_sub(expression);
	case BINEXPR_MUL:
		return create_arithmetic_binop(expression, new_d_Mul);
	case BINEXPR_BITWISE_AND:
		return create_arithmetic_binop(expression, new_d_And);
	case BINEXPR_BITWISE_OR:
		return create_arithmetic_binop(expression, new_d_Or);
	case BINEXPR_BITWISE_XOR:
		return create_arithmetic_binop(expression, new_d_Eor);
	case BINEXPR_SHIFTLEFT:
	case BINEXPR_SHIFTRIGHT:
		return create_shift(expression);
	case BINEXPR_DIV:
	case BINEXPR_MOD:
		return create_divmod(expression);
	case BINEXPR_LOGICAL_AND:
	case BINEXPR_LOGICAL_OR:
		return create_lazy_op(expression);
	case BINEXPR_COMMA:
		expression_to_firm(expression->left);
		return expression_to_firm(expression->right);
	case BINEXPR_ADD_ASSIGN:
		return create_arithmetic_assign_binop(expression, new_d_Add);
	case BINEXPR_SUB_ASSIGN:
		return create_arithmetic_assign_binop(expression, new_d_Sub);
	case BINEXPR_MUL_ASSIGN:
		return create_arithmetic_assign_binop(expression, new_d_Mul);
	case BINEXPR_DIV_ASSIGN:
		return create_arithmetic_assign_divmod(expression);
	case BINEXPR_BITWISE_AND_ASSIGN:
		return create_arithmetic_assign_binop(expression, new_d_And);
	case BINEXPR_BITWISE_OR_ASSIGN:
		return create_arithmetic_assign_binop(expression, new_d_Or);
	case BINEXPR_BITWISE_XOR_ASSIGN:
		return create_arithmetic_assign_binop(expression, new_d_Eor);
	case BINEXPR_SHIFTLEFT_ASSIGN:
	case BINEXPR_SHIFTRIGHT_ASSIGN:
		return create_arithmetic_assign_shift(expression);
	default:
		panic("TODO binexpr type");
	}
}

static ir_node *array_access_addr(const array_access_expression_t *expression)
{
	dbg_info *dbgi      = get_dbg_info(&expression->expression.source_position);
	ir_node  *base_addr = expression_to_firm(expression->array_ref);
	ir_node  *offset    = expression_to_firm(expression->index);
	offset              = create_conv(dbgi, offset, mode_Iu);

	type_t *ref_type = skip_typeref(expression->array_ref->datatype);
	assert(is_type_pointer(ref_type));
	pointer_type_t *pointer_type = (pointer_type_t*) ref_type;

	unsigned elem_size       = get_type_size(pointer_type->points_to);
	ir_node *elem_size_const = new_Const_long(mode_Iu, elem_size);
	ir_node *real_offset     = new_d_Mul(dbgi, offset, elem_size_const,
	                                     mode_Iu);
	ir_node *result          = new_d_Add(dbgi, base_addr, real_offset, mode_P);

	return result;
}

static ir_node *array_access_to_firm(
		const array_access_expression_t *expression)
{
	dbg_info *dbgi   = get_dbg_info(&expression->expression.source_position);
	ir_node  *addr   = array_access_addr(expression);
	type_t   *type   = revert_automatic_type_conversion(
			(const expression_t*) expression);
	type             = skip_typeref(type);
	ir_type  *irtype = get_ir_type(type);

	return deref_address(irtype, addr, dbgi);
}

static ir_node *sizeof_to_firm(const sizeof_expression_t *expression)
{
	type_t *type = expression->type;
	if(type == NULL) {
		type = expression->size_expression->datatype;
		assert(type != NULL);
	}

	ir_mode  *mode      = get_ir_mode(expression->expression.datatype);
	unsigned  size      = get_type_size(type);
	ir_node  *size_node = new_Const_long(mode, size);

	return size_node;
}

static ir_node *conditional_to_firm(const conditional_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->expression.source_position);

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
	if (true_val == NULL || false_val == NULL) return NULL;

	ir_node *in[2] = { true_val, false_val };
	ir_mode *mode  = get_irn_mode(true_val);
	assert(get_irn_mode(false_val) == mode);
	ir_node *val   = new_d_Phi(dbgi, 2, in, mode);

	return val;
}

static ir_node *select_addr(const select_expression_t *expression)
{
	dbg_info *dbgi = get_dbg_info(&expression->expression.source_position);

	ir_node *compound_addr = expression_to_firm(expression->compound);

	declaration_t *entry = expression->compound_entry;
	assert(entry->declaration_type == DECLARATION_TYPE_COMPOUND_MEMBER);
	ir_entity     *entity = entry->v.entity;

	assert(entity != NULL);

	ir_node *sel = new_d_simpleSel(dbgi, new_NoMem(), compound_addr, entity);

	return sel;
}

static ir_node *select_to_firm(const select_expression_t *expression)
{
	dbg_info *dbgi   = get_dbg_info(&expression->expression.source_position);
	ir_node  *addr   = select_addr(expression);
	type_t   *type   = revert_automatic_type_conversion(
			(const expression_t*) expression);
	type             = skip_typeref(type);
	ir_type  *irtype = get_ir_type(type);

	return deref_address(irtype, addr, dbgi);
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
	const type_t *const type = expr->type_expression->datatype;

	gcc_type_class tc;
	switch (type->type)
	{
		case TYPE_ATOMIC: {
			const atomic_type_t *const atomic_type = (const atomic_type_t*)type;
			switch (atomic_type->atype) {
				// should not be reached
				case ATOMIC_TYPE_INVALID:
					tc = no_type_class;
					break;

				// gcc cannot do that
				case ATOMIC_TYPE_VOID:
					tc = void_type_class;
					break;

				case ATOMIC_TYPE_CHAR:      // gcc handles this as integer
				case ATOMIC_TYPE_SCHAR:     // gcc handles this as integer
				case ATOMIC_TYPE_UCHAR:     // gcc handles this as integer
				case ATOMIC_TYPE_SHORT:
				case ATOMIC_TYPE_USHORT:
				case ATOMIC_TYPE_INT:
				case ATOMIC_TYPE_UINT:
				case ATOMIC_TYPE_LONG:
				case ATOMIC_TYPE_ULONG:
				case ATOMIC_TYPE_LONGLONG:
				case ATOMIC_TYPE_ULONGLONG:
				case ATOMIC_TYPE_BOOL:      // gcc handles this as integer
					tc = integer_type_class;
					break;

				case ATOMIC_TYPE_FLOAT:
				case ATOMIC_TYPE_DOUBLE:
				case ATOMIC_TYPE_LONG_DOUBLE:
					tc = real_type_class;
					break;

#ifdef PROVIDE_COMPLEX
				case ATOMIC_TYPE_FLOAT_COMPLEX:
				case ATOMIC_TYPE_DOUBLE_COMPLEX:
				case ATOMIC_TYPE_LONG_DOUBLE_COMPLEX:
					tc = complex_type_class;
					break;
				case ATOMIC_TYPE_FLOAT_IMAGINARY:
				case ATOMIC_TYPE_DOUBLE_IMAGINARY:
				case ATOMIC_TYPE_LONG_DOUBLE_IMAGINARY:
					tc = complex_type_class;
					break;
#endif

				default:
					panic("Unimplemented case in classify_type_to_firm().");
			}
			break;
		}

		case TYPE_ARRAY:           // gcc handles this as pointer
		case TYPE_FUNCTION:        // gcc handles this as pointer
		case TYPE_POINTER:         tc = pointer_type_class; break;
		case TYPE_COMPOUND_STRUCT: tc = record_type_class;  break;
		case TYPE_COMPOUND_UNION:  tc = union_type_class;   break;

		// gcc handles this as integer
		case TYPE_ENUM:            tc = integer_type_class; break;

		default:
			panic("Unimplemented case in classify_type_to_firm().");
	}

	dbg_info *const dbgi = get_dbg_info(&expr->expression.source_position);
	ir_mode  *const mode = mode_Is;
	tarval   *const tv   = new_tarval_from_long(tc, mode);
	return new_d_Const(dbgi, mode, tv);
}

static ir_node *function_name_to_firm(const string_literal_t *const expr)
{
	if (current_function_name == NULL) {
		const source_position_t *const src_pos =
			&expr->expression.source_position;
		const char *const name = current_function_decl->symbol->string;
		current_function_name = string_to_firm(src_pos, "__func__", name);
	}

	return current_function_name;
}

static ir_node *statement_expression_to_firm(const statement_expression_t *expr)
{
	statement_t *statement = expr->statement;

	assert(statement->type == STATEMENT_COMPOUND);
	return compound_statement_to_firm((compound_statement_t*) statement);
}

static ir_node *dereference_addr(const unary_expression_t *const expression)
{
	assert(expression->type == UNEXPR_DEREFERENCE);
	return expression_to_firm(expression->value);
}

static ir_node *expression_to_addr(const expression_t *expression)
{
	switch(expression->type) {
	case EXPR_REFERENCE:
		return reference_addr((const reference_expression_t*) expression);
	case EXPR_ARRAY_ACCESS:
		return array_access_addr((const array_access_expression_t*) expression);
	case EXPR_SELECT:
		return select_addr((const select_expression_t*) expression);
	case EXPR_UNARY: {
		const unary_expression_t *const unary_expr =
			(const unary_expression_t*)expression;
		if (unary_expr->type == UNEXPR_DEREFERENCE) {
			return dereference_addr(unary_expr);
		}
		break;
	}
	default:
		break;
	}
	panic("trying to get address of non-lvalue");
}

static ir_node *_expression_to_firm(const expression_t *expression)
{
	switch(expression->type) {
	case EXPR_CONST:
		return const_to_firm((const const_t*) expression);
	case EXPR_STRING_LITERAL:
		return string_literal_to_firm((const string_literal_t*) expression);
	case EXPR_REFERENCE:
		return reference_expression_to_firm(
				(const reference_expression_t*) expression);
	case EXPR_CALL:
		return call_expression_to_firm((const call_expression_t*) expression);
	case EXPR_UNARY:
		return unary_expression_to_firm((const unary_expression_t*) expression);
	case EXPR_BINARY:
		return binary_expression_to_firm(
				(const binary_expression_t*) expression);
	case EXPR_ARRAY_ACCESS:
		return array_access_to_firm(
				(const array_access_expression_t*) expression);
	case EXPR_SIZEOF:
		return sizeof_to_firm((const sizeof_expression_t*) expression);
	case EXPR_CONDITIONAL:
		return conditional_to_firm((const conditional_expression_t*)expression);
	case EXPR_SELECT:
		return select_to_firm((const select_expression_t*) expression);
	case EXPR_CLASSIFY_TYPE:
		return classify_type_to_firm((const classify_type_expression_t*)expression);
	case EXPR_FUNCTION:
	case EXPR_PRETTY_FUNCTION:
		return function_name_to_firm((const string_literal_t*)expression);
	case EXPR_STATEMENT:
		return statement_expression_to_firm(
				(const statement_expression_t*) expression);
	case EXPR_OFFSETOF:
	case EXPR_VA_ARG:
	case EXPR_BUILTIN_SYMBOL:
		panic("unimplemented expression found");

	case EXPR_UNKNOWN:
	case EXPR_INVALID:
		break;
	}
	panic("invalid expression found");
}

static ir_node *expression_to_firm(const expression_t *expression)
{
	ir_node *res = _expression_to_firm(expression);

	if(res != NULL && get_irn_mode(res) == mode_b) {
		ir_mode *mode = get_ir_mode(expression->datatype);
		res           = create_conv(NULL, res, mode);
	}

	return res;
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
	switch(expression->type) {
	case EXPR_UNARY: {
		unary_expression_t *unary_expression = (unary_expression_t*) expression;
		if(unary_expression->type == UNEXPR_NOT) {
			create_condition_evaluation(unary_expression->value, false_block,
			                            true_block);
			return;
		}
		break;
	}
	case EXPR_BINARY: {
		binary_expression_t *binary_expression
			= (binary_expression_t*) expression;
		if(binary_expression->type == BINEXPR_LOGICAL_AND) {
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
		if(binary_expression->type == BINEXPR_LOGICAL_OR) {
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
		break;
	}
	default:
		break;
	}

	dbg_info *dbgi       = get_dbg_info(&expression->source_position);
	ir_node  *condition  = expression_to_modeb(expression);
	ir_node  *cond       = new_d_Cond(dbgi, condition);
	ir_node  *true_proj  = new_d_Proj(dbgi, cond, mode_X, pn_Cond_true);
	ir_node  *false_proj = new_d_Proj(dbgi, cond, mode_X, pn_Cond_false);

	add_immBlock_pred(true_block, true_proj);
	add_immBlock_pred(false_block, false_proj);

	set_cur_block(NULL);
}


static void return_statement_to_firm(return_statement_t *statement)
{
	if(get_cur_block() == NULL)
		return;


	ir_type *func_irtype = get_ir_type(current_function_decl->type);

	ir_node *value = NULL;
	if(statement->return_value != NULL) {
		value = expression_to_firm(statement->return_value);
	}

	ir_node *in[1];
	int      in_len;
	if(get_method_n_ress(func_irtype) > 0) {
		if(value == NULL) {
			ir_type *res_type = get_method_res_type(func_irtype, 0);
			ir_mode *mode     = get_type_mode(res_type);
			value             = new_Unknown(mode);
		}

		in[0]  = value;
		in_len = 1;
	} else {
		in_len = 0;
	}

	dbg_info *dbgi  = get_dbg_info(&statement->statement.source_position);
	ir_node  *store = get_store();
	ir_node  *ret   = new_d_Return(dbgi, store, in_len, in);

	ir_node *end_block = get_irg_end_block(current_ir_graph);
	add_immBlock_pred(end_block, ret);

	set_cur_block(NULL);
}

static ir_node *expression_statement_to_firm(expression_statement_t *statement)
{
	if(get_cur_block() == NULL)
		return NULL;

	return expression_to_firm(statement->expression);
}

static ir_node *compound_statement_to_firm(compound_statement_t *compound)
{
	ir_node     *result    = NULL;
	statement_t *statement = compound->statements;
	for( ; statement != NULL; statement = statement->next) {
		//context2firm(&statement->context);

		if(statement->next == NULL && statement->type == STATEMENT_EXPRESSION) {
			result = expression_statement_to_firm(
					(expression_statement_t*) statement);
			break;
		}
		statement_to_firm(statement);
	}

	return result;
}

static void if_statement_to_firm(if_statement_t *statement)
{
	ir_node *cur_block = get_cur_block();

	ir_node *fallthrough_block = new_immBlock();

	/* the true (blocks) */
	ir_node *true_block;
	if (statement->true_statement != NULL) {
		true_block = new_immBlock();
		statement_to_firm(statement->true_statement);
		if(get_cur_block() != NULL) {
			ir_node *jmp = new_Jmp();
			add_immBlock_pred(fallthrough_block, jmp);
		}
	} else {
		true_block = fallthrough_block;
	}

	/* the false (blocks) */
	ir_node *false_block;
	if(statement->false_statement != NULL) {
		false_block = new_immBlock();

		statement_to_firm(statement->false_statement);
		if(get_cur_block() != NULL) {
			ir_node *jmp = new_Jmp();
			add_immBlock_pred(fallthrough_block, jmp);
		}
	} else {
		false_block = fallthrough_block;
	}

	/* create the condition */
	if(cur_block != NULL) {
		set_cur_block(cur_block);
		create_condition_evaluation(statement->condition, true_block,
		                            false_block);
	}

	mature_immBlock(true_block);
	if(false_block != fallthrough_block) {
		mature_immBlock(false_block);
	}
	mature_immBlock(fallthrough_block);

	set_cur_block(fallthrough_block);
}

static void while_statement_to_firm(while_statement_t *statement)
{
	ir_node *jmp = NULL;
	if(get_cur_block() != NULL) {
		jmp = new_Jmp();
	}

	/* create the header block */
	ir_node *header_block = new_immBlock();
	if(jmp != NULL) {
		add_immBlock_pred(header_block, jmp);
	}

	/* the false block */
	ir_node *false_block = new_immBlock();

	/* the loop body */
	ir_node *body_block;
	if (statement->body != NULL) {
		ir_node *old_continue_label = continue_label;
		ir_node *old_break_label    = break_label;
		continue_label              = header_block;
		break_label                 = false_block;

		body_block = new_immBlock();
		statement_to_firm(statement->body);

		assert(continue_label == header_block);
		assert(break_label    == false_block);
		continue_label = old_continue_label;
		break_label    = old_break_label;

		if(get_cur_block() != NULL) {
			ir_node *jmp = new_Jmp();
			add_immBlock_pred(header_block, jmp);
		}
	} else {
		body_block = header_block;
	}

	/* create the condition */
	set_cur_block(header_block);

	create_condition_evaluation(statement->condition, body_block, false_block);
	mature_immBlock(body_block);
	mature_immBlock(false_block);
	mature_immBlock(header_block);

	set_cur_block(false_block);
}

static void do_while_statement_to_firm(do_while_statement_t *statement)
{
	ir_node *jmp = NULL;
	if(get_cur_block() != NULL) {
		jmp = new_Jmp();
	}

	/* create the header block */
	ir_node *header_block = new_immBlock();

	/* the false block */
	ir_node *false_block = new_immBlock();

	/* the loop body */
	ir_node *body_block = new_immBlock();
	if(jmp != NULL) {
		add_immBlock_pred(body_block, jmp);
	}

	if (statement->body != NULL) {
		ir_node *old_continue_label = continue_label;
		ir_node *old_break_label    = break_label;
		continue_label              = header_block;
		break_label                 = false_block;

		statement_to_firm(statement->body);

		assert(continue_label == header_block);
		assert(break_label    == false_block);
		continue_label = old_continue_label;
		break_label    = old_break_label;

		if (get_cur_block() == NULL) {
			mature_immBlock(header_block);
			mature_immBlock(body_block);
			mature_immBlock(false_block);
			return;
		}
	}

	ir_node *body_jmp = new_Jmp();
	add_immBlock_pred(header_block, body_jmp);
	mature_immBlock(header_block);

	/* create the condition */
	set_cur_block(header_block);

	create_condition_evaluation(statement->condition, body_block, false_block);
	mature_immBlock(body_block);
	mature_immBlock(false_block);
	mature_immBlock(header_block);

	set_cur_block(false_block);
}

static void for_statement_to_firm(for_statement_t *statement)
{
	ir_node *jmp = NULL;
	if (get_cur_block() != NULL) {
		if(statement->initialisation != NULL) {
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
			ir_node *const jmp = new_Jmp();
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
		ir_node *jmp = new_Jmp();
		add_immBlock_pred(body_block, jmp);
	}

	mature_immBlock(body_block);
	mature_immBlock(false_block);
	mature_immBlock(step_block);
	mature_immBlock(header_block);
	mature_immBlock(false_block);

	set_cur_block(false_block);
}

static void create_declaration_entity(declaration_t *declaration,
                                      declaration_type_t declaration_type,
                                      ir_type *parent_type)
{
	ident     *id     = new_id_from_str(declaration->symbol->string);
	ir_type   *irtype = get_ir_type(declaration->type);
	ir_entity *entity = new_entity(parent_type, id, irtype);
	set_entity_ld_ident(entity, id);

	declaration->declaration_type = declaration_type;
	declaration->v.entity         = entity;
	set_entity_variability(entity, variability_uninitialized);
	/* TODO: visibility? */
}

typedef struct compound_graph_path_entry_t compound_graph_path_entry_t;

enum compound_graph_entry_type_t {
	COMPOUND_GRAPH_ENTRY_ARRAY,
	COMPOUND_GRAPH_ENTRY_COMPOUND
};

struct compound_graph_path_entry_t {
	int type;
	union {
		ir_entity *entity;
		int        array_index;
	} v;
	compound_graph_path_entry_t *prev;
};

static void create_initializer_object(initializer_t *initializer, type_t *type,
		ir_entity *entity, compound_graph_path_entry_t *entry, int len);

static compound_graph_path *create_compound_path(ir_type *type,
		compound_graph_path_entry_t *entry, int len)
{
	compound_graph_path *path = new_compound_graph_path(type, len);

	int i = len - 1;
	for( ; entry != NULL; entry = entry->prev, --i) {
		assert(i >= 0);
		if(entry->type == COMPOUND_GRAPH_ENTRY_COMPOUND) {
			set_compound_graph_path_node(path, i, entry->v.entity);
		} else {
			assert(entry->type == COMPOUND_GRAPH_ENTRY_ARRAY);
			set_compound_graph_path_array_index(path, i, entry->v.array_index);
		}
	}
	assert(i == -1);

	return path;
}

static void create_initializer_value(initializer_value_t *initializer,
                                     ir_entity *entity,
                                     compound_graph_path_entry_t *entry,
                                     int len)
{
	ir_node             *node = expression_to_firm(initializer->value);
	ir_type             *type = get_entity_type(entity);
	compound_graph_path *path = create_compound_path(type, entry, len);
	add_compound_ent_value_w_path(entity, node, path);
}

static void create_initializer_compound(initializer_list_t *initializer,
                                        compound_type_t *type,
                                        ir_entity *entity,
                                        compound_graph_path_entry_t *last_entry,
                                        int len)
{
	declaration_t *compound_declaration = type->declaration;

	declaration_t *compound_entry = compound_declaration->context.declarations;

	compound_graph_path_entry_t entry;
	entry.type = COMPOUND_GRAPH_ENTRY_COMPOUND;
	entry.prev = last_entry;
	++len;

	size_t i = 0;
	for( ; compound_entry != NULL; compound_entry = compound_entry->next) {
		if(compound_entry->symbol == NULL)
			continue;
		if(compound_entry->namespc != NAMESPACE_NORMAL)
			continue;

		if(i >= initializer->len)
			break;

		entry.v.entity = compound_entry->v.entity;

		initializer_t *sub_initializer = initializer->initializers[i];

		assert(compound_entry != NULL);
		assert(compound_entry->declaration_type
				== DECLARATION_TYPE_COMPOUND_MEMBER);

		if(sub_initializer->type == INITIALIZER_VALUE) {
			create_initializer_value(&sub_initializer->value,
			                         entity, &entry, len);
		} else {
			type_t *type = skip_typeref(compound_entry->type);
			create_initializer_object(sub_initializer, type, entity, &entry,
			                          len);
		}

		++i;
	}
}

static void create_initializer_array(initializer_list_t *initializer,
                                     array_type_t *type, ir_entity *entity,
                                     compound_graph_path_entry_t *last_entry,
                                     int len)
{
	type_t *element_type = type->element_type;
	element_type         = skip_typeref(element_type);

	compound_graph_path_entry_t entry;
	entry.type = COMPOUND_GRAPH_ENTRY_ARRAY;
	entry.prev = last_entry;
	++len;

	size_t i;
	for(i = 0; i < initializer->len; ++i) {
		entry.v.array_index = i;

		initializer_t *sub_initializer = initializer->initializers[i];

		if(sub_initializer->type == INITIALIZER_VALUE) {
			create_initializer_value(&sub_initializer->value,
			                         entity, &entry, len);
		} else {
			create_initializer_object(sub_initializer, element_type, entity,
			                          &entry, len);
		}
	}

#if 0
	/* TODO: initializer rest... */
	if(type->size_expression != NULL) {
		size_t array_len = fold_constant(type->size_expression);
		for( ; i < array_len; ++i) {

		}
	}
#endif
}

static void create_initializer_string(initializer_string_t *initializer,
                                      array_type_t *type, ir_entity *entity,
                                      compound_graph_path_entry_t *last_entry,
                                      int len)
{
	type_t *element_type = type->element_type;
	element_type         = skip_typeref(element_type);

	compound_graph_path_entry_t entry;
	entry.type = COMPOUND_GRAPH_ENTRY_ARRAY;
	entry.prev = last_entry;
	++len;

	ir_type    *irtype  = get_entity_type(entity);
	size_t      arr_len = get_array_type_size(type);
	const char *p       = initializer->string;
	size_t      i       = 0;
	for(i = 0; i < arr_len; ++i, ++p) {
		entry.v.array_index = i;

		ir_node             *node = new_Const_long(mode_Bs, *p);
		compound_graph_path *path = create_compound_path(irtype, &entry, len);
		add_compound_ent_value_w_path(entity, node, path);

		if(*p == '\0')
			break;
	}
}

static void create_initializer_object(initializer_t *initializer, type_t *type,
		ir_entity *entity, compound_graph_path_entry_t *entry, int len)
{
	if(type->type == TYPE_ARRAY) {
		array_type_t *array_type = (array_type_t*) type;

		if(initializer->type == INITIALIZER_STRING) {
			initializer_string_t *string = &initializer->string;
			create_initializer_string(string, array_type, entity, entry, len);
		} else {
			assert(initializer->type == INITIALIZER_LIST);
			initializer_list_t *list = &initializer->list;
			create_initializer_array(list, array_type, entity, entry, len);
		}
	} else {
		assert(initializer->type == INITIALIZER_LIST);
		initializer_list_t *list = &initializer->list;

		assert(type->type == TYPE_COMPOUND_STRUCT
				|| type->type == TYPE_COMPOUND_UNION);
		compound_type_t *compound_type = (compound_type_t*) type;
		create_initializer_compound(list, compound_type, entity, entry, len);
	}
}

static void create_initializer_local_variable_entity(declaration_t *declaration)
{
	initializer_t *initializer = declaration->init.initializer;
	dbg_info      *dbgi        = get_dbg_info(&declaration->source_position);
	ir_entity     *entity      = declaration->v.entity;
	ir_node       *memory      = get_store();
	ir_node       *nomem       = new_NoMem();
	ir_node       *frame       = get_irg_frame(current_ir_graph);
	ir_node       *addr        = new_d_simpleSel(dbgi, nomem, frame, entity);

	if(is_atomic_entity(entity)) {
		assert(initializer->type == INITIALIZER_VALUE);
		initializer_value_t *initializer_value = &initializer->value;

		ir_node *value     = expression_to_firm(initializer_value->value);
		ir_node *store     = new_d_Store(dbgi, memory, addr, value);
		ir_node *store_mem = new_d_Proj(dbgi, store, mode_M, pn_Store_M);
		set_store(store_mem);
		return;
	}

	/* create a "template" entity which is copied to the entity on the stack */
	ident     *id          = unique_ident("initializer");
	ir_type   *irtype      = get_ir_type(declaration->type);
	ir_type   *global_type = get_glob_type();
	ir_entity *init_entity = new_entity(global_type, id, irtype);
	set_entity_ld_ident(init_entity, id);

	set_entity_variability(init_entity, variability_initialized);
	set_entity_visibility(init_entity, visibility_local);

	ir_graph *old_current_ir_graph = current_ir_graph;
	current_ir_graph = get_const_code_irg();

	type_t *type = skip_typeref(declaration->type);
	create_initializer_object(initializer, type, init_entity, NULL, 0);

	assert(current_ir_graph == get_const_code_irg());
	current_ir_graph = old_current_ir_graph;

	ir_node *src_addr  = create_symconst(dbgi, init_entity);
	ir_node *copyb     = new_d_CopyB(dbgi, memory, addr, src_addr, irtype);

	ir_node *copyb_mem = new_Proj(copyb, mode_M, pn_CopyB_M_regular);
	set_store(copyb_mem);
}

static void create_initializer(declaration_t *declaration)
{
	initializer_t *initializer = declaration->init.initializer;
	if(initializer == NULL)
		return;

	declaration_type_t declaration_type = (declaration_type_t)declaration->declaration_type;
	if(declaration_type == DECLARATION_TYPE_LOCAL_VARIABLE_ENTITY) {
		create_initializer_local_variable_entity(declaration);
		return;
	}

	if(initializer->type == INITIALIZER_VALUE) {
		initializer_value_t *initializer_value = &initializer->value;

		ir_node *value = expression_to_firm(initializer_value->value);

		if(declaration_type == DECLARATION_TYPE_LOCAL_VARIABLE) {
			set_value(declaration->v.value_number, value);
		} else {
			assert(declaration_type == DECLARATION_TYPE_GLOBAL_VARIABLE);

			ir_entity *entity = declaration->v.entity;

			set_entity_variability(entity, variability_initialized);
			set_atomic_ent_value(entity, value);
		}
	} else {
		declaration_type_t declaration_type = (declaration_type_t)declaration->declaration_type;
		assert(declaration_type == DECLARATION_TYPE_LOCAL_VARIABLE_ENTITY
				|| declaration_type == DECLARATION_TYPE_GLOBAL_VARIABLE);

		ir_entity *entity = declaration->v.entity;
		set_entity_variability(entity, variability_initialized);

		type_t *type = skip_typeref(declaration->type);
		create_initializer_object(initializer, type, entity, NULL, 0);
	}
}

static void create_local_variable(declaration_t *declaration)
{
	assert(declaration->declaration_type == DECLARATION_TYPE_UNKNOWN);

	bool needs_entity = declaration->address_taken;
	type_t *type = skip_typeref(declaration->type);

	if(type->type == TYPE_ARRAY
			|| type->type == TYPE_COMPOUND_STRUCT
			|| type->type == TYPE_COMPOUND_UNION) {
		needs_entity = true;
	}

	if(needs_entity) {
		ir_type *frame_type = get_irg_frame_type(current_ir_graph);
		create_declaration_entity(declaration,
		                          DECLARATION_TYPE_LOCAL_VARIABLE_ENTITY,
		                          frame_type);
	} else {
		declaration->declaration_type = DECLARATION_TYPE_LOCAL_VARIABLE;
		declaration->v.value_number   = next_value_number_function;
		++next_value_number_function;
	}

	create_initializer(declaration);
}

static void create_local_static_variable(declaration_t *declaration)
{
	assert(declaration->declaration_type == DECLARATION_TYPE_UNKNOWN);

	type_t    *type        = skip_typeref(declaration->type);
	ir_type   *global_type = get_glob_type();
	ident     *id          = unique_ident(declaration->symbol->string);
	ir_type   *irtype      = get_ir_type(type);
	ir_entity *entity      = new_entity(global_type, id, irtype);
	set_entity_ld_ident(entity, id);

	declaration->declaration_type = DECLARATION_TYPE_GLOBAL_VARIABLE;
	declaration->v.entity         = entity;
	set_entity_variability(entity, variability_uninitialized);
	set_entity_visibility(entity, visibility_local);

	ir_graph *old_current_ir_graph = current_ir_graph;
	current_ir_graph = get_const_code_irg();

	create_initializer(declaration);

	assert(current_ir_graph == get_const_code_irg());
	current_ir_graph = old_current_ir_graph;
}

static void declaration_statement_to_firm(declaration_statement_t *statement)
{
	declaration_t *declaration = statement->declarations_begin;
	declaration_t *end         = statement->declarations_end->next;
	for( ; declaration != end; declaration = declaration->next) {
		type_t *type = declaration->type;

		switch(declaration->storage_class) {
		case STORAGE_CLASS_TYPEDEF:
			continue;
		case STORAGE_CLASS_STATIC:
			create_local_static_variable(declaration);
			continue;
		case STORAGE_CLASS_ENUM_ENTRY:
			panic("enum entry declaration in local block found");
		case STORAGE_CLASS_EXTERN:
			panic("extern declaration in local block found");
		case STORAGE_CLASS_NONE:
		case STORAGE_CLASS_AUTO:
		case STORAGE_CLASS_REGISTER:
			if(type->type == TYPE_FUNCTION) {
				panic("nested functions not supported yet");
			} else {
				create_local_variable(declaration);
			}
			continue;
		}
		panic("invalid storage class found");
	}
}

static void create_jump_statement(const statement_t *statement,
                                  ir_node *target_block)
{
	if(get_cur_block() == NULL)
		return;

	dbg_info *dbgi = get_dbg_info(&statement->source_position);
	ir_node  *jump = new_d_Jmp(dbgi);
	add_immBlock_pred(target_block, jump);

	set_cur_block(NULL);
}

static void switch_statement_to_firm(const switch_statement_t *statement)
{
	dbg_info *dbgi = get_dbg_info(&statement->statement.source_position);

	ir_node *expression  = expression_to_firm(statement->expression);
	ir_node *cond        = new_d_Cond(dbgi, expression);
	ir_node *break_block = new_immBlock();

	set_cur_block(NULL);

	ir_node *const old_switch_cond       = current_switch_cond;
	ir_node *const old_break_label       = break_label;
	const bool     old_saw_default_label = saw_default_label;
	current_switch_cond                  = cond;
	break_label                          = break_block;

	statement_to_firm(statement->body);

	if(get_cur_block() != NULL) {
		ir_node *jmp = new_Jmp();
		add_immBlock_pred(break_block, jmp);
	}

	if (!saw_default_label) {
		set_cur_block(get_nodes_block(cond));
		ir_node *const proj = new_d_defaultProj(dbgi, cond,
		                                        MAGIC_DEFAULT_PN_NUMBER);
		add_immBlock_pred(break_block, proj);
	}

	assert(current_switch_cond == cond);
	assert(break_label         == break_block);
	current_switch_cond = old_switch_cond;
	break_label         = old_break_label;
	saw_default_label   = old_saw_default_label;

	mature_immBlock(break_block);
	set_cur_block(break_block);
}

static long fold_constant(const expression_t *expression)
{
	ir_graph *old_current_ir_graph = current_ir_graph;
	current_ir_graph = get_const_code_irg();

	ir_node *cnst = expression_to_firm(expression);
	if(!is_Const(cnst)) {
		panic("couldn't fold constantl");
	}
	tarval *tv = get_Const_tarval(cnst);
	if(!tarval_is_long(tv)) {
		panic("folded constant not an integer");
	}

	long res = get_tarval_long(tv);

	current_ir_graph = old_current_ir_graph;
	return res;
}

static void case_label_to_firm(const case_label_statement_t *statement)
{
	dbg_info *dbgi = get_dbg_info(&statement->statement.source_position);

	ir_node *const fallthrough = (get_cur_block() == NULL ? NULL : new_Jmp());

	/* let's create a node and hope firm constant folding creates a Const
	 * node... */
	ir_node *proj;
	set_cur_block(get_nodes_block(current_switch_cond));
	if(statement->expression) {
		long pn = fold_constant(statement->expression);
		if(pn == MAGIC_DEFAULT_PN_NUMBER) {
			/* oops someone detected our cheating... */
			panic("magic default pn used");
		}
		proj = new_d_Proj(dbgi, current_switch_cond, mode_X, pn);
	} else {
		saw_default_label = true;
		proj = new_d_defaultProj(dbgi, current_switch_cond,
		                         MAGIC_DEFAULT_PN_NUMBER);
	}

	ir_node *block = new_immBlock();
	if (fallthrough != NULL) {
		add_immBlock_pred(block, fallthrough);
	}
	add_immBlock_pred(block, proj);
	mature_immBlock(block);

	statement_to_firm(statement->label_statement);
}

static ir_node *get_label_block(declaration_t *label)
{
	assert(label->namespc == NAMESPACE_LABEL);

	if(label->declaration_type == DECLARATION_TYPE_LABEL_BLOCK) {
		return label->v.block;
	}
	assert(label->declaration_type == DECLARATION_TYPE_UNKNOWN);

	ir_node *old_cur_block = get_cur_block();
	ir_node *block         = new_immBlock();
	set_cur_block(old_cur_block);

	label->declaration_type = DECLARATION_TYPE_LABEL_BLOCK;
	label->v.block          = block;

	ARR_APP1(ir_node *, imature_blocks, block);

	return block;
}

static void label_to_firm(const label_statement_t *statement)
{
	ir_node *block = get_label_block(statement->label);

	if(get_cur_block() != NULL) {
		ir_node *jmp = new_Jmp();
		add_immBlock_pred(block, jmp);
	}

	set_cur_block(block);
	keep_alive(block);

	statement_to_firm(statement->label_statement);
}

static void goto_to_firm(const goto_statement_t *statement)
{
	if(get_cur_block() == NULL)
		return;

	ir_node *block = get_label_block(statement->label);
	ir_node *jmp   = new_Jmp();
	add_immBlock_pred(block, jmp);

	set_cur_block(NULL);
}

static void statement_to_firm(statement_t *statement)
{
	switch(statement->type) {
	case STATEMENT_COMPOUND:
		compound_statement_to_firm((compound_statement_t*) statement);
		return;
	case STATEMENT_RETURN:
		return_statement_to_firm((return_statement_t*) statement);
		return;
	case STATEMENT_EXPRESSION:
		expression_statement_to_firm((expression_statement_t*) statement);
		return;
	case STATEMENT_IF:
		if_statement_to_firm((if_statement_t*) statement);
		return;
	case STATEMENT_WHILE:
		while_statement_to_firm((while_statement_t*) statement);
		return;
	case STATEMENT_DO_WHILE:
		do_while_statement_to_firm((do_while_statement_t*) statement);
		return;
	case STATEMENT_DECLARATION:
		declaration_statement_to_firm((declaration_statement_t*) statement);
		return;
	case STATEMENT_BREAK:
		create_jump_statement(statement, break_label);
		return;
	case STATEMENT_CONTINUE:
		create_jump_statement(statement, continue_label);
		return;
	case STATEMENT_SWITCH:
		switch_statement_to_firm((switch_statement_t*) statement);
		return;
	case STATEMENT_CASE_LABEL:
		case_label_to_firm((case_label_statement_t*) statement);
		return;
	case STATEMENT_FOR:
		for_statement_to_firm((for_statement_t*) statement);
		return;
	case STATEMENT_LABEL:
		label_to_firm((label_statement_t*) statement);
		return;
	case STATEMENT_GOTO:
		goto_to_firm((goto_statement_t*) statement);
		return;
	default:
		break;
	}
	panic("Statement not implemented\n");
}

static int count_local_declarations(const declaration_t *      decl,
                                    const declaration_t *const end)
{
	int count = 0;
	for (; decl != end; decl = decl->next) {
		const type_t *type = skip_typeref(decl->type);
		switch (type->type) {
			case TYPE_ATOMIC:
			case TYPE_ENUM:
			case TYPE_POINTER:
				if (!decl->address_taken) ++count;
				break;

			default: break;
		}
	}
	return count;
}

static int count_decls_in_stmts(const statement_t *stmt)
{
	int count = 0;
	for (; stmt != NULL; stmt = stmt->next) {
		switch (stmt->type) {
			case STATEMENT_DECLARATION: {
				const declaration_statement_t *const decl_stmt =
					(const declaration_statement_t*)stmt;
				count += count_local_declarations(decl_stmt->declarations_begin,
				                                  decl_stmt->declarations_end->next);
				break;
			}

			case STATEMENT_COMPOUND: {
				const compound_statement_t *const comp =
					(const compound_statement_t*)stmt;
				count += count_decls_in_stmts(comp->statements);
				break;
			}

			case STATEMENT_IF: {
				const if_statement_t *const if_stmt = (const if_statement_t*)stmt;
				count += count_decls_in_stmts(if_stmt->true_statement);
				count += count_decls_in_stmts(if_stmt->false_statement);
				break;
			}

			case STATEMENT_SWITCH: {
				const switch_statement_t *const switch_stmt =
					(const switch_statement_t*)stmt;
				count += count_decls_in_stmts(switch_stmt->body);
				break;
			}

			case STATEMENT_LABEL: {
				const label_statement_t *const label_stmt =
					(const label_statement_t*)stmt;
				count += count_decls_in_stmts(label_stmt->label_statement);
				break;
			}

			case STATEMENT_WHILE: {
				const while_statement_t *const while_stmt =
					(const while_statement_t*)stmt;
				count += count_decls_in_stmts(while_stmt->body);
				break;
			}

			case STATEMENT_DO_WHILE: {
				const do_while_statement_t *const do_while_stmt =
					(const do_while_statement_t*)stmt;
				count += count_decls_in_stmts(do_while_stmt->body);
				break;
			}

			case STATEMENT_FOR: {
				const for_statement_t *const for_stmt =
					(const for_statement_t*)stmt;
				/* TODO initialisation */
				count += count_decls_in_stmts(for_stmt->body);
				break;
			}

			case STATEMENT_BREAK:
			case STATEMENT_CASE_LABEL:
			case STATEMENT_CONTINUE:
			case STATEMENT_EXPRESSION:
			case STATEMENT_GOTO:
			case STATEMENT_INVALID:
			case STATEMENT_RETURN:
				break;
		}
	}
	return count;
}

static int get_function_n_local_vars(declaration_t *declaration)
{
	int count = 0;

	/* count parameters */
	count += count_local_declarations(declaration->context.declarations, NULL);

	/* count local variables declared in body */
	count += count_decls_in_stmts(declaration->init.statement);

	/* TODO FIXME: Matze: I'm lazy don't want to scan all expressions
	 * for expression statements... */
	count += 10;

	return count;
}

static void initialize_function_parameters(declaration_t *declaration)
{
	ir_graph        *irg             = current_ir_graph;
	ir_node         *args            = get_irg_args(irg);
	ir_node         *start_block     = get_irg_start_block(irg);
	ir_type         *function_irtype = get_ir_type(declaration->type);

	int            n         = 0;
	declaration_t *parameter = declaration->context.declarations;
	for( ; parameter != NULL; parameter = parameter->next, ++n) {
		assert(parameter->declaration_type == DECLARATION_TYPE_UNKNOWN);
		type_t *type = skip_typeref(parameter->type);

		bool needs_entity = parameter->address_taken;
		if(type->type == TYPE_COMPOUND_STRUCT
				|| type->type == TYPE_COMPOUND_UNION) {
			needs_entity = true;
		}

		if(needs_entity) {
			ir_entity *entity = get_method_value_param_ent(function_irtype, n);
			ident     *id     = new_id_from_str(parameter->symbol->string);
			set_entity_ident(entity, id);

			parameter->declaration_type
				= DECLARATION_TYPE_LOCAL_VARIABLE_ENTITY;
			parameter->v.entity = entity;
			continue;
		}

		ir_mode *mode = get_ir_mode(parameter->type);
		long     pn   = n;
		ir_node *proj = new_r_Proj(irg, start_block, args, mode, pn);

		parameter->declaration_type = DECLARATION_TYPE_LOCAL_VARIABLE;
		parameter->v.value_number   = next_value_number_function;
		++next_value_number_function;

		set_value(parameter->v.value_number, proj);
	}
}

static void create_function(declaration_t *declaration)
{
	ir_entity *entity = get_function_entity(declaration);

	if(declaration->init.statement == NULL)
		return;

	current_function_decl = declaration;
	current_function_name = NULL;

	assert(imature_blocks == NULL);
	imature_blocks = NEW_ARR_F(ir_node*, 0);

	int       n_local_vars = get_function_n_local_vars(declaration);
	ir_graph *irg          = new_ir_graph(entity, n_local_vars);
	ir_node  *first_block  = get_cur_block();

	next_value_number_function = 0;
	initialize_function_parameters(declaration);

	statement_to_firm(declaration->init.statement);

	ir_node *end_block = get_irg_end_block(irg);

	/* do we have a return statement yet? */
	if(get_cur_block() != NULL) {
		assert(declaration->type->type == TYPE_FUNCTION);
		const function_type_t* const func_type
			= (const function_type_t*) declaration->type;
		ir_node *ret;
		if (func_type->result_type == type_void) {
			ret = new_Return(get_store(), 0, NULL);
		} else {
			ir_mode *const mode = get_ir_mode(func_type->result_type);
			ir_node *      in[1];
			// §5.1.2.2.3 main implicitly returns 0
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
		if(align > align_all)
			align_all = align;
		int misalign = 0;
		if(align > 0) {
			misalign  = offset % align;
			if(misalign > 0) {
				offset += align - misalign;
			}
		}

		set_entity_offset(entity, offset);
		offset += get_type_size_bytes(entity_type);
	}
	set_type_size_bytes(frame_type, offset);
	set_type_alignment_bytes(frame_type, align_all);
	set_type_state(frame_type, layout_fixed);

	irg_vrfy(irg);
}

static void create_global_variable(declaration_t *declaration)
{
	ir_type   *global_type = get_glob_type();
	create_declaration_entity(declaration, DECLARATION_TYPE_GLOBAL_VARIABLE,
	                          global_type);

	ir_entity *entity = declaration->v.entity;
	if(declaration->storage_class == STORAGE_CLASS_STATIC) {
		set_entity_visibility(entity, visibility_local);
	} else if(declaration->storage_class == STORAGE_CLASS_EXTERN) {
		set_entity_visibility(entity, visibility_external_allocated);
	} else {
		set_entity_visibility(entity, visibility_external_visible);
	}
	current_ir_graph = get_const_code_irg();
	create_initializer(declaration);
}

static void context_to_firm(context_t *context)
{
	/* first pass: create declarations */
	declaration_t *declaration = context->declarations;
	for( ; declaration != NULL; declaration = declaration->next) {
		if(declaration->namespc != NAMESPACE_NORMAL)
			continue;
		if(declaration->storage_class == STORAGE_CLASS_ENUM_ENTRY
				|| declaration->storage_class == STORAGE_CLASS_TYPEDEF)
			continue;
		if(declaration->symbol == NULL)
			continue;

		type_t *type = declaration->type;
		if(type->type == TYPE_FUNCTION) {
			get_function_entity(declaration);
		} else {
			create_global_variable(declaration);
		}
	}

	/* second pass: create code */
	declaration = context->declarations;
	for( ; declaration != NULL; declaration = declaration->next) {
		if(declaration->namespc != NAMESPACE_NORMAL)
			continue;
		if(declaration->storage_class == STORAGE_CLASS_ENUM_ENTRY
				|| declaration->storage_class == STORAGE_CLASS_TYPEDEF)
			continue;
		if(declaration->symbol == NULL)
			continue;

		type_t *type = declaration->type;
		if(type->type != TYPE_FUNCTION)
			continue;

		create_function(declaration);
	}
}

void translation_unit_to_firm(translation_unit_t *unit)
{
	/* just to be sure */
	continue_label      = NULL;
	break_label         = NULL;
	current_switch_cond = NULL;

	context_to_firm(& unit->context);
}
