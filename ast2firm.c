#include <config.h>

#define _GNU_SOURCE

#include <assert.h>
#include <string.h>

#include <libfirm/firm.h>
#include <libfirm/adt/obst.h>

#include "adt/error.h"
#include "token_t.h"
#include "type_t.h"
#include "ast_t.h"

static ir_type *ir_type_const_char;
static ir_type *ir_type_void;
static ir_type *ir_type_int;
static ir_type *ir_type_void_ptr;

static type_t *type_const_char;
static type_t *type_void;
static type_t *type_int;

typedef struct type2firm_env_t type2firm_env_t;
struct type2firm_env_t {
	int can_cache;       /* nonzero if type can safely be cached because
	                        no typevariables are in the hierarchy */
};

static ir_type *_get_ir_type(type2firm_env_t *env, type_t *type);
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
	type_void       = make_atomic_type(ATOMIC_TYPE_VOID, 0);
	type_int        = make_atomic_type(ATOMIC_TYPE_INT, 0);

	ir_type_int        = get_ir_type(type_int);
	ir_type_const_char = get_ir_type(type_const_char);
	ir_type_void       = get_ir_type(type_int); /* we don't have a real void
	                                               type in firm */
	ir_type_void_ptr   = new_type_pointer(new_id_from_str("void_ptr"),
	                                      ir_type_void, mode_P_data);
}

void exit_ast2firm(void)
{
}

static unsigned unique_id = 0;

static ident *unique_ident(const char *tag)
{
	char buf[256];

	snprintf(buf, sizeof(buf), "%s.%d", tag, unique_id);
	unique_id++;
	return new_id_from_str(buf);
}

#if 0
static symbol_t *unique_symbol(const char *tag)
{
	obstack_printf(&symbol_obstack, "%s.%d", tag, unique_id);
	unique_id++;

	const char *string = obstack_finish(&symbol_obstack);
	symbol_t   *symbol = symbol_table_insert(string);

	assert(symbol->string == string);

	return symbol;
}
#endif

static type_t *skip_typeref(type_t *type)
{
	while(1) {
		switch(type->type) {
		case TYPE_TYPEDEF: {
			const typedef_type_t *typedef_type = (const typedef_type_t*) type;
			type = typedef_type->declaration->type;
			continue;
		}
		case TYPE_TYPEOF: {
			const typeof_type_t *typeof_type = (const typeof_type_t *) type;
			if(typeof_type->typeof_type != NULL) {
				type = typeof_type->typeof_type;
			} else {
				type = typeof_type->expression->datatype;
			}
			continue;
		}
		default:
			break;
		}
		break;
	}

	return type;
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
		panic("tried to get mode from void type");
		break;
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
	ir_type *irtype = get_ir_type(&type->type);
	return get_type_size_bytes(irtype);
}

static unsigned get_array_type_size(array_type_t *type)
{
	ir_type *irtype = get_ir_type(&type->type);
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




static ir_type *get_atomic_type(type2firm_env_t *env, const atomic_type_t *type)
{
	(void) env;
	ir_mode *mode   = get_atomic_mode(type);
	ident   *id     = get_mode_ident(mode);
	ir_type *irtype = new_type_primitive(id, mode);

	return irtype;
}

static ir_type *get_method_type(type2firm_env_t *env,
                                const function_type_t *function_type)
{
	type_t  *result_type  = function_type->result_type;

	ident   *id           = unique_ident("functiontype");
	int      n_parameters = count_parameters(function_type);
	int      n_results    = result_type == type_void ? 0 : 1;
	ir_type *irtype       = new_type_method(id, n_parameters, n_results);

	if(result_type != type_void) {
		ir_type *restype = _get_ir_type(env, result_type);
		set_method_res_type(irtype, 0, restype);
	}

	function_parameter_t *parameter = function_type->parameters;
	int                   n         = 0;
	for( ; parameter != NULL; parameter = parameter->next) {
		ir_type *p_irtype = _get_ir_type(env, parameter->type);
		set_method_param_type(irtype, n, p_irtype);
		++n;
	}

	if(function_type->variadic) {
		set_method_variadicity(irtype, variadicity_variadic);
	}

	return irtype;
}

static ir_type *get_pointer_type(type2firm_env_t *env, pointer_type_t *type)
{
	type_t  *points_to = type->points_to;
	ir_type *ir_points_to;
	/* Avoid endless recursion if the points_to type contains this poiner type
	 * again (might be a struct). We therefore first create a void* pointer
	 * and then set the real points_to type
	 */
	ir_type *ir_type_void = get_ir_type(type_void);
	ir_type *ir_type      = new_type_pointer(unique_ident("pointer"),
                                             ir_type_void, mode_P_data);
	type->type.firm_type  = ir_type;

	ir_points_to = _get_ir_type(env, points_to);
	set_pointer_points_to_type(ir_type, ir_points_to);

	return ir_type;
}

static ir_type *get_array_type(type2firm_env_t *env, array_type_t *type)
{
	type_t  *element_type    = type->element_type;
	ir_type *ir_element_type = _get_ir_type(env, element_type);

	/* TODO... */
	int n_elements = 0;
	panic("TODO arraytpye size not implemented yet");

	ir_type *ir_type = new_type_array(unique_ident("array"), 1, ir_element_type);
	set_array_bounds_int(ir_type, 0, 0, n_elements);

	size_t elemsize = get_type_size_bytes(ir_element_type);
	int align = get_type_alignment_bytes(ir_element_type);
	if(elemsize % align > 0) {
		elemsize += align - (elemsize % align);
	}
	set_type_size_bytes(ir_type, n_elements * elemsize);
	set_type_alignment_bytes(ir_type, align);
	set_type_state(ir_type, layout_fixed);

	return ir_type;
}

#define INVALID_TYPE ((ir_type_ptr)-1)

static ir_type *get_struct_type(type2firm_env_t *env, compound_type_t *type)
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
		ident       *ident         = new_id_from_str(entry->symbol->string);
		ir_type_ptr  entry_ir_type = _get_ir_type(env, entry->type);

		int entry_size      = get_type_size_bytes(entry_ir_type);
		int entry_alignment = get_type_alignment_bytes(entry_ir_type);
		int misalign = offset % entry_alignment;
		offset += misalign;

		ir_entity *entity = new_entity(ir_type, ident, entry_ir_type);
		set_entity_offset(entity, offset);
		add_struct_member(ir_type, entity);
		entry->entity = entity;

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

static ir_type *get_union_type(type2firm_env_t *env, compound_type_t *type)
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
		ident       *ident         = new_id_from_str(entry->symbol->string);
		ir_type_ptr  entry_ir_type = _get_ir_type(env, entry->type);

		int entry_size      = get_type_size_bytes(entry_ir_type);
		int entry_alignment = get_type_alignment_bytes(entry_ir_type);

		ir_entity *entity = new_entity(ir_type, ident, entry_ir_type);
		add_union_member(ir_type, entity);
		set_entity_offset(entity, 0);
		entry->entity = entity;

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

static ir_type *_get_ir_type(type2firm_env_t *env, type_t *type)
{
	assert(type != NULL);

	type = skip_typeref(type);

	if(type->firm_type != NULL) {
		assert(type->firm_type != INVALID_TYPE);
		return type->firm_type;
	}

	ir_type *firm_type = NULL;
	switch(type->type) {
	case TYPE_ATOMIC:
		firm_type = get_atomic_type(env, (atomic_type_t*) type);
		break;
	case TYPE_FUNCTION:
		firm_type = get_method_type(env, (function_type_t*) type);
		break;
	case TYPE_POINTER:
		firm_type = get_pointer_type(env, (pointer_type_t*) type);
		break;
	case TYPE_ARRAY:
		firm_type = get_array_type(env, (array_type_t*) type);
		break;
	case TYPE_COMPOUND_STRUCT:
		firm_type = get_struct_type(env, (compound_type_t*) type);
		break;
	case TYPE_COMPOUND_UNION:
		firm_type = get_union_type(env, (compound_type_t*) type);
		break;
	case TYPE_ENUM:
		firm_type = ir_type_int;
		break;
	case TYPE_BUILTIN:
	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
	case TYPE_INVALID:
		break;
	}
	if(firm_type == NULL)
		panic("unknown type found");

	if(env->can_cache) {
		type->firm_type = firm_type;
	}
	return firm_type;

}

static ir_type *get_ir_type(type_t *type)
{
	type2firm_env_t env;
	env.can_cache = 1;

	return _get_ir_type(&env, type);
}

static inline ir_mode *get_ir_mode(type_t *type)
{
	ir_type *irtype = get_ir_type(type);
	ir_mode *mode   = get_type_mode(irtype);
	assert(mode != NULL);
	return mode;
}

static ir_entity* get_entity_function(declaration_t *declaration)
{
	if(declaration->entity != NULL)
		return declaration->entity;

	symbol_t *symbol = declaration->symbol;
	ident    *id     = new_id_from_str(symbol->string);

	ir_type  *global_type    = get_glob_type();
	ir_type  *ir_type_method = get_ir_type(declaration->type);
	assert(is_Method_type(ir_type_method));

	type_t    *type   = declaration->type;
	ir_entity *entity = new_entity(global_type, id, ir_type_method);
	set_entity_ld_ident(entity, id);
	if(declaration->storage_class & STORAGE_CLASS_STATIC
			|| type->qualifiers & TYPE_QUALIFIER_INLINE) {
		set_entity_visibility(entity, visibility_local);
	} else if(declaration->init.statement != NULL) {
		set_entity_visibility(entity, visibility_external_visible);
	} else {
		set_entity_visibility(entity, visibility_external_allocated);
	}

	declaration->entity = entity;
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

	tarval   *tv;
	if(mode_is_float(mode)) {
		tv = new_tarval_from_double(cnst->v.float_value, mode);
	} else {
		tv = new_tarval_from_long(cnst->v.int_value, mode);
	}

	return new_d_Const(dbgi, mode, tv);
}

static ir_node *string_literal_to_firm(const string_literal_t* literal)
{
	ir_type   *global_type = get_glob_type();
	ir_type   *type        = new_type_array(unique_ident("strtype"), 1,
	                                        ir_type_const_char);

	ir_entity *ent = new_entity(global_type, unique_ident("Lstr"), type);
	set_entity_variability(ent, variability_constant);

	ir_type    *elem_type = ir_type_const_char;
	ir_mode    *mode      = get_type_mode(elem_type);

	const char *string = literal->value;
	size_t      slen   = strlen(string) + 1;

	set_array_lower_bound_int(type, 0, 0);
	set_array_upper_bound_int(type, 0, slen);
	set_type_size_bytes(type, slen);
	set_type_state(type, layout_fixed);

	tarval **tvs = xmalloc(slen * sizeof(tvs[0]));
	for(size_t i = 0; i < slen; ++i) {
		tvs[i] = new_tarval_from_long(string[i], mode);
	}

	set_array_entity_values(ent, tvs, slen);
	free(tvs);

	dbg_info *dbgi = get_dbg_info(&literal->expression.source_position);

	union symconst_symbol sym;
	sym.entity_p = ent;
	return new_d_SymConst(dbgi, sym, symconst_addr_ent);
}

#if 0
static ir_node *call_expression_to_firm(const call_expression_t *call)
{
	expression_t  *function = call->function;
	ir_node       *callee   = expression_to_firm(function);

	assert(function->datatype->type == TYPE_FUNCTION);
	pointer_type_t *pointer_type = (pointer_type_t*) function->datatype;
	type_t         *points_to    = pointer_type->points_to;

	assert(points_to->type == TYPE_FUNCTION);
	method_type_t *method_type     = (method_type_t*) points_to;
	ir_type       *ir_method_type  = get_ir_type((type_t*) method_type);
	ir_type       *new_method_type = NULL;

	int              n_parameters = 0;
	call_argument_t *argument     = call->arguments;
	while(argument != NULL) {
		n_parameters++;
		argument = argument->next;
	}

	if(method_type->variable_arguments) {
		/* we need to construct a new method type matching the call
		 * arguments... */
		new_method_type = new_type_method(unique_ident("calltype"),
		                                  n_parameters,
		                                  get_method_n_ress(ir_method_type));
		set_method_calling_convention(new_method_type,
		               get_method_calling_convention(ir_method_type));
		set_method_additional_properties(new_method_type,
		               get_method_additional_properties(ir_method_type));

		for(int i = 0; i < get_method_n_ress(ir_method_type); ++i) {
			set_method_res_type(new_method_type, i,
			                    get_method_res_type(ir_method_type, i));
		}
	}
	ir_node *in[n_parameters];

	argument = call->arguments;
	int n = 0;
	while(argument != NULL) {
		expression_t *expression = argument->expression;

		ir_node *arg_node = expression_to_firm(expression);

		in[n] = arg_node;
		if(new_method_type != NULL) {
			ir_type *irtype = get_ir_type(expression->datatype);
			set_method_param_type(new_method_type, n, irtype);
		}

		argument = argument->next;
		n++;
	}

	if(new_method_type != NULL)
		ir_method_type = new_method_type;

	dbg_info *dbgi  = get_dbg_info(&call->expression.source_position);
	ir_node  *store = get_store();
	ir_node  *node  = new_d_Call(dbgi, store, callee, n_parameters, in,
	                             ir_method_type);
	ir_node  *mem   = new_d_Proj(dbgi, node, mode_M, pn_Call_M_regular);
	set_store(mem);

	type_t  *result_type = method_type->result_type;
	ir_node *result      = NULL;
	if(result_type->type != TYPE_VOID) {
		ir_mode *mode    = get_ir_mode(result_type);
		ir_node *resproj = new_d_Proj(dbgi, node, mode_T, pn_Call_T_result);
		result           = new_d_Proj(dbgi, resproj, mode, 0);
	}

	return result;
}
#endif

static ir_node *expression_to_firm(expression_t *expression)
{
	switch(expression->type) {
	case EXPR_CONST:
		return const_to_firm((const const_t*) expression);
	case EXPR_STRING_LITERAL:
		return string_literal_to_firm((const string_literal_t*) expression);
	default:
		break;
	}
	panic("unsupported expression found");
}



static void statement_to_firm(statement_t *statement);

static void return_statement_to_firm(return_statement_t *statement)
{
	dbg_info *dbgi = get_dbg_info(&statement->statement.source_position);
	ir_node  *ret;

	if(statement->return_value != NULL) {
		ir_node *retval = expression_to_firm(statement->return_value);
		ir_node *in[1];

		in[0] = retval;
		ret   = new_d_Return(dbgi, get_store(), 1, in);
	} else {
		ret   = new_d_Return(dbgi, get_store(), 0, NULL);
	}
	ir_node *end_block = get_irg_end_block(current_ir_graph);
	add_immBlock_pred(end_block, ret);

	set_cur_block(NULL);
}

static void compound_statement_to_firm(compound_statement_t *compound)
{
	statement_t *statement = compound->statements;
	for( ; statement != NULL; statement = statement->next) {
		//context2firm(&statement->context);
		statement_to_firm(statement);
	}
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
	default:
		break;
	}
	panic("Statement not implemented\n");
}

static int get_function_n_local_vars(declaration_t *declaration)
{
	(void) declaration;
	/* TODO */
	return 30;
}

static void create_function(declaration_t *declaration)
{
	ir_entity *entity = get_entity_function(declaration);

	//context2firm(declaration->context);

	if(declaration->init.statement == NULL)
		return;

	int        n_local_vars = get_function_n_local_vars(declaration);
	ir_graph  *irg          = new_ir_graph(entity, n_local_vars);
	ir_node   *first_block  = get_cur_block();

	statement_to_firm(declaration->init.statement);

	ir_node *end_block = get_irg_end_block(irg);

	/* do we have a return statement yet? */
	if(get_cur_block() != NULL) {
		ir_node *ret = new_Return(get_store(), 0, NULL);
		add_immBlock_pred(end_block, ret);
	}

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
			offset   += misalign;
		}

		set_entity_offset(entity, offset);
		offset += get_type_size_bytes(entity_type);
	}
	set_type_size_bytes(frame_type, offset);
	set_type_alignment_bytes(frame_type, align_all);
	set_type_state(frame_type, layout_fixed);

	irg_vrfy(irg);
}

static void context_to_firm(context_t *context)
{
	declaration_t *declaration = context->declarations;
	for( ; declaration != NULL; declaration = declaration->next) {
		type_t *type = declaration->type;
		if(type->type == TYPE_FUNCTION) {
			create_function(declaration);
		} else {
			/* TODO... */
		}
	}
}

void translation_unit_to_firm(translation_unit_t *unit)
{
	/* remove me later TODO FIXME */
	(void) get_type_size;

	context_to_firm(& unit->context);
}
