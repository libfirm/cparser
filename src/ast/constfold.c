/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#include "constfold.h"
#include "constfoldbits.h"

#include <libfirm/tv.h>
#include <libfirm/irmode.h>

#include "adt/error.h"
#include "adt/unicode.h"
#include "adt/util.h"
#include "ast_t.h"
#include "driver/lang_features.h"
#include "driver/target.h"
#include "type_t.h"
#include "types.h"

static ir_mode *mode_float_arithmetic;
ir_mode *atomic_modes[ATOMIC_TYPE_LAST+1];

ir_mode *get_ir_mode_storage(type_t *type)
{
	type = skip_typeref(type);
	switch (type->kind) {
	case TYPE_COMPLEX:
	case TYPE_POINTER:
	case TYPE_ARRAY:
	case TYPE_REFERENCE:
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
	case TYPE_FUNCTION: /* function is questionable here... */
		return mode_P;

	case TYPE_ATOMIC:
	case TYPE_IMAGINARY:
	case TYPE_ENUM:
		return atomic_modes[type->atomic.akind];

	case TYPE_ERROR:
	case TYPE_VOID:
	case TYPE_TYPEOF:
	case TYPE_TYPEDEF:
	case TYPE_BUILTIN_TEMPLATE:
		break;
	}
	panic("invalid type");
}

ir_mode *get_ir_mode_arithmetic(type_t *type)
{
	ir_mode *mode = get_ir_mode_storage(type);
	if (mode_is_float(mode) && mode_float_arithmetic != NULL) {
		return mode_float_arithmetic;
	}

	return mode;
}

ir_tarval *fold_builtin_inf(call_expression_t const *const call,
                            type_t const *const function_type)
{
	/* TODO: interpret string arg if provided */
	(void)call;
	type_t  *type = function_type->function.return_type;
	ir_mode *mode = get_ir_mode_storage(type);
	return get_mode_infinite(mode);
}

ir_tarval *fold_builtin_nan(call_expression_t const *const call,
                            type_t const *const function_type)
{
	/* TODO: interpret string arg if provided */
	(void)call;
	type_t  *type = function_type->function.return_type;
	ir_mode *mode = get_ir_mode_storage(type);
	return get_mode_NAN(mode);
}

static ir_tarval *get_type_size_tarval(type_t *const type, ir_mode *const mode)
{
	size_t const size = get_type_size(type);
	return new_tarval_from_long(size, mode);
}

static ir_tarval *fold_expression_to_address(expression_t const *const expr)
{
	switch (expr->kind) {
	case EXPR_SELECT: {
		select_expression_t const *const sel = &expr->select;
		type_t    *const type      = skip_typeref(sel->compound->base.type);
		ir_tarval *const base_addr = is_type_pointer(type) ? fold_expression(sel->compound) : fold_expression_to_address(sel->compound);
		ir_mode   *const mode      = get_tarval_mode(base_addr);
		ir_mode   *const mode_uint = get_reference_mode_unsigned_eq(mode);
		ir_tarval *const offset    = new_tarval_from_long(sel->compound_entry->compound_member.offset, mode_uint);
		return tarval_add(base_addr, offset);
	}

	case EXPR_ARRAY_ACCESS: {
		ir_tarval *const base_addr = fold_expression_to_address(expr->array_access.array_ref);
		ir_tarval *const idx       = fold_expression(expr->array_access.index);
		ir_mode   *const mode      = get_ir_mode_arithmetic(type_size_t);
		ir_tarval *const idx_conv  = tarval_convert_to(idx, mode);
		type_t    *const elem_type = skip_typeref(expr->array_access.array_ref->base.type);
		ir_tarval *const elem_size = get_type_size_tarval(elem_type, mode);
		return tarval_add(base_addr, tarval_mul(idx_conv, elem_size));
	}

	case EXPR_UNARY_DEREFERENCE:
		return fold_expression(expr->unary.value);

	default:
		panic("unexpected expression kind");
	}
}

typedef ir_tarval* (*fold_binary_func)(ir_tarval *left, ir_tarval *right);

static ir_tarval *fold_binary_expression_arithmetic(
		binary_expression_t const *const binexpr, fold_binary_func fold)
{
	ir_tarval *const left   = fold_expression(binexpr->left);
	ir_tarval *const right  = fold_expression(binexpr->right);
	type_t    *const type   = skip_typeref(binexpr->base.type);
	ir_mode   *const mode   = get_ir_mode_arithmetic(type);
	ir_tarval *const cleft  = tarval_convert_to(left, mode);
	ir_tarval *const cright = tarval_convert_to(right, mode);
	return fold(cleft, cright);
}

static ir_tarval *fold_binary_add(binary_expression_t const *const binexpr)
{
	ir_tarval *const l     = fold_expression(binexpr->left);
	ir_tarval *const r     = fold_expression(binexpr->right);
	ir_tarval       *ll    = l;
	ir_tarval       *rr    = r;
	type_t    *const typel = skip_typeref(binexpr->left->base.type);
	type_t    *const typer = skip_typeref(binexpr->right->base.type);
	if (is_type_pointer(typel)) {
		type_t    *const elem = skip_typeref(typel->pointer.points_to);
		ir_mode   *const mode = get_ir_mode_arithmetic(typer);
		ir_tarval *const size = get_type_size_tarval(elem, mode);
		rr = tarval_mul(rr, size);
	} else if (is_type_pointer(typer)) {
		type_t    *const elem = skip_typeref(typer->pointer.points_to);
		ir_mode   *const mode = get_ir_mode_arithmetic(typel);
		ir_tarval *const size = get_type_size_tarval(elem, mode);
		ll = tarval_mul(ll, size);
	} else {
		type_t  *const type = skip_typeref(binexpr->base.type);
		ir_mode *const mode = get_ir_mode_arithmetic(type);
		ll = tarval_convert_to(l, mode);
		rr = tarval_convert_to(r, mode);
	}
	return tarval_add(ll, rr);
}

static ir_tarval *fold_binary_sub(binary_expression_t const *const binexpr)
{
	ir_tarval *const l        = fold_expression(binexpr->left);
	ir_tarval *const r        = fold_expression(binexpr->right);
	type_t    *const type     = skip_typeref(binexpr->base.type);
	ir_mode   *const res_mode = get_ir_mode_arithmetic(type);
	type_t    *const typel    = skip_typeref(binexpr->left->base.type);
	if (is_type_pointer(typel)) {
		type_t *const elem  = skip_typeref(typel->pointer.points_to);
		type_t *const typer = skip_typeref(binexpr->right->base.type);
		if (is_type_pointer(typer)) {
			ir_tarval *const size = get_type_size_tarval(elem, res_mode);
			ir_tarval *const diff = tarval_sub(l, r, res_mode);
			return tarval_div(diff, size);
		} else {
			ir_mode   *const mode = get_tarval_mode(r);
			ir_tarval *const size = get_type_size_tarval(elem, mode);
			ir_tarval *const rr   = tarval_mul(r, size);
			return tarval_sub(l, rr, res_mode);
		}
	} else {
		ir_tarval *const conv_l = tarval_convert_to(l, res_mode);
		ir_tarval *const conv_r = tarval_convert_to(r, res_mode);
		return tarval_sub(conv_l, conv_r, NULL);
	}
}

ir_relation get_relation(const expression_kind_t kind)
{
	switch (kind) {
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

static ir_tarval *create_tarval_from_bool(ir_mode *const mode, bool const v)
{
	return (v ? get_mode_one : get_mode_null)(mode);
}

static ir_tarval *fold_binary_comparison(
		binary_expression_t const *const binexpr)
{
	ir_tarval   *const left   = fold_expression(binexpr->left);
	ir_tarval   *const right  = fold_expression(binexpr->right);
	type_t      *const atype  = skip_typeref(binexpr->left->base.type);
	ir_mode     *const amode  = get_ir_mode_arithmetic(atype);
	assert(amode == get_ir_mode_arithmetic(skip_typeref(binexpr->right->base.type)));
	ir_tarval   *const lefta  = tarval_convert_to(left, amode);
	ir_tarval   *const righta = tarval_convert_to(right, amode);

	type_t      *const type = skip_typeref(binexpr->base.type);
	ir_mode     *const mode = get_ir_mode_arithmetic(type);
	ir_relation  const rel  = get_relation(binexpr->base.kind);
	return create_tarval_from_bool(mode, tarval_cmp(lefta, righta) & rel);
}

static ir_tarval *fold_call_builtin(call_expression_t const *const call)
{
	expression_t const *const function = call->function;
	assert(function->kind == EXPR_REFERENCE);
	reference_expression_t const *const ref = &function->reference;
	assert(ref->entity->kind == ENTITY_FUNCTION);

	type_t *expr_type = skip_typeref(ref->base.type);
	assert(is_type_pointer(expr_type));
	type_t *function_type = skip_typeref(expr_type->pointer.points_to);
	assert(is_type_function(function_type));

	switch (ref->entity->function.btk) {
	case BUILTIN_INF: return fold_builtin_inf(call, function_type);
	case BUILTIN_NAN: return fold_builtin_nan(call, function_type);
	default:
		panic("builtin is no constant");
	}
}

static ir_tarval *literal_to_tarval_(const literal_expression_t *literal,
                                    ir_mode *mode)
{
	switch (literal->base.kind) {
	case EXPR_LITERAL_INTEGER:
		assert(literal->target_value != NULL);
		return literal->target_value;

	case EXPR_LITERAL_FLOATINGPOINT:
		return new_tarval_from_str(literal->value->begin,
		                           literal->value->size, mode);

	case EXPR_LITERAL_BOOLEAN:
		if (literal->value->begin[0] == 't') {
			return get_mode_one(mode);
		} else {
			assert(literal->value->begin[0] == 'f');
	case EXPR_LITERAL_MS_NOOP:
			return get_mode_null(mode);
		}

	default:
		panic("invalid literal kind");
	}
}

static ir_tarval *literal_to_tarval(const literal_expression_t *literal)
{
	type_t  *const type = skip_typeref(literal->base.type);
	ir_mode *const mode = get_ir_mode_storage(type);
	return literal_to_tarval_(literal, mode);
}

static ir_tarval *char_literal_to_tarval(string_literal_expression_t const *literal)
{
	type_t     *type   = skip_typeref(literal->base.type);
	ir_mode    *mode   = get_ir_mode_storage(type);
	const char *string = literal->value->begin;
	size_t      size   = literal->value->size;

	switch (literal->value->encoding) {
	case STRING_ENCODING_WIDE: {
		utf32  v = read_utf8_char(&string);
		char   buf[128];
		size_t len = snprintf(buf, sizeof(buf), UTF32_PRINTF_FORMAT, v);

		return new_tarval_from_str(buf, len, mode);
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

		return new_tarval_from_str(buf, len, mode);
	}

	default:
		panic("invalid literal kind");
	}
}

void determine_enum_values(enum_t *const enume)
{
	ir_mode   *const mode    = atomic_modes[enume->akind];
	ir_tarval *const one     = get_mode_one(mode);
	ir_tarval *      tv_next = get_mode_null(mode);

	for (entity_t *entry = enume->first_value;
	     entry != NULL && entry->kind == ENTITY_ENUM_VALUE;
	     entry = entry->base.next) {
		expression_t *const init = entry->enum_value.value;
		if (init != NULL) {
			type_t *const init_type = skip_typeref(init->base.type);
			if (!is_type_valid(init_type))
				continue;
			tv_next = fold_expression(init);
		}
		assert(entry->enum_value.tv == NULL || entry->enum_value.tv == tv_next);
		entry->enum_value.tv = tv_next;
		tv_next = tarval_add(tv_next, one);
	}
}

ir_tarval *get_enum_value(const enum_value_t *enum_value)
{
	if (enum_value->tv == NULL)
		determine_enum_values(enum_value->enume);
	return enum_value->tv;
}

static ir_tarval *enum_constant_to_tarval(reference_expression_t const *const ref)
{
	entity_t *entity = ref->entity;
	assert(entity->kind == ENTITY_ENUM_VALUE);
	return get_enum_value(&entity->enum_value);
}

static long get_offsetof_offset(const offsetof_expression_t *expression)
{
	type_t *orig_type = expression->type;
	long    offset    = 0;

	designator_t *designator = expression->designator;
	for ( ; designator != NULL; designator = designator->next) {
		type_t *type = skip_typeref(orig_type);

		if (designator->symbol != NULL) {
			assert(is_type_compound(type));
			symbol_t *symbol = designator->symbol;

			compound_t *compound = type->compound.compound;
			entity_t   *iter     = compound->members.entities;
			for (; iter->base.symbol != symbol; iter = iter->base.next) {}

			assert(iter->kind == ENTITY_COMPOUND_MEMBER);
			offset += iter->compound_member.offset;

			orig_type = iter->declaration.type;
		} else {
			expression_t *array_index = designator->array_index;
			assert(designator->array_index != NULL);
			assert(is_type_array(type));

			long    index_long   = fold_expression_to_int(array_index);
			type_t *element_type = type->array.element_type;
			long    element_size = get_type_size(element_type);

			/* TODO: check for overflow */
			offset += index_long * element_size;

			orig_type = type->array.element_type;
		}
	}

	return offset;
}

static ir_tarval *offsetof_to_tarval(offsetof_expression_t const *const expression)
{
	ir_mode *const mode   = get_ir_mode_storage(expression->base.type);
	long     const offset = get_offsetof_offset(expression);
	return new_tarval_from_long(offset, mode);
}

static unsigned get_object_alignment(expression_t const *expr);

static unsigned get_address_alignment(expression_t const *const expr)
{
	if (expr->kind == EXPR_UNARY_TAKE_ADDRESS) {
		return get_object_alignment(expr->unary.value);
	} else {
		type_t *const type = skip_typeref(expr->base.type);
		assert(is_type_pointer(type));
		return get_type_alignment(type->pointer.points_to);
	}
}

static unsigned get_object_alignment(expression_t const *const expr)
{
	entity_t *ent;
	switch (expr->kind) {
	case EXPR_ARRAY_ACCESS:      return get_address_alignment(expr->array_access.array_ref);
	case EXPR_UNARY_DEREFERENCE: return get_address_alignment(expr->unary.value);
	case EXPR_REFERENCE:         ent = expr->reference.entity;      break;
	case EXPR_SELECT:            ent = expr->select.compound_entry; break;
	default:                     return get_type_alignment(expr->base.type);
	}
	assert(is_declaration(ent));
	return ent->declaration.alignment;
}

static ir_tarval *alignof_to_tarval(const typeprop_expression_t *expression)
{
	unsigned const alignment = expression->tp_expression
		? get_object_alignment(expression->tp_expression)
		: get_type_alignment(expression->type);
	ir_mode *const mode = get_ir_mode_storage(expression->base.type);
	return new_tarval_from_long(alignment, mode);
}

static ir_tarval *builtin_constant_to_tarval(
		builtin_constant_expression_t const *const expression)
{
	ir_mode *const mode = get_ir_mode_storage(expression->base.type);
	bool     const v    = is_constant_expression(expression->value) != EXPR_CLASS_VARIABLE;
	return create_tarval_from_bool(mode, v);
}

static ir_tarval *builtin_types_compatible_to_tarval(
		builtin_types_compatible_expression_t const *const expression)
{
	type_t  *const left  = skip_typeref(expression->left);
	type_t  *const right = skip_typeref(expression->right);
	bool     const value = types_compatible_ignore_qualifiers(left, right);
	ir_mode *const mode  = get_ir_mode_storage(expression->base.type);
	return create_tarval_from_bool(mode, value);
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

static ir_tarval *classify_type_to_tarval(const classify_type_expression_t *const expr)
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
				panic("Unexpected atomic type.");
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
			/* gcc cannot do that */
			case TYPE_VOID:            tc = void_type_class;    goto make_const;

			/* gcc classifies the referenced type */
			case TYPE_REFERENCE: type = type->reference.refers_to; continue;

			/* typedef/typeof should be skipped already */
			case TYPE_TYPEDEF:
			case TYPE_TYPEOF:
			case TYPE_ERROR:
			case TYPE_BUILTIN_TEMPLATE:
				break;
		}
		panic("unexpected type.");
	}

make_const:;
	ir_mode *const mode = atomic_modes[ATOMIC_TYPE_INT];
	return new_tarval_from_long(tc, mode);
}

ir_tarval *fold_expression(expression_t const *const expr)
{
	switch (expr->kind) {
	case EXPR_CONDITIONAL: {
		conditional_expression_t const *const cond = &expr->conditional;
		if (cond->true_expression != NULL) {
			/* note that we need this if in case of a complex expression as
			 * condition */
			bool condval = fold_expression_to_bool(cond->condition);
			expression_t *res = condval ? cond->true_expression
			                            : cond->false_expression;
			return fold_expression(res);
		}
		ir_tarval *const val = fold_expression(cond->condition);
		return
			tarval_is_null(val)   ? fold_expression(cond->false_expression) :
			cond->true_expression ? fold_expression(cond->true_expression)  :
			val;
	}

	case EXPR_SIZEOF: {
		type_t  *const type = skip_typeref(expr->typeprop.type);
		ir_mode *const mode = get_ir_mode_arithmetic(skip_typeref(expr->base.type));
		return get_type_size_tarval(type, mode);
	}

	case EXPR_ALIGNOF:                    return alignof_to_tarval(                 &expr->typeprop);
	case EXPR_BUILTIN_CONSTANT_P:         return builtin_constant_to_tarval(        &expr->builtin_constant);
	case EXPR_BUILTIN_TYPES_COMPATIBLE_P: return builtin_types_compatible_to_tarval(&expr->builtin_types_compatible);
	case EXPR_CLASSIFY_TYPE:              return classify_type_to_tarval(           &expr->classify_type);
	case EXPR_ENUM_CONSTANT:              return enum_constant_to_tarval(           &expr->reference);
	case EXPR_LITERAL_CASES:              return literal_to_tarval(                 &expr->literal);
	case EXPR_LITERAL_CHARACTER:          return char_literal_to_tarval(            &expr->string_literal);
	case EXPR_OFFSETOF:                   return offsetof_to_tarval(                &expr->offsetofe);
	case EXPR_UNARY_TAKE_ADDRESS:         return fold_expression_to_address(         expr->unary.value);

	case EXPR_UNARY_NEGATE:         return tarval_neg(fold_expression(expr->unary.value));
	case EXPR_UNARY_PLUS:           return fold_expression(expr->unary.value);
	case EXPR_UNARY_COMPLEMENT:     return tarval_not(fold_expression(expr->unary.value));

	case EXPR_UNARY_NOT: {
		type_t    *const type = skip_typeref(expr->base.type);
		ir_mode   *const mode = get_ir_mode_arithmetic(type);
		ir_tarval *const val  = fold_expression(expr->unary.value);
		return create_tarval_from_bool(mode, tarval_is_null(val));
	}

	case EXPR_UNARY_CAST: {
		type_t    *const type = skip_typeref(expr->base.type);
		ir_mode   *const mode = get_ir_mode_arithmetic(type);
		ir_tarval *const val  = fold_expression(expr->unary.value);
		if (is_type_atomic(type, ATOMIC_TYPE_BOOL)) {
			return create_tarval_from_bool(mode, !tarval_is_null(val));
		} else {
			return tarval_convert_to(val, mode);
		}
	}

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
		return fold_binary_comparison(&expr->binary);
	case EXPR_BINARY_ADD:
		return fold_binary_add(&expr->binary);
	case EXPR_BINARY_SUB:
		return fold_binary_sub(&expr->binary);
	case EXPR_BINARY_MUL:
		return fold_binary_expression_arithmetic(&expr->binary, tarval_mul);
	case EXPR_BINARY_DIV:
		return fold_binary_expression_arithmetic(&expr->binary, tarval_div);
	case EXPR_BINARY_MOD:
		return fold_binary_expression_arithmetic(&expr->binary, tarval_mod);
	case EXPR_BINARY_BITWISE_OR:
		return fold_binary_expression_arithmetic(&expr->binary, tarval_or);
	case EXPR_BINARY_BITWISE_AND:
		return fold_binary_expression_arithmetic(&expr->binary, tarval_and);
	case EXPR_BINARY_BITWISE_XOR:
		return fold_binary_expression_arithmetic(&expr->binary, tarval_eor);
	case EXPR_BINARY_SHIFTLEFT:
		return fold_binary_expression_arithmetic(&expr->binary, tarval_shl);
	case EXPR_BINARY_SHIFTRIGHT: {
		fold_binary_func fold = is_type_signed(skip_typeref(expr->base.type))
		                      ? tarval_shrs : tarval_shr;
		return fold_binary_expression_arithmetic(&expr->binary, fold);
	}

	case EXPR_BINARY_LOGICAL_AND: {
		bool const c =
			!tarval_is_null(fold_expression(expr->binary.left)) &&
			!tarval_is_null(fold_expression(expr->binary.right));
		type_t  *const type = skip_typeref(expr->base.type);
		ir_mode *const mode = get_ir_mode_arithmetic(type);
		return create_tarval_from_bool(mode, c);
	}

	case EXPR_BINARY_LOGICAL_OR: {
		bool const c =
			!tarval_is_null(fold_expression(expr->binary.left)) ||
			!tarval_is_null(fold_expression(expr->binary.right));
		type_t  *const type = skip_typeref(expr->base.type);
		ir_mode *const mode = get_ir_mode_arithmetic(type);
		return create_tarval_from_bool(mode, c);
	}

	case EXPR_UNARY_REAL: {
		complex_constant cnst = fold_complex(expr->unary.value);
		return cnst.real;
	}
	case EXPR_UNARY_IMAG: {
		complex_constant cnst = fold_complex(expr->unary.value);
		return cnst.imag;
	}
	case EXPR_CALL:
		return fold_call_builtin(&expr->call);

	case EXPR_ARRAY_ACCESS:
	case EXPR_BINARY_ADD_ASSIGN:
	case EXPR_BINARY_ASSIGN:
	case EXPR_BINARY_BITWISE_AND_ASSIGN:
	case EXPR_BINARY_BITWISE_OR_ASSIGN:
	case EXPR_BINARY_BITWISE_XOR_ASSIGN:
	case EXPR_BINARY_COMMA:
	case EXPR_BINARY_DIV_ASSIGN:
	case EXPR_BINARY_MOD_ASSIGN:
	case EXPR_BINARY_MUL_ASSIGN:
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:
	case EXPR_BINARY_SUB_ASSIGN:
	case EXPR_COMPOUND_LITERAL:
	case EXPR_ERROR:
	case EXPR_FUNCNAME:
	case EXPR_LABEL_ADDRESS:
	case EXPR_REFERENCE:
	case EXPR_SELECT:
	case EXPR_STATEMENT:
	case EXPR_STRING_LITERAL:
	case EXPR_UNARY_ASSUME:
	case EXPR_UNARY_DELETE:
	case EXPR_UNARY_DELETE_ARRAY:
	case EXPR_UNARY_DEREFERENCE:
	case EXPR_UNARY_POSTFIX_DECREMENT:
	case EXPR_UNARY_POSTFIX_INCREMENT:
	case EXPR_UNARY_PREFIX_DECREMENT:
	case EXPR_UNARY_PREFIX_INCREMENT:
	case EXPR_UNARY_THROW:
	case EXPR_VA_ARG:
	case EXPR_VA_COPY:
	case EXPR_VA_START:
		panic("invalid expression kind for constant folding");
	}

	panic("unexpected expression kind for constant folding");
}

ir_mode *get_complex_mode_storage(type_t *type)
{
	type = skip_typeref(type);
	assert(type->kind == TYPE_COMPLEX);
	return atomic_modes[type->atomic.akind];
}

ir_mode *get_complex_mode_arithmetic(type_t *type)
{
	ir_mode *mode = get_complex_mode_storage(type);
	if (mode_is_float(mode) && mode_float_arithmetic != NULL) {
		return mode_float_arithmetic;
	}

	return mode;
}

typedef complex_constant (*fold_complex_binop_func)(complex_constant left,
                                                    complex_constant right);

static complex_constant fold_complex_add(complex_constant const left,
                                         complex_constant const right)
{
	return (complex_constant) {
		tarval_add(left.real, right.real),
		tarval_add(left.imag, right.imag)
	};
}

static complex_constant fold_complex_sub(complex_constant const left,
                                         complex_constant const right)
{
	return (complex_constant) {
		tarval_sub(left.real, right.real, NULL),
		tarval_sub(left.imag, right.imag, NULL)
	};
}

static complex_constant fold_complex_mul(complex_constant const left,
                                         complex_constant const right)
{
	ir_tarval *const op1 = tarval_mul(left.real, right.real);
	ir_tarval *const op2 = tarval_mul(left.imag, right.imag);
	ir_tarval *const op3 = tarval_mul(left.real, right.imag);
	ir_tarval *const op4 = tarval_mul(left.imag, right.real);
	return (complex_constant) {
		tarval_sub(op1, op2, NULL),
		tarval_add(op3, op4)
	};
}

static complex_constant fold_complex_div(complex_constant const left,
                                         complex_constant const right)
{
	ir_tarval *const op1 = tarval_mul(left.real, right.real);
	ir_tarval *const op2 = tarval_mul(left.imag, right.imag);
	ir_tarval *const op3 = tarval_mul(left.imag, right.real);
	ir_tarval *const op4 = tarval_mul(left.real, right.imag);
	ir_tarval *const op5 = tarval_mul(right.real, right.real);
	ir_tarval *const op6 = tarval_mul(right.imag, right.imag);
	ir_tarval *const real_dividend = tarval_add(op1, op2);
	ir_tarval *const real_divisor  = tarval_add(op5, op6);
	ir_tarval *const imag_dividend = tarval_sub(op3, op4, NULL);
	ir_tarval *const imag_divisor  = tarval_add(op5, op6);
	return (complex_constant) {
		tarval_div(real_dividend, real_divisor),
		tarval_div(imag_dividend, imag_divisor)
	};
}

static complex_constant convert_complex_constant(const complex_constant cnst,
                                                 ir_mode *mode)
{
	if (get_tarval_mode(cnst.real) == mode)
		return cnst;
	return (complex_constant) {
		tarval_convert_to(cnst.real, mode),
		tarval_convert_to(cnst.imag, mode)
	};
}

static complex_constant fold_complex_binop(
		binary_expression_t const *const binexpr, fold_complex_binop_func fold)
{
	complex_constant left  = fold_complex(binexpr->left);
	complex_constant right = fold_complex(binexpr->right);
	ir_mode *mode = get_complex_mode_arithmetic(binexpr->base.type);
	left  = convert_complex_constant(left, mode);
	right = convert_complex_constant(right, mode);
	return fold(left, right);
}

static complex_constant fold_complex_negate(
		unary_expression_t const *const expr)
{
	complex_constant value = fold_complex(expr->value);
	ir_mode         *mode  = get_complex_mode_arithmetic(expr->base.type);
	value = convert_complex_constant(value, mode);
	return (complex_constant) {
		tarval_neg(value.real),
		tarval_neg(value.imag)
	};
}

static complex_constant fold_complex_complement(
		unary_expression_t const *const expr)
{
	complex_constant value = fold_complex(expr->value);
	ir_mode         *mode  = get_complex_mode_arithmetic(expr->base.type);
	value = convert_complex_constant(value, mode);
	return (complex_constant) {
		value.real,
		tarval_neg(value.imag)
	};
}

complex_constant fold_complex_literal(literal_expression_t const *const literal)
{
	type_t    *type     = skip_typeref(literal->base.type);
	ir_mode   *mode     = get_complex_mode_storage(type);
	ir_tarval *litvalue = literal_to_tarval_(literal, mode);
	ir_tarval *zero     = get_mode_null(mode);
	return (complex_constant) {
		zero,
		litvalue
	};
}

static complex_constant fold_complex_conditional(
		conditional_expression_t const *const cond)
{
	type_t const *const condition_type
		= skip_typeref(cond->condition->base.type);
	if (!is_type_complex(condition_type)) {
		bool condval = fold_expression_to_bool(cond->condition);
		expression_t *to_fold = condval ? cond->true_expression
										: cond->false_expression;
		return fold_complex(to_fold);
	}
	complex_constant const val = fold_complex(cond->condition);
	return
		tarval_is_null(val.real) && tarval_is_null(val.imag)
		? fold_complex(cond->false_expression)
		: cond->true_expression
			? fold_complex(cond->true_expression) : val;
}

static complex_constant fold_complex_cast(const unary_expression_t *expression)
{
	const expression_t *const value     = expression->value;
	type_t             *const from_type = skip_typeref(value->base.type);
	type_t             *const to_type   = skip_typeref(expression->base.type);
	ir_mode            *const mode      = get_complex_mode_storage(to_type);

	if (is_type_complex(from_type)) {
		complex_constant const folded = fold_complex(value);
		return convert_complex_constant(folded, mode);
	} else {
		ir_tarval *const folded = fold_expression(value);
		ir_tarval *const casted = tarval_convert_to(folded, mode);
		ir_tarval *const zero   = get_mode_null(mode);
		return (complex_constant) { casted, zero };
	}
}

complex_constant fold_complex(const expression_t *expression)
{
	assert(is_constant_expression(expression) >= EXPR_CLASS_CONSTANT);
	switch (expression->kind) {
	case EXPR_BINARY_ADD:
		return fold_complex_binop(&expression->binary, fold_complex_add);
	case EXPR_BINARY_SUB:
		return fold_complex_binop(&expression->binary, fold_complex_sub);
	case EXPR_BINARY_MUL:
		return fold_complex_binop(&expression->binary, fold_complex_mul);
	case EXPR_BINARY_DIV:
		return fold_complex_binop(&expression->binary, fold_complex_div);
	case EXPR_UNARY_PLUS:
		return fold_complex(expression->unary.value);
	case EXPR_UNARY_NEGATE:
		return fold_complex_negate(&expression->unary);
	case EXPR_UNARY_COMPLEMENT:
		return fold_complex_complement(&expression->unary);
	case EXPR_LITERAL_INTEGER:
	case EXPR_LITERAL_FLOATINGPOINT:
		return fold_complex_literal(&expression->literal);
	case EXPR_CONDITIONAL:
		return fold_complex_conditional(&expression->conditional);
	case EXPR_UNARY_CAST:
		return fold_complex_cast(&expression->unary);
	case EXPR_ARRAY_ACCESS:
	case EXPR_BINARY_ADD_ASSIGN:
	case EXPR_BINARY_ASSIGN:
	case EXPR_BINARY_COMMA:
	case EXPR_BINARY_DIV_ASSIGN:
	case EXPR_BINARY_MUL_ASSIGN:
	case EXPR_BINARY_SUB_ASSIGN:
	case EXPR_CALL:
	case EXPR_REFERENCE:
	case EXPR_SELECT:
	case EXPR_STATEMENT:
	case EXPR_UNARY_DEREFERENCE:
	case EXPR_UNARY_POSTFIX_DECREMENT:
	case EXPR_UNARY_POSTFIX_INCREMENT:
	case EXPR_UNARY_PREFIX_DECREMENT:
	case EXPR_UNARY_PREFIX_INCREMENT:
		panic("invalid expression kind for constant folding");

	case NEVER_COMPLEX_CASES:
		break;
	}
	panic("internal error: non-complex expression in fold_complex");
}

bool folded_expression_is_negative(const expression_t *expression)
{
	ir_tarval *tv = fold_expression(expression);
	return tarval_is_negative(tv);
}

long fold_expression_to_int(const expression_t *expression)
{
	ir_tarval *tv = fold_expression(expression);
	if (!tarval_is_long(tv)) {
		panic("result of constant folding is not integer");
	}

	return get_tarval_long(tv);
}

bool fold_expression_to_bool(const expression_t *expression)
{
	type_t *type = skip_typeref(expression->base.type);
	if (is_type_complex(type)) {
		complex_constant tvs = fold_complex(expression);
		return !tarval_is_null(tvs.real) || !tarval_is_null(tvs.imag);
	} else {
		ir_tarval *tv = fold_expression(expression);
		return !tarval_is_null(tv);
	}
}

static unsigned decide_modulo_shift(unsigned type_size)
{
	if (target.modulo_shift == 0)
		return 0;
	return MAX(type_size, target.modulo_shift);
}

bool enum_bitfield_big_enough(enum_t *enume, type_t *base_type,
                              unsigned bitfield_size)
{
	ir_mode    *mode        = get_ir_mode_storage(base_type);
	ir_tarval  *max         = get_mode_max(mode);
	ir_tarval  *min         = get_mode_min(mode);
	bool       is_signed    = is_type_signed(base_type);
	unsigned   mode_size    = get_mode_size_bits(mode);
	unsigned   shift_amount = mode_size - bitfield_size + is_signed;
	ir_tarval *adjusted_max;
	ir_tarval *adjusted_min;
	/* corner case: signed mode with just sign bit results in shift_amount
	 * being as big as mode_size triggering "modulo shift" which is not what
	 * we want here. */
	if (shift_amount >= mode_size) {
		assert(bitfield_size == 1 && mode_is_signed(mode));
		adjusted_max = get_mode_null(mode);
		adjusted_min = get_mode_all_one(mode);
	} else {
		adjusted_max = tarval_shr_unsigned(max, shift_amount);
		adjusted_min = tarval_shrs_unsigned(min, shift_amount);
	}

	for (entity_t *entry = enume->first_value;
	     entry != NULL && entry->kind == ENTITY_ENUM_VALUE;
	     entry = entry->base.next) {
		ir_tarval *tv = get_enum_value(&entry->enum_value);
		if (tv == NULL)
			continue;
		ir_tarval *tvc = tarval_convert_to(tv, mode);
		if (tarval_cmp(tvc, adjusted_min) == ir_relation_less
		 || tarval_cmp(tvc, adjusted_max) == ir_relation_greater) {
			return false;
		}
	}
	return true;
}

static ir_mode *init_atomic_ir_mode(atomic_type_kind_t kind)
{
	unsigned flags = get_atomic_type_flags(kind);
	unsigned size  = get_atomic_type_size(kind);
	if (flags & ATOMIC_TYPE_FLAG_FLOAT) {
		switch (size) {
		case 4:
			return new_float_mode("F32", irma_ieee754, 8, 23,
			                      target.float_int_overflow);
		case 8:
			return new_float_mode("F64", irma_ieee754, 11, 52,
			                      target.float_int_overflow);
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

void init_constfold(void)
{
	const backend_params *be_params = be_get_backend_param();
	const ir_type *const type_ld = be_params->type_long_double;
	if (type_ld != NULL)
		atomic_modes[ATOMIC_TYPE_LONG_DOUBLE] = get_type_mode(type_ld);
	mode_float_arithmetic = be_params->mode_float_arithmetic;

	for (int i = 0; i <= ATOMIC_TYPE_LAST; ++i) {
		if (atomic_modes[i] != NULL)
			continue;
		atomic_modes[i] = init_atomic_ir_mode((atomic_type_kind_t) i);
	}

	tarval_set_wrap_on_overflow(true);
}
