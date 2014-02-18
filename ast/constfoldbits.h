#ifndef CONSTFOLDBITS_H
#define CONSTFOLDBITS_H

#include "ast_t.h"

extern ir_mode *atomic_modes[ATOMIC_TYPE_LAST+1];

typedef struct complex_constant {
	ir_tarval *real;
	ir_tarval *imag;
} complex_constant;

complex_constant fold_complex(const expression_t *expression);

ir_tarval *fold_builtin_inf(call_expression_t const *call, type_t const *type);
ir_tarval *fold_builtin_nan(call_expression_t const *call, type_t const *type);

void determine_enum_values(enum_type_t *type);

ir_relation get_relation(expression_kind_t kind);

complex_constant fold_complex_literal(const literal_expression_t *literal);

ir_mode *get_ir_mode_storage(type_t *type);
/**
 * get arithmetic mode for a type. This is different from get_ir_mode_storage,
 * int that it returns bigger modes for floating point on some platforms
 * (x87 internally does arithemtic with 80bits)
 */
ir_mode *get_ir_mode_arithmetic(type_t *type);

ir_mode *get_complex_mode_arithmetic(type_t *type);
ir_mode *get_complex_mode_storage(type_t *type);

/** list of expression kinds that never return a complex result */
#define NEVER_COMPLEX_CASES  \
	     EXPR_ALIGNOF:                    \
	case EXPR_BINARY_BITWISE_AND:         \
	case EXPR_BINARY_BITWISE_AND_ASSIGN:  \
	case EXPR_BINARY_BITWISE_OR:          \
	case EXPR_BINARY_BITWISE_OR_ASSIGN:   \
	case EXPR_BINARY_BITWISE_XOR:         \
	case EXPR_BINARY_BITWISE_XOR_ASSIGN:  \
	case EXPR_BINARY_EQUAL:               \
	case EXPR_BINARY_GREATER:             \
	case EXPR_BINARY_GREATEREQUAL:        \
	case EXPR_BINARY_ISGREATER:           \
	case EXPR_BINARY_ISGREATEREQUAL:      \
	case EXPR_BINARY_ISLESS:              \
	case EXPR_BINARY_ISLESSEQUAL:         \
	case EXPR_BINARY_ISLESSGREATER:       \
	case EXPR_BINARY_ISUNORDERED:         \
	case EXPR_BINARY_LESS:                \
	case EXPR_BINARY_LESSEQUAL:           \
	case EXPR_BINARY_LOGICAL_AND:         \
	case EXPR_BINARY_LOGICAL_OR:          \
	case EXPR_BINARY_MOD:                 \
	case EXPR_BINARY_MOD_ASSIGN:          \
	case EXPR_BINARY_NOTEQUAL:            \
	case EXPR_BINARY_SHIFTLEFT:           \
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:    \
	case EXPR_BINARY_SHIFTRIGHT:          \
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:   \
	case EXPR_BUILTIN_CONSTANT_P:         \
	case EXPR_BUILTIN_TYPES_COMPATIBLE_P: \
	case EXPR_CLASSIFY_TYPE:              \
	case EXPR_COMPOUND_LITERAL:           \
	case EXPR_ENUM_CONSTANT:              \
	case EXPR_ERROR:                      \
	case EXPR_FUNCNAME:                   \
	case EXPR_LABEL_ADDRESS:              \
	case EXPR_LITERAL_BOOLEAN:            \
	case EXPR_LITERAL_CHARACTER:          \
	case EXPR_LITERAL_MS_NOOP:            \
	case EXPR_OFFSETOF:                   \
	case EXPR_SIZEOF:                     \
	case EXPR_STRING_LITERAL:             \
	case EXPR_UNARY_ASSUME:               \
	case EXPR_UNARY_DELETE:               \
	case EXPR_UNARY_DELETE_ARRAY:         \
	case EXPR_UNARY_IMAG:                 \
	case EXPR_UNARY_NOT:                  \
	case EXPR_UNARY_REAL:                 \
	case EXPR_UNARY_TAKE_ADDRESS:         \
	case EXPR_UNARY_THROW:                \
	case EXPR_VA_ARG:                     \
	case EXPR_VA_COPY:                    \
	case EXPR_VA_START                    \

#endif
