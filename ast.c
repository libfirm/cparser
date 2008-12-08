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

#include "ast_t.h"
#include "symbol_t.h"
#include "type_t.h"
#include "parser.h"
#include "lang_features.h"
#include "entity_t.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#ifdef __INTEL_COMPILER
#include <mathimf.h>
#else
#include <math.h>
#endif

#include "adt/error.h"

struct obstack ast_obstack;

static FILE *out;
static int   indent;

/** If set, implicit casts are printed. */
bool print_implicit_casts = false;

/** If set parenthesis are printed to indicate operator precedence. */
bool print_parenthesis = false;

static void print_statement(const statement_t *statement);
static void print_expression_prec(const expression_t *expression, unsigned prec);

void change_indent(int delta)
{
	indent += delta;
	assert(indent >= 0);
}

void print_indent(void)
{
	for (int i = 0; i < indent; ++i)
		fputc('\t', out);
}

/**
 * Returns 1 if a given precedence level has right-to-left
 * associativity, else 0.
 *
 * @param precedence   the operator precedence
 */
static int right_to_left(unsigned precedence)
{
	switch (precedence) {
		case PREC_ASSIGNMENT:
		case PREC_CONDITIONAL:
		case PREC_UNARY:
			return 1;

		default:
			return 0;
	}
}

/**
 * Return the precedence of an expression given by its kind.
 *
 * @param kind   the expression kind
 */
static unsigned get_expression_precedence(expression_kind_t kind)
{
	static const unsigned prec[] = {
		[EXPR_UNKNOWN]                   = PREC_PRIMARY,
		[EXPR_INVALID]                   = PREC_PRIMARY,
		[EXPR_REFERENCE]                 = PREC_PRIMARY,
		[EXPR_REFERENCE_ENUM_VALUE]      = PREC_PRIMARY,
		[EXPR_CHARACTER_CONSTANT]        = PREC_PRIMARY,
		[EXPR_WIDE_CHARACTER_CONSTANT]   = PREC_PRIMARY,
		[EXPR_CONST]                     = PREC_PRIMARY,
		[EXPR_STRING_LITERAL]            = PREC_PRIMARY,
		[EXPR_WIDE_STRING_LITERAL]       = PREC_PRIMARY,
		[EXPR_COMPOUND_LITERAL]          = PREC_UNARY,
		[EXPR_CALL]                      = PREC_POSTFIX,
		[EXPR_CONDITIONAL]               = PREC_CONDITIONAL,
		[EXPR_SELECT]                    = PREC_POSTFIX,
		[EXPR_ARRAY_ACCESS]              = PREC_POSTFIX,
		[EXPR_SIZEOF]                    = PREC_UNARY,
		[EXPR_CLASSIFY_TYPE]             = PREC_UNARY,
		[EXPR_ALIGNOF]                   = PREC_UNARY,

		[EXPR_FUNCNAME]                  = PREC_PRIMARY,
		[EXPR_BUILTIN_SYMBOL]            = PREC_PRIMARY,
		[EXPR_BUILTIN_CONSTANT_P]        = PREC_PRIMARY,
		[EXPR_BUILTIN_PREFETCH]          = PREC_PRIMARY,
		[EXPR_OFFSETOF]                  = PREC_PRIMARY,
		[EXPR_VA_START]                  = PREC_PRIMARY,
		[EXPR_VA_ARG]                    = PREC_PRIMARY,
		[EXPR_STATEMENT]                 = PREC_PRIMARY,
		[EXPR_LABEL_ADDRESS]             = PREC_PRIMARY,

		[EXPR_UNARY_NEGATE]              = PREC_UNARY,
		[EXPR_UNARY_PLUS]                = PREC_UNARY,
		[EXPR_UNARY_BITWISE_NEGATE]      = PREC_UNARY,
		[EXPR_UNARY_NOT]                 = PREC_UNARY,
		[EXPR_UNARY_DEREFERENCE]         = PREC_UNARY,
		[EXPR_UNARY_TAKE_ADDRESS]        = PREC_UNARY,
		[EXPR_UNARY_POSTFIX_INCREMENT]   = PREC_POSTFIX,
		[EXPR_UNARY_POSTFIX_DECREMENT]   = PREC_POSTFIX,
		[EXPR_UNARY_PREFIX_INCREMENT]    = PREC_UNARY,
		[EXPR_UNARY_PREFIX_DECREMENT]    = PREC_UNARY,
		[EXPR_UNARY_CAST]                = PREC_UNARY,
		[EXPR_UNARY_CAST_IMPLICIT]       = PREC_UNARY,
		[EXPR_UNARY_ASSUME]              = PREC_PRIMARY,
		[EXPR_UNARY_DELETE]              = PREC_UNARY,
		[EXPR_UNARY_DELETE_ARRAY]        = PREC_UNARY,
		[EXPR_UNARY_THROW]               = PREC_ASSIGNMENT,

		[EXPR_BINARY_ADD]                = PREC_ADDITIVE,
		[EXPR_BINARY_SUB]                = PREC_ADDITIVE,
		[EXPR_BINARY_MUL]                = PREC_MULTIPLICATIVE,
		[EXPR_BINARY_DIV]                = PREC_MULTIPLICATIVE,
		[EXPR_BINARY_MOD]                = PREC_MULTIPLICATIVE,
		[EXPR_BINARY_EQUAL]              = PREC_EQUALITY,
		[EXPR_BINARY_NOTEQUAL]           = PREC_EQUALITY,
		[EXPR_BINARY_LESS]               = PREC_RELATIONAL,
		[EXPR_BINARY_LESSEQUAL]          = PREC_RELATIONAL,
		[EXPR_BINARY_GREATER]            = PREC_RELATIONAL,
		[EXPR_BINARY_GREATEREQUAL]       = PREC_RELATIONAL,
		[EXPR_BINARY_BITWISE_AND]        = PREC_AND,
		[EXPR_BINARY_BITWISE_OR]         = PREC_OR,
		[EXPR_BINARY_BITWISE_XOR]        = PREC_XOR,
		[EXPR_BINARY_LOGICAL_AND]        = PREC_LOGICAL_AND,
		[EXPR_BINARY_LOGICAL_OR]         = PREC_LOGICAL_OR,
		[EXPR_BINARY_SHIFTLEFT]          = PREC_SHIFT,
		[EXPR_BINARY_SHIFTRIGHT]         = PREC_SHIFT,
		[EXPR_BINARY_ASSIGN]             = PREC_ASSIGNMENT,
		[EXPR_BINARY_MUL_ASSIGN]         = PREC_ASSIGNMENT,
		[EXPR_BINARY_DIV_ASSIGN]         = PREC_ASSIGNMENT,
		[EXPR_BINARY_MOD_ASSIGN]         = PREC_ASSIGNMENT,
		[EXPR_BINARY_ADD_ASSIGN]         = PREC_ASSIGNMENT,
		[EXPR_BINARY_SUB_ASSIGN]         = PREC_ASSIGNMENT,
		[EXPR_BINARY_SHIFTLEFT_ASSIGN]   = PREC_ASSIGNMENT,
		[EXPR_BINARY_SHIFTRIGHT_ASSIGN]  = PREC_ASSIGNMENT,
		[EXPR_BINARY_BITWISE_AND_ASSIGN] = PREC_ASSIGNMENT,
		[EXPR_BINARY_BITWISE_XOR_ASSIGN] = PREC_ASSIGNMENT,
		[EXPR_BINARY_BITWISE_OR_ASSIGN]  = PREC_ASSIGNMENT,
		[EXPR_BINARY_COMMA]              = PREC_EXPRESSION,

		[EXPR_BINARY_ISGREATER]          = PREC_PRIMARY,
		[EXPR_BINARY_ISGREATEREQUAL]     = PREC_PRIMARY,
		[EXPR_BINARY_ISLESS]             = PREC_PRIMARY,
		[EXPR_BINARY_ISLESSEQUAL]        = PREC_PRIMARY,
		[EXPR_BINARY_ISLESSGREATER]      = PREC_PRIMARY,
		[EXPR_BINARY_ISUNORDERED]        = PREC_PRIMARY
	};
	assert((unsigned)kind < (sizeof(prec)/sizeof(prec[0])));
	unsigned res = prec[kind];

	assert(res != PREC_BOTTOM);
	return res;
}

/**
 * Print a constant expression.
 *
 * @param cnst  the constant expression
 */
static void print_const(const const_expression_t *cnst)
{
	if (cnst->base.type == NULL)
		return;

	const type_t *const type = skip_typeref(cnst->base.type);

	if (is_type_atomic(type, ATOMIC_TYPE_BOOL)) {
		fputs(cnst->v.int_value ? "true" : "false", out);
	} else if (is_type_integer(type)) {
		fprintf(out, "%lld", cnst->v.int_value);
	} else if (is_type_float(type)) {
		long double const val = cnst->v.float_value;
#ifdef _WIN32
		/* ARG, no way to print long double */
		fprintf(out, "%.20g", (double)val);
#else
		fprintf(out, "%.20Lg", val);
#endif
		if (isfinite(val) && truncl(val) == val)
			fputs(".0", out);
	} else {
		panic("unknown constant");
	}

	char const* suffix;
	switch (type->atomic.akind) {
		case ATOMIC_TYPE_UINT:        suffix = "U";   break;
		case ATOMIC_TYPE_LONG:        suffix = "L";   break;
		case ATOMIC_TYPE_ULONG:       suffix = "UL";  break;
		case ATOMIC_TYPE_LONGLONG:    suffix = "LL";  break;
		case ATOMIC_TYPE_ULONGLONG:   suffix = "ULL"; break;
		case ATOMIC_TYPE_FLOAT:       suffix = "F";   break;
		case ATOMIC_TYPE_LONG_DOUBLE: suffix = "L";   break;

		default: return;
	}
	fputs(suffix, out);
}

/**
 * Print a quoted string constant.
 *
 * @param string  the string constant
 * @param border  the border char
 * @param skip    number of chars to skip at the end
 */
static void print_quoted_string(const string_t *const string, char border, int skip)
{
	fputc(border, out);
	const char *end = string->begin + string->size - skip;
	for (const char *c = string->begin; c != end; ++c) {
		if (*c == border) {
			fputc('\\', out);
		}
		switch (*c) {
		case '\\':  fputs("\\\\", out); break;
		case '\a':  fputs("\\a", out); break;
		case '\b':  fputs("\\b", out); break;
		case '\f':  fputs("\\f", out); break;
		case '\n':  fputs("\\n", out); break;
		case '\r':  fputs("\\r", out); break;
		case '\t':  fputs("\\t", out); break;
		case '\v':  fputs("\\v", out); break;
		case '\?':  fputs("\\?", out); break;
		case 27:
			if (c_mode & _GNUC) {
				fputs("\\e", out); break;
			}
			/* FALLTHROUGH */
		default:
			if (!isprint(*c)) {
				fprintf(out, "\\%03o", (unsigned)(unsigned char)*c);
				break;
			}
			fputc(*c, out);
			break;
		}
	}
	fputc(border, out);
}

/**
 * Prints a wide string literal expression.
 *
 * @param wstr    the wide string literal expression
 * @param border  the border char
 * @param skip    number of chars to skip at the end
 */
static void print_quoted_wide_string(const wide_string_t *const wstr,
                                     char border, int skip)
{
	fputc('L', out);
	fputc(border, out);
	const wchar_rep_t *end = wstr->begin + wstr->size - skip;
	for (const wchar_rep_t *c = wstr->begin; c != end; ++c) {
		switch (*c) {
			case L'\"':  fputs("\\\"", out); break;
			case L'\\':  fputs("\\\\", out); break;
			case L'\a':  fputs("\\a",  out); break;
			case L'\b':  fputs("\\b",  out); break;
			case L'\f':  fputs("\\f",  out); break;
			case L'\n':  fputs("\\n",  out); break;
			case L'\r':  fputs("\\r",  out); break;
			case L'\t':  fputs("\\t",  out); break;
			case L'\v':  fputs("\\v",  out); break;
			case L'\?':  fputs("\\?",  out); break;
			case 27:
				if (c_mode & _GNUC) {
					fputs("\\e", out); break;
				}
				/* FALLTHROUGH */
			default: {
				const unsigned tc = *c;
				if (tc < 0x80U) {
					if (!isprint(*c)) {
						fprintf(out, "\\%03o", (char)*c);
					} else {
						fputc(*c, out);
					}
				} else if (tc < 0x800) {
					fputc(0xC0 | (tc >> 6),   out);
					fputc(0x80 | (tc & 0x3F), out);
				} else if (tc < 0x10000) {
					fputc(0xE0 | ( tc >> 12),         out);
					fputc(0x80 | ((tc >>  6) & 0x3F), out);
					fputc(0x80 | ( tc        & 0x3F), out);
				} else {
					fputc(0xF0 | ( tc >> 18),         out);
					fputc(0x80 | ((tc >> 12) & 0x3F), out);
					fputc(0x80 | ((tc >>  6) & 0x3F), out);
					fputc(0x80 | ( tc        & 0x3F), out);
				}
			}
		}
	}
	fputc(border, out);
}

/**
 * Print a constant character expression.
 *
 * @param cnst  the constant character expression
 */
static void print_character_constant(const const_expression_t *cnst)
{
	print_quoted_string(&cnst->v.character, '\'', 0);
}

static void print_wide_character_constant(const const_expression_t *cnst)
{
	print_quoted_wide_string(&cnst->v.wide_character, '\'', 0);
}

/**
 * Prints a string literal expression.
 *
 * @param string_literal  the string literal expression
 */
static void print_string_literal(
		const string_literal_expression_t *string_literal)
{
	print_quoted_string(&string_literal->value, '"', 1);
}

/**
 * Prints a predefined symbol.
 */
static void print_funcname(const funcname_expression_t *funcname)
{
	const char *s = "";
	switch (funcname->kind) {
	case FUNCNAME_FUNCTION:        s = (c_mode & _C99) ? "__func__" : "__FUNCTION__"; break;
	case FUNCNAME_PRETTY_FUNCTION: s = "__PRETTY_FUNCTION__"; break;
	case FUNCNAME_FUNCSIG:         s = "__FUNCSIG__"; break;
	case FUNCNAME_FUNCDNAME:       s = "__FUNCDNAME__"; break;
	}
	fputs(s, out);
}

static void print_wide_string_literal(
	const wide_string_literal_expression_t *const wstr)
{
	print_quoted_wide_string(&wstr->value, '"', 1);
}

static void print_compound_literal(
		const compound_literal_expression_t *expression)
{
	fputc('(', out);
	print_type(expression->type);
	fputc(')', out);
	print_initializer(expression->initializer);
}

static void print_assignment_expression(const expression_t *const expr)
{
	print_expression_prec(expr, PREC_ASSIGNMENT);
}

/**
 * Prints a call expression.
 *
 * @param call  the call expression
 */
static void print_call_expression(const call_expression_t *call)
{
	unsigned prec = get_expression_precedence(call->base.kind);
	print_expression_prec(call->function, prec);
	fputc('(', out);
	call_argument_t *argument = call->arguments;
	int              first    = 1;
	while (argument != NULL) {
		if (!first) {
			fputs(", ", out);
		} else {
			first = 0;
		}
		print_assignment_expression(argument->expression);

		argument = argument->next;
	}
	fputc(')', out);
}

/**
 * Prints a binary expression.
 *
 * @param binexpr   the binary expression
 */
static void print_binary_expression(const binary_expression_t *binexpr)
{
	unsigned prec = get_expression_precedence(binexpr->base.kind);
	int      r2l  = right_to_left(prec);

	print_expression_prec(binexpr->left, prec + r2l);
	char const* op;
	switch (binexpr->base.kind) {
	case EXPR_BINARY_COMMA:              op = ", ";    break;
	case EXPR_BINARY_ASSIGN:             op = " = ";   break;
	case EXPR_BINARY_ADD:                op = " + ";   break;
	case EXPR_BINARY_SUB:                op = " - ";   break;
	case EXPR_BINARY_MUL:                op = " * ";   break;
	case EXPR_BINARY_MOD:                op = " % ";   break;
	case EXPR_BINARY_DIV:                op = " / ";   break;
	case EXPR_BINARY_BITWISE_OR:         op = " | ";   break;
	case EXPR_BINARY_BITWISE_AND:        op = " & ";   break;
	case EXPR_BINARY_BITWISE_XOR:        op = " ^ ";   break;
	case EXPR_BINARY_LOGICAL_OR:         op = " || ";  break;
	case EXPR_BINARY_LOGICAL_AND:        op = " && ";  break;
	case EXPR_BINARY_NOTEQUAL:           op = " != ";  break;
	case EXPR_BINARY_EQUAL:              op = " == ";  break;
	case EXPR_BINARY_LESS:               op = " < ";   break;
	case EXPR_BINARY_LESSEQUAL:          op = " <= ";  break;
	case EXPR_BINARY_GREATER:            op = " > ";   break;
	case EXPR_BINARY_GREATEREQUAL:       op = " >= ";  break;
	case EXPR_BINARY_SHIFTLEFT:          op = " << ";  break;
	case EXPR_BINARY_SHIFTRIGHT:         op = " >> ";  break;

	case EXPR_BINARY_ADD_ASSIGN:         op = " += ";  break;
	case EXPR_BINARY_SUB_ASSIGN:         op = " -= ";  break;
	case EXPR_BINARY_MUL_ASSIGN:         op = " *= ";  break;
	case EXPR_BINARY_MOD_ASSIGN:         op = " %= ";  break;
	case EXPR_BINARY_DIV_ASSIGN:         op = " /= ";  break;
	case EXPR_BINARY_BITWISE_OR_ASSIGN:  op = " |= ";  break;
	case EXPR_BINARY_BITWISE_AND_ASSIGN: op = " &= ";  break;
	case EXPR_BINARY_BITWISE_XOR_ASSIGN: op = " ^= ";  break;
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:   op = " <<= "; break;
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:  op = " >>= "; break;
	default: panic("invalid binexpression found");
	}
	fputs(op, out);
	print_expression_prec(binexpr->right, prec + 1 - r2l);
}

/**
 * Prints an unary expression.
 *
 * @param unexpr   the unary expression
 */
static void print_unary_expression(const unary_expression_t *unexpr)
{
	unsigned prec = get_expression_precedence(unexpr->base.kind);
	switch (unexpr->base.kind) {
	case EXPR_UNARY_NEGATE:           fputc('-',          out); break;
	case EXPR_UNARY_PLUS:             fputc('+',          out); break;
	case EXPR_UNARY_NOT:              fputc('!',          out); break;
	case EXPR_UNARY_BITWISE_NEGATE:   fputc('~',          out); break;
	case EXPR_UNARY_PREFIX_INCREMENT: fputs("++",         out); break;
	case EXPR_UNARY_PREFIX_DECREMENT: fputs("--",         out); break;
	case EXPR_UNARY_DEREFERENCE:      fputc('*',          out); break;
	case EXPR_UNARY_TAKE_ADDRESS:     fputc('&',          out); break;
	case EXPR_UNARY_DELETE:           fputs("delete ",    out); break;
	case EXPR_UNARY_DELETE_ARRAY:     fputs("delete [] ", out); break;

	case EXPR_UNARY_POSTFIX_INCREMENT:
		print_expression_prec(unexpr->value, prec);
		fputs("++", out);
		return;
	case EXPR_UNARY_POSTFIX_DECREMENT:
		print_expression_prec(unexpr->value, prec);
		fputs("--", out);
		return;
	case EXPR_UNARY_CAST_IMPLICIT:
	case EXPR_UNARY_CAST:
		fputc('(', out);
		print_type(unexpr->base.type);
		fputc(')', out);
		break;
	case EXPR_UNARY_ASSUME:
		fputs("__assume(", out);
		print_assignment_expression(unexpr->value);
		fputc(')', out);
		return;

	case EXPR_UNARY_THROW:
		if (unexpr->value == NULL) {
			fputs("throw", out);
			return;
		}
		fputs("throw ", out);
		break;

	default:
		panic("invalid unary expression found");
	}
	print_expression_prec(unexpr->value, prec);
}

/**
 * Prints a reference expression.
 *
 * @param ref   the reference expression
 */
static void print_reference_expression(const reference_expression_t *ref)
{
	fputs(ref->entity->base.symbol->string, out);
}

/**
 * Prints a label address expression.
 *
 * @param ref   the reference expression
 */
static void print_label_address_expression(const label_address_expression_t *le)
{
	fprintf(out, "&&%s", le->label->base.symbol->string);
}

/**
 * Prints an array expression.
 *
 * @param expression   the array expression
 */
static void print_array_expression(const array_access_expression_t *expression)
{
	unsigned prec = get_expression_precedence(expression->base.kind);
	if (!expression->flipped) {
		print_expression_prec(expression->array_ref, prec);
		fputc('[', out);
		print_expression(expression->index);
		fputc(']', out);
	} else {
		print_expression_prec(expression->index, prec);
		fputc('[', out);
		print_expression(expression->array_ref);
		fputc(']', out);
	}
}

/**
 * Prints a typeproperty expression (sizeof or __alignof__).
 *
 * @param expression   the type property expression
 */
static void print_typeprop_expression(const typeprop_expression_t *expression)
{
	if (expression->base.kind == EXPR_SIZEOF) {
		fputs("sizeof", out);
	} else {
		assert(expression->base.kind == EXPR_ALIGNOF);
		fputs("__alignof__", out);
	}
	if (expression->tp_expression != NULL) {
		/* always print the '()' here, sizeof x is right but unusual */
		fputc('(', out);
		print_expression(expression->tp_expression);
		fputc(')', out);
	} else {
		fputc('(', out);
		print_type(expression->type);
		fputc(')', out);
	}
}

/**
 * Prints an builtin symbol.
 *
 * @param expression   the builtin symbol expression
 */
static void print_builtin_symbol(const builtin_symbol_expression_t *expression)
{
	fputs(expression->symbol->string, out);
}

/**
 * Prints a builtin constant expression.
 *
 * @param expression   the builtin constant expression
 */
static void print_builtin_constant(const builtin_constant_expression_t *expression)
{
	fputs("__builtin_constant_p(", out);
	print_assignment_expression(expression->value);
	fputc(')', out);
}

/**
 * Prints a builtin prefetch expression.
 *
 * @param expression   the builtin prefetch expression
 */
static void print_builtin_prefetch(const builtin_prefetch_expression_t *expression)
{
	fputs("__builtin_prefetch(", out);
	print_assignment_expression(expression->adr);
	if (expression->rw) {
		fputc(',', out);
		print_assignment_expression(expression->rw);
	}
	if (expression->locality) {
		fputc(',', out);
		print_assignment_expression(expression->locality);
	}
	fputc(')', out);
}

/**
 * Prints a conditional expression.
 *
 * @param expression   the conditional expression
 */
static void print_conditional(const conditional_expression_t *expression)
{
	print_expression_prec(expression->condition, PREC_LOGICAL_OR);
	fputs(" ? ", out);
	if (expression->true_expression != NULL) {
		print_expression_prec(expression->true_expression, PREC_EXPRESSION);
		fputs(" : ", out);
	} else {
		fputs(": ", out);
	}
	precedence_t prec = c_mode & _CXX ? PREC_ASSIGNMENT : PREC_CONDITIONAL;
	print_expression_prec(expression->false_expression, prec);
}

/**
 * Prints a va_start expression.
 *
 * @param expression   the va_start expression
 */
static void print_va_start(const va_start_expression_t *const expression)
{
	fputs("__builtin_va_start(", out);
	print_assignment_expression(expression->ap);
	fputs(", ", out);
	fputs(expression->parameter->base.base.symbol->string, out);
	fputc(')', out);
}

/**
 * Prints a va_arg expression.
 *
 * @param expression   the va_arg expression
 */
static void print_va_arg(const va_arg_expression_t *expression)
{
	fputs("__builtin_va_arg(", out);
	print_assignment_expression(expression->ap);
	fputs(", ", out);
	print_type(expression->base.type);
	fputc(')', out);
}

/**
 * Prints a select expression (. or ->).
 *
 * @param expression   the select expression
 */
static void print_select(const select_expression_t *expression)
{
	unsigned prec = get_expression_precedence(expression->base.kind);
	print_expression_prec(expression->compound, prec);
	if (is_type_pointer(skip_typeref(expression->compound->base.type))) {
		fputs("->", out);
	} else {
		fputc('.', out);
	}
	fputs(expression->compound_entry->base.symbol->string, out);
}

/**
 * Prints a type classify expression.
 *
 * @param expr   the type classify expression
 */
static void print_classify_type_expression(
	const classify_type_expression_t *const expr)
{
	fputs("__builtin_classify_type(", out);
	print_assignment_expression(expr->type_expression);
	fputc(')', out);
}

/**
 * Prints a designator.
 *
 * @param designator  the designator
 */
static void print_designator(const designator_t *designator)
{
	for ( ; designator != NULL; designator = designator->next) {
		if (designator->symbol == NULL) {
			fputc('[', out);
			print_expression(designator->array_index);
			fputc(']', out);
		} else {
			fputc('.', out);
			fputs(designator->symbol->string, out);
		}
	}
}

/**
 * Prints an offsetof expression.
 *
 * @param expression   the offset expression
 */
static void print_offsetof_expression(const offsetof_expression_t *expression)
{
	fputs("__builtin_offsetof", out);
	fputc('(', out);
	print_type(expression->type);
	fputc(',', out);
	print_designator(expression->designator);
	fputc(')', out);
}

/**
 * Prints a statement expression.
 *
 * @param expression   the statement expression
 */
static void print_statement_expression(const statement_expression_t *expression)
{
	fputc('(', out);
	print_statement(expression->statement);
	fputc(')', out);
}

/**
 * Prints an expression with parenthesis if needed.
 *
 * @param expression  the expression to print
 * @param top_prec    the precedence of the user of this expression.
 */
static void print_expression_prec(const expression_t *expression, unsigned top_prec)
{
	if (expression->kind == EXPR_UNARY_CAST_IMPLICIT && !print_implicit_casts) {
		expression = expression->unary.value;
	}

	bool parenthesized =
		expression->base.parenthesized                 ||
		(print_parenthesis && top_prec != PREC_BOTTOM) ||
		top_prec > get_expression_precedence(expression->base.kind);

	if (parenthesized)
		fputc('(', out);
	switch (expression->kind) {
	case EXPR_UNKNOWN:
	case EXPR_INVALID:
		fputs("$invalid expression$", out);
		break;
	case EXPR_CHARACTER_CONSTANT:
		print_character_constant(&expression->conste);
		break;
	case EXPR_WIDE_CHARACTER_CONSTANT:
		print_wide_character_constant(&expression->conste);
		break;
	case EXPR_CONST:
		print_const(&expression->conste);
		break;
	case EXPR_FUNCNAME:
		print_funcname(&expression->funcname);
		break;
	case EXPR_STRING_LITERAL:
		print_string_literal(&expression->string);
		break;
	case EXPR_WIDE_STRING_LITERAL:
		print_wide_string_literal(&expression->wide_string);
		break;
	case EXPR_COMPOUND_LITERAL:
		print_compound_literal(&expression->compound_literal);
		break;
	case EXPR_CALL:
		print_call_expression(&expression->call);
		break;
	EXPR_BINARY_CASES
		print_binary_expression(&expression->binary);
		break;
	case EXPR_REFERENCE:
	case EXPR_REFERENCE_ENUM_VALUE:
		print_reference_expression(&expression->reference);
		break;
	case EXPR_ARRAY_ACCESS:
		print_array_expression(&expression->array_access);
		break;
	case EXPR_LABEL_ADDRESS:
		print_label_address_expression(&expression->label_address);
		break;
	EXPR_UNARY_CASES
		print_unary_expression(&expression->unary);
		break;
	case EXPR_SIZEOF:
	case EXPR_ALIGNOF:
		print_typeprop_expression(&expression->typeprop);
		break;
	case EXPR_BUILTIN_SYMBOL:
		print_builtin_symbol(&expression->builtin_symbol);
		break;
	case EXPR_BUILTIN_CONSTANT_P:
		print_builtin_constant(&expression->builtin_constant);
		break;
	case EXPR_BUILTIN_PREFETCH:
		print_builtin_prefetch(&expression->builtin_prefetch);
		break;
	case EXPR_CONDITIONAL:
		print_conditional(&expression->conditional);
		break;
	case EXPR_VA_START:
		print_va_start(&expression->va_starte);
		break;
	case EXPR_VA_ARG:
		print_va_arg(&expression->va_arge);
		break;
	case EXPR_SELECT:
		print_select(&expression->select);
		break;
	case EXPR_CLASSIFY_TYPE:
		print_classify_type_expression(&expression->classify_type);
		break;
	case EXPR_OFFSETOF:
		print_offsetof_expression(&expression->offsetofe);
		break;
	case EXPR_STATEMENT:
		print_statement_expression(&expression->statement);
		break;

	default:
		/* TODO */
		fprintf(out, "some expression of type %d", (int)expression->kind);
		break;
	}
	if (parenthesized)
		fputc(')', out);
}

/**
 * Print an compound statement.
 *
 * @param block  the compound statement
 */
static void print_compound_statement(const compound_statement_t *block)
{
	fputs("{\n", out);
	++indent;

	statement_t *statement = block->statements;
	while (statement != NULL) {
		if (statement->base.kind == STATEMENT_CASE_LABEL)
			--indent;
		if (statement->kind != STATEMENT_LABEL)
			print_indent();
		print_statement(statement);

		statement = statement->base.next;
	}
	--indent;
	print_indent();
	fputs("}\n", out);
}

/**
 * Print a return statement.
 *
 * @param statement  the return statement
 */
static void print_return_statement(const return_statement_t *statement)
{
	expression_t const *const val = statement->value;
	if (val != NULL) {
		fputs("return ", out);
		print_expression(val);
		fputs(";\n", out);
	} else {
		fputs("return;\n", out);
	}
}

/**
 * Print an expression statement.
 *
 * @param statement  the expression statement
 */
static void print_expression_statement(const expression_statement_t *statement)
{
	print_expression(statement->expression);
	fputs(";\n", out);
}

/**
 * Print a goto statement.
 *
 * @param statement  the goto statement
 */
static void print_goto_statement(const goto_statement_t *statement)
{
	fputs("goto ", out);
	if (statement->expression != NULL) {
		fputc('*', out);
		print_expression(statement->expression);
	} else {
		fputs(statement->label->base.symbol->string, out);
	}
	fputs(";\n", out);
}

/**
 * Print a label statement.
 *
 * @param statement  the label statement
 */
static void print_label_statement(const label_statement_t *statement)
{
	fprintf(out, "%s:\n", statement->label->base.symbol->string);
	print_indent();
	print_statement(statement->statement);
}

/**
 * Print an if statement.
 *
 * @param statement  the if statement
 */
static void print_if_statement(const if_statement_t *statement)
{
	fputs("if (", out);
	print_expression(statement->condition);
	fputs(") ", out);
	print_statement(statement->true_statement);

	if (statement->false_statement != NULL) {
		print_indent();
		fputs("else ", out);
		print_statement(statement->false_statement);
	}
}

/**
 * Print a switch statement.
 *
 * @param statement  the switch statement
 */
static void print_switch_statement(const switch_statement_t *statement)
{
	fputs("switch (", out);
	print_expression(statement->expression);
	fputs(") ", out);
	print_statement(statement->body);
}

/**
 * Print a case label (including the default label).
 *
 * @param statement  the case label statement
 */
static void print_case_label(const case_label_statement_t *statement)
{
	if (statement->expression == NULL) {
		fputs("default:\n", out);
	} else {
		fputs("case ", out);
		print_expression(statement->expression);
		if (statement->end_range != NULL) {
			fputs(" ... ", out);
			print_expression(statement->end_range);
		}
		fputs(":\n", out);
	}
	++indent;
	if (statement->statement != NULL) {
		if (statement->statement->base.kind == STATEMENT_CASE_LABEL) {
			--indent;
		}
		print_indent();
		print_statement(statement->statement);
	}
}

static void print_local_label(const local_label_statement_t *statement)
{
	fputs("__label__ ", out);

	bool      first  = true;
	entity_t *entity = statement->labels_begin;
	for (;
		entity != statement->labels_end->base.next;
		entity = entity->base.next) {
		if (!first) {
			fputs(", ", out);
		} else {
			first = false;
		}
		fputs(entity->base.symbol->string, out);
	}
	fputs(";\n", out);
}

static void print_typedef(const entity_t *entity)
{
	fputs("typedef ", out);
	print_type_ext(entity->typedefe.type, entity->base.symbol, NULL);
	fputc(';', out);
}

/**
 * returns true if the entity is a compiler generated one and has no real
 * correspondenc in the source file
 */
static bool is_generated_entity(const entity_t *entity)
{
	if (entity->kind == ENTITY_TYPEDEF)
		return entity->typedefe.builtin;

	if (is_declaration(entity))
		return entity->declaration.implicit;

	return false;
}

/**
 * Print a declaration statement.
 *
 * @param statement   the statement
 */
static void print_declaration_statement(
		const declaration_statement_t *statement)
{
	bool first = true;
	entity_t *entity = statement->declarations_begin;
	if (entity == NULL) {
		fputs("/* empty declaration statement */\n", out);
		return;
	}

	entity_t *const end = statement->declarations_end->base.next;
	for (; entity != end; entity = entity->base.next) {
		if (!is_declaration(entity) && entity->kind != ENTITY_TYPEDEF)
			continue;
		if (is_generated_entity(entity))
			continue;

		if (!first) {
			print_indent();
		} else {
			first = false;
		}

		if (entity->kind == ENTITY_TYPEDEF) {
			print_typedef(entity);
		} else {
			assert(is_declaration(entity));
			print_declaration(entity);
		}

		fputc('\n', out);
	}
}

/**
 * Print a while statement.
 *
 * @param statement   the statement
 */
static void print_while_statement(const while_statement_t *statement)
{
	fputs("while (", out);
	print_expression(statement->condition);
	fputs(") ", out);
	print_statement(statement->body);
}

/**
 * Print a do-while statement.
 *
 * @param statement   the statement
 */
static void print_do_while_statement(const do_while_statement_t *statement)
{
	fputs("do ", out);
	print_statement(statement->body);
	print_indent();
	fputs("while (", out);
	print_expression(statement->condition);
	fputs(");\n", out);
}

/**
 * Print a for statement.
 *
 * @param statement   the statement
 */
static void print_for_statement(const for_statement_t *statement)
{
	fputs("for (", out);
	entity_t *entity = statement->scope.entities;
	while (entity != NULL && is_generated_entity(entity))
		entity = entity->base.next;

	if (entity != NULL) {
		assert(statement->initialisation == NULL);
		assert(is_declaration(entity));
		print_declaration(entity);
		if (entity->base.next != NULL) {
			panic("multiple declarations in for statement not supported yet");
		}
	} else {
		if (statement->initialisation) {
			print_expression(statement->initialisation);
		}
		fputc(';', out);
	}
	if (statement->condition != NULL) {
		fputc(' ', out);
		print_expression(statement->condition);
	}
	fputc(';', out);
	if (statement->step != NULL) {
		fputc(' ', out);
		print_expression(statement->step);
	}
	fputs(") ", out);
	print_statement(statement->body);
}

/**
 * Print assembler arguments.
 *
 * @param arguments   the arguments
 */
static void print_asm_arguments(asm_argument_t *arguments)
{
	asm_argument_t *argument = arguments;
	for (; argument != NULL; argument = argument->next) {
		if (argument != arguments)
			fputs(", ", out);

		if (argument->symbol) {
			fprintf(out, "[%s] ", argument->symbol->string);
		}
		print_quoted_string(&argument->constraints, '"', 1);
		fputs(" (", out);
		print_expression(argument->expression);
		fputc(')', out);
	}
}

/**
 * Print assembler clobbers.
 *
 * @param clobbers   the clobbers
 */
static void print_asm_clobbers(asm_clobber_t *clobbers)
{
	asm_clobber_t *clobber = clobbers;
	for (; clobber != NULL; clobber = clobber->next) {
		if (clobber != clobbers)
			fputs(", ", out);

		print_quoted_string(&clobber->clobber, '"', 1);
	}
}

/**
 * Print an assembler statement.
 *
 * @param statement   the statement
 */
static void print_asm_statement(const asm_statement_t *statement)
{
	fputs("asm ", out);
	if (statement->is_volatile) {
		fputs("volatile ", out);
	}
	fputc('(', out);
	print_quoted_string(&statement->asm_text, '"', 1);
	if (statement->outputs  == NULL &&
	    statement->inputs   == NULL &&
	    statement->clobbers == NULL)
		goto end_of_print_asm_statement;

	fputs(" : ", out);
	print_asm_arguments(statement->outputs);
	if (statement->inputs == NULL && statement->clobbers == NULL)
		goto end_of_print_asm_statement;

	fputs(" : ", out);
	print_asm_arguments(statement->inputs);
	if (statement->clobbers == NULL)
		goto end_of_print_asm_statement;

	fputs(" : ", out);
	print_asm_clobbers(statement->clobbers);

end_of_print_asm_statement:
	fputs(");\n", out);
}

/**
 * Print a microsoft __try statement.
 *
 * @param statement   the statement
 */
static void print_ms_try_statement(const ms_try_statement_t *statement)
{
	fputs("__try ", out);
	print_statement(statement->try_statement);
	print_indent();
	if (statement->except_expression != NULL) {
		fputs("__except(", out);
		print_expression(statement->except_expression);
		fputs(") ", out);
	} else {
		fputs("__finally ", out);
	}
	print_statement(statement->final_statement);
}

/**
 * Print a microsoft __leave statement.
 *
 * @param statement   the statement
 */
static void print_leave_statement(const leave_statement_t *statement)
{
	(void)statement;
	fputs("__leave;\n", out);
}

/**
 * Print a statement.
 *
 * @param statement   the statement
 */
void print_statement(const statement_t *statement)
{
	switch (statement->kind) {
	case STATEMENT_EMPTY:
		fputs(";\n", out);
		break;
	case STATEMENT_COMPOUND:
		print_compound_statement(&statement->compound);
		break;
	case STATEMENT_RETURN:
		print_return_statement(&statement->returns);
		break;
	case STATEMENT_EXPRESSION:
		print_expression_statement(&statement->expression);
		break;
	case STATEMENT_LABEL:
		print_label_statement(&statement->label);
		break;
	case STATEMENT_LOCAL_LABEL:
		print_local_label(&statement->local_label);
		break;
	case STATEMENT_GOTO:
		print_goto_statement(&statement->gotos);
		break;
	case STATEMENT_CONTINUE:
		fputs("continue;\n", out);
		break;
	case STATEMENT_BREAK:
		fputs("break;\n", out);
		break;
	case STATEMENT_IF:
		print_if_statement(&statement->ifs);
		break;
	case STATEMENT_SWITCH:
		print_switch_statement(&statement->switchs);
		break;
	case STATEMENT_CASE_LABEL:
		print_case_label(&statement->case_label);
		break;
	case STATEMENT_DECLARATION:
		print_declaration_statement(&statement->declaration);
		break;
	case STATEMENT_WHILE:
		print_while_statement(&statement->whiles);
		break;
	case STATEMENT_DO_WHILE:
		print_do_while_statement(&statement->do_while);
		break;
	case STATEMENT_FOR:
		print_for_statement(&statement->fors);
		break;
	case STATEMENT_ASM:
		print_asm_statement(&statement->asms);
		break;
	case STATEMENT_MS_TRY:
		print_ms_try_statement(&statement->ms_try);
		break;
	case STATEMENT_LEAVE:
		print_leave_statement(&statement->leave);
		break;
	case STATEMENT_INVALID:
		fputs("$invalid statement$\n", out);
		break;
	}
}

/**
 * Print a storage class.
 *
 * @param storage_class   the storage class
 */
static void print_storage_class(storage_class_tag_t storage_class)
{
	const char *text;
	switch (storage_class) {
	case STORAGE_CLASS_NONE:     return;
	case STORAGE_CLASS_TYPEDEF:  text = "typedef ";  break;
	case STORAGE_CLASS_EXTERN:   text = "extern ";   break;
	case STORAGE_CLASS_STATIC:   text = "static ";   break;
	case STORAGE_CLASS_AUTO:     text = "auto ";     break;
	case STORAGE_CLASS_REGISTER: text = "register "; break;
	}
	fputs(text, out);
}

/**
 * Print an initializer.
 *
 * @param initializer  the initializer
 */
void print_initializer(const initializer_t *initializer)
{
	if (initializer == NULL) {
		fputs("{}", out);
		return;
	}

	switch (initializer->kind) {
	case INITIALIZER_VALUE: {
		const initializer_value_t *value = &initializer->value;
		print_assignment_expression(value->value);
		return;
	}
	case INITIALIZER_LIST: {
		assert(initializer->kind == INITIALIZER_LIST);
		fputs("{ ", out);
		const initializer_list_t *list = &initializer->list;

		for (size_t i = 0 ; i < list->len; ++i) {
			const initializer_t *sub_init = list->initializers[i];
			print_initializer(list->initializers[i]);
			if (i < list->len-1) {
				if (sub_init == NULL || sub_init->kind != INITIALIZER_DESIGNATOR)
					fputs(", ", out);
			}
		}
		fputs(" }", out);
		return;
	}
	case INITIALIZER_STRING:
		print_quoted_string(&initializer->string.string, '"', 1);
		return;
	case INITIALIZER_WIDE_STRING:
		print_quoted_wide_string(&initializer->wide_string.string, '"', 1);
		return;
	case INITIALIZER_DESIGNATOR:
		print_designator(initializer->designator.designator);
		fputs(" = ", out);
		return;
	}

	panic("invalid initializer kind found");
}

/**
 * Print microsoft extended declaration modifiers.
 */
static void print_ms_modifiers(const declaration_t *declaration)
{
	if ((c_mode & _MS) == 0)
		return;

	decl_modifiers_t modifiers = declaration->modifiers;

	bool        ds_shown = false;
	const char *next     = "(";

	if (declaration->base.kind == ENTITY_VARIABLE) {
		variable_t *variable = (variable_t*)declaration;
		if (variable->alignment != 0
				|| variable->get_property_sym != NULL
				|| variable->put_property_sym != NULL) {
			if (!ds_shown) {
				fputs("__declspec", out);
				ds_shown = true;
			}

			if (variable->alignment != 0) {
				fputs(next, out); next = ", "; fprintf(out, "align(%u)", variable->alignment);
			}
			if (variable->get_property_sym != NULL
					|| variable->put_property_sym != NULL) {
				char *comma = "";
				fputs(next, out); next = ", "; fputs("property(", out);
				if (variable->get_property_sym != NULL) {
					fprintf(out, "get=%s", variable->get_property_sym->string);
					comma = ", ";
				}
				if (variable->put_property_sym != NULL)
					fprintf(out, "%sput=%s", comma, variable->put_property_sym->string);
				fputc(')', out);
			}
		}
	}

	/* DM_FORCEINLINE handled outside. */
	if ((modifiers & ~DM_FORCEINLINE) != 0) {
		if (!ds_shown) {
			fputs("__declspec", out);
			ds_shown = true;
		}
		if (modifiers & DM_DLLIMPORT) {
			fputs(next, out); next = ", "; fputs("dllimport", out);
		}
		if (modifiers & DM_DLLEXPORT) {
			fputs(next, out); next = ", "; fputs("dllexport", out);
		}
		if (modifiers & DM_THREAD) {
			fputs(next, out); next = ", "; fputs("thread", out);
		}
		if (modifiers & DM_NAKED) {
			fputs(next, out); next = ", "; fputs("naked", out);
		}
		if (modifiers & DM_THREAD) {
			fputs(next, out); next = ", "; fputs("thread", out);
		}
		if (modifiers & DM_SELECTANY) {
			fputs(next, out); next = ", "; fputs("selectany", out);
		}
		if (modifiers & DM_NOTHROW) {
			fputs(next, out); next = ", "; fputs("nothrow", out);
		}
		if (modifiers & DM_NORETURN) {
			fputs(next, out); next = ", "; fputs("noreturn", out);
		}
		if (modifiers & DM_NOINLINE) {
			fputs(next, out); next = ", "; fputs("noinline", out);
		}
		if (modifiers & DM_DEPRECATED) {
			fputs(next, out); next = ", "; fputs("deprecated", out);
			if (declaration->deprecated_string != NULL)
				fprintf(out, "(\"%s\")",
				        declaration->deprecated_string);
		}
		if (modifiers & DM_RESTRICT) {
			fputs(next, out); next = ", "; fputs("restrict", out);
		}
		if (modifiers & DM_NOALIAS) {
			fputs(next, out); next = ", "; fputs("noalias", out);
		}
	}

	if (ds_shown)
		fputs(") ", out);
}

static void print_scope(const scope_t *scope)
{
	const entity_t *entity = scope->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		print_indent();
		print_entity(entity);
		fputs("\n", out);
	}
}

static void print_namespace(const namespace_t *namespace)
{
	fputs("namespace ", out);
	if (namespace->base.symbol != NULL) {
		fputs(namespace->base.symbol->string, out);
		fputc(' ', out);
	}

	fputs("{\n", out);
	++indent;

	print_scope(&namespace->members);

	--indent;
	print_indent();
	fputs("}\n", out);
}

/**
 * Print a variable or function declaration
 */
void print_declaration(const entity_t *entity)
{
	assert(is_declaration(entity));
	const declaration_t *declaration = &entity->declaration;

	print_storage_class((storage_class_tag_t)declaration->declared_storage_class);
	if (entity->kind == ENTITY_FUNCTION) {
		function_t *function = (function_t*)declaration;
		if (function->is_inline) {
			if (declaration->modifiers & DM_FORCEINLINE) {
				fputs("__forceinline ", out);
			} else if (declaration->modifiers & DM_MICROSOFT_INLINE) {
				fputs("__inline ", out);
			} else {
				fputs("inline ", out);
			}
		}
	}
	print_ms_modifiers(declaration);
	switch (entity->kind) {
		case ENTITY_FUNCTION:
			print_type_ext(entity->declaration.type, entity->base.symbol,
					&entity->function.parameters);

			if (entity->function.statement != NULL) {
				fputc('\n', out);
				print_indent();
				print_statement(entity->function.statement);
				return;
			}
			break;

		case ENTITY_VARIABLE:
			if (entity->variable.thread_local)
				fputs("__thread ", out);
			print_type_ext(declaration->type, declaration->base.symbol, NULL);
			if (entity->variable.initializer != NULL) {
				fputs(" = ", out);
				print_initializer(entity->variable.initializer);
			}
			break;

		default:
			print_type_ext(declaration->type, declaration->base.symbol, NULL);
			break;
	}
	fputc(';', out);
}

/**
 * Prints an expression.
 *
 * @param expression  the expression
 */
void print_expression(const expression_t *expression)
{
	print_expression_prec(expression, PREC_BOTTOM);
}

/**
 * Print a declaration.
 *
 * @param declaration  the declaration
 */
void print_entity(const entity_t *entity)
{
	if (entity->base.namespc != NAMESPACE_NORMAL && entity->base.symbol == NULL)
		return;

	switch ((entity_kind_tag_t)entity->kind) {
	case ENTITY_VARIABLE:
	case ENTITY_PARAMETER:
	case ENTITY_COMPOUND_MEMBER:
		print_declaration(entity);
		return;
	case ENTITY_FUNCTION:
		print_declaration(entity);
		return;
	case ENTITY_TYPEDEF:
		print_typedef(entity);
		return;
	case ENTITY_STRUCT:
		fputs("struct ", out);
		fputs(entity->base.symbol->string, out);
		if (entity->structe.complete) {
			fputc(' ', out);
			print_compound_definition(&entity->structe);
		}
		fputc(';', out);
		return;
	case ENTITY_UNION:
		fputs("union ", out);
		fputs(entity->base.symbol->string, out);
		if (entity->unione.complete) {
			fputc(' ', out);
			print_compound_definition(&entity->unione);
		}
		fputc(';', out);
		return;
	case ENTITY_ENUM:
		fputs("enum ", out);
		fputs(entity->base.symbol->string, out);
		fputc(' ', out);
		print_enum_definition(&entity->enume);
		fputc(';', out);
		return;
	case ENTITY_NAMESPACE:
		print_namespace(&entity->namespacee);
		return;
	case ENTITY_LABEL:
	case ENTITY_ENUM_VALUE:
	case ENTITY_LOCAL_LABEL:
		panic("print_entity used on unexpected entity type");
	case ENTITY_INVALID:
		break;
	}
	panic("Invalid entity type encountered");
}

/**
 * Print the AST of a translation unit.
 *
 * @param unit   the translation unit
 */
void print_ast(const translation_unit_t *unit)
{
	inc_type_visited();

	entity_t *entity = unit->scope.entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		if (entity->kind == ENTITY_ENUM_VALUE)
			continue;
		if (entity->base.namespc != NAMESPACE_NORMAL
				&& entity->base.symbol == NULL)
			continue;
		if (is_generated_entity(entity))
			continue;

		print_indent();
		print_entity(entity);
		fputc('\n', out);
	}
}

bool is_constant_initializer(const initializer_t *initializer)
{
	switch (initializer->kind) {
	case INITIALIZER_STRING:
	case INITIALIZER_WIDE_STRING:
	case INITIALIZER_DESIGNATOR:
		return true;

	case INITIALIZER_VALUE:
		return is_constant_expression(initializer->value.value);

	case INITIALIZER_LIST:
		for (size_t i = 0; i < initializer->list.len; ++i) {
			initializer_t *sub_initializer = initializer->list.initializers[i];
			if (!is_constant_initializer(sub_initializer))
				return false;
		}
		return true;
	}
	panic("invalid initializer kind found");
}

static bool is_object_with_linker_constant_address(const expression_t *expression)
{
	switch (expression->kind) {
	case EXPR_UNARY_DEREFERENCE:
		return is_address_constant(expression->unary.value);

	case EXPR_SELECT: {
		type_t *base_type = skip_typeref(expression->select.compound->base.type);
		if (is_type_pointer(base_type)) {
			/* it's a -> */
			return is_address_constant(expression->select.compound);
		} else {
			return is_object_with_linker_constant_address(expression->select.compound);
		}
	}

	case EXPR_ARRAY_ACCESS:
		return is_constant_expression(expression->array_access.index)
			&& is_address_constant(expression->array_access.array_ref);

	case EXPR_REFERENCE: {
		entity_t *entity = expression->reference.entity;
		if (is_declaration(entity)) {
			switch ((storage_class_tag_t)entity->declaration.storage_class) {
			case STORAGE_CLASS_NONE:
			case STORAGE_CLASS_EXTERN:
			case STORAGE_CLASS_STATIC:
				return
					entity->kind != ENTITY_VARIABLE ||
					!entity->variable.thread_local;

			case STORAGE_CLASS_REGISTER:
			case STORAGE_CLASS_TYPEDEF:
			case STORAGE_CLASS_AUTO:
				break;
			}
		}
		return false;
	}

	default:
		return false;
	}
}

bool is_address_constant(const expression_t *expression)
{
	switch (expression->kind) {
	case EXPR_UNARY_TAKE_ADDRESS:
		return is_object_with_linker_constant_address(expression->unary.value);

	case EXPR_UNARY_DEREFERENCE: {
		type_t *real_type
			= revert_automatic_type_conversion(expression->unary.value);
		/* dereferencing a function is a NOP */
		if (is_type_function(real_type)) {
			return is_address_constant(expression->unary.value);
		}
		/* FALLTHROUGH */
	}

	case EXPR_UNARY_CAST: {
		type_t *dest = skip_typeref(expression->base.type);
		if (!is_type_pointer(dest) && (
		    	dest->kind != TYPE_ATOMIC                                               ||
		    	!(get_atomic_type_flags(dest->atomic.akind) & ATOMIC_TYPE_FLAG_INTEGER) ||
		    	get_atomic_type_size(dest->atomic.akind) < get_atomic_type_size(get_intptr_kind())
		    ))
			return false;

		return (is_constant_expression(expression->unary.value)
			|| is_address_constant(expression->unary.value));
	}

	case EXPR_BINARY_ADD:
	case EXPR_BINARY_SUB: {
		expression_t *left  = expression->binary.left;
		expression_t *right = expression->binary.right;

		if (is_type_pointer(skip_typeref(left->base.type))) {
			return is_address_constant(left) && is_constant_expression(right);
		} else if (is_type_pointer(skip_typeref(right->base.type))) {
			return is_constant_expression(left)	&& is_address_constant(right);
		}

		return false;
	}

	case EXPR_REFERENCE: {
		entity_t *entity = expression->reference.entity;
		if (!is_declaration(entity))
			return false;

		type_t *type = skip_typeref(entity->declaration.type);
		if (is_type_function(type))
			return true;
		if (is_type_array(type)) {
			return is_object_with_linker_constant_address(expression);
		}
		/* Prevent stray errors */
		if (!is_type_valid(type))
			return true;
		return false;
	}

	case EXPR_ARRAY_ACCESS: {
		type_t *const type =
			skip_typeref(revert_automatic_type_conversion(expression));
		return
			is_type_array(type)                                    &&
			is_constant_expression(expression->array_access.index) &&
			is_address_constant(expression->array_access.array_ref);
	}

	default:
		return false;
	}
}

static bool is_builtin_const_call(const expression_t *expression)
{
	expression_t *function = expression->call.function;
	if (function->kind != EXPR_BUILTIN_SYMBOL) {
		return false;
	}

	symbol_t *symbol = function->builtin_symbol.symbol;

	switch (symbol->ID) {
	case T___builtin_huge_val:
	case T___builtin_inf:
	case T___builtin_inff:
	case T___builtin_infl:
	case T___builtin_nan:
	case T___builtin_nanf:
	case T___builtin_nanl:
		return true;
	}

	return false;
}

static bool is_constant_pointer(const expression_t *expression)
{
	if (is_constant_expression(expression))
		return true;

	switch (expression->kind) {
	case EXPR_UNARY_CAST:
		return is_constant_pointer(expression->unary.value);
	default:
		return false;
	}
}

static bool is_object_with_constant_address(const expression_t *expression)
{
	switch (expression->kind) {
	case EXPR_SELECT: {
		expression_t *compound      = expression->select.compound;
		type_t       *compound_type = compound->base.type;
		compound_type = skip_typeref(compound_type);
		if (is_type_pointer(compound_type)) {
			return is_constant_pointer(compound);
		} else {
			return is_object_with_constant_address(compound);
		}
	}

	case EXPR_ARRAY_ACCESS: {
		array_access_expression_t const* const array_access =
			&expression->array_access;
		return
			is_constant_expression(array_access->index) && (
				is_object_with_constant_address(array_access->array_ref) ||
				is_constant_pointer(array_access->array_ref)
			);
	}

	case EXPR_UNARY_DEREFERENCE:
		return is_constant_pointer(expression->unary.value);
	default:
		return false;
	}
}

bool is_constant_expression(const expression_t *expression)
{
	switch (expression->kind) {

	case EXPR_CONST:
	case EXPR_CHARACTER_CONSTANT:
	case EXPR_WIDE_CHARACTER_CONSTANT:
	case EXPR_STRING_LITERAL:
	case EXPR_WIDE_STRING_LITERAL:
	case EXPR_CLASSIFY_TYPE:
	case EXPR_FUNCNAME:
	case EXPR_OFFSETOF:
	case EXPR_ALIGNOF:
	case EXPR_BUILTIN_CONSTANT_P:
	case EXPR_LABEL_ADDRESS:
	case EXPR_REFERENCE_ENUM_VALUE:
		return true;

	case EXPR_SIZEOF: {
		type_t *type = expression->typeprop.type;
		if (type == NULL)
			type = expression->typeprop.tp_expression->base.type;

		type = skip_typeref(type);
		if (is_type_array(type) && type->array.is_vla)
			return false;
		return true;
	}

	case EXPR_BUILTIN_SYMBOL:
	case EXPR_BUILTIN_PREFETCH:
	case EXPR_SELECT:
	case EXPR_VA_START:
	case EXPR_VA_ARG:
	case EXPR_STATEMENT:
	case EXPR_REFERENCE:
	case EXPR_UNARY_POSTFIX_INCREMENT:
	case EXPR_UNARY_POSTFIX_DECREMENT:
	case EXPR_UNARY_PREFIX_INCREMENT:
	case EXPR_UNARY_PREFIX_DECREMENT:
	case EXPR_UNARY_ASSUME: /* has VOID type */
	case EXPR_UNARY_DEREFERENCE:
	case EXPR_UNARY_DELETE:
	case EXPR_UNARY_DELETE_ARRAY:
	case EXPR_UNARY_THROW:
	case EXPR_BINARY_ASSIGN:
	case EXPR_BINARY_MUL_ASSIGN:
	case EXPR_BINARY_DIV_ASSIGN:
	case EXPR_BINARY_MOD_ASSIGN:
	case EXPR_BINARY_ADD_ASSIGN:
	case EXPR_BINARY_SUB_ASSIGN:
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:
	case EXPR_BINARY_BITWISE_AND_ASSIGN:
	case EXPR_BINARY_BITWISE_XOR_ASSIGN:
	case EXPR_BINARY_BITWISE_OR_ASSIGN:
	case EXPR_BINARY_COMMA:
	case EXPR_ARRAY_ACCESS:
		return false;

	case EXPR_UNARY_TAKE_ADDRESS:
		return is_object_with_constant_address(expression->unary.value);

	case EXPR_CALL:
		return is_builtin_const_call(expression);

	case EXPR_UNARY_NEGATE:
	case EXPR_UNARY_PLUS:
	case EXPR_UNARY_BITWISE_NEGATE:
	case EXPR_UNARY_NOT:
		return is_constant_expression(expression->unary.value);

	case EXPR_UNARY_CAST:
	case EXPR_UNARY_CAST_IMPLICIT:
		return is_type_arithmetic(skip_typeref(expression->base.type))
			&& is_constant_expression(expression->unary.value);

	case EXPR_BINARY_ADD:
	case EXPR_BINARY_SUB:
	case EXPR_BINARY_MUL:
	case EXPR_BINARY_DIV:
	case EXPR_BINARY_MOD:
	case EXPR_BINARY_EQUAL:
	case EXPR_BINARY_NOTEQUAL:
	case EXPR_BINARY_LESS:
	case EXPR_BINARY_LESSEQUAL:
	case EXPR_BINARY_GREATER:
	case EXPR_BINARY_GREATEREQUAL:
	case EXPR_BINARY_BITWISE_AND:
	case EXPR_BINARY_BITWISE_OR:
	case EXPR_BINARY_BITWISE_XOR:
	case EXPR_BINARY_LOGICAL_AND:
	case EXPR_BINARY_LOGICAL_OR:
	case EXPR_BINARY_SHIFTLEFT:
	case EXPR_BINARY_SHIFTRIGHT:
	case EXPR_BINARY_ISGREATER:
	case EXPR_BINARY_ISGREATEREQUAL:
	case EXPR_BINARY_ISLESS:
	case EXPR_BINARY_ISLESSEQUAL:
	case EXPR_BINARY_ISLESSGREATER:
	case EXPR_BINARY_ISUNORDERED:
		return is_constant_expression(expression->binary.left)
			&& is_constant_expression(expression->binary.right);

	case EXPR_COMPOUND_LITERAL:
		return is_constant_initializer(expression->compound_literal.initializer);

	case EXPR_CONDITIONAL: {
		expression_t *condition = expression->conditional.condition;
		if (!is_constant_expression(condition))
			return false;

		long val = fold_constant(condition);
		if (val != 0)
			return is_constant_expression(expression->conditional.true_expression);
		else
			return is_constant_expression(expression->conditional.false_expression);
	}

	case EXPR_INVALID:
		return true;

	case EXPR_UNKNOWN:
		break;
	}
	panic("invalid expression found (is constant expression)");
}

/**
 * Initialize the AST construction.
 */
void init_ast(void)
{
	obstack_init(&ast_obstack);
}

/**
 * Free the AST.
 */
void exit_ast(void)
{
	obstack_free(&ast_obstack, NULL);
}

/**
 * Set the output stream for the AST printer.
 *
 * @param stream  the output stream
 */
void ast_set_output(FILE *stream)
{
	out = stream;
	type_set_output(stream);
}

/**
 * Allocate an AST object of the given size.
 *
 * @param size  the size of the object to allocate
 *
 * @return  A new allocated object in the AST memeory space.
 */
void *(allocate_ast)(size_t size)
{
	return _allocate_ast(size);
}
