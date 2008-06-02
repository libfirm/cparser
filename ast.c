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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

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
	for(int i = 0; i < indent; ++i)
		fprintf(out, "\t");
}

enum precedence_t {
	PREC_BOTTOM  =  0,
	PREC_COMMA   =  2, /* ,                                    left to right */
	PREC_ASSIGN  =  4, /* = += -= *= /= %= <<= >>= &= ^= |=    right to left */
	PREC_COND    =  6, /* ?:                                   right to left */
	PREC_LOG_OR  =  8, /* ||                                   left to right */
	PREC_LOG_AND = 10, /* &&                                   left to right */
	PREC_BIT_OR  = 12, /* |                                    left to right */
	PREC_BIT_XOR = 14, /* ^                                    left to right */
	PREC_BIT_AND = 16, /* &                                    left to right */
	PREC_EQ      = 18, /* == !=                                left to right */
	PREC_CMP     = 20, /* < <= > >=                            left to right */
	PREC_SHF     = 22, /* << >>                                left to right */
	PREC_PLUS    = 24, /* + -                                  left to right */
	PREC_MUL     = 26, /* * / %                                left to right */
	PREC_UNARY   = 28, /* ! ~ ++ -- + - (type) * & sizeof      right to left */
	PREC_ACCESS  = 30, /* () [] -> .                           left to right */
	PREC_PRIM    = 32, /* primary */
	PREC_TOP     = 34
};

/**
 * Returns 1 if a given precedence level has right-to-left
 * associativity, else -1.
 *
 * @param precedence   the operator precedence
 */
static int right_to_left(unsigned precedence) {
	return (precedence == PREC_ASSIGN || precedence == PREC_COND ||
		precedence == PREC_UNARY) ? 1 : -1;
}

/**
 * Return the precedence of an expression given by its kind.
 *
 * @param kind   the expression kind
 */
static unsigned get_expression_precedence(expression_kind_t kind)
{
	static const unsigned prec[] = {
		[EXPR_UNKNOWN]                   = PREC_PRIM,
		[EXPR_INVALID]                   = PREC_PRIM,
		[EXPR_REFERENCE]                 = PREC_PRIM,
		[EXPR_CHARACTER_CONSTANT]        = PREC_PRIM,
		[EXPR_WIDE_CHARACTER_CONSTANT]   = PREC_PRIM,
		[EXPR_CONST]                     = PREC_PRIM,
		[EXPR_STRING_LITERAL]            = PREC_PRIM,
		[EXPR_WIDE_STRING_LITERAL]       = PREC_PRIM,
		[EXPR_COMPOUND_LITERAL]          = PREC_UNARY,
		[EXPR_CALL]                      = PREC_PRIM,
		[EXPR_CONDITIONAL]               = PREC_COND,
		[EXPR_SELECT]                    = PREC_ACCESS,
		[EXPR_ARRAY_ACCESS]              = PREC_ACCESS,
		[EXPR_SIZEOF]                    = PREC_UNARY,
		[EXPR_CLASSIFY_TYPE]             = PREC_UNARY,
		[EXPR_ALIGNOF]                   = PREC_UNARY,

		[EXPR_FUNCNAME]                  = PREC_PRIM,
		[EXPR_BUILTIN_SYMBOL]            = PREC_PRIM,
		[EXPR_BUILTIN_CONSTANT_P]        = PREC_PRIM,
		[EXPR_BUILTIN_PREFETCH]          = PREC_PRIM,
		[EXPR_OFFSETOF]                  = PREC_PRIM,
		[EXPR_VA_START]                  = PREC_PRIM,
		[EXPR_VA_ARG]                    = PREC_PRIM,
		[EXPR_STATEMENT]                 = PREC_ACCESS,

		[EXPR_UNARY_NEGATE]              = PREC_UNARY,
		[EXPR_UNARY_PLUS]                = PREC_UNARY,
		[EXPR_UNARY_BITWISE_NEGATE]      = PREC_UNARY,
		[EXPR_UNARY_NOT]                 = PREC_UNARY,
		[EXPR_UNARY_DEREFERENCE]         = PREC_UNARY,
		[EXPR_UNARY_TAKE_ADDRESS]        = PREC_UNARY,
		[EXPR_UNARY_POSTFIX_INCREMENT]   = PREC_UNARY,
		[EXPR_UNARY_POSTFIX_DECREMENT]   = PREC_UNARY,
		[EXPR_UNARY_PREFIX_INCREMENT]    = PREC_UNARY,
		[EXPR_UNARY_PREFIX_DECREMENT]    = PREC_UNARY,
		[EXPR_UNARY_CAST]                = PREC_UNARY,
		[EXPR_UNARY_CAST_IMPLICIT]       = PREC_UNARY,
		[EXPR_UNARY_ASSUME]              = PREC_PRIM,

		[EXPR_BINARY_ADD]                = PREC_PLUS,
		[EXPR_BINARY_SUB]                = PREC_PLUS,
		[EXPR_BINARY_MUL]                = PREC_MUL,
		[EXPR_BINARY_DIV]                = PREC_MUL,
		[EXPR_BINARY_MOD]                = PREC_MUL,
		[EXPR_BINARY_EQUAL]              = PREC_EQ,
		[EXPR_BINARY_NOTEQUAL]           = PREC_EQ,
		[EXPR_BINARY_LESS]               = PREC_CMP,
		[EXPR_BINARY_LESSEQUAL]          = PREC_CMP,
		[EXPR_BINARY_GREATER]            = PREC_CMP,
		[EXPR_BINARY_GREATEREQUAL]       = PREC_CMP,
		[EXPR_BINARY_BITWISE_AND]        = PREC_BIT_AND,
		[EXPR_BINARY_BITWISE_OR]         = PREC_BIT_OR,
		[EXPR_BINARY_BITWISE_XOR]        = PREC_BIT_XOR,
		[EXPR_BINARY_LOGICAL_AND]        = PREC_LOG_AND,
		[EXPR_BINARY_LOGICAL_OR]         = PREC_LOG_OR,
		[EXPR_BINARY_SHIFTLEFT]          = PREC_SHF,
		[EXPR_BINARY_SHIFTRIGHT]         = PREC_SHF,
		[EXPR_BINARY_ASSIGN]             = PREC_ASSIGN,
		[EXPR_BINARY_MUL_ASSIGN]         = PREC_ASSIGN,
		[EXPR_BINARY_DIV_ASSIGN]         = PREC_ASSIGN,
		[EXPR_BINARY_MOD_ASSIGN]         = PREC_ASSIGN,
		[EXPR_BINARY_ADD_ASSIGN]         = PREC_ASSIGN,
		[EXPR_BINARY_SUB_ASSIGN]         = PREC_ASSIGN,
		[EXPR_BINARY_SHIFTLEFT_ASSIGN]   = PREC_ASSIGN,
		[EXPR_BINARY_SHIFTRIGHT_ASSIGN]  = PREC_ASSIGN,
		[EXPR_BINARY_BITWISE_AND_ASSIGN] = PREC_ASSIGN,
		[EXPR_BINARY_BITWISE_XOR_ASSIGN] = PREC_ASSIGN,
		[EXPR_BINARY_BITWISE_OR_ASSIGN]  = PREC_ASSIGN,
		[EXPR_BINARY_COMMA]              = PREC_COMMA,

		[EXPR_BINARY_BUILTIN_EXPECT]     = PREC_PRIM,
		[EXPR_BINARY_ISGREATER]          = PREC_PRIM,
		[EXPR_BINARY_ISGREATEREQUAL]     = PREC_PRIM,
		[EXPR_BINARY_ISLESS]             = PREC_PRIM,
		[EXPR_BINARY_ISLESSEQUAL]        = PREC_PRIM,
		[EXPR_BINARY_ISLESSGREATER]      = PREC_PRIM,
		[EXPR_BINARY_ISUNORDERED]        = PREC_PRIM
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
	if(cnst->base.type == NULL)
		return;

	const type_t *const type = skip_typeref(cnst->base.type);

	if (is_type_integer(type)) {
		fprintf(out, "%lld", cnst->v.int_value);
	} else if (is_type_float(type)) {
		fprintf(out, "%Lf", cnst->v.float_value);
	} else {
		panic("unknown constant");
	}
}

/**
 * Print a quoted string constant.
 *
 * @param string  the string constant
 * @param border  the border char
 */
static void print_quoted_string(const string_t *const string, char border)
{
	fputc(border, out);
	const char *end = string->begin + string->size - 1;
	for (const char *c = string->begin; c != end; ++c) {
		if (*c == border) {
			fputc('\\', out);
		}
		switch(*c) {
		case '\\':  fputs("\\\\", out); break;
		case '\a':  fputs("\\a", out); break;
		case '\b':  fputs("\\b", out); break;
		case '\f':  fputs("\\f", out); break;
		case '\n':  fputs("\\n", out); break;
		case '\r':  fputs("\\r", out); break;
		case '\t':  fputs("\\t", out); break;
		case '\v':  fputs("\\v", out); break;
		case '\?':  fputs("\\?", out); break;
		default:
			if(!isprint(*c)) {
				fprintf(out, "\\%03o", *c);
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
 * @param wstr  the wide string literal expression
 */
static void print_quoted_wide_string(const wide_string_t *const wstr,
                                     char border)
{
	fputc('L', out);
	fputc(border, out);
	for (const wchar_rep_t *c = wstr->begin, *end = wstr->begin + wstr->size-1;
	     c != end; ++c) {
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
			default: {
				const unsigned tc = *c;
				if (tc < 0x80U) {
					if (!isprint(*c))  {
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
	print_quoted_string(&cnst->v.character, '\'');
}

static void print_wide_character_constant(const const_expression_t *cnst)
{
	print_quoted_wide_string(&cnst->v.wide_character, '\'');
}

/**
 * Prints a string literal expression.
 *
 * @param string_literal  the string literal expression
 */
static void print_string_literal(
		const string_literal_expression_t *string_literal)
{
	print_quoted_string(&string_literal->value, '"');
}

/**
 * Prints a predefined symbol.
 */
static void print_funcname(
		const funcname_expression_t *funcname)
{
	const char *s = "";
	switch(funcname->kind) {
	case FUNCNAME_FUNCTION:        s = (c_mode & _C99) ? "__func__" : "__FUNCTION__"; break;
	case FUNCNAME_PRETTY_FUNCTION: s = "__PRETTY_FUNCTION__"; break;
	case FUNCNAME_FUNCSIG:         s = "__FUNCSIG__"; break;
	case FUNCNAME_FUNCDNAME:       s = "__FUNCDNAME__"; break;
	}
	fputc('"', out);
	fputs(s, out);
	fputc('"', out);
}

static void print_wide_string_literal(
	const wide_string_literal_expression_t *const wstr)
{
	print_quoted_wide_string(&wstr->value, '"');
}

static void print_compound_literal(
		const compound_literal_expression_t *expression)
{
	fputc('(', out);
	print_type(expression->type);
	fputs(") ", out);
	print_initializer(expression->initializer);
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
	fprintf(out, "(");
	call_argument_t *argument = call->arguments;
	int              first    = 1;
	while(argument != NULL) {
		if(!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}
		print_expression_prec(argument->expression, PREC_COMMA + 1);

		argument = argument->next;
	}
	fprintf(out, ")");
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

	if(binexpr->base.kind == EXPR_BINARY_BUILTIN_EXPECT) {
		fputs("__builtin_expect(", out);
		print_expression_prec(binexpr->left, prec);
		fputs(", ", out);
		print_expression_prec(binexpr->right, prec);
		fputc(')', out);
		return;
	}

	print_expression_prec(binexpr->left, prec + r2l);
	if (binexpr->base.kind != EXPR_BINARY_COMMA) {
		fputc(' ', out);
	}
	switch (binexpr->base.kind) {
	case EXPR_BINARY_COMMA:              fputs(",", out);     break;
	case EXPR_BINARY_ASSIGN:             fputs("=", out);     break;
	case EXPR_BINARY_ADD:                fputs("+", out);     break;
	case EXPR_BINARY_SUB:                fputs("-", out);     break;
	case EXPR_BINARY_MUL:                fputs("*", out);     break;
	case EXPR_BINARY_MOD:                fputs("%", out);     break;
	case EXPR_BINARY_DIV:                fputs("/", out);     break;
	case EXPR_BINARY_BITWISE_OR:         fputs("|", out);     break;
	case EXPR_BINARY_BITWISE_AND:        fputs("&", out);     break;
	case EXPR_BINARY_BITWISE_XOR:        fputs("^", out);     break;
	case EXPR_BINARY_LOGICAL_OR:         fputs("||", out);    break;
	case EXPR_BINARY_LOGICAL_AND:        fputs("&&", out);    break;
	case EXPR_BINARY_NOTEQUAL:           fputs("!=", out);    break;
	case EXPR_BINARY_EQUAL:              fputs("==", out);    break;
	case EXPR_BINARY_LESS:               fputs("<", out);     break;
	case EXPR_BINARY_LESSEQUAL:          fputs("<=", out);    break;
	case EXPR_BINARY_GREATER:            fputs(">", out);     break;
	case EXPR_BINARY_GREATEREQUAL:       fputs(">=", out);    break;
	case EXPR_BINARY_SHIFTLEFT:          fputs("<<", out);    break;
	case EXPR_BINARY_SHIFTRIGHT:         fputs(">>", out);    break;

	case EXPR_BINARY_ADD_ASSIGN:         fputs("+=", out);    break;
	case EXPR_BINARY_SUB_ASSIGN:         fputs("-=", out);    break;
	case EXPR_BINARY_MUL_ASSIGN:         fputs("*=", out);    break;
	case EXPR_BINARY_MOD_ASSIGN:         fputs("%=", out);    break;
	case EXPR_BINARY_DIV_ASSIGN:         fputs("/=", out);    break;
	case EXPR_BINARY_BITWISE_OR_ASSIGN:  fputs("|=", out);    break;
	case EXPR_BINARY_BITWISE_AND_ASSIGN: fputs("&=", out);    break;
	case EXPR_BINARY_BITWISE_XOR_ASSIGN: fputs("^=", out);    break;
	case EXPR_BINARY_SHIFTLEFT_ASSIGN:   fputs("<<=", out);   break;
	case EXPR_BINARY_SHIFTRIGHT_ASSIGN:  fputs(">>=", out);   break;
	default: panic("invalid binexpression found");
	}
	fputc(' ', out);
	print_expression_prec(binexpr->right, prec - r2l);
}

/**
 * Prints an unary expression.
 *
 * @param unexpr   the unary expression
 */
static void print_unary_expression(const unary_expression_t *unexpr)
{
	unsigned prec = get_expression_precedence(unexpr->base.kind);
	switch(unexpr->base.kind) {
	case EXPR_UNARY_NEGATE:           fputs("-", out);  break;
	case EXPR_UNARY_PLUS:             fputs("+", out);  break;
	case EXPR_UNARY_NOT:              fputs("!", out);  break;
	case EXPR_UNARY_BITWISE_NEGATE:   fputs("~", out);  break;
	case EXPR_UNARY_PREFIX_INCREMENT: fputs("++", out); break;
	case EXPR_UNARY_PREFIX_DECREMENT: fputs("--", out); break;
	case EXPR_UNARY_DEREFERENCE:      fputs("*", out);  break;
	case EXPR_UNARY_TAKE_ADDRESS:     fputs("&", out);  break;

	case EXPR_UNARY_POSTFIX_INCREMENT:
		print_expression_prec(unexpr->value, prec);
		fputs("++", out);
		return;
	case EXPR_UNARY_POSTFIX_DECREMENT:
		print_expression_prec(unexpr->value, prec);
		fputs("--", out);
		return;
	case EXPR_UNARY_CAST_IMPLICIT:
		if(!print_implicit_casts) {
			print_expression_prec(unexpr->value, prec);
			return;
		}
		/* fallthrough */
	case EXPR_UNARY_CAST:
		fputc('(', out);
		print_type(unexpr->base.type);
		fputc(')', out);
		break;
	case EXPR_UNARY_ASSUME:
		fputs("__assume(", out);
		print_expression_prec(unexpr->value, PREC_COMMA + 1);
		fputc(')', out);
		return;
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
	fprintf(out, "%s", ref->declaration->symbol->string);
}

/**
 * Prints an array expression.
 *
 * @param expression   the array expression
 */
static void print_array_expression(const array_access_expression_t *expression)
{
	unsigned prec = get_expression_precedence(expression->base.kind);
	if(!expression->flipped) {
		print_expression_prec(expression->array_ref, prec);
		fputc('[', out);
		print_expression_prec(expression->index, prec);
		fputc(']', out);
	} else {
		print_expression_prec(expression->index, prec);
		fputc('[', out);
		print_expression_prec(expression->array_ref, prec);
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
	if(expression->tp_expression != NULL) {
		/* always print the '()' here, sizeof x is right but unusual */
		fputc('(', out);
		print_expression_prec(expression->tp_expression, PREC_ACCESS);
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
	print_expression_prec(expression->value, PREC_COMMA + 1);
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
	print_expression_prec(expression->adr, PREC_COMMA + 1);
	if (expression->rw) {
		fputc(',', out);
		print_expression_prec(expression->rw, PREC_COMMA + 1);
	}
	if (expression->locality) {
		fputc(',', out);
		print_expression_prec(expression->locality, PREC_COMMA + 1);
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
	unsigned prec = get_expression_precedence(expression->base.kind);
	fputs("(", out);
	print_expression_prec(expression->condition, prec);
	fputs(" ? ", out);
	print_expression_prec(expression->true_expression, prec);
	fputs(" : ", out);
	print_expression_prec(expression->false_expression, prec);
	fputs(")", out);
}

/**
 * Prints a va_start expression.
 *
 * @param expression   the va_start expression
 */
static void print_va_start(const va_start_expression_t *const expression)
{
	fputs("__builtin_va_start(", out);
	print_expression_prec(expression->ap, PREC_COMMA + 1);
	fputs(", ", out);
	fputs(expression->parameter->symbol->string, out);
	fputs(")", out);
}

/**
 * Prints a va_arg expression.
 *
 * @param expression   the va_arg expression
 */
static void print_va_arg(const va_arg_expression_t *expression)
{
	fputs("__builtin_va_arg(", out);
	print_expression_prec(expression->ap, PREC_COMMA + 1);
	fputs(", ", out);
	print_type(expression->base.type);
	fputs(")", out);
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
	if(is_type_pointer(expression->compound->base.type)) {
		fputs("->", out);
	} else {
		fputc('.', out);
	}
	fputs(expression->symbol->string, out);
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
	print_expression_prec(expr->type_expression, PREC_COMMA + 1);
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
			print_expression_prec(designator->array_index, PREC_ACCESS);
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
	unsigned prec = get_expression_precedence(expression->base.kind);
	if (print_parenthesis && top_prec != PREC_BOTTOM)
		top_prec = PREC_TOP;
	if (top_prec > prec)
		fputc('(', out);
	switch(expression->kind) {
	case EXPR_UNKNOWN:
	case EXPR_INVALID:
		fprintf(out, "$invalid expression$");
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
		print_reference_expression(&expression->reference);
		break;
	case EXPR_ARRAY_ACCESS:
		print_array_expression(&expression->array_access);
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
		fprintf(out, "some expression of type %d", (int) expression->kind);
		break;
	}
	if (top_prec > prec)
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
	while(statement != NULL) {
		if (statement->base.kind == STATEMENT_CASE_LABEL)
			--indent;
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
	fprintf(out, "return ");
	if(statement->value != NULL)
		print_expression(statement->value);
	fputs(";\n", out);
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
	fprintf(out, "goto ");
	fputs(statement->label->symbol->string, out);
	fprintf(stderr, "(%p)", (void*) statement->label);
	fputs(";\n", out);
}

/**
 * Print a label statement.
 *
 * @param statement  the label statement
 */
static void print_label_statement(const label_statement_t *statement)
{
	fprintf(stderr, "(%p)", (void*) statement->label);
	fprintf(out, "%s:\n", statement->label->symbol->string);
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

	if(statement->false_statement != NULL) {
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
	if(statement->expression == NULL) {
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
	if(statement->statement != NULL) {
		if (statement->statement->base.kind == STATEMENT_CASE_LABEL) {
			--indent;
		}
		print_indent();
		print_statement(statement->statement);
	}
}

/**
 * Print a declaration statement.
 *
 * @param statement   the statement
 */
static void print_declaration_statement(
		const declaration_statement_t *statement)
{
	int first = 1;
	declaration_t *declaration = statement->declarations_begin;
	for( ; declaration != statement->declarations_end->next;
	       declaration = declaration->next) {
	    if(declaration->storage_class == STORAGE_CLASS_ENUM_ENTRY)
	    	continue;

		if(!first) {
			print_indent();
		} else {
			first = 0;
		}
		print_declaration(declaration);
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
	if(statement->scope.declarations != NULL) {
		assert(statement->initialisation == NULL);
		print_declaration(statement->scope.declarations);
		if(statement->scope.declarations->next != NULL) {
			panic("multiple declarations in for statement not supported yet");
		}
		fputc(' ', out);
	} else {
		if(statement->initialisation) {
			print_expression(statement->initialisation);
		}
		fputs("; ", out);
	}
	if(statement->condition != NULL) {
		print_expression(statement->condition);
	}
	fputs("; ", out);
	if(statement->step != NULL) {
		print_expression(statement->step);
	}
	fputs(")", out);
	print_statement(statement->body);
}

/**
 * Print assembler constraints.
 *
 * @param constraints   the constraints
 */
static void print_asm_constraints(asm_constraint_t *constraints)
{
	asm_constraint_t *constraint = constraints;
	for( ; constraint != NULL; constraint = constraint->next) {
		if(constraint != constraints)
			fputs(", ", out);

		if(constraint->symbol) {
			fprintf(out, "[%s] ", constraint->symbol->string);
		}
		print_quoted_string(&constraint->constraints, '"');
		fputs(" (", out);
		print_expression(constraint->expression);
		fputs(")", out);
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
	for( ; clobber != NULL; clobber = clobber->next) {
		if(clobber != clobbers)
			fputs(", ", out);

		print_quoted_string(&clobber->clobber, '"');
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
	if(statement->is_volatile) {
		fputs("volatile ", out);
	}
	fputs("(", out);
	print_quoted_string(&statement->asm_text, '"');
	if(statement->inputs == NULL && statement->outputs == NULL
			&& statement->clobbers == NULL)
		goto end_of_print_asm_statement;

	fputs(" : ", out);
	print_asm_constraints(statement->inputs);
	if(statement->outputs == NULL && statement->clobbers == NULL)
		goto end_of_print_asm_statement;

	fputs(" : ", out);
	print_asm_constraints(statement->outputs);
	if(statement->clobbers == NULL)
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
	if(statement->except_expression != NULL) {
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
	(void) statement;
	fputs("__leave;\n", out);
}

/**
 * Print a statement.
 *
 * @param statement   the statement
 */
void print_statement(const statement_t *statement)
{
	switch(statement->kind) {
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
		fprintf(out, "$invalid statement$");
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
	switch(storage_class) {
	case STORAGE_CLASS_ENUM_ENTRY:
	case STORAGE_CLASS_NONE:
		break;
	case STORAGE_CLASS_TYPEDEF:       fputs("typedef ",        out); break;
	case STORAGE_CLASS_EXTERN:        fputs("extern ",         out); break;
	case STORAGE_CLASS_STATIC:        fputs("static ",         out); break;
	case STORAGE_CLASS_AUTO:          fputs("auto ",           out); break;
	case STORAGE_CLASS_REGISTER:      fputs("register ",       out); break;
	case STORAGE_CLASS_THREAD:        fputs("__thread",        out); break;
	case STORAGE_CLASS_THREAD_EXTERN: fputs("extern __thread", out); break;
	case STORAGE_CLASS_THREAD_STATIC: fputs("static __thread", out); break;
	}
}

/**
 * Print an initializer.
 *
 * @param initializer  the initializer
 */
void print_initializer(const initializer_t *initializer)
{
	if(initializer == NULL) {
		fputs("{}", out);
		return;
	}

	switch(initializer->kind) {
	case INITIALIZER_VALUE: {
		const initializer_value_t *value = &initializer->value;
		print_expression(value->value);
		return;
	}
	case INITIALIZER_LIST: {
		assert(initializer->kind == INITIALIZER_LIST);
		fputs("{ ", out);
		const initializer_list_t *list = &initializer->list;

		for(size_t i = 0 ; i < list->len; ++i) {
			const initializer_t *sub_init = list->initializers[i];
			print_initializer(list->initializers[i]);
			if(i < list->len-1) {
				if(sub_init == NULL || sub_init->kind != INITIALIZER_DESIGNATOR)
					fputs(", ", out);
			}
		}
		fputs(" }", out);
		return;
	}
	case INITIALIZER_STRING:
		print_quoted_string(&initializer->string.string, '"');
		return;
	case INITIALIZER_WIDE_STRING:
		print_quoted_wide_string(&initializer->wide_string.string, '"');
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
static void print_ms_modifiers(const declaration_t *declaration) {
	if((c_mode & _MS) == 0)
		return;

	decl_modifiers_t modifiers = declaration->decl_modifiers;

	/* DM_FORCEINLINE handled outside. */
	if((modifiers & ~DM_FORCEINLINE) != 0 ||
	    declaration->alignment != 0 || declaration->deprecated != 0 ||
	    declaration->get_property_sym != NULL || declaration->put_property_sym != NULL) {
		char *next = "(";

		fputs("__declspec", out);
		if(modifiers & DM_DLLIMPORT) {
			fputs(next, out); next = ", "; fputs("dllimport", out);
		}
		if(modifiers & DM_DLLEXPORT) {
			fputs(next, out); next = ", "; fputs("dllexport", out);
		}
		if(modifiers & DM_THREAD) {
			fputs(next, out); next = ", "; fputs("thread", out);
		}
		if(modifiers & DM_NAKED) {
			fputs(next, out); next = ", "; fputs("naked", out);
		}
		if(modifiers & DM_THREAD) {
			fputs(next, out); next = ", "; fputs("thread", out);
		}
		if(modifiers & DM_SELECTANY) {
			fputs(next, out); next = ", "; fputs("selectany", out);
		}
		if(modifiers & DM_NOTHROW) {
			fputs(next, out); next = ", "; fputs("nothrow", out);
		}
		if(modifiers & DM_NORETURN) {
			fputs(next, out); next = ", "; fputs("noreturn", out);
		}
		if(modifiers & DM_NOINLINE) {
			fputs(next, out); next = ", "; fputs("noinline", out);
		}
		if(declaration->deprecated != 0) {
			fputs(next, out); next = ", "; fputs("deprecated", out);
			if(declaration->deprecated_string != NULL)
				fprintf(out, "(\"%s\")", declaration->deprecated_string);
		}
		if(declaration->alignment != 0) {
			fputs(next, out); next = ", "; fprintf(out, "align(%u)", declaration->alignment);
		}
		if(modifiers & DM_RESTRICT) {
			fputs(next, out); next = ", "; fputs("restrict", out);
		}
		if(modifiers & DM_NOALIAS) {
			fputs(next, out); next = ", "; fputs("noalias", out);
		}
	    if(declaration->get_property_sym != NULL || declaration->put_property_sym != NULL) {
	    	char *comma = "";
			fputs(next, out); next = ", "; fprintf(out, "property(");
	    	if(declaration->get_property_sym != NULL) {
	    		fprintf(out, "get=%s", declaration->get_property_sym->string);
	    		comma = ", ";
			}
	    	if(declaration->put_property_sym != NULL)
	    		fprintf(out, "%sput=%s", comma, declaration->put_property_sym->string);
			fputc(')', out);
		}
		fputs(") ", out);
	}
}

/**
 * Print a declaration in the NORMAL namespace.
 *
 * @param declaration  the declaration
 */
static void print_normal_declaration(const declaration_t *declaration)
{
	print_storage_class((storage_class_tag_t) declaration->declared_storage_class);
	if(declaration->is_inline) {
		if(declaration->decl_modifiers & DM_FORCEINLINE)
			fputs("__forceinline ", out);
		else {
			if(declaration->decl_modifiers & DM_MICROSOFT_INLINE)
				fputs("__inline ", out);
			else
				fputs("inline ", out);
		}
	}
	print_ms_modifiers(declaration);
	print_type_ext(declaration->type, declaration->symbol,
	               &declaration->scope);

	if(declaration->type->kind == TYPE_FUNCTION) {
		if(declaration->init.statement != NULL) {
			fputs("\n", out);
			print_statement(declaration->init.statement);
			return;
		}
	} else if(declaration->init.initializer != NULL) {
		fputs(" = ", out);
		print_initializer(declaration->init.initializer);
	}
	fputc(';', out);
}

/**
 * Prints an expression.
 *
 * @param expression  the expression
 */
void print_expression(const expression_t *expression) {
	print_expression_prec(expression, PREC_BOTTOM);
}

/**
 * Print a declaration.
 *
 * @param declaration  the declaration
 */
void print_declaration(const declaration_t *declaration)
{
	if(declaration->namespc != NAMESPACE_NORMAL &&
			declaration->symbol == NULL)
		return;

	switch(declaration->namespc) {
	case NAMESPACE_NORMAL:
		print_normal_declaration(declaration);
		break;
	case NAMESPACE_STRUCT:
		fputs("struct ", out);
		fputs(declaration->symbol->string, out);
		fputc(' ', out);
		print_compound_definition(declaration);
		fputc(';', out);
		break;
	case NAMESPACE_UNION:
		fputs("union ", out);
		fputs(declaration->symbol->string, out);
		fputc(' ', out);
		print_compound_definition(declaration);
		fputc(';', out);
		break;
	case NAMESPACE_ENUM:
		fputs("enum ", out);
		fputs(declaration->symbol->string, out);
		fputc(' ', out);
		print_enum_definition(declaration);
		fputc(';', out);
		break;
	}
}

/**
 * Print the AST of a translation unit.
 *
 * @param unit   the translation unit
 */
void print_ast(const translation_unit_t *unit)
{
	inc_type_visited();

	declaration_t *declaration = unit->scope.declarations;
	for( ; declaration != NULL; declaration = declaration->next) {
		if(declaration->storage_class == STORAGE_CLASS_ENUM_ENTRY)
			continue;
		if(declaration->namespc != NAMESPACE_NORMAL &&
				declaration->symbol == NULL)
			continue;

		print_indent();
		print_declaration(declaration);
		fputc('\n', out);
	}
}

bool is_constant_initializer(const initializer_t *initializer)
{
	switch(initializer->kind) {
	case INITIALIZER_STRING:
	case INITIALIZER_WIDE_STRING:
	case INITIALIZER_DESIGNATOR:
		return true;

	case INITIALIZER_VALUE:
		return is_constant_expression(initializer->value.value);

	case INITIALIZER_LIST:
		for(size_t i = 0; i < initializer->list.len; ++i) {
			initializer_t *sub_initializer = initializer->list.initializers[i];
			if(!is_constant_initializer(sub_initializer))
				return false;
		}
		return true;
	}
	panic("invalid initializer kind found");
}

static bool is_object_with_constant_address(const expression_t *expression)
{
	switch(expression->kind) {
	case EXPR_UNARY_DEREFERENCE:
		return is_address_constant(expression->unary.value);

	case EXPR_SELECT: {
		if(is_type_pointer(expression->select.compound->base.type)) {
			/* it's a -> */
			return is_address_constant(expression->select.compound);
		} else {
			return is_object_with_constant_address(expression->select.compound);
		}
	}

	case EXPR_ARRAY_ACCESS:
		return is_constant_expression(expression->array_access.index)
			&& is_address_constant(expression->array_access.array_ref);

	case EXPR_REFERENCE: {
		declaration_t *declaration = expression->reference.declaration;
		switch((storage_class_tag_t) declaration->storage_class) {
		case STORAGE_CLASS_NONE:
		case STORAGE_CLASS_EXTERN:
		case STORAGE_CLASS_STATIC:
			return true;
		default:
			return false;
		}
	}

	default:
		return false;
	}
}

bool is_address_constant(const expression_t *expression)
{
	switch(expression->kind) {
	case EXPR_UNARY_TAKE_ADDRESS:
		return is_object_with_constant_address(expression->unary.value);

	case EXPR_UNARY_DEREFERENCE: {
		type_t *real_type = revert_automatic_type_conversion(expression->unary.value);
		/* dereferencing a function is a NOP */
		if(is_type_function(real_type)) {
			return is_address_constant(expression->unary.value);
		}
	}

	case EXPR_UNARY_CAST:
		return is_type_pointer(skip_typeref(expression->base.type))
			&& (is_constant_expression(expression->unary.value)
			|| is_address_constant(expression->unary.value));

	case EXPR_BINARY_ADD:
	case EXPR_BINARY_SUB: {
		expression_t *left  = expression->binary.left;
		expression_t *right = expression->binary.right;

		if(is_type_pointer(skip_typeref(left->base.type))) {
			return is_address_constant(left) && is_constant_expression(right);
		} else if(is_type_pointer(skip_typeref(right->base.type))) {
			return is_constant_expression(left)	&& is_address_constant(right);
		}

		return false;
	}

	case EXPR_REFERENCE: {
		declaration_t *declaration = expression->reference.declaration;
		type_t *type = skip_typeref(declaration->type);
		if(is_type_function(type))
			return true;
		if(is_type_array(type)) {
			return is_object_with_constant_address(expression);
		}
		return false;
	}

	default:
		return false;
	}
}

bool is_constant_expression(const expression_t *expression)
{
	switch(expression->kind) {

	case EXPR_CONST:
	case EXPR_CHARACTER_CONSTANT:
	case EXPR_WIDE_CHARACTER_CONSTANT:
	case EXPR_STRING_LITERAL:
	case EXPR_WIDE_STRING_LITERAL:
	case EXPR_SIZEOF:
	case EXPR_CLASSIFY_TYPE:
	case EXPR_FUNCNAME:
	case EXPR_OFFSETOF:
	case EXPR_ALIGNOF:
	case EXPR_BUILTIN_CONSTANT_P:
		return true;

	case EXPR_BUILTIN_SYMBOL:
	case EXPR_BUILTIN_PREFETCH:
	case EXPR_CALL:
	case EXPR_SELECT:
	case EXPR_VA_START:
	case EXPR_VA_ARG:
	case EXPR_STATEMENT:
	case EXPR_UNARY_POSTFIX_INCREMENT:
	case EXPR_UNARY_POSTFIX_DECREMENT:
	case EXPR_UNARY_PREFIX_INCREMENT:
	case EXPR_UNARY_PREFIX_DECREMENT:
	case EXPR_UNARY_ASSUME: /* has VOID type */
	case EXPR_UNARY_TAKE_ADDRESS:
	case EXPR_UNARY_DEREFERENCE:
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
		return false;

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
	case EXPR_BINARY_BUILTIN_EXPECT:
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
		if(!is_constant_expression(condition))
			return false;

		long val = fold_constant(condition);
		if(val != 0)
			return is_constant_expression(expression->conditional.true_expression);
		else
			return is_constant_expression(expression->conditional.false_expression);
	}

	case EXPR_ARRAY_ACCESS:
		return is_constant_expression(expression->array_access.array_ref)
			&& is_constant_expression(expression->array_access.index);

	case EXPR_REFERENCE: {
		declaration_t *declaration = expression->reference.declaration;
		if(declaration->storage_class == STORAGE_CLASS_ENUM_ENTRY)
			return true;

		return false;
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
