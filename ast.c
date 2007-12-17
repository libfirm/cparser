#include <config.h>

#include "ast_t.h"
#include "type_t.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "adt/error.h"

struct obstack ast_obstack;

static FILE *out;
static int   indent;
static int   print_implicit_casts = 1;

static void print_statement(const statement_t *statement);

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

static void print_const(const const_expression_t *cnst)
{
	if(cnst->base.type == NULL)
		return;

	if(is_type_integer(cnst->base.type)) {
		fprintf(out, "%lld", cnst->v.int_value);
	} else if(is_type_float(cnst->base.type)) {
		fprintf(out, "%Lf", cnst->v.float_value);
	}
}

static void print_quoted_string(const string_t *const string)
{
	fputc('"', out);
	const char *end = string->begin + string->size;
	for (const char *c = string->begin; c != end; ++c) {
		switch(*c) {
		case '\"':  fputs("\\\"", out); break;
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
	fputc('"', out);
}

static void print_string_literal(
		const string_literal_expression_t *string_literal)
{
	print_quoted_string(&string_literal->value);
}

static void print_wide_string_literal(
	const wide_string_literal_expression_t *const wstr)
{
	fputs("L\"", out);
	for (const wchar_rep_t *c   = wstr->value.begin,
	                       *end = c + wstr->value.size;
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
	fputc('"', out);
}

static void print_call_expression(const call_expression_t *call)
{
	print_expression(call->function);
	fprintf(out, "(");
	call_argument_t *argument = call->arguments;
	int              first    = 1;
	while(argument != NULL) {
		if(!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}
		print_expression(argument->expression);

		argument = argument->next;
	}
	fprintf(out, ")");
}

static void print_binary_expression(const binary_expression_t *binexpr)
{
	if(binexpr->base.kind == EXPR_BINARY_BUILTIN_EXPECT) {
		fputs("__builtin_expect(", out);
		print_expression(binexpr->left);
		fputs(", ", out);
		print_expression(binexpr->right);
		fputs(")", out);
		return;
	}

	fprintf(out, "(");
	print_expression(binexpr->left);
	fprintf(out, " ");
	switch(binexpr->base.kind) {
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
	fprintf(out, " ");
	print_expression(binexpr->right);
	fprintf(out, ")");
}

static void print_unary_expression(const unary_expression_t *unexpr)
{
	switch(unexpr->base.kind) {
	case EXPR_UNARY_NEGATE:           fputs("-", out);  break;
	case EXPR_UNARY_PLUS:             fputs("+", out);  break;
	case EXPR_UNARY_NOT:              fputs("!", out);  break;
	case EXPR_UNARY_BITWISE_NEGATE:   fputs("~", out);  break;
	case EXPR_UNARY_PREFIX_INCREMENT: fputs("++", out); break;
	case EXPR_UNARY_PREFIX_DECREMENT: fputs("--", out); break;
	case EXPR_UNARY_DEREFERENCE:      fputs("*", out);  break;
	case EXPR_UNARY_TAKE_ADDRESS:     fputs("&", out);  break;

	case EXPR_UNARY_BITFIELD_EXTRACT:
		print_expression(unexpr->value);
		return;

	case EXPR_UNARY_POSTFIX_INCREMENT:
		fputs("(", out);
		print_expression(unexpr->value);
		fputs(")", out);
		fputs("++", out);
		return;
	case EXPR_UNARY_POSTFIX_DECREMENT:
		fputs("(", out);
		print_expression(unexpr->value);
		fputs(")", out);
		fputs("--", out);
		return;
	case EXPR_UNARY_CAST_IMPLICIT:
		if(!print_implicit_casts) {
			print_expression(unexpr->value);
			return;
		}
		/* fallthrough */
	case EXPR_UNARY_CAST:
		fputs("(", out);
		print_type(unexpr->base.type);
		fputs(")", out);
		break;
	case EXPR_UNARY_ASSUME:
		fputs("__assume", out);
		break;
	default:
		panic("invalid unary expression found");
	}
	fputs("(", out);
	print_expression(unexpr->value);
	fputs(")", out);
}

static void print_reference_expression(const reference_expression_t *ref)
{
	fprintf(out, "%s", ref->declaration->symbol->string);
}

static void print_array_expression(const array_access_expression_t *expression)
{
	if(!expression->flipped) {
		fputs("(", out);
		print_expression(expression->array_ref);
		fputs(")[", out);
		print_expression(expression->index);
		fputs("]", out);
	} else {
		fputs("(", out);
		print_expression(expression->index);
		fputs(")[", out);
		print_expression(expression->array_ref);
		fputs("]", out);
	}
}

static void print_typeprop_expression(const typeprop_expression_t *expression)
{
	if (expression->base.kind == EXPR_SIZEOF) {
		fputs("sizeof", out);
	} else {
		assert(expression->base.kind == EXPR_ALIGNOF);
		fputs("__alignof__", out);
	}
	if(expression->tp_expression != NULL) {
		fputc('(', out);
		print_expression(expression->tp_expression);
		fputc(')', out);
	} else {
		fputc('(', out);
		print_type(expression->type);
		fputc(')', out);
	}
}

static void print_builtin_symbol(const builtin_symbol_expression_t *expression)
{
	fputs(expression->symbol->string, out);
}

static void print_builtin_constant(const builtin_constant_expression_t *expression)
{
	fputs("__builtin_constant_p(", out);
	print_expression(expression->value);
	fputc(')', out);
}

static void print_builtin_prefetch(const builtin_prefetch_expression_t *expression)
{
	fputs("__builtin_prefetch(", out);
	print_expression(expression->adr);
	if (expression->rw) {
		fputc(',', out);
		print_expression(expression->rw);
	}
	if (expression->locality) {
		fputc(',', out);
		print_expression(expression->locality);
	}
	fputc(')', out);
}

static void print_conditional(const conditional_expression_t *expression)
{
	fputs("(", out);
	print_expression(expression->condition);
	fputs(" ? ", out);
	print_expression(expression->true_expression);
	fputs(" : ", out);
	print_expression(expression->false_expression);
	fputs(")", out);
}

static void print_va_start(const va_start_expression_t *const expression)
{
	fputs("__builtin_va_start(", out);
	print_expression(expression->ap);
	fputs(", ", out);
	fputs(expression->parameter->symbol->string, out);
	fputs(")", out);
}

static void print_va_arg(const va_arg_expression_t *expression)
{
	fputs("__builtin_va_arg(", out);
	print_expression(expression->ap);
	fputs(", ", out);
	print_type(expression->base.type);
	fputs(")", out);
}

static void print_select(const select_expression_t *expression)
{
	print_expression(expression->compound);
	if(expression->compound->base.type == NULL ||
			expression->compound->base.type->kind == TYPE_POINTER) {
		fputs("->", out);
	} else {
		fputc('.', out);
	}
	fputs(expression->symbol->string, out);
}

static void print_classify_type_expression(
	const classify_type_expression_t *const expr)
{
	fputs("__builtin_classify_type(", out);
	print_expression(expr->type_expression);
	fputc(')', out);
}

static void print_designator(const designator_t *designator)
{
	fputs(designator->symbol->string, out);
	for (designator = designator->next; designator != NULL; designator = designator->next) {
		if (designator->array_access) {
			fputc('[', out);
			print_expression(designator->array_access);
			fputc(']', out);
		} else {
			fputc('.', out);
			fputs(designator->symbol->string, out);
		}
	}
}

static void print_offsetof_expression(const offsetof_expression_t *expression)
{
	fputs("__builtin_offsetof", out);
	fputc('(', out);
	print_type(expression->type);
	fputc(',', out);
	print_designator(expression->designator);
	fputc(')', out);
}

static void print_statement_expression(const statement_expression_t *expression)
{
	fputc('(', out);
	print_statement(expression->statement);
	fputc(')', out);
}

void print_expression(const expression_t *expression)
{
	switch(expression->kind) {
	case EXPR_UNKNOWN:
	case EXPR_INVALID:
		fprintf(out, "*invalid expression*");
		break;
	case EXPR_CONST:
		print_const(&expression->conste);
		break;
	case EXPR_FUNCTION:
	case EXPR_PRETTY_FUNCTION:
	case EXPR_STRING_LITERAL:
		print_string_literal(&expression->string);
		break;
	case EXPR_WIDE_STRING_LITERAL:
		print_wide_string_literal(&expression->wide_string);
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
}

static void print_compound_statement(const compound_statement_t *block)
{
	fputs("{\n", out);
	indent++;

	statement_t *statement = block->statements;
	while(statement != NULL) {
		print_indent();
		print_statement(statement);

		statement = statement->base.next;
	}
	indent--;
	print_indent();
	fputs("}\n", out);
}

static void print_return_statement(const return_statement_t *statement)
{
	fprintf(out, "return ");
	if(statement->value != NULL)
		print_expression(statement->value);
	fputs(";\n", out);
}

static void print_expression_statement(const expression_statement_t *statement)
{
	print_expression(statement->expression);
	fputs(";\n", out);
}

static void print_goto_statement(const goto_statement_t *statement)
{
	fprintf(out, "goto ");
	fputs(statement->label->symbol->string, out);
	fprintf(stderr, "(%p)", (void*) statement->label);
	fputs(";\n", out);
}

static void print_label_statement(const label_statement_t *statement)
{
	fprintf(stderr, "(%p)", (void*) statement->label);
	fprintf(out, "%s:\n", statement->label->symbol->string);
	if(statement->statement != NULL) {
		print_statement(statement->statement);
	}
}

static void print_if_statement(const if_statement_t *statement)
{
	fputs("if(", out);
	print_expression(statement->condition);
	fputs(") ", out);
	if(statement->true_statement != NULL) {
		print_statement(statement->true_statement);
	}

	if(statement->false_statement != NULL) {
		print_indent();
		fputs("else ", out);
		print_statement(statement->false_statement);
	}
}

static void print_switch_statement(const switch_statement_t *statement)
{
	fputs("switch(", out);
	print_expression(statement->expression);
	fputs(") ", out);
	print_statement(statement->body);
}

static void print_case_label(const case_label_statement_t *statement)
{
	if(statement->expression == NULL) {
		fputs("default:\n", out);
	} else {
		fputs("case ", out);
		print_expression(statement->expression);
		fputs(":\n", out);
	}
	if(statement->statement != NULL) {
		print_statement(statement->statement);
	}
}

static void print_declaration_statement(
		const declaration_statement_t *statement)
{
	int first = 1;
	declaration_t *declaration = statement->declarations_begin;
	for( ; declaration != statement->declarations_end->next;
	       declaration = declaration->next) {
		if(!first) {
			print_indent();
		} else {
			first = 0;
		}
		print_declaration(declaration);
		fputc('\n', out);
	}
}

static void print_while_statement(const while_statement_t *statement)
{
	fputs("while(", out);
	print_expression(statement->condition);
	fputs(") ", out);
	print_statement(statement->body);
}

static void print_do_while_statement(const do_while_statement_t *statement)
{
	fputs("do ", out);
	print_statement(statement->body);
	print_indent();
	fputs("while(", out);
	print_expression(statement->condition);
	fputs(");\n", out);
}

static void print_for_statement(const for_statement_t *statement)
{
	fputs("for(", out);
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

static void print_asm_constraints(asm_constraint_t *constraints)
{
	asm_constraint_t *constraint = constraints;
	for( ; constraint != NULL; constraint = constraint->next) {
		if(constraint != constraints)
			fputs(", ", out);

		if(constraint->symbol) {
			fprintf(out, "[%s] ", constraint->symbol->string);
		}
		print_quoted_string(&constraint->constraints);
		fputs(" (", out);
		print_expression(constraint->expression);
		fputs(")", out);
	}
}

static void print_asm_clobbers(asm_clobber_t *clobbers)
{
	asm_clobber_t *clobber = clobbers;
	for( ; clobber != NULL; clobber = clobber->next) {
		if(clobber != clobbers)
			fputs(", ", out);

		print_quoted_string(&clobber->clobber);
	}
}

static void print_asm_statement(const asm_statement_t *statement)
{
	fputs("asm ", out);
	if(statement->is_volatile) {
		fputs("volatile ", out);
	}
	fputs("(", out);
	print_quoted_string(&statement->asm_text);
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

void print_statement(const statement_t *statement)
{
	switch(statement->kind) {
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
	case STATEMENT_INVALID:
		fprintf(out, "*invalid statement*");
		break;
	}
}

static void print_storage_class(unsigned storage_class)
{
	switch((storage_class_tag_t) storage_class) {
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

void print_initializer(const initializer_t *initializer)
{
	if(initializer->kind == INITIALIZER_VALUE) {
		const initializer_value_t *value = &initializer->value;
		print_expression(value->value);
		return;
	}

	assert(initializer->kind == INITIALIZER_LIST);
	fputs("{ ", out);
	const initializer_list_t *list = &initializer->list;

	for(size_t i = 0 ; i < list->len; ++i) {
		if(i > 0) {
			fputs(", ", out);
		}
		print_initializer(list->initializers[i]);
	}
	fputs("}", out);
}

static void print_normal_declaration(const declaration_t *declaration)
{
	print_storage_class(declaration->storage_class);
	if(declaration->is_inline) {
		if (declaration->modifiers & DM_FORCEINLINE)
			fputs("__forceinline ", out);
		else
			fputs("inline ", out);
	}
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

bool is_constant_expression(const expression_t *expression)
{
	switch(expression->kind) {

	case EXPR_CONST:
	case EXPR_STRING_LITERAL:
	case EXPR_WIDE_STRING_LITERAL:
	case EXPR_SIZEOF:
	case EXPR_CLASSIFY_TYPE:
	case EXPR_FUNCTION:
	case EXPR_PRETTY_FUNCTION:
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
	case EXPR_UNARY_BITFIELD_EXTRACT:
	case EXPR_UNARY_ASSUME: /* has VOID type */
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
	case EXPR_UNARY_DEREFERENCE:
	case EXPR_UNARY_TAKE_ADDRESS:
	case EXPR_UNARY_CAST:
	case EXPR_UNARY_CAST_IMPLICIT:
		return is_constant_expression(expression->unary.value);

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

	case EXPR_CONDITIONAL:
		/* TODO: not correct, we only have to test expressions which are
		 * evaluated, which means either the true or false part might be not
		 * constant */
		return is_constant_expression(expression->conditional.condition)
			&& is_constant_expression(expression->conditional.true_expression)
			&& is_constant_expression(expression->conditional.false_expression);

	case EXPR_ARRAY_ACCESS:
		return is_constant_expression(expression->array_access.array_ref)
			&& is_constant_expression(expression->array_access.index);

	case EXPR_REFERENCE: {
		declaration_t *declaration = expression->reference.declaration;
		if(declaration->storage_class == STORAGE_CLASS_ENUM_ENTRY)
			return true;

		return false;
	}

	case EXPR_UNKNOWN:
	case EXPR_INVALID:
		break;
	}
	panic("invalid expression found (is constant expression)");
}


void init_ast(void)
{
	obstack_init(&ast_obstack);
}

void exit_ast(void)
{
	obstack_free(&ast_obstack, NULL);
}

void ast_set_output(FILE *stream)
{
	out = stream;
	type_set_output(stream);
}

void* (allocate_ast) (size_t size)
{
	return _allocate_ast(size);
}
