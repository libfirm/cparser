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
#ifndef WARNING_H
#define WARNING_H

#include <stdbool.h>

void set_warning_opt(const char *opt);

void disable_all_warnings(void);

void print_warning_opt_help(void);

typedef enum warning_t {
	WARN_ADDRESS,                       /**< Warn about suspicious uses of memory addresses */
	WARN_AGGREGATE_RETURN,              /**< Warn if any functions that return structures or unions are defined or called */
	WARN_ATTRIBUTE,                     /**< Warn if an unexpected `__attribute__' is used or function attributes applied to variables, etc. */
#if 0 // TODO
	WARN_BAD_FUNCTION_CAST,             /**< Warn whenever a function call is cast to a non-matching type */
	WARN_CAST_ALIGN,                    /**< Warn whenever a pointer is cast such that the required alignment of the target is increased */
#endif
	WARN_CAST_QUAL,                     /**< Warn whenever a pointer is cast so as to remove a type qualifier from the target type */
	WARN_CHAR_SUBSCRIPTS,               /**< Warn if an array subscript has the type 'char' */
	WARN_COMMENT,                       /**< Warn whenever a comment-start sequence appears in a comment, or whenever a Backslash-Newline appears in a '//' comment. */
	WARN_CONVERSION,                    /**< Warn if a prototype causes a type conversion that is different from what would happen to the same argument in the absence of a prototype */
#if 0 // TODO
	WARN_CPP_COMPAT,                    /**< Warn about ISO C constructs that are outside of the common subset of ISO C and ISO C++. */
#endif
	WARN_DECLARATION_AFTER_STATEMENT,   /**< Warn when a declaration is found after a statement in a block */
	WARN_DEPRECATED_DECLARATIONS,       /* TODO implement for types */ /**< Warn about uses of functions, variables and types marked as deprecated by using the 'deprecated' attribute */
	WARN_DIV_BY_ZERO,                   /**< Warn about compile-time integer division by zero */
	WARN_EMPTY_BODY,                    /**< Warn about an empty body of an if or else statement */
	WARN_EMPTY_STATEMENT,               /**< Warn about empty statements, i.e. lone ';'  */
#if 0 // TODO
	WARN_ENDIF_LABELS,                  /**< Warn whenever an '#else' or an '#endif' are followed by text */
#endif
	WARN_ERROR,                         /**< Treat warnings as errors */
	WARN_FATAL_ERRORS,                  /**< First error stops the compilation */
	WARN_FLOAT_EQUAL,                   /**< Warn if floating point values are used in equality comparisons */
	WARN_FORMAT,                        /**< Check printf-style format strings */
	WARN_IGNORED_QUALIFIERS,            /**< Warn when type qualifiers are meaningless */
	WARN_IMPLICIT_FUNCTION_DECLARATION, /**< Warn whenever a function is used before being declared */
	WARN_IMPLICIT_INT,                  /**< Warn when a declaration does not specify a type */
#if 0 // TODO
	WARN_INLINE,                        /**< Warn if a function can not be inlined and it was declared as inline */
	WARN_INT_TO_POINTER_CAST,           /**< Warn if cast from integer to pointer of different size. */
#endif
	WARN_LONG_LONG,                     /**< Warn if 'long long' type is used */
	WARN_MAIN,                          /**< Warn if the type of 'main' is suspicious */
#if 0 // TODO
	WARN_MISSING_BRACES,                /**< Warn if an aggregate or union initializer is not fully bracketed */
#endif
	WARN_MISSING_DECLARATIONS,          /**< Warn if a global function is defined without a previous declaration */
#if 0 // TODO
	WARN_MISSING_FIELD_INITIALIZERS,    /**< Warn if a structure's initializer has some fields missing. */
	WARN_MISSING_FORMAT_ATTRIBUTE,      /**< If '-Wformat' is enabled, also warn about functions which might be candidates for 'format' attributes */
#endif
	WARN_MISSING_NORETURN,              /**< Warn about functions which might be candidates for attribute 'noreturn' */
	WARN_MISSING_PROTOTYPES,            /**< Warn if a global function is defined without a previous prototype declaration */
	WARN_MULTICHAR,                     /**< Warn if a multicharacter constant ('FOOF') is used. */
	WARN_NESTED_EXTERNS,                /**< Warn if an 'extern' declaration is encountered within a function. */
	WARN_NONNULL,                       /**< Warn about passing a null pointer for arguments marked nonnull. */
	WARN_OLD_STYLE_DEFINITION,          /**< Warn if an old-style function definition is used. */
	WARN_OTHER,                         /**< Warnings not covered by any other option */
	WARN_PACKED,                        /**< Warn if a structure is given the packed attribute, but the packed attribute has no effect on the layout or size of the structure */
	WARN_PADDED,                        /**< Warn if padding is included in a structure, either to align an element of the structure or to align the whole structure */
	WARN_PARENTHESES,                   /**< Warn if parentheses are omitted in certain contexts (assignment where truth value is expected, if-else-braces) */
	WARN_POINTER_ARITH,                 /**< Warn about anything that depends on the "size of" a function type or of 'void' */
#if 0 // TODO
	WARN_POINTER_TO_INT_CAST,           /**< Warn if cast from pointer to integer of different size. */
#endif
	WARN_REDUNDANT_DECLS,               /**< Warn about redundant declarations */
	WARN_RETURN_TYPE,                   /* TODO not fully implemented */ /**< Warn about function definitions with a return-type that defaults to 'int'.  Also warn about any 'return' statement with no return-value in a function whose return-type is not 'void'. */
#if 0 // TODO
	WARN_SEQUENCE_POINT,                /**< Warn about code that may have undefined semantics because of violations of sequence point rules */
#endif
	WARN_SHADOW,                        /**< Warn whenever a local variable shadows another local variable, parameter or global variable or whenever a built-in function is shadowed */
	WARN_SHADOW_LOCAL,
	WARN_SIGN_COMPARE,                  /**< Warn when a comparison between signed and unsigned values could produce an incorrect result when the signed value is converted to unsigned */
	WARN_STRAY_SEMICOLON,               /**< Warn about stray semicolons outside of functions */
#if 0 // TODO
	WARN_STRICT_ALIASING,               /**< Warn about code which might break the strict aliasing rules that the compiler is using for optimization. */
#endif
	WARN_STRICT_PROTOTYPES,             /**< Warn if a function declaration has an unspecified parameter list */
	WARN_SWITCH_DEFAULT,                /**< Warn whenever a 'switch' statement does not have a 'default' case */
	WARN_SWITCH_ENUM,                   /**< Warn about 'switch' statements with an enum as index type and missing case labels or case labels outside the enum range TODO has an alias -Wswitch? */
	WARN_TRADITIONAL,                   /**< Warn about certain constructs that behave differently in traditional and ISO C */
#if 0 // TODO
	WARN_UNDEF,                         /**< Warn if an undefined identifier is evaluated in an '#if' directive */
#endif
	WARN_UNINITIALIZED,                 /**< Warn if an automatic variable is used without being initialized or if a variable may be clobbered by a 'setjmp' call. */
	WARN_UNKNOWN_PRAGMAS,               /**< Warn when a #pragma directive is encountered which is not understood */
	WARN_UNREACHABLE_CODE,              /**< Warn if the compiler detects that code will never be executed */
	WARN_UNUSED_FUNCTION,               /**< Warn whenever a static function is declared but not defined or a non-inline static function is unused */
	WARN_UNUSED_LABEL,                  /**< Warn whenever a label is declared but not used */
	WARN_UNUSED_PARAMETER,              /**< Warn whenever a function parameter is unused aside from its declaration */
	WARN_UNUSED_VALUE,                  /**< Warn whenever a statement computes a result that is explicitly not used */
	WARN_UNUSED_VARIABLE,               /**< Warn whenever a local variable or non-constant static variable is unused aside from its declaration */
	WARN_WRITE_STRINGS,                 /**< Give string constants the type 'const char[LENGTH]' so that copying the address of one into a 'char *' pointer will get a warning */
} warning_t;

typedef enum warn_state_t {
	WARN_STATE_NONE     = 0,
	WARN_STATE_ON       = 1U << 0,
	WARN_STATE_ERROR    = 1U << 1,
	WARN_STATE_NO_ERROR = 1U << 2
} warn_state_t;

typedef struct warning_switch_t {
	warn_state_t      state;
	char const* const name;
} warning_switch_t;

warning_switch_t const *get_warn_switch(warning_t);

static inline bool is_warn_on(warning_t const warn)
{
	return get_warn_switch(warn)->state & WARN_STATE_ON;
}

#endif
