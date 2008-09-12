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
#ifndef WARNING_H
#define WARNING_H

#include <stdbool.h>

void set_warning_opt(const char *opt);

typedef struct warning_t {
	bool aggregate_return:1;              /**< Warn if any functions that return structures or unions are defined or called */
	bool attribute:1;                     /**< Warn if an unexpected `__attribute__' is used or function attributes applied to variables, etc. */
#if 0 // TODO
	bool bad_function_cast:1;             /**< Warn whenever a function call is cast to a non-matching type */
	bool cast_align:1;                    /**< Warn whenever a pointer is cast such that the required alignment of the target is increased */
#endif
	bool cast_qual:1;                     /**< Warn whenever a pointer is cast so as to remove a type qualifier from the target type */
	bool char_subscripts:1;               /**< Warn if an array subscript has the type 'char' */
	bool comment:1;                       /**< Warn whenever a comment-start sequence appears in a comment, or whenever a Backslash-Newline appears in a '//' comment. */
#if 0 // TODO
	bool conversion:1;                    /**< Warn if a prototype causes a type conversion that is different from what would happen to the same argument in the absence of a prototype */
	bool cpp_compat:1;                    /**< Warn about ISO C constructs that are outside of the common subset of ISO C and ISO C++. */
#endif
	bool declaration_after_statement:1;   /**< Warn when a declaration is found after a statement in a block */
	bool deprecated_declarations:1;       /* TODO implement for types */ /**< Warn about uses of functions, variables and types marked as deprecated by using the 'deprecated' attribute */
	bool div_by_zero:1;                   /**< Warn about compile-time integer division by zero */
	bool empty_statement:1;               /**< Warn about empty statements, i.e. lone ';'  */
#if 0 // TODO
	bool endif_labels:1;                  /**< Warn whenever an '#else' or an '#endif' are followed by text */
#endif
	bool fatal_errors:1;                  /**< First error stops the compilation */
	bool float_equal:1;                   /**< Warn if floating point values are used in equality comparisons */
	bool format:1;                        /**< Check printf-style format strings */
	bool implicit_function_declaration:1; /**< Warn whenever a function is used before being declared */
	bool implicit_int:1;                  /**< Warn when a declaration does not specify a type */
	bool init_self:1;                     /**< Warn about uninitialized variables which are initialized with themselves. */
#if 0 // TODO
	bool inline:1;                        /**< Warn if a function can not be inlined and it was declared as inline */
	bool int_to_pointer_cast:1;           /**< Warn if cast from integer to pointer of different size. */
#endif
	bool long_long:1;                     /**< Warn if 'long long' type is used */
	bool main:1;                          /**< Warn if the type of 'main' is suspicious */
#if 0 // TODO
	bool missing_braces:1;                /**< Warn if an aggregate or union initializer is not fully bracketed */
#endif
	bool missing_declarations:1;          /**< Warn if a global function is defined without a previous declaration */
#if 0 // TODO
	bool missing_field_initializers:1;    /**< Warn if a structure's initializer has some fields missing. */
	bool missing_format_attribute:1;      /**< If '-Wformat' is enabled, also warn about functions which might be candidates for 'format' attributes */
#endif
	bool missing_noreturn:1;              /**< Warn about functions which might be candidates for attribute 'noreturn' */
	bool missing_prototypes:1;            /**< Warn if a global function is defined without a previous prototype declaration */
	bool multichar:1;                     /**< Warn if a multicharacter constant ('FOOF') is used. */
	bool nested_externs:1;                /**< Warn if an 'extern' declaration is encountered within a function. */
	bool nonnull:1;                       /**< Warn about passing a null pointer for arguments marked nonnull. */
#if 0 // TODO
	bool old_style_definition:1;          /**< Warn if an old-style function definition is used. */
	bool packed:1;                        /**< Warn if a structure is given the packed attribute, but the packed attribute has no effect on the layout or size of the structure */
	bool padded:1;                        /**< Warn if padding is included in a structure, either to align an element of the structure or to align the whole structure */
	bool parentheses:1;                   /**< Warn if parentheses are omitted in certain contexts (assignment where truth value is expected, if-else-braces) */
#endif
	bool pointer_arith:1;                 /**< Warn about anything that depends on the "size of" a function type or of 'void' */
#if 0 // TODO
	bool pointer_to_int_cast:1;           /**< Warn if cast from pointer to integer of different size. */
#endif
	bool redundant_decls:1;               /**< Warn about redundant declarations */
	bool return_type:1;                   /* TODO not fully implemented */ /**< Warn about function definitions with a return-type that defaults to 'int'.  Also warn about any 'return' statement with no return-value in a function whose return-type is not 'void'. */
	bool s_are_errors:1;                  /**< Treat warnings as errors */
#if 0 // TODO
	bool sequence_point:1;                /**< Warn about code that may have undefined semantics because of violations of sequence point rules */
#endif
	bool shadow:1;                        /**< Warn whenever a local variable shadows another local variable, parameter or global variable or whenever a built-in function is shadowed */
	bool sign_compare:1;                  /**< Warn when a comparison between signed and unsigned values could produce an incorrect result when the signed value is converted to unsigned */
#if 0 // TODO
	bool strict_aliasing:1;               /**< Warn about code which might break the strict aliasing rules that the compiler is using for optimization. */
#endif
	bool strict_prototypes:1;             /**< Warn if a function declaration has an unspecified parameter list */
	bool switch_default:1;                /**< Warn whenever a 'switch' statement does not have a 'default' case */
	bool switch_enum:1;                   /**< Warn about 'switch' statements with an enum as index type and missing case labels or case labels outside the enum range TODO has an alias -Wswitch? */
#if 0 // TODO
	bool traditional:1;                   /**< Warn about certain constructs that behave differently in traditional and ISO C */
	bool undef:1;                         /**< Warn if an undefined identifier is evaluated in an '#if' directive */
	bool uninitialized:1;                 /**< Warn if an automatic variable is used without being initialized or if a variable may be clobbered by a 'setjmp' call. */
#endif
	bool unknown_pragmas:1;               /**< Warn when a #pragma directive is encountered which is not understood */
	bool unreachable_code:1;              /**< Warn if the compiler detects that code will never be executed */
	bool unused_function:1;               /**< Warn whenever a static function is declared but not defined or a non-inline static function is unused */
	bool unused_label:1;                  /**< Warn whenever a label is declared but not used */
	bool unused_parameter:1;              /**< Warn whenever a function parameter is unused aside from its declaration */
	bool unused_value:1;                  /**< Warn whenever a statement computes a result that is explicitly not used */
	bool unused_variable:1;               /**< Warn whenever a local variable or non-constant static variable is unused aside from its declaration */
	bool write_strings:1;                 /**< Give string constants the type 'const char[LENGTH]' so that copying the address of one into a 'char *' pointer will get a warning */
} warning_t;

extern warning_t warning;

#endif
