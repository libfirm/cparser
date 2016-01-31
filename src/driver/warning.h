/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef WARNING_H
#define WARNING_H

#include <stdbool.h>

void set_warning_opt(const char *opt);

void disable_all_warnings(void);

void print_warning_opt_help(void);

#define WARNINGS(M) \
	M(WARN_ADDRESS,                       ON,  "address",                       "Warn about suspicious uses of memory addresses") \
	M(WARN_AGGREGATE_RETURN,              OFF, "aggregate-return",              "Warn if any functions that return structures or unions are defined or called") \
	M(WARN_ATTRIBUTE,                     ON,  "attribute",                     "Warn if an unexpected '__attribute__' is used or function attributes applied to variables, etc.") \
	M(WARN_BITFIELD_SIZE,                 ON,  "bitfield-size",                 "warn if enum bitfield size is too small for all entries") \
	M(WARN_BUILTIN_MACRO_REDEFINED,       ON,  "builtin-macro-redefined",       "warn when undefining/redefining builtin macros") \
	M(WARN_CAST_QUAL,                     OFF, "cast-qual",                     "Warn whenever a pointer is cast so as to remove a type qualifier from the target type") \
	M(WARN_CHAR_CTYPE,                    OFF, "char-ctype",                    "Warn if arguments to functions from the ctype.h header have type 'char'") \
	M(WARN_CHAR_SUBSCRIPTS,               ON,  "char-subscripts",               "Warn if an array subscript has the type 'char'") \
	M(WARN_COMMENT,                       OFF, "comment",                       "Warn whenever a comment-start sequence appears in a comment, or whenever a Backslash-Newline appears in a '//' comment.") \
	M(WARN_COMPAT_OPTION,                 ON,  "compat-option",                 "Warn when gcc switches are specified which cparser ignores.") \
	M(WARN_CONVERSION,                    OFF, "conversion",                    "Warn if a prototype causes a type conversion that is different from what would happen to the same argument in the absence of a prototype") \
	M(WARN_CPP,                           ON,  "cpp",                           "Show warning messages emitted by '#warning' directives") \
	M(WARN_DECLARATION_AFTER_STATEMENT,   OFF, "declaration-after-statement",   "Warn when a declaration is found after a statement in a block") \
	M(WARN_DEPRECATED_DECLARATIONS,       ON,  "deprecated-declarations",       "Warn about uses of functions, variables and types marked as deprecated by using the 'deprecated' attribute") /* TODO implement for types */ \
	M(WARN_DISTINCT_POINTER_TYPES,        ON,  "distinct-pointer-types",        "warn when comparing pointers to incompatible types") \
	M(WARN_DIV_BY_ZERO,                   ON,  "div-by-zero",                   "Warn about compile-time integer division by zero") \
	M(WARN_EMPTY_BODY,                    OFF, "empty-body",                    "Warn about an empty body of an if or else statement") \
	M(WARN_EMPTY_STATEMENT,               OFF, "empty-statement",               "Warn about empty statements, i.e. lone ';' ") \
	M(WARN_ENDIF_LABELS,                  ON,  "endif-labels",                  "Warn whenever an '#else' or an '#endif' are followed by text") \
	M(WARN_ENUM_CONVERSION,               ON,  "enum-conversion",               "warn about implicit conversion between different enum types") \
	M(WARN_ERROR,                         OFF, "error",                         "Treat warnings as errors") \
	M(WARN_EXPERIMENTAL,                  ON,  "experimental",                  "Warn if experimental/unstable compiler features are used") \
	M(WARN_FATAL_ERRORS,                  OFF, "fatal-errors",                  "First error stops the compilation") \
	M(WARN_FLOAT_EQUAL,                   OFF, "float-equal",                   "Warn if floating point values are used in equality comparisons") \
	M(WARN_FORMAT,                        ON,  "format",                        "Check printf-style format strings") \
	M(WARN_IGNORED_QUALIFIERS,            ON,  "ignored-qualifiers",            "Warn when type qualifiers are meaningless") \
	M(WARN_IMPLICIT_FUNCTION_DECLARATION, ON,  "implicit-function-declaration", "Warn whenever a function is used before being declared") \
	M(WARN_IMPLICIT_INT,                  ON,  "implicit-int",                  "Warn when a declaration does not specify a type") \
	M(WARN_INVALID_BYTE_SEQUENCE,         ON,  "invalid-byte-sequence",         "Warn on invalid byte sequences for chosen input charset") \
	M(WARN_LONG_LONG,                     OFF, "long-long",                     "Warn if 'long long' type is used") \
	M(WARN_MAIN,                          ON,  "main",                          "Warn if the type of 'main' is suspicious") \
	M(WARN_MISSING_INCLUDE_DIRS,          OFF, "missing-include-dirs",          "" /* TODO explanation */) \
	M(WARN_MISSING_NORETURN,              OFF, "missing-noreturn",              "Warn about functions which might be candidates for attribute 'noreturn'") \
	M(WARN_MISSING_PROTOTYPES,            OFF, "missing-prototypes",            "Warn if a global function is defined without a previous prototype declaration") \
	M(WARN_MISSING_VARIABLE_DECLARATIONS, OFF, "missing-variable-declarations", "Warn if a global variable is defined without a previous declaration") \
	M(WARN_MULTICHAR,                     ON,  "multichar",                     "Warn if a multicharacter constant ('FOOF') is used.") \
	M(WARN_NESTED_EXTERNS,                OFF, "nested-externs",                "Warn if an 'extern' declaration is encountered within a function.") \
	M(WARN_NONNULL,                       ON,  "nonnull",                       "Warn about passing a null pointer for arguments marked nonnull.") \
	M(WARN_NOT_COMPOUND_ASSIGN,           ON,  "not-compound-assign",           "Warn for constructs like x =+ 2 which are probably mistyped compound assignments.") \
	M(WARN_OLD_STYLE_DEFINITION,          OFF, "old-style-definition",          "Warn if an old-style function definition is used.") \
	M(WARN_OTHER,                         ON,  "other",                         "Warnings not covered by any other option") \
	M(WARN_PACKED,                        OFF, "packed",                        "Warn if a structure is given the packed attribute, but the packed attribute has no effect on the layout or size of the structure") \
	M(WARN_PADDED,                        OFF, "padded",                        "Warn if padding is included in a structure, either to align an element of the structure or to align the whole structure") \
	M(WARN_PARENTHESES,                   OFF, "parentheses",                   "Warn if parentheses are omitted in certain contexts (assignment where truth value is expected, if-else-braces)") \
	M(WARN_PEDANTIC,                      OFF, "pedantic",                      "Warn for cases where the c standard isn't stricly followed (but where we can easily continue)") \
	M(WARN_POINTER_ARITH,                 ON,  "pointer-arith",                 "Warn about anything that depends on the 'size of' a function type or of 'void'") \
	M(WARN_POINTER_SIGN,                  ON,  "pointer-sign",                  "gcc silently allows casting int* to unsigned* if this is off") \
	M(WARN_REDUNDANT_DECLS,               ON,  "redundant-decls",               "Warn about redundant declarations") \
	M(WARN_RETURN_LOCAL_ADDR,             ON,  "return-local-addr",             "Warn about returning a pointer (or in C++, a reference) to a variable that goes out of scope after the function returns.") \
	M(WARN_RETURN_TYPE,                   ON,  "return-type",                   "Warn about function definitions with a return-type that defaults to 'int'.  Also warn about any 'return' statement with no return-value in a function whose return-type is not 'void'.") \
	M(WARN_SHADOW,                        OFF, "shadow",                        "Warn whenever a local variable shadows another local variable, parameter or global variable or whenever a built-in function is shadowed") \
	M(WARN_SHADOW_LOCAL,                  OFF, "shadow-local",                  "" /* TODO explanation */) \
	M(WARN_SIGN_COMPARE,                  OFF, "sign-compare",                  "Warn when a comparison between signed and unsigned values could produce an incorrect result when the signed value is converted to unsigned") \
	M(WARN_STRAY_SEMICOLON,               ON,  "stray-semicolon",               "Warn about stray semicolons outside of functions") \
	M(WARN_STRICT_PROTOTYPES,             ON,  "strict-prototypes",             "Warn if a function declaration has an unspecified parameter list") \
	M(WARN_SWITCH_DEFAULT,                OFF, "switch-default",                "Warn whenever a 'switch' statement does not have a 'default' case") \
	M(WARN_SWITCH_ENUM,                   OFF, "switch-enum",                   "Warn about 'switch' statements with an enum as index type and missing case labels or case labels outside the enum range TODO has an alias -Wswitch?") \
	M(WARN_SYSTEM_HEADERS,                OFF, "system-headers",                "Show warnings in system headers.") \
	M(WARN_TRADITIONAL,                   OFF, "traditional",                   "Warn about certain constructs that behave differently in traditional and ISO C") \
	M(WARN_TRIGRAPHS,                     OFF, "trigraphs",                     "" /* TODO explanation */) \
	M(WARN_UNDEF,                         OFF, "undef",                         "Warn if an undefined identifier is evaluated in an '#if' directive") \
	M(WARN_UNINITIALIZED,                 ON,  "uninitialized",                 "Warn if an automatic variable is used without being initialized or if a variable may be clobbered by a 'setjmp' call.") \
	M(WARN_UNKNOWN_PRAGMAS,               ON,  "unknown-pragmas",               "Warn when a #pragma directive is encountered which is not understood") \
	M(WARN_UNKNOWN_WARNING_OPTION,        ON,  "unknown-warning-option",        "Warn whenever an unknown warning is requested") \
	M(WARN_UNREACHABLE_CODE,              ON,  "unreachable-code",              "Warn if the compiler detects that code will never be executed") \
	M(WARN_UNUSED_FUNCTION,               ON,  "unused-function",               "Warn whenever a static function is declared but not defined or a non-inline static function is unused") \
	M(WARN_UNUSED_LABEL,                  ON,  "unused-label",                  "Warn whenever a label is declared but not used") \
	M(WARN_UNUSED_OPTION,                 ON,  "unused-option",                 "Warn whenever a commandline option was not used") \
	M(WARN_UNUSED_PARAMETER,              ON,  "unused-parameter",              "Warn whenever a function parameter is unused aside from its declaration") \
	M(WARN_UNUSED_VALUE,                  ON,  "unused-value",                  "Warn whenever a statement computes a result that is explicitly not used") \
	M(WARN_UNUSED_VARIABLE,               ON,  "unused-variable",               "Warn whenever a local variable or non-constant static variable is unused aside from its declaration") \
	M(WARN_WRITE_STRINGS,                 OFF, "write-strings",                 "Give string constants the type 'const char[LENGTH]' so that copying the address of one into a 'char*' pointer will get a warning")

typedef enum warning_t {
#define M(warning, state, option, explanation) warning,
	WARNINGS(M)
#undef M
} warning_t;

typedef enum warn_state_t {
	WARN_STATE_NONE     = 0,
	WARN_STATE_ON       = 1U << 0,
	WARN_STATE_ERROR    = 1U << 1,
	WARN_STATE_NO_ERROR = 1U << 2
} warn_state_t;

typedef struct warning_switch_t {
	warn_state_t      state;
	char const *const name;
	char const *const explanation;
} warning_switch_t;

warning_switch_t const *get_warn_switch(warning_t);

static inline bool is_warn_on(warning_t const warn)
{
	return get_warn_switch(warn)->state & WARN_STATE_ON;
}

#endif
