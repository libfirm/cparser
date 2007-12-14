#ifndef WARNING_H
#define WARNING_H

#include <stdbool.h>

void set_warning_opt(const char *opt);

typedef struct warning_t {
#if 0 // TODO
	bool aggregate_return:1;              /* Warn if any functions that return structures or unions are defined or called */
	bool bad_function_cast:1;             /* Warn whenever a function call is cast to a non-matching type */
#endif
	bool char_subscripts:1;               /* Warn if an array subscript has the type 'char' */
	bool check_format:1;                  /* Check printf-style format strings */
#if 0 // TODO
	bool cast_align:1;                    /* Warn whenever a pointer is cast such that the required alignment of the target is increased */
	bool cast_qual:1;                     /* Warn whenever a pointer is cast so as to remove a type qualifier from the target type */
	bool conversion:1;                    /* Warn if a prototype causes a type conversion that is different from what would happen to the same argument in the absence of a prototype */
	bool declaration_after_statement:1;   /* Warn when a declaration is found after a statement in a block */
	bool deprecated_declarations:1;       /* Warn about uses of functions, variables and types marked as deprecated by using the 'deprecated' attribute */
	bool div_by_zero:1;                   /* Warn about compile-time integer division by zero */
#endif
	bool empty_statement:1;               /* Warn about empty statements, i.e. lone ';'  */
#if 0 // TODO
	bool endif_labels:1;                  /* Warn whenever an '#else' or an '#endif' are followed by text */
#endif
	bool fatal_errors:1;                  /* First error stops the compilation */
#if 0 // TODO
	bool float_equal:1;                   /* Warn if floating point values are used in equality comparisons */
#endif
	bool implicit_function_declaration:1; /* Warn whenever a function is used before being declared */
	bool implicit_int:1;                  /* Warn when a declaration does not specify a type */
#if 0 // TODO
	bool inline:1;                        /* Warn if a function can not be inlined and it was declared as inline */
	bool long_long:1;                     /* Warn if 'long long' type is used */
	bool main:1;                          /* Warn if the type of 'main' is suspicious */
	bool missing_braces:1;                /* Warn if an aggregate or union initializer is not fully bracketed */
#endif
	bool missing_declarations:1;          /* Warn if a global function is defined without a previous declaration */
#if 0 // TODO
	bool missing_format_attribute:1;      /* If '-Wformat' is enabled, also warn about functions which might be candidates for 'format' attributes */
	bool missing_noreturn:1;              /* Warn about functions which might be candidates for attribute 'noreturn' */
#endif
	bool missing_prototypes:1;            /* Warn if a global function is defined without a previous prototype declaration */
#if 0 // TODO
	bool multichar:1;                     /* Warn if a multicharacter constant ('FOOF') is used. */
	bool nested_externs:1;                /* Warn if an 'extern' declaration is encountered within a function */
	bool packed:1;                        /* Warn if a structure is given the packed attribute, but the packed attribute has no effect on the layout or size of the structure */
	bool padded:1;                        /* Warn if padding is included in a structure, either to align an element of the structure or to align the whole structure */
	bool parentheses:1;                   /* Warn if parentheses are omitted in certain contexts (assignment where truth value is expected, if-else-braces) */
	bool pointer_arith:1;                 /* Warn about anything that depends on the "size of" a function type or of 'void' */
#endif
	bool redundant_decls:1;               /* Warn about redundant declarations */
#if 0 // TODO
	bool return_type:1;                   /* Warn about function definitions with a return-type that defaults to 'int'.  Also warn about any 'return' statement with no return-value in a function whose return-type is not 'void'. */
#endif
	bool s_are_errors:1;                  /* Treat warnings as errors */
#if 0 // TODO
	bool sequence_point:1;                /* Warn about code that may have undefined semantics because of violations of sequence point rules */
	bool shadow:1;                        /* Warn whenever a local variable shadows another local variable, parameter or global variable or whenever a built-in function is shadowed */
	bool sign_compare:1;                  /* Warn when a comparison between signed and unsigned values could produce an incorrect result when the signed value is converted to unsigned */
	bool strict_aliasing:1;               /* Warn about code which might break the strict aliasing rules that the compiler is using for optimization. */
#endif
	bool strict_prototypes:1;             /* warn if a function declaration has an unspecified parameter list */
	bool switch_default:1;                /* Warn whenever a 'switch' statement does not have a 'default' case */
#if 0 // TODO
	bool switch_enum:1;                   /* Warn about 'switch' statements with an enum as index type and missing case labels or case labels outside the enum range TODO has an alias -Wswitch? */
	bool traditional:1;                   /* Warn about certain constructs that behave differently in traditional and ISO C */
	bool undef:1;                         /* Warn if an undefined identifier is evaluated in an '#if' directive */
	bool uninitialized:1;                 /* Warn if an automatic variable is used without being initialized or if a variable may be clobbered by a 'setjmp' call. */
#endif
	bool unknown_pragmas:1;               /* Warn when a #pragma directive is encountered which is not understood */
#if 0 // TODO
	bool unreachable_code:1;              /* Warn if the compiler detects that code will never be executed */
	bool unused_function:1;               /* Warn whenever a static function is declared but not defined or a non-inline static function is unused */
	bool unused_label:1;                  /* Warn whenever a label is declared but not used */
	bool unused_parameter:1;              /* Warn whenever a function parameter is unused aside from its declaration */
	bool unused_variable:1;               /* Warn whenever a local variable or non-constant static variable is unused aside from its declaration */
#endif
	bool unused_value:1;                  /* Warn whenever a statement computes a result that is explicitly not used */
#if 0 // TODO
	bool write_strings:1;                 /* Give string constants the type 'const char[LENGTH]' so that copying the address of one into a 'char *' pointer will get a warning */
#endif
} warning_t;

extern warning_t warning;

#endif
