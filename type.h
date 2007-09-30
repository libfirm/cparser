#ifndef TYPE_H
#define TYPE_H

#include <stdio.h>
#include <stdbool.h>
#include "symbol.h"

typedef struct type_t              type_t;
typedef struct atomic_type_t       atomic_type_t;
typedef struct pointer_type_t      pointer_type_t;
typedef struct method_parameter_t  method_parameter_t;
typedef struct method_type_t       method_type_t;
typedef struct compound_type_t     compound_type_t;
typedef struct enum_type_t         enum_type_t;
typedef struct builtin_type_t      builtin_type_t;
typedef struct array_type_t        array_type_t;
typedef struct typedef_type_t      typedef_type_t;
typedef struct typeof_type_t       typeof_type_t;

void init_types(void);
void exit_types(void);

void print_type(type_t *type);

/**
 * prints a human readable form of @p type. prints an abstract typename
 * if symbol is NULL
 */
void print_type_ext(type_t *type, const symbol_t *symbol,
                    const context_t *context);

void print_enum_definition(const declaration_t *declaration);
void print_compound_definition(const declaration_t *declaration);

/**
 * set output stream for the type printer
 */
void type_set_output(FILE *out);

void inc_type_visited(void);

void set_print_compound_entries(bool enabled);


/**
 * returns true if type contains integer numbers
 */
bool is_type_integer(const type_t *type);

/**
 * returns true if type contains floating point numbers
 */
bool is_type_floating(const type_t *type);

/**
 * returns true if the type is valid. A type is valid if it contains no
 * unresolved references anymore and is not of TYPE_INVALID.
 */
bool type_valid(const type_t *type);

/**
 * returns true if the type is an arithmetic type (6.2.18)
 */
bool is_type_arithmetic(const type_t *type);

/**
 * returns true if the type is a scalar type (6.2.21)
 */
bool is_type_scalar(const type_t *type);

#endif
