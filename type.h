#ifndef TYPE_H
#define TYPE_H

#include <stdio.h>

typedef struct type_t                   type_t;
typedef struct atomic_type_t            atomic_type_t;
typedef struct pointer_type_t           pointer_type_t;
typedef struct method_parameter_type_t  method_parameter_type_t;
typedef struct method_type_t            method_type_t;
typedef struct compound_entry_t         compound_entry_t;
typedef struct compound_type_t          compound_type_t;
typedef struct enum_type_t              enum_type_t;
typedef struct builtin_type_t           builtin_type_t;

void init_types(void);
void exit_types(void);

/**
 * prints a human readable form of @p type to a stream
 */
void print_type(FILE* out, const type_t *type);

/**
 * returns 1 if type contains integer numbers
 */
int is_type_int(const type_t *type);

/**
 * returns 1 if the type is valid. A type is valid if it contains no unresolved
 * references anymore and is not of TYPE_INVALID.
 */
int type_valid(const type_t *type);

#endif
