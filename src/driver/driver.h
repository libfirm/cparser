/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#ifndef DRIVER_H
#define DRIVER_H

#include <stdbool.h>
#include "adt/obst.h"
#include "tempfile.h"

typedef enum compilation_unit_type_t {
	COMPILATION_UNIT_AUTODETECT,
	COMPILATION_UNIT_C,
	COMPILATION_UNIT_PREPROCESSED_C,
	COMPILATION_UNIT_CXX,
	COMPILATION_UNIT_PREPROCESSED_CXX,
	COMPILATION_UNIT_LEXER_TOKENS_C,
	COMPILATION_UNIT_LEXER_TOKENS_CXX,
	COMPILATION_UNIT_LEXER_TOKENS_ASSEMBLER,
	COMPILATION_UNIT_AST,
	COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION,
	COMPILATION_UNIT_INTERMEDIATE_REPRESENTATION_OPTIMIZED,
	COMPILATION_UNIT_ASSEMBLER,
	COMPILATION_UNIT_PREPROCESSED_ASSEMBLER,
	COMPILATION_UNIT_OBJECT,
	COMPILATION_UNIT_IR,
	COMPILATION_UNIT_DEPENDENCIES,
	COMPILATION_UNIT_UNKNOWN,
	COMPILATION_UNIT_LAST = COMPILATION_UNIT_UNKNOWN
} compilation_unit_type_t;

typedef struct compilation_unit_t compilation_unit_t;
typedef struct compilation_env_t  compilation_env_t;

typedef bool (*compilation_unit_handler)(compilation_env_t *,
                                         compilation_unit_t*);

void set_unit_handler(compilation_unit_type_t type,
                      compilation_unit_handler handler, bool stop_after);

bool process_unit(compilation_env_t *env, compilation_unit_t *unit);
bool process_all_units(compilation_env_t *env);

compilation_unit_type_t autodetect_input(char const *path);

extern const char         *outname;
extern bool                produce_statev;
extern const char         *filtev;
extern int                 colorterm;
extern bool                do_timing;
extern bool                print_timing;
extern struct obstack      file_obst;
extern compilation_unit_t *units;

void driver_add_input(const char *filename, compilation_unit_type_t type);
void driver_add_flag(struct obstack *obst, const char *format, ...);

FILE *open_temp_file(const char *basename, const char *extension,
                     const char **final_name);
const char *get_output_name(const char *inputname, const char *newext);

bool open_input(compilation_unit_t *unit);
bool close_input(compilation_unit_t *unit);

bool open_output_for_unit(compilation_env_t *env, compilation_unit_t *unit,
                          const char *default_extension);
bool open_output(compilation_env_t *env);
void close_output(compilation_env_t *env);

void copy_file(FILE *dest, FILE *input);
bool do_copy_file(compilation_env_t *env, compilation_unit_t *unit);

void begin_statistics(void);
void end_statistics(void);

int action_print_file_name(const char *argv0);
int action_compile(const char *argv0);

void init_driver(void);
void exit_driver(void);

#endif
