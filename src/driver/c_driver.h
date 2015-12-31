/*
 * This file is part of cparser.
 * Copyright (C) 2014 Matthias Braun <matze@braunis.de>
 */
#ifndef C_DRIVER_H
#define C_DRIVER_H

#include "driver.h"

typedef enum lang_standard_t {
	STANDARD_DEFAULT, /* gnu99 (for C, GCC does gnu89) or gnu++98 (for C++) */
	STANDARD_ANSI,    /* ISO C90 (for C) or ISO C++ 1998 (for C++) */
	STANDARD_C89,     /* ISO C90 (sic) */
	STANDARD_C89AMD1, /* ISO C90 as modified in amendment 1 */
	STANDARD_C99,     /* ISO C99 */
	STANDARD_C11,     /* ISO C11 */
	STANDARD_GNU89,   /* ISO C90 plus GNU extensions (including some C99) */
	STANDARD_GNU99,   /* ISO C99 plus GNU extensions */
	STANDARD_GNU11,   /* ISO C11 plus GNU extensions */
	STANDARD_CXX98,   /* ISO C++ 1998 plus amendments */
	STANDARD_GNUXX98  /* ISO C++ 1998 plus amendments and GNU extensions */
} lang_standard_t;

extern struct obstack  cppflags_obst;
extern struct obstack  ldflags_obst;
extern struct obstack  asflags_obst;
extern struct obstack  codegenflags_obst;
extern struct obstack  c_cpp_cppflags_obst;
extern lang_standard_t standard;
extern unsigned        features_on;
extern unsigned        features_off;
extern bool            construct_dep_target;
extern bool            dump_defines;
extern bool            print_dependencies_instead_of_preprocessing;
extern bool            include_system_headers_in_dependencies;
extern bool            print_phony_targets;
extern const char     *dependency_file;
extern const char     *dependency_target;
extern bool            dont_escape_target;
extern const char     *dumpfunction;
/** -1: auto (use if not crosscompiling), 0 - no, 1 - yes */
extern int             driver_use_integrated_preprocessor;
extern bool            driver_verbose;
extern bool            driver_no_stdinc;
extern const char     *driver_default_exe_output;
extern const char     *print_file_name_file;

void record_cmdline_define(bool is_define, char const *define);

void set_default_handlers(void);

bool link_program(compilation_env_t *env, compilation_unit_t *units);
bool write_ir_file(compilation_env_t *env, compilation_unit_t *units);
bool dump_irg(compilation_env_t *env, compilation_unit_t *units);

bool print_preprocessing_tokens(compilation_env_t *env,
                                compilation_unit_t *unit);
bool generate_dependencies(compilation_env_t *env, compilation_unit_t *unit);
bool do_nothing(compilation_env_t *env, compilation_unit_t *unit);
bool do_print_ast(compilation_env_t *env, compilation_unit_t *unit);
bool do_parsing(compilation_env_t *env, compilation_unit_t *unit);
bool build_firm_ir(compilation_env_t *env, compilation_unit_t *unit);
bool generate_code_intermediate(compilation_env_t *env,
                                compilation_unit_t *unit);
bool generate_code_final(compilation_env_t *env, compilation_unit_t *unit);
bool assemble_intermediate(compilation_env_t *env, compilation_unit_t *unit);
bool assemble_final(compilation_env_t *env, compilation_unit_t *unit);

void init_default_driver(void);
void exit_default_driver(void);

#endif
