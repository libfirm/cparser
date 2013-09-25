/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#ifndef DRIVER_H
#define DRIVER_H

#include "adt/obst.h"
#include "ast.h"
#include "input.h"
#include "tempfile.h"

typedef enum compile_mode_t {
	MODE_BENCHMARK_PARSER,
	MODE_PREPROCESS_ONLY,
	MODE_GENERATE_DEPENDENCIES,
	MODE_PARSE_ONLY,
	MODE_COMPILE,
	MODE_COMPILE_DUMP,
	MODE_COMPILE_EXPORTIR,
	MODE_COMPILE_ASSEMBLE,
	MODE_COMPILE_ASSEMBLE_LINK,
	MODE_PRINT_AST,
	MODE_PRINT_FLUFFY,
	MODE_PRINT_JNA,
	MODE_PRINT_COMPOUND_SIZE,
} compile_mode_t;

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
	COMPILATION_UNIT_ASSEMBLER,
	COMPILATION_UNIT_PREPROCESSED_ASSEMBLER,
	COMPILATION_UNIT_OBJECT,
	COMPILATION_UNIT_IR,
	COMPILATION_UNIT_DEPENDENCIES,
	COMPILATION_UNIT_UNKNOWN
} compilation_unit_type_t;

typedef struct compilation_unit_t compilation_unit_t;
struct compilation_unit_t {
	const char             *name;  /**< filename or "-" for stdin */
	FILE                   *input; /**< input (NULL if not opened yet) */
	input_t                *input_decoder;
	bool                    input_is_pipe;
	compilation_unit_type_t type;
	lang_standard_t         standard;
	translation_unit_t     *ast;
	compilation_unit_t     *next;
};

extern compile_mode_t  mode;
extern const char     *outname;
extern struct obstack  cppflags_obst;
extern struct obstack  ldflags_obst;
extern struct obstack  asflags_obst;
extern struct obstack  codegenflags_obst;
extern struct obstack  c_cpp_cppflags_obst;
extern unsigned        features_on;
extern unsigned        features_off;
extern bool            construct_dep_target;
extern bool            dump_defines;
extern bool            produce_statev;
extern lang_standard_t standard;
extern int             colorterm;
extern const char     *filtev;
extern const char     *dumpfunction;
extern const char     *driver_linker;
extern const char     *driver_preprocessor;
extern const char     *driver_assembler;
extern const char     *driver_default_exe_output;
extern bool            driver_use_external_preprocessor;
extern bool            driver_verbose;
extern bool            driver_no_stdinc;
extern bool            do_timing;
extern bool            print_timing;

void init_c_dialect(bool is_cpp, lang_standard_t standard);

compilation_unit_type_t get_unit_type_from_string(const char *string);

void driver_add_input(const char *filename, compilation_unit_type_t type);
void driver_add_flag(struct obstack *obst, const char *format, ...);

void driver_print_file_name(const char *name);

int driver_go(void);

void init_driver(void);
void exit_driver(void);

#endif
