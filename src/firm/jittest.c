/*
 * This file is part of cparser.
 * Copyright (C) 2016 Matthias Braun <matze@braunis.de>
 */
#include "jittest.h"

#if defined(__APPLE__) || defined(__unix__)

#define _GNU_SOURCE
#include <libfirm/irprog.h>
#include <libfirm/jit.h>
#include <libfirm/typerep.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>

#include "adt/array.h"
#include "adt/panic.h"
#include "adt/strutil.h"
#include "adt/util.h"
#include "firm_opt.h"

void jit_compile_execute_main(void)
{
	unsigned const alignment  = 16;
	unsigned const align_mask = ~(alignment-1);

	optimize_lower_ir_prog(true);

	/* This is somewhat ad-hoc testing code for the jit, it is limited and
	 * will not handle all firm programs. */

	const struct {
		char const *name;
		void const *func;
	} external_functions[] = {
		{ "atoi",    (void const*)(intptr_t)atoi    },
		{ "free",    (void const*)(intptr_t)free    },
		{ "getchar", (void const*)(intptr_t)getchar },
		{ "malloc",  (void const*)(intptr_t)malloc  },
		{ "printf",  (void const*)(intptr_t)printf  },
		{ "puts",    (void const*)(intptr_t)puts    },
		{ "rand",    (void const*)(intptr_t)rand    },
		{ "realloc", (void const*)(intptr_t)realloc },
	};

	ir_jit_segment_t   *const segment   = be_new_jit_segment();
	ir_entity         **      funcents  = NEW_ARR_F(ir_entity*, 0);
	ir_jit_function_t **      functions = NEW_ARR_F(ir_jit_function_t*, 0);
	ir_entity          *main_entity     = NULL;

	size_t         code_size   = 0;
	ir_type *const global_type = get_glob_type();
	for (size_t i = 0, n = get_compound_n_members(global_type); i < n; ++i) {
		ir_entity  *const entity  = get_compound_member(global_type, i);
		char const *const ld_name = get_entity_ld_name(entity);

		if (is_method_entity(entity)
		 && !(get_entity_linkage(entity) & IR_LINKAGE_NO_CODEGEN)) {
			ir_graph *const irg = get_entity_irg(entity);
			if (irg != NULL) {
				ir_jit_function_t *const func = be_jit_compile(segment, irg);
				if (func == NULL)
					panic("Could not jit compile '%s'", ld_name);
				unsigned const misalign = alignment - (code_size%alignment);
				code_size += (misalign != alignment ? misalign : 0);

				ARR_APP1(ir_jit_function_t*, functions, func);
				ARR_APP1(ir_entity*, funcents, entity);
				code_size += be_get_function_size(func);
				if (streq(ld_name, "main") || streq(ld_name, "_main"))
					main_entity = entity;
				continue;
			}
		}

		/* See if its one of our well-known functions */
		for (unsigned f = 0; f < ARRAY_SIZE(external_functions); ++f) {
			char const *const name = external_functions[f].name;
			void const *const func = external_functions[f].func;
			if (streq(ld_name, name)
			 || (ld_name[0] == '_' && streq(&ld_name[1], name))) {
				be_jit_set_entity_addr(entity, func);
				break;
			}
		}
	}

	if (main_entity == NULL)
		panic("Could not find main() function");

	/* Allocate executable memory */
	char *const memory = mmap(0, code_size, PROT_READ | PROT_WRITE,
	                          MAP_PRIVATE | MAP_ANON, -1, 0);
	if (memory == NULL)
		panic("Could not mmap memory");

	/* Determine final function addresses */
	size_t const n_functions = ARR_LEN(functions);
	assert(ARR_LEN(funcents) == n_functions);
	unsigned offset = 0;
	for (size_t i = 0; i < n_functions; ++i) {
		offset = (offset + alignment - 1) & align_mask;
		char      *const dest   = memory + offset;
		ir_entity *const entity = funcents[i];
		be_jit_set_entity_addr(entity, dest);
		offset += be_get_function_size(functions[i]);
	}
	assert(offset == code_size);

	/* Emit and resolve */
	for (size_t i = 0; i < n_functions; ++i) {
		ir_entity  *const entity = funcents[i];
		char       *const dest
			= memory + ((char const*)be_jit_get_entity_addr(entity) - memory);
		be_emit_function(dest, functions[i]);
	}
	be_destroy_jit_segment(segment);

	if (mprotect(memory, code_size, PROT_EXEC) != 0)
		panic("Couldn't make memory executable");

	typedef int (*mainfunc)(int argc, char **argv);
	mainfunc main_ptr = (mainfunc)(intptr_t)be_jit_get_entity_addr(main_entity);
	int res = main_ptr(0, NULL);
	fprintf(stderr, "Exit code: %d\n", res);

	munmap(memory, code_size);
}

#else

#include "adt/panic.h"

/* We don't know how to allocate executable ememory on this system */
void jit_compile_execute_main(void)
{
	panic("jit compilation test not supported");
}

#endif
