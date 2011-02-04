/**
 * @file firm_codegen.c
 *
 * Compile when BACK_END_IS_CP_FIRM_BE is defined
 *
 * (C) 2005-2009  Michael Beck  beck@ipd.info.uni-karlsruhe.de
 *
 * $Id$
 */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <libfirm/dbginfo.h>
#include <libfirm/adt/xmalloc.h>
#include <libfirm/be.h>

#ifdef FIRM2C_BACKEND
#include "cbackend.h"
#endif

#include "firm_codegen.h"
#include "firm_cmdline.h"
#include "firm_opt.h"
#include "firm_timing.h"

/**
 * Substitutes '.c' for '.s'.
 */
static char *generate_asm_file_name(const char *file_name) {
  int len             = strlen(file_name);
  char *asm_file_name = xmalloc(len + 5);   /* .asm + \0 */

  strcpy(asm_file_name, file_name);
  if (asm_file_name[len - 2] == '.' && asm_file_name[len - 1] == 'c')
    asm_file_name[len - 2] = '\0';

  switch (firm_be_opt.selection) {
#ifdef FIRM2C_BACKEND
  case BE_FIRM2C:
    strncat(asm_file_name, ".s.c", 4);
    break;
#endif

  case BE_FIRM_BE:
    strncat(asm_file_name, ".s", 4);
    break;
  }

  return asm_file_name;
}  /* generate_asm_file_name */

/**
 * Calls the specified backend.
 * Code is written to file <file_name> ('.c' is substituted for '.asm')
 */
void do_codegen(FILE *out, const char *file_name)
{
	FILE *close_out = NULL;
	if (out == NULL) {
		char *asm_file_name = generate_asm_file_name(file_name);

		if ((out = fopen(asm_file_name, "w")) == NULL) {
			fprintf(stderr, "Could not open output file %s\n", asm_file_name);
			exit(1);
		}
		free(asm_file_name);
		close_out = out;
	}

	switch (firm_be_opt.selection) {
#ifdef FIRM2C_BACKEND
		case BE_FIRM2C: {
			ir_timer_t *timer = ir_timer_new();
			timer_register(timer, "Firm: C-generating backend");
			timer_start(timer);
			generate_code_file(out);
			timer_stop(timer);
			break;
		}
#endif

		case BE_FIRM_BE: {
			ir_timer_t *timer = ir_timer_new();
			timer_register(timer, "Firm: backend");
			timer_start(timer);
			be_main(out, file_name);
			timer_stop(timer);
			break;
		}

		default:
			fprintf(stderr, "Fatal: Unknown backend %d\n", firm_be_opt.selection);
	} /* switch (firm_be_opt.selection) */

	if (close_out)
		fclose(close_out);
}
