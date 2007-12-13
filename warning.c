#include <stdio.h>
#include <string.h>
#include "warning.h"

void set_warning_opt(const char *const opt)
{
	const char* s = opt;

	bool state = true;
	/* no- modifier */
	if (s[0] == 'n' && s[1] == 'o' && s[2] == '-') {
		s += 3;
		state = false;
	}

	if (0) {}
#define OPTX(x)   else if (strcmp(s, x) == 0)
#define SET(y)    warning.y = state;
#define OPT(x, y) OPTX(x) SET(y)
	OPT("char-subscripts",               char_subscripts)
	OPT("error",                         s_are_errors)
	OPT("fatal-errors",                  fatal_errors)
	OPT("format",                        check_format)
	OPTX("implicit") {
		SET(implicit_function_declaration)
		SET(implicit_int)
	}
	OPT("implicit-function-declaration", implicit_function_declaration)
	OPT("implicit-int",                  implicit_int)
	OPT("missing-declarations",          missing_declarations)
	OPT("redundant-decls",               redundant_decls)
	OPT("strict-prototypes",             strict_prototypes)
#if 0
	OPTX("unused") {
		SET(unused_function)
		SET(unused_label)
		SET(unused_parameter)
		SET(unused_variable)
		SET(unused_value)
	}
#endif
#undef OPT
	else {
		fprintf(stderr, "warning: ignoring unknown option -W%s\n", opt);
	}
}

warning_t warning = {
	.char_subscripts               = true,
	.check_format                  = true,
	.implicit_function_declaration = true,
	.implicit_int                  = true,
	.missing_declarations          = true,
	.strict_prototypes             = true,
	.redundant_decls               = true
};
