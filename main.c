#include <config.h>

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

#ifndef WITH_LIBCORE
#define WITH_LIBCORE
#endif

#include <libfirm/firm.h>
#include <libfirm/be.h>

#include "lexer.h"
#include "token_t.h"
#include "type_hash.h"
#include "parser.h"
#include "ast2firm.h"
#include "adt/error.h"
#include "write_fluffy.h"

#ifndef PREPROCESSOR
#define PREPROCESSOR "cpp -std=c99 -U__WCHAR_TYPE__ -D__WCHAR_TYPE__=int"
#endif

#ifndef LINKER
#define LINKER       "gcc"
#endif

#ifndef ASSEMBLER
#define ASSEMBLER "as"
#endif

#ifdef _WIN32
/* remap some names */
#define popen(cmd, mode)  _popen(cmd, mode)
#define pclose(file)      _pclose(file)
#endif /* _WIN32 */

static int  verbose;
static bool do_dump;

static const ir_settings_if_conv_t *if_conv_info = NULL;
static const backend_params        *be_params    = NULL;

static void initialize_firm(void)
{
	be_opt_register();
	firm_init_options(NULL, 0, NULL);

	firm_parameter_t params;
	memset(&params, 0, sizeof(params));

	params.size = sizeof(params);
	params.enable_statistics = 0;
	params.initialize_local_func = uninitialized_local_var;
	params.cc_mask = 0;
	params.builtin_dbg = NULL;

	/* initialize backend */
	be_params = be_init();
	ir_set_debug_retrieve(retrieve_dbg);
	params.arch_op_settings = be_params->arch_op_settings;
	if_conv_info            = be_params->if_conv_info;

	(void) if_conv_info; /* avoid unused warning */

	/* intialize firm itself */
	init_firm(&params);
	dbg_init(NULL, NULL, dbg_snprint);

	set_opt_constant_folding(1);
	set_opt_unreachable_code(1);
	set_opt_control_flow_straightening(1);
	set_opt_control_flow_weak_simplification(1);
	set_opt_control_flow_strong_simplification(1);
	set_opt_dyn_meth_dispatch(1);
	set_opt_normalize(1);
	set_opt_precise_exc_context(0);
	set_opt_strength_red(0);
	set_opt_fragile_ops(0);
	set_opt_optimize_class_casts(0);
	set_opt_suppress_downcast_optimization(0);
	set_opt_remove_confirm(1);
	set_opt_scalar_replacement(1);
	set_opt_ldst_only_null_ptr_exceptions(1);
	set_opt_alias_analysis(1);

	dump_consts_local(1);
	dump_keepalive_edges(1);
}

static void dump(ir_graph *irg, const char *suffix)
{
	if(do_dump) {
		dump_ir_block_graph(irg, suffix);
	}
}

static void get_output_name(char *buf, size_t buflen, const char *inputname,
                            const char *newext)
{
	size_t last_dot = 0xffffffff;
	size_t i = 0;

	if(inputname == NULL) {
		snprintf(buf, buflen, "a%s", newext);
		return;
	}

	for(const char *c = inputname; *c != 0; ++c) {
		if(*c == '.')
			last_dot = i;
		++i;
	}
	if(last_dot == 0xffffffff)
		last_dot = i;

	if(last_dot >= buflen)
		panic("filename too long");
	memcpy(buf, inputname, last_dot);

	size_t extlen = strlen(newext) + 1;
	if(extlen + last_dot >= buflen)
		panic("filename too long");
	memcpy(buf+last_dot, newext, extlen);
}

static translation_unit_t *do_parsing(FILE *const in, const char *const input)
{
	lexer_open_stream(in, input);
	translation_unit_t *unit = parse();
	return unit;
}

static void lextest(FILE *in, const char *fname)
{
	lexer_open_stream(in, fname);

	do {
		lexer_next_preprocessing_token();
		print_token(stdout, &lexer_token);
		puts("");
	} while(lexer_token.type != T_EOF);
}

static void emit(FILE *out, const char *input_name)
{
	be_main(out, input_name);
}

static FILE* preprocess(FILE* in, const char *fname)
{
	char buf[4096];

	if(in != stdin) {
		snprintf(buf, sizeof(buf), PREPROCESSOR " %s", fname);
	} else {
		/* read from stdin */
		snprintf(buf, sizeof(buf), PREPROCESSOR " -");
	}

	if(verbose) {
		puts(buf);
	}
	FILE* f = popen(buf, "r");
	if (f == NULL) {
		fprintf(stderr, "invoking preprocessor failed\n");
		exit(1);
	}
	return f;
}

static void do_link(const char *out, const char *in)
{
	char buf[4096];

	snprintf(buf, sizeof(buf), "%s %s -o %s", LINKER, in, out);
	if(verbose) {
		puts(buf);
	}
	int err = system(buf);
	if(err != 0) {
		fprintf(stderr, "linker reported an error\n");
		exit(1);
	}
}

static void assemble(const char *out, const char *in)
{
	char buf[4096];

	snprintf(buf, sizeof(buf), "%s %s -o %s", ASSEMBLER, in, out);
	if(verbose) {
		puts(buf);
	}

	int err = system(buf);
	if(err != 0) {
		fprintf(stderr, "assembler reported an error\n");
		exit(1);
	}
}

static const char *try_dir(const char *dir)
{
	if(dir == NULL)
		return dir;
	if(access(dir, R_OK | W_OK | X_OK) == 0)
		return dir;
	return NULL;
}

static const char *get_tempdir(void)
{
	static const char *tmpdir = NULL;

	if(tmpdir != NULL)
		return tmpdir;

	if(tmpdir == NULL)
		tmpdir = try_dir(getenv("TMPDIR"));
	if(tmpdir == NULL)
		tmpdir = try_dir(getenv("TMP"));
	if(tmpdir == NULL)
		tmpdir = try_dir(getenv("TEMP"));

#ifdef P_tmpdir
	if(tmpdir == NULL)
		tmpdir = try_dir(P_tmpdir);
#endif

	if(tmpdir == NULL)
		tmpdir = try_dir("/var/tmp");
	if(tmpdir == NULL)
		tmpdir = try_dir("/usr/tmp");
	if(tmpdir == NULL)
		tmpdir = try_dir("/tmp");

	if(tmpdir == NULL)
		tmpdir = ".";

	return tmpdir;
}

/**
 * an own version of tmpnam, which: writes in a buffer, appends a user specified
 * suffix, emits no warnings during linking (like glibc/gnu ld do for tmpnam)...
 */
static FILE *make_temp_file(char *buffer, size_t buflen,
                            const char *prefix, const char *suffix)
{
	const char *tempdir = get_tempdir();

	/* oh well... mkstemp doesn't accept a suffix after XXXXXX... */
	(void) suffix;
	suffix = "";

	snprintf(buffer, buflen, "%s/%sXXXXXX%s", tempdir, prefix, suffix);

	int fd = mkstemp(buffer);
	if(fd == -1) {
		fprintf(stderr, "couldn't create temporary file: %s\n",
		        strerror(errno));
		exit(1);
	}
	FILE *out = fdopen(fd, "w");
	if(out == NULL) {
		fprintf(stderr, "couldn't create temporary file FILE*\n");
		exit(1);
	}

	return out;
}

static void create_firm_prog(translation_unit_t *unit)
{
	translation_unit_to_firm(unit);

	//dump_globals_as_text(dump_verbosity_max, "-globals");

	int n_irgs = get_irp_n_irgs();
	for(int i = 0; i < n_irgs; ++i) {
		ir_graph *const irg = get_irp_irg(i);
		dump(irg, "-start");
	}

	lower_params_t params;

	params.def_ptr_alignment    = 4;
	params.flags                = LF_COMPOUND_RETURN | LF_RETURN_HIDDEN;
	params.hidden_params        = ADD_HIDDEN_ALWAYS_IN_FRONT;
	params.find_pointer_type    = NULL;
	params.ret_compound_in_regs = NULL;
	lower_calls_with_compounds(&params);

	lower_highlevel();

	for(int i = 0; i < n_irgs; ++i) {
		ir_graph *const irg = get_irp_irg(i);
		dump(irg, "-lower");
	}
}

static void optimize(void)
{
	int         arr_len;
	ir_entity **keep_methods;

	cgana(&arr_len, &keep_methods);
	gc_irgs(arr_len, keep_methods);
	free(keep_methods);

	optimize_funccalls(0);

	lwrdw_param_t lwrdw_param = {
		1,
		1,
		mode_Ls, mode_Lu,
		mode_Is, mode_Iu,
		def_create_intrinsic_fkt,
		NULL
	};
	if (be_params->arch_create_intrinsic_fkt) {
		lwrdw_param.create_intrinsic = be_params->arch_create_intrinsic_fkt;
		lwrdw_param.ctx              = be_params->create_intrinsic_ctx;
	}

	for(int i = 0; i < get_irp_n_irgs(); ++i) {
		ir_graph *irg = get_irp_irg(i);

		optimize_graph_df(irg);
		dump(irg, "-01-localopt");
		place_code(irg);
		dump(irg, "-02-place");
		optimize_cf(irg);
		dump(irg, "-03-cf");
		lower_dw_ops(&lwrdw_param);
		dump(irg, "-04-dw");
		optimize_graph_df(irg);
		dump(irg, "-05-localopt");
		optimize_cf(irg);
		dump(irg, "-06-cf");
	}
}

typedef enum compile_mode_t {
	Compile,
	CompileDump,
	CompileAssemble,
	CompileAssembleLink,
	LexTest,
	PrintAst,
	PrintFluffy
} compile_mode_t;

static void usage(const char *argv0)
{
	fprintf(stderr, "Usage %s input [-o output] [-c]\n", argv0);
}

int main(int argc, char **argv)
{
	initialize_firm();

	init_symbol_table();
	init_tokens();
	init_types();
	init_typehash();
	init_lexer();
	init_ast();
	init_parser();
	init_ast2firm();

	const char *input        = NULL;
	const char *outname      = NULL;
	const char *dumpfunction = NULL;
	compile_mode_t mode = CompileAssembleLink;

	for(int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if(strcmp(arg, "-o") == 0) {
			++i;
			if(i >= argc) {
				usage(argv[0]);
				return 1;
			}
			outname = argv[i];
		} else if(strcmp(arg, "-c") == 0) {
			mode = CompileAssemble;
		} else if(strcmp(arg, "-S") == 0) {
			mode = Compile;
		} else if(strcmp(arg, "--lextest") == 0) {
			mode = LexTest;
		} else if(strcmp(arg, "--print-ast") == 0) {
			mode = PrintAst;
		} else if(strcmp(arg, "--print-fluffy") == 0) {
			mode = PrintFluffy;
		} else if(strcmp(arg, "--dump") == 0) {
			do_dump = true;
		} else if(strcmp(arg, "--dump-function") == 0) {
			++i;
			if(i >= argc) {
				usage(argv[0]);
				return 1;
			}
			dumpfunction = argv[i];
			mode         = CompileDump;
		} else if(strcmp(arg, "-v") == 0) {
			verbose = 1;
		} else if(arg[0] == '-' && arg[1] == 'f') {
			const char *opt = &arg[2];
			if(opt[0] == 0) {
				++i;
				if(i >= argc) {
					usage(argv[0]);
					return 1;
				}
				opt = argv[i];
				if(opt[0] == '-') {
					usage(argv[0]);
					return 1;
				}
			}
			//firm_option(opt);
		} else if(arg[0] == '-' && arg[1] == 'b') {
			const char *opt = &arg[2];
			if(opt[0] == 0) {
				++i;
				if(i >= argc) {
					usage(argv[0]);
					return 1;
				}
				opt = argv[i];
				if(opt[0] == '-') {
					usage(argv[0]);
					return 1;
				}
			}
			//be_parse_arg(opt);
		} else if(arg[0] == '-') {
			if (arg[1] == '\0') {
				input = "-";
			} else if (arg[1] == 'D' ||
					arg[1] == 'O' ||
					arg[1] == 'f' ||
					arg[1] == 'W' ||
					arg[1] == 'g' ||
					strncmp(arg + 1, "std=", 4) == 0) {
				fprintf(stderr, "Warning: Ignoring option '%s'\n", arg);
			} else {
				usage(argv[0]);
				return 1;
			}
		} else {
			if(input != NULL) {
				fprintf(stderr, "Error: multiple input files specified\n");
				usage(argv[0]);
				return 1;
			} else {
				input = arg;
			}
		}
	}

	FILE *out;
	char  outnamebuf[4096];
	if(outname == NULL) {
		switch(mode) {
		case PrintAst:
		case PrintFluffy:
		case LexTest:
			break;
		case Compile:
			get_output_name(outnamebuf, sizeof(outnamebuf), input, ".s");
			outname = outnamebuf;
			break;
		case CompileAssemble:
			get_output_name(outnamebuf, sizeof(outnamebuf), input, ".o");
			outname = outnamebuf;
			break;
		case CompileDump:
			get_output_name(outnamebuf, sizeof(outnamebuf), dumpfunction,
			                ".vcg");
			outname = outnamebuf;
			break;
		case CompileAssembleLink:
			outname = "a.out";
			break;
		}
		assert(outname != NULL);
	}

	if(strcmp(outname, "-") == 0) {
		out = stdout;
	} else {
		out = fopen(outname, "w");
		if(out == NULL) {
			fprintf(stderr, "Couldn't open '%s' for writing: %s\n", outname,
			        strerror(errno));
			return 1;
		}
	}

	FILE *in;
	if(input == NULL) {
		fprintf(stderr, "%s: no input files\n", argv[0]);
		return 1;
	} else if(strcmp(input, "-") == 0) {
		in    = stdin;
		input = "<stdin>";
	} else {
		in = fopen(input, "r");
		if(in == NULL) {
			fprintf(stderr, "Couldn't open '%s': %s\n", input, strerror(errno));
			return 1;
		}
	}

	if(mode == LexTest) {
		lextest(in, input);
		fclose(in);
		return 0;
	}

	FILE *preprocessed_in = preprocess(in, input);
	translation_unit_t *const unit = do_parsing(preprocessed_in, input);
	pclose(preprocessed_in);
	if(unit == NULL)
		return 1;

	if(mode == PrintAst) {
		ast_set_output(out);
		print_ast(unit);
		return 0;
	}
	if(mode == PrintFluffy) {
		write_fluffy_decls(out, unit);
	}

	create_firm_prog(unit);
	optimize();

	if(mode == CompileDump) {
		/* find irg */
		ident    *id     = new_id_from_str(dumpfunction);
		ir_graph *irg    = NULL;
		int       n_irgs = get_irp_n_irgs();
		for(int i = 0; i < n_irgs; ++i) {
			ir_graph *tirg   = get_irp_irg(i);
			ident    *irg_id = get_entity_ident(get_irg_entity(tirg));
			if(irg_id == id) {
				irg = tirg;
				break;
			}
		}

		if(irg == NULL) {
			fprintf(stderr, "No graph for function '%s' found\n", dumpfunction);
			return 1;
		}

		dump_ir_block_graph_file(irg, out);
		fclose(out);
		return 0;
	}

	/* generate code and emit assembler */
	FILE *asm_out;
	char  asm_tempfile[1024];
	if(mode == Compile) {
		asm_out = out;
	} else {
		asm_out
			= make_temp_file(asm_tempfile, sizeof(asm_tempfile), "cc", ".s");
	}
	emit(asm_out, input);
	fclose(asm_out);

	/* assemble assembler and create object file */
	char obj_tfile[1024];
	if(mode == CompileAssemble || mode == CompileAssembleLink) {
		const char *obj_outfile;
		if(mode == CompileAssemble) {
			fclose(out);
			obj_outfile = outname;
		} else {
			FILE *tempf
				= make_temp_file(obj_tfile, sizeof(obj_tfile), "cc", ".o");
			fclose(tempf);
			obj_outfile = obj_tfile;
		}

		assemble(obj_outfile, asm_tempfile);
	}

	/* link object file */
	if(mode == CompileAssembleLink) {
		do_link(outname, obj_tfile);
	}

	exit_ast2firm();
	exit_parser();
	exit_ast();
	exit_lexer();
	exit_typehash();
	exit_types();
	exit_tokens();
	exit_symbol_table();
	return 0;
}
