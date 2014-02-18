# include user-defined makefile settings
-include config.mak

top_srcdir   ?= .
top_builddir ?= build
VPATH        ?= $(top_srcdir)

include config.default.mak

variant  ?= debug# Different libfirm variants (debug, optimize, profile, coverage)
srcdir   ?= $(top_srcdir)
builddir ?= $(top_builddir)/$(variant)

CPPFLAGS  = -I$(top_srcdir) -I$(builddir) $(SYSTEM_INCLUDE_DIR) $(LOCAL_INCLUDE_DIR) $(COMPILER_INCLUDE_DIR) $(MULTILIB_INCLUDE_DIR) $(MULTILIB_M32_TRIPLE) $(MULTILIB_M64_TRIPLE) $(HOST_TRIPLE)
CPPFLAGS += $(FIRM_CPPFLAGS)

CFLAGS += -Wall -W -Wstrict-prototypes -Wmissing-prototypes -std=c99 -pedantic
CFLAGS_debug    = -O0 -g
CFLAGS_optimize = -O3 -fomit-frame-pointer -DNDEBUG
CFLAGS_profile  = -pg -O3 -fno-inline
CFLAGS_coverage = --coverage -O0
CFLAGS += $(CFLAGS_$(variant))

LINKFLAGS_profile  = -pg
LINKFLAGS_coverage = --coverage
LINKFLAGS += $(LINKFLAGS_$(variant)) $(FIRM_LIBS)

cparser_SOURCES = \
	actions.c \
	adt/pset_new.c \
	adt/strset.c \
	adt/strutil.c \
	ast/ast.c \
	ast/attribute.c \
	ast/constfold.c \
	ast/entity.c \
	ast/printer.c \
	ast/string_rep.c \
	ast/symbol_table.c \
	ast/type.c \
	ast/type_hash.c \
	ast/types.c \
	ast/walk.c \
	diagnostic.c \
	driver.c \
	firm/ast2firm.c \
	firm/entitymap.c \
	firm/firm_opt.c \
	firm/firm_timing.c \
	firm/jump_target.c \
	firm/mangle.c \
	help.c \
	machine_triple.c \
	main.c \
	options.c \
	parser/builtins.c \
	parser/format_check.c \
	parser/input.c \
	parser/parser.c \
	parser/preprocessor.c \
	parser/token.c \
	predefs.c \
	target.c \
	tempfile.c \
	warning.c \
	wrappergen/write_fluffy.c \
	wrappergen/write_jna.c \
	wrappergen/write_compoundsizes.c
cparser_OBJECTS = $(cparser_SOURCES:%.c=$(builddir)/%.o)
cparser_DEPS    = $(cparser_OBJECTS:%.o=%.d)

SPLINTS = $(addsuffix .splint, $(cparser_SOURCES))
CPARSERS = $(addsuffix .cparser, $(cparser_SOURCES))
CPARSEROS = $(cparser_SOURCES:%.c=$(builddir)/cpb/%.o)
CPARSEROS_E = $(cparser_SOURCES:%.c=$(builddir)/cpbe/%.o)
CPARSEROS2 = $(cparser_SOURCES:%.c=$(builddir)/cpb2/%.o)

# This hides the noisy commandline outputs. Show them with "make V=1"
ifneq ($(V),1)
Q ?= @
endif

GOAL = $(builddir)/cparser
all: $(GOAL)

# disable make builtin suffix rules
.SUFFIXES:

-include $(cparser_DEPS)

.PHONY: all bootstrap bootstrap2 bootstrape clean selfcheck splint libfirm_subdir

DIRS   := $(sort $(dir $(cparser_OBJECTS)))
UNUSED := $(shell mkdir -p $(DIRS) $(DIRS:$(builddir)/%=$(builddir)/cpb/%) $(DIRS:$(builddir)/%=$(builddir)/cpb2/%) $(DIRS:$(builddir)/%=$(builddir)/cpbe/%))

REVISIONH = $(builddir)/revision.h
REVISION ?= $(shell git --git-dir $(top_srcdir)/.git describe --abbrev=40 --always --dirty --match '')

# Update revision.h if necessary
UNUSED := $(shell \
	REV="\#define cparser_REVISION \"$(REVISION)\""; \
	echo "$$REV" | cmp -s - $(REVISIONH) 2> /dev/null || echo "$$REV" > $(REVISIONH) \
)
# determine if we can use "cparser-beta" as quickcheck
QUICKCHECK_DEFAULT := $(shell which cparser-beta || echo true) -fsyntax-only
QUICKCHECK ?= $(QUICKCHECK_DEFAULT)
QUICKCHECK_FLAGS ?= -Wno-shadow

$(GOAL): $(LIBFIRM_FILE) $(cparser_OBJECTS)
	@echo "===> LD $@"
	$(Q)$(CC) $(cparser_OBJECTS) $(LIBFIRM_FILE) -o $(GOAL) $(LINKFLAGS)

ifneq ("$(LIBFIRM_FILE)", "")
ifneq ("$(MAKECMDGOALS)", "clean")
$(LIBFIRM_FILE): libfirm_subdir
# Re-evaluate Makefile after libfirm_subdir has been executed
Makefile: libfirm_subdir
# Build libfirm in subdirectory
libfirm_subdir:
	$(Q)$(MAKE) -C $(FIRM_HOME) $(LIBFIRM_FILE_BASE)
endif
endif

splint: $(SPLINTS)

selfcheck: $(CPARSERS)

bootstrap: cparser.bootstrap

bootstrape: cparser.bootstrape

bootstrap2: cparser.bootstrap2

%.c.splint: %.c
	@echo '===> SPLINT $<'
	$(Q)splint $(CPPFLAGS) $<

%.c.cparser: %.c $(GOAL)
	@echo '===> CPARSER $<'
	$(Q)$(GOAL) $(CPPFLAGS) -fsyntax-only $<

$(builddir)/cpb/%.o: %.c $(builddir)/cparser
	@echo '===> CPARSER $<'
	$(Q)./$(builddir)/cparser $(CPPFLAGS) -std=c99 -Wall -g3 -c $< -o $@

$(builddir)/cpbe/%.o: %.c
	@echo '===> ECCP $@'
	$(Q)eccp $(CPPFLAGS) -std=c99 -Wall -c $< -o $@

$(builddir)/cpb2/%.o: %.c cparser.bootstrap
	@echo '===> CPARSER.BOOTSTRAP $<'
	$(Q)./cparser.bootstrap $(CPPFLAGS) -Wall -g -c $< -o $@

cparser.bootstrap: $(CPARSEROS)
	@echo "===> LD $@"
	$(Q)./$(builddir)/cparser $(CPARSEROS) $(LIBFIRM_FILE) $(LINKFLAGS) -o $@

cparser.bootstrape: $(CPARSEROS_E)
	@echo "===> LD $@"
	$(Q)gcc $(CPARSEROS_E) $(LINKFLAGS) -o $@

cparser.bootstrap2: cparser.bootstrap $(CPARSEROS2)
	@echo "===> LD $@"
	$(Q)./cparser.bootstrap $(CPARSEROS2) $(LIBFIRM_FILE) $(LINKFLAGS) -o $@

$(builddir)/%.o: %.c
	@echo '===> CC $@'
	$(Q)$(QUICKCHECK) $(CPPFLAGS) $(QUICKCHECK_FLAGS) $<
	$(Q)$(CC) $(CPPFLAGS) $(CFLAGS) -MP -MMD -c -o $@ $<

clean:
	@echo '===> CLEAN'
	$(Q)rm -rf $(builddir)
