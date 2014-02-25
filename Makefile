# include user-defined makefile settings
-include config.mak

top_srcdir   ?= .
top_builddir ?= build
VPATH        ?= $(top_srcdir)/src

include config.default.mak

variant  ?= debug# Different libfirm variants (debug, optimize, profile, coverage)
srcdir   ?= $(top_srcdir)
builddir ?= $(top_builddir)/$(variant)

AR ?= ar

CPPFLAGS  = -I$(top_srcdir)/src -I$(builddir) $(SYSTEM_INCLUDE_DIR) $(LOCAL_INCLUDE_DIR) $(COMPILER_INCLUDE_DIR) $(MULTILIB_INCLUDE_DIR) $(MULTILIB_M32_TRIPLE) $(MULTILIB_M64_TRIPLE) $(HOST_TRIPLE)
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

libcparser_SOURCES := $(wildcard $(top_srcdir)/src/*/*.c)
libcparser_OBJECTS = $(libcparser_SOURCES:%.c=$(builddir)/%.o)
libcparser_DEPS    = $(libcparser_OBJECTS:%.o=%.d)
libcparser_A       = $(builddir)/libcparser.a

cparser_SOURCES = main.c $(libcparser_SOURCES)
cparser_OBJECTS = $(cparser_SOURCES:%.c=$(builddir)/%.o)
cparser_DEPS    = $(cparser_OBJECTS:%.o=%.d)
cparser_EXE     = $(builddir)/cparser

CPARSERS = $(addsuffix .check, $(cparser_SOURCES) $(libcparser_SOURCES))
CPARSEROS = $(libcparser_SOURCES:%.c=$(builddir)/cpb/%.o) $(cparser_SOURCES:%.c=$(builddir)/cpb/%.o)
CPARSEROS2 = $(libcparser_SOURCES:%.c=$(builddir)/cpb2/%.o) $(cparser_SOURCES:%.c=$(builddir)/cpb2/%.o)

# This hides the noisy commandline outputs. Show them with "make V=1"
ifneq ($(V),1)
Q ?= @
endif

all: $(cparser_EXE)

# disable make builtin suffix rules
.SUFFIXES:

-include $(cparser_DEPS)

.PHONY: all bootstrap bootstrap2 clean check libfirm_subdir

DIRS   := $(sort $(dir $(cparser_OBJECTS) $(libcparser_OBJECTS)))
UNUSED := $(shell mkdir -p $(DIRS) $(DIRS:$(builddir)/%=$(builddir)/cpb/%) $(DIRS:$(builddir)/%=$(builddir)/cpb2/%))

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

$(cparser_EXE): $(LIBFIRM_FILE) $(cparser_OBJECTS)
	@echo 'LD $@'
	$(Q)$(CC) $(cparser_OBJECTS) $(LIBFIRM_FILE) -o $@ $(LINKFLAGS)

$(libcparser_A): $(libcparser_OBJECTS)
	@echo 'AR $@'
	$(Q)$(AR) -crsu $@ $^

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

check: $(CPARSERS)

bootstrap: cparser.bootstrap

bootstrap2: cparser.bootstrap2

%.c.check: %.c $(cparser_EXE)
	@echo 'CHECK $<'
	$(Q)$(cparser_EXE) $(CPPFLAGS) -Wall -Wno-shadow -fsyntax-only $<

$(builddir)/cpb/%.o: %.c $(cparser_EXE)
	@echo 'CPARSER $<'
	$(Q)$(cparser_EXE) -m32 $(CPPFLAGS) -std=c99 -Wall -g3 -c $< -o $@

$(builddir)/cpb2/%.o: %.c cparser.bootstrap
	@echo 'CPARSER.BOOTSTRAP $<'
	$(Q)./cparser.bootstrap -m32 $(CPPFLAGS) -Wall -g -c $< -o $@

cparser.bootstrap: $(CPARSEROS)
	@echo 'LD $@'
	$(Q)$(cparser_EXE) -m32 $(CPARSEROS) $(LIBFIRM_FILE) $(LINKFLAGS) -o $@

cparser.bootstrap2: cparser.bootstrap $(CPARSEROS2)
	@echo 'LD $@'
	$(Q)./cparser.bootstrap -m32 $(CPARSEROS2) $(LIBFIRM_FILE) $(LINKFLAGS) -o $@

$(builddir)/%.o: %.c
	@echo 'CC $@'
	$(Q)$(QUICKCHECK) $(CPPFLAGS) $(QUICKCHECK_FLAGS) $<
	$(Q)$(CC) $(CPPFLAGS) $(CFLAGS) -MP -MMD -c -o $@ $<

clean:
	@echo 'CLEAN'
	$(Q)rm -rf $(builddir)
