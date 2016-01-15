# include user-defined makefile settings
-include config.mak

top_srcdir   ?= .
top_builddir ?= build
VPATH        ?= $(top_srcdir)/src

variant  ?= debug# Different libfirm variants (debug, optimize, profile, coverage)
srcdir   ?= $(top_srcdir)
builddir ?= $(top_builddir)/$(variant)

include config.default.mak

AR ?= ar
DLLEXT ?= .so
LINK ?= $(CC)

CPPFLAGS += -I$(top_srcdir)/src -I$(builddir)
ifneq ("$(SYSTEM_INCLUDE_DIR)","")
	CPPFLAGS += -DSYSTEM_INCLUDE_DIR=\"$(SYSTEM_INCLUDE_DIR)\"
endif
ifneq ("$(LOCAL_INCLUDE_DIR)","")
	CPPFLAGS += -DLOCAL_INCLUDE_DIR=\"$(LOCAL_INCLUDE_DIR)\"
endif
ifneq ("$(COMPILER_INCLUDE_DIR)","")
	CPPFLAGS += -DCOMPILER_INCLUDE_DIR=\"$(COMPILER_INCLUDE_DIR)\"
endif
ifneq ("$(HOST_TRIPLE)","")
	CPPFLAGS += -DHOST_TRIPLE=\"$(HOST_TRIPLE)\"
endif
ifneq ("$(MULTILIB_M32_TRIPLE)","")
	CPPFLAGS += -DAPPEND_MULTILIB_DIRS -DMULTILIB_M32_TRIPLE=\"$(MULTILIB_M32_TRIPLE)\" -DMULTILIB_M64_TRIPLE=\"$(MULTILIB_M64_TRIPLE)\"
endif

CPPFLAGS += $(FIRM_CPPFLAGS)

CFLAGS += -Wall -W -Wstrict-prototypes -Wmissing-prototypes
# With -std=c99 we get __STRICT_ANSI__ which disables all posix declarations
# in cygwin, regardless of a set POSIX_C_SOURCE feature test macro.
ifneq ($(filter %cygwin %mingw32, $(shell $(CC) $(CFLAGS) -dumpmachine)),)
CFLAGS += -std=gnu99
else
CFLAGS += -std=c99 -pedantic
endif
CFLAGS_debug    = -O0 -g
CFLAGS_optimize = -O3 -fomit-frame-pointer -DNDEBUG -DNO_DEFAULT_VERIFY
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
libcparser_DLL     = $(builddir)/libcparser$(DLLEXT)

cparser_SOURCES = main.c $(libcparser_SOURCES)
cparser_OBJECTS = $(cparser_SOURCES:%.c=$(builddir)/%.o)
cparser_DEPS    = $(cparser_OBJECTS:%.o=%.d)
cparser_EXE     = $(builddir)/cparser

CPARSERS = $(addsuffix .check, $(cparser_SOURCES) $(libcparser_SOURCES))
CPARSEROS = $(cparser_SOURCES:%.c=$(builddir)/cpb/%.o)
CPARSEROS2 = $(cparser_SOURCES:%.c=$(builddir)/cpb2/%.o)

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

# Flags to be used when cparser checks its own sourcecode for warnings
SELFCHECK_FLAGS ?= -Wall -Wno-shadow -Werror

# Update revision.h if necessary
UNUSED := $(shell \
	REV="\#define cparser_REVISION \"$(REVISION)\""; \
	echo "$$REV" | cmp -s - $(REVISIONH) 2> /dev/null || echo "$$REV" > $(REVISIONH) \
)
# determine if we can use "cparser-beta" as quickcheck
QUICKCHECK_DEFAULT := $(shell which cparser-beta 2> /dev/null || echo true) -fsyntax-only
QUICKCHECK ?= $(QUICKCHECK_DEFAULT)

$(cparser_EXE): $(LIBFIRM_FILE) $(cparser_OBJECTS)
	@echo 'LD $@'
	$(Q)$(LINK) $(cparser_OBJECTS) $(LIBFIRM_FILE) -o $@ $(LINKFLAGS)

$(libcparser_A): $(libcparser_OBJECTS)
	@echo 'AR $@'
	$(Q)$(AR) -crs $@ $^

$(libcparser_DLL): $(libcparser_OBJECTS) $(LIBFIRM_FILE_DLL)
	@echo 'LINK $@'
	$(Q)$(LINK) -shared $^ $(LINKFLAGS) -o "$@"

ifneq ("$(LIBFIRM_FILE)", "")
ifneq ("$(MAKECMDGOALS)", "clean")
$(LIBFIRM_FILE): libfirm_subdir
# Re-evaluate Makefile after libfirm_subdir has been executed
Makefile: libfirm_subdir
# Build libfirm in subdirectory
libfirm_subdir:
	$(Q)$(MAKE) -C $(FIRM_HOME) $(LIBFIRM_FILE_BASE)

$(LIBFIRM_FILE_DLL): libfirm_subdir_dll
libfirm_subdir_dll:
	$(Q)$(MAKE) -C $(FIRM_HOME) $(LIBFIRM_FILE_DLL_BASE)
endif
endif

check: $(CPARSERS)
	@echo 'CHECK OPTIONS'
	$(Q)cd $(top_srcdir) ; support/check_options.py

bootstrap: cparser.bootstrap

bootstrap2: cparser.bootstrap2

%.c.check: %.c $(cparser_EXE)
	@echo 'CHECK $<'
	$(Q)$(cparser_EXE) $(CPPFLAGS) $(SELFCHECK_FLAGS) -fsyntax-only $<

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
	$(Q)$(QUICKCHECK) $(CPPFLAGS) $(SELFCHECK_FLAGS) $<
	$(Q)$(CC) $(CPPFLAGS) $(CFLAGS) -MP -MMD -c -o $@ $<

clean:
	@echo 'CLEAN'
	$(Q)rm -rf $(builddir)

.PHONY: install
PREFIX ?= /usr/local
INSTALL ?= install
BINDIR = $(DESTDIR)$(PREFIX)/bin
MANDIR = $(DESTDIR)$(PREFIX)/share/man
install: $(cparser_EXE)
	$(INSTALL) -d $(DESTDIR)$(COMPILER_INCLUDE_DIR)
	$(INSTALL) -m0644 include/*.h $(DESTDIR)$(COMPILER_INCLUDE_DIR)
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) -m0755 $< $(BINDIR)
	$(INSTALL) -d $(MANDIR)/man1
	$(INSTALL) -m0644 cparser.1 $(MANDIR)/man1
