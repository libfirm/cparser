GOAL = $(BUILDDIR)/cparser

BUILDDIR ?= build
variant  ?= debug# Different libfirm variants (debug, optimize, profile)

FIRM_HOME   = libfirm
FIRM_CPPFLAGS = -I$(FIRM_HOME)/include
FIRM_LIBS   = -lm
LIBFIRM_FILE = build/$(variant)/libfirm.a
FIRM_VERSION = 1.19.1
FIRM_URL = http://downloads.sourceforge.net/project/libfirm/libfirm/$(FIRM_VERSION)/libfirm-$(FIRM_VERSION).tar.bz2?r=http%3A%2F%2Fsourceforge.net%2Fprojects%2Flibfirm%2Ffiles%2Flibfirm%2F&ts=1299786346&use_mirror=ignum

CPPFLAGS  = -I.
CPPFLAGS += $(FIRM_CPPFLAGS)

CFLAGS += -Wall -W -Wstrict-prototypes -Wmissing-prototypes -std=c99 -pedantic
CFLAGS_debug = -O2 -g
CFLAGS_optimize = -O3 -fomit-frame-pointer -DNDEBUG
CFLAGS_profile = -pg -O3 -fno-inline
CFLAGS += $(CFLAGS_$(variant))
ICC_CFLAGS = -O0 -g3 -std=c99 -Wall
#LFLAGS += -pg
ICC    ?= true
GCCO1  ?= true

LFLAGS += $(FIRM_LIBS)

SOURCES := \
	adt/hashset.c \
	adt/strset.c \
	adt/strutil.c \
	attribute.c \
	parser.c \
	ast.c \
	ast2firm.c \
	builtins.c \
	diagnostic.c \
	driver/firm_machine.c \
	driver/firm_opt.c \
	driver/firm_timing.c \
	entity.c \
	entitymap.c \
	format_check.c \
	input.c \
	lexer.c \
	main.c \
	mangle.c \
	preprocessor.c \
	printer.c \
	symbol_table.c \
	token.c \
	type.c \
	type_hash.c \
	types.c \
	help.c \
	warning.c \
	walk.c \
	wrappergen/write_fluffy.c \
	wrappergen/write_jna.c

OBJECTS = $(SOURCES:%.c=build/%.o)

SPLINTS = $(addsuffix .splint, $(SOURCES))
CPARSERS = $(addsuffix .cparser, $(SOURCES))
CPARSEROS = $(SOURCES:%.c=build/cpb/%.o)
CPARSEROS_E = $(SOURCES:%.c=build/cpbe/%.o)
CPARSEROS2 = $(SOURCES:%.c=build/cpb2/%.o)

Q = @

all: $(GOAL)

.PHONY: all clean bootstrap bootstrap2 $(FIRM_HOME)/$(LIBFIRM_FILE)

ifeq ($(findstring $(MAKECMDGOALS), clean depend),)
-include .depend
endif

config.h:
	cp config.h.in $@

%.h:
	@true

REVISION ?= $(shell git describe --always --dirty --match '')

revision.h:
	@echo "===> GEN $@"
	@echo "#define cparser_REVISION \"$(REVISION)\"" > .revision.h
	$(Q)if diff -Nq .revision.h revision.h > /dev/null; then \
	      rm .revision.h;                                    \
	    else                                                 \
	      echo "===> UPDATING revision.h";                   \
	      mv .revision.h revision.h;                         \
	    fi

.depend: config.h revision.h $(SOURCES)
	@echo "===> DEPEND"
	@rm -f $@ && touch $@ && makedepend -p "$@ build/" -Y -f $@ -- $(CPPFLAGS) -- $(SOURCES) 2> /dev/null && rm $@.bak

DIRS = build build/adt build/driver build/wrappergen build/cpb build/cpb/adt build/cpb/driver build/cpb/wrappergen build/cpb2 build/cpb2/adt build/cpb2/driver build/cpb2/wrappergen build/cpbe build/cpbe/adt build/cpbe/driver build/cpbe2/wrappergen
UNUSED := $(shell mkdir -p $(DIRS))

$(FIRM_HOME)/$(LIBFIRM_FILE):
ifeq "$(wildcard $(FIRM_HOME) )" ""
	@echo 'Download and extract libfirm tarball ...'
	$(Q)curl -s -L "${FIRM_URL}" -o "libfirm-$(FIRM_VERSION).tar.bz2"
	$(Q)tar xf "libfirm-$(FIRM_VERSION).tar.bz2"
	$(Q)mv "libfirm-$(FIRM_VERSION)" libfirm
endif
	cd libfirm && $(MAKE) $(LIBFIRM_FILE)

$(GOAL): $(FIRM_HOME)/$(LIBFIRM_FILE) $(OBJECTS)
	@echo "===> LD $@"
	$(Q)$(CC) $(OBJECTS) $(LFLAGS) $(FIRM_HOME)/$(LIBFIRM_FILE) -o $(GOAL)

splint: $(SPLINTS)

selfcheck: $(CPARSERS)

bootstrap: build/cpb build/cpb/adt build/cpb/driver $(CPARSEROS) cparser.bootstrap

bootstrape: build/cpb build/cpb/adt build/cpb/driver $(CPARSEROS_E) cparser.bootstrape

bootstrap2: build/cpb2 build/cpb2/adt build/cpb2/driver $(CPARSEROS2) cparser.bootstrap2

%.c.splint: %.c
	@echo '===> SPLINT $<'
	$(Q)splint $(CPPFLAGS) $<

%.c.cparser: %.c
	@echo '===> CPARSER $<'
	$(Q)./cparser $(CPPFLAGS) -fsyntax-only $<

build/cpb/%.o: %.c build/cparser
	@echo '===> CPARSER $<'
	$(Q)./build/cparser $(CPPFLAGS) -std=c99 -Wall -g3 -c $< -o $@

build/cpbe/%.o: %.c
	@echo '===> ECCP $<'
	$(Q)eccp $(CPPFLAGS) -std=c99 -Wall -c $< -o $@

build/cpb2/%.o: %.c cparser.bootstrap
	@echo '===> CPARSER.BOOTSTRAP $<'
	$(Q)./cparser.bootstrap $(CPPFLAGS) -Wall -g -c $< -o $@

cparser.bootstrap: $(CPARSEROS)
	@echo "===> LD $@"
	$(Q)./build/cparser $(CPARSEROS) $(LFLAGS) -o $@

cparser.bootstrape: $(CPARSEROS_E)
	@echo "===> LD $@"
	$(Q)gcc $(CPARSEROS_E) $(LFLAGS) -o $@

cparser.bootstrap2: $(CPARSEROS2)
	@echo "===> LD $@"
	$(Q)./cparser.bootstrap $(CPARSEROS2) $(LFLAGS) -o $@

build/%.o: %.c
	@echo '===> CC $<'
#$(Q)$(ICC) $(CPPFLAGS) $(ICC_CFLAGS) -c $< -o $@
#$(Q)$(GCCO1) $(CPPFLAGS) $(CFLAGS) -O1 -c $< -o $@
	$(Q)$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean:
	@echo '===> CLEAN'
	$(Q)rm -rf build/* $(GOAL) .depend
