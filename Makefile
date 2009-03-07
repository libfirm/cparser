-include config.mak

GOAL = $(BUILDDIR)/cparser

BUILDDIR ?= build

FIRM_CFLAGS ?= `pkg-config --cflags libfirm`
FIRM_LIBS   ?= `pkg-config --libs libfirm`

CPPFLAGS  = -DHAVE_CONFIG_H -DFIRM_BACKEND
CPPFLAGS += -I.
CPPFLAGS += $(FIRM_CFLAGS)

CFLAGS += -Wall -W -Wstrict-prototypes -Wmissing-prototypes -Werror -std=c99 -pedantic
CFLAGS += -O0 -g3
#CFLAGS += -O3 -march=pentium4 -fomit-frame-pointer -DNDEBUG
#CFLAGS += -pg -O3 -fno-inline
ICC_CFLAGS = -O0 -g3 -std=c99 -Wall -Werror
#LFLAGS += -pg
ICC    ?= true
GCCO1  ?= true

LFLAGS += $(FIRM_LIBS)

SOURCES := \
	adt/hashset.c \
	adt/obstack.c \
	adt/obstack_printf.c \
	adt/strset.c \
	adt/xmalloc.c \
	attribute.c \
	parser.c \
	ast.c \
	ast2firm.c \
	diagnostic.c \
	driver/firm_cmdline.c \
	driver/firm_codegen.c \
	driver/firm_opt.c \
	driver/firm_timing.c \
	driver/gen_firm_asm.c \
	entity.c \
	entitymap.c \
	format_check.c \
	lexer.c \
	main.c \
	mangle.c \
	preprocessor.c \
	symbol_table.c \
	token.c \
	type.c \
	type_hash.c \
	types.c \
	walk_statements.c \
	warning.c \
	wrappergen/write_caml.c \
	wrappergen/write_fluffy.c \
	wrappergen/write_jna.c

OBJECTS = $(SOURCES:%.c=build/%.o)

SPLINTS = $(addsuffix .splint, $(SOURCES))
CPARSERS = $(addsuffix .cparser, $(SOURCES))
CPARSEROS = $(SOURCES:%.c=build/cpb/%.o)
CPARSEROS_E = $(SOURCES:%.c=build/cpbe/%.o)
CPARSEROS2 = $(SOURCES:%.c=build/cpb2/%.o)

Q = @

all: $(DIRS) $(GOAL)

.PHONY: all clean dirs bootstrap bootstrap2

ifeq ($(findstring $(MAKECMDGOALS), clean depend),)
-include .depend
endif

config.h:
	cp config.h.in $@

%.h:
	@true

REVISION ?= $(shell svnversion -n .)

.depend: config.h $(SOURCES)
	@echo "#define cparser_REVISION \"$(REVISION)\"" > .revision.h
	$(Q)if diff -Nq .revision.h revision.h > /dev/null; then \
	      rm .revision.h;                                    \
	    else                                                 \
	      echo "===> UPDATING revision.h";                   \
	      mv .revision.h revision.h;                         \
	    fi
	@echo "===> DEPEND"
	@rm -f $@ && touch $@ && makedepend -p "$@ build/" -Y -f $@ -- $(CPPFLAGS) -- $(SOURCES) 2> /dev/null && rm $@.bak

DIRS = build build/adt build/driver build/wrappergen build/cpb build/cpb/adt build/cpb/driver build/cpb/wrappergen build/cpb2 build/cpb2/adt build/cpb2/driver build/cpb2/wrappergen build/cpbe build/cpbe/adt build/cpbe/driver build/cpbe2/wrappergen
UNUSED := $(shell mkdir -p $(DIRS))

$(GOAL): $(OBJECTS) $(LIBFIRM_FILE)
	@echo "===> LD $@"
	$(Q)$(CC) $(OBJECTS) $(LFLAGS) -o $(GOAL)

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

$(DIRS):
	@echo "===> MKDIR $@"
	$(Q)mkdir -p $@

builtins.h: builtins/builtins.c create_builtins_h.sh
	@echo '===> CREATE_BUILTINS $<'
	$(Q)./create_builtins_h.sh > $@

main.c: builtins.h

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
	$(Q)rm -rf builtins.h build/* $(GOAL) .depend
