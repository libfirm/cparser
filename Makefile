GOAL = cparser

FIRM_HOME = $(HOME)/projects/firm
FIRM_BUILD = $(FIRM_HOME)/build/i686-pc-linux-gnu/debug/
FIRM_CFLAGS = -I$(FIRM_HOME)/libfirm/include -I$(FIRM_HOME)/obstack -I$(FIRM_HOME)/libcore -I$(FIRM_HOME)/libcore/libcore -I$(FIRM_HOME)
FIRM_LIBS = -L$(FIRM_BUILD) -lfirm -llpp -lcore -lm -lz -ldl

CPPFLAGS  = -DHAVE_CONFIG_H -DFIRM_BACKEND
CPPFLAGS += -I.
CPPFLAGS += $(FIRM_CFLAGS)

CFLAGS += -Wall -W -Wstrict-prototypes -Wmissing-prototypes -Werror -std=c99 -pedantic
CFLAGS += -O0 -g3
ICC_CFLAGS = -O0 -g3 -std=c99 -Wall -Werror
#CFLAGS += -O3 -march=pentium4 -fomit-frame-pointer -DNDEBUG

LFLAGS = $(FIRM_LIBS)

SOURCES := \
	adt/hashset.c \
	adt/strset.c \
	adt/xmalloc.c \
	ast.c \
	ast2firm.c \
	lexer.c \
	main.c \
	parser.c \
	symbol_table.c \
	token.c \
	type.c \
	type_hash.c \
	write_fluffy.c

OBJECTS = $(SOURCES:%.c=build/%.o)

SPLINTS = $(addsuffix .splint, $(SOURCES))

Q = @

.PHONY : all clean dirs

all: $(GOAL)

ifeq ($(findstring $(MAKECMDGOALS), clean depend),)
-include .depend
endif

.depend: $(SOURCES)
	@echo "===> DEPEND"
	@rm -f $@ && touch $@ && makedepend -p "$@ build/" -Y -f $@ -- $(CPPFLAGS) -- $(SOURCES) 2> /dev/null && rm $@.bak

$(GOAL): build/adt $(OBJECTS)
	@echo "===> LD $@"
	$(Q)$(CC) $(OBJECTS) $(LFLAGS) -o $(GOAL)

splint: $(SPLINTS)

%.c.splint: %.c
	@echo '===> SPLINT $<'
	$(Q)splint $(CPPFLAGS) $<

build/adt:
	@echo "===> MKDIR $@"
	$(Q)mkdir -p $@

build/%.o: %.c
	@echo '===> CC $<'
	$(Q)icc $(CPPFLAGS) $(ICC_CFLAGS) -c $< -o $@
	$(Q)$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean:
	@echo '===> CLEAN'
	$(Q)rm -rf build $(GOAL) .depend
