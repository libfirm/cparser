GOAL = cparser

#FIRM_CFLAGS = `pkg-config --cflags libfirm`
#FIRM_LIBS = `pkg-config --libs libfirm`
FIRM_CFLAGS = -I$(HOME)/projects/firm/libfirm/include -I$(HOME)/projects/firm/libcore
FIRM_LIBS = -L$(HOME)/projects/firm/build/i686-pc-linux-gnu/debug -lfirm -llpp -lcore -lm

CFLAGS += -Wall -W -Werror -O0 -g3 -std=c99
CFLAGS += -DHAVE_CONFIG_H
CFLAGS += -I .
CFLAGS += $(FIRM_CFLAGS)

LFLAGS = $(FIRM_LIBS) -llpp -ldl --export-dynamic

SOURCES := \
	adt/hashset.c \
	adt/pset.c \
	adt/strset.c \
	adt/xmalloc.c \
	ast.c \
	lexer.c \
	main.c \
	parser.c \
	symbol_table.c \
	token.c \
	type.c

OBJECTS = $(SOURCES:%.c=build/%.o)

Q = @

.PHONY : all clean dirs

all: $(GOAL)

ifeq ($(findstring $(MAKECMDGOALS), clean depend),)
-include .depend
endif

.depend: $(SOURCES)
	@echo "===> DEPEND"
	@rm -f $@ && touch $@ && makedepend -p "$@ build/" -Y -f $@ -- $(CFLAGS) -- $(SOURCES) 2> /dev/null && rm $@.bak

$(GOAL): build/adt $(OBJECTS)
	@echo "===> LD $@"
	$(Q)$(CC) -rdynamic $(OBJECTS) $(LFLAGS) -o $(GOAL)

build/adt:
	@echo "===> MKDIR $@"
	$(Q)mkdir -p $@

build/%.o: %.c
	@echo '===> CC $<'
	$(Q)$(CC) $(CFLAGS) -c $< -o $@

clean:
	@echo '===> CLEAN'
	$(Q)rm -rf build $(GOAL) .depend
