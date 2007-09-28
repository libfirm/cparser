GOAL = cparser

CFLAGS += -Wall -W -Werror -std=c99 -pedantic
CFLAGS += -DHAVE_CONFIG_H
CFLAGS += -I .
CFLAGS += -O0 -g3
#CFLAGS += -O3 -march=pentium4 -fomit-frame-pointer -DNDEBUG

LFLAGS =

SOURCES := \
	adt/array.c \
	adt/hashset.c \
	adt/strset.c \
	adt/xmalloc.c \
	ast.c \
	lexer.c \
	main.c \
	parser.c \
	symbol_table.c \
	token.c \
	type.c \
	type_hash.c \
	write_fluffy.c

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
