#!/bin/bash

CPARSER=../build/cparser
CPARSER_FLAGS="-O3 -fno-inline"

rm -f messages.cparser messages.gcc
for i in *.c shouldpass/*.c; do
	echo -n "Compile $i..."
	"$CPARSER" $i $CPARSER_FLAGS -std=c99 -o prog.cparser >> messages.cparser 2>&1 || echo -n " CPARSER COMPILE FAILED"
	gcc -m32 -std=c99 $i -O3 -o prog.gcc >> messages.gcc 2>&1 || echo -n " GCC COMPILE FAILED"
	./prog.cparser > out.cparser || echo -n " FAILED CPARSER RUN"
	./prog.gcc > out.gcc || echo -n " FAILED GCC RUN"
	diff -u out.cparser out.gcc > /dev/null || echo -n " RESULTS MISCOMPARE"
	echo ""
done

for i in gnu99/*.c; do
	echo -n "Compile $i..."
	"$CPARSER" $i $CPARSER_FLAGS -std=gnu99 -o prog.cparser >> messages.cparser 2>&1 || echo -n " CPARSER COMPILE FAILED"
	gcc -m32 -std=gnu99 $i -O3 -o prog.gcc >> messages.gcc 2>&1 || echo -n " GCC COMPILE FAILED"
	./prog.cparser > out.cparser || echo -n " FAILED CPARSER RUN"
	./prog.gcc > out.gcc || echo -n " FAILED GCC RUN"
	diff -u out.cparser out.gcc > /dev/null || echo -n " RESULTS MISCOMPARE"
	echo ""
done

for i in MS/*.c; do
	echo -n "Compile $i..."
	"$CPARSER" $CPARSER_FLAGS --ms $i -o prog.cparser >> messages.cparser 2>&1 || echo -n " CPARSER COMPILE FAILED"
	./prog.cparser > out.cparser || echo -n " FAILED CPARSER RUN"
	echo ""
done

for i in should_warn/*.c; do
	echo -n "Compile $i..."
	"$CPARSER" $CPARSER_FLAGS $i -Wall -o prog.cparser >> messages.cparser 2>&1 || echo -n " CPARSER DID NOT COMPILE"
	gcc -Wall -m32 -std=gnu99 $i -O3 -o prog.gcc >> messages.gcc 2>&1 || echo -n " GCC DID NOT COMPILER"
	echo ""
	if diff -u /dev/null messages.cparser > /dev/null; then
		echo -n " CPARSER HAD NO WARNINGS"
	fi
	if diff -u /dev/null messages.gcc > /dev/null; then
		echo -n " GCC HAD NO WARNINGS"
	fi
done

for i in shouldfail/*.c; do
	echo -n "Compile $i..."
	"$CPARSER" $CPARSER_FLAGS $i -std=c99 -o prog.cparser >> messages.cparser 2>&1 && echo -n " CPARSER COMPILED"
	gcc -m32 -std=c99 $i -O3 -o prog.gcc >> messages.gcc 2>&1 && echo -n " GCC COMPILED"
	echo ""
done
