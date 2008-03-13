#!/bin/bash

rm -f messages.cparser messages.gcc
for i in *.c; do
	echo -n "Compile $i..."
	cparser $i -O3 -o prog.cparser >> messages.cparser 2>&1 || echo -n " CPARSER COMPILE FAILED"
	gcc -m32 -std=gnu99 $i -O3 -o prog.gcc >> messages.gcc 2>&1 || echo -n " GCC COMPILE FAILED"
	./prog.cparser > out.cparser || echo -n " FAILED CPARSER RUN"
	./prog.gcc > out.gcc || echo -n " FAILED GCC RUN"
	diff -u out.cparser out.gcc > /dev/null || echo -n " RESULTS MISCOMPARE"
	echo ""
done

for i in shouldfail/*.c; do
	echo -n "Compile $i..."
	../cparser $i -O3 -o prog.cparser >> messages.cparser 2>&1 && echo -n " CPARSER COMPILED"
	gcc -m32 -std=gnu99 $i -O3 -o prog.gcc >> messages.gcc 2>&1 && echo -n " GCC COMPILED"
	echo ""
done
