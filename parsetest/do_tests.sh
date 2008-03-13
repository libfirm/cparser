#!/bin/bash

rm -f messages.cparser messages.gcc
for i in *.c; do
	echo -n "Compile $i..."
	../cparser $i -O3 -o prog.cparser >> messages.cparser 2>&1
	gcc $i -O3 -o prog.gcc >> messages.gcc 2>&1
	./prog.cparser > out.cparser
	./prog.gcc > out.gcc
	diff -u out.cparser out.gcc > /dev/null || echo -n " FAILED"
	echo ""
done
