#!/bin/bash

for i in *.c; do
	echo -n "$i... "
	pptest $i > /tmp/$i
	if ! diff -u refresults/$i /tmp/$i > /dev/null; then
		echo "FAILED"
	else
		echo "OK"
	fi
done
