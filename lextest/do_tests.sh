#!/bin/sh
cd `dirname $0`
for i in tokenstreams/*; do
	if [ "$i" != "tokenstreams/refresults" ]; then
		echo "==> Checking $i"
		../cparser --lextest $i > /tmp/tokenstream
		diff -u tokenstreams/refresults/`basename $i` /tmp/tokenstream
	fi
done
