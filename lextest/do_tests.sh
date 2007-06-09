#!/bin/sh
cd `dirname $0`
for i in tokenstreams/*; do
	if [ "$i" != "tokenstreams/refresults" ]; then
		echo "==> Checking $i"
		../cparser $i > /tmp/tokenstream
		diff -u /tmp/tokenstream tokenstreams/refresults/`basename $i` || exit 1
	fi
done
