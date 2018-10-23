#!/bin/sh
"$CPARSER" -o preprocessed.tmp.ii -E testcase1.c 2>case1.stderr.tmp
[ `grep -c "_Pragma is not currently supported" case1.stderr.tmp` -eq 8 ] || exit 1 #expected 8 warnings for the testcase
if ! [ -z "$TESTS_CC" ]; then
    "$TESTS_CC" -o testcase1.tmp preprocessed.tmp.ii || exit 1
fi

rm -f *.tmp* 2>/dev/null
