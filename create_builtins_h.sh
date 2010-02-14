#!/bin/sh

INPUT="$1"

cat <<__EOF__
/* WARNING: automatically generated file. Generated from builtins/builtins.c */

static const char builtins[] =
__EOF__

cat "$INPUT" | sed -e "s/^/\\\"/g" -e "s/$/\\\n\\\"/g"

cat << __EOF__
;
__EOF__
