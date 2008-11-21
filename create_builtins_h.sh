#!/bin/sh

cat <<__EOF__
/* WARNING: automatically generated file. Generated from builtins/builtins.c */

static const char builtins[] =
__EOF__

cat builtins/builtins.c | sed -e "s/^/\\\"/g" -e "s/$/\\\n\\\"/g"

cat << __EOF__
;
__EOF__
