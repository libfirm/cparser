#!/bin/sh
sh success.sh || exit 1
sh fail.sh && exit 1
exit 0
