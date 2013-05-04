#!/bin/sh

set -eu
#set -x
WORKDIR="release"
VERSION_MAJOR="0"
VERSION_MINOR="9"
VERSION_PATCHLEVEL="14"
VERSION="${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCHLEVEL}"
RELEASEDIR="cparser-$VERSION"
FULLRELEASEDIR="$WORKDIR/$RELEASEDIR"
RELEASEFILE="cparser-$VERSION.tar.bz2"
SOURCEDIRS="adt driver win32 wrappergen ."
ADDFILES="README.md NEWS.md AUTHOR COPYING cparser.1"

# test if versions match
echo "Checking for version mismatch"
egrep -q "#define CPARSER_MAJOR\\s*\"$VERSION_MAJOR\"" main.c
egrep -q "#define CPARSER_MINOR\\s*\"$VERSION_MINOR\"" main.c
egrep -q "#define CPARSER_PATCHLEVEL\\s*\"$VERSION_PATCHLEVEL\"" main.c

rm -rf "$FULLRELEASEDIR"

echo "Preparing $FULLRELEASEDIR"
mkdir -p "$WORKDIR"
mkdir -p "$FULLRELEASEDIR"

for dir in $SOURCEDIRS; do
	mkdir -p "$FULLRELEASEDIR/$dir"
	cp -p "$dir/"*.[ch] "$FULLRELEASEDIR/$dir"
done
cp $ADDFILES "$FULLRELEASEDIR"
rm -f "$FULLRELEASEDIR/revision.h"
echo "REVISION = \"\"" > "$FULLRELEASEDIR/Makefile"
cat Makefile >> "$FULLRELEASEDIR/Makefile"

echo "creating $RELEASEFILE"
tar cjf "$RELEASEFILE" -C "$WORKDIR" "$RELEASEDIR"
