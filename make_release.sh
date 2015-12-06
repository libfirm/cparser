#!/bin/sh

set -eu
#set -x
WORKDIR="release"
VERSION_MAJOR="1"
VERSION_MINOR="22"
VERSION_PATCHLEVEL="0"
VERSION="${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCHLEVEL}"
RELEASEDIR="cparser-$VERSION"
FULLRELEASEDIR="$WORKDIR/$RELEASEDIR"
RELEASEFILE="cparser-$VERSION.tar.bz2"
SOURCEDIRS="src src/adt src/ast src/driver src/firm src/parser src/wrappergen"
ADDFILES="AUTHOR config.default.mak COPYING cparser.1 NEWS.md README.md CMakeLists.txt"

# test if versions match
echo "Checking for version mismatch"
egrep -q "#define CPARSER_MAJOR\\s*\"$VERSION_MAJOR\"" src/driver/version.h
egrep -q "#define CPARSER_MINOR\\s*\"$VERSION_MINOR\"" src/driver/version.h
egrep -q "#define CPARSER_PATCHLEVEL\\s*\"$VERSION_PATCHLEVEL\"" src/driver/version.h

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
echo "#define cparser_REVISION \"\"" > "$FULLRELEASEDIR/src/revision.h"

echo "creating $RELEASEFILE"
tar cjf "$RELEASEFILE" -C "$WORKDIR" "$RELEASEDIR"
