#!/bin/sh

#set -x
WORKDIR="release"
VERSION="0.9.14"
RELEASEDIR="cparser-$VERSION"
FULLRELEASEDIR="$WORKDIR/$RELEASEDIR"
RELEASEFILE="cparser-$VERSION.tar.bz2"
SOURCEDIRS="adt builtins driver win32 wrappergen ."
ADDFILES="README.md NEWS.md AUTHOR COPYING cparser.1"

rm -rf "$FULLRELEASEDIR"

echo "Preparing $FULLRELEASEDIR"
mkdir -p "$WORKDIR"
mkdir -p "$FULLRELEASEDIR"

for dir in $SOURCEDIRS; do
	mkdir -p "$FULLRELEASEDIR/$dir"
	cp -p "$dir/"*.sh "$FULLRELEASEDIR/$dir"
	cp -p "$dir/"*.def "$FULLRELEASEDIR/$dir"
	cp -p "$dir/"*.inc "$FULLRELEASEDIR/$dir"
	cp -p "$dir/"*.[ch] "$FULLRELEASEDIR/$dir"
	cp -p "$dir/"*.[ch].* "$FULLRELEASEDIR/$dir"
done
cp $ADDFILES "$FULLRELEASEDIR"
rm -f "$FULLRELEASEDIR/revision.h"
rm -f "$FULLRELEASEDIR/config.h"
echo "REVISION = \"$VERSION\"" > "$FULLRELEASEDIR/Makefile"
cat Makefile >> "$FULLRELEASEDIR/Makefile"

echo "creating $RELEASEFILE"
tar cjf "$RELEASEFILE" -C "$WORKDIR" "$RELEASEDIR"
