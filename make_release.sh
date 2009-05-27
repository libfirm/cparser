#!/bin/sh

#set -x
WORKDIR="release"
VERSION="0.9.9"
RELEASEDIR="cparser-$VERSION"
FULLRELEASEDIR="$WORKDIR/$RELEASEDIR"
RELEASEFILE="cparser-$VERSION.tar.bz2"
SOURCEDIRS="adt builtins driver win32 wrappergen ."
ADDFILES="README TODO NEWS AUTHOR COPYING cparser.1"

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
echo "REVISION = \"$VERSION\"" > "$FULLRELEASEDIR/Makefile"
cat Makefile >> "$FULLRELEASEDIR/Makefile"

echo "creating $RELEASEFILE"
tar cjf "$RELEASEFILE" -C "$WORKDIR" "$RELEASEDIR"
