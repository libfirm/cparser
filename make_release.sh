#!/bin/bash

#set -x
WORKDIR="release"
VERSION="0.9.1"
RELEASEDIR="cparser-$VERSION"
FULLRELEASEDIR="$WORKDIR/$RELEASEDIR"
RELEASEFILE="cparser-$VERSION.tar.bz2"
SOURCEDIRS="adt driver ."
ADDFILES="README TODO NEWS AUTHOR COPYING Makefile"

echo "Preparing $FULLRELEASEDIR"
mkdir -p "$WORKDIR"
mkdir -p "$FULLRELEASEDIR"

for dir in $SOURCEDIRS; do
	mkdir -p "$FULLRELEASEDIR/$dir"
	cp -p "$dir/"*.def "$FULLRELEASEDIR/$dir"
	cp -p "$dir/"*.inc "$FULLRELEASEDIR/$dir"
	cp -p "$dir/"*.[ch] "$FULLRELEASEDIR/$dir"
done
cp $ADDFILES "$FULLRELEASEDIR"

echo "creating $RELEASEFILE"
pushd "$WORKDIR"
tar -cjf "$RELEASEFILE" "$RELEASEDIR"
popd
