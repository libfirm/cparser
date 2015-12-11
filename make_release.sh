#!/bin/sh
set -eu

# Check that our git checkout is clean (remember that we use git archive
# which will miss things uncommitted changes)
if [ "$(git status --porcelain)" != "" ]; then
	echo "Git checkout not clean!"
	exit 1
fi

WORKDIR="release"
VERSION_MAJOR="1"
VERSION_MINOR="22"
VERSION_MICRO="0"
VERSION="${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_MICRO}"
RELEASEFILE="build/cparser-$VERSION.tar.bz2"
VERSIONFILE=src/driver/version.h

# test if versions match
echo "Checking version in $VERSIONFILE"
egrep -q "#define CPARSER_MAJOR\\s*\"$VERSION_MAJOR\"" "$VERSIONFILE"
egrep -q "#define CPARSER_MINOR\\s*\"$VERSION_MINOR\"" "$VERSIONFILE"
egrep -q "#define CPARSER_PATCHLEVEL\\s*\"$VERSION_MICRO\"" "$VERSIONFILE"
echo "Checking version in CMakeLists.txt"
grep -q "set(cparser_VERSION \"${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_MICRO}\")" CMakeLists.txt
echo "Checking version in NEWS.md"
egrep -q "$VERSION_MAJOR.$VERSION_MINOR.$VERSION_MICRO" NEWS.md

echo "creating $RELEASEFILE"
mkdir -p "$(dirname "$RELEASEFILE")"
git archive --prefix cparser-$VERSION/ HEAD | bzip2 > "$RELEASEFILE"
