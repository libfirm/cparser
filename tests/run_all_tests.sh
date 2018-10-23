#!/bin/sh

#success=0
#error=1
#
#   run_all_tests.sh <cparser_path> [cc_tests_compiler]
#
# Expects to be run in the same directory as the script itself.
# Expects to have CPARSER defined as an environement variable, or passed as the first argument
#
# Optionaly if the environement variable CC_PARSER or the second argument is not the empty string then some tests will compile executables using it
# this may also be cparser


if ! [ -z "$1" ]; then
    export "CPARSER=$1"
elif [ -z "$CPARSER" ]; then
    echo "error, CPARSER environement path was not defined and it was not passed as the first argument"
    exit 1;
fi
if ! [ -e "test_tests/test.sh" ]; then
    echo "error, run_all_tests.sh was run in a wrong directory"
    exit 1;
fi

if ! [ -z "$2" ]; then
    export "CC_TESTS=$2"
fi

if ! [ -z "$TESTS_CC" ]; then
    echo "tests cc: $TESTS_CC"
else
    echo "skipping cc tests"
fi


tests_result=0

for directory in test_*; do
    if ! [ -d "$directory" ]; then
        continue;
    elif ! [ -e "$directory/test.sh" ]; then
        echo "warning: "$directory/test.sh" was not found.";
        continue
    fi
    cd "$directory"
    if sh "test.sh"; then
        echo "$directory succeeded"
    else
        echo "      [Test failed]" 
        echo
        echo "$directory failed" 
        echo
        tests_result=1
    fi
    cd ..
done

if [ "$tests_result" -eq 0 ]; then
    echo 
    echo "all tests succeeded."
    echo 
    exit 0;
else
    echo 
    echo "some tests failed."
    echo
    exit 1;
fi
