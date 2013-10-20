#!/bin/bash


TESTDIR=tests


passed_tests=0
failed_tests=0


# Takes two arguments
#   $1, String to print
#   $2 (optional), color. Possible colors are "red" and "green".
#                       Otherwise, will default to bold
colorprint () {
    bold="\033[1;1m"
    red="\033[1;31m"
    green="\033[1;32m"
    default="\033[1;0m"

    prefix=""
    if [[ "$2" == "red" ]]; then
        prefix=$red
    elif [[ "$2" == "green" ]]; then
        prefix=$green
    else
        prefix=$bold
    fi

    echo -e "$prefix$1$default"
}


# Clean
rm -rf "out"
mkdir -p "out"


# Does test for each assembly file
for file in *.s; do
    failed=0
    basename=${file%.*}
    
    colorprint "Testing: $basename"

    # Corner case for bash glob suckiness
    if [ "$basename" == "*" ]; then
        echo "No Assembly files found. Exiting"
        exit
    fi


    # Assemble and link our asm files
    as -o out/$basename.o $basename.s
    if [[ $? != 0 ]]; then
        colorprint "Assembly error (as)" "red"; failed=1
    fi

    gcc -o "out/$basename-cpi" out/$basename.o
    if [[ $? != 0 ]]; then
        colorprint "Assembly error (gcc)" "red"; failed=1
    fi

    # Compile cpi with GCC
    gcc -x c $basename.cpi -o out/$basename-gcc
    if [[ $? != 0 ]]; then
        colorprint "GCC compile cpi failed" "red";failed=1
    fi

    


    # Compare $basename with $basename-gcc
    result=`out/basename`
    result_gcc=`out/basename-gcc`


    if [ "$result" != "$result_gcc" ]; then
        colorprint "Different output!" "red"
        failed=1
    fi

    if [[ failed ]]; then
        let failed_tests++
        colorprint "Failed: $basename" "red"
    else
        let passed_tests++
        echo "Passed: $basename"
    fi
done


colorprint "Test results:"
colorprint "Passed:$passed_tests" "green"
colorprint "Failed: $failed_tests" "red"
