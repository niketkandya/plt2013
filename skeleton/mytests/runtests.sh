#!/bin/bash


CPI_COMPILER="../microc"
RESULTS_DIR="../../webserver/tests"

# Get the timestamp of latest commit
COMMIT_TIMESTAMP=`git show -s --format="%ci"`

RESULTS_FILE="$RESULTS_DIR/$COMMIT_TIMESTAMP.html"

passed_tests=0
failed_tests=0



# Takes two arguments
#   $1, String to print
#   $2 (optional), color. Possible colors are "red" and "green".
#                       Otherwise, will default to bold
# Will print to both console AND results file in /webserver/tests
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

    # Output result to tests folder in webserver
    echo -e "$1" >> "$RESULTS_FILE"
}


# Clean
rm -rf "out"
mkdir -p "out"

# Remove previous results file for this same commit
rm -f "$RESULTS_FILE"


# Does test for each assembly file
for file in *.cpi; do
    error=""
    basename=${file%.*}
    
    colorprint "Testing: $basename"

    # Corner case for bash glob suckiness
    if [[ "$basename" == "*" ]]; then
        echo "No Assembly files found. Exiting"
        exit
    fi

    # Compile
    $CPI_COMPILER < "$file" > "$basename.s"

    # Assemble and link our asm files
    as -o out/$basename.o $basename.s
    if [[ $? != 0 ]]; then
        error="Assembly error (as)"
    fi

    gcc -o "out/$basename-cpi" out/$basename.o
    if [[ $? != 0 ]] && [[ "$error" == "" ]]; then
        error="Assembly error (gcc)"
    fi

    # Compile cpi with GCC
    gcc -x c $basename.cpi -o out/$basename-gcc
    if [[ $? != 0 ]] && [[ "$error" == "" ]]; then
        error="GCC compile cpi failed"
    fi

    
    # Compare $basename with $basename-gcc
    result_cpi=`out/$basename-cpi`
    return_cpi=`echo $?`

    result_gcc=`out/$basename-gcc`
    return_gcc=`echo $?`


    if [[ "$result_cpi" != "$result_gcc" ]] && [[ "$error" == "" ]]; then
        error="Different output. Got $result_cpi, Expected: $result_gcc"
        # colorprint "Different output!" "red";failed=1
    fi

    if [[ "$return_cpi" != "$return_gcc" ]] && [[ "$error" == "" ]]; then
        error="Different return (exit code)"
        # colorprint "Different return (exit code)" "red"; failed=1
    fi


    if [[ "$error" != "" ]]; then
        let failed_tests++
        colorprint "$error" "red"
        colorprint "Failed: $basename" "red"
        error=""
    else
        let passed_tests++
        colorprint "Passed: $basename" "green"
    fi
done


colorprint "Test results:"
colorprint "Total Passed:$passed_tests" "green"
colorprint "Total Failed:$failed_tests" "red"
