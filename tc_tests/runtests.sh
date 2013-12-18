#!/bin/bash


CPI_COMPILER="../cpi --stdout --stdin"

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
}


# Does test for each assembly file
for file in *.fail; do
    error=""
    basename=${file%.*}
    
    colorprint "Testing: $basename"

    # Corner case for bash glob suckiness
    if [[ "$basename" == "*" ]]; then
        echo "No fail test files found. Exiting"
        exit
    fi

    # Compile
    $CPI_COMPILER < "$file" > "$basename.s"
    if [[ $? != 0 ]]; then
        error="CPI COMPILE ERROR"
    fi

    if [[ "$error" != "" ]]; then
        let passed_tests++
        colorprint "    Passed: $basename" "green"
    else
        let failed_tests++
        colorprint "    Failed: $basename. $error" "red"
        colorprint "    Test passed when error is present" "red"
    fi
done

# Does test for each assembly file
for file in *.pass; do
    error=""
    basename=${file%.*}
    
    colorprint "Testing: $basename"

    # Corner case for bash glob suckiness
    if [[ "$basename" == "*" ]]; then
        echo "No pass test files found."
        break
    fi

    # Compile
    $CPI_COMPILER < "$file" > "$basename.s"
    if [[ $? != 0 ]]; then
        error="CPI COMPILE ERROR"
    fi

    if [[ "$error" != "" ]]; then
        let failed_tests++
        colorprint "    Failed: $basename. $error" "red"
        error=""
    else
        let passed_tests++
        colorprint "    Passed: $basename" "green"
    fi
done

colorprint "\n\nTest results:"
colorprint "Total Passed:$passed_tests" "green"
colorprint "Total Failed:$failed_tests" "red"
