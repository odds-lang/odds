#!/bin/bash

NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

INPUT_FILES="compiler/pass/*.ods"
TMP_FILE=$(mktemp "compiled.XXXXX")
printf "${CYAN}Running compiler 'pass' tests...\n${NC}"

for input_file in $INPUT_FILES; do
    python_file=${input_file/.ods/.py}
    output_file=${input_file/.ods/.out}

    # compile odds program to temp python file
    ../odds.sh -r $input_file $TMP_FILE

    # if python test file exists, compare them
    if [ -e "$python_file" ]; then
        cmp -s $python_file $TMP_FILE
        if [ "$?" -ne 0 ]; then
            printf "%-65s ${RED}ERROR\n${NC}" "  - checking $python_file..." 1>&2
            rm -f $TMP_FILE
            exit 1
        fi
    fi

    # if test output file exists, compare compiled output to it
    if [ -e "$output_file" ]; then
        python $TMP_FILE | cmp -s $output_file -
        if [ "$?" -ne 0 ]; then
            printf "%-65s ${RED}ERROR\n${NC}" "  - checking $output_file..." 1>&2
            rm -f $TMP_FILE
            exit 1
        fi
    fi

    printf "%-65s ${GREEN}SUCCESS\n${NC}" "  - checking $input_file..."
done

rm -f $TMP_FILE
exit 0
