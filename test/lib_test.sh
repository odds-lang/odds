#!/bin/bash

NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

INPUT_FILES="lib/*.ods"
TMP_FILE=$(mktemp "compiled.XXXXX")
printf "${CYAN}Running compiler 'pass' tests...\n${NC}"

for input_file in $INPUT_FILES; do
    output_file=${input_file/.ods/.out}

    # compile odds program to temp python file
    ../compiler/odds.sh -c $input_file $TMP_FILE

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
