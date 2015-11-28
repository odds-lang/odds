#!/bin/bash

NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

INPUT_FILES="compiler/fail/*.ods"
TMP_FILE=$(mktemp "compiled.XXXXX")
printf "${CYAN}Running compiler 'fail' tests...\n${NC}"

for input_file in $INPUT_FILES; do
    output_file=${input_file/.ods/.out}

    ../compiler/odds.sh -c $input_file $TMP_FILE 2>&1 | cmp -s $output_file -
    if [ "$?" -eq 0 ]; then
        printf "%-65s ${GREEN}SUCCESS\n${NC}" "  - checking $input_file..."
    else
        printf "%-65s ${RED}ERROR\n${NC}" "  - checking $input_file..." 1>&2
        rm -f $TMP_FILE
        exit 1
    fi

done

rm -f $TMP_FILE
exit 0
