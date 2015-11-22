#!/bin/bash

NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

INPUT_FILES="compiler/fail/*.ods"
TMP_FILE="temp.py"
printf "${CYAN}Running compiler tests...\n${NC}"

for input_file in $INPUT_FILES; do
    expected_error_file=${input_file/.ods/.out}
    exp_err=$(tr -d "[:space:]" < $expected_error_file)
    error=$(../compiler/odds.sh -c $input_file $TMP_FILE 2>&1 | tr -d "[:space:]")

    if [[ "$exp_err" == "$error" ]]; then
        printf "%-65s ${GREEN}SUCCESS\n${NC}" "  - checking $input_file..."
    else
        printf "%-65s ${RED}ERROR\n${NC}" "  - checking $input_file..." 1>&2
        exit 1
    fi

done

rm -f $TMP_FILE
exit 0
