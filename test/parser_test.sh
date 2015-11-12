#!/bin/bash

NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

INPUT_FILES="parser/*.in"
printf "${CYAN}Running parser tests...\n${NC}"

for input_file in $INPUT_FILES; do
    output_file=${input_file/.in/.out}
    input=$(parser/parserize < $input_file | tr -d "[:space:]")
    output=$(tr -d "[:space:]" < $output_file);
    if [[ "$input" == "$output" ]]; then
        printf "%-65s ${GREEN}SUCCESS\n${NC}" "  - checking $input_file..."
    else
        printf "%-65s ${RED}ERROR\n${NC}" "  - checking $input_file..." 1>&2
        exit 1
    fi
done

exit 0
