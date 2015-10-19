#!/bin/sh

INPUT_FILES="scanner/*_input.txt"
echo "# Running scanner tests..."

for input_file in $INPUT_FILES; do
    output_file=${input_file/input/output}
    cat $input_file | cmp -s $output_file -
    if [ "$?" -eq 0 ]; then
        echo "   - checking $input_file against $output_file: SUCCESS"
    else
        echo "   - checking $input_file against $output_file: ERROR"
        exit 1
    fi
done;

exit 0
