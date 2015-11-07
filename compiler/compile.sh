#!/bin/bash

if [ "$#" -ne 3 ]; then
  echo "Usage: $0 [odds_file] [flag] [output_file]" 1>&2
  exit 1
fi

cat $1 | ./odds $2 $3

exit 0
