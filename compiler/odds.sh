#!/bin/bash

if [ "$#" -ne 3 ]; then
  echo "Usage: $0 [flag] [input_file] [output_file]" 1>&2
  exit 1
fi

MYDIR="$(dirname "$(which "$0")")"
ODDS_FILE="$MYDIR/odds"

cat $2 | $ODDS_FILE $1 $3

exit 0
