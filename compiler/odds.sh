#!/bin/bash

if [ "$#" -ne 3 ]; then
  echo "Usage: $0 [odds_file] [flag] [output_file]" 1>&2
  exit 1
fi

MYDIR="$(dirname "$(which "$0")")"
ODDS_FILE="$MYDIR/odds"

cat $1 | $ODDS_FILE $2 $3

exit 0
