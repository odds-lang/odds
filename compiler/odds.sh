#!/bin/bash

MYDIR="$(dirname "$(which "$0")")"
ODDS_FILE="$MYDIR/odds"

if [ "$#" -ne 3 ] || [ "$1" == "-h" ]; then
  $ODDS_FILE -h
  exit 0
fi

cat $2 | $ODDS_FILE $1 $3

exit 0
