#!/bin/bash

MYDIR="$(dirname "$(which "$0")")"
ODDS_FILE="$MYDIR/odds"

if ([ "$#" -ne 3 ] && [ "$#" -ne 2 ]) || [ "$1" == "-h" ]; then
  $ODDS_FILE -h
  exit 0
fi

$ODDS_FILE $1 $3 < $2

exit 0
