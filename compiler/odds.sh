#!/bin/bash

MYDIR="$(dirname "$(which "$0")")"
ODDS_FILE="$MYDIR/odds"
STDLIB_FILE="lib.py"

# odds.sh (-c | -r) <odds_file> <output_file>
if [ "$#" -eq 3 ]; then
    if [ "$1" == "-c" ]; then
        $ODDS_FILE $1 $3 $STDLIB_FILE < $2
        exit 0
    elif [ "$1" == "-r" ]; then
        $ODDS_FILE $1 $3 < $2
        exit 0
    else
        printf "ERROR: invalid arguments supplied for command $0 $1" 1>&2
        exit 1
    fi

# odds.sh -s <odds_file>
elif [ "$#" -eq 2 ] && [ "$1" == "-s" ]; then
    $ODDS_FILE $1 < $2
    exit 0

# odds.sh -h
elif [ "$#" -eq 1 ] && [ "$1" == "-h" ]; then
    $ODDS_FILE -h
    exit 0
fi

printf "ERROR: invalid arguments. Run $0 -h for usage instructions" 1>&2
exit 1
