#!/bin/bash

MYDIR="$(dirname "$(which "$0")")"
ODDS_FILE="$MYDIR/compiler/odds"
ODDS_LIB="$MYDIR/compiler/lib/lib.ods"
PY_LIB="$MYDIR/compiler/lib/core.py"

if [ ! -f $ODDS_FILE ]; then
    printf "ERROR: not yet compiled, run 'make' first.\n" 1>&2
    exit 1
fi

# odds.sh (-c | -r) <odds_file> <output_file>
if [ "$#" -eq 3 ]; then
    if [ "$1" == "-c" ]; then
        cat $ODDS_LIB $2 | $ODDS_FILE $1 $3 $PY_LIB
        exit 0
    elif [ "$1" == "-r" ]; then
        $ODDS_FILE $1 $3 < $2
        exit 0
    else
        printf "ERROR: invalid arguments supplied for command $0 $1\n" 1>&2
        exit 1
    fi
fi

# odds.sh -s <odds_file>
if [ "$#" -eq 2 ] && [ "$1" == "-s" ]; then
    $ODDS_FILE $1 < $2
    exit 0
fi

# odds.sh -h
if [ "$#" -eq 1 ] && [ "$1" == "-h" ]; then
    $ODDS_FILE -h
    exit 0
fi

printf "ERROR: invalid arguments. Run $0 -h for usage instructions\n" 1>&2
exit 1
