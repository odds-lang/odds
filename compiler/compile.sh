#!/bin/bash

if [ "$#" -ne 3 ] 
then
  echo "Use: ./compile [file_to_compile] [flag] [output.py]"
  exit 1 
else
  make clean
  make 
  cat $1 | ./odds $2 $3
fi
