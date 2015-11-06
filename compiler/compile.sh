#!/bin/bash

make clean
make 
./odds < $1
