# Odds: test Makefile
#  - builds all files needed for testing, then runs tests

default: test

all:
	cd ..; make all
	make

test: build
	./scanner_test.sh
	./parser_test.sh
	./compiler_test.sh

build:
	cd scanner; make
	cd parser; make
	cd sast; make

.PHONY: clean
clean:
	cd scanner; make clean
	cd parser; make clean
	cd sast; make clean
