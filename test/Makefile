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
	./fail_test.sh

build:
	cd scanner; make
	cd parser; make

.PHONY: clean
clean:
	cd scanner; make clean
	cd parser; make clean
