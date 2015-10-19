# Odds: test Makefile
#  - builds all files needed for testing, then runs tests

test: build
	./scanner_test.sh

build:
	cd scanner; make

.PHONY: clean
clean:
	cd scanner; make clean
