# Odds: Makefile
#  - main entrypoint for building compiler and running tests

build:
	cd compiler; make

test: build
	cd test; make

.PHONY: clean
clean:
	cd compiler; make clean
	cd test; make clean