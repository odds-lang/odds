# Odds: Makefile
#  - main entrypoint for building compiler and running tests

default: build

all: clean build

build:
	cd compiler; make

test: build
	cd test; make

sast_printer: build
	cd test/semantic_analysis; make

.PHONY: clean
clean:
	cd compiler; make clean
	cd test; make clean
