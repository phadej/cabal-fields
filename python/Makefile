PYTHON:=python3

.PHONY : all build clean test

all: build

build:
	$(PYTHON) setup.py build_ext --inplace --debug

clean:
	rm -rf *.out *.bin *.exe *.o *.a *.so test build

test: build
	$(PYTHON) cabalfields-demo.py ../fixtures/ok/*.txt
