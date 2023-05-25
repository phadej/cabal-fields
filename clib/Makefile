CC:=gcc
CFLAGS:=-O -Wall

# For coverage to work
CFLAGS+=-fprofile-arcs -ftest-coverage

.PHONY: all clean coverage run-coverage

all : demo

clean :
	rm -f demo test *.o *.html *.gcda *.gcno

coverage: run-coverage coverage.html

run-coverage: demo
	@echo "UNIX"
	@for file in ../fixtures/ok/*.txt; do echo $$file; ./demo $$file >/dev/null; done
	@echo "Windows"
	@for file in ../fixtures/ok/*.txt; do echo $$file; ./demo -w $$file >/dev/null; done
	@echo "macOS"
	@for file in ../fixtures/ok/*.txt; do echo $$file; ./demo -m $$file >/dev/null; done
	@echo "Trailing space"
	@for file in ../fixtures/ok/*.txt; do echo $$file; ./demo -t $$file >/dev/null; done
	@echo "Errors"
	@for file in ../fixtures/errors/*.txt; do echo $$file; ./demo $$file >/dev/null || true; done

include deps.mk

deps.mk :
	$(CC) -MM *.c > deps.mk

%.o : %.c
	$(CC) $(CFLAGS) -c -o $@ $<

demo : demo.o cabalfields.o readfile.o
	$(CC) $(CFLAGS) -o $@ $^

.PHONY: coverage.html
coverage.html :
	gcovr -r . --html --html-details -o coverage.html
