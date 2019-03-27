
all: build1 build2

build1: program1/main.c Makefile
	rm -f program1/output
	cd program1; strace -s999999 -fv -o ../build1 bash -c "gcc -o output main.c"

build2: program2/main.c program2/util.c program2/util.h Makefile
	rm -f program2/output
	cd program2; strace -s999999 -fv -o ../build2 bash -c "gcc -o output main.c util.c"

