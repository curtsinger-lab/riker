
all: build1 build2

build1: program1/main.c Makefile
	cd program1; strace -s999999 -f -o ../build1 bash -c "gcc -o output main.c"

build2: program2/main.c program2/util.c program2/util.h Makefile
	cd program2; strace -s999999 -f -o ../build2 bash -c "gcc -o output main.c util.c"

