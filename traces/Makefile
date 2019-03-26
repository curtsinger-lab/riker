
all: build1 build2

build1: program1/main.c
	cd program1; strace -f -o ../build1 bash -c "gcc -o output main.c"

build2: program2/main.c program2/util.c program2/util.h
	cd program2; strace -f -o ../build2 bash -c "gcc -o output main.c util.c"

