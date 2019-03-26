
all: build1

build1: program1/main.c
	cd program1; strace -f -o ../build1 bash -c "gcc -o output main.c"

