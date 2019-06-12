SET_PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
SET_LD_LIBRARY_PATH=""

all: build1 build1-rebuild build1-make build1-clang build2 build2-clang version1

build1: program1/main.c Makefile
	rm -f program1/output
	cd program1; PATH=$(SET_PATH) LD_LIBRARY_PATH=$(SET_LD_LIBRARY_PATH) strace -s999999 -fv -o ../build1 bash -c "gcc -o output main.c"

build1-rebuild: build1
	cd program1; PATH=$(SET_PATH) LD_LIBRARY_PATH=$(SET_LD_LIBRARY_PATH) strace -s999999 -fv -o ../build1-rebuild bash -c "gcc -o output main.c"

build1-make: program1/main.c program1/Makefile Makefile
	rm -f program1/output
	cd program1; PATH=$(SET_PATH) LD_LIBRARY_PATH=$(SET_LD_LIBRARY_PATH) strace -s999999 -fv -o ../build1-make bash -c "make"

build1-clang: program1/main.c Makefile
	rm -f program1/output
	cd program1; PATH=$(SET_PATH) LD_LIBRARY_PATH=$(SET_LD_LIBRARY_PATH) strace -s999999 -fv -o ../build1-clang bash -c "clang -o output main.c"

build2: program2/main.c program2/util.c program2/util.h Makefile
	rm -f program2/output
	cd program2; PATH=$(SET_PATH) LD_LIBRARY_PATH=$(SET_LD_LIBRARY_PATH) strace -s999999 -fv -o ../build2 bash -c "gcc -o output main.c util.c"

build2-clang: program1/main.c program2/util.c program2/util.h Makefile
	rm -f program2/output
	cd program2; PATH=$(SET_PATH) LD_LIBRARY_PATH=$(SET_LD_LIBRARY_PATH) strace -s999999 -fv -o ../build2-clang bash -c "gcc -o output main.c util.c"

version1: version_tests/caller.c version_tests/child.c
	cd version_tests; rm -f caller; rm -f child; gcc -g -o caller caller.c; gcc -g -o child child.c; strace -s999999 -fv -o ../version1 bash -c "./caller"
