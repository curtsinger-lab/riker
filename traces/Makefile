SET_PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
SET_LD_LIBRARY_PATH=""

all: build1 build1-rebuild build1-make build2

build1: program1/main.c Makefile
	rm -f program1/output
	cd program1; PATH=$(SET_PATH) LD_LIBRARY_PATH=$(SET_LD_LIBRARY_PATH) strace -s999999 -fv -o ../build1 bash -c "gcc -o output main.c"

build1-rebuild: build1
	cd program1; PATH=$(SET_PATH) LD_LIBRARY_PATH=$(SET_LD_LIBRARY_PATH) strace -s999999 -fv -o ../build1-rebuild bash -c "gcc -o output main.c"

build1-make: program1/main.c program1/Makefile Makefile
	rm -f program1/output
	cd program1; PATH=$(SET_PATH) LD_LIBRARY_PATH=$(SET_LD_LIBRARY_PATH) strace -s999999 -fv -o ../build1-make bash -c "make"

build2: program2/main.c program2/util.c program2/util.h Makefile
	rm -f program2/output
	cd program2; PATH=$(SET_PATH) LD_LIBRARY_PATH=$(SET_LD_LIBRARY_PATH) strace -s999999 -fv -o ../build2 bash -c "gcc -o output main.c util.c"

