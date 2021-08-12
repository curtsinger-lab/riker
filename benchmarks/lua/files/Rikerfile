#!/bin/sh

CFLAGS="-std=gnu99 -O2 -Wall -Wextra -DLUA_COMPAT_5_3 -DLUA_USE_LINUX"

cd src

gcc $CFLAGS -c `ls *.c | grep -v lua_`

ar rcu liblua.a lapi.o lcode.o lctype.o ldebug.o ldo.o ldump.o lfunc.o lgc.o llex.o lmem.o lobject.o lopcodes.o lparser.o lstate.o lstring.o ltable.o ltm.o lundump.o lvm.o lzio.o lauxlib.o lbaselib.o lcorolib.o ldblib.o liolib.o lmathlib.o loadlib.o loslib.o lstrlib.o ltablib.o lutf8lib.o linit.o 
ranlib liblua.a

gcc $CFLAGS -c -o lua.o lua.c
gcc -std=gnu99 -o lua   lua.o liblua.a -lm -Wl,-E -ldl
gcc $CFLAGS -c -o luac.o luac.c
gcc -std=gnu99 -o luac   luac.o liblua.a -lm -Wl,-E -ldl