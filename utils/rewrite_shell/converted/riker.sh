#!/bin/sh

 

 
 
 

 
clang  -fPIC  -shared  -O3  -o  libblake3.so  deps/BLAKE3/c/blake3.c  deps/BLAKE3/c/blake3_dispatch.c  deps/BLAKE3/c/blake3_portable.c  deps/BLAKE3/c/blake3_sse2_x86-64_unix.S  deps/BLAKE3/c/blake3_sse41_x86-64_unix.S  deps/BLAKE3/c/blake3_avx2_x86-64_unix.S  deps/BLAKE3/c/blake3_avx512_x86-64_unix.S  

clang++  -O3 -Isrc -Ideps/cereal/include -Ideps/CLI11/include -Ideps/BLAKE3/c -Wall -Wfatal-errors  --std=c++17  -o  rkr  src/*/*.cc  -L.  -lblake3  -lstdc++fs  -lfmt  

clang  -O3 -Isrc -Ideps/cereal/include -Ideps/CLI11/include -Ideps/BLAKE3/c -Wall -Wfatal-errors  -o  rkr-launch  launch/launch.c  
