module Main where

import Development.Rattle

main :: IO ()
main = rattleRun rattleOptions $ do
  cmd Shell "mkdir  -p  self-build/bin"
  cmd Shell "mkdir  -p  self-build/share/rkr/wrappers"
  cmd Shell "mkdir  -p  .blake3_obj"
  cmd Shell "cd  .blake3_obj"
  cmd Shell "clang  -O3 -g -fstandalone-debug -Wall -Wfatal-errors -Isrc/common -Isrc/rkr -I/Users/dbarowy/Documents/Code/riker/utils/rewrite_shell/deps/BLAKE3/c  -c  /Users/dbarowy/Documents/Code/riker/utils/rewrite_shell/deps/BLAKE3/c/blake3.c /Users/dbarowy/Documents/Code/riker/utils/rewrite_shell/deps/BLAKE3/c/blake3_dispatch.c /Users/dbarowy/Documents/Code/riker/utils/rewrite_shell/deps/BLAKE3/c/blake3_portable.c /Users/dbarowy/Documents/Code/riker/utils/rewrite_shell/deps/BLAKE3/c/blake3_sse2_x86-64_unix.S /Users/dbarowy/Documents/Code/riker/utils/rewrite_shell/deps/BLAKE3/c/blake3_sse41_x86-64_unix.S /Users/dbarowy/Documents/Code/riker/utils/rewrite_shell/deps/BLAKE3/c/blake3_avx2_x86-64_unix.S /Users/dbarowy/Documents/Code/riker/utils/rewrite_shell/deps/BLAKE3/c/blake3_avx512_x86-64_unix.S"
  cmd Shell "cd  .."
  cmd Shell "clang  -O3 -g -fstandalone-debug -Wall -Wfatal-errors -Isrc/common -Isrc/rkr -I/Users/dbarowy/Documents/Code/riker/utils/rewrite_shell/deps/BLAKE3/c  -fPIC  -shared  -o  self-build/share/rkr/rkr-inject.so  src/inject/inject.c src/inject/syscall-amd64.s  -ldl  -lpthread"
  cmd Shell "clang  -O3 -g -fstandalone-debug -Wall -Wfatal-errors -Isrc/common -Isrc/rkr -I/Users/dbarowy/Documents/Code/riker/utils/rewrite_shell/deps/BLAKE3/c  -o  self-build/bin/rkr-launch  src/rkr-launch/launch.c"
  cmd Shell "clang++  -O3 -g -fstandalone-debug -Wall -Wfatal-errors -Isrc/common -Isrc/rkr -I/Users/dbarowy/Documents/Code/riker/utils/rewrite_shell/deps/BLAKE3/c --std=c++17 --std=c++17 -Ideps/CLI11/include  -o  self-build/share/rkr/rkr-wrapper  src/wrapper/wrapper.cc  -ldl"
  cmd Shell "for WRAPPER in $WRAPPERS"
  cmd Shell "do"

