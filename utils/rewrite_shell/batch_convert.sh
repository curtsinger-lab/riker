#!/bin/sh
mkdir -p converted

echo "paper..."
./rewrite_shell ../../benchmarks/paper/files/Rikerfile > converted/paper.sh
./gen_rattle converted/paper.sh > ../../benchmarks/paper/files/Main.hs

echo "vim..."
./rewrite_shell ../../benchmarks/vim/files/Rikerfile > converted/vim.sh
./gen_rattle converted/vim.sh > ../../benchmarks/vim/files/Main.hs

echo "coreutils..."
./rewrite_shell ../../benchmarks/coreutils/files/Rikerfile > converted/coreutils.sh
./gen_rattle converted/coreutils.sh > ../../benchmarks/coreutils/files/Main.hs

echo "redis..."
./rewrite_shell ../../benchmarks/redis/files/Rikerfile > converted/redis.sh
./gen_rattle converted/redis.sh > ../../benchmarks/redis/files/Main.hs

echo "lsof..."
./rewrite_shell ../../benchmarks/lsof/files/Rikerfile > converted/lsof.sh
./gen_rattle converted/lsof.sh > ../../benchmarks/lsof/files/Main.hs

echo "sqlite..."
./rewrite_shell ../../benchmarks/sqlite/files/Rikerfile > converted/sqlite.sh
./gen_rattle converted/sqlite.sh > ../../benchmarks/sqlite/files/Main.hs

echo "xz..."
./rewrite_shell ../../benchmarks/xz/files/Rikerfile > converted/xz.sh
./gen_rattle converted/xz.sh > ../../benchmarks/xz/files/Main.hs

echo "lua..."
./rewrite_shell ../../benchmarks/lua/files/Rikerfile > converted/lua.sh
./gen_rattle converted/lua.sh > ../../benchmarks/lua/files/Main.hs

echo "calc..."
./rewrite_shell ../../benchmarks/calc/files/Rikerfile > converted/calc.sh
./gen_rattle converted/calc.sh > ../../benchmarks/calc/files/Main.hs

echo "memcached..."
./rewrite_shell ../../benchmarks/memcached/files/Rikerfile > converted/memcached.sh
./gen_rattle converted/memcached.sh > ../../benchmarks/memcached/files/Main.hs

echo "make..."
./rewrite_shell ../../benchmarks/make/files/Rikerfile > converted/make.sh
./gen_rattle converted/make.sh > ../../benchmarks/make/files/Main.hs

echo "autoconf..."
./rewrite_shell ../../benchmarks/autoconf/files/Rikerfile > converted/autoconf.sh
./gen_rattle converted/autoconf.sh > ../../benchmarks/autoconf/files/Main.hs

echo "xz-clang..."
./rewrite_shell ../../benchmarks/xz-clang/files/Rikerfile > converted/xz-clang.sh
./gen_rattle converted/xz-clang.sh > ../../benchmarks/xz-clang/files/Main.hs

echo "riker..."
./rewrite_shell ../../benchmarks/riker/files/Rikerfile > converted/riker.sh
./gen_rattle converted/riker.sh > ../../benchmarks/riker/files/Main.hs

echo "protobuf..."
./rewrite_shell ../../benchmarks/protobuf/files/Rikerfile > converted/protobuf.sh
./gen_rattle converted/protobuf.sh > ../../benchmarks/protobuf/files/Main.hs

rm -rf converted