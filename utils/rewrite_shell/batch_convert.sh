#!/bin/sh
echo "paper..."; ./rewrite_shell ../../benchmarks/paper/files/Rikerfile > converted/paper.sh; ./gen_rattle converted/paper.sh > ../../benchmarks/paper/files/Main.hs
echo "vim..."; ./rewrite_shell ../../benchmarks/vim/files/Rikerfile > converted/vim.sh
echo "coreutils..."; ./rewrite_shell ../../benchmarks/coreutils/files/Rikerfile > converted/coreutils.sh
echo "redis..."; ./rewrite_shell ../../benchmarks/redis/files/Rikerfile > converted/redis.sh
echo "lsof..."; ./rewrite_shell ../../benchmarks/lsof/files/Rikerfile > converted/lsof.sh
echo "sqlite..."; ./rewrite_shell ../../benchmarks/sqlite/files/Rikerfile > converted/sqlite.sh
echo "xz..."; ./rewrite_shell ../../benchmarks/xz/files/Rikerfile > converted/xz.sh
echo "lua..."; ./rewrite_shell ../../benchmarks/lua/files/Rikerfile > converted/lua.sh
echo "calc..."; ./rewrite_shell ../../benchmarks/calc/files/Rikerfile > converted/calc.sh
echo "memcached..."; ./rewrite_shell ../../benchmarks/memcached/files/Rikerfile > converted/memcached.sh
echo "make..."; ./rewrite_shell ../../benchmarks/make/files/Rikerfile > converted/make.sh
echo "autoconf..."; ./rewrite_shell ../../benchmarks/autoconf/files/Rikerfile > converted/autoconf.sh
echo "xz-clang..."; ./rewrite_shell ../../benchmarks/xz-clang/files/Rikerfile > converted/xz-clang.sh
echo "riker..."; ./rewrite_shell ../../benchmarks/riker/files/Rikerfile > converted/riker.sh
echo "protobuf..."; ./rewrite_shell ../../benchmarks/protobuf/files/Rikerfile > converted/protobuf.sh

