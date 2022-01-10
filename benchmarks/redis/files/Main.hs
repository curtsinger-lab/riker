module Main where

import Development.Rattle

main :: IO ()
main = rattleRun rattleOptions $ do
  cmd Shell "cd  src"
  cmd Shell "echo  #define REDIS_GIT_SHA1 \"a3bf4f66\"  > release.h"
  cmd Shell "echo  #define REDIS_GIT_DIRTY \"94\"  >> release.h"
  cmd Shell "echo  #define REDIS_BUILD_ID \"williams-197-253.williams.edu-1641849767\"  >> release.h"
  cmd Shell "gcc  -g -ggdb -pedantic -DREDIS_STATIC= -std=c11 -Wall -W -Wno-missing-field-initializers -O2 -g -ggdb -I../deps/hiredis -I../deps/linenoise -I../deps/lua/src -I../deps/hdr_histogram -DUSE_JEMALLOC -I../deps/jemalloc/include  -c"
  cmd Shell "gcc  -g -ggdb -pedantic -DREDIS_STATIC= -std=c11 -Wall -W -Wno-missing-field-initializers -O2 -g -ggdb -I../deps/hiredis -I../deps/linenoise -I../deps/lua/src -I../deps/hdr_histogram -DUSE_JEMALLOC -I../deps/jemalloc/include  -o  redis-server  adlist.o quicklist.o ae.o anet.o dict.o server.o sds.o zmalloc.o lzf_c.o lzf_d.o pqsort.o zipmap.o sha1.o ziplist.o release.o networking.o util.o object.o db.o replication.o rdb.o t_string.o t_list.o t_set.o t_zset.o t_hash.o config.o aof.o pubsub.o multi.o debug.o sort.o intset.o syncio.o cluster.o crc16.o endianconv.o slowlog.o scripting.o bio.o rio.o rand.o memtest.o crcspeed.o crc64.o bitops.o sentinel.o notify.o setproctitle.o blocked.o hyperloglog.o latency.o sparkline.o redis-check-rdb.o redis-check-aof.o geo.o lazyfree.o module.o evict.o expire.o geohash.o geohash_helper.o childinfo.o defrag.o siphash.o rax.o t_stream.o listpack.o localtime.o lolwut.o lolwut5.o lolwut6.o acl.o gopher.o tracking.o connection.o tls.o sha256.o timeout.o setcpuaffinity.o monotonic.o mt19937-64.o  -rdynamic ../deps/hiredis/libhiredis.a ../deps/lua/src/liblua.a ../deps/hdr_histogram/hdr_histogram.o ../deps/linenoise/linenoise.o ../deps/jemalloc/lib/libjemalloc.a -lm -lpthread -ldl"
  cmd Shell "gcc  -g -ggdb -pedantic -DREDIS_STATIC= -std=c11 -Wall -W -Wno-missing-field-initializers -O2 -g -ggdb -I../deps/hiredis -I../deps/linenoise -I../deps/lua/src -I../deps/hdr_histogram -DUSE_JEMALLOC -I../deps/jemalloc/include  -o  redis-cli  anet.o adlist.o dict.o redis-cli.o zmalloc.o release.o ae.o crcspeed.o crc64.o siphash.o crc16.o monotonic.o cli_common.o mt19937-64.o  -rdynamic ../deps/hiredis/libhiredis.a ../deps/lua/src/liblua.a ../deps/hdr_histogram/hdr_histogram.o ../deps/linenoise/linenoise.o ../deps/jemalloc/lib/libjemalloc.a -lm -lpthread -ldl"
  cmd Shell "gcc  -g -ggdb -pedantic -DREDIS_STATIC= -std=c11 -Wall -W -Wno-missing-field-initializers -O2 -g -ggdb -I../deps/hiredis -I../deps/linenoise -I../deps/lua/src -I../deps/hdr_histogram -DUSE_JEMALLOC -I../deps/jemalloc/include  -o  redis-benchmark  ae.o anet.o redis-benchmark.o adlist.o dict.o zmalloc.o release.o crcspeed.o crc64.o siphash.o crc16.o monotonic.o cli_common.o mt19937-64.o  -rdynamic ../deps/hiredis/libhiredis.a ../deps/lua/src/liblua.a ../deps/hdr_histogram/hdr_histogram.o ../deps/linenoise/linenoise.o ../deps/jemalloc/lib/libjemalloc.a -lm -lpthread -ldl"
  cmd Shell "install  redis-server  redis-sentinel"
  cmd Shell "install  redis-server  redis-check-aof"
  cmd Shell "install  redis-server  redis-check-rdb"

