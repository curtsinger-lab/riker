module Main where

import Development.Rattle

main :: IO ()
main = rattleRun rattleOptions $ do
  cmd "gcc  -DHAVE_CONFIG_H -I. -g -O2 -pthread -pthread -Wall -Werror -pedantic -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls  -DNDEBUG  -o  memcached  memcached.c hash.c jenkins_hash.c murmur3_hash.c slabs.c items.c assoc.c thread.c daemon.c stats_prefix.c util.c cache.c bipbuffer.c logger.c crawler.c itoa_ljust.c slab_automove.c authfile.c restart.c proto_text.c proto_bin.c extstore.c crc32c.c storage.c slab_automove_extstore.c  -levent"
  cmd "gcc  -DHAVE_CONFIG_H -I. -g -O2 -pthread -pthread -Wall -Werror -pedantic -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls  -fprofile-arcs  -ftest-coverage  -DMEMCACHED_DEBUG  -o  memcached-debug  memcached.c hash.c jenkins_hash.c murmur3_hash.c slabs.c items.c assoc.c thread.c daemon.c stats_prefix.c util.c cache.c bipbuffer.c logger.c crawler.c itoa_ljust.c slab_automove.c authfile.c restart.c proto_text.c proto_bin.c extstore.c crc32c.c storage.c slab_automove_extstore.c  -levent"
  cmd "gcc  -DHAVE_CONFIG_H -I. -g -O2 -pthread -pthread -Wall -Werror -pedantic -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls  -o  sizes  sizes.c  -levent"
  cmd "gcc  -DHAVE_CONFIG_H -I. -g -O2 -pthread -pthread -Wall -Werror -pedantic -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls  -o  testapp  testapp.c util.c stats_prefix.c jenkins_hash.c murmur3_hash.c cache.c crc32c.c  -levent"
  cmd "gcc  -DHAVE_CONFIG_H -I. -g -O2 -pthread -pthread -Wall -Werror -pedantic -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls  -o  timedrun  timedrun.c  -levent"

