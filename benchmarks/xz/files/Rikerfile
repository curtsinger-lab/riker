#!/bin/sh

CFLAGS='-DHAVE_CHECK_CRC32 -DHAVE_CHECK_CRC64 -DHAVE_CHECK_SHA256 -DHAVE_DECL_PROGRAM_INVOCATION_NAME -DHAVE_DECODERS -DHAVE_DECODER_ARM -DHAVE_DECODER_ARMTHUMB -DHAVE_DECODER_DELTA -DHAVE_DECODER_IA64 -DHAVE_DECODER_LZMA1 -DHAVE_DECODER_LZMA2 -DHAVE_DECODER_POWERPC -DHAVE_DECODER_SPARC -DHAVE_DECODER_X86 -DHAVE_ENCODERS -DHAVE_ENCODER_ARM -DHAVE_ENCODER_ARMTHUMB -DHAVE_ENCODER_DELTA -DHAVE_ENCODER_IA64 -DHAVE_ENCODER_LZMA1 -DHAVE_ENCODER_LZMA2 -DHAVE_ENCODER_POWERPC -DHAVE_ENCODER_SPARC -DHAVE_ENCODER_X86 -DHAVE_FUTIMENS -DHAVE_INTTYPES_H -DHAVE_MBRTOWC -DHAVE_MF_BT2 -DHAVE_MF_BT3 -DHAVE_MF_BT4 -DHAVE_MF_HC3 -DHAVE_MF_HC4 -DHAVE_POSIX_FADVISE -DHAVE_PTHREAD_CONDATTR_SETCLOCK -DHAVE_STDBOOL_H -DHAVE_STDINT_H -DHAVE_STRUCT_STAT_ST_ATIM_TV_NSEC -DHAVE_WCWIDTH -DHAVE__BOOL -DHAVE___BUILTIN_ASSUME_ALIGNED -DHAVE___BUILTIN_BSWAPXX -DMYTHREAD_POSIX -DNDEBUG -DPACKAGE_BUGREPORT="no" -DPACKAGE_NAME="XZ" -DPACKAGE_URL="no" -DTUKLIB_FAST_UNALIGNED_ACCESS -DASSUME_RAM=128 -D_GNU_SOURCE -Wall -Wextra -pthread'

INCLUDES='-Isrc/liblzma/api -Isrc/liblzma/common -Isrc/liblzma/check -Isrc/liblzma/lz -Isrc/liblzma/rangecoder -Isrc/liblzma/lzma -Isrc/liblzma/delta -Isrc/liblzma/simple -Isrc/common'

# Move some unbuilt files out of the way
mv src/liblzma/check/crc32_small.c src/liblzma/check/crc32_small.c.exclude
mv src/liblzma/check/crc64_small.c src/liblzma/check/crc64_small.c.exclude
mv src/liblzma/check/crc32_tablegen.c src/liblzma/check/crc32_tablegen.c.exclude
mv src/liblzma/check/crc64_tablegen.c src/liblzma/check/crc64_tablegen.c.exclude
mv src/liblzma/rangecoder/price_tablegen.c src/liblzma/rangecoder/price_tablegen.c.exclude

# Build liblzma.so.5.3.1
gcc -fPIC -Wl,--version-script=$PWD/src/liblzma/liblzma.map -shared -Wl,-soname,liblzma.so.5 -o liblzma.so.5.3.1 $CFLAGS src/common/*.c src/liblzma/*/*.c $INCLUDES

# Restore the unbuilt files
mv src/liblzma/check/crc32_small.c.exclude src/liblzma/check/crc32_small.c
mv src/liblzma/check/crc64_small.c.exclude src/liblzma/check/crc64_small.c
mv src/liblzma/check/crc32_tablegen.c.exclude src/liblzma/check/crc32_tablegen.c
mv src/liblzma/check/crc64_tablegen.c.exclude src/liblzma/check/crc64_tablegen.c
mv src/liblzma/rangecoder/price_tablegen.c.exclude src/liblzma/rangecoder/price_tablegen.c

# Link to liblzma.so
rm -f liblzma.so
ln liblzma.so.5.3.1 liblzma.so

INCLUDES='-Isrc/common -Isrc/liblzma/api'

# Build xzdec
gcc $CFLAGS -o xzdec src/common/*.c src/xzdec/*.c $INCLUDES -L. -llzma

# Build xz
gcc $CFLAGS -o xz src/common/*.c src/xz/*.c $INCLUDES -L. -llzma
