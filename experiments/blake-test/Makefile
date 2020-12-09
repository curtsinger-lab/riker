BP=BLAKE3/c
CFLAGS=-O3

test: test.c ${BP}/blake3.c ${BP}/blake3_dispatch.c ${BP}/blake3_portable.c \
    ${BP}/blake3_sse2_x86-64_unix.S ${BP}/blake3_sse41_x86-64_unix.S ${BP}/blake3_avx2_x86-64_unix.S \
    ${BP}/blake3_avx512_x86-64_unix.S
	clang ${CFLAGS} -o test test.c ${BP}/blake3.c ${BP}/blake3_dispatch.c ${BP}/blake3_portable.c \
    ${BP}/blake3_sse2_x86-64_unix.S ${BP}/blake3_sse41_x86-64_unix.S ${BP}/blake3_avx2_x86-64_unix.S \
    ${BP}/blake3_avx512_x86-64_unix.S

.PHONY: clean
clean:
	rm -f *.o
	rm -f test
