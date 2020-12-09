#include "BLAKE3/c/blake3.h"
#include <stdio.h>
#include <stdlib.h>

#define BUFSZ 65536

/*
 * Modified example from BLAKE3/c/README.md.
 */

int main(int argc, char **argv) {
    // did the user supply a file?
    if (argc != 2) {
        printf("Usage: %s <file>\n", argv[0]);
        printf("\tCalculates and prints the BLAKE3 hash of <file>.\n");
    }

    // initialize hasher
    blake3_hasher hasher;
    blake3_hasher_init(&hasher);

    // buffer for file read
    unsigned char buf[BUFSZ];

    // read from given file
    FILE *f = fopen(argv[1], "r");
    if (!f) {
        printf("Unable to open file '%s'\n", argv[1]);
        exit(EXIT_FAILURE);
    }

    ssize_t n;
    unsigned int i = 0;
    while ((n = fread(buf, sizeof(char), sizeof(buf), f)) > 0) {
        i++;
        blake3_hasher_update(&hasher, buf, n);
    }
    fclose(f);

    // finalize the hash, whatever that means
    uint8_t output[BLAKE3_OUT_LEN];
    blake3_hasher_finalize(&hasher, output, BLAKE3_OUT_LEN);

    // print the hash in hexadecimal
    for (size_t i = 0; i < BLAKE3_OUT_LEN; i++) {
        printf("%02x", output[i]);
    }
    printf("\n");

    return 0;
}