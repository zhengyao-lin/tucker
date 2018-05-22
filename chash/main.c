#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mine.h"
#include "test.h"
#include "sha256.h"
#include "common.h"
#include "wagner.h"

int main()
{
    do_test();

    // byte_t raw[] =
    //     "\x00\x00\x00\x00\x6f\xe2\x8c\x0a" "\xb6\xf1\xb3\x72\xc1\xa6\xa2\x46"
    //     "\xae\x63\xf7\x4f\x93\x1e\x83\x65" "\xe1\x5a\x08\x9c\x68\xd6\x19\x00"
    //     "\x00\x00\x00\x00\xf6\xa8\xfc\xb4" "\x03\xb8\x64\xbd\x65\x30\xb1\x6e"
    //     "\xba\xe8\xdf\x20\x47\x6b\x81\x01" "\xc8\x91\x11\x15\xef\x27\x70\xd6"
    //     "\xba\x44\x00\x3a\x29\xab\x5f\x49" "\xff\xff\x00\x1d";

    // hash256_t target =
    //     "\x00\x00\x00\x00\x00\x00\x00\x00" "\x00\x00\x00\x00\x00\x00\x00\x00"
    //     "\x00\x00\x00\x00\x00\x00\x00\x00" "\x00\x00\xff\xff\x00\x00\x00\x00";

    // do_mine(raw, sizeof(raw) - 1, target, 4);

    // printf("%d\n", mask_bits("\x58\x7d\x42", 12, 20));
    // printf("%d\n", mask_bits("\x58\x7d\x82", 12, 20));
    // return 0;

    // printf("%lu\n", sizeof_ctx(2097125));

    #define N 2965504
    // #define N 10000

    // printf("result: %d\n", compare_bits("abd", "abc", 0));
    // return 0;

    hash256_t *test_hash = malloc(N * sizeof(*test_hash));
    char dat[10] = "fgdsad";
    int *hash_idx = (int *)(dat + 6);

    byte_t *list = malloc(WAGNER_N / 8 * N);
    index_t *sol = malloc((1 << WAGNER_K) * sizeof(*sol));

    for (int i = 0; i < N; i++) {
        *hash_idx = i;
        sha256(dat, 10, test_hash[i]);

        // copy the first 25 bytes
        memcpy(list + i * WAGNER_N / 8, test_hash[i], WAGNER_N / 8);
    }

    bool found = wagner_solve(list, N, sol);
    byte_t *str, *prev, res[WAGNER_N / 8];

    printf("%d\n", found);

    if (found) {
        for (int i = 0; i < (1 << WAGNER_K); i++) {
            str = list + sol[i] * (WAGNER_N / 8);

            printf("%3.d: %7.d: ", i, sol[i]);

            for (int j = 0; j < WAGNER_N / 8; j++) {
                print_byte(str[j]);
            }

            if (i == 0) {
                memcpy(res, str, WAGNER_N / 8);
            } else {
                xor_all(res, str, res, WAGNER_N / 8);
            }

            printf("\n");
        }

        for (int j = 0; j < WAGNER_N / 8; j++) {
            print_byte(res[j]);
        }

        printf("\n");
    }

    free(test_hash);
    free(list);
    free(sol);

    return 0;
}
