#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

#include "sha256.h"

void print_byte(char b)
{
    char *tab = "0123456789abcdef";
    printf("%c%c", tab[(b >> 4) & 0xf], tab[b & 0xf]);
}

void print_hash256(const hash256_t hash)
{
    int i;
    
    for (i = 0; i < 32; i++)
        print_byte(hash[i]);

    printf("\n");
}

int hex2int(char h)
{
    if (h >= '0' && h <= '9') return h - '0';
    else return h - 'a' + 10;
}

void read_hash256(const char *str, hash256_t hash)
{
    int i, j;

    for (i = 0, j = 0; i < 32; i++, j += 2) {
        hash[i] = hex2int(str[j]) << 4 | hex2int(str[j + 1]);
    }
}

struct {
    byte_t *dat;
    size_t size;
    char *expected;
} tests[] = {
    { "hello", 5, "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" },
    { "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0", 64, "f5a5fd42d16a20302798ef6ed309979b43003d2320d9f0e8ea9831a92759fb4b" },

    { "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0", 63, "c7723fa1e0127975e49e62e753db53924c1bd84b8ac1ac08df78d09270f3d971" },

    { "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0"
      "\0\0\0\0\0\0\0\0\0", 65, "98ce42deef51d40269d542f5314bef2c7468d401ad5d85168bfab4c0108f75f7" },
};

void do_tests()
{
    int i;
    hash256_t exp, hash;

    for (i = 0; i < sizeof(tests) / sizeof(*tests); i++) {
        sha256(tests[i].dat, tests[i].size, hash);
        read_hash256(tests[i].expected, exp);

        if (memcmp(exp, hash, sizeof(hash)) != 0) {
            fprintf(stderr, "test error for case %d\n", i);
            print_hash256(exp);
            print_hash256(hash);
        }
    }
}

int main()
{
    // printf("%u\n", ROTATER(0x10086, 10));
    // printf("%u\n", ROTATER(0x10086, 0));
    // printf("%u\n", ROTATER(0x10086, 32));
    // printf("%u\n", rotate_l(0x10086, 10));
    // printf("%u\n", rotate_l(0x10086, 0));
    // printf("%u\n", rotate_l(0x10086, 32));

    int i, times = 1 << 22;
    clock_t begin, end;
    byte_t *dat =
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0"
        "\0\0\0\0\0\0\0\0" "\0\0\0\0\0\0\0\0";

    hash256_t hash;

    do_tests();

    printf("hash rate test\n");

    begin = clock();

    for (i = 0; i < times; i++) {
        double_sha256(dat, 8 * 32, hash);
    }

    end = clock();

    printf("%f sec, %d hashes(sha256^2)\n", (double)(end - begin) / CLOCKS_PER_SEC, times);

    return 0;
}
