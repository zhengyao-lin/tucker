#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

#include "sha256.h"
#include "common.h"
#include "mine.h"

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

    { "\1\1\1\1\1\1\1\1"
      "\1\1\1\1\1\1\1\1"
      "\1\1\1\1\1\1\1\1"
      "\1\1\1\1\1\1\1\1"
      "\1\1\1\1\1\1\1\1"
      "\1\1\1\1\1\1\1\1"
      "\1\1\1\1\1\1\1\1"
      "\1\1\1\1\1\1\1\1", 64, "7c8975e1e60a5c8337f28edf8c33c3b180360b7279644a9bc1af3c51e6220bf5" },
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
        }

        print_hash256(exp);
        print_hash256(hash);
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

    // printf("hash rate test\n");

    // begin = clock();

    // for (i = 0; i < times; i++) {
    //     double_sha256(dat, 8 * 32, hash);
    // }

    // end = clock();

    // printf("%f sec, %d hashes(sha256^2)\n", (double)(end - begin) / CLOCKS_PER_SEC, times);

    byte_t raw[] =
        "\x00\x00\x00\x00\x6f\xe2\x8c\x0a" "\xb6\xf1\xb3\x72\xc1\xa6\xa2\x46"
        "\xae\x63\xf7\x4f\x93\x1e\x83\x65" "\xe1\x5a\x08\x9c\x68\xd6\x19\x00"
        "\x00\x00\x00\x00\xf6\xa8\xfc\xb4" "\x03\xb8\x64\xbd\x65\x30\xb1\x6e"
        "\xba\xe8\xdf\x20\x47\x6b\x81\x01" "\xc8\x91\x11\x15\xef\x27\x70\xd6"
        "\xba\x44\x00\x3a\x29\xab\x5f\x49" "\xff\xff\x00\x1d";

    hash256_t target =
        "\x00\x00\x00\x00\x00\x00\x00\x00" "\x00\x00\x00\x00\x00\x00\x00\x00"
        "\x00\x00\x00\x00\x00\x00\x00\x00" "\x00\x00\xff\xff\x00\x00\x00\x00";

    // printf("%lu\n", sizeof(raw));
    // 1042958299

    // double_sha256(raw, sizeof(raw) - 1, hash);
    // print_hash256(hash);
    // print_hash256(target);

    // msec_t a = get_cpu_ms();

    // for (i = 0; i < 2000000000; i++);

    // printf("%u\n", get_cpu_ms() - a);

    do_mine(raw, sizeof(raw) - 1, target, 4);

    return 0;
}
