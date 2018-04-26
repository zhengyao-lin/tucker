#include <string.h>

#include "test.h"
#include "sha256.h"

struct {
    char *dat;
    size_t size;
    char *expected;
} sha256_tests[] = {
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

struct {
    enum {
        ARITH_ADD,
        ARITH_MUL,
        ARITH_DIV,
        ARITH_NEG
    } type;

    char *a;
    char *b;
    char *expected;
} arith_tests[] = {
    {
        ARITH_ADD,
        "7c8975e1e60a5c8337f28edf8c33c3b180360b7279644a9bc1af3c51e6220bf5",
        "98ce42deef51d40269d542f5314bef2c7468d401ad5d85168bfab4c0108f75f7",
        "1458b8bfd65c3086a0c7d1d4be7eb2def49edf7326c2cfb14caaf111f7b180ec"
    },

    {
        ARITH_NEG,
        "0000000000000000000000000000000000000000000000000000000000000000",
        NULL,
        "0000000000000000000000000000000000000000000000000000000000000000"
    },

    {
        ARITH_NEG,
        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        NULL,
        "0100000000000000000000000000000000000000000000000000000000000000"
    },

    {
        ARITH_NEG,
        "00ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        NULL,
        "0001000000000000000000000000000000000000000000000000000000000000"
    },

    {
        ARITH_NEG,
        "7c8975e1e60a5c8337f28edf8c33c3b180360b7279644a9bc1af3c51e6220bf5",
        NULL,
        "84768a1e19f5a37cc80d712073cc3c4e7fc9f48d869bb5643e50c3ae19ddf40a"
    },

    {
        ARITH_MUL,
        "7c8975e1e60a5c8337f28edf8c33c3b180360b7279644a9bc1af3c51e6220bf5",
        "98ce42deef51d40269d542f5314bef2c7468d401ad5d85168bfab4c0108f75f7",
        "a069636ccde661960097ea5309975186078ee752b994de35379ed19207663b16"
    },

    {
        ARITH_MUL,
        "0000000000000000000000000000000000000000000000000000000000000000",
        "98ce42deef51d40269d542f5314bef2c7468d401ad5d85168bfab4c0108f75f7",
        "0000000000000000000000000000000000000000000000000000000000000000"
    },

    {
        ARITH_MUL,
        "0100000000000000000000000000000000000000000000000000000000000000",
        "98ce42deef51d40269d542f5314bef2c7468d401ad5d85168bfab4c0108f75f7",
        "98ce42deef51d40269d542f5314bef2c7468d401ad5d85168bfab4c0108f75f7"
    },

    {
        ARITH_MUL,
        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        "0100000000000000000000000000000000000000000000000000000000000000"
    },
};

void do_test()
{
    int i;
    hash256_t exp, hash, a, b, c;

    // 1557b8c0d55c3085a1c7d1d4bd7fb2ddf49edf7426c1cfb24da9f111f6b181ec
    // 0cee6369b9b5c1b6d1eb521615f0292907158ceead1de97a900ad37c9f335693

    for (i = 0; i < sizeof(arith_tests) / sizeof(*arith_tests); i++) {
        switch (arith_tests[i].type) {
            case ARITH_ADD:
                read_hash256(arith_tests[i].a, a);
                read_hash256(arith_tests[i].b, b);
                read_hash256(arith_tests[i].expected, exp);
                hash256_add(a, b, c);
                
                if (memcmp(exp, c, sizeof(c)) != 0) {
                    fprintf(stderr, "arith test(add) error for case %d\n", i);
                    print_hash256(exp);
                    print_hash256(c);
                }

                break;

            case ARITH_MUL:
                read_hash256(arith_tests[i].a, a);
                read_hash256(arith_tests[i].b, b);
                read_hash256(arith_tests[i].expected, exp);
                hash256_mul(a, b, c);
                
                if (memcmp(exp, c, sizeof(c)) != 0) {
                    fprintf(stderr, "arith test(mul) error for case %d\n", i);
                    print_hash256(exp);
                    print_hash256(c);
                }

                break;

            case ARITH_DIV:
                break;

            case ARITH_NEG:
                read_hash256(arith_tests[i].a, a);
                read_hash256(arith_tests[i].expected, exp);
                hash256_neg(a, c);
                
                if (memcmp(exp, c, sizeof(c)) != 0) {
                    fprintf(stderr, "arith test(neg) error for case %d\n", i);
                    print_hash256(exp);
                    print_hash256(c);
                }

                break;

        }
    }

    // sha256 tests
    for (i = 0; i < sizeof(sha256_tests) / sizeof(*sha256_tests); i++) {
        sha256((byte_t *)sha256_tests[i].dat, sha256_tests[i].size, hash);
        read_hash256(sha256_tests[i].expected, exp);

        if (memcmp(exp, hash, sizeof(hash)) != 0) {
            fprintf(stderr, "sha256 test error for case %d\n", i);
            print_hash256(exp);
            print_hash256(hash);
        }
    }
}
