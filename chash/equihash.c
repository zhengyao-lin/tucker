#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "equihash.h"
#include "blake/blake2.h"

// the result needs to be freed
byte_t *gen_problem(const byte_t *dat, size_t size)
{
    byte_t *hashes = malloc(WAGNER_N_BYTE * WAGNER_INIT_NSTR);

    blake2b_state s0, s1;

    uint32_t idx;

    blake2b_init(&s0, WAGNER_N_BYTE * 2);
    blake2b_update(&s0, dat, size);

    // init data
    // generate WAGNER_INIT_NSTR / 2 50-byte hashes
    // then split each into two 25-byte data used for
    // equihash
    for (idx = 0; idx < WAGNER_INIT_NSTR / 2; idx++) {
        s1 = s0;

        blake2b_update(&s1, (byte_t *)&idx, sizeof(idx));

        // hash and take the first 50 bytes(2 * WAGNER_N_BYTE)
        blake2b_final(&s1, hashes + idx * WAGNER_N_BYTE * 2, WAGNER_N_BYTE * 2);
    }

    return hashes;
}

inline static
void xor_string(const byte_t *a, const byte_t *b, byte_t *dst, int size)
{
    int i;

    for (i = 0; i < size; i++) {
        dst[i] = a[i] ^ b[i];
    }
}

bool equihash_verify(const byte_t *dat, size_t size, index_t *sol)
{
    byte_t *prob = gen_problem(dat, size);
    byte_t xor_sum[WAGNER_N_BYTE];

    bool *used = malloc(WAGNER_INIT_NSTR * sizeof(*used));

    index_t idx;
    int i, j;
    
    bool valid = true;

#define STR(i) (prob + (i) * WAGNER_N_BYTE)

    bzero(used, WAGNER_INIT_NSTR * sizeof(*used));

    for (i = 0; i < WAGNER_SOLUTION; i++) {
        idx = sol[i];

        // check solution range
        if (idx >= WAGNER_INIT_NSTR) {
            printf("solution not in range\n");

            valid = false;
            break;
        }

        if (used[idx]) {
            printf("trivial solution %d\n", idx);
            
            valid = false;
            break;
        }

        used[idx] = true;
        
        if (i == 0) {
            memcpy(xor_sum, STR(idx), WAGNER_N_BYTE);
        } else {
            xor_string(STR(idx), xor_sum, xor_sum, WAGNER_N_BYTE);
        }

        // printf("%3.d: %7.d: ", i, idx);

        // for (int j = 0; j < WAGNER_N_BYTE; j++) {
        //     print_byte(STR(idx)[j]);
        // }

        // printf("\n");
    }

#undef STR

    printf("final: ");

    for (j = 0; j < WAGNER_N_BYTE; j++) {
        print_byte(xor_sum[j]);
    }

    printf("\n");

    for (i = 0; i < WAGNER_N_BYTE; i++) {
        if (xor_sum[i] != 0) {
            printf("xor sum not zero on byte %d\n", i);
            valid = false;
            break;
        }
    }

    free(prob);
    free(used);

    return valid;
}

int equihash(const byte_t *dat, size_t size,
             index_t *sols, int max_sol)
{
    byte_t *hashes;
    int i, found;

    msec_t start, end;

    hashes = gen_problem(dat, size);

    start = get_cpu_ms();

    found = wagner_solve(hashes, sols, max_sol);
    
    end = get_cpu_ms();

    printf("searching finished after %dms\n", end - start);

    for (i = 0; i < found; i++) {
        if (!equihash_verify(dat, size, sols + i * WAGNER_SOLUTION)) {
            printf("verification failed on %d\n", i);
            *((int *)0) = 1;
        }
    }

    free(hashes);

    return found;
}
