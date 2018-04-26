#include <string.h>

#include "hash256.h"

int8_t hash256_compare(const hash256_t a, const hash256_t b)
{
    int i, cmp;

    for (i = sizeof(hash256_t) - 1; i >= 0; i--) {
        cmp = ((byte_t *)a)[i] - ((byte_t *)b)[i];

        if (cmp > 0) return 1; // a > b
        else if (cmp < 0) return -1; // a < b
        // cmp == 0, continue
    }
    
    // a == b
    return 0;
}

void hash256_add(const hash256_t a, const hash256_t b, hash256_t result)
{
    int i, c, carry = 0;

    for (i = 0; i < sizeof(hash256_t); i++) {
        c = ((byte_t *)a)[i] + ((byte_t *)b)[i] + carry;
        ((byte_t *)result)[i] = c;
        carry = (c >> 8) & 0xff;
    }
}

static inline void
hash256_mul_single_acc(byte_t a, const hash256_t b, int ofs, hash256_t result)
{
    int i, c, d;
    byte_t c0 = 0, c1 = 0; // c0 for multiplication carry, c1 for addition carry

    for (i = 0; i + ofs < sizeof(hash256_t); i++) {
        c = a * ((byte_t *)b)[i] + c0;
        d = ((byte_t *)result)[i + ofs] + (byte_t)c + c1;
        
        ((byte_t *)result)[i + ofs] = d;

        c0 = (c >> 8) & 0xff;
        c1 = (d >> 8) & 0xff;
    }
}

// small n(32), use long multiplication
void hash256_mul(const hash256_t a, const hash256_t b, hash256_t result)
{
    int i;

    bzero(result, sizeof(hash256_t));

    for (i = 0; i < sizeof(hash256_t); i++) {
        if (((byte_t *)a)[i])
            hash256_mul_single_acc(((byte_t *)a)[i], b, i, result);
    }
}

void hash256_neg(const hash256_t a, hash256_t result)
{
    int i;

    for (i = 0; i < sizeof(hash256_t); i++) {
        result[i] = ~a[i];
    }

    for (i = 0; i < sizeof(hash256_t); i++) {
        if ((result[i] += 1) != 0) // no overflow
            break;
    }
}
