#ifndef _HASH256_H_
#define _HASH256_H_

#include "common.h"

// -1 if a < b, 0 if a == b, 1 if a > b
// hash is interpreted in little-endianness
int8_t hash256_compare(const hash256_t a, const hash256_t b);

void hash256_add(const hash256_t a, const hash256_t b, hash256_t result);
void hash256_mul(const hash256_t a, const hash256_t b, hash256_t result);
void hash256_neg(const hash256_t a, hash256_t result);

#endif
