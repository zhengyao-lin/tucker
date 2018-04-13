#ifndef _SHA256_H_
#define _SHA256_H_

#include <stdint.h>

typedef unsigned char byte_t;
typedef uint32_t word_t;
typedef byte_t hash256_t[32];

// size is in bytes
void sha256(const byte_t *dat, size_t size, byte_t *hash);
void double_sha256(const byte_t *dat, size_t size, byte_t *hash);

#endif
