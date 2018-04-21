#ifndef _SHA256_H_
#define _SHA256_H_

#include "common.h"

typedef struct {
    word_t h0, h1, h2, h3, h4, h5, h6, h7;
} ctx_sha256_t;

#define CHUNK_SIZE (512 / 8) // in bytes
#define INIT_CTX { 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 }

// intermediate steps that could be used for optimization
size_t sha256_update(ctx_sha256_t *ctx, const byte_t *dat, size_t osize);
void sha256_finalize(ctx_sha256_t *ctx, const byte_t *remain, size_t rsize, size_t osize);
void write_ctx(ctx_sha256_t *ctx, hash256_t hash);

// size is in bytes
void sha256(const byte_t *dat, size_t size, byte_t *hash);
void double_sha256(const byte_t *dat, size_t size, byte_t *hash);

#endif
