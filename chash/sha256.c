#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sha256.h"

// #define assert(c) ({ if (!(c)) { fprintf(stderr, "assert failure: " #c); fprintf(stderr, "\n"); abort(); } })

/* 32-bit */
/* n < 32 */
#define ONES(n) (~((word_t)0xffffffff << (n)))

#define ROTATER(v, n) (((v) >> (n)) | ((v) << (32 - (n))))
#define ROTATEL(v, n) (((v) << (n)) | ((v) >> (32 - (n))))

#define CH(x, y, z) (((x) & (y)) ^ (~(x) & (z)))
#define MAJ(x, y, z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define EP0(x) (ROTATER(x, 2) ^ ROTATER(x, 13) ^ ROTATER(x, 22))
#define EP1(x) (ROTATER(x, 6) ^ ROTATER(x, 11) ^ ROTATER(x, 25))
#define SIG0(x) (ROTATER(x, 7) ^ ROTATER(x, 18) ^ ((x) >> 3))
#define SIG1(x) (ROTATER(x, 17) ^ ROTATER(x, 19) ^ ((x) >> 10))

word_t k[] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

static inline word_t
to_be(word_t v)
{
    uint8_t b[4];

    b[3] = v;
    b[2] = v >> 8;
    b[1] = v >> 16;
    b[0] = v >> 24;

    return *(word_t *)b;
}

static inline uint64_t
to_be64(uint64_t v)
{
    uint8_t b[8];

    b[7] = v >> 0;
    b[6] = v >> 8;
    b[5] = v >> 16;
    b[4] = v >> 24;
    b[3] = v >> 32;
    b[2] = v >> 40;
    b[1] = v >> 48;
    b[0] = v >> 56;

    return *(uint64_t *)b;
}

/* see https://en.wikipedia.org/wiki/SHA-2 for the pseudocode */
void digest_chunk(ctx_sha256_t *ctx, const word_t chunk[16])
{
    int i;
    word_t tmp1, tmp2;
    ctx_sha256_t th;
    word_t w[64];

    // memcpy(w, chunk, 16 * sizeof(word_t));
    // write the data in big-endian
    for (i = 0; i < 16; i++)
        w[i] = to_be(chunk[i]);

    for (; i < 64; i++)
        w[i] = w[i - 16] + SIG0(w[i - 15]) + w[i - 7] + SIG1(w[i - 2]);

    // printf("check: %d\n", w[26]);

    th = *ctx;

    // printf("%u, %u, %u, %u, %u, %u, %u, %u\n", th.h0, th.h1, th.h2, th.h3, th.h4, th.h5, th.h6, th.h7);

    for (i = 0; i < 64; i++) {
        tmp1 = th.h7 + EP1(th.h4) + CH(th.h4, th.h5, th.h6) + k[i] + w[i];
		tmp2 = EP0(th.h0) + MAJ(th.h0, th.h1, th.h2);

        th.h7 = th.h6;
        th.h6 = th.h5;
        th.h5 = th.h4;
        th.h4 = th.h3 + tmp1;
        th.h3 = th.h2;
        th.h2 = th.h1;
        th.h1 = th.h0;
        th.h0 = tmp1 + tmp2;
    }

    ctx->h0 += th.h0;
    ctx->h1 += th.h1;
    ctx->h2 += th.h2;
    ctx->h3 += th.h3;
    ctx->h4 += th.h4;
    ctx->h5 += th.h5;
    ctx->h6 += th.h6;
    ctx->h7 += th.h7;
}

void write_ctx(ctx_sha256_t *ctx, hash256_t hash)
{
    ((word_t *)hash)[0] = to_be(ctx->h0);
    ((word_t *)hash)[1] = to_be(ctx->h1);
    ((word_t *)hash)[2] = to_be(ctx->h2);
    ((word_t *)hash)[3] = to_be(ctx->h3);
    ((word_t *)hash)[4] = to_be(ctx->h4);
    ((word_t *)hash)[5] = to_be(ctx->h5);
    ((word_t *)hash)[6] = to_be(ctx->h6);
    ((word_t *)hash)[7] = to_be(ctx->h7);
}

/* return k(number of 0 bytes to append) */
/* l in bytes */
static inline size_t
get_k(size_t l)
{
    /* l + 1 + 64 + k == 0 (mod 512) */
    size_t e = (l + 1 + 8) % CHUNK_SIZE;
    return e ? CHUNK_SIZE - e : 0;
}

// take as many chunks as possible from data
// and return the processed length
size_t sha256_update(ctx_sha256_t *ctx, const byte_t *dat, size_t osize)
{
    size_t i;

    for (i = 0; i + CHUNK_SIZE <= osize; i += CHUNK_SIZE) {
        digest_chunk(ctx, (word_t *)(dat + i));
    }

    return i;
}

void sha256_finalize(ctx_sha256_t *ctx, const byte_t *remain, size_t rsize, size_t osize)
{
    size_t k = get_k(rsize);
    size_t nappend = 1 + k + 8;

    word_t last_chunk[16] = {0};

    // assert rsize < 512

    // fill in the rest of the data
    memcpy(last_chunk, remain, rsize);
    // set the appending 1 bit
    ((byte_t *)last_chunk)[rsize] = 0x80;

    if (nappend > CHUNK_SIZE) {
        // digest the data first, then the appending data
        digest_chunk(ctx, last_chunk);
        memset(last_chunk, 0, sizeof(last_chunk));
    }

    *(uint64_t *)(last_chunk + 14) = to_be64(osize * 8); // set bit length

    digest_chunk(ctx, last_chunk);
}

static inline void
sha256_i(const byte_t *dat, size_t osize, byte_t *hash)
{
    ctx_sha256_t ctx = INIT_CTX;

    size_t processed = sha256_update(&ctx, dat, osize);

    sha256_finalize(&ctx, dat + processed, osize - processed, osize);

    write_ctx(&ctx, hash);
}

void sha256(const byte_t *dat, size_t osize, byte_t *hash)
{
    sha256_i(dat, osize, hash);
}

void double_sha256(const byte_t *dat, size_t osize, byte_t *hash)
{
    hash256_t hash0;
    sha256_i(dat, osize, hash0);
    sha256_i(hash0, 32, hash);
}
