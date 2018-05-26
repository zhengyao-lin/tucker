#ifndef _WAGNER_H_
#define _WAGNER_H_

#include "common.h"

#define WAGNER_N 200
#define WAGNER_K 9

#define WAGNER_N_BYTE (WAGNER_N / 8)
#define WAGNER_BITS (WAGNER_N / (WAGNER_K + 1)) // bits per stage
#define WAGNER_BUCKET_BITS 8
#define WAGNER_COLLISION_BITS (WAGNER_BITS - WAGNER_BUCKET_BITS)

// | <-  total bits/stage   -> | 
// [ bucket bits ] [ sort bits ]

#define WAGNER_BUCKET (1 << WAGNER_BUCKET_BITS)

#define WAGNER_SOLUTION (1 << WAGNER_K)

#define WAGNER_TOTAL_STAGE WAGNER_K
#define WAGNER_TOTAL_CHUNK (WAGNER_K + 1)

#define WAGNER_INIT_NSTR (1 << (WAGNER_BITS + 1))

// MAX_IDX_BITS is the bit length of MAX_PAIR
// these two definitions are correlated and need to be changed together
#define WAGNER_MAX_PAIR ((size_t)(WAGNER_INIT_NSTR))
#define WAGNER_MAX_IDX_BITS (WAGNER_BITS + 1)

#define WAGNER_LEN_AT_STAGE(stage) ((WAGNER_N - (stage) * WAGNER_BITS + 7) / 8)
#define WAGNER_ALIGNED_LEN_AT_STAGE(stage) ((WAGNER_N - (stage) * WAGNER_BITS + 31) / 32 * 4)
#define WAGNER_OFS_AT_STAGE(stage) ((stage) * WAGNER_BITS % 8)

#define WAGNER_BUCKET_ELEM ((WAGNER_MAX_PAIR / WAGNER_BUCKET) * 5 / 4)
#define WAGNER_BUCKET_SIZE sizeof(wagner_bucket_t)
#define WAGNER_PAIR_SET_SIZE sizeof(wagner_pair_set_t)
#define WAGNER_MEM_UNIT WAGNER_PAIR_SET_SIZE

#if WAGNER_ALIGNED_LEN_AT_STAGE(0) == WAGNER_LEN_AT_STAGE(0)
    #define WAGNER_INIT_ALIGNED 1
#else
    #define WAGNER_INIT_ALIGNED 0
#endif

// #define WAGNER_CHUNK_AT_STAGE(stage) (WAGNER_TOTAL_STAGE - (stage) + 1)

#if WAGNER_BITS <= 32
    typedef uint32_t wagner_chunk_t; // WAGNER_BITS of data
    typedef uint32_t index_t;
#elif WAGNER_BITS <= 64
    typedef uint64_t wagner_chunk_t;
    typedef uint64_t index_t;
#else
    #error "too many bits per round"
#endif

typedef struct {
    index_t i, j;
} wagner_pair_t;

typedef struct {
    wagner_pair_t pairs[WAGNER_MAX_PAIR];
} wagner_pair_set_t;

typedef struct {
    uint32_t count;
    uint32_t where; // position of the recent pair added
} wagner_entry_t;

typedef struct {
    wagner_entry_t tab[1 << WAGNER_COLLISION_BITS];
} wagner_hash_table_t;

typedef struct {
    index_t idx;
    wagner_chunk_t head;
} wagner_bucket_item_t;

typedef struct {
    // index_t size;
    wagner_bucket_item_t buck[WAGNER_BUCKET_ELEM];
} wagner_bucket_t;

typedef struct {
    void *ctx;

    wagner_bucket_t *bucks;
    wagner_hash_table_t *hashtab;

    index_t *sols;
    int max_sol;

    int nstr;
} wagner_state_t;

int wagner_solve(const byte_t *init_list, index_t *sols, int max_sol);

#endif
