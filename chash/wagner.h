#ifndef _WAGNER_H_
#define _WAGNER_H_

#include "common.h"

#define WAGNER_N 200
#define WAGNER_K 9

#define WAGNER_BITS (WAGNER_N / (WAGNER_K + 1)) // bits per stage
#define WAGNER_BUCKET_BITS 8
#define WAGNER_SORT_BITS (WAGNER_BITS - WAGNER_BUCKET_BITS)

// | <-  total bits/stage   -> | 
// [ bucket bits ] [ sort bits ]

#define WAGNER_BUCKET (1 << WAGNER_BUCKET_BITS)

#define WAGNER_TOTAL_STAGE WAGNER_K

#define WAGNER_LEN_AT_STAGE(stage) ((WAGNER_N - (stage) * WAGNER_BITS + 7) / 8)
#define WAGNER_I_AT_STAGE(stage) ((stage) * WAGNER_BITS % 8)
#define WAGNER_J_AT_STAGE(stage) (WAGNER_I_AT_STAGE(stage) + WAGNER_BITS)

// assuming WAGNER_BUCKET_BITS <= 32 && WAGNER_SORT_BITS <= 32

typedef byte_t *wagner_string_list_t;

// we need a hash table: WAGNER_BITS -> 

typedef uint32_t index_t;

typedef struct {
    index_t i, j;
} wagner_pair_t;

typedef struct {
    index_t size;
    wagner_pair_t pairs[];
} wagner_pair_set_t;

typedef struct {
    int count;
    index_t where; // position of the recent pair added
} wagner_entry_t;

typedef struct {
    wagner_entry_t tab[1 << WAGNER_SORT_BITS];
} wagner_hash_table_t;

typedef struct {
    index_t size;
    index_t buck[];
} wagner_bucket_t;

typedef struct {
    wagner_bucket_t *bucks[WAGNER_BUCKET];
    wagner_hash_table_t *hashtab;

    void *ctx;
    int stage;
    int nstr;
    int init_nstr;
} wagner_state_t;

inline static
size_t sizeof_pair_set(int nstr)
{
    return nstr * sizeof(wagner_pair_t) + sizeof(index_t);
}

inline static
size_t sizeof_string_list(int nstr)
{
    return (WAGNER_N / 8) * nstr;
}

inline static
size_t sizeof_bucket(int nstr)
{
    return (nstr / WAGNER_BUCKET) * 2 * sizeof(index_t) + sizeof(index_t);
}

inline static
size_t mem_unit(wagner_state_t *state)
{
    return sizeof_pair_set(state->init_nstr);
}

inline static
wagner_pair_set_t *
pair_set_at_stage(wagner_state_t *state, int stage)
{
    return state->ctx + mem_unit(state) * stage;
}

inline static
byte_t *list_at_stage(wagner_state_t *state, int stage)
{
    byte_t *base = state->ctx + mem_unit(state) * (stage + 2);

    if (stage & 1) {
        return base + state->init_nstr * WAGNER_LEN_AT_STAGE(stage - 1);
    } else
        return base;
}

inline static
byte_t *string_at(byte_t *list, int stage, int i)
{
    return list + WAGNER_LEN_AT_STAGE(stage) * i;
}

inline static
size_t sizeof_ctx(wagner_state_t *state)
{
    size_t max = 0, tmp;
    int i;

    state->ctx = NULL;

    for (i = 0; i < WAGNER_TOTAL_STAGE; i++) {
        tmp = (size_t)(list_at_stage(state, i) +
                       state->init_nstr * WAGNER_LEN_AT_STAGE(i));
    
        if (tmp > max) {
            printf("max at %d\n", i);
            max = tmp;
        }
    }

    return max;

    // return sizeof_pair_set(state->init_nstr) * WAGNER_TOTAL_STAGE +
    //        sizeof_string_list(state->init_nstr) * 2;
}

bool wagner_solve(const byte_t *init_list, int nstr, index_t *sol);

#endif
