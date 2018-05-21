#include <stdlib.h>
#include <string.h>

#include "wagner.h"

// generalized wagner's algorithm using buckets
// n - number of bits in each string
// k - we want to find 2^k strings that xor to zero, also the number of round - 1
// L - N strings each n bit long
// b - bucket index size(8 bit in default), must be less than n / (k + 1)

// on each round, we only focus on n / (k + 1) bits in a subset A
// 1. put all strings in A to a bucket with last b bits of the string as the index
// 2. in each bucket, sort the string by the first 12 bits
// 3. in each bucket produce a list of pairs (i, j) such that Ai == Aj in the current range
// 4. combine pairs produced in each bucket
// 5. write the final result of xor of each pair to a new subset A'
// 6. repeat the steps in the next round with A'

#define STR_AT(list, i) ((list) + (i) * (WAGNER_N / 8))
#define FINAL_STAGE WAGNER_K

// ONLY bits in the range [i, j) in dst are VALID
void xor_bits(const byte_t *a, const byte_t *b, byte_t *dst, int i, int j)
{
    int m = i / 8, // first byte
        n = j / 8; // last byte

    for (; m <= n; m++) {
        dst[m] = a[m] ^ b[m];
    }
}

// [i, j)
int compare_bits(const byte_t *a, const byte_t *b, int i, int j)
{
    int k, ei = i % 8, ej = j % 8;
    int m = k = i / 8, // first byte
        n = j / 8; // last byte

    byte_t ca, cb;

    for (; m <= n; m++) {
        if (m == k) {
            ca = a[m] >> ei;
            cb = b[m] >> ei;
        } else if (m == n) {
            // trim useless bits in the last byte
            ca = a[m] << (8 - ej);
            cb = b[m] << (8 - ej);
        } else {
            // no trim in a middle byte
            ca = a[m];
            cb = b[m];
        }

        // printf("comparing %d %d %d %d byte\n", m, n, ca, cb);

        if (ca > cb) return 1;
        if (ca < cb) return -1;
    }

    return 0;
}

void swap(index_t *result, int m, int n)
{
    int tmp = result[m];
    result[m] = result[n];
    result[n] = tmp;
}

int partition(byte_t *list, index_t *index, index_t size, int i, int j)
{
    byte_t *pivot = STR_AT(list, index[0]);
    int m = -1, n = size;

    while (1) {
        do m++;
        while (compare_bits(STR_AT(list, index[m]), pivot, i, j) < 0);

        do n--;
        while (compare_bits(STR_AT(list, index[n]), pivot, i, j) > 0);

        if (m >= n) return n;

        swap(index, m, n);
    }
}

void sort_list_c(byte_t *list, index_t *result, index_t size, int i, int j)
{
    int p;

    // printf("size: %d\n", size);

    if (size <= 1) return;

    p = partition(list, result, size, i, j);

    // printf("size: %d %d %d %d\n", size, p, p + 1, size - p - 1);

    if ((int)size < 0)
        *((int *)0) = 1;

    sort_list_c(list, result, p + 1, i, j);
    sort_list_c(list, result + p + 1, size - (p + 1), i, j);
}

// sort list by the bits in range[i, j]
void sort_list(byte_t *list, index_t *index, index_t size, int i, int j)
{
    int k;

    // printf("size: %u %d %d\n", size, i, j);

    // init result list
    for (k = 0; k < size; k++) {
        index[k] = k;
    }

    sort_list_c(list, index, size, i, j);
}

size_t mem_pair_set(int nstr)
{
    return nstr * sizeof(wagner_pair_t) + sizeof(index_t) /* size field */;
}

// storage for one list
size_t mem_list(int nstr)
{
    return nstr * (WAGNER_N / 8);
}

size_t mem_total(int nstr)
{
    return
        mem_pair_set(nstr) * WAGNER_K +
        mem_list(nstr) * 2 + // storage for the list
        nstr * sizeof(index_t); // index list
}

wagner_pair_set_t *pair_set_at_stage(wagner_state_t *state, int stage)
{
    return state->ctx + stage * mem_pair_set(state->init_nstr);
}

byte_t *list_at_stage(wagner_state_t *state, int stage)
{
    byte_t *base = state->ctx + WAGNER_K * mem_pair_set(state->init_nstr);

    if (stage & 1) {
        return base + mem_list(state->init_nstr);
    } else {
        return base;
    }
}

index_t *index_list(wagner_state_t *state)
{
    return state->ctx +
           mem_pair_set(state->init_nstr) * WAGNER_K +
           mem_list(state->init_nstr) * 2;
}

bool append_pair(wagner_state_t *state, wagner_pair_set_t *set, index_t a, index_t b)
{
    if (set->size < state->init_nstr) {
        set->pair[set->size] = (wagner_pair_t){ a, b };
        set->size++;

        return true;
    } else {
        return false;
    }
}

bool has_dup(wagner_pair_t a, wagner_pair_t b)
{
    // return false;
    return a.i == b.i || a.j == b.j; // || a.i == b.j || a.j == b.i;
}

void wagner_transform(wagner_state_t *state)
{
    // sort list
    // linear scan and find common interesting bits(first WAGNER_BITS bits in each string in the list)
    
    int stage = state->stage;

    wagner_pair_set_t *pair_set = pair_set_at_stage(state, stage);
    wagner_pair_set_t *prev_set;

    byte_t *cur_list = list_at_stage(state, stage);
    byte_t *next_list = list_at_stage(state, stage + 1);
    index_t *index = index_list(state);

    int i = WAGNER_BITS * stage,
        j = i + WAGNER_BITS;

    int m, n, k, t;

    byte_t *tmp, *dst;

    index_t cur_nstr = state->cur_nstr;

    printf("stage: %d\n", stage);

    // sort the previous list
    // final index remappings are stored in index
    sort_list(cur_list, index, cur_nstr, i, j);

    printf("sorted\n");

    pair_set->size = 0;
    
    // find collisions
    for (m = 0; m < cur_nstr; m = n) {
        tmp = STR_AT(cur_list, index[m]);

        for (n = m + 1;
             n < cur_nstr &&
             compare_bits(tmp, STR_AT(cur_list, index[n]), i, j) == 0;
             n++)
            if (stage == FINAL_STAGE) {
                // FINAL STAGE: if found, directly return
                append_pair(state, pair_set, index[m], index[n]);
                return;
            }
        
        // strings in the range [m, n) have the common interesting bits

        for (k = m; k < n; k++) {
            for (t = k + 1; t < n; t++) {
                // check for duplicates
                if (stage != 0) {
                    prev_set = pair_set_at_stage(state, stage - 1);
                    if (has_dup(prev_set->pair[index[k]], prev_set->pair[index[t]]))
                        continue;
                }

                // add k - t, pair
                if (append_pair(state, pair_set, index[k], index[t])) {
                    // printf("found pair\n");

                    // generate a new string in the next list
                    dst = STR_AT(next_list, pair_set->size - 1);

                    // xor bits in [i, N)
                    xor_bits(STR_AT(cur_list, index[k]), STR_AT(cur_list, index[t]), dst, i, WAGNER_N);
                } // if not, ignore the pair
            }
        }
    }

    // prepare for the next state
    state->cur_nstr = pair_set->size;
    state->stage++;
}

int wagner_trace_solution(wagner_state_t *state, wagner_pair_t from,
                          int stage, int cur_size, index_t *sol)
{
    wagner_pair_set_t *prev;
    int new_size;

    if (stage == 0) {
        // write the actual index
        sol[cur_size++] = from.i;
        sol[cur_size++] = from.j;
        new_size = cur_size;
    } else {
        // continue tracing
        prev = pair_set_at_stage(state, stage - 1);

        // traverse left child
        new_size = wagner_trace_solution(state, prev->pair[from.i], stage - 1, cur_size, sol);

        if (stage != FINAL_STAGE)
            // traverse right child if it's not the final stage
            new_size = wagner_trace_solution(state, prev->pair[from.j], stage - 1, new_size, sol);
    }

    return new_size;
}

bool wagner_finalize(wagner_state_t *state, index_t *sol)
{
    wagner_pair_set_t *pair_set = pair_set_at_stage(state, FINAL_STAGE);

    if (pair_set->size) {
        // trace the tree
        wagner_trace_solution(state, pair_set->pair[0], FINAL_STAGE, 0, sol);
        return true;
    } else {
        return false;
    }
}

// assuming each string is WAGNER_N-bit long
// maximum nstr pairs found in each stage
// 1 unit = nstr * sizeof(pair) = nstr * 8
// max mem needed = 1 unit * WAGNER_K + 1 unit for storage of the list
bool wagner_solve(const byte_t *init_list, int nstr, index_t *sol)
{
    wagner_state_t state = {
        .ctx = malloc(mem_total(nstr)),
        .cur_nstr = nstr,
        .init_nstr = nstr,
        .stage = 0
    };

    int i;
    bool found;

    printf("allocating %lu bytes\n", mem_total(nstr));

    memcpy(list_at_stage(&state, 0), init_list, mem_list(nstr));

    for (i = 0; i <= WAGNER_K; i++) {
        wagner_transform(&state);
    }

    found = wagner_finalize(&state, sol);

    free(state.ctx);

    return found;
}
