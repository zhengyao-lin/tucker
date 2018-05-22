#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "wagner.h"

// #define FINAL_STAGE (WAGNER_TOTAL_STAGE - 1) // the last collection stage
#define FINAL_STAGE WAGNER_TOTAL_STAGE

void bucket_add(wagner_bucket_t *bucket, index_t idx)
{
    bucket->buck[bucket->size++] = idx;
}

// mask bits from i to j in a string
// precond j - i <= 32
uint32_t mask_bits(byte_t *str, int i, int j)
{
    int ei = i % 8,
        ej = j % 8;

    int k, base = 0;
    int m = k = i / 8,
        n = (j + 7) / 8 - 1;

    uint32_t ret = 0;

    for (; m <= n; m++) {
        if (m == k) {
            // first byte
            if (m == n) {
                ret |= (str[m] >> ei) & ~(~0 << (j - i));
            } else {
                ret |= str[m] >> ei;
                base += 8 - ei;
            }
        } else if (m == n && ej) {
            // last byte
            ret |= ((uint32_t)str[m] & (~(~0 << ej))) << base;
        } else {
            ret |= (uint32_t)str[m] << base;
            base += 8;
        }
    }

    return ret;
}

uint32_t mask_bucket_bits(byte_t *str, int i, int j)
{
    return mask_bits(str, j - WAGNER_BUCKET_BITS, j);
}

uint32_t mask_sort_bits(byte_t *str, int i, int j)
{
    return mask_bits(str, i, j - WAGNER_BUCKET_BITS);
}

// add a new pair to pair set
index_t append_pair(wagner_state_t *state, wagner_pair_set_t *set, index_t a, index_t b)
{
    index_t idx = set->size;

    if (set->size < state->init_nstr) {
        set->pairs[set->size++] = (wagner_pair_t){ a, b };
    }

    return idx;
}

// ONLY bits in the range [i, j) in dst are VALID
void xor_all(const byte_t *a, const byte_t *b, byte_t *dst, int size)
{
    int i;

    for (i = 0; i < size; i++) {
        dst[i] = a[i] ^ b[i];
    }
}

bool has_dup(wagner_pair_t a, wagner_pair_t b)
{
    // return false;
    return a.i == b.i || a.j == b.j || a.i == b.j || a.j == b.i;
}

// append a new pair to the pair set,
// then xor a, b in the current list and put the result to the next list
index_t collide(wagner_state_t *state, index_t ai, index_t bi)
{
    int stage = state->stage;

    wagner_pair_set_t *pair_set = pair_set_at_stage(state, stage);
    wagner_pair_set_t *prev_set;
    byte_t *cur_list = list_at_stage(state, stage);
    byte_t *next_list = list_at_stage(state, stage + 1);

    index_t idx;

    byte_t *a = string_at(cur_list, stage, ai),
           *b = string_at(cur_list, stage, bi),
           *dst;

    byte_t c[WAGNER_LEN_AT_STAGE(stage)];

    int j = WAGNER_J_AT_STAGE(stage);

    int diff;

    // check duplication
    if (stage != 0) {
        prev_set = pair_set_at_stage(state, stage - 1);

        if (has_dup(prev_set->pairs[ai], prev_set->pairs[bi])) {
            return -1;
        }
    }

    // only accept pairs with 2 * WAGNER_BITS bits equal
    // in the last stage
    idx = append_pair(state, pair_set, ai, bi);

    if (idx < state->init_nstr) {
        xor_all(a, b, c, WAGNER_LEN_AT_STAGE(stage));

        diff = WAGNER_LEN_AT_STAGE(stage) - WAGNER_LEN_AT_STAGE(stage + 1);
        dst = string_at(next_list, stage + 1, idx);
        
        memcpy(dst, c + diff, WAGNER_LEN_AT_STAGE(stage + 1));

        return idx;
    }

    return -1;
}

void wagner_transform(wagner_state_t *state)
{
    int stage = state->stage;
    int nstr = state->nstr;

    wagner_pair_set_t *pair_set = pair_set_at_stage(state, stage);
    // wagner_pair_set_t *prev_set;

    wagner_bucket_t *cur_buck;
    wagner_hash_table_t *hashtab = state->hashtab;
    wagner_entry_t *entry;

    byte_t *cur_list = list_at_stage(state, stage);
    byte_t *tmp;

    uint32_t tmp_idx, idx, cur_idx;

    int i = WAGNER_I_AT_STAGE(stage),
        j = WAGNER_J_AT_STAGE(stage),
        k = j - WAGNER_BUCKET_BITS;

    int count;

    // for each string in the current stage
    // bits i -> k are the sort bits
    // bits k -> j are the bucket bits

    int m, n, start, end;

    printf("stage: %d, %d bytes per string: %d, %d, %d\n", stage, nstr, WAGNER_LEN_AT_STAGE(stage), i, j);
    printf("cur list: %p\n", cur_list);

    // init buckets
    for (m = 0; m < WAGNER_BUCKET; m++) {
        state->bucks[m]->size = 0;
    }

    // init pair set
    pair_set->size = 0;

    // linear scan to sort all strings to buckets
    for (m = 0; m < nstr; m++) {
        tmp = string_at(cur_list, stage, m);
        // 0x5576ec4
        bucket_add(state->bucks[mask_bucket_bits(tmp, i, j)], m);
    }

    // printf("bucket sorted\n");

    for (m = 0; m < WAGNER_BUCKET; m++) {
        cur_buck = state->bucks[m];

        // init hash table
        bzero(hashtab, sizeof(*hashtab));

        // printf("bucket size: %d\n", cur_buck->size);

        for (n = 0; n < cur_buck->size; n++) {
            cur_idx = cur_buck->buck[n];

            tmp = string_at(cur_list, stage, cur_idx);

            entry = &hashtab->tab[mask_sort_bits(tmp, i, j)];

            if (entry->count == 0) {
                // add pair
                entry->count++;
                entry->where = cur_idx;
            } else if (entry->count == 1) {
                // add pair (entry->where, cur_buck->buck[n])
                idx = collide(state, entry->where, cur_idx);

                if (idx != -1) {
                    // write result to the next string list
                    entry->count++;
                    entry->where = idx;
                }
            } else {
                // add multiple pairs
               
                // we have, from entry->where, entry->count - 1 pairs
                // (a, k), (b, k), (c, k) ... (a + count - 1, k)
                // we pair (a, b, c .. a + count - 1, k) each with the current index(cur_buck->buck[n])

                start = entry->where;
                end = start + entry->count - 2;

                count = 0;
                idx = -1;

                #define PAIR_WITH(a) \
                    tmp_idx = collide(state, (a), cur_idx); \
                    if (tmp_idx != -1) { \
                        if (idx == -1) idx = tmp_idx; \
                        count++; \
                    }

                // iterate though each previous pair
                for (; start <= end; start++) {
                    PAIR_WITH(pair_set->pairs[start].i);
                }

                // add the last pair
                PAIR_WITH(pair_set->pairs[end].j);

                if (idx != -1) {
                    entry->count = count + 1;
                    entry->where = idx;
                }
            }
        }
    }

    // prepare for the next state
    state->nstr = pair_set->size;
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
        new_size = wagner_trace_solution(state, prev->pairs[from.i], stage - 1, cur_size, sol);

        // traverse right child
        new_size = wagner_trace_solution(state, prev->pairs[from.j], stage - 1, new_size, sol);
    }

    return new_size;
}

bool wagner_finalize(wagner_state_t *state, index_t *sol)
{
    byte_t *last_list = list_at_stage(state, FINAL_STAGE);
    byte_t *tmp;

    int nstr = state->nstr;
    int m, n;

    bool found = false;

    // find any pair that yields zero
    for (m = 0; m < nstr; m++) {
        tmp = string_at(last_list, FINAL_STAGE, m);
        found = true;

        for (n = 0; n < WAGNER_LEN_AT_STAGE(FINAL_STAGE); n++) {
            if (tmp[n]) found = false;
        }

        if (found) break;
    }

    printf("found: %d\n", found);

    wagner_pair_set_t *prev_set = pair_set_at_stage(state, FINAL_STAGE - 1);

    if (found) {
        // trace the tree
        wagner_trace_solution(state, prev_set->pairs[m], FINAL_STAGE - 1, 0, sol);
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
    wagner_hash_table_t hashtab;
    wagner_state_t state = {
        .ctx = NULL,
        .hashtab = &hashtab,
        .nstr = nstr,
        .init_nstr = nstr,
        .stage = 0
    };

    int i;
    bool found;
    size_t size = sizeof_ctx(&state);

    printf("allocating %lu bytes\n", size);

    state.ctx = malloc(size);

    memcpy(list_at_stage(&state, 0), init_list, nstr * WAGNER_LEN_AT_STAGE(0));

    for (i = 0; i < WAGNER_BUCKET; i++) {
        state.bucks[i] = malloc(sizeof_bucket(nstr));
    }

    for (i = 0; i < WAGNER_TOTAL_STAGE; i++) {
        wagner_transform(&state);
    }

    found = wagner_finalize(&state, sol);

    for (i = 0; i < WAGNER_BUCKET; i++) {
        free(state.bucks[i]);
    }

    free(state.ctx);

    return found;
}
