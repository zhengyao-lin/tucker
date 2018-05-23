#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "wagner.h"

// #define FINAL_STAGE (WAGNER_TOTAL_STAGE - 1) // the last collection stage
#define FINAL_STAGE WAGNER_TOTAL_STAGE

inline static
size_t sizeof_pair_set(int nstr)
{
    return nstr * sizeof(wagner_pair_t) + sizeof(index_t);
}

inline static
index_t max_bucket_size(int nstr)
{
    return (nstr / WAGNER_BUCKET) * WAGNER_BUCKET_RATIO;
}

inline static
size_t sizeof_bucket(int nstr)
{
    return max_bucket_size(nstr) * sizeof(index_t) + sizeof(index_t);
}

inline static
size_t mem_unit(wagner_state_t *state)
{
    return sizeof_pair_set(WAGNER_MAX_PAIR);
}

inline static
wagner_pair_set_t *
pair_set_at_stage(wagner_state_t *state, int stage)
{
    return (wagner_pair_set_t *)((byte_t *)state->ctx + mem_unit(state) * stage);
}

inline static
wagner_chunk_t *list_at_stage(wagner_state_t *state, int stage)
{
    wagner_chunk_t *base =
        (wagner_chunk_t *)((byte_t *)state->ctx + mem_unit(state) * (stage + 2));

    if (stage & 1) {
        return base + WAGNER_MAX_PAIR * WAGNER_CHUNK_AT_STAGE(stage - 1);
    } else
        return base;
}

inline static
wagner_chunk_t *string_at(wagner_chunk_t *list, int stage, int i)
{
    return list + WAGNER_CHUNK_AT_STAGE(stage) * i;
}

inline static
size_t sizeof_ctx(wagner_state_t *state)
{
    size_t max = 0, tmp;
    int i;

    state->ctx = NULL;

    for (i = 0; i < WAGNER_TOTAL_STAGE; i++) {
        tmp = (size_t)(list_at_stage(state, i) +
                       WAGNER_MAX_PAIR * WAGNER_CHUNK_AT_STAGE(i));
    
        if (tmp > max) max = tmp;
    }

    return max;
}

inline static
wagner_pair_t to_pair(index_t i, index_t j)
{
    return (wagner_pair_t) { i, j };
}

inline static
index_t pair_i(wagner_pair_t pair)
{
    return pair.i;
}

inline static
index_t pair_j(wagner_pair_t pair)
{
    return pair.j;
}

inline static
void bucket_add(wagner_state_t *state, wagner_bucket_t *bucket, index_t idx)
{
    if (bucket->size < max_bucket_size(WAGNER_MAX_PAIR)) {
        bucket->buck[bucket->size++] = idx;
    } else {
        // printf("bucket overflow %d %d\n", bucket->size, max_bucket_size(WAGNER_MAX_PAIR));
        // *((int *)0) = 1;
    }
}

// mask bits from i to j in a string
// precond j - i <= 32
inline static
uint32_t mask_bits(const byte_t *str, int i, int j)
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

// uint32_t mask_bucket_bits(byte_t *str, int i, int j)
// {
//     return mask_bits(str, j - WAGNER_BUCKET_BITS, j);
// }

// uint32_t mask_sort_bits(byte_t *str, int i, int j)
// {
//     return mask_bits(str, i, j - WAGNER_BUCKET_BITS);
// }

inline static
uint32_t mask_bucket_bits(const wagner_chunk_t *str)
{
    return (*str) >> WAGNER_SORT_BITS;
}

inline static
uint32_t mask_sort_bits(const wagner_chunk_t *str)
{
    return (*str) & ~(~0 << WAGNER_SORT_BITS);
}

// add a new pair to pair set
inline static
index_t append_pair(wagner_state_t *state, wagner_pair_set_t *set, index_t a, index_t b)
{
    index_t idx = set->size;

    if (idx < WAGNER_MAX_PAIR) {
        set->pairs[idx] = to_pair(a, b);
        set->size++;

        return idx;
    } else {
        return -1;
    }
}

inline static
void xor_chunks(const wagner_chunk_t *a,
                const wagner_chunk_t *b,
                wagner_chunk_t *dst, int size)
{
    int i;

    for (i = 0; i < size; i++) {
        dst[i] = a[i] ^ b[i];
    }
}

inline static
bool has_dup(wagner_pair_t a, wagner_pair_t b)
{
    // return false;
    index_t ai = pair_i(a),
            aj = pair_j(a),
            bi = pair_i(b),
            bj = pair_j(b);

    return ai == bi || aj == bj ||
           ai == bj || aj == bi;
}

// append a new pair to the pair set,
// then xor a, b in the current list and put the result to the next list
index_t collide(wagner_state_t *state, index_t ai, index_t bi)
{
    int stage = state->stage;

    wagner_pair_set_t *pair_set = pair_set_at_stage(state, stage);
    wagner_pair_set_t *prev_set;
    wagner_chunk_t *cur_list = list_at_stage(state, stage);
    wagner_chunk_t *next_list = list_at_stage(state, stage + 1);

    index_t idx;

    wagner_chunk_t
        *a = string_at(cur_list, stage, ai),
        *b = string_at(cur_list, stage, bi),
        *dst;

    // wagner_chunk_t c[WAGNER_CHUNK_AT_STAGE(stage)];

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

    if (idx != -1) {
        dst = string_at(next_list, stage + 1, idx);
        xor_chunks(a + 1, b + 1, dst, WAGNER_CHUNK_AT_STAGE(stage + 1));
    }

    return idx;
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

    wagner_chunk_t *cur_list = list_at_stage(state, stage);
    wagner_chunk_t *tmp;

    uint32_t tmp_idx, idx, cur_idx;

    int count;

    // for each string in the current stage
    // bits i -> k are the sort bits
    // bits k -> j are the bucket bits

    int m, n, start, end;

    printf("stage: %d, inputs: %d, chunks: %d\n", stage, nstr, WAGNER_CHUNK_AT_STAGE(stage));
    // printf("cur list: %p\n", cur_list);

    // init buckets
    for (m = 0; m < WAGNER_BUCKET; m++) {
        state->bucks[m]->size = 0;
    }

    // init pair set
    pair_set->size = 0;

    int a = 0;

    // linear scan to sort all strings to buckets
    for (m = 0; m < nstr; m++) {
        tmp = string_at(cur_list, stage, m);
        bucket_add(state, state->bucks[mask_bucket_bits(tmp)], m);
    }

    for (m = 0; m < WAGNER_BUCKET; m++) {
        if (pair_set->size >= WAGNER_MAX_PAIR) break;

        cur_buck = state->bucks[m];

        // init hash table
        bzero(hashtab, sizeof(*hashtab));

        // printf("bucket size: %d\n", cur_buck->size);

        for (n = 0; n < cur_buck->size; n++) {
            cur_idx = cur_buck->buck[n];

            tmp = string_at(cur_list, stage, cur_idx);
            entry = &hashtab->tab[mask_sort_bits(tmp)];

            switch (entry->count) {
                case 0:
                    entry->count++;
                    entry->where = cur_idx;
                    break;

                case 1:
                    idx = collide(state, entry->where, cur_idx);

                    if (idx != -1) {
                        // write result to the next string list
                        entry->count++;
                        entry->where = idx;
                    }

                    break;

                default:
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
                        PAIR_WITH(pair_i(pair_set->pairs[start]));
                    }

                    // add the last pair
                    PAIR_WITH(pair_j(pair_set->pairs[end]));

                    if (idx != -1) {
                        // printf("added: %d\n", count);
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
        sol[cur_size++] = pair_i(from);
        sol[cur_size++] = pair_j(from);
        new_size = cur_size;
    } else {
        // continue tracing
        prev = pair_set_at_stage(state, stage - 1);

        // traverse left child
        new_size = wagner_trace_solution(state, prev->pairs[pair_i(from)], stage - 1, cur_size, sol);

        // traverse right child
        new_size = wagner_trace_solution(state, prev->pairs[pair_j(from)], stage - 1, new_size, sol);
    }

    return new_size;
}

int wagner_finalize(wagner_state_t *state, index_t *sols, int max_sol)
{
    wagner_chunk_t *last_list = list_at_stage(state, FINAL_STAGE);
    wagner_chunk_t *tmp;

    int nstr = state->nstr;
    int i, found = 0;

    wagner_pair_set_t *prev_set = pair_set_at_stage(state, FINAL_STAGE - 1);

    // find any pair that yields zero
    for (i = 0; i < nstr; i++) {
        tmp = string_at(last_list, FINAL_STAGE, i);
        if (tmp[0] == 0 && found < max_sol) {
            wagner_trace_solution(state, prev_set->pairs[i], FINAL_STAGE - 1, 0,
                                  sols + found * WAGNER_SOLUTION);
            found++;
        }
    }
    
    printf("found: %d\n", found);

    return found;
}

// sols should have max_sol * WAGNER_SOLUTION elems
int wagner_solve(const byte_t *init_list, index_t *sols, int max_sol)
{
    wagner_hash_table_t hashtab;
    wagner_state_t state = {
        .ctx = NULL,
        .hashtab = &hashtab,
        .nstr = WAGNER_INIT_NSTR,
        .stage = 0
    };

    int i, j, found;
    size_t size = sizeof_ctx(&state);
    wagner_chunk_t *init_chunk, *tmp;
    const byte_t *str;

    printf("allocating %lu bytes\n", size);

    state.ctx = malloc(size);
    init_chunk = list_at_stage(&state, 0);

    // init chunks
    for (i = 0; i < WAGNER_INIT_NSTR; i++) {
        tmp = string_at(init_chunk, 0, i);
        str = init_list + WAGNER_N_BYTE * i;

        for (j = 0; j < WAGNER_TOTAL_CHUNK; j++) {
            tmp[j] = mask_bits(str, j * WAGNER_BITS, (j + 1) * WAGNER_BITS);
        }
    }
    
    printf("bucket size: %lu\n", sizeof_bucket(WAGNER_MAX_PAIR));

    for (i = 0; i < WAGNER_BUCKET; i++) {
        state.bucks[i] = malloc(sizeof_bucket(WAGNER_MAX_PAIR));
    }

    for (i = 0; i < WAGNER_TOTAL_STAGE; i++) {
        wagner_transform(&state);
    }

    found = wagner_finalize(&state, sols, max_sol);

    for (i = 0; i < WAGNER_BUCKET; i++) {
        free(state.bucks[i]);
    }

    free(state.ctx);

    return found;
}
