#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "wagner.h"

// #define FINAL_STAGE (WAGNER_TOTAL_STAGE - 1) // the last collection stage
#define FINAL_STAGE (WAGNER_TOTAL_STAGE - 1)

#define ONES(n) (~(~(wagner_chunk_t)0 << (n)))

#define MAX(a, b) ((a) > (b) ? (a) : (b))

inline static
wagner_pair_set_t *
pair_set_at_stage(void *ctx, int stage)
{
    return (wagner_pair_set_t *)((byte_t *)ctx + WAGNER_MEM_UNIT * stage);
}

inline static
byte_t *list_at_stage(void *ctx, int stage)
{
    byte_t *base = (byte_t *)ctx + WAGNER_MEM_UNIT * (stage + 2);

    if (stage & 1) {
        // return base + MAX(WAGNER_MAX_PAIR * WAGNER_LEN_AT_STAGE(stage - 1);
        return base + MAX(WAGNER_MAX_PAIR * WAGNER_ALIGNED_LEN_AT_STAGE(stage - 1),
                          WAGNER_MEM_UNIT + WAGNER_MAX_PAIR * WAGNER_ALIGNED_LEN_AT_STAGE(stage + 1));
    } else
        return base;
}

inline static
size_t sizeof_ctx()
{
    size_t max = 0, tmp;
    int i;

    for (i = 0; i < WAGNER_TOTAL_STAGE; i++) {
        tmp = (size_t)(list_at_stage(NULL, i) + WAGNER_MAX_PAIR * WAGNER_ALIGNED_LEN_AT_STAGE(i));
    
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

// mask bits at range [i, j) in a string
// precond j - i <= chunk size

inline static
wagner_chunk_t mask_bits(byte_t *str, int i, int j)
{
    int ei = i % 8,
        ej = j % 8;

    int k, base = 0;
    int m = k = i / 8,
        n = (j + 7) / 8 - 1;

    wagner_chunk_t ret = 0;

    for (; m <= n; m++) {
        if (m == k) {
            // first byte
            if (m == n) {
                ret |= (str[m] >> ei) & ONES(j - i);
            } else {
                ret |= str[m] >> ei;
                base += 8 - ei;
            }
        } else if (m == n && ej) {
            // last byte
            ret |= ((wagner_chunk_t)str[m] & ONES(ej)) << base;
        } else {
            ret |= (wagner_chunk_t)str[m] << base;
            base += 8;
        }
    }

    return ret;
}

// specialized for BITS == 20, even stage
inline static
wagner_chunk_t mask_bits_even(byte_t *str)
{
    return
        ((wagner_chunk_t)str[0]) |
        ((wagner_chunk_t)str[1] << 8) |
        (((wagner_chunk_t)str[2] & ONES(4)) << 16);
}

// specialized for BITS == 20, odd stage
inline static
wagner_chunk_t mask_bits_odd(byte_t *str)
{
    return
        ((wagner_chunk_t)str[0] >> 4) |
        ((wagner_chunk_t)str[1] << 4) |
        ((wagner_chunk_t)str[2] << 12);
}

inline static
wagner_chunk_t mask_bucket_bits(wagner_chunk_t head)
{
    return head >> WAGNER_COLLISION_BITS;
}

inline static
wagner_chunk_t mask_collision_bits(wagner_chunk_t head)
{
    return head & ONES(WAGNER_COLLISION_BITS);
}

inline static
byte_t *string_at(byte_t *list, int stage, int i)
{
    return list + i * WAGNER_ALIGNED_LEN_AT_STAGE(stage);
}

#if WAGNER_BITS == 20
    inline static
    wagner_chunk_t head_at(byte_t *list, int stage, int i)
    {
        if (stage & 1) {
            return mask_bits_odd(string_at(list, stage, i));
        } else {
            return mask_bits_even(string_at(list, stage, i));
        }
    }
#else
    inline static
    wagner_chunk_t head_at(byte_t *list, int stage, int i)
    {
        int ofs = WAGNER_OFS_AT_STAGE(stage);
        return mask_bits(string_at(list, stage, i), ofs, ofs + WAGNER_BITS);
    }
#endif

inline static
void xor_string(const byte_t *a,
                const byte_t *b,
                byte_t *dst, int size)
{
    int i;

    for (i = 0; i < size; i += 4) {
        ((uint32_t *)dst)[i / 4] = ((uint32_t *)a)[i / 4] ^
                                   ((uint32_t *)b)[i / 4];
    }

    i -= 4;

    switch (size - i) {
        case 3: dst[i] = a[i] ^ b[i]; i++;
        case 2: dst[i] = a[i] ^ b[i]; i++;
        case 1: dst[i] = a[i] ^ b[i]; i++;
    }
}

inline static
bool has_dup(wagner_pair_t a, wagner_pair_t b)
{
    index_t ai = pair_i(a),
            aj = pair_j(a),
            bi = pair_i(b),
            bj = pair_j(b);
            
    return ai == bi || aj == bj ||
           ai == bj || aj == bi;
}

// check if the collision is valid
inline static
bool check_collide(int stage, wagner_pair_t *prev_pair_list, index_t ai, index_t bi)
{
    // check duplication
    return !stage || !has_dup(prev_pair_list[ai], prev_pair_list[bi]);
}

int wagner_trace_solution(void *ctx, wagner_pair_t from,
                          int stage, int cur_size, index_t *sol,
                          bool *used)
{
    wagner_pair_set_t *prev;

    wagner_pair_t p1, p2;
    index_t i = pair_i(from),
            j = pair_j(from);

    int new_size;

    if (cur_size == -1) return -1;

    if (stage == 0) {
        // have reached the leaves, write the actual index
        if (used[i] || used[j]) return -1;

        used[i] = true;
        used[j] = true;

        sol[cur_size++] = i;
        sol[cur_size++] = j;
        
        return cur_size;
    } else {
        // continue tracing
        prev = pair_set_at_stage(ctx, stage - 1);

        p1 = prev->pairs[i];
        p2 = prev->pairs[j];
        
        new_size = wagner_trace_solution(ctx, p1, stage - 1, cur_size, sol, used);
        new_size = wagner_trace_solution(ctx, p2, stage - 1, new_size, sol, used);
    
        return new_size;
    }
}

// we can put the first chunk of each string at the front
// and put the rest data at the end
inline static
int wagner_collide(wagner_state_t *state, int stage)
{
    void *ctx = state->ctx;
    // int stage = state->stage;
    int nstr = state->nstr;

    wagner_pair_set_t *pair_set = pair_set_at_stage(ctx, stage);

    wagner_bucket_t *bucks = state->bucks;
    wagner_bucket_item_t *cur_buck;
    wagner_entry_t *tab = state->hashtab->tab;
    wagner_entry_t *entry;

    byte_t *cur_list = list_at_stage(ctx, stage);
    byte_t *next_list = list_at_stage(ctx, stage + 1);
    wagner_chunk_t chunk, buck_bits, col_bits;

    wagner_pair_t *prev_pair_list =
        stage ? pair_set_at_stage(ctx, stage - 1)->pairs : NULL;

    wagner_pair_t *pair_list = pair_set->pairs;
    index_t pair_next = 0;
    
    index_t idx, cur_idx, m, n, i, j;

    index_t bucket_size[WAGNER_BUCKET];

    int count, diff;

    // for each string in the current stage
    // bits i -> k are the sort bits
    // bits k -> j are the bucket bits

    printf("stage: %d, inputs: %d, ofs: %d, len: %d, aligned len: %d\n",
           stage, nstr, WAGNER_OFS_AT_STAGE(stage),
           WAGNER_LEN_AT_STAGE(stage),
           WAGNER_ALIGNED_LEN_AT_STAGE(stage));
    // printf("cur list: %p\n", cur_list);

    // init buckets
    bzero(bucket_size, sizeof(bucket_size));

    // linear scan to sort all strings to buckets
    for (m = 0; m < nstr; m++) {
        chunk = head_at(cur_list, stage, m);

        buck_bits = mask_bucket_bits(chunk);

        if (bucket_size[buck_bits] != WAGNER_BUCKET_ELEM) {
            bucks[buck_bits].buck[bucket_size[buck_bits]++] =
                (wagner_bucket_item_t) {
                    m, mask_collision_bits(chunk)
                };
        }
    }

    for (m = 0; m < WAGNER_BUCKET; m++) {
        cur_buck = bucks[m].buck;

        // init hash table
        bzero(tab, sizeof(wagner_hash_table_t));

        // printf("bucket size: %d %d\n", bucket_size[m], pair_next);

        for (n = 0; n < bucket_size[m]; n++) {
            cur_idx = cur_buck[n].idx;

            entry = tab + cur_buck[n].head;

            switch (entry->count) {
                case 0:
                    entry->count++;
                    entry->where = cur_idx;
                    break;

                case 1:
                    if (pair_next != WAGNER_MAX_PAIR) {
                        if (check_collide(stage, prev_pair_list, entry->where, cur_idx)) {
                            // if (head_at(cur_list, stage, entry->where) != head_at(cur_list, stage, cur_idx))
                            //     printf("impossible\n");

                            pair_list[pair_next] = to_pair(entry->where, cur_idx);
                            entry->count++;
                            entry->where = pair_next;
                            pair_next++;
                        }
                    } else goto L_END;

                    break;

                default:
                    // add multiple pairs
                
                    // we have, from entry->where, entry->count - 1 pairs
                    // (a, k), (b, k), (c, k) ... (a + count - 1, k)
                    // we pair (a, b, c .. a + count - 1, k) each with the current index(cur_buck->buck[n])

                    i = entry->where;
                    j = i + entry->count - 2;

                    count = 0;
                    idx = -1;

                    #define PAIR_WITH(a) \
                        if (pair_next != WAGNER_MAX_PAIR) { \
                            if (check_collide(stage, prev_pair_list, (a), cur_idx)) { \
                                pair_list[pair_next] = to_pair((a), cur_idx); \
                                if (idx == -1) idx = pair_next; \
                                count++; \
                                pair_next++; \
                            } \
                        } else goto L_END;

                    // iterate though each previous pair
                    for (; i <= j; i++) {
                        PAIR_WITH(pair_i(pair_list[i]));
                    }

                    // add the last pair
                    PAIR_WITH(pair_j(pair_list[j]));

                    if (idx != -1) {
                        entry->count = count + 1;
                        entry->where = idx;
                    }
            }
        }
    }

L_END:

    diff = WAGNER_LEN_AT_STAGE(stage) - WAGNER_LEN_AT_STAGE(stage + 1);

    if (stage != FINAL_STAGE) {
        for (m = 0; m < pair_next; m++) {
            i = pair_i(pair_list[m]);
            j = pair_j(pair_list[m]);

            xor_string(string_at(cur_list, stage, i) + diff,
                       string_at(cur_list, stage, j) + diff,
                       string_at(next_list, stage + 1, m),
                       WAGNER_LEN_AT_STAGE(stage + 1));
        }

        // prepare for the next state
        state->nstr = pair_next; // pair_set->size;

        return -1;
    } else {
        // finalize solutions

        int found = 0;
        bool *used = malloc(WAGNER_INIT_NSTR * sizeof(*used));

        for (m = 0; m < pair_next; m++) {
            i = pair_i(pair_list[m]);
            j = pair_j(pair_list[m]);

            if (memcmp(string_at(cur_list, stage, i) + diff,
                       string_at(cur_list, stage, j) + diff,
                       WAGNER_LEN_AT_STAGE(stage + 1)) == 0) {

                if (found < state->max_sol) {
                    bzero(used, WAGNER_INIT_NSTR * sizeof(*used));
                    if (wagner_trace_solution(ctx, pair_list[m], FINAL_STAGE, 0,
                                              state->sols + found * WAGNER_SOLUTION, used) != -1) {
                        found++;
                    }
                } else break;
            }
        }

        free(used);

        return found;
    }
}

// sols should have max_sol * WAGNER_SOLUTION elems
int wagner_solve(const byte_t *init_list, index_t *sols, int max_sol)
{
    wagner_hash_table_t hashtab;
    wagner_state_t state = {
        .ctx = NULL,
        .hashtab = &hashtab,
        .sols = sols,
        .max_sol = max_sol,
        .nstr = WAGNER_INIT_NSTR
    };

    int i, found;
    size_t size = sizeof_ctx();
    // wagner_chunk_t *init_chunk, *tmp_head, *tmp_tail;
    // const byte_t *str;

    printf("allocating %lu bytes\n", size);

    state.ctx = malloc(size);
    
#if WAGNER_INIT_ALIGNED
    memcpy(list_at_stage(state.ctx, 0), init_list, WAGNER_INIT_NSTR * WAGNER_LEN_AT_STAGE(0));
#else
    // copy the stirngs one by one to ensure the alignment

    byte_t *list = list_at_stage(state.ctx, 0);

    for (i = 0; i < WAGNER_INIT_NSTR; i++) {
        memcpy(string_at(list, 0, i), init_list + i * WAGNER_LEN_AT_STAGE(0), WAGNER_LEN_AT_STAGE(0));
    }
#endif

    printf("bucket size: %lu\n", WAGNER_BUCKET_SIZE);

    state.bucks = malloc(WAGNER_BUCKET * WAGNER_BUCKET_SIZE);

#if WAGNER_TOTAL_STAGE == 9
    wagner_collide(&state, 0);
    wagner_collide(&state, 1);
    wagner_collide(&state, 2);
    wagner_collide(&state, 3);
    wagner_collide(&state, 4);
    wagner_collide(&state, 5);
    wagner_collide(&state, 6);
    wagner_collide(&state, 7);
    found = wagner_collide(&state, 8);
#else
    for (i = 0; i < WAGNER_TOTAL_STAGE; i++) {
        found = wagner_collide(&state, i);
    }
#endif

    // found = wagner_finalize(&state, sols, max_sol);

    printf("found: %d\n", found);

    free(state.bucks);
    free(state.ctx);

    return found;
}

/*

allocating 209715200 bytes
bucket size: 81920
stage: 0, inputs: 2097152, chunks: 10
stage: 1, inputs: 2097152, chunks: 9
stage: 2, inputs: 2096607, chunks: 8
stage: 3, inputs: 2097152, chunks: 7
stage: 4, inputs: 2097152, chunks: 6
stage: 5, inputs: 2097152, chunks: 5
stage: 6, inputs: 2097152, chunks: 4
stage: 7, inputs: 2096375, chunks: 3
stage: 8, inputs: 2093012, chunks: 2
found: 0
searching finished after 2047ms

*/
