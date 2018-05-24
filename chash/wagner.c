#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "wagner.h"

// #define FINAL_STAGE (WAGNER_TOTAL_STAGE - 1) // the last collection stage
#define FINAL_STAGE WAGNER_TOTAL_STAGE

#define ONES(n) (~(~(wagner_chunk_t)0 << (n)))

inline static
wagner_pair_set_t *
pair_set_at_stage(void *ctx, int stage)
{
    return (wagner_pair_set_t *)((byte_t *)ctx + WAGNER_MEM_UNIT * stage);
}

inline static
wagner_chunk_t *list_at_stage(void *ctx, int stage)
{
    wagner_chunk_t *base =
        (wagner_chunk_t *)((byte_t *)ctx + WAGNER_MEM_UNIT * (stage + 2));

    if (stage & 1) {
        return base + WAGNER_MAX_PAIR * WAGNER_CHUNK_AT_STAGE(stage - 1);
    } else
        return base;
}

// inline static
// wagner_chunk_t *string_at(wagner_chunk_t *list, int stage, int i)
// {
//     return list + WAGNER_CHUNK_AT_STAGE(stage) * i;
// }

inline static
wagner_chunk_t *head_at(wagner_chunk_t *list, int i)
{
    return list + i;
}

inline static
wagner_chunk_t *tail_at(wagner_chunk_t *list, int stage, int i)
{
                /* all head chunks */
    return list + WAGNER_MAX_PAIR + (WAGNER_CHUNK_AT_STAGE(stage) - 1) * i;
}

inline static
size_t sizeof_ctx()
{
    size_t max = 0, tmp;
    int i;

    for (i = 0; i < WAGNER_TOTAL_STAGE; i++) {
        tmp = (size_t)(list_at_stage(NULL, i) +
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

// mask bits at range [i, j) in a string
// precond j - i <= chunk size
inline static
wagner_chunk_t mask_bits(const byte_t *str, int i, int j)
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
    index_t ai = pair_i(a),
            aj = pair_j(a),
            bi = pair_i(b),
            bj = pair_j(b);
            
    return ai == bi || aj == bj ||
           ai == bj || aj == bi;
}

// append a new pair to the pair set,
// then xor a, b in the current list and put the result to the next list
inline static
bool collide_pair(void *ctx, int stage,
                  wagner_pair_t *prev_pair_list,
                  index_t pair_next, index_t ai, index_t bi)
{
    wagner_chunk_t *cur_list = list_at_stage(ctx, stage);
    wagner_chunk_t *next_list = list_at_stage(ctx, stage + 1);

    wagner_chunk_t
        *a_tail = tail_at(cur_list, stage, ai),
        *b_tail = tail_at(cur_list, stage, bi),
        *dst_head,
        *dst_tail;

    // check duplication
    if (stage != 0 &&
        has_dup(prev_pair_list[ai], prev_pair_list[bi])) {
        return false;
    }

    dst_head = head_at(next_list, pair_next);
    dst_tail = tail_at(next_list, stage + 1, pair_next);

    *dst_head = *a_tail ^ *b_tail; // process the head separately
    xor_chunks(a_tail + 1, b_tail + 1, dst_tail, WAGNER_CHUNK_AT_STAGE(stage + 1) - 1);

    return true;
}

// we can put the first chunk of each string at the front
// and put the rest data at the end
void wagner_collide(wagner_state_t *state)
{
    void *ctx = state->ctx;
    int stage = state->stage;
    int nstr = state->nstr;

    wagner_pair_set_t *pair_set = pair_set_at_stage(ctx, stage);

    wagner_bucket_t *bucks = state->bucks;
    wagner_bucket_t *cur_buck;
    wagner_hash_table_t *hashtab = state->hashtab;
    wagner_entry_t *entry;

    wagner_chunk_t *cur_list = list_at_stage(ctx, stage);
    wagner_chunk_t chunk, buck_bits, col_bits;

    wagner_pair_t *prev_pair_list =
        stage ? pair_set_at_stage(ctx, stage - 1)->pairs : NULL;

    wagner_pair_t *pair_list = pair_set->pairs;
    index_t pair_next = 0;
    
    index_t idx, cur_idx;

    index_t bucket_size[WAGNER_BUCKET];

    int count;

    // for each string in the current stage
    // bits i -> k are the sort bits
    // bits k -> j are the bucket bits

    int m, n, start, end;

    printf("stage: %d, inputs: %d, chunks: %d\n", stage, nstr, WAGNER_CHUNK_AT_STAGE(stage));
    // printf("cur list: %p\n", cur_list);

    // init buckets
    bzero(bucket_size, sizeof(bucket_size));

    // linear scan to sort all strings to buckets
    for (m = 0; m < nstr; m++) {
        chunk = *head_at(cur_list, m);
        buck_bits = mask_bucket_bits(chunk);

        if (bucket_size[buck_bits] != WAGNER_BUCKET_ELEM) {
            bucks[buck_bits].buck[bucket_size[buck_bits]++] =
                (wagner_bucket_item_t) {
                    m, mask_collision_bits(chunk)
                };
        }
    }

    for (m = 0; m < WAGNER_BUCKET; m++) {
        cur_buck = &bucks[m];

        // init hash table
        bzero(hashtab, sizeof(*hashtab));

        // printf("bucket size: %d %d\n", bucket_size[m], pair_next);

        for (n = 0; n < bucket_size[m]; n++) {
            cur_idx = cur_buck->buck[n].idx;

            entry = &hashtab->tab[cur_buck->buck[n].head];

            switch (entry->count) {
                case 0:
                    entry->count++;
                    entry->where = cur_idx;
                    break;

                case 1:
                    if (pair_next != WAGNER_MAX_PAIR) {
                        if (collide_pair(ctx, stage, prev_pair_list,
                                         pair_next, entry->where, cur_idx)) {
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

                    start = entry->where;
                    end = start + entry->count - 2;

                    count = 0;
                    idx = -1;

                    #define PAIR_WITH(a) \
                        if (pair_next != WAGNER_MAX_PAIR) { \
                            if (collide_pair(ctx, stage, prev_pair_list, \
                                             pair_next, (a), cur_idx)) { \
                                pair_list[pair_next] = to_pair((a), cur_idx); \
                                if (idx == -1) idx = pair_next; \
                                count++; \
                                pair_next++; \
                            } \
                        } else goto L_END;

                    // iterate though each previous pair
                    for (; start <= end; start++) {
                        PAIR_WITH(pair_i(pair_list[start]));
                    }

                    // add the last pair
                    PAIR_WITH(pair_j(pair_list[end]));

                    if (idx != -1) {
                        // printf("added: %d\n", count);
                        entry->count = count + 1;
                        entry->where = idx;
                    }
            }
        }
    }

L_END:

    // prepare for the next state
    state->nstr = pair_next; // pair_set->size;
    state->stage++;
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

int wagner_finalize(wagner_state_t *state, index_t *sols, int max_sol)
{
    bool *used = malloc(WAGNER_INIT_NSTR * sizeof(*used));
    void *ctx = state->ctx;

    wagner_chunk_t *last_list = list_at_stage(ctx, FINAL_STAGE);

    int nstr = state->nstr;
    int i, found = 0;

    wagner_pair_set_t *prev_set = pair_set_at_stage(ctx, FINAL_STAGE - 1);

    // find any pair that yields zero
    for (i = 0; i < nstr; i++) {
        if (*head_at(last_list, i) == 0 &&
            found < max_sol) {
            bzero(used, WAGNER_INIT_NSTR * sizeof(*used));

            if (wagner_trace_solution(ctx, prev_set->pairs[i], FINAL_STAGE - 1, 0,
                                      sols + found * WAGNER_SOLUTION, used) != -1) {
                found++;
            }
        }
    }
    
    free(used);

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
    size_t size = sizeof_ctx();
    wagner_chunk_t *init_chunk, *tmp_head, *tmp_tail;
    const byte_t *str;

    printf("allocating %lu bytes\n", size);

    state.ctx = malloc(size);
    init_chunk = list_at_stage(state.ctx, 0);

    // init chunks
    for (i = 0; i < WAGNER_INIT_NSTR; i++) {
        tmp_head = head_at(init_chunk, i);
        tmp_tail = tail_at(init_chunk, 0, i);

        str = init_list + WAGNER_N_BYTE * i;

        *tmp_head = mask_bits(str, 0, WAGNER_BITS);

        for (j = 0; j < WAGNER_TOTAL_CHUNK - 1; j++) {
            tmp_tail[j] = mask_bits(str, (j + 1) * WAGNER_BITS, (j + 2) * WAGNER_BITS);
        }
    }
    
    printf("bucket size: %lu\n", WAGNER_BUCKET_SIZE);

    state.bucks = malloc(WAGNER_BUCKET * WAGNER_BUCKET_SIZE);

    for (i = 0; i < WAGNER_TOTAL_STAGE; i++) {
        wagner_collide(&state);
    }

    found = wagner_finalize(&state, sols, max_sol);

    free(state.bucks);
    free(state.ctx);

    return found;
}
