#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#include "mine.h"
#include "sha256.h"
#include "hash256.h"

// hash comparison in little-endian(a <= b)
static inline bool
less_or_equal_to_hash_le(const hash256_t a, const hash256_t b)
{
    return hash256_compare(a, b) <= 0;
}

#define MAX_BOUND ((nonce_t)-1)

// a single worker
void *miner(void *arg)
{
    job_t job = *(job_t *)arg;

    byte_t *ndat = malloc(job.state->nsize),
           *remain = ndat + job.state->osize - job.state->rsize;

    nonce_t *hole = (nonce_t *)(ndat + job.state->osize),
            i, j = job.to, a, b;

    hash256_t hash0, hash;
    size_t proc;

    ctx_sha256_t ctx;

    msec_t begin, span0, span1;
    double span;

#define MEASURE_TIME (1 << 16)

    memcpy(ndat, job.state->dat, job.state->osize);

    printf("thread created, job [%u, %u]\n", job.from, job.to);

    span0 = begin = get_cpu_ms();

    for (i = job.from; i <= j && !job.state->found; i++) {
        if (i && i % MEASURE_TIME == 0) {
            span1 = get_cpu_ms();
            span = (double)(span1 - span0) / 1000;

            a = i - job.from;
            b = j - job.from;

            printf("\rprogress(%d): %u/%u(%.2f%%) global rate: %.1f H/s",
                   job.id, a, b, (double)a / b * 100,
                   MEASURE_TIME / span * job.state->njob);

            span0 = span1;
        }

        *hole = i;
        ctx = job.state->base_ctx; // copy context
        
        proc = sha256_update(&ctx, remain, job.state->rsize); // further update

        // finalize the first round
        sha256_finalize(&ctx, remain + proc,
                        job.state->rsize - proc + sizeof(nonce_t),
                        job.state->nsize);
        
        write_ctx(&ctx, hash0);
        sha256(hash0, sizeof(hash0), hash); // double sha256

        // double_sha256(ndat, job.state->nsize, hash);

        if (less_or_equal_to_hash_le(hash, job.state->target)) {
            printf("\nfound after %.1f seconds\n", (double)(get_cpu_ms() - begin) / 1000);
            // print_hash256(hash);
            // print_hash256(job.state->target);

            free(ndat);
            
            job.state->answer = i;
            job.state->found = true;

            return NULL;
        }
    }

    free(ndat);
    return NULL;
}

// append a nonce to dat until the double sha256 of it
// is smaller(in little-endian) than the target
miner_state_t *init_miner(const byte_t *dat, size_t size, const hash256_t target, int njob)
{
    size_t nsize = size + 4;

    size_t rsize = size % CHUNK_SIZE,
           preproc = size - rsize;

    nonce_t answer = MAX_BOUND;

    // jobs/state
    miner_state_t *state = malloc(sizeof(*state));

    nonce_t intv = MAX_BOUND / njob;
    int i;

    *state = (miner_state_t) {
        .dat = dat,
        .target = {0},
        .rsize = rsize,
        .nsize = nsize,
        .osize = size,
        .base_ctx = INIT_CTX,
        .answer = -1,
        .threads = malloc(sizeof(*state->threads) * njob),
        .jobs = malloc(sizeof(*state->jobs) * njob),
        .njob = njob,
        .found = false
    };

    memcpy(state->target, target, sizeof(hash256_t));

    // preprocess some part of the data(that does not contain nonce)
    sha256_update(&state->base_ctx, dat, size - rsize);

    for (i = 0; i < njob; i++) {
        state->jobs[i] = (job_t) {
            .state = state,
            .from = i * intv,
            .to = i + 1 == njob ? MAX_BOUND : i * intv + intv - 1,
            .id = i
        };

        if (pthread_create(state->threads + i, NULL, miner, state->jobs + i))
            fprintf(stderr, "failed to create thread\n");
    }

    return state;
}

nonce_t *join_miner(miner_state_t *state)
{
    int i;

    for (i = 0; i < state->njob; i++) {
        pthread_join(state->threads[i], NULL);
    }

    if (state->found) {
        printf("found: %u\n", state->answer);
        return &state->answer;
    } else {
        printf("not found\n");
        return NULL;
    }
}

void free_miner(miner_state_t *state)
{
    free(state->threads);
    free(state);
}

void kill_miner(miner_state_t *state)
{
    state->found = true;
}
