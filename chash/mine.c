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

typedef struct {
    const byte_t *dat;
    hash256_t target;

    size_t rsize; // remaining part besides the nonce
    size_t nsize;
    size_t osize;
    ctx_sha256_t *base_ctx;

    nonce_t *answer;
    int njob;
    bool stop;
} common_env_t;

typedef struct {
    common_env_t *env;
    nonce_t from;
    nonce_t to;
    int id;
} job_t;

#define MAX_BOUND ((nonce_t)-1)

// a single worker
void *miner(void *arg)
{
    job_t job = *(job_t *)arg;

    byte_t *ndat = malloc(job.env->nsize),
           *remain = ndat + job.env->osize - job.env->rsize;

    nonce_t *hole = (nonce_t *)(ndat + job.env->osize), i, j = job.to - job.from;

    hash256_t hash0, hash;
    size_t proc;

    ctx_sha256_t ctx;

    msec_t begin, span0, span1;
    double span;

#define MEASURE_TIME 40960

    memcpy(ndat, job.env->dat, job.env->osize);

    printf("thread created, job [%u, %u]\n", job.from, job.to);

    span0 = begin = get_cpu_ms();

    for (i = 0; i <= j && !job.env->stop; i++) {
        if (i && i % MEASURE_TIME == 0) {
            span1 = get_cpu_ms();
            span = (double)(span1 - span0) / 1000;

            printf("\rprogress(%d): %u/%u(%.2f%%) global rate: %.1f H/s",
                   job.id, i, j, (double)i / j * 100,
                   MEASURE_TIME / span * job.env->njob);

            span0 = span1;
        }

        *hole = i + job.from;

        ctx = *job.env->base_ctx; // copy context
        
        proc = sha256_update(&ctx, remain, job.env->rsize); // further update

        // finalize the first round
        sha256_finalize(&ctx, remain + proc,
                        job.env->rsize - proc + sizeof(nonce_t),
                        job.env->nsize);
        
        write_ctx(&ctx, hash0);
        sha256(hash0, sizeof(hash0), hash); // double sha256

        // double_sha256(ndat, job.env->nsize, hash);

        if (less_or_equal_to_hash_le(hash, job.env->target)) {
            printf("\nfound after %.1f seconds\n", (double)(get_cpu_ms() - begin) / 1000);
            // print_hash256(hash);
            // print_hash256(job.env->target);

            free(ndat);
            
            *job.env->answer = i + job.from;
            job.env->stop = true;

            return NULL;
        }
    }

    free(ndat);
    return NULL;
}

// append a nonce to dat until the double sha256 of it
// is smaller(in little-endian) than the target
nonce_t do_mine(const byte_t *dat, size_t size, const hash256_t target, int njob)
{
    size_t nsize = size + 4;

    size_t rsize = size % CHUNK_SIZE,
           preproc = size - rsize;

    ctx_sha256_t ctx = INIT_CTX;

    nonce_t answer = MAX_BOUND;

    // jobs/env
    common_env_t env = {
        .dat = dat,
        .target = {0},
        .rsize = rsize,
        .nsize = nsize,
        .osize = size,
        .base_ctx = &ctx,
        .answer = &answer,
        .njob = njob,
        .stop = false
    };
    
    pthread_t threads[njob];
    job_t jobs[njob];

    nonce_t intv = MAX_BOUND / njob;
    int i;

    memcpy(env.target, target, sizeof(hash256_t));

    // preprocess some part of the data(that does not contain nonce)
    sha256_update(&ctx, dat, size - rsize);

    for (i = 0; i < njob; i++) {
        jobs[i] = (job_t) {
            .env = &env,
            .from = i * intv,
            .to = i + 1 == njob ? MAX_BOUND : i * intv + intv - 1,
            .id = i
        };

        if (pthread_create(threads + i, NULL, miner, jobs + i))
            fprintf(stderr, "failed to create thread\n");
    }

    for (i = 0; i < njob; i++) {
        pthread_join(threads[i], NULL);
    }

    printf("found: %u\n", answer);

    return answer;
}
