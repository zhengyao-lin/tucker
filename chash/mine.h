#ifndef _MINE_H_
#define _MINE_H_

#include <pthread.h>

#include "common.h"
#include "sha256.h"
#include "hash256.h"

typedef struct {
    struct miner_state_t_tag *state;
    nonce_t from;
    nonce_t to;
    int id;
} job_t;

#define STOP_FOUND 1
#define STOP_KILL 2

typedef struct miner_state_t_tag {
    ctx_sha256_t base_ctx;

    const byte_t *dat;
    hash256_t target;

    size_t rsize; // remaining part besides the nonce
    size_t nsize;
    size_t osize;

    nonce_t answer;

    pthread_t *threads;
    job_t *jobs;

    int njob;
    int stop;
} miner_state_t;

// append a 4-byte nonce to dat such that the hash of the appended string is
// lower than the target in little-endian
miner_state_t *init_miner(const byte_t *dat, size_t size, const hash256_t target, int njob);
nonce_t *join_miner(miner_state_t *state);
void free_miner(miner_state_t *state);
void kill_miner(miner_state_t *state);

#endif
