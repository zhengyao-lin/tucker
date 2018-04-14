#ifndef _COMMON_H_
#define _COMMON_H_

#include <time.h>
#include <stdint.h>
#include <stdbool.h>
#include <pthread.h>

typedef unsigned char byte_t;
typedef uint32_t word_t;
typedef byte_t hash256_t[32];
typedef uint32_t nonce_t;

typedef uint32_t msec_t;

static inline msec_t
get_cpu_ms()
{
    struct timespec tp;

    clock_gettime(CLOCK_MONOTONIC, &tp);

    return (tp.tv_sec * 1000 + tp.tv_nsec / 1000000);
}

#endif
