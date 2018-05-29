#ifndef _COMMON_H_
#define _COMMON_H_

#include <time.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <pthread.h>

typedef uint8_t byte_t;
typedef uint32_t word_t;
typedef byte_t hash256_t[32];
typedef uint32_t nonce_t;

typedef uint64_t size_t;

typedef uint32_t msec_t;

// align a number to 2^k
#define ALIGNK(k, i) (((i) + (1 << (k)) - 1) & (~((1 << (k)) - 1)))
#define ALIGN4(i) ALIGNK(2, i)
#define ALIGN8(i) ALIGNK(3, i)
#define ALIGN16(i) ALIGNK(4, i)

static inline msec_t
get_cpu_ms()
{
    struct timespec tp;

    clock_gettime(CLOCK_MONOTONIC, &tp);

    return (tp.tv_sec * 1000 + tp.tv_nsec / 1000000);
}

static inline void
print_byte(char b)
{
    char *tab = "0123456789abcdef";
    printf("%c%c", tab[(b >> 4) & 0xf], tab[b & 0xf]);
}

static inline void
print_hash256(const hash256_t hash)
{
    int i;
    
    for (i = 0; i < 32; i++)
        print_byte(hash[i]);

    printf("\n");
}

static inline int
hex2int(char h)
{
    if (h >= '0' && h <= '9') return h - '0';
    else return h - 'a' + 10;
}

static inline void
read_hash256(const char *str, hash256_t hash)
{
    int i, j;

    for (i = 0, j = 0; i < 32; i++, j += 2) {
        hash[i] = hex2int(str[j]) << 4 | hex2int(str[j + 1]);
    }
}

#endif
