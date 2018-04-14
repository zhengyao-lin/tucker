#ifndef _MINE_H_
#define _MINE_H_

#include "common.h"

// append a 4-byte nonce to dat such that the hash of the appended string is
// lower than the target in little-endian
nonce_t do_mine(const byte_t *dat, size_t size, const hash256_t target, int njob);

#endif
