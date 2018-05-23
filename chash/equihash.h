#ifndef _EQUIHASH_H_
#define _EQUIHASH_H_

#include "common.h"
#include "wagner.h"

int equihash(const byte_t *dat, size_t size, index_t *sols, int max_sol);

#endif
