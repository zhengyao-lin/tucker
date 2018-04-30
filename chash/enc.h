#ifndef _ENC_H_
#define _ENC_H_

#include "common.h"

typedef int8_t endian_t;

#define ENC_LITTLE_ENDIAN 1
#define ENC_BIG_ENDIAN 0

/*
    encoding:
        encoder for word8 to word64

    decoding:
        decoder for word8 to word64

 */

void fast_encode_word16(byte_t *raw, endian_t end, uint16_t i);
void fast_encode_word32(byte_t *raw, endian_t end, uint32_t i);
void fast_encode_word64(byte_t *raw, endian_t end, uint64_t i);

uint16_t fast_decode_word16(const byte_t *raw, endian_t end);
uint32_t fast_decode_word32(const byte_t *raw, endian_t end);
uint64_t fast_decode_word64(const byte_t *raw, endian_t end);

#endif
