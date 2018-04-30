#include "enc.h"

static inline bool
is_le()
{
    return ((union { int x; byte_t c; }){1}).c;
}

static inline bool
is_cpu_end(endian_t end)
{
    // ENC_LITTLE_ENDIAN == 1
    // ENC_BIG_ENDIAN == 0
    return is_le() == end;
}

void fast_encode_word16(byte_t *raw, endian_t end, uint16_t i)
{
    if (is_cpu_end(end)) *(uint16_t *)raw = i;
    else {
        union {
            byte_t b[2];
            uint16_t i;
        } u;

        u.i = i;

        raw[0] = u.b[1];
        raw[1] = u.b[0];
    }
}

void fast_encode_word32(byte_t *raw, endian_t end, uint32_t i)
{
    if (is_cpu_end(end)) *(uint32_t *)raw = i;
    else {
        union {
            byte_t b[4];
            uint32_t i;
        } u;
    
        u.i = i;

        raw[0] = u.b[3];
        raw[1] = u.b[2];
        raw[2] = u.b[1];
        raw[3] = u.b[0];
    }
}

void fast_encode_word64(byte_t *raw, endian_t end, uint64_t i)
{
    if (is_cpu_end(end)) *(uint64_t *)raw = i;
    else {
        union {
            byte_t b[8];
            uint64_t i;
        } u;

        u.i = i;

        raw[0] = u.b[7];
        raw[1] = u.b[6];
        raw[2] = u.b[5];
        raw[3] = u.b[4];
        raw[4] = u.b[3];
        raw[5] = u.b[2];
        raw[6] = u.b[1];
        raw[7] = u.b[0];
    }
}

uint16_t fast_decode_word16(const byte_t *raw, endian_t end)
{
    if (is_cpu_end(end)) return *(uint16_t *)raw;
    else {
        union {
            byte_t b[2];
            uint16_t i;
        } u;

        u.b[0] = raw[1];
        u.b[1] = raw[0];

        return u.i;
    }
}

uint32_t fast_decode_word32(const byte_t *raw, endian_t end)
{
    if (is_cpu_end(end)) return *(uint32_t *)raw;
    else {
        union {
            byte_t b[4];
            uint32_t i;
        } u;

        u.b[0] = raw[3];
        u.b[1] = raw[2];
        u.b[2] = raw[1];
        u.b[3] = raw[0];

        return u.i;
    }
}

uint64_t fast_decode_word64(const byte_t *raw, endian_t end)
{
    if (is_cpu_end(end)) return *(uint64_t *)raw;
    else {
        union {
            byte_t b[8];
            uint64_t i;
        } u;

        u.b[0] = raw[7];
        u.b[1] = raw[6];
        u.b[2] = raw[5];
        u.b[3] = raw[4];
        u.b[4] = raw[3];
        u.b[5] = raw[2];
        u.b[6] = raw[1];
        u.b[7] = raw[0];

        return u.i;
    }
}
