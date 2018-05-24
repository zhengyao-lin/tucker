#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mine.h"
#include "test.h"
#include "sha256.h"
#include "common.h"
#include "equihash.h"

#include "blake/blake2.h"

int main()
{
    char dat[] = "hello, pop pop pop pop pop pop 12300 1233 dsfasaf";
    index_t sols[20 * WAGNER_SOLUTION];

    int found;
    
    do_test();
    found = equihash((byte_t *)dat, sizeof(dat), sols, 20);

    return 0;
}
