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
    char dat[] = "hello, my name is rod. im just trying to make this string longer and longer and longer and longer and longer";
    index_t sols[20 * WAGNER_SOLUTION];
    
    do_test();
    equihash((byte_t *)dat, sizeof(dat), sols, 20);

    return 0;
}
