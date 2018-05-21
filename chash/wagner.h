#ifndef _WAGNER_H_
#define _WAGNER_H_

#include "common.h"

#define WAGNER_N 200
#define WAGNER_K 9
#define WAGNER_BITS (WAGNER_N / (WAGNER_K + 1))

typedef uint32_t index_t;

typedef struct {
    index_t i, j;
} wagner_pair_t;

// typedef struct wagner_tree_t_tag {
//     wagner_pair_t *pairs;
//     // set of pairs found in the current stage
//     // indices in this pair set is pointing to the result
//     // of pair operations from the previous set

//     struct wagner_tree_t_tag *prev;
// } wagner_tree_t;

typedef struct {
    index_t size;
    wagner_pair_t pair[];
} wagner_pair_set_t;

typedef struct {
    void *ctx;
    // byte_t *list;
    // wagner_tree_t *tree;

    int cur_nstr; // number of strings in the current list
    int init_nstr; // number of strings in the initial list
    int stage; // counting from 0
} wagner_state_t;

// typedef index_t *wagner_solution_t;

bool wagner_solve(const byte_t *init_list, int nstr, index_t *sol);

#endif
