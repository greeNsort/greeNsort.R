/*
# greeNsort DIET CP partition B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_DIET_partB_h
#define ALREADY_DEFINED_DIET_partB_h

#include "DIET_count.h"

// ascending partitioning of data between l and r around pivot
// returns 1 if partitioning is done (due to all ties)
// and does not copy data in the done case
static IndexT DIET_CP_partitionB_TieRight(
    ValueT *dat   // pointer to memory with current source data now
  , ValueT *buf   // pointer to memory for buffer (receives target data)
  , IndexT l      // leftmost position to be sorted
  , IndexT r      // rightmost position to be sorted
  , ValueT v      // pivot value
  , IndexT *c     // pointer to K non-initialized counters
)
{
  IndexT i, done = DIET_count_TieRight(dat, l, r, v, c);
  if (!done){
    bool b;
    // initialize positions in buffer part
    IndexT d[2];
    d[0] = l;
    d[1] = l + c[0];
    // partition
    for (i=l; i<=r; i++){
      b = GE(dat[i], v);
      buf[d[b]++] = dat[i];
    }
  }
  return(done);
}

static IndexT DIET_CP_partitionB_TieLeft(
    ValueT *dat       // pointer to memory with current source data now
  , ValueT *buf       // pointer to memory for buffer (receives target data)
  , IndexT l      // leftmost position to be sorted
  , IndexT r      // rightmost position to be sorted
  , ValueT v     // pivot value
  , IndexT *c     // pointer to K non-initialized counters
)
{
  IndexT i, done = DIET_count_TieLeft(dat, l, r, v, c);
  if (!done){
    bool b;
    // initialize positions in buffer part
    IndexT d[2];
    d[0] = l;
    d[1] = l + c[0];
    // partition
    for (i=l; i<=r; i++){
      b = GT(dat[i], v);
      buf[d[b]++] = dat[i];
    }
  }
  return(done);
}

#endif
