/*
# greeNsort DIET CP partition
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_DIET_CP_part_h
#define ALREADY_DEFINED_DIET_CP_part_h

#include "DIET_count.h"

// ascending partitioning of data between l and r around pivot
// returns 1 if partitioning is done (due to all ties)
// and does not copy data in the done case
static IndexT DIET_CP_partition_TieRight(
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
    IndexT i0, i1;  // 2 positions to write to
    // initialize positions in buffer part
    i0 = l;
    i1 = i0 + c[0];
    // partition
    for (i=l; i<=r; i++){
      if (LT(dat[i], v)){
        buf[i0++] = dat[i];
      }else{
        buf[i1++] = dat[i];
      }
      // LT(dat[i], v) ? (buf[i0++] = dat[i]) : (buf[i1++] = dat[i]);
    }
  }
  return(done);
}

static IndexT DIET_CP_partition_TieLeft(
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
    IndexT i0, i1;  // 2 positions to write to
    // initialize positions in buffer part
    i0 = l;
    i1 = i0 + c[0];
    // partition
    for (i=l; i<=r; i++){
      if (LE(dat[i], v)){
        buf[i0++] = dat[i];
      }else{
        buf[i1++] = dat[i];
      }
      // LE(dat[i], v) ? (buf[i0++] = dat[i]) : (buf[i1++] = dat[i]);
    }
  }
  return(done);
}

#endif
