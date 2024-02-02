/*
# greeNsort DIET cP2 partition
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_DIET_cP2_part_h
#define ALREADY_DEFINED_DIET_cP2_part_h

#include "algo.h"

static void DIET_P_partition_TieRight(
    ValueT *dat       // pointer to memory with current source data now
  , ValueT *buf       // pointer to memory for buffer (receives target data)
  , IndexT l      // leftmost position to be sorted
  , IndexT r      // rightmost position to be sorted
  , ValueT v     // pivot value
  , IndexT *c     // pointer to K initialized counters
)
{
  IndexT i;
  IndexT i0, i1;  // 2 positions to write to
  // initialize positions in buffer part
  i0 = l;
  i1 = i0 + c[0];
  for (i=l; i<=r; i++){
    if (LT(dat[i], v)){
      buf[i0++] = dat[i];
    }else{
      buf[i1++] = dat[i];
    }
  }
}

static void DIET_P_partition_TieLeft(
    ValueT *dat       // pointer to memory with current source data now
  , ValueT *buf       // pointer to memory for buffer (receives target data)
  , IndexT l      // leftmost position to be sorted
  , IndexT r      // rightmost position to be sorted
  , ValueT v     // pivot value
  , IndexT *c     // pointer to K initialized counters
)
{
  IndexT i;
  IndexT i0, i1;  // 2 positions to write to
  // initialize positions in buffer part
  i0 = l;
  i1 = i0 + c[0];
  for (i=l; i<=r; i++){
    if (LE(dat[i], v)){
      buf[i0++] = dat[i];
    }else{
      buf[i1++] = dat[i];
    }
  }
}


#endif
