/*
# greeNsort approximate median
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_approxMedian_insitu_right_h
#define ALREADY_DEFINED_approxMedian_insitu_right_h

#include "algo.h"
#include "Insertionsort_l2r.h"

// exchanges elements in x
// median of medians from right and returns in rightmost position
static ValueT approxMedian_insitu_right(
  ValueT *x
, IndexT l
, IndexT r
){
  IndexT i,j;
  ValueT t;
#if INSERTIONSORT_LIMIT > 0
  if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    SWAP(x[r], x[r-(r-l)/2],t);
    return x[r];
  }
#endif
  for(i=r+1,j=r-4; j>=l; j-=5){
    Insertionsort_l2r(x, j, j+4);
    i--;
    SWAP(x[i], x[j+2], t)
  }
  j+=4;
  if (j>=l){ // if not reached left end
    Insertionsort_l2r(x, l, j);
    i--;
    SWAP(x[i], x[j+(r-j)/2], t)
  }
  if (i >= (r-1))
    return x[r];
  else
    return approxMedian_insitu_right(x, i, r);
}

#endif
