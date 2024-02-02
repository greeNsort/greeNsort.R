/*
# greeNsort approximate median
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_approxMedian_insitu_left_h
#define ALREADY_DEFINED_approxMedian_insitu_left_h

#include "algo.h"
#include "Insertionsort_l2r.h"

// exchanges elements in x
// median of medians from left and returns in leftmost position
static ValueT approxMedian_insitu_left(
  ValueT *x
, IndexT l
, IndexT r
){
  IndexT i,j;
  ValueT t;
#if INSERTIONSORT_LIMIT > 0
  if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    SWAP(x[l], x[l+(r-l)/2],t);
    return x[l];
  }
#endif
  for(i=l-1,j=l+4; j<=r; j+=5){
    Insertionsort_l2r(x, j-4, j);
    i++;
    SWAP(x[i], x[j-2], t)
  }
  j-=4;
  if (j<=r){ // if not reached right end
    Insertionsort_l2r(x, j, r);
    i++;
    SWAP(x[i], x[j+(r-j)/2], t)
  }
  if (i <= (l+1))
    return x[l];
  else
    return approxMedian_insitu_left(x, l, i);
}

#endif
