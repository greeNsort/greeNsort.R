/*
# greeNsort Slicksort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "Quickselect2.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


static void Slicksort(ValueT *x, IndexT l, IndexT r)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
  IndexT i = (r+l)/2;
  ValueT v = Quickselect2(x, l, r, i);
  // this algorithm is shitty even if we skip the usual partitioniong because Pickselect has already done it
  IndexT j;
  ValueT t;
  x[i] = x[r]; x[r] = v; // SWAP  // pivot now in v and x[r]
  i = l-1; j = r;
  for (;;){
    while (LT(x[++i], v)); // sentinel stop of for loop
    while (LT(v, x[--j])){
      if (j <= i)       // explicit stop of for loop
        break;
    }
    if (j <= i)
      break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[i], x[r], t);
  Slicksort(x, l, i-1);
  Slicksort(x, i+1, r);
}

void Slicksort_insitu(ValueT *x, IndexT n)
{
  Slicksort(x, 0, n-1);
}