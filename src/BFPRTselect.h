/*
# greeNsort BFPRT headers
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_BFPRTselect_h
#define ALREADY_DEFINED_BFPRTselect_h

#include "algo.h"
#include "approxMedian_insitu_right.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


// selection using approx median
static ValueT BFPRTselect(ValueT *x, IndexT l, IndexT r, IndexT k)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#endif
    assert(l <= r);
    if (l == r)
      return x[r];
#if INSERTIONSORT_LIMIT > 0
    Insertionsort_l2r(x, l, r);
    return x[k];
  }
#endif
  IndexT j, i;
  ValueT t, v;
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
    v = x[r];
  else{
    j = r - (r-i)/2;
    v = BFPRTselect(x, i, r, j);
    SWAP(x[j], x[r], t);
    assert(t == v);
  }

  i = l-1; j = r;
  for (;;){
    while (LT(x[++i], v)); // sentinel stop of for loop
    while (LT(v, x[--j]))
      if (j <= i)       // explicit stop of for loop
        break;
    if (j <= i)
      break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[i], x[r], t);
  if (i == k)
    return x[i];
  else if(k<i)
    return BFPRTselect(x, l, i-1, k);
  else
    return BFPRTselect(x, i+1, r, k);
}

#endif
