/*
# greeNsort partial Quickselect 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_Quickselect2_h
#define ALREADY_DEFINED_Quickselect2_h

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static ValueT Quickselect2(ValueT *x, IndexT l, IndexT r, IndexT k)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#endif
    assert(l <= r);
    if (l == r)
      return x[l];
#if INSERTIONSORT_LIMIT > 0
    Insertionsort_l2r(x, l, r);
    return x[k];
  }
#endif
  IndexT j,i = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[i], x[r], v); // now pivot is in v and x[r]
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
    return Quickselect2(x, l, i-1, k);
  else
    return Quickselect2(x, i+1, r, k);
  // if(k<i)
  //   return Quickselect2(x, l, i-1, k);
  // else if (k>i)
  //   return Quickselect2(x, i+1, r, k);
  // else
  //   return x[i];
}

#endif
