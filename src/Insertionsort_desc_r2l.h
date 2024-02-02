/*
# greeNsort Insertionsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// insertion sorting reverse descending (from right to left)
// using LE instead of LT is the price to pay for stable symmetry:
// we need additional exchanges in case of ties
// in order to completely reverse the order
// from left-to-right
// to right-to-left
// this also shows that COMSWAP is a unfavorate abstraction
// we should only use LT LE GE GT MV EX
// !! furthermore with LE we no longer can use a sentinel
// !! (when just pluggin LE instead of LT, we get nasty non-deterministic errors)

#ifndef ALREADY_DEFINED_Insertionsort_desc_r2l_h
#define ALREADY_DEFINED_Insertionsort_desc_r2l_h

#include "algo.h"

static void Insertionsort_desc_r2l(
  ValueT *x
, IndexT l
, IndexT r
)
{
  IndexT i,j;
  for (i=r-1;i>=l;i--){
    j=i;
    ValueT v;
    MOVE(v, x[i]);
    while (j<r && GE(v,x[j+1])){
      MOVE(x[j], x[j+1]);
      j++;
    }
    MOVE(x[j], v);
  }
}

#endif
