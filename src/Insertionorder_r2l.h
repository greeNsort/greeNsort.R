/*
# greeNsort Insertionorder
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_Insertionorder_r2l_h
#define ALREADY_DEFINED_Insertionorder_r2l_h

#include "algo.h"

// insertion ordering reverse ascending (from right to left)
// using LE instead of LT is the price to pay for stable symmetry:
// we need additional exchanges in case of ties
// in order to completely reverse the order
// from left-to-right
// to right-to-left
// this also shows that COMSWAP is a unfavorate abstraction
// we should only use LT LE GE GT MV EX
// !! furthermore with LE we no longer can use a sentinel
// !! (when just pluggin LE instead of LT, we get nasty non-deterministic errors)

static void Insertionorder_r2l(
  ValueT *x
, int *o
, int l
, int r
)
{
  int i,j;
  ValueT v;
  int p;
  for (i=l+1;i<=r;i++){
    j=i;
    MOVE(p, o[i]);
      MOVE(v, x[p]);
      while (j>l && LE(x[o[j-1]], v)){
        MOVE(o[j], o[j-1]);
        j--;
      }
      MOVE(o[j], p);
  }
}

#endif
