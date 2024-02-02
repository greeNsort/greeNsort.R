/*
# greeNsort Gapped Insertionsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// insertion sorting ascending from left to right
// modifies *x
// moves smallest value as sentinel to the left

#ifndef ALREADY_DEFINED_GInsertionsort_l2r_h
#define ALREADY_DEFINED_GInsertionsort_l2r_h

#include "algo.h"

/* sentinel version
static void GInsertionsort_l2r(
    ValueT *x
  , IndexT l
  , IndexT r
)
{
  IndexT i;
  ValueT t;
  l*=2;
  r*=2;
  for (i=r;i>l;i-=2){
    if (LT(x[i],x[i-2])) {
      SWAP(x[i-2],x[i],t);
    }
  }
  for (i=l+4;i<=r;INC2(i)){
    IndexT j=i;
    ValueT v;
    MOVE(v, x[i]);
      while (LT(v,x[j-2])){
        MOVE(x[j], x[j-2]);
        DEC2(j);
      }
      MOVE(x[j], v);
  }
}
*/


/* no-sentinel version is more RAM-friendly */
static void GInsertionsort_l2r(
    ValueT *x
  , IndexT l
  , IndexT r
)
{
  IndexT i;
  l*=2;
  r*=2;
  for (i=l+2;i<=r;INC2(i)){
    IndexT j=i;
    ValueT v;
    MOVE(v, x[i]);
      while (j>l && LT(v,x[j-2])){
        MOVE(x[j], x[j-2]);
        DEC2(j);
      }
      MOVE(x[j], v);
  }
}

#endif
