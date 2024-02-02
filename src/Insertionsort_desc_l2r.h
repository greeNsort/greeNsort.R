/*
# greeNsort Insertionsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// insertion sorting descending from left to right
// modifies *x

#ifndef ALREADY_DEFINED_Insertionsort_desc_l2r_h
#define ALREADY_DEFINED_Insertionsort_desc_l2r_h

#include "algo.h"

// starting from left
static void Insertionsort_desc_l2r(
    ValueT *x
  , IndexT l
  , IndexT r
)
{
  IndexT i, j;
  ValueT t;
  for (i=l+1;i<=r;i++){
    j=i;
    MOVE(t, x[j]);
    while (j>l && LT(x[j-1], t)){
      MOVE(x[j], x[j-1]);
      j--;
    }
    MOVE(x[j], t);
  }
}

#endif

