/*
# greeNsort direct stabilized Insertionsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// insertion sorting ascending from left to right
// modifies *x
// moves smallest value as sentinel to the left

#ifndef ALREADY_DEFINED_SInsertionsort_l2r_h
#define ALREADY_DEFINED_SInsertionsort_l2r_h

#include "algo.h"

#undef LT
#define LT(A,B,a,b) ( (KEY(A) < KEY(B)) || ((KEY(A) == KEY(B)) && (a < b) ))
#undef GE
#define GE(A,B,a,b)( (KEY(A) > KEY(B)) || ( (KEY(A) == KEY(B)) && (a>=b)) )

static void SInsertionsort_l2r(
    ValueT *x
  , IndexT *o
  , IndexT l
  , IndexT r
)
{
  IndexT i;
  ValueT t;
  IndexT s;
  for (i=r;i>l;i--){
    if (LT(x[i],x[i-1],o[i],o[i-1])){
      SWAP(x[i-1],x[i],t);
      SWAP(o[i-1],o[i],s);
    }
  }
  for (i=l+2;i<=r;i++){
    IndexT j=i;
    MOVE(t, x[i]);
    MOVE(s, o[i]);
      while (LT(t,x[j-1],s,o[j-1])){
        MOVE(x[j], x[j-1]);
        MOVE(o[j], o[j-1]);
        j--;
      }
      MOVE(x[j], t);
      MOVE(o[j], s);
  }
}

#endif
