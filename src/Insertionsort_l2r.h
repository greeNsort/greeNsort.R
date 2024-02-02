/*
# greeNsort Insertionsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// insertion sorting ascending from left to right
// modifies *x

#ifndef ALREADY_DEFINED_Insertionsort_l2r_h
#define ALREADY_DEFINED_Insertionsort_l2r_h

#include "algo.h"

/*
// starting from left
static void Insertionsort_l2r(
  ValueT *x
, IndexT l
, IndexT r
)
{
  IndexT i,j;
  ValueT t;
  // Sedgewick: put correct element as sentinel into rightmost position
  // this ruins scaling of sorting almost sorted data to big problems
  for (i=r;i>l;i--){
    if (LT(x[i],x[i-1])) {
      SWAP(x[i-1],x[i],t);
    }
  }
  for (i=l+2;i<=r;i++){
    j=i;
    MOVE(t, x[i]);
    while (LT(t,x[j-1])){
      MOVE(x[j], x[j-1]);
    j--;
    }
    MOVE(x[j], t);
  }
}
*/


// starting from left
static void Insertionsort_l2r(
    ValueT *x
  , IndexT l
  , IndexT r
)
{
  IndexT i, j;
  ValueT t;
  for (i=l+1;i<=r;i++){
    j=i;
    MOVE(t, x[i]);
    while (j>l && GT(x[j-1], t)){
      MOVE(x[j], x[j-1]);
      j--;
    }
    MOVE(x[j], t);
  }
}



/* The following version with a sentinel would be 10% faster, but is not possible for _r2l

static void SentinelInsertionsort_l2r(
    ValueT *x
  , IndexT l
  , IndexT r
)
{
  IndexT i, j;
  ValueT t;

  MOVE(t, x[l]);
  for (i=l+1;i<=r;i++){
    if (GT(t, x[i]))
      SWAP(x[i], x[1], t);
  }

  for (i=l+1;i<=r;i++){
    j=i;
    MOVE(t, x[i]);
    while (GT(x[j-1], t)){
      MOVE(x[j], x[j-1]);
      j--;
    }
    MOVE(x[j], t);
  }
}

*/

#endif
