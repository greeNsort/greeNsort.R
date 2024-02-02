/*
# greeNsort Zack selection
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_Zackselect_h
#define ALREADY_DEFINED_Zackselect_h

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static RangeIndexT Zackselect_TieLeft(
ValueT *x
, IndexT l, IndexT r, IndexT k
);

// pivot partioning placing all pivot ties high
static RangeIndexT Zackselect_TieRight(
ValueT *x
, IndexT l, IndexT r, IndexT k
){
  RangeIndexT ret;
  IndexT i,j;
  assert(l<=k);
  assert(k<=r);
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    for(i=k-1;i>=l;i--){
      if (NE(x[i],x[k]))
        break;
    }
    for(j=k+1;j<=r;j++){
      if (NE(x[j],x[k]))
        break;
    }
    ret.min = i+1; ret.max = j-1; return ret;
  }
#else
  if (l >= r){
    ret.min = ret.max = k; return ret;
  }
#endif
  ValueT t, v;
  i = l+randIndex(r-l+1);
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]

  // elegant
  j = r;
  while (EQ(x[--j], v)) if (j <= l) goto final;  // explicit stop of for loop

  // restore  the usual initialization i=l-1; j=r, i.e. we test the same value that was EQ again for GE
  i = l - 1;  j++;
  for (;;){
    while(LT(x[++i], v));                      // sentinel stop guaranteed by pivot at the right
    while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
    if (j <= i)break;
    SWAP(x[i], x[j], t);
  }
  //not needed SWAP(x[i], x[r], t);
  if(k<i){
    return Zackselect_TieLeft(x, l, i-1, k);
  }else{
    return Zackselect_TieLeft(x, i, r, k);
  }

  final: ret.min = l; ret.max = r; return ret;
}

// pivot partioning placing all pivot ties low
static RangeIndexT Zackselect_TieLeft(
ValueT *x
, IndexT l, IndexT r, IndexT k
){
  RangeIndexT ret;
  IndexT i,j;
  assert(l<=k);
  assert(k<=r);
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    for(i=k-1;i>=l;i--){
      if (NE(x[i],x[k]))
        break;
    }
    for(j=k+1;j<=r;j++){
      if (NE(x[j],x[k]))
        break;
    }
    ret.min = i+1; ret.max = j-1; return ret;
  }
#else
  if (l >= r){
    ret.min = ret.max = k; return ret;
  }
#endif
  j = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[j], x[l], v);  // first argument pivot now in v and x[l]

  // elegant
  i = l;
  while (EQ(x[++i], v)) if (i >= r) goto final;  // explicit stop of for loop

  // restore  the usual initialization i=l; j=r+1, i.e. we test the same value that was EQ again for GE
  i--; j = r+1;
  for (;;){
    while(GT(x[--j], v));                      // sentinel stop guaranteed by pivot at the left
    while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
    if (i >= j)break;
    SWAP(x[i], x[j], t);
  }
  if(k<=j){
    return Zackselect_TieRight(x, l, j, k);
  }else{
    return Zackselect_TieRight(x, j+1, r, k);
  }
  final: ret.min = l; ret.max = r; return ret;
}


#endif
