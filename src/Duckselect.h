/*
# greeNsort Duck selection algorithm
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_Duckselect_h
#define ALREADY_DEFINED_Duckselect_h

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#include "randIndex.h"
#  include "Insertionsort_l2r.h"
#endif
#include "bsearch.h"

static RangeIndexT Duckselect_TieLeft(
    ValueT *x
  , IndexT l, IndexT r, IndexT k
);

// pivot partioning placing all pivot ties high
static RangeIndexT Duckselect_TieRight(
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
    ret.min = i+1;
    ret.max = j-1;
    return ret;
  }
#else
  if (l >= r){
    ret.min = ret.max = k;
    return ret;
  }
#endif
  ValueT t, v;
  // POET replaces DIET: tentatively invest more COMPs than DIET (and MAIN has to restart from scratch)
  j = r;
  while (--j, LE(x[j], x[j+1])) if (j <= l) goto final;  // explicit stop of for loop

  // MAIN
  j = r - randIndex(r-l+1);
  SWAP(x[j], x[r], v);  // first argument pivot now in v and x[r]
  j = r;
  i = l - 1;
  for (;;){
    while(LT(x[++i], v));                     // sentinel stop of for loop
    while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
    if (j <= i)break;
    SWAP(x[i], x[j], t);
  }

  if(k<i){
    return Duckselect_TieRight(x, l, i-1, k);
  }else{
    return Duckselect_TieLeft(x, i, r, k);
  }

final:
  v = x[k];
  i=k; BSEARCH_ASC_TOLEFT(x, l, i, v);
  j=k; BSEARCH_ASC_TORIGHT(x, j, r, v);
  ret.min = l;
  ret.max = r;
  return ret;
}

// pivot partioning placing all pivot ties low
static RangeIndexT Duckselect_TieLeft(
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
    ret.min = i+1;
    ret.max = j-1;
    return ret;
  }
#else
  if (l >= r){
    ret.min = ret.max = k;
    return ret;
  }
#endif
  ValueT t, v;
  // POET replaces DIET: tentatively invest more COMPs than DIET (and MAIN has to restart from scratch)
  i = l;
  while (++i, LE(x[i-1], x[i])) if (i >= r) goto final; // explicit stop of for loop

  // MAIN
  i = l + randIndex(r-l+1);
  SWAP(x[i], x[l], v);  // first argument pivot now in v and x[l]
  i = l;
  j = r + 1;
  for (;;){
    while(GT(x[--j], v)); // sentinel stop guaranteed by pivot at the left
    while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
    if (i >= j) break;
    SWAP(x[i], x[j], t);
  }

  if(k<=j){
    return Duckselect_TieRight(x, l, j, k);
  }else{
    return Duckselect_TieLeft(x, j+1, r, k);
  }

final:
  v = x[k];
  i=k; BSEARCH_ASC_TOLEFT(x, l, i, v);
  j=k; BSEARCH_ASC_TORIGHT(x, j, r, v);
  ret.min = l;
  ret.max = r;
  return ret;
}

#endif
