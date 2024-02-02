/*
# greeNsort Zick selection
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_Zickselect_h
#define ALREADY_DEFINED_Zickselect_h

#include "algo.h"
#include "approxMedian_insitu_left.h"
#include "approxMedian_insitu_right.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

//#define CHECK Rprintf("l=%d k=%d r=%d v=%f i=%d j=%d\n", l, k, r, v, i, j); for (int ii=l; ii<=r; ii++)Rprintf("x[%d]=%f\n", ii, x[ii]);
#define CHECK

static RangeIndexT Zickselect_TieLeft(
ValueT *x
, IndexT l, IndexT r, IndexT k
);
static RangeIndexT Zickselect_TieRight(
ValueT *x
, IndexT l, IndexT r, IndexT k
);

// these are simplified copies of the two main procedures above
// they do not choose their own pivot but receive it in x[l] resp. x[r]
// this is needed to avoid infinite recursion which can happen if the approx median oscillates
static RangeIndexT Zickselect_TieLeft_again(
ValueT *x
, IndexT l, IndexT r, IndexT k
);
static RangeIndexT Zickselect_TieRight_again(
ValueT *x
, IndexT l, IndexT r, IndexT k
);

// pivot partioning placing all pivot ties high
static RangeIndexT Zickselect_TieRight(
ValueT *x
, IndexT l, IndexT r, IndexT k
){
  RangeIndexT ret;
  IndexT i,j;
  assert(l<=k);
  assert(k<=r);
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
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
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
  ValueT t, v = approxMedian_insitu_right(x, l, r);
  assert(v == x[r]);
  IndexT distinct=FALSE;
  i = l; j = r;
  while(LT(x[i], v)) ++i; // sentinel stop guaranteed by pivot at the right
  // during the first pointer movement we don't need to check all-ties (moving implies distinct=TRUE at first element)
  if (i > l || NE(x[i], v))
    distinct=TRUE;
  // here we ignore that we might know (distinct=TRUE) -- usually the following loop is traversed few times
  while (GE(x[--j], v)) {
    if (NE(x[j], v)) {
      distinct=TRUE;
      while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
      break;
    } else if (j <= i) break; // explicit stop of for loop
  };
  if (i < j){
    distinct=TRUE;
    SWAP(x[i], x[j], t);
    for (;;){
      while(LT(x[++i], v));                      // sentinel stop guaranteed by pivot at the right
      while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
      if (j <= i)break;
      SWAP(x[i], x[j], t);
    }
  }
  CHECK
  if (distinct){  // not all value tie with pivot
    // now pivot in i and in upper half
    // the index i from the first loop is guaranteed to be in a legal position
    if(k<i){
      SWAP(x[i], x[r], t);
      return Zickselect_TieLeft(x, l, i-1, k);
    }else{
      if (i==l){
        // since this pivot did not shrink r-l we need to do the mirrored procedure WITH THE SAME PIVOT
        SWAP(x[l], x[r], t);
        return Zickselect_TieLeft_again(x, l, r, k);
      }else{
        // not needed: SWAP(x[i], x[r], t);
        return Zickselect_TieLeft(x, i, r, k);
      }
    }
  }else{
    ret.min = l;
    ret.max = r;
    return ret;
  }
}

// pivot partioning placing all pivot ties low
static RangeIndexT Zickselect_TieLeft(
ValueT *x
, IndexT l, IndexT r, IndexT k
){
  RangeIndexT ret;
  IndexT i,j;
  assert(l<=k);
  assert(k<=r);
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
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
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
  IndexT distinct=FALSE;
  ValueT t, v = approxMedian_insitu_left(x, l, r);
  assert(v == x[l]);
  i = l; j = r;
  while(GT(x[j], v)) --j;                    // sentinel stop guaranteed by pivot at the left
  // during the first pointer movement we don't need to check all-ties (moving implies distinct=TRUE at first element)
  if (j < r || NE(x[j], v))
    distinct=TRUE;
  // here we ignore that we might know (distinct=TRUE) -- usually the following loop is traversed few times
  while (LE(x[++i], v)) {
    if (NE(x[i], v)) {
      distinct=TRUE;
      while (LE(x[++i], v)) if (j <= i) break;  // explicit stop of for loop
      break;
    } else if (j <= i) break; // explicit stop of for loop
  };
  if (i < j){
    distinct=TRUE;
    SWAP(x[i], x[j], t);
    for (;;){
      while(GT(x[--j], v));                      // sentinel stop guaranteed by pivot at the left
      while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
      if (i >= j)break;
      SWAP(x[i], x[j], t);
    }
  }
  CHECK
  if (distinct){  // not all value tie with pivot
    // now pivot in j and in lower half
    // the index j from the first loop is guaranteed to be in a legal position
    if(k<=j){
      if (j==r){
        // since this pivot did not shrink r-l we need to do the mirrored procedure WITH THE SAME PIVOT
        SWAP(x[l], x[r], t);
        return Zickselect_TieRight_again(x, l, r, k);
      }else{
        // not needed: SWAP(x[l], x[j], t);
        return Zickselect_TieRight(x, l, j, k);
      }
    }else{
      SWAP(x[l], x[j], t);
      return Zickselect_TieRight(x, j+1, r, k);
    }
  }else{
    ret.min = l;
    ret.max = r;
    return ret;
  }
}


// pivot partioning placing all pivot ties high
static RangeIndexT Zickselect_TieRight_again(
ValueT *x
, IndexT l, IndexT r, IndexT k
){
  RangeIndexT ret;
  IndexT i,j;
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
  ValueT t, v = x[r];
  IndexT distinct=FALSE;
  i = l; j = r;
  while(LT(x[i], v)) ++i; // sentinel stop guaranteed by pivot at the right
  // during the first pointer movement we don't need to check all-ties (moving implies distinct=TRUE at first element)
  if (i > l || NE(x[i], v))
    distinct=TRUE;
  // here we ignore that we might know (distinct=TRUE) -- usually the following loop is traversed few times
  while (GE(x[--j], v)) {
    if (NE(x[j], v)) {
      distinct=TRUE;
      while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
      break;
    } else if (j <= i) break; // explicit stop of for loop
  };
  if (i < j){
    distinct=TRUE;
    SWAP(x[i], x[j], t);
    for (;;){
      while(LT(x[++i], v));                      // sentinel stop guaranteed by pivot at the right
      while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
      if (j <= i)break;
      SWAP(x[i], x[j], t);
    }
  }
  CHECK
  if (distinct){  // not all value tie with pivot
    // now pivot in i and in upper half
    // the index i from the first loop is guaranteed to be in a legal position
    if(k<i){
      SWAP(x[i], x[r], t);
      return Zickselect_TieLeft(x, l, i-1, k);
    }else{
      // not needed: SWAP(x[i], x[r], t);
      return Zickselect_TieLeft(x, i, r, k);
    }
  }else{
    ret.min = l;
    ret.max = r;
    return ret;
  }
}

// pivot partioning placing all pivot ties low
static RangeIndexT Zickselect_TieLeft_again(
ValueT *x
, IndexT l, IndexT r, IndexT k
){
  RangeIndexT ret;
  IndexT i,j;
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
  IndexT distinct=FALSE;
  ValueT t, v = x[l];
  i = l; j = r;
  while(GT(x[j], v)) --j;                    // sentinel stop guaranteed by pivot at the left
  // during the first pointer movement we don't need to check all-ties (moving implies distinct=TRUE at first element)
  if (j < r || NE(x[j], v))
    distinct=TRUE;
  // here we ignore that we might know (distinct=TRUE) -- usually the following loop is traversed few times
  while (LE(x[++i], v)) {
    if (NE(x[i], v)) {
      distinct=TRUE;
      while (LE(x[++i], v)) if (j <= i) break;  // explicit stop of for loop
      break;
    } else if (j <= i) break; // explicit stop of for loop
  };
  if (i < j){
    distinct=TRUE;
    SWAP(x[i], x[j], t);
    for (;;){
      while(GT(x[--j], v));                      // sentinel stop guaranteed by pivot at the left
      while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
      if (i >= j)break;
      SWAP(x[i], x[j], t);
    }
  }
  CHECK
  if (distinct){  // not all value tie with pivot
    // now pivot in j and in lower half
    // the index j from the first loop is guaranteed to be in a legal position
    if(k<=j){
      // not needed: SWAP(x[l], x[j], t);
      return Zickselect_TieRight(x, l, j, k);
    }else{
      SWAP(x[l], x[j], t);
      return Zickselect_TieRight(x, j+1, r, k);
    }
  }else{
    ret.min = l;
    ret.max = r;
    return ret;
  }
}

#endif
