/*
# greeNsort DIET CPT sort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_DIET_CPT_sort_h
#define ALREADY_DEFINED_DIET_CPT_sort_h

#include "DIET_CPT_part.h"
#include "randIndex.h"

static void DietCPTsort_rec_TieLeft(
ValueT *x  // pointer to data and buffer
, IndexT l    // leftmost position to be sorted
, IndexT r    // rightmost position to be sorted
);

static void DietCPTsort_rec_TieRight(
ValueT *x  // pointer to data and buffer
, IndexT l    // leftmost position to be sorted
, IndexT r    // rightmost position to be sorted
){
  IndexT n = r-l+1;
  if (n>1){
    ValueT v;
    IndexT c[2];
    v = x[l + randIndex(n)];
    if (!DietCPTsort_partition_TieRight(x, l, r, v, c)){
      if (c[0]>1)
        DietCPTsort_rec_TieLeft(x, l, l+c[0]-1);
      l += c[0] + c[0];
      if (c[1]>1)
        DietCPTsort_rec_TieLeft(x, l, l+c[1]-1);
    }
  }
}

static void DietCPTsort_rec_TieLeft(
ValueT *x  // pointer to data and buffer
, IndexT l    // leftmost position to be sorted
, IndexT r    // rightmost position to be sorted
){
  IndexT n = r-l+1;
  if (n>1){
    ValueT v;
    IndexT c[2];
    v = x[l + randIndex(n)];
    if (!DietCPTsort_partition_TieLeft(x, l, r, v, c)){
      if (c[0]>1)
        DietCPTsort_rec_TieRight(x, l, l+c[0]-1);
      l += c[0] + c[0];
      if (c[1]>1)
        DietCPTsort_rec_TieRight(x, l, l+c[1]-1);
    }
  }
}

#endif
