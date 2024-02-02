/*
# greeNsort DIET ACP sort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_DIET_ACP_sort_h
#define ALREADY_DEFINED_DIET_ACP_sort_h

#include "DIET_CP_part.h"
#include "approxMedian_exsitu.h"

static void DietACPsort_rec_TieLeft(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
, ValueT *aux5  // pointer to 5 aux elements
);

static void DietACPsort_rec_TieRight(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
, ValueT *aux5  // pointer to 5 aux elements
){
  IndexT i;
  if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(dat, l, r);
    goto fin;
  }
  IndexT c[2];
  ValueT v = approxMedian_exsitu_rec(dat+l, buf+l, aux5, r-l+1);
  if (DIET_CP_partition_TieRight(dat, buf, l, r, v, c)){
    // done: data was not copied from dat to buf
    goto fin;
  }else{
    // dat was partitioned to buf memory
    if (c[0]){
      DietACPsort_rec_TieLeft(ori, buf, dat, l, r-c[1], aux5);
    }
    if (c[1]){
      DietACPsort_rec_TieLeft(ori, buf, dat, l+c[0], r, aux5);
    }
  }
  return;
  fin:
    if (dat != ori){
      for (i=l; i<=r; i++)
        ori[i] =  dat[i];
    }
}

static void DietACPsort_rec_TieLeft(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
, ValueT *aux5  // pointer to 5 aux elements
){
  IndexT i;
  if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(dat, l, r);
    goto fin;
  }
  IndexT c[2];
  ValueT v = approxMedian_exsitu_rec(dat+l, buf+l, aux5, r-l+1);
  if (DIET_CP_partition_TieLeft(dat, buf, l, r, v, c)){
    // done: data was not copied from dat to buf
    goto fin;
  }else{
    // dat was partitioned to buf memory
    if (c[0]){
      DietACPsort_rec_TieRight(ori, buf, dat, l, r-c[1], aux5);
    }
    if (c[1]){
      DietACPsort_rec_TieRight(ori, buf, dat, l+c[0], r, aux5);
    }
  }
  return;

  fin:
    if (dat != ori){
      for (i=l; i<=r; i++)
        ori[i] =  dat[i];
    }
}

#endif
