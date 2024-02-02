/*
# greeNsort DIET CP sort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_DIET_CP_sort_h
#define ALREADY_DEFINED_DIET_CP_sort_h

#include "DIET_CP_part.h"
#  include "Insertionsort_l2r.h"
#include "randIndex.h"

static void DietCPsort_rec_TieLeft(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
);

static void DietCPsort_rec_TieRight(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
){
  IndexT i;
  if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(dat, l, r);
    goto fin;
  }
  IndexT c[2];
  if (DIET_CP_partition_TieRight(dat, buf, l, r, dat[l + randIndex(r-l+1)], c)){
    // done: data was not copied from dat to buf
    goto fin;
  }else{
    // dat was partitioned to buf memory
      DietCPsort_rec_TieLeft(ori, buf, dat, l, r-c[1]);
      DietCPsort_rec_TieLeft(ori, buf, dat, l+c[0], r);
      return;
  }
  fin:
    if (dat != ori){
    for (i=l; i<=r; i++)
      ori[i] =  dat[i];
  }
}

static void DietCPsort_rec_TieLeft(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
){
  IndexT i;
  if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(dat, l, r);
    goto fin;
  }
  IndexT c[2];
  if (DIET_CP_partition_TieLeft(dat, buf, l, r, dat[l + randIndex(r-l+1)], c)){
    // done: data was not copied from dat to buf
    goto fin;
  }else{
    // dat was partitioned to buf memory
      DietCPsort_rec_TieRight(ori, buf, dat, l, r-c[1]);
      DietCPsort_rec_TieRight(ori, buf, dat, l+c[0], r);
      return;
  }
  fin:
    if (dat!=ori) // check if dat memory equal original memory, if not, then copy back
      for (i=l; i<=r; i++)
        ori[i] = dat[i];
}

#endif
