/*
# greeNsort DIET cP2 sort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_DIET_cP2_sort_h
#define ALREADY_DEFINED_DIET_cP2_sort_h

#include "DIET_cP2_part.h"
#include "DIET_count.h"
#include "Insertionsort_l2r.h"
#include "randIndex.h"

static void DietcP2sort_rec_TieLeft(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
, ValueT v    // pivot value
, IndexT *c   // pointer to K initialized counters
, IndexT done
);

static void DietcP2sort_rec_TieRight(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
, ValueT v    // pivot value
, IndexT *c     // pointer to K initialized counters
, IndexT done
){
  IndexT i;
  // Rprintf("DietcP2sort_rec_TieRight l=%d nl=%d v=%f nr=%d r=%d done=%d\n", l, c[0], v, c[1], r, done);
  if (done){
    goto fin;
  }else if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(dat, l, r);
    goto fin;
  }
  IndexT c2[2];
  DIET_P_partition_TieRight(dat, buf, l, r, v, c);
  if (c[0]){
    v = buf[l + randIndex(c[0])];
    done = DIET_count_TieLeft(buf, l, r-c[1], v, c2);
    DietcP2sort_rec_TieLeft(ori, buf, dat, l, r-c[1], v, c2, done);
  }
  if (c[1]){
    v = buf[l + c[0] + randIndex(c[1])];
    done = DIET_count_TieLeft(buf, l+c[0], r, v, c2);
    DietcP2sort_rec_TieLeft(ori, buf, dat, l+c[0], r, v, c2, done);
  }
  return;

  fin:
  if (dat != ori){
    for (i=l; i<=r; i++)
      ori[i] =  dat[i];
  }
}

static void DietcP2sort_rec_TieLeft(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
, ValueT v    // pivot value
, IndexT *c     // pointer to K initialized counters
, IndexT done
){
  IndexT i;
  // Rprintf("DietcP2sort_rec_TieLeft l=%d nl=%d v=%f nr=%d r=%d done=%d\n", l, c[0], v, c[1], r, done);
  if (done){
    goto fin;
  }else if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(dat, l, r);
    goto fin;
  }
  IndexT c2[2];
  DIET_P_partition_TieLeft(dat, buf, l, r, v, c);
  if (c[0]){
    // if (pl==-1 || pr==-1){
    v = buf[l + randIndex(c[0])];
    done = DIET_count_TieRight(buf, l, r-c[1], v, c2);
    DietcP2sort_rec_TieRight(ori, buf, dat, l, r-c[1], v, c2, done);
  }
  if (c[1]){
    // if (pl==-1 || pr==-1){
    v = buf[l + c[0] + randIndex(c[1])];
    done = DIET_count_TieRight(buf, l+c[0], r, v, c2);
    DietcP2sort_rec_TieRight(ori, buf, dat, l+c[0], r, v, c2, done);
  }
  return;

  fin:
    if (dat != ori){
      for (i=l; i<=r; i++)
        ori[i] =  dat[i];
    }
}

#endif
