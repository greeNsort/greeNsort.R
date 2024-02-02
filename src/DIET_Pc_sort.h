/*
# greeNsort DIET cP2 sort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_DIET_Pc_sort_h
#define ALREADY_DEFINED_DIET_Pc_sort_h

// whatever we choose here, DIET_Pc is slower than DIET_cP2
#define DIET_Pc_TRIALS 8

#include "DIET_Pc_part.h"
#include "DIET_count.h"
#include "Insertionsort_l2r.h"
//#include "Insertionorder_l2r.h"
#include "randIndex.h"

static void DietPcsort_rec_TieLeft(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
, ValueT v    // pivot value
, IndexT *c   // pointer to K initialized counters
, IndexT done  // flag optionally set to 1 for early termination after counting and before partitioning
);

static void DietPcsort_rec_TieRight(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
, ValueT v    // pivot value
, IndexT *c     // pointer to K initialized counters
, IndexT done  // flag optionally set to 1 for early termination after counting and before partitioning
){
  IndexT i;
  // Rprintf("DietPcsort_rec_TieRight l=%d nl=%d v=%f nr=%d r=%d done=%d\n", l, c[0], v, c[1], r, done);
  // for (i=l;i<=r;i++)
  //   Rprintf("dat[%d]=%f\n", i, dat[i]);
  if (done)
    goto fin;
  else if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(dat, l, r);
    goto fin;
  }
  IndexT ntrials=0;
  IndexT pi;
  IndexT pl = -1;
  IndexT pr = -1;
  ValueT vl,vr;
  IndexT cl[2];
  IndexT cr[2];
  while((pl==-1 || pr==-1) && ntrials++<DIET_Pc_TRIALS){
    pi = l + randIndex(r-l+1);
    if (LT(dat[pi], v))
      pl = pi;
    else
      pr = pi;
    // Rprintf("pl=%d pr=%d\n", pl, pr);
  }
  if (pl==-1 || pr==-1){
    // fallback if not both pivots found
    if (DIET_P_partition_TieRight(dat, buf, l, r, v, c))
      goto fin;
    if (c[0]){
      vl = buf[l + randIndex(c[0])];
      done = DIET_count_TieLeft(buf, l, r-c[1], vl, cl);
      DietPcsort_rec_TieLeft(ori, buf, dat, l, r-c[1], vl, cl, done);
    }
    if (c[1]){
      vr = buf[l + c[0] + randIndex(c[1])];
      done = DIET_count_TieLeft(buf, l+c[0], r, vr, cr);
      DietPcsort_rec_TieLeft(ori, buf, dat, l+c[0], r, vr, cr, done);
    }
  }else{
    vl = dat[pl];
    vr = dat[pr];
    // Rprintf("vl=%f vr=%f\n", vl, vr);
    if (DIET_Pc_partition_TieRight(dat, buf, l, r, v, c, vl, vr, cl, cr))
      goto fin;
    // Rprintf("cl0=%d cl1=%d   cr0=%d cr1=%d\n", cl[0], cl[1], cr[0], cr[1]);
    if (c[0]){
      DietPcsort_rec_TieLeft(ori, buf, dat, l, r-c[1], vl, cl, 0);
    }
    if (c[1]){
      DietPcsort_rec_TieLeft(ori, buf, dat, l+c[0], r, vr, cr, 0);
    }
  }
  return;

  fin:
  if (dat != ori){
    for (i=l; i<=r; i++)
      ori[i] =  dat[i];
  }
}

static void DietPcsort_rec_TieLeft(
    ValueT *ori
  , ValueT *dat
  , ValueT *buf
  , IndexT l
  , IndexT r
  , ValueT v    // pivot value
  , IndexT *c     // pointer to K initialized counters
  , IndexT done  // flag optionally set to 1 for early termination after counting and before partitioning
){
  IndexT i;
  // Rprintf("DietPcsort_rec_TieLeft l=%d nl=%d v=%f nr=%d r=%d done=%d\n", l, c[0], v, c[1], r, done);
  // for (i=l;i<=r;i++)
  //   Rprintf("dat[%d]=%f\n", i, dat[i]);
  if (done)
    goto fin;
  else if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(dat, l, r);
    goto fin;
  }
  IndexT ntrials=0;
  IndexT pi;
  IndexT pl = -1;
  IndexT pr = -1;
  ValueT vl,vr;
  IndexT cl[2];
  IndexT cr[2];
  while((pl==-1 || pr==-1) && ntrials++<DIET_Pc_TRIALS){
    pi = l + randIndex(r-l+1);
    if (LE(dat[pi], v))
      pl = pi;
    else
      pr = pi;
    // Rprintf("pl=%d pr=%d\n", pl, pr);
  }
  if (pl==-1 || pr==-1){
    // fallback if not both pivots found
    if (DIET_P_partition_TieLeft(dat, buf, l, r, v, c))
      goto fin;
    if (c[0]){
      vl = buf[l + randIndex(c[0])];
      done = DIET_count_TieRight(buf, l, r-c[1], vl, cl);
      DietPcsort_rec_TieRight(ori, buf, dat, l, r-c[1], vl, cl, done);
    }
    if (c[1]){
      vr = buf[l + c[0] + randIndex(c[1])];
      done = DIET_count_TieRight(buf, l+c[0], r, vr, cr);
      DietPcsort_rec_TieRight(ori, buf, dat, l+c[0], r, vr, cr, done);
    }
  }else{
    vl = dat[pl];
    vr = dat[pr];
    // Rprintf("vl=%f vr=%f\n", vl, vr);
    if (DIET_Pc_partition_TieLeft(dat, buf, l, r, v, c, vl, vr, cl, cr))
      goto fin;
    // Rprintf("cl0=%d cl1=%d   cr0=%d cr1=%d\n", cl[0], cl[1], cr[0], cr[1]);
    if (c[0]){
      DietPcsort_rec_TieRight(ori, buf, dat, l, r-c[1], vl, cl, 0);
    }
    if (c[1]){
      DietPcsort_rec_TieRight(ori, buf, dat, l+c[0], r, vr, cr, 0);
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
