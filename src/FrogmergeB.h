/*
# greeNsort common Frog merging B-tuned (not really)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_FrogmergeB_h
#define ALREADY_DEFINED_FrogmergeB_h

#include <stdbool.h>
#include "algo.h"

static void FrogmergeB_asc_right_full(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  IndexT ll = 0;
  const IndexT lr = nl - 1;
  IndexT rl = 0;
  const IndexT rr = nr - 1;
  nr = 0;  // re-using nr as write-index

#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u = ldat[lr], v = rdat[rr];
#endif
  if (LE(ldat[lr], rdat[rr])){
    // left exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(ll<lr)
      GT(u, v) ? (x[nr++] = v, v = rdat[++rl]) : (x[nr++] = u, u = ldat[++ll]);
#endif
    while(ll<=lr)
      x[nr++] = GT(ldat[ll], rdat[rl]) ? rdat[rl++] : ldat[ll++];
  }else{
    // right exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(rl<rr)
      GT(u, v) ? (x[nr++] = v, v = rdat[++rl]) : (x[nr++] = u, u = ldat[++ll]);
#endif
    while(rl<=rr)
      x[nr++] = GT(ldat[ll], rdat[rl]) ? rdat[rl++] : ldat[ll++];
  }

  while(ll<=lr){
      x[nr++] = ldat[ll++];
  }
  while(rl<=rr){
      x[nr++] = rdat[rl++];
  }
  return;
}

static void FrogmergeB_asc_right_final(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  IndexT ll = 0;
  const IndexT lr = nl - 1;
  IndexT rl = 0;
  const IndexT rr = nr - 1;
  nr = 0;  // re-using nr as write-index

#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u = ldat[ll], v = rdat[rl];
#endif
  if (LE(ldat[lr], rdat[rr])){
    // left exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(ll<lr)
      GT(u, v) ? (x[nr++] = v, v= rdat[++rl]) : (x[nr++] = u, u = ldat[++ll]);
#endif
    while(ll<=lr)
      x[nr++] = GT(ldat[ll], rdat[rl]) ? rdat[rl++] : ldat[ll++];
  }else{
    // right exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(rl<rr)
      GT(u, v) ? (x[nr++] = v, v = rdat[++rl]) : (x[nr++] = u, u = ldat[++ll]);
#endif
    while(rl<=rr)
      x[nr++] = GT(ldat[ll], rdat[rl]) ? rdat[rl++] : ldat[ll++];
  }

  while(ll<=lr){
      x[nr++] = ldat[ll++];
  }
  return;
}



static void FrogmergeB_asc_right_stable(
    ValueT *x
  , IndexT nl  // == b
  , IndexT nr
  , IndexT R
){
  const IndexT rr = R;
  IndexT rl = R - nr + 1;
  const IndexT lr = rl - nl - 1;
  IndexT ll = lr - nl + 1;
  nr  = lr + 1;  // re-using nr as write-index
#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u = x[ll], v = x[rl];
#endif
  if (LE(x[lr], x[rr])){
    // left exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(ll<lr)
      GT(u, v) ? (x[nr++] = v, v = x[++rl]) : (x[nr++] = u, u = x[++ll]);
#endif
    while(ll<=lr)
      x[nr++] = GT(x[ll], x[rl]) ? x[rl++] : x[ll++];
  }else{
    // right exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(rl<rr)
      GT(u, v) ? (x[nr++] = v, v = x[++rl]) : (x[nr++] = u, u = x[++ll]);
#endif
    while(rl<=rr)
      x[nr++] = GT(x[ll], x[rl]) ? x[rl++] : x[ll++];
  }

  while(ll<=lr){
    x[nr++] = x[ll++];
  }
  return;
}


static void FrogmergeB_asc_left_stable(
    ValueT *x
  , IndexT nl
  , IndexT nr  // == b
  , IndexT L
){
  const IndexT ll = L;
  IndexT lr = L + nl - 1;
  const IndexT rl = lr + nr + 1;
  IndexT rr = rl + nr - 1;
  nl  = rl - 1;  // re-using nl as write-index

#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u = x[lr], v = x[rr];
#endif
  if (LE(x[ll], x[rl])){
    // right exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(rl<rr)
      GT(u, v) ? (x[nl--] = u, u = x[--lr]) : (x[nl--] = v, v = x[--rr]);
#endif
    while(rl<=rr)
      x[nl--] = GT(x[lr], x[rr]) ? x[lr--] : x[rr--];
  }else{
    // left exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(ll<lr)
      GT(u, v) ? (x[nl--] = u, u = x[--lr]) : (x[nl--] = v, v = x[--rr]);
#endif
    while(ll<=lr)
      x[nl--] = GT(x[lr], x[rr]) ? x[lr--] : x[rr--];
  }

  while(rl<=rr){
    x[nl--] = x[rr--];
  }
  return;
}

#endif
