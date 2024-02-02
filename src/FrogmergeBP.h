/*
# greeNsort common Frog merging B-tuned (not really) (pointered implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_FrogmergeBP_h
#define ALREADY_DEFINED_FrogmergeBP_h

#include "algo.h"

static void FrogmergeBP_asc_right(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u = *ll, v = *rl;
#endif
  if (LE(*lr, *rr)){
    // left exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(ll<lr)
      GT(u, v) ? (*(x++) = v, v = *(++rl)) : (*(x++) = u, u = *(++ll));
#endif
    while(ll<=lr)
      *(x++) = GT(*ll, *rl) ? *(rl++) : *(ll++);
  }else{
    // right exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(rl<rr)
      GT(u, v) ? (*(x++) = v, v = *(++rl)) : (*(x++) = u, u = *(++ll));
#endif
    while(rl<=rr)
      *(x++) = GT(*ll, *rl) ? *(rl++) : *(ll++);
  }

  while(ll<=lr){
    *(x++) = *(ll++);
  }
  return;
}

static void FrogmergeBP_asc_left(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u = *lr, v = *rr;
#endif
  if (LE(*ll, *rl)){
    // right exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(rl<rr)
      GT(u, v) ? (*(x--) = u, u = *(--lr)) : (*(x--) = v, v = *(--rr));
#endif
    while(rl<=rr)
      *(x--) = GT(*lr, *rr) ? *(lr--) : *(rr--);
  }else{
    // left exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(ll<lr)
      GT(u, v) ? (*(x--) = u, u = *(--lr)) : (*(x--) = v, v = *(--rr));
#endif
    while(ll<=lr)
      *(x--) = GT(*lr, *rr) ? *(lr--) : *(rr--);
  }

  while(rl<=rr){
    *(x--) = *(rr--);
  }
  return;
}

#endif
