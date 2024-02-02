/*
# greeNsort direct stabilized Quicksort2 B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#include "SInsertionsort_l2r.h"

static void SQuicksort2B(ValueT *x, IndexT *o, IndexT l, IndexT r)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
    SInsertionsort_l2r(x, o, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
#ifdef SCANNED_RANGE
  scanned_range += r-l+1;
#endif
  IndexT k = l+randIndex(r-l+1);
  //Rprintf("l=%d  i=%d  r=%d\n", l, i, r);
  ValueT t, v;
  IndexT s, w;
  SWAP(x[k], x[r], v); // first argument pivot now in v and x[r]
  SWAP(o[k], o[r], w); // first argument pivot now in v and x[r]

  // Begin Block-Processing
  IndexT L = l, R = r-1;
  unsigned char indexL[QUICKSORT_BLOCKSIZE], indexR[QUICKSORT_BLOCKSIZE];
  IndexT iL = 0;
  IndexT iR = 0;
  IndexT sL = 0;
  IndexT sR = 0;
  IndexT num;
  while (R - L + 1 > 2 * QUICKSORT_BLOCKSIZE) {
    if (iL == 0) {
      sL = 0;
      for (k = 0; k < QUICKSORT_BLOCKSIZE; k++) {
        indexL[iL] = k;
        iL += GE((x+L)[k], v, (o+L)[k], w); // == !LT
      }
    }
    if (iR == 0) {
      sR = 0;
      for (k = 0; k < QUICKSORT_BLOCKSIZE; k++) {
        indexR[iR] = k;
        iR += GE(v, x[R - k], w, o[R - k]); // == !LT
      }
    }
    num = MIN(iL, iR);
    if (num != 0) {
      t = (x+L)[indexL[sL]];
      s = (o+L)[indexL[sL]];
      (x+L)[+indexL[sL]] = (x+R)[-indexR[sR]];
      (o+L)[+indexL[sL]] = (o+R)[-indexR[sR]];
      for (k = 1; k < num; k++) {
        (x+R)[-indexR[sR + k - 1]] = (x+L)[+indexL[sL + k]];
        (x+L)[+indexL[sL + k    ]] = (x+R)[-indexR[sR + k]];
        (o+R)[-indexR[sR + k - 1]] = (o+L)[+indexL[sL + k]];
        (o+L)[+indexL[sL + k    ]] = (o+R)[-indexR[sR + k]];
      }
      (x+R)[-indexR[sR + num - 1]] = t;
      (o+R)[-indexR[sR + num - 1]] = s;
    }
    iL -= num;
    iR -= num;
    sL += num;
    sR += num;
    if (iL == 0)
      L += QUICKSORT_BLOCKSIZE;
    if (iR == 0)
      R -= QUICKSORT_BLOCKSIZE;
  }
  L--;
  R++;
  // End Block-Processing

  for (;;){
    while (++L,(LT(x[L], v, o[L], w))); // sentinel stop of for loop
    while ((--R,LT(v, x[R], w, o[R]))){
      if (R <= L)       // explicit stop of for loop
        break;
    }
    if (R <= L)
      break;
    SWAP(x[L], x[R], t);
    SWAP(o[L], o[R], s);
  }
  SWAP(x[L], x[r], t);
  SWAP(o[L], o[r], s);
  k = L;
  SQuicksort2B(x, o, l, k-1);
  SQuicksort2B(x, o, k+1, r);
}

void SQuicksort2B_insitu(ValueT *x, IndexT n, PerfT *p)
{
  IndexT i;
  IndexT *o = (IndexT*) MALLOC(n, typeof(IndexT));
  for (i=0;i<n;i++)
    o[i] = i;
  SQuicksort2B(x, o, 0, n-1);
  FREE(o);
  p->secs = getNewSecs();
  p->size = (sizeof(ValueT) + sizeof(IndexT)) / ((double) sizeof(ValueT));
}

void SQuicksort2B_exsitu(ValueT *x, IndexT n, PerfT *p)
{
  PerfT p1,p2;
  IndexT i;
  ValueT * y = (ValueT *) MALLOC(n, ValueT);
  IndexT *o = (IndexT*) MALLOC(n, typeof(IndexT));
  for (i=0;i<n;i++){
    y[i] = x[i];
    o[i] = i;
  }
  SQuicksort2B(y, o, 0, n-1);
  FREE(o);
  p1.secs = getNewSecs();
  p1.size = (sizeof(ValueT) + sizeof(IndexT)) / ((double)sizeof(ValueT));
  for (i=0;i<n;i++){
    x[i] = y[i];
  }
  FREE(y);
  p2.secs = getNewSecs() - p1.secs;
  p2.size = 1;

  p->secs = p1.secs + p2.secs;
  p->size= (p1.secs*p1.size + p2.secs*p2.size) / p->secs;
}
