/*
# greeNsort indirect Quicksort2 B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#include "RInsertionsort_l2r.h"

static void RQuicksort2B(ValueT **x, IndexT l, IndexT r)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    RInsertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
  IndexT k = l+randIndex(r-l+1);
  //Rprintf("l=%d  i=%d  r=%d\n", l, i, r);
  ValueT *t, *v;
  SWAP(x[k], x[r], v); // first argument pivot now in v and x[r]

  // Begin Block-Processing
  ValueT **L = x+l, **R = x+r-1;
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
        iL += GE(L[k], v); // == !LT
      }
    }
    if (iR == 0) {
      sR = 0;
      for (k = 0; k < QUICKSORT_BLOCKSIZE; k++) {
        indexR[iR] = k;
        iR += GE(v, (*(R - k))); // == !LT
      }
    }
    num = MIN(iL, iR);
    if (num != 0) {
      t = L[indexL[sL]];
      L[+indexL[sL]] = R[-indexR[sR]];
      for (k = 1; k < num; k++) {
        R[-indexR[sR + k - 1]] = L[+indexL[sL + k]];
        L[+indexL[sL + k    ]] = R[-indexR[sR + k]];
      }
      R[-indexR[sR + num - 1]] = t;
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
    while (LT(*(++L), v)); // sentinel stop of for loop
    while (LT(v, *(--R))){
      if (R <= L)       // explicit stop of for loop
        break;
    }
    if (R <= L)
      break;
    SWAP(*L, *R, t);
  }
  SWAP(*L, x[r], t);
  k = L - x;
  RQuicksort2B(x, l, k-1);
  RQuicksort2B(x, k+1, r);
}

void RQuicksort2B_insitu(ValueT *x, IndexT n, int reorder_inplace, PerfT *p)
{
  PerfT p1,p2;
  IndexT i;
  ValueT **z = (ValueT**) MALLOC(n, typeof(ValueT*));
  for (i=0;i<n;i++){
    z[i] = x+i;
  }
  RQuicksort2B(z, 0, n-1);

  p1.secs = getNewSecs();
  p1.size = (sizeof(ValueT*) + sizeof(ValueT)) / sizeof(ValueT);

  if (reorder_inplace != 0){
    // reorder inplace rather slow
    IndexT j,k;
    ValueT t;
    for (i = 0; i < n; i++){
      t = x[i];
      j = i;
      for(;;){
        k = z[j] - x;
        z[j] = x + j;
        if (k == i)
          break;
        x[j] = x[k];
        j = k;
      }
      x[j] = t;
    }
    p2.size = (sizeof(ValueT*) + sizeof(ValueT)) / ((double)sizeof(ValueT));
  }else{
    // reorder using extra buffer is faster and leads to lower sizesecs
    ValueT *y = (ValueT *) MALLOC(n, typeof(ValueT));
    for (i=0;i<n;i++){
      y[i] = *z[i];
    }
    for (i=0;i<n;i++){
      x[i] = y[i];
    }
    FREE(y);
    p2.size = (sizeof(ValueT*) + 2*sizeof(ValueT)) / ((double)sizeof(ValueT));
  }

  FREE(z);
  p2.secs = getNewSecs() - p1.secs;

  p->secs = p1.secs + p2.secs;
  p->size= (p1.secs*p1.size + p2.secs*p2.size) / p->secs;
}

void RQuicksort2B_exsitu(ValueT *x, IndexT n, PerfT *p)
{
  IndexT i;
  ValueT *y = (ValueT *) MALLOC(n, typeof(ValueT));
  ValueT **z = (ValueT**) MALLOC(n, typeof(ValueT*));
  for (i=0;i<n;i++){
    y[i] = x[i];
    z[i] = y+i;
  }
  RQuicksort2B(z, 0, n-1);
  // now copy back stably
  for (i=0;i<n;i++){
    x[i] = *z[i];
  }
  FREE(y);
  FREE(z);
  p->secs = getNewSecs();
  p->size = (sizeof(ValueT*) + sizeof(ValueT)) / ((double)sizeof(ValueT));
}
