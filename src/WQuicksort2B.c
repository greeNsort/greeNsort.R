/*
# greeNsort indirect stabilized Quicksort2 for variable-sized strings B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#include "WInsertionsort_l2r.h"
#include "strtools.h"

static void WQuicksort2B(char **x, IndexT l, IndexT r)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
    WInsertionsort_l2r(x, l, r);
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
  char *t, *v;
  SWAP(x[k], x[r], v); // first argument pivot now in v and x[r]

  // Begin Block-Processing
  char **L = x+l, **R = x+r-1;
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
  WQuicksort2B(x, l, k-1);
  WQuicksort2B(x, k+1, r);
}

void WQuicksort2B_insitu(char *x, IndexT n, IndexT m, PerfT *p)
{
  PerfT p1,p2;
  IndexT i,s;
  char **z = (char **) MALLOC(n, typeof(char*));
  m = 1;
  for (i=0;i<n;i++){
    s = strlen_lr(x+m);
    z[i] = x+m;
    m += s;
  }
  WQuicksort2B(z, 0, n-1);
  p1.secs = getNewSecs();
  p1.size = (n*sizeof(char*) + m*sizeof(char)) / ((double) m*sizeof(char));

  char *y = (char *) MALLOC(m, typeof(char));
  // now copy back stably
  m = 1;
  for (i=0;i<n;i++){
    m += strcpylen_lr(y+m, z[i]);
  }
  for (i=0;i<m;i++){
    x[i] = y[i];
  }
  FREE(y);
  FREE(z);
  p2.secs = getNewSecs() - p1.secs;
  p2.size = (n*sizeof(char*) + 2*m*sizeof(char)) / ((double) m*sizeof(char));
  p->secs = p1.secs + p2.secs;
  p->size= (p1.secs*p1.size + p2.secs*p2.size) / p->secs;
}

void WQuicksort2B_exsitu(char *x, IndexT n, IndexT m, PerfT *p)
{
  IndexT i,s;
  char *y = (char *) MALLOC(m, typeof(char));
  char **z = (char **) MALLOC(n, typeof(char*));
  m = 1;
  for (i=0;i<n;i++){
    s = strcpylen_lr(y+m, x+m);
    z[i] = y+m;
    m += s;
  }
  WQuicksort2B(z, 0, n-1);
  // now copy back
  m = 1;
  for (i=0;i<n;i++){
    m += strcpylen_lr(x+m, z[i]);
  }
  FREE(y);
  FREE(z);
  p->secs = getNewSecs();
  p->size = (n*sizeof(char*) + m*sizeof(char)) / ((double) m*sizeof(char));
}
