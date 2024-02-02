/*
# greeNsort Quicksort2 B-tuned 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static void Quicksort2B(ValueT *x, IndexT l, IndexT r)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
    Insertionsort_l2r(x, l, r);
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
  SWAP(x[k], x[r], v); // first argument pivot now in v and x[r]

  // Begin Block-Processing
  ValueT *L = x+l, *R = x+r-1;
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
    while (LT(v, *(--R)))
      if (R <= L)       // explicit stop of for loop
        break;
    if (R <= L)
      break;
    SWAP(*L, *R, t);
  }
  SWAP(*L, x[r], t);
  k = L - x;
  Quicksort2B(x, l, k-1);
  Quicksort2B(x, k+1, r);
}

void Quicksort2B_insitu(ValueT *x, IndexT n)
{
  Quicksort2B(x, 0, n-1);
}

void Quicksort2B_exsitu(ValueT *x, IndexT n)
{
	IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Quicksort2B(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
