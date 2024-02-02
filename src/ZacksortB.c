/*
# greeNsort Zacksort B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


static void ZacksortB_TieLeft(
ValueT *x
, IndexT l, IndexT r
);

// pivot partioning placing all pivot ties high
static void ZacksortB_TieRight(
ValueT *x
, IndexT l, IndexT r
){
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
  IndexT k = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[k], x[r], v); // first argument pivot now in v and x[r]
  ValueT *L = x+l, *R = x+r;

  // elegant preloop
  while (EQ(*(--R), v)) if (R <= L) return;  // explicit stop of for loop

  //if (L < R){
    // Begin Block-Processing
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
          iL += GE(L[k], v);
        }
      }
      if (iR == 0) {
        sR = 0;
        for (k = 0; k < QUICKSORT_BLOCKSIZE; k++) {
          indexR[iR] = k;
          iR += GT(v, (*(R - k)));
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
      while(LT(*(++L), v));                     // sentinel stop guaranteed by pivot at the right
      while (GE(*(--R), v)) if (R <= L) break;  // explicit stop of for loop
      if (R <= L)break;
      SWAP(*L, *R, t);
    }

    SWAP(*L, x[r], t); // the index i from the first loop is guaranteed to be in a legal position
    k = L - x;
    ZacksortB_TieLeft(x, l, k-1);
    ZacksortB_TieLeft(x, k+1, r);

  //}
}

// pivot partioning placing all pivot ties low
static void ZacksortB_TieLeft(
ValueT *x
, IndexT l, IndexT r
){
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
  IndexT k = l+randIndex(r-l+1); //, distinct=FALSE;
  ValueT t, v;
  SWAP(x[k], x[l], v);  // first argument pivot now in v and x[l]
  ValueT *L = x+l, *R = x+r;

  while (EQ(*(++L), v)) if (L >= R) return;  // explicit stop of for loop

  //if (L < R){
    // Begin Block-Processing
    unsigned char indexL[QUICKSORT_BLOCKSIZE], indexR[QUICKSORT_BLOCKSIZE];
    IndexT iL = 0;
    IndexT iR = 0;
    IndexT sL = 0;
    IndexT sR = 0;
    IndexT num;
    while (R - L + 1 > 2 * QUICKSORT_BLOCKSIZE) {
      if (iR == 0) {
        sR = 0;
        for (k = 0; k < QUICKSORT_BLOCKSIZE; k++) {
          indexR[iR] = k;
          iR += LE((*(R - k)), v);
        }
      }
      if (iL == 0) {
        sL = 0;
        for (k = 0; k < QUICKSORT_BLOCKSIZE; k++) {
          indexL[iL] = k;
          iL += LT(v, L[k]);
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
    // End Block-Processing
    L--;
    R++;

    for (;;){
      while(GT(*(--R), v));                      // sentinel stop guaranteed by pivot at the left
      while (LE(*(++L), v)) if (L >= R) break;  // explicit stop of for loop
      if (L >= R)break;
      SWAP(*L, *R, t);
    }

    SWAP(x[l], *R, t); // the index j from the first loop is guaranteed to be in a legal position
    k = R - x;
    ZacksortB_TieRight(x, l, k-1);
    ZacksortB_TieRight(x, k+1, r);

    //}
}


void ZacksortB_insitu(
  ValueT *x
, IndexT n
)
{
  ZacksortB_TieLeft(x, 0, n-1);
}

void ZacksortB_exsitu(
  ValueT *x
, IndexT n
)
{
	IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  ZacksortB_TieLeft(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
