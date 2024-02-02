/*
# greeNsort partial Zucksort B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static void ZuckpartB_TieLeft(
    ValueT *x
  , IndexT l, IndexT r, IndexT *p
);

// pivot partioning placing all pivot ties high
static void ZuckpartB_TieRight(
    ValueT *x
  , IndexT l, IndexT r, IndexT *p
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
  IndexT i = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]

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
      for (i = 0; i < QUICKSORT_BLOCKSIZE; i++) {
        indexL[iL] = i;
        iL += GE(L[i], v);
      }
    }
    if (iR == 0) {
      sR = 0;
      for (i = 0; i < QUICKSORT_BLOCKSIZE; i++) {
        indexR[iR] = i;
        iR += GT(v, (*(R - i)));
      }
    }
    num = MIN(iL, iR);
    if (num != 0) {
      t = L[indexL[sL]];
      L[+indexL[sL]] = R[-indexR[sR]];
      for (i = 1; i < num; i++) {
        R[-indexR[sR + i - 1]] = L[+indexL[sL + i]];
        L[+indexL[sL + i    ]] = R[-indexR[sR + i]];
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
  i = L - x;

  if (i < p[0]){
    ZuckpartB_TieLeft(x, i+1, r, p);
  }else if(i > p[1]){
    ZuckpartB_TieRight(x, l, i-1, p);
  }else{
    ZuckpartB_TieRight(x, l, i-1, p);
    ZuckpartB_TieLeft(x, i+1, r, p);
  }
}

// pivot partioning placing all pivot ties low
static void ZuckpartB_TieLeft(
    ValueT *x
  , IndexT l, IndexT r, IndexT *p
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
  IndexT j = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[j], x[l], v);  // first argument pivot now in v and x[l]
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
      for (j = 0; j < QUICKSORT_BLOCKSIZE; j++) {
        indexR[iR] = j;
        iR += LE((*(R - j)), v);
      }
    }
    if (iL == 0) {
      sL = 0;
      for (j = 0; j < QUICKSORT_BLOCKSIZE; j++) {
        indexL[iL] = j;
        iL += LT(v, L[j]);
      }
    }
    num = MIN(iL, iR);
    if (num != 0) {
      t = L[indexL[sL]];
      L[+indexL[sL]] = R[-indexR[sR]];
      for (j = 1; j < num; j++) {
        R[-indexR[sR + j - 1]] = L[+indexL[sL + j]];
        L[+indexL[sL + j    ]] = R[-indexR[sR + j]];
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

  j = R - x;
  SWAP(x[l], *R, t); // the index j from the first loop is guaranteed to be in a legal position

  if (j < p[0]){
    ZuckpartB_TieLeft(x, j+1, r, p);
  }else if(j > p[1]){
    ZuckpartB_TieRight(x, l, j-1, p);
  }else{
    ZuckpartB_TieRight(x, l, j-1, p);
    ZuckpartB_TieLeft(x, j+1, r, p);
  }
}

void ZuckpartB_insitu(ValueT *x, IndexT n, IndexT *partial){
  ZuckpartB_TieLeft(x, 0, n-1, partial);
  return;
}

void ZuckpartB_exsitu(ValueT *x, IndexT n, IndexT *partial){
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0; i<n; i++)
    aux[i] = x[i];
  ZuckpartB_TieLeft(aux, 0, n-1, partial);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
  return;
}
