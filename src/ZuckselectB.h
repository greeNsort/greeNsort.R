/*
# greeNsort Zuck selection B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_ZuckselectB_h
#define ALREADY_DEFINED_ZuckselectB_h

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static RangeIndexT ZuckselectB_TieLeft(
    ValueT *x
  , IndexT l, IndexT r, IndexT k
);

// pivot partioning placing all pivot ties high
static RangeIndexT ZuckselectB_TieRight(
    ValueT *x
  , IndexT l, IndexT r, IndexT k
){
  RangeIndexT ret;
  IndexT i;
  assert(l<=k);
  assert(k<=r);
#if INSERTIONSORT_LIMIT > 0
  IndexT j;
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    for(i=k-1;i>=l;i--){
      if (NE(x[i],x[k]))
        break;
    }
    for(j=k+1;j<=r;j++){
      if (NE(x[j],x[k]))
        break;
    }
    ret.min = i+1;
    ret.max = j-1;
    return ret;
  }
#else
  if (l >= r){
    ret.min = ret.max = k;
    return ret;
  }
#endif
  ValueT t, v;
  i = l+randIndex(r-l+1);
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
  ValueT *L = x+l, *R = x+r;

  // elegant preloop
  while (EQ(*(--R), v)) if (R <= L) goto final;  // explicit stop of for loop

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

  i = L - x;
  //}

  if(k<i){
    return ZuckselectB_TieRight(x, l, i-1, k);
  }else{
    return ZuckselectB_TieLeft(x, i, r, k);
  }

  final:
  ret.min = l;
  ret.max = r;
  return ret;

}

// pivot partioning placing all pivot ties low
static RangeIndexT ZuckselectB_TieLeft(
    ValueT *x
  , IndexT l, IndexT r, IndexT k
){
  RangeIndexT ret;
  IndexT j;
  assert(l<=k);
  assert(k<=r);
#if INSERTIONSORT_LIMIT > 0
  IndexT i;
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    for(i=k-1;i>=l;i--){
      if (NE(x[i],x[k]))
        break;
    }
    for(j=k+1;j<=r;j++){
      if (NE(x[j],x[k]))
        break;
    }
    ret.min = i+1;
    ret.max = j-1;
    return ret;
  }
#else
  if (l >= r){
    ret.min = ret.max = k;
    return ret;
  }
#endif
  j = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[j], x[l], v);  // first argument pivot now in v and x[l]

  ValueT *L = x+l, *R = x+r;

  while (EQ(*(++L), v)) if (L >= R) goto final;  // explicit stop of for loop

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
  if(k<=j){
    return ZuckselectB_TieRight(x, l, j, k);
  }else{
    return ZuckselectB_TieLeft(x, j+1, r, k);
  }

  final:
  ret.min = l;
  ret.max = r;
  return ret;
}

#endif
