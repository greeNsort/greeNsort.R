/*
# greeNsort indirect Zacksort for variable-sized strings B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#include "UInsertionsort_l2r.h"
#include "strtools.h"

static void UZacksortB_TieLeft(
    char **x
  , IndexT l, IndexT r
);

// pivot partioning placing all pivot ties high
static void UZacksortB_TieRight(
    char **x
  , IndexT l, IndexT r
){
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
    UInsertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
#ifdef SCANNED_RANGE
  scanned_range += r-l+1;
#endif
  IndexT k = l+randIndex(r-l+1), distinct=FALSE;
  char *t, *v;
  SWAP(x[k], x[r], v); // first argument pivot now in v and x[r]
  char **L = x+l, **R = x+r;
  while(LT(*L, v)) ++L; // sentinel stop guaranteed by pivot at the right
  // during the first pointer movement we don't need to check all-ties (moving implies distinct=TRUE at first element)
  if (L > x+l || NE(*L, v))
    distinct=TRUE;
  // here we ignore that we might know (distinct=TRUE) -- usually the following loop is traversed few times
  while (GE(*(--R), v)) {
    if (NE(*R, v)) {
      distinct=TRUE;
      if(L<R){
        while (GE(*(--R), v)) if (R <= L) break;  // explicit stop of for loop
      }
      break;
    } else if (R <= L) break; // explicit stop of for loop
  };
  if (L < R){
    distinct=TRUE;
    SWAP(*L, *R, t);
    L++; R--;
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
  }
  if (distinct){  // not all value tie with pivot
    SWAP(*L, x[r], t); // the index i from the first loop is guaranteed to be in a legal position
    k = L - x;
    UZacksortB_TieLeft(x, l, k-1);
    UZacksortB_TieLeft(x, k+1, r);
  }
}

// pivot partioning placing all pivot ties low
static void UZacksortB_TieLeft(
    char **x
  , IndexT l, IndexT r
){
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
    UInsertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
#ifdef SCANNED_RANGE
  scanned_range += r-l+1;
#endif
  IndexT k = l+randIndex(r-l+1), distinct=FALSE;
  char *t, *v;
  SWAP(x[k], x[l], v);  // first argument pivot now in v and x[l]
  char **L = x+l, **R = x+r;
  while(GT(*R, v)) --R;                    // sentinel stop guaranteed by pivot at the left
  // during the first pointer movement we don't need to check all-ties (moving implies distinct=TRUE at first element)
  if (R < x+r || NE(*R, v))
    distinct=TRUE;
  // here we ignore that we might know (distinct=TRUE) -- usually the following loop is traversed few times
  while (LE(*(++L), v)) {
    if (NE(*L, v)) {
      distinct=TRUE;
      if(L<R){
        while (LE(*(++L), v)) if (R <= L) break;  // explicit stop of for loop
      }
      break;
    } else if (R <= L) break; // explicit stop of for loop
  };
  if (L < R){
    distinct=TRUE;
    SWAP(*L, *R, t);

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
  }
  if (distinct){  // not all value tie with pivot
    SWAP(x[l], *R, t); // the index j from the first loop is guaranteed to be in a legal position
    k = R - x;
    UZacksortB_TieRight(x, l, k-1);
    UZacksortB_TieRight(x, k+1, r);
  }
}


void UZacksortB_insitu(char *x, IndexT n, IndexT m, PerfT *p)
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
  UZacksortB_TieLeft(z, 0, n-1);
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

void UZacksortB_exsitu(char *x, IndexT n, IndexT m, PerfT *p)
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
  UZacksortB_TieLeft(z, 0, n-1);
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
