/*
# greeNsort indirect Zacksort for variable-sized strings
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#include "UInsertionsort_l2r.h"
#include "strtools.h"

static void UZacksort_TieLeft(
    char **x
  , IndexT l, IndexT r
);

// pivot partioning placing all pivot ties high
static void UZacksort_TieRight(
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
  IndexT j, i = l+randIndex(r-l+1), distinct=FALSE;
  char *t, *v;
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
  i = l; j = r;
  while(LT(x[i], v)) ++i; // sentinel stop guaranteed by pivot at the right
  // during the first pointer movement we don't need to check all-ties (moving implies distinct=TRUE at first element)
  if (i > l || NE(x[i], v))
    distinct=TRUE;
  // here we ignore that we might know (distinct=TRUE) -- usually the following loop is traversed few times
  while (GE(x[--j], v)) {
    if (NE(x[j], v)) {
      distinct=TRUE;
      if(i<j){
        while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
      }
      break;
    } else if (j <= i) break; // explicit stop of for loop
  };
  if (i < j){
    distinct=TRUE;
    SWAP(x[i], x[j], t);
    for (;;){
      while(LT(x[++i], v));                      // sentinel stop guaranteed by pivot at the right
      while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
      if (j <= i)break;
      SWAP(x[i], x[j], t);
    }
  }
  if (distinct){  // not all value tie with pivot
    SWAP(x[i], x[r], t); // the index i from the first loop is guaranteed to be in a legal position
    UZacksort_TieLeft(x, l, i-1);
    UZacksort_TieLeft(x, i+1, r);
  }
}

// pivot partioning placing all pivot ties low
static void UZacksort_TieLeft(
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
  IndexT i, j = l+randIndex(r-l+1), distinct=FALSE;
  char *t, *v;
  SWAP(x[j], x[l], v);  // first argument pivot now in v and x[l]
  i = l; j = r;
  while(GT(x[j], v)) --j;                    // sentinel stop guaranteed by pivot at the left
  // during the first pointer movement we don't need to check all-ties (moving implies distinct=TRUE at first element)
  if (j < r || NE(x[j], v))
    distinct=TRUE;
  // here we ignore that we might know (distinct=TRUE) -- usually the following loop is traversed few times
  while (LE(x[++i], v)) {
    if (NE(x[i], v)) {
      distinct=TRUE;
      if(i<j){
        while (LE(x[++i], v)) if (j <= i) break;  // explicit stop of for loop
      }
      break;
    } else if (j <= i) break; // explicit stop of for loop
  };
  if (i < j){
    distinct=TRUE;
    SWAP(x[i], x[j], t);
    for (;;){
      while(GT(x[--j], v));                      // sentinel stop guaranteed by pivot at the left
      while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
      if (i >= j)break;
      SWAP(x[i], x[j], t);
    }
  }
  if (distinct){  // not all value tie with pivot
    SWAP(x[l], x[j], t); // the index j from the first loop is guaranteed to be in a legal position
    UZacksort_TieRight(x, l, j-1);
    UZacksort_TieRight(x, j+1, r);
  }
}


void UZacksort_insitu(char *x, IndexT n, IndexT m, PerfT *p)
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
  UZacksort_TieLeft(z, 0, n-1);
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

void UZacksort_exsitu(char *x, IndexT n, IndexT m, PerfT *p)
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
  UZacksort_TieLeft(z, 0, n-1);
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
