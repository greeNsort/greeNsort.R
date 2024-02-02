/*
# greeNsort indirect stabilized Quicksort2 for variable-sized strings
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#include "WInsertionsort_l2r.h"
#include "strtools.h"

static void WQuicksort2(
    char **x
  , IndexT l, IndexT r
){
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
    IndexT j, i = l+randIndex(r-l+1);
    //Rprintf("l=%d  i=%d  r=%d\n", l, i, r);
    char *t, *v;
    SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
    i = l-1; j = r;
    for (;;){
      while (LT(x[++i], v)); // sentinel stop of for loop
      while (LT(v, x[--j])){
        if (j <= i)       // explicit stop of for loop
          break;
      }
      if (j <= i)
        break;
      SWAP(x[i], x[j], t);
    }
    SWAP(x[i], x[r], t);
    WQuicksort2(x, l, i-1);
    WQuicksort2(x, i+1, r);
  }
}

void WQuicksort2_insitu(char *x, IndexT n, IndexT m, PerfT *p)
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
  WQuicksort2(z, 0, n-1);
  p1.secs = getNewSecs();
  p1.size = (n*sizeof(char*) + m*sizeof(char)) / ((double) m*sizeof(char));

  // reorder using extra buffer is faster and much easier for size varying elements
  char *y = (char *) MALLOC(m, typeof(char));
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

void WQuicksort2_exsitu(char *x, IndexT n, IndexT m, PerfT *p)
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
  WQuicksort2(z, 0, n-1);
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
