/*
# greeNsort indirect Quicksort2
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#include "RInsertionsort_l2r.h"

static void RQuicksort2(
    ValueT **x
  , IndexT l, IndexT r
){
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
    IndexT j, i = l+randIndex(r-l+1);
    // Rprintf("start: l=%d  i=%d  r=%d\n", l, i, r);
    ValueT *t, *v;
    SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
    i = l-1; j = r;
    for (;;){
      // Rprintf("pre search i=%d  j=%d  v=%f\n", i, j, *v);
      while (LT(x[++i], v)); // sentinel stop of for loop
      while (LT(v, x[--j])){
        if (j <= i)       // explicit stop of for loop
          break;
      }
      if (j <= i)
        break;
      // Rprintf("post search i=%d  j=%d  v=%f\n", i, j, *v);
      SWAP(x[i], x[j], t);
    }
    // Rprintf("stop: l=%d  i=%d  r=%d\n", l, i, r);
    SWAP(x[i], x[r], t);
    RQuicksort2(x, l, i-1);
    RQuicksort2(x, i+1, r);
  }
}

void RQuicksort2_insitu(ValueT *x, IndexT n, int reorder_inplace, PerfT *p)
{
  PerfT p1,p2;
  IndexT i;
  ValueT **z = (ValueT**) MALLOC(n, typeof(ValueT*));
  for (i=0;i<n;i++){
    z[i] = x+i;
  }
  RQuicksort2(z, 0, n-1);

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

void RQuicksort2_exsitu(ValueT *x, IndexT n, PerfT *p)
{
  IndexT i;
  ValueT *y = (ValueT *) MALLOC(n, typeof(ValueT));
  ValueT **z = (ValueT**) MALLOC(n, typeof(ValueT*));
  for (i=0;i<n;i++){
    y[i] = x[i];
    z[i] = y+i;
  }
  RQuicksort2(z, 0, n-1);
  // now copy back stably
  for (i=0;i<n;i++){
    x[i] = *z[i];
  }
  FREE(y);
  FREE(z);
  p->secs = getNewSecs();
  p->size = (sizeof(ValueT*) + sizeof(ValueT)) / ((double)sizeof(ValueT));
}
