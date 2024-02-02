/*
# greeNsort direct stabilized Quicksort2
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#include "SInsertionsort_l2r.h"

static void SQuicksort2(ValueT *x, IndexT *o, IndexT l, IndexT r)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
    SInsertionsort_l2r(x, o, l, r);
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
  ValueT t, v;
  IndexT s, w;
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
  SWAP(o[i], o[r], w); // first argument pivot now in v and x[r]
  i = l-1; j = r;
  for (;;){
    while ((++i, LT(x[i], v, o[i], w))); // sentinel stop of for loop
    while ((--j, LT(v, x[j], w, o[j]))){
      if (j <= i)       // explicit stop of for loop
        break;
    }
    if (j <= i)
      break;
    SWAP(x[i], x[j], t);
    SWAP(o[i], o[j], s);
  }
  SWAP(x[i], x[r], t);
  SWAP(o[i], o[r], s);
  SQuicksort2(x, o, l, i-1);
  SQuicksort2(x, o, i+1, r);
}


void SQuicksort2_insitu(ValueT *x, IndexT n, PerfT *p)
{
  IndexT i;
  IndexT *o = (IndexT*) MALLOC(n, typeof(IndexT));
  for (i=0;i<n;i++)
    o[i] = i;
  SQuicksort2(x, o, 0, n-1);
  FREE(o);
  p->secs = getNewSecs();
  p->size = (sizeof(ValueT) + sizeof(IndexT)) / ((double) sizeof(ValueT));
}

void SQuicksort2_exsitu(ValueT *x, IndexT n, PerfT *p)
{
  PerfT p1,p2;
  IndexT i;
  ValueT * y = (ValueT *) MALLOC(n, ValueT);
  IndexT *o = (IndexT*) MALLOC(n, typeof(IndexT));
  for (i=0;i<n;i++){
    y[i] = x[i];
    o[i] = i;
  }
  SQuicksort2(y, o, 0, n-1);
  FREE(o);
  p1.secs = getNewSecs();
  p1.size = (sizeof(ValueT) + sizeof(IndexT)) / ((double)sizeof(ValueT));
  for (i=0;i<n;i++){
    x[i] = y[i];
  }
  FREE(y);
  p2.secs = getNewSecs() - p1.secs;
  p2.size = 1;

  p->secs = p1.secs + p2.secs;
  p->size= (p1.secs*p1.size + p2.secs*p2.size) / p->secs;
}
