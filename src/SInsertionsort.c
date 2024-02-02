/*
# greeNsort direct stabilized Insertionsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "SInsertionsort_l2r.h"

void SInsertionsort_insitu(
  ValueT *x
, IndexT n
, PerfT *p
)
{
  IndexT i;
  IndexT *o = (IndexT*) MALLOC(n, typeof(IndexT*));
  for (i=0;i<n;i++){
    o[i] = i;
  }
  SInsertionsort_l2r(x, o, 0, n-1);
  FREE(o);
  p->secs = getNewSecs();
  p->size = (sizeof(ValueT) + sizeof(IndexT)) / ((double) sizeof(ValueT));
}


void SInsertionsort_exsitu(
    ValueT *x
  , IndexT n
  , PerfT *p
)
{
  PerfT p1,p2;
  IndexT i;
  ValueT *y = (ValueT *) MALLOC(n, typeof(ValueT));
  IndexT *o = (IndexT*) MALLOC(n, typeof(IndexT));
  for (i=0;i<n;i++){
    y[i] = x[i];
    o[i] = i;
  }
  SInsertionsort_l2r(y, o, 0, n-1);
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

