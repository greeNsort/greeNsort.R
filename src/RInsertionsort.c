/*
# greeNsort indirect Insertionsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "RInsertionsort_l2r.h"

void RInsertionsort_insitu(
  ValueT *x
, IndexT n
, PerfT *p
)
{
  PerfT p1,p2;
  IndexT i;
  ValueT **z = (ValueT**) MALLOC(n, typeof(ValueT*));
  for (i=0;i<n;i++){
    z[i] = x+i;
  }
  RInsertionsort_l2r(z, 0, n-1);

  p1.secs = getNewSecs();
  p1.size = (sizeof(ValueT*) + sizeof(ValueT)) / sizeof(ValueT);

  // reorder inplace
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

  // now copy back stably
  // ValueT *y = (ValueT *) MALLOC(n, typeof(ValueT));
  // for (i=0;i<n;i++){
  //   y[i] = *z[i];
  // }
  // for (i=0;i<n;i++){
  //   x[i] = y[i];
  // }
  // FREE(y);

  FREE(z);
  p2.secs = getNewSecs() - p1.secs;
  p2.size = (sizeof(ValueT*) + sizeof(ValueT)) / ((double)sizeof(ValueT));

  p->secs = p1.secs + p2.secs;
  p->size= (p1.secs*p1.size + p2.secs*p2.size) / p->secs;
}


void RInsertionsort_exsitu(
    ValueT *x
  , IndexT n
  , PerfT *p
)
{
  IndexT i;
  ValueT *y = (ValueT *) MALLOC(n, typeof(ValueT));
  ValueT **z = (ValueT**) MALLOC(n, typeof(ValueT*));
  for (i=0;i<n;i++){
    y[i] = x[i];
    z[i] = y+i;
  }
  RInsertionsort_l2r(z, 0, n-1);
  // now copy back stably
  for (i=0;i<n;i++){
    x[i] = *z[i];
  }
  FREE(y);
  FREE(z);
  p->secs = getNewSecs();
  p->size = (sizeof(ValueT*) + sizeof(ValueT)) / ((double)sizeof(ValueT));
}





