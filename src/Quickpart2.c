/*
# greeNsort partial Quicksort2 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static void Quickpart2(ValueT *x, IndexT l, IndexT r, IndexT *p)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
  IndexT j,i = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[i], x[r], v); // now pivot is in v and x[r]
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
  if (i < p[0]){
    Quickpart2(x, i+1, r, p);
  }else if(i > p[1]){
    Quickpart2(x, l, i-1, p);
  }else{
    Quickpart2(x, l, i-1, p);
    Quickpart2(x, i+1, r, p);
  }
}

void Quickpart2_insitu(ValueT *x, IndexT n, IndexT *partial){
  Quickpart2(x, 0, n-1, partial);
}

void Quickpart2_exsitu(ValueT *x, IndexT n, IndexT *partial){
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0; i<n; i++)
    aux[i] = x[i];
  Quickpart2(aux, 0, n-1, partial);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
