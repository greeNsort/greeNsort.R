/*
# greeNsort Chunksort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#  include "Insertionsort_desc_l2r.h"
#endif


static void Chunksort_asc(ValueT *x, IndexT l, IndexT r)
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
  IndexT j, i = l+randIndex(r-l+1);
  //Rprintf("l=%d  i=%d  r=%d\n", l, i, r);
  ValueT t, v;
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
  Chunksort_asc(x, l, i-1);
  Chunksort_asc(x, i+1, r);
}

static void Chunksort_desc(ValueT *x, IndexT l, IndexT r)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_desc_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
  IndexT j, i = l+randIndex(r-l+1);
  //Rprintf("l=%d  i=%d  r=%d\n", l, i, r);
  ValueT t, v;
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
  i = l-1; j = r;
  for (;;){
    while (GT(x[++i], v)); // sentinel stop of for loop
    while (GT(v, x[--j])){
      if (j <= i)       // explicit stop of for loop
        break;
    }
    if (j <= i)
      break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[i], x[r], t);
  Chunksort_desc(x, l, i-1);
  Chunksort_desc(x, i+1, r);
}


void Chunksort_insitu(ValueT *x, IndexT n, IndexT b, int d)
{
  IndexT l=0,r=b;
  if (d>0){
    while (r<n){
      Chunksort_asc(x, l, r);
      l+=b;
      r+=b;
    }
    if (l<(n-1))
      Chunksort_asc(x, l, n-1);
  }else if (d<0){
    while (r<n){
      Chunksort_desc(x, l, r);
      l+=b;
      r+=b;
    }
    if (l<(n-1))
      Chunksort_desc(x, l, n-1);
  }else{
    d = 1;
    while (r<n){
      if (d>0)
        Chunksort_asc(x, l, r);
      else
        Chunksort_desc(x, l, r);
      d = 1 - d;
      l+=b;
      r+=b;
    }
    if (l<(n-1)){
      if (d>0)
        Chunksort_asc(x, l, n-1);
      else
        Chunksort_desc(x, l, n-1);
    }
  }
}

void Chunksort_exsitu(ValueT *x, IndexT n, IndexT b, int d)
{
	IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Chunksort_insitu(aux, 0, n-1, d);
  for (i=0;i<n;i++)
    x[i] = aux[i];
}
