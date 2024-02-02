/*
# greeNsort Lomutosort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

/* this is the version in Beautiful Code (without sentinel)
static void Lomutosort_no_sentinel(ValueT *x, IndexT l, IndexT r)
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
  ValueT t, v;
  IndexT j, i = l+randIndex(r-l+1);
  SWAP(x[i], x[l], v); // first argument pivot now in v and x[l]
  j = l;
  for (i = l+1; i <= r; i++)
    if (LT(x[i], v)){
      ++j; SWAP(x[j], x[i], t);
    }
  SWAP(x[l], x[j], t);
  Lomutosort_no_sentinel(x, l, j-1);
  Lomutosort_no_sentinel(x, j+1, r);
}
*/

/* This is Sedgewicks sentinel-version of the above  */
static void Lomutosort_sentinel(ValueT *x, IndexT l, IndexT r)
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
  ValueT t, v;
  IndexT j, i = l+randIndex(r-l+1);
  SWAP(x[i], x[l], v); // first argument pivot now in v and x[l]
  j = i = r + 1;
  do {
    while (--i, LT(x[i], v));
    --j; SWAP(x[j], x[i], t);
  } while(i != l);
  Lomutosort_sentinel(x, l, j-1);
  Lomutosort_sentinel(x, j+1, r);
}


void Lomutosort_insitu(ValueT *x, IndexT n)
{
  Lomutosort_sentinel(x, 0, n-1);
}

void Lomutosort_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Lomutosort_sentinel(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
