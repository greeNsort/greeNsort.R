/*
# greeNsort Quicksort3 (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


static void Quicksort3P(ValueT *x, IndexT n)
{
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
#else
  if (n < 2)
    return;
#endif
  IndexT k, p, q, j, i = randIndex(n);
  ValueT t, v;
  j = q = n-1;
  SWAP(x[i], x[j], v);  // first argument pivot now in v and x[j]
  i = p = -1;
  for (;;){
    while (LT(x[++i],v)); // sentinel stop of for loop
    while (LT(v,x[--j])) if (j <= i) break; // explicit stop of for loop
    if (j <= i) break;
    SWAP(x[i], x[j], t);
    if (EQ(x[i],v)) {++p; SWAP(x[p], x[i],t);}
    if (EQ(v,x[j])) {--q; SWAP(x[j], x[q], t);}
  }
  SWAP(x[i], x[n-1], t);
  j = i-1; i = i+1;
  for (k = 0; k < p; k++, j--) SWAP(x[k], x[j], t);
  for (k = n-2; k > q; k--, i++) SWAP(x[i], x[k], t);
  Quicksort3P(x  , j+1);
  Quicksort3P(x+i, n-i);
}

void Quicksort3P_insitu(ValueT *x, IndexT n)
{
  Quicksort3P(x, n);
}

void Quicksort3P_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Quicksort3P(aux, n);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
