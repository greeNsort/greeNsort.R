/*
# greeNsort Quicksort2 (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

/* even more pointer, however, not faster
static void Quicksort2P(ValueT *x, ValueT *r)
 {
  IndexT n = r-x+1;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
  scanned_range += n;
#endif
  Insertionsort_l2r(x, 0, n-1);
  return;
 }
#else
  if (n < 2)
  return;
#endif
#ifdef SCANNED_RANGE
  scanned_range += n;
#endif
  ValueT *j, *i = x+randIndex(n);
  ValueT t, v;
  j = r;
  SWAP(*i, *j, v); // first argument pivot now in v and x[j]
  i = x-1;
  for (;;){
    while (LT(*(++i), v)); // sentinel stop of for loop
    while (LT(v, *(--j)))
      if (j <= i)       // explicit stop of for loop
        break;
    if (j <= i)
    break;
 SWAP(*i, *j, t);
 }
 SWAP(*i, *r, t);
 Quicksort2P(x, i-1);
 Quicksort2P(i+1, r);
 }


void Quicksort2P_insitu(ValueT *x, IndexT n)
{
  Quicksort2P(x, x+(n-1));
}

void Quicksort2P_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Quicksort2P(aux, aux+(n-1));
  for (i=0;i<n;i++)
    x[i] = aux[i];
}
*/

static void Quicksort2P(ValueT *x, IndexT n)
{
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += n;
#endif
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
#else
  if (n < 2)
    return;
#endif
#ifdef SCANNED_RANGE
    scanned_range += n;
#endif
  IndexT j, i = randIndex(n);
  ValueT t, v;
  j = n-1;
  SWAP(x[i], x[j], v); // first argument pivot now in v and x[j]
  i = -1;
  for (;;){
    while (LT(x[++i], v)); // sentinel stop of for loop
    while (LT(v, x[--j]))
      if (j <= i)       // explicit stop of for loop
        break;
    if (j <= i)
      break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[i], x[n-1], t);
  Quicksort2P(x, i);
  Quicksort2P(x+i+1, n-i-1);
}



void Quicksort2P_insitu(ValueT *x, IndexT n)
{
  Quicksort2P(x, n);
}

void Quicksort2P_exsitu(ValueT *x, IndexT n)
{
	IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Quicksort2P(aux, n);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
