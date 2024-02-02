/*
# greeNsort Ininsort (implementing the asymmetric serial semi-inplace algorithm of Thomas Kloecker (2008) according to H.W. Lang)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


// Note: we do use pointer arithmetic to implement this mind-boggling algo
// because it is very difficult to implement with l,m,r logic
// This may give a slight performance advantage to Ininsort
// however, Frogsort is still superior
// this has double loop checking and no register tuning for the two elements being compared.

// merges array 'x' and the second half of array 'X' into array 'X'
static void Ininsort_merge(ValueT *X, ValueT *x, IndexT N, IndexT n)
{
    IndexT i, j, k;
    i=0; j=N; k=0;
    // { -- original loop with double termination checks
    while (k<j && j<n){
        if (LT(X[j], x[i]))
            X[k++]=X[j++];
        else
            X[k++]=x[i++];
    }
    while (i<N){
        X[k++]=x[i++];
    }
    return;
}

static void Ininsort_sortInto(ValueT *X, ValueT *x, IndexT n);

// sorts array 'X' of length n in-place using array 'x' as auxiliary array
static void Ininsort_sortInPlace(ValueT *X, ValueT *x, IndexT n)
{
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
#endif
    if (n<=1)
        return;
#if INSERTIONSORT_LIMIT > 0
    Insertionsort_l2r(X, 0, n-1);
    return;
  }
#endif
    IndexT m=(n+1)/2;    // length of first half

    // sort second half of length n-m in-place using x as auxiliary array
    Ininsort_sortInPlace(X+m, x, n-m);

    // sort first half of length m, put result into x
    Ininsort_sortInto(X, x, m);

    Ininsort_merge(X, x, m, n);
}

// sorts array 'X' of length n and puts the result into array 'x'
static void Ininsort_sortInto(ValueT *X, ValueT *x, IndexT n)
{
IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
#endif
    if (n<=0)
        return;
#if INSERTIONSORT_LIMIT > 0
    for (m=0; m<n; m++)
      x[m] = X[m];
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
#endif
    m=(n+1)/2; // // length of first half

    // sort first half of length m in-place using x as auxiliary array
    Ininsort_sortInPlace(X, x, m);

    // sort second half of length n-m, put result into second half of x
    Ininsort_sortInto(X+m, x+m, n-m);

    Ininsort_merge(x, X, m, n);
}

void Ininsort_insitu(ValueT *x, IndexT n)
{
  IndexT m=(n+1)/2;
  ValueT *aux = (ValueT *) MALLOC(m, ValueT);
  Ininsort_sortInPlace(x, aux, n);
  FREE(aux);
  return;
}
void Ininsort_exsitu(ValueT *x, IndexT n)
{
  IndexT m=(n+1)/2;
  ValueT *aux = (ValueT *) MALLOC(n+m, ValueT);
  IndexT i;
  for (i=0; i<n; i++)
    aux[i] = x[i];
  Ininsort_sortInPlace(aux, aux+n, n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
  return;
}
