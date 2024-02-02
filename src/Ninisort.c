/*
# greeNsort Ninisort (Ininsort improved with Knuths merge)
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
// This may give a slight performance advantage to Ninisort
// however, Frogsort is still superior

// merges array 'x' and the second half of array 'X' into array 'X'
static void Ninisort_merge(ValueT *X, ValueT *x, IndexT N, IndexT n)
{
    IndexT i, j, k;
    i=0; j=N; k=0;
    // { -- original loop with double termination checks
    // while (k<j && j<n){
        // if (LT(X[j], x[i]))
            // X[k++]=X[j++];
        // else
            // X[k++]=x[i++];
    // }
    // } -- original loop with double termination checks
    // { -- J.O. comparable implementation quality with only one termination check per main loop
    if (k<j && j<n){
      ValueT u=x[i], v=X[j];
      for(;;){
        if (LT(v, u)){
            X[k++]=v; j++;
            if (j == n)
              break;
            v = X[j];
        }else{
            X[k++]=u; i++;
            if (k == j)
              break;
            u = x[i];
        }
      }
    }
    // } -- J.O. comparable implementation quality with only one termination check per main loop
    while (i<N){
        X[k++]=x[i++];
    }
    return;
}

static void Ninisort_sortInto(ValueT *X, ValueT *x, IndexT n);

// sorts array 'X' of length n in-place using array 'x' as auxiliary array
static void Ninisort_sortInPlace(ValueT *X, ValueT *x, IndexT n)
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
    Ninisort_sortInPlace(X+m, x, n-m);

    // sort first half of length m, put result into x
    Ninisort_sortInto(X, x, m);

    Ninisort_merge(X, x, m, n);
}

// sorts array 'X' of length n and puts the result into array 'x'
static void Ninisort_sortInto(ValueT *X, ValueT *x, IndexT n)
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
    Ninisort_sortInPlace(X, x, m);

    // sort second half of length n-m, put result into second half of x
    Ninisort_sortInto(X+m, x+m, n-m);

    Ninisort_merge(x, X, m, n);
}

void Ninisort_insitu(ValueT *x, IndexT n)
{
  IndexT m=(n+1)/2;
  ValueT *aux = (ValueT *) MALLOC(m, ValueT);
  Ninisort_sortInPlace(x, aux, n);
  FREE(aux);
  return;
}
void Ninisort_exsitu(ValueT *x, IndexT n)
{
  IndexT m=(n+1)/2;
  ValueT *aux = (ValueT *) MALLOC(n+m, ValueT);
  IndexT i;
  for (i=0; i<n; i++)
    aux[i] = x[i];
  Ninisort_sortInPlace(aux, aux+n, n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
  return;
}
