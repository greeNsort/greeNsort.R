/*
# greeNsort approximate median
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_approxMedian_exsitu_h
#define ALREADY_DEFINED_approxMedian_exsitu_h

#include "algo.h"
#include "Insertionsort_l2r.h"

#if INSERTIONSORT_LIMIT > 0
static ValueT approxMedian_exsitu_ret(
  ValueT *x
, IndexT n
){
  ValueT aux[INSERTIONSORT_LIMIT];
  IndexT i;
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Insertionsort_l2r(aux, 0, n-1);
  return(aux[n/2]);
}
#endif


// leaves x unchanged
static ValueT approxMedian_exsitu_rec(
  ValueT *x
, ValueT *buf // at least n/5+n%5 elements
, ValueT *aux // at least 5 elements
, IndexT n
){
  IndexT i,j,k,l=0,r=n-1;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    return approxMedian_exsitu_ret(x, n);
  }
#endif
  for(i=l-1,j=l+4; j<=r; j+=5){
    for (k=0;k<5;k++)
      aux[k] = x[j-4+k];
    Insertionsort_l2r(aux, 0, 4);
    buf[++i] = aux[2];
  }
  j-=4;
  if (j<=r){ // if not reached right end
    for (k=0;j<=r;k++,j++)
      aux[k] = x[j];
    Insertionsort_l2r(aux, 0, k-1);
    buf[++i] = aux[k/2];
  }
  if (i <= (l+1))
    return buf[0];
  else
    return approxMedian_exsitu_rec(buf, buf, aux, i+1);
}

#endif
