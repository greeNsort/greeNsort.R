/*
# greeNsort Selectionsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"

void selectionsort(ValueT *x, IndexT l, IndexT r){
  IndexT i,j,k;
  ValueT t,t2;
  for (i= r; i > l; i--){
    j = 0;
    k = j;
    t = x[j];
    while(j < i){
      j++;
      if (GT(x[j], t)){
        t = x[j];
        k = j;
      }
    }
    t2 = x[i];
    x[i] = t;
    x[k] = t2;
  }
}



void Selectionsort_insitu(ValueT *x, IndexT n)
{
  selectionsort(x, 0, n-1);
}

void Selectionsort_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  selectionsort(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
