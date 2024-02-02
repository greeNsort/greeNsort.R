/*
# greeNsort NoCopysort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

// Sedgewick's looping: a strawman with three termination tests per main loop
static void Nocosort_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){ 
  IndexT k=l, i=l, j=m+1; 
  for (; k <= r; k++){ 
    if (i > m) 
    { 
      tar[k] = src[j++];
      continue; 
    } 
    if (j > r) 
    { 
      tar[k] = src[i++]; 
      continue; 
    } 
    tar[k] = LT(src[j], src[i]) ? src[j++] : src[i++];
  }
} 


static void Nocosort_recurse(ValueT *a, ValueT *b, IndexT l, IndexT r){
  if (l<r){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if ((r-l) < INSERTIONSORT_LIMIT){
      for (m=l;m<=r; m++)
        a[m] = b[m];
      Insertionsort_l2r(a, l, r);
      return;
    }
#endif  
    m = l + (r-l)/2;
    Nocosort_recurse(b, a, l  , m);
    Nocosort_recurse(b, a, m+1, r);
    Nocosort_merge(a, b, l, m, r);
  }
}


void Nocosort_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  Nocosort_recurse(x, aux, 0, n-1);
  FREE(aux);
}

void Nocosort_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  Nocosort_recurse(aux, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
