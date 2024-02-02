/*
# greeNsort Knuthsort A-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


// Following Knuth we need only one termination check per iteration
static void KnuthsortA_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT i=l,j=m+1,k=l;
  ValueT u=src[i], v=src[j];
  if (GT(src[m], v)) for (;;){
    if (LT(v, u)){
      tar[k++] = v; j++;
      if (j > r)  // check either here
        break;
      v = src[j];
    }else{
      tar[k++] = u; i++;
      if (i > m)  // or check here
        break;
      u = src[i];
    }
  }
  while(i <= m)
    tar[k++] = src[i++];
  while(j <= r)
    tar[k++] = src[j++];
}

static void KnuthsortA_recurse(ValueT *a, ValueT *b, IndexT l, IndexT r){
  if (l<r){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if ((r-l) < INSERTIONSORT_LIMIT){
      Insertionsort_l2r(a, l, r);
      return ;
    }
#endif
    m = l + (r-l)/2;
    KnuthsortA_recurse(b, a, l  , m);
    KnuthsortA_recurse(b, a, m+1, r);
    KnuthsortA_merge(a, b, l, m, r);
  }
}

void KnuthsortA_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  KnuthsortA_recurse(x, aux, 0, n-1);
  FREE(aux);
}


void KnuthsortA_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  KnuthsortA_recurse(aux, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
