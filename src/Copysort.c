/*
# greeNsort Copysort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

// Sedgewick's looping: a strawman with three termination tests per main loop
static void Copysort_merge(ValueT *x, ValueT *aux, IndexT l, IndexT m, IndexT r){
  IndexT k=l, i=l, j=m+1;
  // copy forth
  for (k=l; k <= r; k++)
    aux[k] = x[k];
  // merge back
  // for (k=l; k <= r; k++){
  //   if (i > m)
  //   {
  //     x[k] = aux[j++];
  //     continue;
  //   }
  //   if (j > r)
  //   {
  //     x[k] = aux[i++];
  //     continue;
  //   }
  //   x[k] = LT(aux[j], aux[i]) ? aux[j++] : aux[i++];
  // }
  ValueT u=aux[i], v=aux[j];
  for (k=l;;){
    if (LT(v, u)){
      x[k++] = v; j++;
      if (j > r)  // check either here
        break;
      v = aux[j];
    }else{
      x[k++] = u; i++;
      if (i > m)  // or check here
        break;
      u = aux[i];
    }
  }
  while(i <= m)
    x[k++] = aux[i++];
  while(j <= r)
    x[k++] = aux[j++];
}

static void Copysort_recurse(ValueT *x, ValueT *aux, IndexT l, IndexT r){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
  }else
#else
    if (l<r)
#endif
  {
    m = l + (r-l)/2;
    Copysort_recurse(x, aux, l  , m);
    Copysort_recurse(x, aux, m+1, r);
    Copysort_merge(x, aux, l, m, r);
  }
}


void Copysort_insitu(ValueT *x, IndexT n)
{
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  Copysort_recurse(x, aux, 0, n-1);
  FREE(aux);
}

void Copysort_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  Copysort_recurse(aux, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
