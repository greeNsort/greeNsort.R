/*
# greeNsort Knuthsort (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

// Following Knuth we need only one termination check per iteration
static void KnuthsortP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT *j=m+1;
  ValueT u=*l, v=*j;
  for (;;){
    if (LT(v, u)){
      *(z++) = v; j++;
      if (j > r)  // check either here
        break;
      v = *j;
    }else{
      *(z++) = u; l++;
      if (l > m)  // or check here
        break;
      u= *l;
    }
  }
  while(l <= m)
    *(z++) = *(l++);
  while(j <= r)
    *(z++) = *(j++);
}

static void KnuthsortP_recurse(ValueT *a, ValueT *b, IndexT n){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    Insertionsort_l2r(a, 0, n-1);
  }else
#else
  if (n>1)
#endif
  {
    m = n/2;
    KnuthsortP_recurse(b, a, m);
    KnuthsortP_recurse(b+m, a+m, n-m);
    KnuthsortP_merge(a, b, b+m-1, b+n-1);
  }
}

void KnuthsortP_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  //KnuthsortP_recurse(x, aux, 0, n-1);
  KnuthsortP_recurse(x, aux, n);
  FREE(aux);
}

void KnuthsortP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  KnuthsortP_recurse(aux, aux2, n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
