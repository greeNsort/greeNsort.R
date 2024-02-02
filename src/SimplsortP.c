/*
# greeNsort Simplsort (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


static void SimplsortP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT *j=m+1;
  //ValueT u=*l, v=*j;
  while(l<=m && j<=r){
    *(z++) = LT(*j, *l) ? *(j++) : *(l++);
  }
  while(l <= m)
    *(z++) = *(l++);
  while(j <= r)
    *(z++) = *(j++);
}


static void SimplsortP_recurse(ValueT *a, ValueT *b, IndexT n){
  if (n>1){
    IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if (n <= INSERTIONSORT_LIMIT){
      for (m=0;m<n; m++)
        a[m] = b[m];
      Insertionsort_l2r(a, 0, n-1);
      return ;
    }
#endif
    m = n/2;
    SimplsortP_recurse(b, a, m);
    SimplsortP_recurse(b+m, a+m, n-m);
    SimplsortP_merge(a, b, b+m-1, b+n-1);
  }
}

void SimplsortP_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  //SimplsortP_recurse(x, aux, 0, n-1);
  SimplsortP_recurse(x, aux, n);
  FREE(aux);
}

void SimplsortP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  //SimplsortP_recurse(aux, aux2, 0, n-1);
  SimplsortP_recurse(aux, aux2, n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
