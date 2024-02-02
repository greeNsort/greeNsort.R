/*
# greeNsort Bimesort (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#  include "Insertionsort_r2l.h"
#endif


void BimesortP_merge_asc(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT u=*l,v=*r;
  while(l<=m){
    if (LT(v,u)){
      *(z++) = v; v = *(--r);
    }else{
      *(z++) = u; u = *(++l);
    }
  }
  while(m<r){
    *(z++) = *(r--);
  }
}
void BimesortP_merge_asc_rev(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT u=*l, v=*r;
  while(m<r){
    if (LT(v,u)){
      *(z++) = u; u = *(++l);
    }else{
      *(z++) = v; v = *(--r);
    }
  }
  while(l<=m){
    *(z++) = *(l++);
  }
}


void BimesortP_reverse(ValueT *a, ValueT *b, IndexT n);
void BimesortP_recurse(ValueT *a, ValueT *b, IndexT n){
    IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    for (m=0;m<n; m++)
      a[m] = b[m];
    Insertionsort_l2r(a, 0, n-1);
  }else
#else
  if (n>1)
#endif
  {
    m = n/2;
    BimesortP_recurse(b, a, m);
    BimesortP_reverse(b+m, a+m, n-m);
    BimesortP_merge_asc(a, b, b+m-1, b+n-1);
  }
}
void BimesortP_reverse(ValueT *a, ValueT *b, IndexT n){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    for (m=0;m<n; m++)
      a[m] = b[m];
    Insertionsort_r2l(a, 0, n-1);
  }else
#else
  if (n>1)
#endif
  {
    m = n/2;
    BimesortP_reverse(b, a, m);
    BimesortP_recurse(b+m, a+m, n-m);
    BimesortP_merge_asc_rev(a, b, b+m-1, b+n-1);
  }
}


void BimesortP_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  BimesortP_recurse(x, aux, n);
  FREE(aux);
}

void BimesortP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  BimesortP_recurse(aux, aux2, n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}

