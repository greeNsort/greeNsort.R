/*
# greeNsort Bimesort B-tuned (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#  include "Insertionsort_r2l.h"
#endif

void BimesortBP_merge_asc(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
#ifdef MERGESORT_BTUNE_WITH_REGISTER
ValueT u=*l,v=*r;
  while(l<m){
    LT(v,u) ? (*(z++) = v, v = *(--r)) : (*(z++) = u, u = *(++l));
  }
#endif
  while(l<=m){
    *(z++) = LT(*r,*l) ? *(r--) : *(l++);
  }
  while(m<r){
    *(z++) = *(r--);
  }
}
void BimesortBP_merge_asc_rev(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u=*l,v=*r;
  while(m<r){
    LT(v, u) ? (*(z++) = u, u = *(++l)) : (*(z++) = v, v = *(--r));
  }
#else
  while(m<r){
    *(z++) = LT(*r, *l) ? *(l++) : *(r--);
  }
#endif
  while(l<=m){
    *(z++) = *(l++);
  }
}


void BimesortBP_reverse(ValueT *a, ValueT *b, IndexT n);
void BimesortBP_recurse(ValueT *a, ValueT *b, IndexT n){
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
  BimesortBP_recurse(b, a, m);
  BimesortBP_reverse(b+m, a+m, n-m);
  BimesortBP_merge_asc(a, b, b+m-1, b+n-1);
}
}
void BimesortBP_reverse(ValueT *a, ValueT *b, IndexT n){
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
  BimesortBP_reverse(b, a, m);
  BimesortBP_recurse(b+m, a+m, n-m);
  BimesortBP_merge_asc_rev(a, b, b+m-1, b+n-1);
}
}


void BimesortBP_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  BimesortBP_recurse(x, aux, n);
  FREE(aux);
}

void BimesortBP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  BimesortBP_recurse(aux, aux2, n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
