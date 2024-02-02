/*
# greeNsort Bimesort B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#  include "Insertionsort_r2l.h"
#endif

void BimesortB_merge_asc(ValueT *a, ValueT *b, IndexT l, IndexT m, IndexT r){
  IndexT k=l;
#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u=b[l], v=b[r];
  while(l<m){
    LT(v, u) ? (a[k++] = v, v = b[--r]) : (a[k++] = u, u = b[++l]);
  }
#endif
  while(l<=m){
    a[k++] = LT(b[r], b[l]) ? b[r--] : b[l++];
  }
  while(m<r){
    a[k++] = b[r--];
  }
}
void BimesortB_merge_asc_rev(ValueT *a, ValueT *b, IndexT l, IndexT m, IndexT r){
  IndexT k=l;
#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u=b[l], v=b[r];
  while(m<r){
    LT(v, u) ? (a[k++] = u, u = b[++l]) : (a[k++] = v, v = b[--r]);
  }
#else
  while(m<r){
    a[k++] = LT(b[r], b[l]) ? b[l++] : b[r--];
  }
#endif
  while(l<=m){
    a[k++] = b[l++];
  }
}


void BimesortB_reverse(ValueT *a, ValueT *b, IndexT l, IndexT r);
void BimesortB_recurse(ValueT *a, ValueT *b, IndexT l, IndexT r){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if ((r-l) < INSERTIONSORT_LIMIT){
    for (m=l;m<=r; m++)
      a[m] = b[m];
    Insertionsort_l2r(a, l, r);
  }else
#else
    if (l<r)
#endif
{
  m = (l+r)/2;
  BimesortB_recurse(b, a, l, m);
  BimesortB_reverse(b, a, m+1, r);
  BimesortB_merge_asc(a, b, l, m, r);
}
}
void BimesortB_reverse(ValueT *a, ValueT *b, IndexT l, IndexT r){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if ((r-l) < INSERTIONSORT_LIMIT){
    for (m=l;m<=r; m++)
      a[m] = b[m];
    Insertionsort_r2l(a, l, r);
  }else
#else
    if (l<r)
#endif
{
  m = (l+r)/2;
  BimesortB_reverse(b, a, l, m);
  BimesortB_recurse(b, a, m+1, r);
  BimesortB_merge_asc_rev(a, b, l, m, r);
}
}



void BimesortB_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  BimesortB_recurse(x, aux, 0, n-1);
  FREE(aux);
}

void BimesortB_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  BimesortB_recurse(aux, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
