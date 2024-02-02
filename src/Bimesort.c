/*
# greeNsort Bimesort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#  include "Insertionsort_r2l.h"
#endif

void Bimesort_merge_asc(ValueT *a, ValueT *b, IndexT l, IndexT m, IndexT r){
  IndexT k=l;
  ValueT u=b[l], v=b[r];
  while(l<=m){
    if (LT(v,u)){
      a[k++] = v; v = b[--r];
    }else{
      a[k++] = u; u = b[++l];
    }
  }
  while(m<r){
    a[k++] = b[r--];
  }
}
void Bimesort_merge_asc_rev(ValueT *a, ValueT *b, IndexT l, IndexT m, IndexT r){
  IndexT k=l;
  ValueT u=b[l], v=b[r];
  while(m<r){
    if (LT(v,u)){
      a[k++] = u; u = b[++l];
    }else{
      a[k++] = v; v = b[--r];
    }
  }
  while(l<=m){
    a[k++] = b[l++];
  }
}


void Bimesort_reverse(ValueT *a, ValueT *b, IndexT l, IndexT r);
void Bimesort_recurse(ValueT *a, ValueT *b, IndexT l, IndexT r){
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
    Bimesort_recurse(b, a, l, m);
    Bimesort_reverse(b, a, m+1, r);
    Bimesort_merge_asc(a, b, l, m, r);
  }
}
void Bimesort_reverse(ValueT *a, ValueT *b, IndexT l, IndexT r){
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
    Bimesort_reverse(b, a, l, m);
    Bimesort_recurse(b, a, m+1, r);
    Bimesort_merge_asc_rev(a, b, l, m, r);
  }
}



/* Segdewick 1990: not mentioned that not stable
void CopyBimesort(ValueT a[], ValueT b[], IndexT l, IndexT r){
  IndexT i,j,k,m;
#if INSERTIONSORT_LIMIT > 0
  if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(a, l, r);
  }else
#else
    if (l<r)
#endif
  {
    m = (r+l) /2;
    CopyBimesort(a, b, l, m);
    CopyBimesort(a, b, m+1, r);
    // Rprintf("l=%d r=%d\n", l, r);
    // for (i = l; i<=r; i++)
    //   Rprintf("x[%d]=%f\n", i, a[i]);
    for (i = m+1; i > l; i--) b[i-1] = a[i-1];
    for (j = m; j < r; j++) b[r+m-j] = a[j+1];
    //for (k = l; k <= r; k++) a[k] = LT(b[i], b[j]) ? b[i++] : b[j--];
    Bimesort_merge_asc(a,b,l,m,r);
  }
}

 void Bimesort_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  CopyBimesort(x, aux, 0, n-1);
  FREE(aux);
}
*/

void Bimesort_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  Bimesort_recurse(x, aux, 0, n-1);
  FREE(aux);
}

void Bimesort_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  Bimesort_recurse(aux, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
