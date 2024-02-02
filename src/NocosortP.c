/*
# greeNsort NoCopysort (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

// Sedgewicks looping with 3 termination checks
// Sedgewick's looping: a strawman with three termination tests per main loop
static void NocosortP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT *Z=z+(r-l);
  ValueT *j=m+1;
  for (; z <= Z; z++){
    if (l > m)
    {
      *z = *(j++);
      continue;
    }
    if (j > r)
    {
      *z = *(l++);
      continue;
    }
    *z = LT(*j, *l) ? *(j++) : *(l++);
  }
}
/* theoretically branchless but slow
static void NocosortP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT *ij[2];
  ij[0] = l;
  ij[1] = m+1;
  ValueT uv[2];
  uv[0] = *ij[0];
  uv[1] = *ij[1];
  int c;
  while(ij[0] <= m && ij[1] <= r){
    c = LT(uv[1], uv[0]);
    *z++ = uv[c];
    ij[c]++;
    uv[c] = *ij[c];
  }
  while(ij[0] <= m){
    *z++ = *ij[0]++;
  }
  while(ij[1] <= r){
    *z++ = *ij[1]++;
  }
}
*/

static void NocosortP_recurse(ValueT *a, ValueT *b, IndexT n){
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
    NocosortP_recurse(b, a, m);
    NocosortP_recurse(b+m, a+m, n-m);
    NocosortP_merge(a, b, b+m-1, b+n-1);
  }
}

void NocosortP_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying to aux2 can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  NocosortP_recurse(x, aux, n);
  FREE(aux);
  return;
}

void NocosortP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i];  // half of initial copying to aux2 can be avoided, see bMsort
  }
  NocosortP_recurse(aux, aux2, n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
  return;
}

