/*
# greeNsort Copysort (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static void CopysortP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT *Z=z;
  ValueT *j=l;
  // copy forth to aux
  while(j <= r)
    *(Z++) = *(j++);
  // switch pointers
  j = l;
  r = z+(r-l);
  m = z+(m-l);
  l = z;
  z = j;
  j=m+1;
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


static void CopysortP_recurse(ValueT *x, ValueT *aux, IndexT n){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, 0, n-1);
  }else
#else
    if (n>1)
#endif
  {
    m = n/2;
    CopysortP_recurse(x, aux, m);
    CopysortP_recurse(x+m, aux+m, n-m);
    CopysortP_merge(aux, x, x+m-1, x+n-1);
  }
}

void CopysortP_insitu(ValueT *x, IndexT n)
{
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  CopysortP_recurse(x, aux, n);
  FREE(aux);
  return;
}

void CopysortP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux[i] = x[i];  // half of initial copying to aux2 can be avoided, see bMsort
  }
  CopysortP_recurse(aux, aux2, n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
  return;
}

