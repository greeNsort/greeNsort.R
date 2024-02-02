/*
# greeNsort Katasort A-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

//#define KMERGE_DEBUG 1
#undef KMERGE_DEBUG
#include "kmerge.h"

//#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


static void KatasortA_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT k=l, i=l, j=m+1;
  ValueT u=src[i], v=src[j];
  if (GT(src[m], v)){
      if (LT(src[r], src[m])){
        // m is the last one, hence j exhausts first
        for(;;){
          if (LT(v, u)){
            tar[k++] = v;
            if (j == r)
              break;
            v = src[++j];
          }else{
            tar[k++] = u;
            u = src[++i];
          }
        }
        while(i <= m)
          tar[k++] = src[i++];
      }else{
        // r is the last one, hence i exhausts first
        for(;;){
          if (LT(src[j], src[i])){
            tar[k++] = v;
            v= src[++j];
          }else{
            tar[k++] = u;
            if (i == m)
              break;
            u=src[++i];
          }
        }
        while(j <= r)
          tar[k++] = src[j++];
    }
  }else{
    while(i <= m)
      tar[k++] = src[i++];
    while(j <= r)
      tar[k++] = src[j++];
  }

}


static void KatasortA_recurse(ValueT *a, ValueT *b, IndexT l, IndexT r){
  if (l<r){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if ((r-l) < INSERTIONSORT_LIMIT){
      Insertionsort_l2r(a, l, r);
      return ;
    }
#endif
    m = l + (r-l)/2;
    KatasortA_recurse(b, a, l  , m);
    KatasortA_recurse(b, a, m+1, r);
    KatasortA_merge(a, b, l, m, r);
  }
}


void KatasortA_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  KatasortA_recurse(x, aux, 0, n-1);
  FREE(aux);
}

void KatasortA_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  KatasortA_recurse(aux, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
