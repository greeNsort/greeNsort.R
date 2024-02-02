/*
# greeNsort Katasort B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include <stdbool.h>
#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static void KatasortB_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT i=l, j=m+1;
#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u = src[i], v = src[j];
#endif
  if (LT(src[r], src[m])){
    // m is the last one, hence j exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(j < r){  // checking always is faster than checking only if j was incremented
      LT(v, u)  ? (tar[l++] = v, v = src[++j]) : (tar[l++] = u, u=src[++i]);
    }
#endif
    while(j <= r){  // checking always is faster than checking only if j was incremented
      tar[l++] = LT(src[j], src[i])  ? src[j++] : src[i++];
    }
    while(i <= m)
      tar[l++] = src[i++];
  }else{
    // r is the last one, hence i exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(i < m){  // checking always is faster than checking only if i was incremented
      LT(v, u)  ? (tar[l++] = v, v = src[++j]) : (tar[l++] = u, u=src[++i]);
    }
#endif
    while(i <= m){  // checking always is faster than checking only if i was incremented
      tar[l++] = LT(src[j], src[i]) ? src[j++] : src[i++];
    }
    while(j <= r)
      tar[l++] = src[j++];
  }
}

static void KatasortB_recurse(ValueT *a, ValueT *b, IndexT l, IndexT r){
  if (l<r){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if ((r-l) < INSERTIONSORT_LIMIT){
      Insertionsort_l2r(a, l, r);
      return ;
    }
#endif
    m = l + (r-l)/2;
    KatasortB_recurse(b, a, l  , m);
    KatasortB_recurse(b, a, m+1, r);
    KatasortB_merge(a, b, l, m, r);
  }
}


void KatasortB_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  KatasortB_recurse(x, aux, 0, n-1);
  FREE(aux);
}

void KatasortB_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  KatasortB_recurse(aux, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
