/*
# greeNsort Katasort B-tuned (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include <stdbool.h>
#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

/*
static void KatasortBP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT *j=m+1;
#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u = *l, v = *j;
#endif
  if (LT(*r, *m)){
    // m is the last one, hence j exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(j < r){  // checking always is faster than checking only if j was incremented
      LT(v, u)  ? (*(z++) = v, v = *(++j)) : (*(z++) = u, u = *(++l));
    }
#endif
    while(j <= r){  // checking always is faster than checking only if j was incremented
      *(z++) = LT(*j, *l)  ? *(j++) : *(l++);
    }
    while(l <= m)
      *(z++) = *(l++);
  }else{
    // r is the last one, hence i exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(l < m){  // checking always is faster than checking only if i was incremented
      LT(v, u)  ? (*(z++) = v, v = *(++j)) : (*(z++) = u, u = *(++l));
    }
#endif
    while(l <= m){  // checking always is faster than checking only if i was incremented
      *(z++) = LT(*j, *l)  ? *(j++) : *(l++);
    }
    while(j <= r)
      *(z++) = *(j++);
  }
}
 */

static void KatasortBP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT *j=m+1;
#ifdef MERGESORT_BTUNE_WITH_REGISTER
  ValueT u = *l, v = *j;
#endif
  if (LT(*r, *m)){
    // m is the last one, hence j exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(j < r){  // checking always is faster than checking only if j was incremented
      LT(v, u)  ? (*(z++) = v, v = *(++j)) : (*(z++) = u, u = *(++l));
    }
#endif
    while(j <= r){  // checking always is faster than checking only if j was incremented
      *(z++) = LT(*j, *l)  ? *(j++) : *(l++);
    }
    while(l <= m)
      *(z++) = *(l++);
  }else{
    // r is the last one, hence i exhausts first
#ifdef MERGESORT_BTUNE_WITH_REGISTER
    while(l < m){  // checking always is faster than checking only if i was incremented
      LT(v, u)  ? (*(z++) = v, v = *(++j)) : (*(z++) = u, u = *(++l));
    }
#endif
    while(l <= m){  // checking always is faster than checking only if i was incremented
      *(z++) = LT(*j, *l)  ? *(j++) : *(l++);
    }
    while(j <= r)
      *(z++) = *(j++);
  }
}


static void KatasortBP_recurse(ValueT *a, ValueT *b, IndexT n){
  if (n>1){
    IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if (n <= INSERTIONSORT_LIMIT){
      Insertionsort_l2r(a, 0, n-1);
      return ;
    }
#endif
    m = n/2;
    KatasortBP_recurse(b, a, m);
    KatasortBP_recurse(b+m, a+m, n-m);
    KatasortBP_merge(a, b, b+m-1, b+n-1);
  }
}



void KatasortBP_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  KatasortBP_recurse(x, aux, n);
  FREE(aux);
}

void KatasortBP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  KatasortBP_recurse(aux, aux2, n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
