/*
# greeNsort Knuthsort A-tuned (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include <stdbool.h>
#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

#define OMIT_CORRELATED_PRESORTING
//#define SKIP_CORRELATED_PRESORTING

// Following Knuth we need only one termination check per iteration
static void KnuthsortAP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT *j=m+1;
  ValueT v=*j;
#ifdef OMIT_CORRELATED_PRESORTING
    if (GT(*m, v)){
#endif
#ifdef SKIP_CORRELATED_PRESORTING
      ValueT *i=l+(m-l)/2;
      while (i<m && GE(v,*i)){
        while (l<=i)
          *(z++) = *(l++);
        i=l+(m-l)/2;
      }
#endif
      ValueT u=*l;
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
#ifdef OMIT_CORRELATED_PRESORTING
    }
#endif
  while(l <= m)
    *(z++) = *(l++);
  while(j <= r)
    *(z++) = *(j++);
}

/*
// Following Knuth we need only one termination check per iteration
static void KnuthsortAP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT *j=m+1;
  ValueT u=*l, v=*j;
  if (GT(*m, v)) for (;;){
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
 */


static void KnuthsortAP_recurse(ValueT *a, ValueT *b, IndexT n){
  if (n>1){
    IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if (n <= INSERTIONSORT_LIMIT){
      Insertionsort_l2r(a, 0, n-1);
      return ;
    }
#endif
    m = n/2;
    KnuthsortAP_recurse(b, a, m);
    KnuthsortAP_recurse(b+m, a+m, n-m);
    KnuthsortAP_merge(a, b, b+m-1, b+n-1);
    //right_merge2(a, b, b+m-1, b+m, b+n-1);
  }
}

/*
static void KnuthsortAP_recurse(ValueT *a, ValueT *b, IndexT l, IndexT r){
  if (l<r){
    IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if ((r-l) < INSERTIONSORT_LIMIT){
      Insertionsort_l2r(a, l, r);
      return ;
    }
#endif
    m = l + (r-l)/2;
    KnuthsortAP_recurse(b, a, l  , m);
    KnuthsortAP_recurse(b, a, m+1, r);
    KnuthsortAP_merge(a+l, b+l, b+m, b+r);
  }
}
*/


void KnuthsortAP_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  //KnuthsortAP_recurse(x, aux, 0, n-1);
  KnuthsortAP_recurse(x, aux, n);
  FREE(aux);
}

void KnuthsortAP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  //KnuthsortAP_recurse(aux, aux2, 0, n-1);
  KnuthsortAP_recurse(aux, aux2, n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
