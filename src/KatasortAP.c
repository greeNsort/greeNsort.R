/*
# greeNsort Katasort A-tuned (pointer implementation)
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

// the version below has the same performance:
static void KatasortAP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT*j=m+1;
  ValueT u=*l, v=*j;
  if (GT(*m, v)){
    if (LT(*r, *m)){
      // m is the last one, hence j exhausts first
      for(;;){
        if (LT(v, u)){
          *(z++) = v;
          if (j == r)
            break;
          v = *(++j);
        }else{
          *(z++) = u;
          u = *(++l);
        }
      }
      while(l <= m)
        *(z++) = *(l++);
    }else{
      // r is the last one, hence i exhausts first
      for(;;){
        if (LT(*j, *l)){
          *(z++) = v;
          v= *(++j);
        }else{
          *(z++) = u;
          if (l == m)
            break;
          u=*(++l);
        }
      }
      while(j <= r)
        *(z++) = *(j++);
    }
  }else{
    while(l <= m)
      *(z++) = *(l++);
    while(j <= r)
      *(z++) = *(j++);
  }
}

/*
static void KatasortAP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  if (LT(*r, *m)){
    // m is the last one, hence j exhausts first
    right_merge2_exhausts2(z, l, m, m+1, r);
  }else{
    // r is the last one, hence i exhausts first
    right_merge2_exhausts1(z, l, m, m+1, r);
  }
}
*/

static void KatasortAP_recurse(ValueT *a, ValueT *b, IndexT n){
  if (n>1){
    IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if (n <= INSERTIONSORT_LIMIT){
      Insertionsort_l2r(a, 0, n-1);
      return ;
    }
#endif
    m = n/2;
    KatasortAP_recurse(b, a, m);
    KatasortAP_recurse(b+m, a+m, n-m);
    KatasortAP_merge(a, b, b+m-1, b+n-1);
  }
}



void KatasortAP_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  KatasortAP_recurse(x, aux, n);
  FREE(aux);
}

void KatasortAP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  KatasortAP_recurse(aux, aux2, n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
