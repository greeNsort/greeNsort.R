/*
# greeNsort parallel Knuthsort (pointer implementation) 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"

#include <stdbool.h>
#include <pthread.h>
#include "ntile2.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


typedef struct Pcopy_t{
  ValueT *t;
  ValueT *s;
  IndexT n;
} Pcopy_t;

static void *Pcopy(void *arg){
  Pcopy_t *parg = (Pcopy_t*)arg;
  IndexT i;
  for (i=0; i < parg->n; i++){
    (parg->t)[i] = (parg->s)[i];
  }
  return NULL;
}


typedef struct Pcopy2_t{
  ValueT *t;
  ValueT *t2;
  ValueT *s;
  IndexT n;
} Pcopy2_t;

static void *Pcopy2(void *arg){
  Pcopy2_t *parg = (Pcopy2_t*)arg;
  IndexT i;
  for (i=0; i < parg->n; i++){
    (parg->t2)[i] = (parg->t)[i] = (parg->s)[i];
  }
  return NULL;
}


static void KnuthsortP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  ValueT *j=m+1;
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

typedef struct PKnuthsortP_merge_t{
  ValueT *z;
  ValueT *ll;
  ValueT *lr;
  ValueT *rl;
  ValueT *rr;
} PKnuthsortP_merge_t;


static void *PKnuthsortP_merge(void *arg){
  PKnuthsortP_merge_t *parg = (PKnuthsortP_merge_t*)arg;
  ValueT *z = parg->z;
  ValueT *ll = parg->ll;
  ValueT *lr = parg->lr;
  ValueT *rl = parg->rl;
  ValueT *rr = parg->rr;
  if (ll <= lr && rl <= rr){ // attention: can be called with an empty input stream
      ValueT u=*ll, v=*rl;
      for (;;){
        if (LT(v, u)){
          *(z++) = v; rl++;
          if (rl > rr)  // check either here
            break;
          v = *rl;
        }else{
          *(z++) = u; ll++;
          if (ll > lr)  // or check here
            break;
          u= *ll;
        }
      }
    }
    while(ll <= lr)
      *(z++) = *(ll++);
    while(rl <= rr)
      *(z++) = *(rl++);
    return NULL;
}


static void KnuthsortP_recurse(ValueT *a, ValueT *b, IndexT n){
  if (n>1){
    IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if (n <= INSERTIONSORT_LIMIT){
      Insertionsort_l2r(a, 0, n-1);
      return ;
    }
#endif
    m = n/2;
    KnuthsortP_recurse(b, a, m);
    KnuthsortP_recurse(b+m, a+m, n-m);
    KnuthsortP_merge(a, b, b+m-1, b+n-1);
  }
}

typedef struct PKnuthsortP_recurse_t{
  ValueT *a;
  ValueT *b;
  IndexT n;
  double threads;
} PKnuthsortP_recurse_t;


static void * PKnuthsortP_recurse(void *arg){
  PKnuthsortP_recurse_t *parg = (PKnuthsortP_recurse_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    KnuthsortP_recurse(parg->a, parg->b, parg->n);
  }else{
    int t = MIN(ceilf(parg->threads), parg->n / THREAD_LIMIT_SORT);

    pthread_t *threads = (pthread_t *) MALLOC(t, pthread_t);
    PKnuthsortP_merge_t *args = (PKnuthsortP_merge_t *) MALLOC(t, PKnuthsortP_merge_t);
    PKnuthsortP_recurse_t L, R;

    IndexT m = parg->n/2;

    // ping-pong sort to b
    L.a = parg->b;
    L.b = parg->a;
    L.n = m;
    L.threads = parg->threads / 2.0;

    R.a = parg->b + m;
    R.b = parg->a + m;
    R.n = parg->n - m;
    R.threads = L.threads;

    pthread_create(threads, NULL, PKnuthsortP_recurse, &L);
    pthread_create(threads+1, NULL, PKnuthsortP_recurse, &R);
    //instead of: PKnuthsortP_recurse(&R);
    //because saving the second spawn is slower!!
    pthread_join(threads[0], NULL);
    pthread_join(threads[1], NULL);

    // merge from parg->b to parg->a (which is L.a to L.b and R.a to R.b)
    //KnuthsortP_merge(parg->a, parg->b, parg->b+m-1, parg->b+parg->n-1);
    // args[0].z  = parg->a;
    // args[0].ll = parg->b;
    // args[0].lr = args[0].ll + m - 1;
    // args[0].rl = parg->b + m;
    // args[0].rr = args[0].rl + (parg->n - m) - 1;
    // PKnuthsortP_merge(args+0);
    double s = parg->n / (double) t;
    double cums=s;
    IndexT k;
    Ntile2Struct ntile,cum;
    cum.l = 0;
    cum.r = 0;
    for (int i=0; i<t; i++){
      k = roundf(cums - (cum.l + cum.r));
      //Rprintf("i=%d s=%f cums=%f k=%d N=%d\n", i, s, cums, k, L.n+R.n); R_FlushConsole();
      ntile = Ntile2_asc_asc_left_to_right(L.a + cum.l, R.a + cum.r, L.n, R.n, k);
      args[i].z  = parg->a + (cum.l + cum.r);
      args[i].ll = parg->b + cum.l;
      args[i].lr = args[i].ll + ntile.l - 1;
      args[i].rl = parg->b + m + cum.r;
      args[i].rr = args[i].rl + ntile.r - 1;
      //PKnuthsortP_merge(args+i);
      pthread_create(threads+i, NULL, PKnuthsortP_merge, args+i);
      cum.l += ntile.l;
      cum.r += ntile.r;
      L.n -= ntile.l;
      R.n -= ntile.r;
      //Rprintf("i=%d k=%d  N=%d Ln=%d Rn=%d  n=%d l=%d r=%d   cuml=%d cumr=%d cumn=%d\n", i, k, L.n+R.n, L.n, R.n, ntile.l+ntile.r, ntile.l, ntile.r, cum.l, cum.r, cum.l+cum.r); R_FlushConsole();
      cums +=s;
    }
    for (int i=0; i<t; i++){
      pthread_join(threads[i], NULL);
    }

    FREE(args);
    FREE(threads);

  }
  return NULL;
}


void PKnuthsortP_insitu(ValueT *x, IndexT n, double t)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);

  //for (i = 0; i < n; i++) aux[i] = x[i];
  pthread_t *threads = (pthread_t *) MALLOC(t, pthread_t);
  Pcopy_t *args = (Pcopy_t *) MALLOC(t, Pcopy_t);
  double c = 0, d = n/((double)t);
  IndexT k=0, l = 0;
  for (i=0; i<t; i++){
    c += d;
    k = roundf(c);
    args[i].t = aux + l;
    args[i].s = x + l;
    args[i].n = k - l;
    pthread_create(threads+i, NULL, Pcopy, args+i);
    l = k;
  }
  for (i=0; i<t; i++){
    pthread_join(threads[i], NULL);
  }
  FREE(args);
  FREE(threads);

  // KnuthsortP_recurse(x, aux, n);
  PKnuthsortP_recurse_t arg;
  arg.a = x;
  arg.b = aux;
  arg.n = n;
  arg.threads = t;
  PKnuthsortP_recurse(&arg);

  FREE(aux);
}

void PKnuthsortP_exsitu(ValueT *x, IndexT n, double t)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;

  pthread_t *threads = (pthread_t *) MALLOC(t, pthread_t);
  double c, d = n/((double)t);
  IndexT k, l;

  //for (i = 0; i < n; i++) aux2[i] = aux[i] = x[i];
  Pcopy2_t *args2 = (Pcopy2_t *) MALLOC(t, Pcopy2_t);
  c = 0; k=0; l = 0;
  for (i=0; i<t; i++){
    c += d;
    k = roundf(c);
    args2[i].t = aux + l;
    args2[i].t2 = aux2 + l;
    args2[i].s = x + l;
    args2[i].n = k - l;
    pthread_create(threads+i, NULL, Pcopy2, args2+i);
    l = k;
  }
  for (i=0; i<t; i++){
    pthread_join(threads[i], NULL);
  }
  FREE(args2);

  //KnuthsortP_recurse(aux, aux2, n);
  PKnuthsortP_recurse_t arg;
  arg.threads = t;
  arg.a = aux;
  arg.b = aux2;
  arg.n = n;
  PKnuthsortP_recurse(&arg);

  //for (i=0; i<n; i++) x[i] = aux[i];
  Pcopy_t *args = (Pcopy_t *) MALLOC(t, Pcopy_t);
  c = 0; k=0; l = 0;
  for (i=0; i<t; i++){
    c += d;
    k = roundf(c);
    args[i].t = x + l;
    args[i].s = aux + l;
    args[i].n = k - l;
    pthread_create(threads+i, NULL, Pcopy, args+i);
    l = k;
  }
  for (i=0; i<t; i++){
    pthread_join(threads[i], NULL);
  }
  FREE(args);

  FREE(threads);

  FREE(aux);
}
