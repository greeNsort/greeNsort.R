/*
# greeNsort parallel Frogmerge (pointer implementation) 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_PFrogmergeP_h
#define ALREADY_DEFINED_PFrogmergeP_h

#include "ntile2.h"
#include <pthread.h>

static void FrogmergeP_asc_right(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  // Rprintf("FrogmergeP_asc_right n=%d nl=%d nr=%d\n", rr-rl+lr-ll+2, lr-ll+1, rr-rl+1);
  // for (ValueT *i=ll;i<=lr;i++)
  //   Rprintf(" l=%f\n", *i);
  // for (ValueT *i=rl;i<=rr;i++)
  //   Rprintf(" r=%f\n", *i);
  ValueT u=*ll, v=*rl;
  for(;;){
    if (GT(u, v)){
      *(x++) = v; rl++;
      if (rl>rr)
        break;
      v = *rl;
    }else{
      *(x++) = u; ll++;
      if (ll>lr)
        return;
      u = *ll;
    }
  }
  while(ll<=lr){
    *(x++) = *(ll++);
  }
  return;
}

static void FrogmergeP_asc_left(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  // Rprintf("FrogmergeP_asc_left n=%d nl=%d nr=%d\n", rr-rl+lr-ll+2, lr-ll+1, rr-rl+1);
  // for (ValueT *i=ll;i<=lr;i++)
  //   Rprintf(" l=%f\n", *i);
  // for (ValueT *i=rl;i<=rr;i++)
  //   Rprintf(" r=%f\n", *i);
  ValueT u=*lr, v=*rr;
  for(;;){
    if (GT(u, v)){
      *(x--) = u; lr--;
      if (ll>lr)
        break;
      u = *lr;
    }else{
      *(x--) = v; rr--;
      if (rl>rr)
        return;
      v = *rr;
    }
  }
  while(rl<=rr){
    *(x--) = *rr--;
  }
  return;
}


static void copy(ValueT *t, ValueT *s, IndexT n){
  IndexT i;
  for (i=0; i < n; i++){
    t[i] = s[i];
  }
}

typedef struct Pcopy_t{
  ValueT *t;
  ValueT *s;
  IndexT n;
} Pcopy_t;

static void *Pcopy(void *arg){
  Pcopy_t *parg = (Pcopy_t*)arg;
  ValueT *t = parg->t;
  ValueT *s = parg->s;
  IndexT i,n=parg->n;
  for (i=0; i < n; i++){
    t[i] = s[i];
  }
  return NULL;
}

static void Scopy(ValueT *target, ValueT *source, IndexT n, double maxt){
  IndexT t = ceilf(maxt);
  Pcopy_t *args = (Pcopy_t *) MALLOC(t, Pcopy_t);
  pthread_t *threads = (pthread_t *) MALLOC(t, pthread_t);
  double d = n/((double)t);
  double c = 0;
  IndexT i, k=0, l=0;
  for (i=0; i<t; i++){
    c += d;
    k = roundf(c);
    args[i].t = target+l;
    args[i].s = source+l;
    args[i].n = k - l;
    pthread_create(threads+i, NULL, Pcopy, args+i);
    l = k;
  }
  for (i=0; i<t; i++){
    pthread_join(threads[i], NULL);
  }
  FREE(threads);
  FREE(args);
}



typedef struct FrogmergeP_asc_t{
  ValueT *x;
  ValueT *ll;
  ValueT *lr;
  ValueT *rl;
  ValueT *rr;
} FrogmergeP_asc_t;

static void * PFrogmergeP_asc_right(void *arg){
  FrogmergeP_asc_t *parg = (FrogmergeP_asc_t*)arg;
  ValueT *x = parg->x;
  ValueT *ll = parg->ll;
  ValueT *lr = parg->lr;
  ValueT *rl = parg->rl;
  ValueT *rr = parg->rr;
  if (ll <= lr && rl <= rr){
    ValueT u=*(ll), v=*(rl);
    for(;;){
      if (GT(u, v)){
        *(x++) = v; rl++;
        if (rl > rr)
          break;
        v = *(rl);
      }else{
        *(x++) = u; ll++;
        if (ll > lr)
          break;
        u = *(ll);
      }
    }
  }
  while(ll <= lr)
    *(x++) = *(ll++);
  while(rl <= rr)
    *(x++) = *(rl++);
  return NULL;
}

static void * PFrogmergeP_asc_left(void *arg){
  FrogmergeP_asc_t *parg = (FrogmergeP_asc_t*)arg;
  ValueT *x = parg->x;
  ValueT *ll = parg->ll;
  ValueT *lr = parg->lr;
  ValueT *rl = parg->rl;
  ValueT *rr = parg->rr;
  if (ll <= lr && rl <= rr){
    ValueT u=*(lr), v=*(rr);
    for(;;){
      if (GT(u, v)){
        *(x--) = u; lr--;
        if (ll > lr)
          break;
        u = *(lr);
      }else{
        *(x--) = v; rr--;
        if (rl > rr)
          break;
        v = *(rr);
      }
    }
  }
  while(rl <= rr)
    *(x--) = *(rr--);
  while(ll <= lr)
    *(x--) = *(lr--);
  return NULL;
}

static void SFrogmergeP_asc_left(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
  , double maxt
){
  IndexT rest_l = lr - ll + 1;
  IndexT rest_r = rr - rl + 1;
  int i, t = ceilf(maxt);
  FrogmergeP_asc_t *args = (FrogmergeP_asc_t *) MALLOC(t, FrogmergeP_asc_t);
  pthread_t *threads = (pthread_t *) MALLOC(t, pthread_t);
  Ntile2Struct ntile;
  IndexT s = x - lr;
  IndexT k,cumk;
  double d,cumd;
  while(s > THREAD_LIMIT_MERGE){
    t = MIN(t, s / THREAD_LIMIT_MERGE);
    d = s / t;
    cumd = d;
    cumk = 0;
    for (i = 0; i<t; i++){
      k = roundf(cumd - cumk);
      ntile = Ntile2_asc_asc_right_to_left(
        /*Ll*/ ll
        , /*Rl*/ rl
        , /*Ln*/ rest_l
        , /*Rn*/ rest_r
        , /* k*/ k
      );
      args[i].x = x;
      args[i].ll = lr - ntile.l + 1;
      args[i].lr = lr;
      args[i].rl = rr - ntile.r + 1;
      args[i].rr = rr;
      //PFrogmergeP_asc_left(args+i);
      pthread_create(threads+i, NULL, PFrogmergeP_asc_left, args+i);
      x -= k;
      lr -= ntile.l;
      rr -= ntile.r;
      rest_l -= ntile.l;
      rest_r -= ntile.r;
      cumk += k;
      cumd += d;
    }
    for (i = 0; i<t; i++){
      pthread_join(threads[i], NULL);
    }
    s = x - lr;
  }
  FREE(threads);
  args[0].x = x;
  args[0].ll = ll;
  args[0].lr = lr;
  args[0].rl = rl;
  args[0].rr = rr;
  PFrogmergeP_asc_left(args);
  FREE(args);
  return;
}

static void SFrogmergeP_asc_right(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
  , double maxt
){
  IndexT rest_l = lr - ll + 1;
  IndexT rest_r = rr - rl + 1;
  int i, t = ceilf(maxt);
  FrogmergeP_asc_t *args = (FrogmergeP_asc_t *) MALLOC(t, FrogmergeP_asc_t);
  pthread_t *threads = (pthread_t *) MALLOC(t, pthread_t);
  Ntile2Struct ntile;
  IndexT s = rl - x;
  IndexT k,cumk;
  double d,cumd;
  while(s > THREAD_LIMIT_MERGE){
    t = MIN(t, s / THREAD_LIMIT_MERGE);
    d = s / t;
    cumd = d;
    cumk = 0;
    for (i = 0; i<t; i++){
      k = roundf(cumd - cumk);
      ntile = Ntile2_asc_asc_left_to_right(
        /*Ll*/ ll
        , /*Rl*/ rl
        , /*Ln*/ rest_l
        , /*Rn*/ rest_r
        , /* k*/ k
      );
      args[i].x = x;
      args[i].ll = ll;
      args[i].lr = ll + ntile.l - 1;
      args[i].rl = rl;
      args[i].rr = rl + ntile.r - 1;
      //PFrogmergeP_asc_right(args+i);
      pthread_create(threads+i, NULL, PFrogmergeP_asc_right, args+i);
      x += k;
      ll += ntile.l;
      rl += ntile.r;
      rest_l -= ntile.l;
      rest_r -= ntile.r;
      cumk += k;
      cumd += d;
    }
    for (i = 0; i<t; i++){
      pthread_join(threads[i], NULL);
    }
    s = rl - x;
  }
  FREE(threads);
  args[0].x = x;
  args[0].ll = ll;
  args[0].lr = lr;
  args[0].rl = rl;
  args[0].rr = rr;
  PFrogmergeP_asc_right(args);
  FREE(args);
  return;
}

#endif
