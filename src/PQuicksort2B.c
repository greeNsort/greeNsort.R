/*
# greeNsort branch parallel Quicksort B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include <pthread.h>
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

#define USE_MEDIAN_OF_3_PIVOT 0

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
  pthread_t *threads = (pthread_t *) MALLOC(t, pthread_t);
  Pcopy_t *args = (Pcopy_t *) MALLOC(t, Pcopy_t);
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
  FREE(args);
  FREE(threads);
}


static void Quicksort2B(ValueT *x, IndexT l, IndexT r)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
  IndexT k = l+randIndex(r-l+1);
  //Rprintf("l=%d  i=%d  r=%d\n", l, i, r);
  ValueT t, v;
  SWAP(x[k], x[r], v); // first argument pivot now in v and x[r]

  // Begin Block-Processing
  ValueT *L = x+l, *R = x+r-1;
  unsigned char indexL[QUICKSORT_BLOCKSIZE], indexR[QUICKSORT_BLOCKSIZE];
  IndexT iL = 0;
  IndexT iR = 0;
  IndexT sL = 0;
  IndexT sR = 0;
  IndexT num;
  while (R - L + 1 > 2 * QUICKSORT_BLOCKSIZE) {
    if (iL == 0) {
      sL = 0;
      for (k = 0; k < QUICKSORT_BLOCKSIZE; k++) {
        indexL[iL] = k;
        iL += GE(L[k], v); // == !LT
      }
    }
    if (iR == 0) {
      sR = 0;
      for (k = 0; k < QUICKSORT_BLOCKSIZE; k++) {
        indexR[iR] = k;
        iR += GE(v, (*(R - k))); // == !LT
      }
    }
    num = MIN(iL, iR);
    if (num != 0) {
      t = L[indexL[sL]];
      L[+indexL[sL]] = R[-indexR[sR]];
      for (k = 1; k < num; k++) {
        R[-indexR[sR + k - 1]] = L[+indexL[sL + k]];
        L[+indexL[sL + k    ]] = R[-indexR[sR + k]];
      }
      R[-indexR[sR + num - 1]] = t;
    }
    iL -= num;
    iR -= num;
    sL += num;
    sR += num;
    if (iL == 0)
      L += QUICKSORT_BLOCKSIZE;
    if (iR == 0)
      R -= QUICKSORT_BLOCKSIZE;
  }
  L--;
  R++;
  // End Block-Processing

  for (;;){
    while (LT(*(++L), v)); // sentinel stop of for loop
    while (LT(v, *(--R))){
      if (R <= L)       // explicit stop of for loop
        break;
    }
    if (R <= L)
      break;
    SWAP(*L, *R, t);
  }
  SWAP(*L, x[r], t);
  k = L - x;

  Quicksort2B(x, l, k-1);
  Quicksort2B(x, k+1, r);
}


typedef struct PQuicksort2B_t{
  ValueT *x;
  IndexT l;
  IndexT r;
  double threads;
} PQuicksort2B_t;



static void * PQuicksort2B(void *arg)
{
  PQuicksort2B_t *parg = (PQuicksort2B_t*)arg;
  if (parg->threads <= 1 || ((parg->r - parg->l + 1) <= THREAD_LIMIT_SORT)){
    Quicksort2B(parg->x, parg->l, parg->r);
  }else{
    pthread_t thread0, thread1;
    PQuicksort2B_t Larg,Rarg;

    ValueT *x = parg->x;
    IndexT l = parg->l;
    IndexT r = parg->r;

  IndexT k;
#if USE_MEDIAN_OF_3_PIVOT
  IndexT a,b,c,n=r-l+1;
  a = l+randIndex(n);
  b = l+randIndex(n);
  c = l+randIndex(n);
  k = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
#else
  k = l+randIndex(r-l+1);
#endif
    ValueT t, v;
    SWAP(x[k], x[r], v); // first argument pivot now in v and x[r]

    // Begin Block-Processing
    ValueT *L = x+l, *R = x+r-1;
    unsigned char indexL[QUICKSORT_BLOCKSIZE], indexR[QUICKSORT_BLOCKSIZE];
    IndexT iL = 0;
    IndexT iR = 0;
    IndexT sL = 0;
    IndexT sR = 0;
    IndexT num;
    while (R - L + 1 > 2 * QUICKSORT_BLOCKSIZE) {
      if (iL == 0) {
        sL = 0;
        for (k = 0; k < QUICKSORT_BLOCKSIZE; k++) {
          indexL[iL] = k;
          iL += GE(L[k], v); // == !LT
        }
      }
      if (iR == 0) {
        sR = 0;
        for (k = 0; k < QUICKSORT_BLOCKSIZE; k++) {
          indexR[iR] = k;
          iR += GE(v, (*(R - k))); // == !LT
        }
      }
      num = MIN(iL, iR);
      if (num != 0) {
        t = L[indexL[sL]];
        L[+indexL[sL]] = R[-indexR[sR]];
        for (k = 1; k < num; k++) {
          R[-indexR[sR + k - 1]] = L[+indexL[sL + k]];
          L[+indexL[sL + k    ]] = R[-indexR[sR + k]];
        }
        R[-indexR[sR + num - 1]] = t;
      }
      iL -= num;
      iR -= num;
      sL += num;
      sR += num;
      if (iL == 0)
        L += QUICKSORT_BLOCKSIZE;
      if (iR == 0)
        R -= QUICKSORT_BLOCKSIZE;
    }
    L--;
    R++;
    // End Block-Processing

    for (;;){
      while (LT(*(++L), v)); // sentinel stop of for loop
      while (LT(v, *(--R))){
        if (R <= L)       // explicit stop of for loop
          break;
      }
      if (R <= L)
        break;
      SWAP(*L, *R, t);
    }
    SWAP(*L, x[r], t);
    k = L - x;

    //Quicksort2B(x, l, k-1);
    //Quicksort2B(x, k+1, r);
    Larg.x = x;
    Larg.l = l;
    Larg.r = k-1;
    Larg.threads = (k-l)/((double)(r-l+1));
    Rarg.x = x;
    Rarg.l = k+1;
    Rarg.r = r;
    Rarg.threads = parg->threads - Larg.threads;

    // PQuicksort2B(&Larg);
    // PQuicksort2B(&Rarg);

    pthread_create(&thread0, NULL, PQuicksort2B, &Larg);
    pthread_create(&thread1, NULL, PQuicksort2B, &Rarg);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}


void PQuicksort2B_insitu(ValueT *x, IndexT n, double t)
{
  //Quicksort2B(x, 0, n-1, t);
  PQuicksort2B_t arg;
  arg.x = x;
  arg.l = 0;
  arg.r = n-1;
  arg.threads = t;
  PQuicksort2B(&arg);
}

void PQuicksort2B_exsitu(ValueT *x, IndexT n, double t)
{
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  Scopy(aux, x, n, t);

  //Quicksort2B(aux, 0, n-1, t);
  PQuicksort2B_t arg;
  arg.x = aux;
  arg.l = 0;
  arg.r = n-1;
  arg.threads = t;
  PQuicksort2B(&arg);

  Scopy(x, aux, n, t);
  FREE(aux);
}

#undef USE_MEDIAN_OF_3_PIVOT



