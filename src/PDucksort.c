/*
# greeNsort branch parallel Ducksort
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

#define SAMPLE_SIZE_PARALLEL 64  // 0 = Other Methods  >0  =sample size
#define USE_MEDIAN_OF_3_PARALLEL 0
#define USE_NINTHER_PARALLEL 0
#define USE_MEDIAN_OF_3_PIVOT 0
#define NINTHER_THRESHOLD 0  // 0 = no ninther  128 = Pdq

#if SAMPLE_SIZE_PARALLEL > 0
#include "Insertionorder_l2r.h"
static IndexT sample_pivot(
    ValueT *x
, IndexT l, IndexT r
){
  double diff, d;
  IndexT i, j, ifrom, ito;
  IndexT index[SAMPLE_SIZE_PARALLEL];
  IndexT iindex[SAMPLE_SIZE_PARALLEL];
  ValueT value[SAMPLE_SIZE_PARALLEL];
  diff = (r-l+1)/((double)SAMPLE_SIZE_PARALLEL);
  d = l+diff;
  ifrom = l; ito = round(d);
  for (i=0; i<SAMPLE_SIZE_PARALLEL; i++){
    j = ifrom + randIndex(ito - ifrom + 1);
    iindex[i] = i;
    index[i] = j;
    value[i] = x[j];
    ifrom = ito;
    d += diff;
    ito = round(d);
  }
  Insertionorder_l2r(value, iindex, 0, SAMPLE_SIZE_PARALLEL-1);
  return index[iindex[SAMPLE_SIZE_PARALLEL/2]];
}
#endif

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


static void Ducksort_TieLeft(
    ValueT *x
  , IndexT l, IndexT r
);

// pivot partioning placing all pivot ties high
// pivot partioning placing all pivot ties high
static void Ducksort_TieRight(
    ValueT *x
  , IndexT l, IndexT r
){
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
  ValueT t, v;
  IndexT i, j;

  // POET replaces DIET: tentatively invest more COMPs than DIET (and MAIN has to restart from scratch)
  j = r;
  while (--j, LE(x[j], x[j+1])) if (j <= l) return;  // explicit stop of for loop

#if USE_MEDIAN_OF_3_PIVOT
  IndexT a,b,c,n=r-l+1;
  a = r-randIndex(n);
  b = r-randIndex(n);
  c = r-randIndex(n);
  j = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
#if NINTHER_THRESHOLD > 0
  if (NINTHER_THRESHOLD < n){
    IndexT j2,j3;
    a = r-randIndex(n);
    b = r-randIndex(n);
    c = r-randIndex(n);
    j2 = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
    a = r-randIndex(n);
    b = r-randIndex(n);
    c = r-randIndex(n);
    j3 = LT(x[a], x[b ]) ? (LT(x[b ], x[c ]) ? b  : LT(x[a], x[c ]) ? c  : a)  : (LT(x[c ], x[b ]) ? b  : LT(x[c ], x[a]) ? c  : a);
    j  = LT(x[j], x[j2]) ? (LT(x[j2], x[j3]) ? j2 : LT(x[j], x[j3]) ? j3 : j)  : (LT(x[j3], x[j2]) ? j2 : LT(x[j3], x[j]) ? j3 : j);
  }
#endif
#else
  j = r-randIndex(r-l+1);
#endif
  SWAP(x[j], x[r], v);  // first argument pivot now in v and x[r]

  // MAIN
  j = r;
  i = l - 1;
  for (;;){
    while(LT(x[++i], v))  if (j <= i) break;  // explicit stop of for loop
    while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
    if (j <= i)break;
    SWAP(x[i], x[j], t);
  }
  // no swap of pivot to j, j belongs to left
  Ducksort_TieLeft(x, l, i-1);
  Ducksort_TieRight(x, i, r);
}


// pivot partioning placing all pivot ties low
static void Ducksort_TieLeft(
    ValueT *x
  , IndexT l, IndexT r
){
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
  ValueT t, v;
  IndexT i, j;

  // POET replaces DIET: tentatively invest more COMPs than DIET (and MAIN has to restart from scratch)
  i = l;
  while (++i, LE(x[i-1], x[i])) if (i >= r) return;  // explicit stop of for loop

#if USE_MEDIAN_OF_3_PIVOT
  IndexT a,b,c,n=r-l+1;
  a = l+randIndex(n);
  b = l+randIndex(n);
  c = l+randIndex(n);
  i = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
#if NINTHER_THRESHOLD > 0
  if (NINTHER_THRESHOLD < n){
    IndexT i2,i3;
    a = l+randIndex(n);
    b = l+randIndex(n);
    c = l+randIndex(n);
    i2 = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
    a = l+randIndex(n);
    b = l+randIndex(n);
    c = l+randIndex(n);
    i3 = LT(x[a], x[b ]) ? (LT(x[b ], x[c ]) ? b  : LT(x[a], x[c ]) ? c  : a)  : (LT(x[c ], x[b ]) ? b  : LT(x[c ], x[a]) ? c  : a);
    i  = LT(x[i], x[i2]) ? (LT(x[i2], x[i3]) ? i2 : LT(x[i], x[i3]) ? i3 : i)  : (LT(x[i3], x[i2]) ? i2 : LT(x[i3], x[i]) ? i3 : i);
  }
#endif
#else
  i = l+randIndex(r-l+1);
#endif
  SWAP(x[i], x[l], v);  // first argument pivot now in v and x[l]

  // MAIN
  i = l;
  j = r + 1;
  for (;;){
    while(GT(x[--j], v)); // sentinel stop guaranteed by pivot at the left
    while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
    if (i >= j) break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[j], x[l], t); // the index j from the first loop is guaranteed to be in a legal position
  Ducksort_TieRight(x, l, j-1);
  Ducksort_TieLeft(x, j+1, r);
}





typedef struct PDucksort_t{
  ValueT *x;
  IndexT l;
  IndexT r;
  double threads;
} PDucksort_t;


static void * PDucksort_TieLeft(void *arg);

static void * PDucksort_TieRight(void *arg)
{
  PDucksort_t *parg = (PDucksort_t*)arg;
  if (parg->threads <= 1 || ((parg->r - parg->l + 1) <= THREAD_LIMIT_SORT)){
    Ducksort_TieRight(parg->x, parg->l, parg->r);
  }else{
    pthread_t thread0, thread1;
    PDucksort_t Larg,Rarg;

    ValueT *x = parg->x;
    IndexT l = parg->l;
    IndexT r = parg->r;

    IndexT j, i;
    ValueT t, v;

    // POET replaces DIET: tentatively invest more COMPs than DIET (and MAIN has to restart from scratch)
    j = r;
    while (--j, LE(x[j], x[j+1])) if (j <= l) return NULL;  // explicit stop of for loop

#if SAMPLE_SIZE_PARALLEL > 0
  j = sample_pivot(x, l, r);
#else
#if USE_MEDIAN_OF_3_PARALLEL
    IndexT a,b,c,n=r-l+1;
    a = r-randIndex(n);
    b = r-randIndex(n);
    c = r-randIndex(n);
    j = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
#if USE_NINTHER_PARALLEL
    IndexT j2,j3;
    a = r-randIndex(n);
    b = r-randIndex(n);
    c = r-randIndex(n);
    j2 = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
    a = r-randIndex(n);
    b = r-randIndex(n);
    c = r-randIndex(n);
    j3 = LT(x[a], x[b ]) ? (LT(x[b ], x[c ]) ? b  : LT(x[a], x[c ]) ? c  : a)  : (LT(x[c ], x[b ]) ? b  : LT(x[c ], x[a]) ? c  : a);
    j  = LT(x[j], x[j2]) ? (LT(x[j2], x[j3]) ? j2 : LT(x[j], x[j3]) ? j3 : j)  : (LT(x[j3], x[j2]) ? j2 : LT(x[j3], x[j]) ? j3 : j);
#endif
#else
    j = r-randIndex(r-l+1);
#endif
#endif
    SWAP(x[j], x[r], v);  // first argument pivot now in v and x[r]

    // MAIN
    j = r;
    i = l - 1;
    for (;;){
      while(LT(x[++i], v))  if (j <= i) break;  // explicit stop of for loop
      while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
      if (j <= i)break;
      SWAP(x[i], x[j], t);
    }
    SWAP(x[i], x[r], t); // the index i from the first loop is guaranteed to be in a legal position

    // Ducksort_TieRight(x, l, i-1);
    // Ducksort_TieLeft(x, i+1, r);
    Larg.x = x;
    Larg.l = l;
    Larg.r = i-1;
    Larg.threads = (i-l)/((double)(r-l+1));
    Rarg.x = x;
    Rarg.l = i+1;
    Rarg.r = r;
    Rarg.threads = parg->threads - Larg.threads;

    // PDucksort_TieRight(&Larg);
    // PDucksort_TieLeft(&Rarg);

    pthread_create(&thread0, NULL, PDucksort_TieRight, &Larg);
    pthread_create(&thread1, NULL, PDucksort_TieLeft, &Rarg);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}


static void * PDucksort_TieLeft(void *arg)
{
  PDucksort_t *parg = (PDucksort_t*)arg;
  if (parg->threads <= 1 || ((parg->r - parg->l + 1) <= THREAD_LIMIT_SORT)){
    Ducksort_TieLeft(parg->x, parg->l, parg->r);
  }else{
    pthread_t thread0, thread1;
    PDucksort_t Larg,Rarg;

    ValueT *x = parg->x;
    IndexT l = parg->l;
    IndexT r = parg->r;

    IndexT j, i;
    ValueT t, v;

    // POET replaces DIET: tentatively invest more COMPs than DIET (and MAIN has to restart from scratch)
    i = l;
    while (++i, LE(x[i-1], x[i])) if (i >= r) return NULL;  // explicit stop of for loop

#if SAMPLE_SIZE_PARALLEL > 0
    i = sample_pivot(x, l, r);
#else
#if USE_MEDIAN_OF_3_PARALLEL
    IndexT a,b,c,n=r-l+1;
    a = l+randIndex(n);
    b = l+randIndex(n);
    c = l+randIndex(n);
    i = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
#if USE_NINTHER_PARALLEL
    IndexT i2,i3;
    a = l+randIndex(n);
    b = l+randIndex(n);
    c = l+randIndex(n);
    i2 = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
    a = l+randIndex(n);
    b = l+randIndex(n);
    c = l+randIndex(n);
    i3 = LT(x[a], x[b ]) ? (LT(x[b ], x[c ]) ? b  : LT(x[a], x[c ]) ? c  : a)  : (LT(x[c ], x[b ]) ? b  : LT(x[c ], x[a]) ? c  : a);
    i  = LT(x[i], x[i2]) ? (LT(x[i2], x[i3]) ? i2 : LT(x[i], x[i3]) ? i3 : i)  : (LT(x[i3], x[i2]) ? i2 : LT(x[i3], x[i]) ? i3 : i);
#endif
#else
    i = l+randIndex(r-l+1);
#endif
#endif
    SWAP(x[i], x[l], v);  // first argument pivot now in v and x[r]

    // MAIN
    i = l;
    j = r + 1;
    for (;;){
      while(GT(x[--j], v)); // sentinel stop guaranteed by pivot at the left
      while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
      if (i >= j) break;
      SWAP(x[i], x[j], t);
    }
    SWAP(x[j], x[l], t); // the index j from the first loop is guaranteed to be in a legal position

    // Ducksort_TieRight(x, l, j-1);
    // Ducksort_TieLeft(x, j+1, r);
    Larg.x = x;
    Larg.l = l;
    Larg.r = j-1;
    Larg.threads = (j-l)/((double)(r-l+1));
    Rarg.x = x;
    Rarg.l = j+1;
    Rarg.r = r;
    Rarg.threads = parg->threads - Larg.threads;

    // PDucksort_TieRight(&Larg);
    // PDucksort_TieLeft(&Rarg);

    pthread_create(&thread0, NULL, PDucksort_TieRight, &Larg);
    pthread_create(&thread1, NULL, PDucksort_TieLeft, &Rarg);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}


void PDucksort_insitu(ValueT *x, IndexT n, double t)
{
  //Ducksort(x, 0, n-1, t);
  PDucksort_t arg;
  arg.x = x;
  arg.l = 0;
  arg.r = n-1;
  arg.threads = t;
  PDucksort_TieLeft(&arg);
}

void PDucksort_exsitu(ValueT *x, IndexT n, double t)
{
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  Scopy(aux, x, n, t);

  //Ducksort(aux, 0, n-1, t);
  PDucksort_t arg;
  arg.x = aux;
  arg.l = 0;
  arg.r = n-1;
  arg.threads = t;
  PDucksort_TieLeft(&arg);

  Scopy(x, aux, n, t);
  FREE(aux);
}

#undef USE_MEDIAN_OF_3_PIVOT
