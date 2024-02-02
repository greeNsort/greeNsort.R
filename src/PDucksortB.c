/*
# greeNsort branch parallel Ducksort B-tuned
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


static void DucksortB_TieLeft(
    ValueT *x
  , IndexT l, IndexT r
);

// pivot partioning placing all pivot ties high
static void DucksortB_TieRight(
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
  IndexT i;
  ValueT t, v;
  ValueT *L = x+l, *R = x+r;

  // DIET preloop
  //while (EQ(*(--R), v)) if (R <= L) return;  // explicit stop of for loop
  // POET preloop
  while (--R, LE(*(R), *(R+1))) if (R <= L) return;  // explicit stop of for loop
  R = x+r;

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
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]

  //if (L < R){
  // Begin Block-Processing
  unsigned char indexL[QUICKSORT_BLOCKSIZE], indexR[QUICKSORT_BLOCKSIZE];
  IndexT iL = 0;
  IndexT iR = 0;
  IndexT sL = 0;
  IndexT sR = 0;
  IndexT num;
  while (R - L + 1 > 2 * QUICKSORT_BLOCKSIZE) {
    if (iL == 0) {
      sL = 0;
      for (i = 0; i < QUICKSORT_BLOCKSIZE; i++) {
        indexL[iL] = i;
        iL += GE(L[i], v);
      }
    }
    if (iR == 0) {
      sR = 0;
      for (i = 0; i < QUICKSORT_BLOCKSIZE; i++) {
        indexR[iR] = i;
        iR += GT(v, (*(R - i)));
      }
    }
    num = MIN(iL, iR);
    if (num != 0) {
      t = L[indexL[sL]];
      L[+indexL[sL]] = R[-indexR[sR]];
      for (i = 1; i < num; i++) {
        R[-indexR[sR + i - 1]] = L[+indexL[sL + i]];
        L[+indexL[sL + i    ]] = R[-indexR[sR + i]];
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
    while(LT(*(++L), v));                     // sentinel stop guaranteed by pivot at the right
    while (GE(*(--R), v)) if (R <= L) break;  // explicit stop of for loop
    if (R <= L)break;
    SWAP(*L, *R, t);
  }

  SWAP(*L, x[r], t); // the index i from the first loop is guaranteed to be in a legal position
  i = L - x;
  DucksortB_TieRight(x, l, i-1);
  DucksortB_TieLeft(x, i+1, r);

  //}
}

// pivot partioning placing all pivot ties low
static void DucksortB_TieLeft(
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
  IndexT j;
  ValueT t, v;
  ValueT *L = x+l, *R = x+r;

  // DIET
  //while (EQ(*(++L), v)) if (L >= R) return;  // explicit stop of for loop
  // POET
  while (++L, LE(*(L-1), *(L))) if (L >= R) return;  // explicit stop of for loop
  L = x+l;

#if USE_MEDIAN_OF_3_PIVOT
  IndexT a,b,c,n=r-l+1;
  a = l+randIndex(n);
  b = l+randIndex(n);
  c = l+randIndex(n);
  j = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
#if NINTHER_THRESHOLD > 0
  if (NINTHER_THRESHOLD < n){
    IndexT j2,j3;
    a = l+randIndex(n);
    b = l+randIndex(n);
    c = l+randIndex(n);
    j2 = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
    a = l+randIndex(n);
    b = l+randIndex(n);
    c = l+randIndex(n);
    j3 = LT(x[a], x[b ]) ? (LT(x[b ], x[c ]) ? b  : LT(x[a], x[c ]) ? c  : a)  : (LT(x[c ], x[b ]) ? b  : LT(x[c ], x[a]) ? c  : a);
    j  = LT(x[j], x[j2]) ? (LT(x[j2], x[j3]) ? j2 : LT(x[j], x[j3]) ? j3 : j)  : (LT(x[j3], x[j2]) ? j2 : LT(x[j3], x[j]) ? j3 : j);
  }
#endif
#else
  j = l+randIndex(r-l+1);
#endif
  SWAP(x[j], x[l], v);  // first argument pivot now in v and x[l]

  //if (L < R){
  // Begin Block-Processing
  unsigned char indexL[QUICKSORT_BLOCKSIZE], indexR[QUICKSORT_BLOCKSIZE];
  IndexT iL = 0;
  IndexT iR = 0;
  IndexT sL = 0;
  IndexT sR = 0;
  IndexT num;
  while (R - L + 1 > 2 * QUICKSORT_BLOCKSIZE) {
    if (iR == 0) {
      sR = 0;
      for (j = 0; j < QUICKSORT_BLOCKSIZE; j++) {
        indexR[iR] = j;
        iR += LE((*(R - j)), v);
      }
    }
    if (iL == 0) {
      sL = 0;
      for (j = 0; j < QUICKSORT_BLOCKSIZE; j++) {
        indexL[iL] = j;
        iL += LT(v, L[j]);
      }
    }
    num = MIN(iL, iR);
    if (num != 0) {
      t = L[indexL[sL]];
      L[+indexL[sL]] = R[-indexR[sR]];
      for (j = 1; j < num; j++) {
        R[-indexR[sR + j - 1]] = L[+indexL[sL + j]];
        L[+indexL[sL + j    ]] = R[-indexR[sR + j]];
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
  // End Block-Processing
  L--;
  R++;

  for (;;){
    while(GT(*(--R), v));                      // sentinel stop guaranteed by pivot at the left
    while (LE(*(++L), v)) if (L >= R) break;  // explicit stop of for loop
    if (L >= R)break;
    SWAP(*L, *R, t);
  }

  j = R - x;
  SWAP(x[l], *R, t); // the index j from the first loop is guaranteed to be in a legal position

  DucksortB_TieRight(x, l, j-1);
  DucksortB_TieLeft(x, j+1, r);

  //}
}



typedef struct PDucksortB_t{
  ValueT *x;
  IndexT l;
  IndexT r;
  double threads;
} PDucksortB_t;



static void * PDucksortB_TieLeft(void *arg);

static void * PDucksortB_TieRight(void *arg)
{
  PDucksortB_t *parg = (PDucksortB_t*)arg;
  if (parg->threads <= 1 || ((parg->r - parg->l + 1) <= THREAD_LIMIT_SORT)){
    DucksortB_TieRight(parg->x, parg->l, parg->r);
  }else{
    pthread_t thread0, thread1;
    PDucksortB_t Larg,Rarg;

    ValueT *x = parg->x;
    IndexT l = parg->l;
    IndexT r = parg->r;

    // POET replaces DIET: tentatively invest more COMPs than DIET (and MAIN has to restart from scratch)
    IndexT i;
    ValueT t, v;
    ValueT *L = x+l, *R = x+r;

    // DIET preloop
    //while (EQ(*(--R), v)) if (R <= L) return;  // explicit stop of for loop
    // POET preloop
    while (--R, LE(*(R), *(R+1))) if (R <= L) return NULL;  // explicit stop of for loop
    R = x+r;

#if SAMPLE_SIZE_PARALLEL > 0
    i = sample_pivot(x, l, r);
#else
#if USE_MEDIAN_OF_3_PARALLEL
    IndexT a,b,c,n=r-l+1;
    a = r-randIndex(n);
    b = r-randIndex(n);
    c = r-randIndex(n);
    i = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
#if USE_NINTHER_PARALLEL
    IndexT i2,i3;
    a = r-randIndex(n);
    b = r-randIndex(n);
    c = r-randIndex(n);
    i2 = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
    a = r-randIndex(n);
    b = r-randIndex(n);
    c = r-randIndex(n);
    i3 = LT(x[a], x[b ]) ? (LT(x[b ], x[c ]) ? b  : LT(x[a], x[c ]) ? c  : a)  : (LT(x[c ], x[b ]) ? b  : LT(x[c ], x[a]) ? c  : a);
    i  = LT(x[i], x[i2]) ? (LT(x[i2], x[i3]) ? i2 : LT(x[i], x[i3]) ? i3 : i)  : (LT(x[i3], x[i2]) ? i2 : LT(x[i3], x[i]) ? i3 : i);
#endif
#else
    i = r-randIndex(r-l+1);
#endif
#endif
    SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]

    //if (L < R){
    // Begin Block-Processing
    unsigned char indexL[QUICKSORT_BLOCKSIZE], indexR[QUICKSORT_BLOCKSIZE];
    IndexT iL = 0;
    IndexT iR = 0;
    IndexT sL = 0;
    IndexT sR = 0;
    IndexT num;
    while (R - L + 1 > 2 * QUICKSORT_BLOCKSIZE) {
      if (iL == 0) {
        sL = 0;
        for (i = 0; i < QUICKSORT_BLOCKSIZE; i++) {
          indexL[iL] = i;
          iL += GE(L[i], v);
        }
      }
      if (iR == 0) {
        sR = 0;
        for (i = 0; i < QUICKSORT_BLOCKSIZE; i++) {
          indexR[iR] = i;
          iR += GT(v, (*(R - i)));
        }
      }
      num = MIN(iL, iR);
      if (num != 0) {
        t = L[indexL[sL]];
        L[+indexL[sL]] = R[-indexR[sR]];
        for (i = 1; i < num; i++) {
          R[-indexR[sR + i - 1]] = L[+indexL[sL + i]];
          L[+indexL[sL + i    ]] = R[-indexR[sR + i]];
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
      while(LT(*(++L), v));                     // sentinel stop guaranteed by pivot at the right
      while (GE(*(--R), v)) if (R <= L) break;  // explicit stop of for loop
      if (R <= L)break;
      SWAP(*L, *R, t);
    }

    SWAP(*L, x[r], t); // the index i from the first loop is guaranteed to be in a legal position
    i = L - x;

    // DucksortB_TieRight(x, l, i-1);
    // DucksortB_TieLeft(x, i+1, r);
    Larg.x = x;
    Larg.l = l;
    Larg.r = i-1;
    Larg.threads = (i-l)/((double)(r-l+1));
    Rarg.x = x;
    Rarg.l = i+1;
    Rarg.r = r;
    Rarg.threads = parg->threads - Larg.threads;

    // PDucksortB_TieRight(&Larg);
    // PDucksortB_TieLeft(&Rarg);

    pthread_create(&thread0, NULL, PDucksortB_TieRight, &Larg);
    pthread_create(&thread1, NULL, PDucksortB_TieLeft, &Rarg);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}


static void * PDucksortB_TieLeft(void *arg)
{
  PDucksortB_t *parg = (PDucksortB_t*)arg;
  if (parg->threads <= 1 || ((parg->r - parg->l + 1) <= THREAD_LIMIT_SORT)){
    DucksortB_TieLeft(parg->x, parg->l, parg->r);
  }else{
    pthread_t thread0, thread1;
    PDucksortB_t Larg,Rarg;

    ValueT *x = parg->x;
    IndexT l = parg->l;
    IndexT r = parg->r;

    IndexT j;
    ValueT t, v;
    ValueT *L = x+l, *R = x+r;

    // DIET
    //while (EQ(*(++L), v)) if (L >= R) return;  // explicit stop of for loop
    // POET
    while (++L, LE(*(L-1), *(L))) if (L >= R) return NULL;  // explicit stop of for loop
    L = x+l;

#if SAMPLE_SIZE_PARALLEL > 0
    j = sample_pivot(x, l, r);
#else
#if USE_MEDIAN_OF_3_PARALLEL
    IndexT a,b,c,n=r-l+1;
    a = l+randIndex(n);
    b = l+randIndex(n);
    c = l+randIndex(n);
    j = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
#if USE_NINTHER_PARALLEL
    IndexT j2,j3;
    a = l+randIndex(n);
    b = l+randIndex(n);
    c = l+randIndex(n);
    j2 = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
    a = l+randIndex(n);
    b = l+randIndex(n);
    c = l+randIndex(n);
    j3 = LT(x[a], x[b ]) ? (LT(x[b ], x[c ]) ? b  : LT(x[a], x[c ]) ? c  : a)  : (LT(x[c ], x[b ]) ? b  : LT(x[c ], x[a]) ? c  : a);
    j  = LT(x[j], x[j2]) ? (LT(x[j2], x[j3]) ? j2 : LT(x[j], x[j3]) ? j3 : j)  : (LT(x[j3], x[j2]) ? j2 : LT(x[j3], x[j]) ? j3 : j);
#endif
#else
    j = l+randIndex(r-l+1);
#endif
#endif
    SWAP(x[j], x[l], v);  // first argument pivot now in v and x[l]

    //if (L < R){
    // Begin Block-Processing
    unsigned char indexL[QUICKSORT_BLOCKSIZE], indexR[QUICKSORT_BLOCKSIZE];
    IndexT iL = 0;
    IndexT iR = 0;
    IndexT sL = 0;
    IndexT sR = 0;
    IndexT num;
    while (R - L + 1 > 2 * QUICKSORT_BLOCKSIZE) {
      if (iR == 0) {
        sR = 0;
        for (j = 0; j < QUICKSORT_BLOCKSIZE; j++) {
          indexR[iR] = j;
          iR += LE((*(R - j)), v);
        }
      }
      if (iL == 0) {
        sL = 0;
        for (j = 0; j < QUICKSORT_BLOCKSIZE; j++) {
          indexL[iL] = j;
          iL += LT(v, L[j]);
        }
      }
      num = MIN(iL, iR);
      if (num != 0) {
        t = L[indexL[sL]];
        L[+indexL[sL]] = R[-indexR[sR]];
        for (j = 1; j < num; j++) {
          R[-indexR[sR + j - 1]] = L[+indexL[sL + j]];
          L[+indexL[sL + j    ]] = R[-indexR[sR + j]];
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
    // End Block-Processing
    L--;
    R++;

    for (;;){
      while(GT(*(--R), v));                      // sentinel stop guaranteed by pivot at the left
      while (LE(*(++L), v)) if (L >= R) break;  // explicit stop of for loop
      if (L >= R)break;
      SWAP(*L, *R, t);
    }

    j = R - x;
    SWAP(x[l], *R, t); // the index j from the first loop is guaranteed to be in a legal position

    // DucksortB_TieRight(x, l, j-1);
    // DucksortB_TieLeft(x, j+1, r);
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

    pthread_create(&thread0, NULL, PDucksortB_TieRight, &Larg);
    pthread_create(&thread1, NULL, PDucksortB_TieLeft, &Rarg);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}



void PDucksortB_insitu(ValueT *x, IndexT n, double t)
{
  //DucksortB(x, 0, n-1, t);
  PDucksortB_t arg;
  arg.x = x;
  arg.l = 0;
  arg.r = n-1;
  arg.threads = t;
  PDucksortB_TieLeft(&arg);
}

void PDucksortB_exsitu(ValueT *x, IndexT n, double t)
{
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  Scopy(aux, x, n, t);

  //DucksortB(aux, 0, n-1, t);
  PDucksortB_t arg;
  arg.x = aux;
  arg.l = 0;
  arg.r = n-1;
  arg.threads = t;
  PDucksortB_TieLeft(&arg);

  Scopy(x, aux, n, t);
  FREE(aux);
}

#undef USE_MEDIAN_OF_3_PIVOT
