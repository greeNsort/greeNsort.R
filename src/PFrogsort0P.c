/*
# greeNsort parallel Frogsort0 (pointer implementation) 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "PFrogmergeP.h"
#include "Insertionsort_l2r.h"

/*
reach = outer + inner + inner

Even Case:
inner = 50%
outer = 50%
reach = 150%

Odd Case:
inner = 50% -1
outer = 50% +1
reach = 150% -1

hence never overlap qed
*/

//#define DEBUG_PRINT
#undef DEBUG_PRINT



static void Frogsort0P_init(
    ValueT *x
  , ValueT *y
  , IndexT n
){
  IndexT L=0,l=0;
  n--;
  while(l < n){
    y[L] = x[l];
    y[L+1] = 0;
    y[L+2] = x[l+1];
    L+=3;
    l+=2;
  }
  if (l==n){
    y[L] = x[l];
  }
}

typedef struct PFrogsort0P_init_t{
  ValueT *x;
  ValueT *y;
  IndexT n;
} PFrogsort0P_init_t;

static void * PFrogsort0P_init(void *arg){
  PFrogsort0P_init_t *parg = (PFrogsort0P_init_t*)arg;
  ValueT *x = parg->x;
  ValueT *y = parg->y;
  IndexT n = parg->n - 1;
  IndexT L=0,l=0;
  while(l < n){
    y[L] = x[l];
    y[L+1] = 0;
    y[L+2] = x[l+1];
    L+=3;
    l+=2;
  }
  if (l==n){
    y[L] = x[l];
  }
  return NULL;
}


static void SFrogsort0P_init(
    ValueT *x
  , ValueT *y
  , IndexT n
  , double maxt
){
  if (maxt <= 1 || (n <= THREAD_LIMIT_SORT)){
    Frogsort0P_init(x, y, n);
  }else{
    // non-segmented parallelization
    int i, t = ceilf(maxt);
    t = MIN(t, n / THREAD_LIMIT_SORT);
    pthread_t *threads = (pthread_t *) MALLOC(t, pthread_t);
    PFrogsort0P_init_t *args = (PFrogsort0P_init_t *) MALLOC(t, PFrogsort0P_init_t);
    IndexT d1 = (n/2) / t;  // d2 must be even
    IndexT d2 = d1 + d1;
    IndexT d3 = d2 + d1;
    IndexT n2 = 0;
    IndexT n3 = 0;
    for (i = 0; i<t; i++){
      args[i].x = x + n2;
      args[i].y = y + n3;
      if (i+1 < t)
        args[i].n = d2;
      else
        args[i].n = n - n2;
      //PFrogsort0P_init(args+i);
      pthread_create(threads+i, NULL, PFrogsort0P_init, args+i);
      n2 += d2;
      n3 += d3;
    }
    for (i = 0; i<t; i++){
      pthread_join(threads[i], NULL);
    }
    FREE(args);
    FREE(threads);
  }
}
static void SFrogsort0P_init_insitu(
    ValueT *x
  , ValueT *y
  , IndexT n
  , double maxt
){
  // non-segmented parallelization
  int i, t = ceilf(maxt);
  pthread_t *threads = (pthread_t *) MALLOC(t, pthread_t);
  PFrogsort0P_init_t *args = (PFrogsort0P_init_t *) MALLOC(t, PFrogsort0P_init_t);
  // we must work in even pairs, and start with not more than a third
  // Rprintf("n=%d x-y=%d t=%d \n", n, x-y, t); R_FlushConsole();
  IndexT nbuff = x - y;
  IndexT next1 = nbuff/3;
  IndexT d1,d2,d3,n2,n3;
  n2 = 0;
  n3 = 0;
  while(next1 > THREAD_LIMIT_SORT){
    t = MIN(t, (next1+next1) / THREAD_LIMIT_SORT);
    // Rprintf("n=%d nbuff=%d t=%d next1=%d  \n", n, nbuff, t, next1); R_FlushConsole();
    d1 = next1 / t;  // thread size in pairs
    d2 = d1 + d1;    // thread size in single elements
    d3 = d2 + d1;    // thread size in single elements including buffer
    for (i = 0; i<t; i++){
      // Rprintf("thread i=%d n2=%d n3=%d d1=%d d2=%d d3=%d\n", i, n2, n3, d1, d2, d3); R_FlushConsole();
      args[i].x = x+n2;
      args[i].y = y+n3;
      args[i].n = d2;
      //PFrogsort0P_init(args+i);
      pthread_create(threads+i, NULL, PFrogsort0P_init, args+i);
      n2 += d2;
      n3 += d3;
    }
    for (i = 0; i<t; i++){
      pthread_join(threads[i], NULL);
    }
    next1 = (nbuff + n2 - n3) / 3;
    // Rprintf("next1=%d\n", next1); R_FlushConsole();
  }
  if (n2 < n){
    // Rprintf("final n2=%d n3=%d n - n2=%d\n", n2, n3, n - n2); R_FlushConsole();
    Frogsort0P_init(x+n2, y+n3, n - n2);
  }
  FREE(args);
  FREE(threads);
}


#if INSERTIONSORT_LIMIT > 0

static void Frogsort0P_TunedCase_left_even(
    ValueT *x // pointer to working memory
  , IndexT l     // leftmost triplet to sort
  , IndexT n     // number of triplets to sort
){

  IndexT i=1,j=2;
  x += 3*l;
  n += n;
  // the first element needs no copying, hence we handle the first triplet outside of the loop
  x[i++] = x[j++];
  while(i<n){
    x[i] = x[j];
    x[i+1] = x[j+2];
    i+=2;
    j+=3;
  }
  Insertionsort_l2r(x, 0, n-1);
}

static void Frogsort0P_TunedCase_right_even(
    ValueT *x // pointer to working memory
  , IndexT l     // leftmost triplet to sort
  , IndexT n     // number of triplets to sort
){
  IndexT i,j;
  x += 3*l;
  i = 3*n-2;
  j = i-1;
  // the first element needs no copying, hence we handle the first triplet outside of the loop
  x[i--] = x[j--];
  while(j>0){
    x[i] = x[j];
    x[i-1] = x[j-2];
    i -= 2;
    j -=3;
  }
  Insertionsort_l2r(x+n, 0, n+n-1);
}

static void Frogsort0P_TunedCase_left_odd(
    ValueT *x // pointer to working memory
  , IndexT l     // leftmost triplet to sort
  , IndexT n     // number of triplets to sort
){
  IndexT i=1,j=2;
  x += 3*l;
  n += n - 2;
  // the first element needs no copying, hence we handle the first triplet outside of the loop
  x[i++] = x[j++];
  while(i<n){
    x[i] = x[j];
    x[i+1] = x[j+2];
    i+=2;
    j+=3;
  }
  // now the last (odd) element
  x[i] = x[j];
  Insertionsort_l2r(x, 0, n); // n-1 - 1odd = n - 2 and n had already -=2 hence n
}

static void Frogsort0P_TunedCase_right_odd(
    ValueT *x // pointer to working memory
  , IndexT l     // leftmost triplet to sort
  , IndexT n     // number of triplets to sort
){
  IndexT i,j;
  x += 3*l;
  // the first triplet has only one element in the leftmost position (goes to the rightmost)
  i = 3*n-1;
  j = i-2;
  x[i--] = x[j--];
  while(j>0){
    x[i] = x[j];
    x[i-1] = x[j-2];
    i -= 2;
    j -=3;
  }
  Insertionsort_l2r(x+n, 1, n+n-1);
}

#else

static void Frogsort0P_BaseCase_left_even(
    ValueT *x // pointer to triplet
){
  if (LT(x[2], x[0])){
    x[1] = x[0];
    x[0] = x[2];
  }else{
    x[1] = x[2];
  }
}

static void Frogsort0P_BaseCase_right_even(
    ValueT *x // pointer to triplet
){
  if (LT(x[2], x[0])){
    x[1] = x[2];
    x[2] = x[0];
  }else{
    x[1] = x[0];
  }
}

static void Frogsort0P_BaseCase_left_odd(
    ValueT *x // pointer to triplet
){
  // nothing to do
}

static void Frogsort0P_BaseCase_right_odd(
    ValueT *x // pointer to triplet
){
    x[2] = x[0];
}

#endif


static void Frogsort0P_sort_right_even(
    ValueT *x // pointer to triplets
  , IndexT l     // leftmost triplet to sort
  , IndexT n     // number of triplets to sort
);
static void Frogsort0P_sort_left_even(
    ValueT *x // pointer to triplets
  , IndexT l     // leftmost triplet to sort
  , IndexT n     // number of triplets to sort
){
#ifdef DEBUG_PRINT
  Rprintf("Frogsort0P_sort_left_even l=%d n=%d\n", l, n); R_FlushConsole();
#endif
#if INSERTIONSORT_LIMIT > 0
  if (n <= TRIPLET_LIMIT){
    Frogsort0P_TunedCase_left_even(x,l,n);
  }
#else
  if (n==1){
    Frogsort0P_BaseCase_left_even(x+3*l);
  }
#endif
  else if(n>1){
    IndexT m = n - n/2; // FrogmergeP_asc_left must have m >= n-m
    Frogsort0P_sort_left_even (x, l  ,   m);
    Frogsort0P_sort_right_even(x, l+m, n-m);
    FrogmergeP_asc_left(x + 3*l+n+n-1, x + 3*l, x + 3*l+m+m-1, x + 3*l+m+m+n, x + 3*(l+n)-1);
  }
#ifdef DEBUG_PRINT
  Rprintf("- Frogsort0P_sort_left_even l=%d n=%d\n", l, n); R_FlushConsole();
  for (int i=3*l; i<(3*(l+n)); i++){
    Rprintf("%d=%f\n", i, x[i]);  R_FlushConsole();
  }
#endif
}
static void Frogsort0P_sort_right_even(
    ValueT *x // pointer to triplets
  , IndexT l     // leftmost triplet to sort
  , IndexT n     // number of triplets to sort
){
#ifdef DEBUG_PRINT
  Rprintf("Frogsort0P_sort_right_even l=%d n=%d\n", l, n);  R_FlushConsole();
#endif
#if INSERTIONSORT_LIMIT > 0
  if (n <= TRIPLET_LIMIT){
    Frogsort0P_TunedCase_right_even(x,l,n);
  }
#else
  if (n==1){
    Frogsort0P_BaseCase_right_even(x+3*l);
  }
#endif
  else if(n>1){
    IndexT m = n/2; // FrogmergeP_asc_right must have m <= n-m
    Frogsort0P_sort_left_even (x, l  ,   m);
    Frogsort0P_sort_right_even(x, l+m, n-m);
    FrogmergeP_asc_right(x + 3*l+n, x + 3*l, x + 3*l+m+m-1, x + 3*l+m+m+n, x + 3*(l+n)-1);
  }
#ifdef DEBUG_PRINT
  Rprintf("- Frogsort0P_sort_right_even l=%d n=%d\n", l, n);  R_FlushConsole();
  for (int i=3*l; i<(3*(l+n)); i++){
    Rprintf("%d=%f\n", i, x[i]);  R_FlushConsole();
  }
#endif
}


static void Frogsort0P_sort_right_odd(
    ValueT *x // pointer to triplets
  , IndexT l     // leftmost triplet to sort
  , IndexT n     // number of triplets to sort
);
static void Frogsort0P_sort_left_odd(
    ValueT *x // pointer to triplets
  , IndexT l     // leftmost triplet to sort
  , IndexT n     // number of triplets to sort
){
#ifdef DEBUG_PRINT
  Rprintf("Frogsort0P_sort_left_odd l=%d n=%d\n", l, n);  R_FlushConsole();
#endif
#if INSERTIONSORT_LIMIT > 0
  if (n <= TRIPLET_LIMIT){
    Frogsort0P_TunedCase_left_odd(x,l,n);
  }
#else
  if (n==1){
    Frogsort0P_BaseCase_left_odd(x+3*l);
  }
#endif
  else if(n>1){
    IndexT m = n - n/2; // FrogmergeP_asc_left must have m >= n-m
    Frogsort0P_sort_left_even (x, l  ,   m);
    Frogsort0P_sort_right_odd(x, l+m, n-m);
    FrogmergeP_asc_left(x + 3*l+n+n-1 - 1, x + 3*l, x + 3*l+m+m-1, x + 3*l+m+m+n + 1, x + 3*(l+n)-1);  // right stream has only 2*ntri-1 elements
  }
#ifdef DEBUG_PRINT
  Rprintf("- Frogsort0P_sort_left_odd l=%d n=%d\n", l, n);  R_FlushConsole();
  for (int i=3*l; i<(3*(l+n)); i++){
    Rprintf("%d=%f\n", i, x[i]);  R_FlushConsole();
  }
#endif
}
static void Frogsort0P_sort_right_odd(
    ValueT *x // pointer to triplets
  , IndexT l     // leftmost triplet to sort
  , IndexT n     // number of triplets to sort
){
#ifdef DEBUG_PRINT
 Rprintf("Frogsort0P_sort_right_odd l=%d n=%d\n", l, n);  R_FlushConsole();
#endif
#if INSERTIONSORT_LIMIT > 0
  if (n <= TRIPLET_LIMIT){
    Frogsort0P_TunedCase_right_odd(x,l,n);
  }
#else
  if (n==1){
    Frogsort0P_BaseCase_right_odd(x+3*l);
  }
#endif
  else if(n>1){
    IndexT m = n/2; // FrogmergeP_asc_right must have m <= n-m
    Frogsort0P_sort_left_even (x, l  ,   m);
    Frogsort0P_sort_right_odd(x, l+m, n-m);
    FrogmergeP_asc_right(x + 3*l+n + 1, x + 3*l, x + 3*l+m+m-1, x + 3*l+m+m+n + 1, x + 3*(l+n)-1); // right stream has only 2*ntri-1 elements
  }
#ifdef DEBUG_PRINT
  Rprintf("- Frogsort0P_sort_right_odd l=%d n=%d\n", l, n);  R_FlushConsole();
  for (int i=3*l; i<(3*(l+n)); i++){
    Rprintf("%d=%f\n", i, x[i]);  R_FlushConsole();
  }
#endif
}



typedef struct Frogsort0P_sort_t{
  ValueT *x;
  IndexT l;
  IndexT n;
  double threads;
} Frogsort0P_sort_t;


static void * PFrogsort0P_sort_right_even(void *arg);
static void * PFrogsort0P_sort_left_even(void *arg){
  Frogsort0P_sort_t *parg = (Frogsort0P_sort_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort0P_sort_left_even(parg->x, parg->l, parg->n);
  }else{
    pthread_t thread0, thread1;
    Frogsort0P_sort_t  L, R;

    IndexT m = parg->n - parg->n/2; // FrogmergeP_asc_left must have m >= n-m

    //PFrogsort0P_sort_left_even (x, l, m);
    L.x = parg->x;
    L.l = parg->l;
    L.n = m;
    L.threads = parg->threads / 2.0;
    //PFrogsort0P_sort_right_even(x, l+m, n-m);
    R.x = parg->x;
    R.l = parg->l + m;
    R.n = parg->n - m;
    R.threads = L.threads;

    // PFrogsort0P_sort_left_even(&L);
    // PFrogsort0P_sort_right_even(&R);
    pthread_create(&thread0, NULL, PFrogsort0P_sort_left_even, &L);
    pthread_create(&thread1, NULL, PFrogsort0P_sort_right_even, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);

    SFrogmergeP_asc_left(
      /* x*/ parg->x + 3*parg->l + parg->n + parg->n - 1
    , /*ll*/ parg->x + 3*parg->l
    , /*lr*/ parg->x + 3*parg->l + m+m-1
    , /*rl*/ parg->x + 3*parg->l + m+m + parg->n
    , /*rr*/ parg->x + 3*(parg->l + parg->n) - 1
    , parg->threads
    );
  }
  return NULL;
}
static void * PFrogsort0P_sort_right_even(void *arg){
  Frogsort0P_sort_t *parg = (Frogsort0P_sort_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort0P_sort_right_even(parg->x, parg->l, parg->n);
  }else{
    pthread_t thread0, thread1;
    Frogsort0P_sort_t  L, R;

    IndexT m = parg->n/2; // FrogmergeP_asc_right must have m <= n-m

    //PFrogsort0P_sort_left_even (x, l  ,   m);
    L.x = parg->x;
    L.l = parg->l;
    L.n = m;
    L.threads = parg->threads / 2.0;
    //PFrogsort0P_sort_right_even(x, l+m, n-m);
    R.x = parg->x;
    R.l = parg->l + m;
    R.n = parg->n - m;
    R.threads = L.threads;

    // PFrogsort0P_sort_left_even(&L);
    // PFrogsort0P_sort_right_even(&R);
    pthread_create(&thread0, NULL, PFrogsort0P_sort_left_even, &L);
    pthread_create(&thread1, NULL, PFrogsort0P_sort_right_even, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);

    SFrogmergeP_asc_right(
      /* x*/ parg->x + 3*parg->l + parg->n
    , /*ll*/ parg->x + 3*parg->l
    , /*lr*/ parg->x + 3*parg->l + m+m-1
    , /*rl*/ parg->x + 3*parg->l + m+m + parg->n
    , /*rr*/ parg->x + 3*(parg->l + parg->n) - 1
    , parg->threads
    );
  }
  return NULL;
}


static void * PFrogsort0P_sort_right_odd(void *arg);
static void * PFrogsort0P_sort_left_odd(void *arg){
  Frogsort0P_sort_t *parg = (Frogsort0P_sort_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort0P_sort_left_odd(parg->x, parg->l, parg->n);
  }else{
    pthread_t thread0, thread1;
    Frogsort0P_sort_t  L, R;

    IndexT m = parg->n - parg->n/2; // FrogmergeP_asc_left must have m >= n-m

    //PFrogsort0P_sort_left_even (x, l, m);
    L.x = parg->x;
    L.l = parg->l;
    L.n = m;
    L.threads = parg->threads / 2.0;
    //PFrogsort0P_sort_right_even(x, l+m, n-m);
    R.x = parg->x;
    R.l = parg->l + m;
    R.n = parg->n - m;
    R.threads = L.threads;

    // PFrogsort0P_sort_left_odd(&L);
    // PFrogsort0P_sort_right_odd(&R);
    pthread_create(&thread0, NULL, PFrogsort0P_sort_left_even, &L);
    pthread_create(&thread1, NULL, PFrogsort0P_sort_right_odd, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);

    SFrogmergeP_asc_left(
      /* x*/ parg->x + 3*parg->l + parg->n + parg->n - 1    - 1 // right stream has only 2*ntri-1 elements
    , /*ll*/ parg->x + 3*parg->l
      , /*lr*/ parg->x + 3*parg->l + m+m-1
    , /*rl*/ parg->x + 3*parg->l + m+m + parg->n            + 1 // right stream has only 2*ntri-1 elements
    , /*rr*/ parg->x + 3*(parg->l + parg->n) - 1
    , parg->threads
    );
  }
  return NULL;
}
static void * PFrogsort0P_sort_right_odd(void *arg){
  Frogsort0P_sort_t *parg = (Frogsort0P_sort_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort0P_sort_right_odd(parg->x, parg->l, parg->n);
  }else{
    pthread_t thread0, thread1;
    Frogsort0P_sort_t  L, R;

    IndexT m = parg->n/2; // FrogmergeP_asc_right must have m <= n-m

    //PFrogsort0P_sort_left_even (x, l  ,   m);
    L.x = parg->x;
    L.l = parg->l;
    L.n = m;
    L.threads = parg->threads / 2.0;
    //PFrogsort0P_sort_right_odd(x, l+m, n-m);
    R.x = parg->x;
    R.l = parg->l + m;
    R.n = parg->n - m;
    R.threads = L.threads;

    // PFrogsort0P_sort_left_even(&L);
    // PFrogsort0P_sort_right_odd(&R);
    pthread_create(&thread0, NULL, PFrogsort0P_sort_left_even, &L);
    pthread_create(&thread1, NULL, PFrogsort0P_sort_right_odd, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);

    SFrogmergeP_asc_right(
      /* x*/ parg->x + 3*parg->l + parg->n               + 1 // right stream has only 2*ntri-1 elements
    , /*ll*/ parg->x + 3*parg->l
    , /*lr*/ parg->x + 3*parg->l + m+m-1
    , /*rl*/ parg->x + 3*parg->l + m+m + parg->n         + 1 // right stream has only 2*ntri-1 elements
    , /*rr*/ parg->x + 3*(parg->l + parg->n) - 1
      , parg->threads
    );
  }
  return NULL;
}


void PFrogsort0P_insitu(ValueT *x, IndexT n, double t)
{
  // Special case: up to n == 5 we switch to insertion sort because naux would be larger than n
  if (n<6){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
  IndexT rtri = n/3;       // number of triplets that fit into original
  IndexT ntri = n/2 + n%2; // total number of triplets needed
  IndexT ltri = ntri - rtri;
  IndexT nl = ltri+ltri;
  IndexT nr = n - nl;
  ValueT *aux = (ValueT *) MALLOC(3*ltri, ValueT);
  ValueT *ori = x + (n - rtri*3);
  //Frogsort0P_init(x, aux, nl);
  SFrogsort0P_init(x, aux, nl, t);
  //Frogsort0P_init(x+nl, ori, nr);
  SFrogsort0P_init_insitu(x+nl, ori, nr, t);

  // for (int i=0; i<(3*rtri); i++)
  //   Rprintf("%d=%f\n", i, ori[i]);

  Frogsort0P_sort_t  L,R;
  L.x = aux;
  L.l = 0;
  L.n = ltri;
  L.threads = t;
  PFrogsort0P_sort_left_even(&L);
  //Frogsort0P_sort_left_even(aux, 0, ltri);

  R.x = ori;
  R.l = 0;
  R.n = rtri;
  R.threads = t;

  if (nr%2){
    //Frogsort0P_sort_right_odd(ori, 0, rtri);
    PFrogsort0P_sort_right_odd(&R);
  }else{
    //Frogsort0P_sort_right_even(ori, 0, rtri);
    PFrogsort0P_sort_right_even(&R);
  }

  SFrogmergeP_asc_right(x, aux, aux + nl-1, ori + rtri+nr%2, ori + 3*rtri-1, t);

  FREE(aux);
  return;
}

void PFrogsort0P_exsitu(ValueT *x, IndexT n, double t)
{
  ValueT *aux;
  // Special case: up to n == 5 we switch to insertion sort because naux would be larger than n
  if (n<6){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
  IndexT ntri = n/2 + n%2;
  aux = (ValueT *) MALLOC(3*ntri, ValueT);

  //Frogsort0P_init(x, aux, n);
  SFrogsort0P_init(x, aux, n, t);

  Frogsort0P_sort_t  L;
  L.x = aux;
  L.l = 0;
  L.n = ntri;
  L.threads = t;

  if (n%2){
    //Frogsort0P_sort_left_odd(aux, 0, ntri);
    PFrogsort0P_sort_left_odd(&L);
  }else{
    //Frogsort0P_sort_left_even(aux, 0, ntri);
    PFrogsort0P_sort_left_even(&L);
  }

  //copy(x, aux, n);
  Scopy(x, aux, n, t);

  FREE(aux);
  return;
}

