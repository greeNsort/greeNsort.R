/*
# greeNsort parallel Frogsort1 (pointer implementation) 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include <pthread.h>
#include "ntile2.h"
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


static void Frogsort1P_init_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT r
, IndexT R
);
static void Frogsort1P_init_left(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT l
, IndexT L
){
  IndexT nr;
  //Rprintf("Frogsort1P_init_left l=%d r=%d L=%d R=%d\n", l, l + n - 1, L, L + n - 1); R_FlushConsole();
  if (n <=
#if INSERTIONSORT_LIMIT > 2
  INSERTIONSORT_LIMIT
#else
  2
#endif
    ){
    x += l;
    y += L;
    for (nr=0; nr<n; nr++){
      //Rprintf("%p %f = %p %f\n", y+nr, y[nr], x+nr, x[nr]);
      y[nr] = x[nr];
    }
    //Insertionsort_l2r(y, 0, n - 1);
  }else{
    nr = n  / 2;
    Frogsort1P_init_left (x, y, n - nr, l    , L       );
    Frogsort1P_init_right(x, y, nr    , l+n-1, L+n+nr-1);
  }
}
static void Frogsort1P_init_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT r
, IndexT R
){
  IndexT nl;
  //Rprintf("Frogsort1P_init_right l=%d r=%d L=%d R=%d\n", r - n + 1, r, R - n + 1, R); R_FlushConsole();
  if (n <=
#if INSERTIONSORT_LIMIT > 2
      INSERTIONSORT_LIMIT
#else
      2
#endif
  ){
    x += r - n + 1;
    y += R - n + 1;
    //for (nl=n-1; nl >= 0; nl--)  // to avoid inplace overwrite
    for (nl=0; nl < n; nl++){
      //Rprintf("%p %f = %p %f\n", y+nl, y[nl], x+nl, x[nl]);
      y[nl] = x[nl];
    }
    //Insertionsort_l2r(y, 0, n - 1);
  }else{
    nl = n  / 2;
    Frogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    Frogsort1P_init_right(x, y, n - nl, r    , R       );
  }
}




static void Frogsort1P_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
);
static void Frogsort1P_sort_left(
    ValueT *y
  , IndexT n
  , IndexT L
){
  if (n <=
#if INSERTIONSORT_LIMIT > 2
      INSERTIONSORT_LIMIT
#else
        2
#endif
  ){
    y += L;
    Insertionsort_l2r(y, 0, n - 1);
  }else{
    IndexT nr = n  / 2;
    Frogsort1P_sort_left (y, n - nr, L       );
    Frogsort1P_sort_right(y, nr    , L+n+nr-1);
    FrogmergeP_asc_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1));
  }
}
static void Frogsort1P_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
){
  if (n <=
#if INSERTIONSORT_LIMIT > 2
      INSERTIONSORT_LIMIT
#else
        2
#endif
  ){
    y += R - n + 1;
    Insertionsort_l2r(y, 0, n - 1);
  }else{
    IndexT nl = n  / 2;
    Frogsort1P_sort_left (y, nl    , R-n-nl+1);
    Frogsort1P_sort_right(y, n - nl, R       );
    FrogmergeP_asc_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R);
  }
}



typedef struct PFrogsort1P_init_left_t{
  ValueT *x;
  ValueT *y;
  IndexT n;
  IndexT l;
  IndexT L;
  double threads;
} PFrogsort1P_init_left_t;
typedef struct PFrogsort1P_init_right_t{
  ValueT *x;
  ValueT *y;
  IndexT n;
  IndexT r;
  IndexT R;
  double threads;
} PFrogsort1P_init_right_t;

typedef struct PFrogsort1P_sort_left_t{
  ValueT *y;
  IndexT n;
  IndexT L;
  double threads;
} PFrogsort1P_sort_left_t;
typedef struct PFrogsort1P_sort_right_t{
  ValueT *y;
  IndexT n;
  IndexT R;
  double threads;
} PFrogsort1P_sort_right_t;





static void * PFrogsort1P_init_right(void *arg);
static void * PFrogsort1P_init_left(void *arg){
  PFrogsort1P_init_left_t *parg = (PFrogsort1P_init_left_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort1P_init_left(parg->x, parg->y, parg->n, parg->l, parg->L);
  }else{
    pthread_t thread0, thread1;
    PFrogsort1P_init_left_t  L;
    PFrogsort1P_init_right_t R;

    IndexT nr = parg->n  / 2;

    //Frogsort1P_init_left (x, y, n - nr, l    , L       );
    L.x = parg->x;
    L.y = parg->y;
    L.n = parg->n - nr;
    L.l = parg->l;
    L.L = parg->L;
    L.threads = parg->threads / 2.0;
    //Frogsort1P_init_right(x, y, nr    , l+n-1, L+n+nr-1);
    R.x = parg->x;
    R.y = parg->y;
    R.n = nr;
    R.r = parg->l + parg->n - 1;
    R.R = parg->L + parg->n + nr - 1;
    R.threads = L.threads;

    // PFrogsort1P_init_left(&L);
    // PFrogsort1P_init_right(&R);

    pthread_create(&thread0, NULL, PFrogsort1P_init_left, &L);
    pthread_create(&thread1, NULL, PFrogsort1P_init_right, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}
static void * PFrogsort1P_init_right(void *arg){
  PFrogsort1P_init_right_t *parg = (PFrogsort1P_init_right_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort1P_init_right(parg->x, parg->y, parg->n, parg->r, parg->R);
  }else{
    pthread_t thread0, thread1;
    PFrogsort1P_init_left_t L;
    PFrogsort1P_init_right_t R;

    IndexT nl = parg->n  / 2;

    //Frogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    L.x = parg->x;
    L.y = parg->y;
    L.n = nl;
    L.l = parg->r - parg->n + 1;
    L.L = parg->R - parg->n - nl + 1;
    L.threads = parg->threads / 2.0;
    //Frogsort1P_init_right(x, y, n - nl, r    , R       );
    R.x = parg->x;
    R.y = parg->y;
    R.n = parg->n - nl;
    R.r = parg->r;
    R.R = parg->R;
    R.threads = L.threads;

    // PFrogsort1P_init_left(&L);
    // PFrogsort1P_init_right(&R);

    pthread_create(&thread0, NULL, PFrogsort1P_init_left, &L);
    pthread_create(&thread1, NULL, PFrogsort1P_init_right, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}





static void * PFrogsort1P_sort_right(void *arg);
static void * PFrogsort1P_sort_left(void *arg){
  PFrogsort1P_sort_left_t *parg = (PFrogsort1P_sort_left_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort1P_sort_left(parg->y, parg->n, parg->L);
  }else{
    PFrogsort1P_sort_left_t  L;
    PFrogsort1P_sort_right_t R;

    IndexT nr = parg->n  / 2;

    //Frogsort1P_sort_left (y, n - nr, L       );
    L.y = parg->y;
    L.n = parg->n - nr;
    L.L = parg->L;
    L.threads = parg->threads / 2.0;
    //Frogsort1P_sort_right(y, nr    , L+n+nr-1);
    R.y = parg->y;
    R.n = nr;
    R.R = parg->L + parg->n + nr - 1;
    R.threads = L.threads;

    // PFrogsort1P_sort_left(&L);
    // PFrogsort1P_sort_right(&R);

    pthread_t thread0, thread1;
    pthread_create(&thread0, NULL, PFrogsort1P_sort_left, &L);
    pthread_create(&thread1, NULL, PFrogsort1P_sort_right, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);

    SFrogmergeP_asc_left(
      /* x*/ parg->y + parg->L + parg->n - 1
    , /*ll*/ parg->y + parg->L
      , /*lr*/ parg->y + (parg->L + (parg->n - nr) - 1)
      , /*rl*/ parg->y + (parg->L + parg->n)
      , /*rr*/ parg->y + (parg->L + parg->n + nr - 1)
      , parg->threads
    );
  }
  return NULL;
}

static void * PFrogsort1P_sort_right(void *arg){
  PFrogsort1P_sort_right_t *parg = (PFrogsort1P_sort_right_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort1P_sort_right(parg->y, parg->n, parg->R);
  }else{
    PFrogsort1P_sort_left_t L;
    PFrogsort1P_sort_right_t R;

    IndexT nl = parg->n  / 2;

    //Frogsort1P_sort_left (y, nl    , R-n-nl+1);
    L.y = parg->y;
    L.n = nl;
    L.L = parg->R - parg->n - nl + 1;
    L.threads = parg->threads / 2.0;
    //Frogsort1P_sort_right(y, n - nl, R       );
    R.y = parg->y;
    R.n = parg->n - nl;
    R.R = parg->R;
    R.threads = L.threads;

    // PFrogsort1P_sort_left(&L);
    // PFrogsort1P_sort_right(&R);

    pthread_t thread0, thread1;
    pthread_create(&thread0, NULL, PFrogsort1P_sort_left, &L);
    pthread_create(&thread1, NULL, PFrogsort1P_sort_right, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);

    SFrogmergeP_asc_right(
      /* x*/ parg->y + parg->R - parg->n + 1
    , /*ll*/ parg->y + (parg->R - parg->n - nl + 1)
      , /*lr*/ parg->y + (parg->R - parg->n)
      , /*rl*/ parg->y + (parg->R - (parg->n - nl) + 1)
      , /*rr*/ parg->y + parg->R
      , parg->threads
    );
  }
  return NULL;
}


static void SFrogsort1P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double threads
);

static void DFrogsort1P_init_left(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT l
  , IndexT L
  , double threads
){
  //Rprintf("DFrogsort1P_init_left l=%d r=%d L=%d R=%d\n", l, l + n - 1, L, L + n - 1); R_FlushConsole();
  if (threads <= 1 || (n <= THREAD_LIMIT_SORT)){
    Frogsort1P_init_left(x, y, n, l, L);
  }else{
    IndexT nr = n  / 2;

    //Frogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    PFrogsort1P_init_left_t arg;
    arg.x = x;
    arg.y = y;
    arg.n = n - nr;
    arg.l = l;
    arg.L = L;
    arg.threads = threads;
    PFrogsort1P_init_left(&arg);

    SFrogsort1P_init_right(x, y, nr    , l+n-1, L+n+nr-1, threads);
  }
}

static void DFrogsort1P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double threads
){
  //Rprintf("DFrogsort1P_init_right l=%d r=%d L=%d R=%d\n", r - n + 1, r, R - n + 1, R); R_FlushConsole();
  if (threads <= 1 || (n <= THREAD_LIMIT_SORT)){
    Frogsort1P_init_right(x, y, n, r, R);
  }else{
    IndexT nl = n  / 2;

    //Frogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    PFrogsort1P_init_left_t arg;
    arg.x = x;
    arg.y = y;
    arg.n = nl;
    arg.l = r-n+1;
    arg.L = R-n-nl+1;
    arg.threads = threads;
    PFrogsort1P_init_left(&arg);

    //Frogsort1P_init_right(x, y, n - nl, r    , R);
    SFrogsort1P_init_right(x, y, n - nl, r    , R       , threads);
  }
}





static void SFrogsort1P_init_left(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT l
  , IndexT L
  , double threads
){
  //Rprintf("SFrogsort1P_init_left l=%d r=%d L=%d R=%d\n", l, l + n - 1, L, L + n - 1); R_FlushConsole();
  if (threads <= 1 || (n <= THREAD_LIMIT_SORT)){
    Frogsort1P_init_left(x, y, n, l, L);
  }else{
    IndexT nr = n  / 2;
    DFrogsort1P_init_left (x, y, n - nr, l    , L       , threads);
    SFrogsort1P_init_right(x, y, nr    , l+n-1, L+n+nr-1, threads);
  }
}
static void SFrogsort1P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double threads
){
  //Rprintf("SFrogsort1P_init_right l=%d r=%d L=%d R=%d\n", r - n + 1, r, R - n + 1, R); R_FlushConsole();
  if (threads <= 1 || (n <= THREAD_LIMIT_SORT)){
    Frogsort1P_init_right(x, y, n, r, R);
  }else{
    IndexT nl = n  / 2;
    DFrogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1, threads);
    SFrogsort1P_init_right(x, y, n - nl, r    , R       , threads);
  }
}
















void PFrogsort1P_insitu(ValueT *x, IndexT n, double t)
{
  // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
  if (n<3){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
  IndexT nr = (n / 3) * 2;
  IndexT nl = n - nr;
  IndexT bl = nl / 2;
  IndexT br = nr / 2;
  ValueT *aux = (ValueT *) MALLOC(nl + bl, ValueT);

  // Frogsort1P_init_left(x      , aux, nl, 0     , 0         );
  PFrogsort1P_init_left_t Linit;
  Linit.x = x;
  Linit.y = aux;
  Linit.n = nl;
  Linit.l = 0;
  Linit.L = 0;
  Linit.threads = t;
  PFrogsort1P_init_left(&Linit);

  // must not be full parallel, either serial or segmented parallel
  Frogsort1P_init_right(x + nl, x + (n - nr - br), nr, nr - 1      , nr + br - 1);
  //SFrogsort1P_init_right(x + nl, x + (n - nr - br), nr, nr - 1      , nr + br - 1, t);

  //Frogsort1P_sort_left(aux, nl, 0);
  PFrogsort1P_sort_left_t Lsort;
  Lsort.y = aux;
  Lsort.n = nl;
  Lsort.L = 0;
  Lsort.threads = t;  // / 3.0

  //Frogsort1P_sort_right(x + (n - nr - br), nr, nr + br - 1);
  PFrogsort1P_sort_right_t Rsort;
  Rsort.y = x + (n - nr - br);
  Rsort.n = nr;
  Rsort.R = nr + br - 1;
  Rsort.threads = t; //  * 2.0/3.0

  // running Lsort and Rsort in parallel is slower due to page faults
  PFrogsort1P_sort_right(&Rsort);
  PFrogsort1P_sort_left(&Lsort);
  // pthread_t thread0, thread1;
  // pthread_create(&thread1, NULL, PFrogsort1P_sort_right, &Rsort);
  // pthread_create(&thread0, NULL, PFrogsort1P_sort_left, &Lsort);
  // pthread_join(thread1, NULL);
  // pthread_join(thread0, NULL);

  // FrogmergeP_asc_right(x     , aux, aux+nl-1 , x+n-nr , x+n-1);
  FrogmergeP_asc_t arg;
  arg.x = x;
  arg.ll = aux;
  arg.lr = aux+nl-1;
  arg.rl = x+n-nr;
  arg.rr = x+n-1;
  PFrogmergeP_asc_right(&arg);
  // SFrogmergeP_asc_right(
  //   x        //ValueT *x
  // , aux      //ValueT *ll
  // , aux+nl-1 //ValueT *lr
  // , x+n-nr   //ValueT *rl
  // , x+n-1    //ValueT *rr
  // , t        //double maxt
  // );

  FREE(aux);
  return;
}


void PFrogsort1P_exsitu(ValueT *x, IndexT n, double t)
{
  // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
  if (n<3){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }

  // standard solution for fair comparison
  IndexT b = n / 2;
  IndexT naux = n + b;
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);

  //Frogsort1P_init_left(x, aux, n, 0, 0);
  PFrogsort1P_init_left_t Linit;
  Linit.x = x;
  Linit.y = aux;
  Linit.n = n;
  Linit.l = 0;
  Linit.L = 0;
  Linit.threads = t;
  PFrogsort1P_init_left(&Linit);

  //Frogsort1P_sort_left(aux, n, 0);
  PFrogsort1P_sort_left_t Lsort;
  Lsort.y = aux;
  Lsort.n = n;
  Lsort.L = 0;
  Lsort.threads = t;
  PFrogsort1P_sort_left(&Lsort);

  Scopy(x, aux, n, t);

  FREE(aux);
  return;
}

