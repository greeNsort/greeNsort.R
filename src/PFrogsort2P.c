/*
# greeNsort parallel Frogsort2 (pointer implementation) 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"

#include "PFrogmergeP.h"
#include "Insertionsort_l2r.h"

static void PFrogsort2P_Frogsort1P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
);
static void PFrogsort2P_Frogsort1P_init_left(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT l
  , IndexT L
){
  IndexT nr;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += l;
    y += L;
    for (nr=0; nr<n; nr++)
      y[nr] = x[nr];
    Insertionsort_l2r(y, 0, n - 1);
  }
#else
  ValueT t;
  if (n <= 2){
    if (n == 2){
      if (LT(x[l+1], x[l])){
        t = x[l+1];  // to avoid inplace overwrite
        y[L+1] = x[l];
        y[L] = t;  // to avoid inplace overwrite
      }else{
        y[L+1] = x[l+1];
        y[L] = x[l];
      }
    }else{
      y[L] = x[l];
    }
  }
#endif
  else{
    nr = n  / 2;
    PFrogsort2P_Frogsort1P_init_left (x, y, n - nr, l    , L       );
    PFrogsort2P_Frogsort1P_init_right(x, y, nr    , l+n-1, L+n+nr-1);
  }
}
static void PFrogsort2P_Frogsort1P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
){
  IndexT nl;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += r - n + 1;
    y += R - n + 1;
    for (nl=n-1; nl >= 0; nl--)  // to avoid inplace overwrite
      y[nl] = x[nl];
    Insertionsort_l2r(y, 0, n - 1);
  }
#else
  ValueT t;
  if (n <= 2){
    if (n == 2){
      if (LT(x[r], x[r-1])){
        t = x[r-1];  // to avoid inplace overwrite
        y[R-1] = x[r];
        y[R] = t;  // to avoid inplace overwrite
      }else{
        y[R-1] = x[r-1];
        y[R] = x[r];
      }
    }else{
      y[R] = x[r];
    }
  }
#endif
  else{
    nl = n  / 2;
    PFrogsort2P_Frogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    PFrogsort2P_Frogsort1P_init_right(x, y, n - nl, r    , R       );
  }
}


static void PFrogsort2P_Frogsort1P_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
);
static void PFrogsort2P_Frogsort1P_sort_left(
    ValueT *y
  , IndexT n
  , IndexT L
){
  IndexT nr;
#if INSERTIONSORT_LIMIT > 0
  if (n > INSERTIONSORT_LIMIT)
#else
    if (n > 2)
#endif
{
  nr = n  / 2;
  PFrogsort2P_Frogsort1P_sort_left (y, n - nr, L       );
  PFrogsort2P_Frogsort1P_sort_right(y, nr    , L+n+nr-1);
  FrogmergeP_asc_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1));
}
}
static void PFrogsort2P_Frogsort1P_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
){
  IndexT nl;
#if INSERTIONSORT_LIMIT > 0
  if (n > INSERTIONSORT_LIMIT)
#else
    if (n > 2)
#endif
{
  nl = n  / 2;
  PFrogsort2P_Frogsort1P_sort_left (y, nl    , R-n-nl+1);
  PFrogsort2P_Frogsort1P_sort_right(y, n - nl, R       );
  FrogmergeP_asc_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R);
}
}


void PFrogsort2P_Frogsort1P_insitu(ValueT *x, IndexT n, PerfT *p)
{
  // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
  if (n<3){
    Insertionsort_l2r(x, 0, n-1);
    p->secs = getNewSecs();
    p->size = 1;
    return;
  }
  IndexT nr = (n / 3) * 2;
  IndexT nl = n - nr;
  IndexT bl = nl / 2;
  IndexT br = nr / 2;
  IndexT naux = nl + bl;
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  PFrogsort2P_Frogsort1P_init_left(x      , aux, nl, 0     , 0         );
  PFrogsort2P_Frogsort1P_init_right(x + nl, x + (n - nr - br), nr, nr - 1      , nr + br - 1);
  PFrogsort2P_Frogsort1P_sort_left(aux, nl, 0);
  PFrogsort2P_Frogsort1P_sort_right(x + (n - nr - br), nr, nr + br - 1);
  FrogmergeP_asc_right(x     , aux, aux+nl-1 , x+n-nr , x+n-1);
  // Better tuning: see below
  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + naux / (double) n;
  return;
}

void PFrogsort2P_Frogsort1P_exsitu(ValueT *x, IndexT n, PerfT *p)
{
  // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
  if (n<3){
    Insertionsort_l2r(x, 0, n-1);
    p->secs = getNewSecs();
    p->size = 1;
    return;
  }

  // standard solution for fair comparison
  IndexT b = n / 2;
  IndexT naux = n + b;
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  PFrogsort2P_Frogsort1P_init_left(x, aux, n, 0, 0);
  PFrogsort2P_Frogsort1P_sort_left(aux, n, 0);
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];

  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}



static void Frogsort2P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double p
);
static void Frogsort2P_init_left(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT l
  , IndexT L
  , double p
){
  IndexT nr = floor(p*n);
  // Rprintf("Frogsort2P_init_left n=%d l=%d L=%d p=%f nr=%d\n", n, l, L, p, nr); R_FlushConsole();
  if (
#if INSERTIONSORT_LIMIT > 0
      n <= INSERTIONSORT_LIMIT || nr <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nr < 1
#endif
  ){
    x += l;
    y += L;
    for (nr=0; nr<n; nr++)
      y[nr] = x[nr];
    // Insertionsort_l2r(y, 0, n - 1);
  }else{
    Frogsort2P_init_left (x, y, n - nr, l    , L       , p);
    Frogsort2P_init_right(x, y, nr    , l+n-1, L+n+nr-1, p);
  }
}
static void Frogsort2P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double p
){
  IndexT nl = floor(p*n);
  // Rprintf("Frogsort2P_init_right n=%d r=%d R=%d p=%f nl=%d\n", n, r, R, p, nl); R_FlushConsole();
  if (
#if INSERTIONSORT_LIMIT > 0
      n <= INSERTIONSORT_LIMIT || nl <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nl < 1
#endif
  ){
    x += r - n + 1;
    y += R - n + 1;
    for (nl=0; nl<n; nl++)
      y[nl] = x[nl];
    // Insertionsort_l2r(y, 0, n - 1);
  }else{
    Frogsort2P_init_left (x, y, nl    , r-n+1, R-n-nl+1, p);
    Frogsort2P_init_right(x, y, n - nl, r    , R       , p);
  }
}

static void Frogsort2P_sort_right(
    ValueT *x
  , IndexT n
  , IndexT R
  , double p
);
static void Frogsort2P_sort_left(
    ValueT *x
  , IndexT n
  , IndexT L
  , double p
){
  IndexT nr = floor(p*n);
  // Rprintf("Frogsort2P_sort_left n=%d L=%d p=%f nr=%d\n", n, L, p, nr); R_FlushConsole();
  if (
#if INSERTIONSORT_LIMIT > 0
      n <= INSERTIONSORT_LIMIT || nr <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nr < 1
#endif
  ){
    Insertionsort_l2r(x, L, L + n - 1);
  }else{
    Frogsort2P_sort_left (x, n - nr, L, p);
    Frogsort2P_sort_right(x, nr, L+n+nr-1, p);
    FrogmergeP_asc_left(x+L+n-1, x+L, x+(L+(n-nr)-1), x+(L+n), x+(L+n+nr-1));
  }
}
static void Frogsort2P_sort_right(
    ValueT *x
  , IndexT n
  , IndexT R
  , double p
){
  IndexT nl = floor(p*n);
  // Rprintf("Frogsort2P_sort_right n=%d R=%d p=%f nl=%d\n", n, R, p, nl); R_FlushConsole();
  if (
#if INSERTIONSORT_LIMIT > 0
      n <= INSERTIONSORT_LIMIT || nl <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nl < 1
#endif
  ){
    Insertionsort_l2r(x, R - n + 1, R);
  }else{
    Frogsort2P_sort_left (x, nl, R-n-nl+1, p);
    Frogsort2P_sort_right(x, n - nl, R, p);
    FrogmergeP_asc_right(x+R-n+1, x+(R-n-nl+1), x+(R-n), x+(R-(n-nl)+1), x+R);
  }
}



typedef struct PFrogsort2P_init_left_t{
  ValueT *x;
  ValueT *y;
  IndexT n;
  IndexT l;
  IndexT L;
  double p;
  double threads;
} PFrogsort2P_init_left_t;
typedef struct PFrogsort2P_init_right_t{
  ValueT *x;
  ValueT *y;
  IndexT n;
  IndexT r;
  IndexT R;
  double p;
  double threads;
} PFrogsort2P_init_right_t;

typedef struct PFrogsort2P_sort_left_t{
  ValueT *x;
  IndexT n;
  IndexT L;
  double p;
  double threads;
} PFrogsort2P_sort_left_t;
typedef struct PFrogsort2P_sort_right_t{
  ValueT *x;
  IndexT n;
  IndexT R;
  double p;
  double threads;
} PFrogsort2P_sort_right_t;


static void * PFrogsort2P_init_right(void *arg);
static void * PFrogsort2P_init_left(void *arg)
{
  PFrogsort2P_init_left_t *parg = (PFrogsort2P_init_left_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort2P_init_left(parg->x, parg->y, parg->n, parg->l, parg->L, parg->p);
  }else{
    pthread_t thread0, thread1;
    PFrogsort2P_init_left_t L;
    PFrogsort2P_init_right_t R;

    IndexT nr = floor(parg->p * parg->n);

    //PFrogsort2P_init_left (parg->x, parg->y, parg->n - nr, parg->l    , parg->L       , parg->p);
    L.x = parg->x;
    L.y = parg->y;
    L.n = parg->n - nr;
    L.l = parg->l;
    L.L = parg->L;
    L.p = parg->p;
    L.threads = parg->threads * (1 - parg->p);

    //PFrogsort2P_init_right(parg->x, parg->y, nr    , parg->l+parg->n-1, parg->L+parg->n+nr-1, parg->p);
    R.x = parg->x;
    R.y = parg->y;
    R.n = nr;
    R.r = parg->l + parg->n - 1;
    R.R = parg->L + parg->n + nr - 1;
    R.p = parg->p;
    R.threads = parg->threads * parg->p;

    // PFrogsort2P_init_left(&L);
    // PFrogsort2P_init_right(&R);

    pthread_create(&thread0, NULL, PFrogsort2P_init_left, &L);
    pthread_create(&thread1, NULL, PFrogsort2P_init_right, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}
static void * PFrogsort2P_init_right(void *arg)
{
  PFrogsort2P_init_right_t *parg = (PFrogsort2P_init_right_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort2P_init_right(parg->x, parg->y, parg->n, parg->r, parg->R, parg->p);
  }else{
      pthread_t thread0, thread1;
      PFrogsort2P_init_left_t L;
      PFrogsort2P_init_right_t R;

      IndexT nl = floor(parg->p * parg->n);

      //PFrogsort2P_init_left (parg->x, parg->y, nl    , parg->r-parg->n+1, parg->R-parg->n-nl+1, parg->p);
      L.x = parg->x;
      L.y = parg->y;
      L.n = nl;
      L.l = parg->r - parg->n + 1;
      L.L = parg->R - parg->n - nl + 1;
      L.p = parg->p;
      L.threads = parg->threads * parg->p;

      //PFrogsort2P_init_right(parg->x, parg->y, parg->n - nl, parg->r    , parg->R       , parg->p);
      R.x = parg->x;
      R.y = parg->y;
      R.n = parg->n - nl;
      R.r = parg->r;
      R.R = parg->R;
      R.p = parg->p;
      R.threads = parg->threads * (1 - parg->p);

      // PFrogsort2P_init_left(&L);
      // PFrogsort2P_init_right(&R);

      pthread_create(&thread0, NULL, PFrogsort2P_init_left, &L);
      pthread_create(&thread1, NULL, PFrogsort2P_init_right, &R);
      pthread_join(thread0, NULL);
      pthread_join(thread1, NULL);
  }
  return NULL;
}

static void * PFrogsort2P_sort_right(void *arg);
static void * PFrogsort2P_sort_left(void *arg)
{
  PFrogsort2P_sort_left_t *parg = (PFrogsort2P_sort_left_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort2P_sort_left(parg->x, parg->n, parg->L, parg->p);
  }else{
    pthread_t thread0, thread1;
    PFrogsort2P_sort_left_t L;
    PFrogsort2P_sort_right_t R;

    IndexT nr = floor(parg->p * parg->n);

    //PFrogsort2P_sort_left (parg->x, parg->n - nr, parg->L       , parg->p);
    L.x = parg->x;
    L.n = parg->n - nr;
    L.L = parg->L;
    L.p = parg->p;
    L.threads = parg->threads * (1 - parg->p);

    //PFrogsort2P_sort_right(parg->x, nr    , parg->L+parg->n+nr-1, parg->p);
    R.x = parg->x;
    R.n = nr;
    R.R = parg->L + parg->n + nr - 1;
    R.p = parg->p;
    R.threads = parg->threads * parg->p;

    // PFrogsort2P_sort_left(&L);
    // PFrogsort2P_sort_right(&R);

    pthread_create(&thread0, NULL, PFrogsort2P_sort_left, &L);
    pthread_create(&thread1, NULL, PFrogsort2P_sort_right, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);

    SFrogmergeP_asc_left(
      parg->x+parg->L+parg->n-1
    , parg->x+parg->L
    , parg->x+(parg->L+(parg->n-nr)-1)
    , parg->x+(parg->L+parg->n)
    , parg->x+(parg->L+parg->n+nr-1)
    , parg->threads
    );
  }
  return NULL;
}
static void * PFrogsort2P_sort_right(void *arg)
{
  PFrogsort2P_sort_right_t *parg = (PFrogsort2P_sort_right_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    Frogsort2P_sort_right(parg->x, parg->n, parg->R, parg->p);
  }else{
    pthread_t thread0, thread1;
    PFrogsort2P_sort_left_t L;
    PFrogsort2P_sort_right_t R;

    IndexT nl = floor(parg->p * parg->n);

    //PFrogsort2P_sort_left (parg->x, nl    , parg->R-parg->n-nl+1, parg->p);
    L.x = parg->x;
    L.n = nl;
    L.L = parg->R - parg->n - nl + 1;
    L.p = parg->p;
    L.threads = parg->threads * parg->p;

    //PFrogsort2P_sort_right(parg->x, parg->n - nl, parg->R       , parg->p);
    R.x = parg->x;
    R.n = parg->n - nl;
    R.R = parg->R;
    R.p = parg->p;
    R.threads = parg->threads * (1 - parg->p);

    // PFrogsort2P_sort_left(&L);
    // PFrogsort2P_sort_right(&R);

    pthread_create(&thread0, NULL, PFrogsort2P_sort_left, &L);
    pthread_create(&thread1, NULL, PFrogsort2P_sort_right, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);

    SFrogmergeP_asc_right(
      parg->x+parg->R-parg->n+1
    , parg->x+(parg->R-parg->n-nl+1)
    , parg->x+(parg->R-parg->n)
    , parg->x+(parg->R-(parg->n-nl)+1)
    , parg->x+parg->R
    , parg->threads
    );
  }
  return NULL;
}


void PFrogsort2P_insitu(ValueT *x
                         , IndexT n
                         , double f
                         , double t
                         , PerfT *p
)
{
  IndexT br = ceil(n * f / (1 + f));
  IndexT nr = n - br;
  IndexT nl = br; // = n - nr
  IndexT bl = floor(nl*f);
  IndexT b = bl+br;
  //Rprintf("f=%f fi=%f n=%d b=%d nl=%d nr=%d bl=%d br=%d\n", f, fi, n, b, nl, nr, bl, br);
  if (bl==0 || br==0){
    PFrogsort2P_Frogsort1P_insitu(x, n, p);
    return;
  }
  ValueT *aux = (ValueT *) MALLOC(b, ValueT);
  ValueT *ori;
  ori = x + (n - nr - br);

  //Frogsort2P_init_left(x      , aux, nl, 0     , 0         , f);
  PFrogsort2P_init_left_t Linit;
  Linit.x = x;
  Linit.y = aux;
  Linit.n = nl;
  Linit.l = 0;
  Linit.L = 0;
  Linit.p = f;
  Linit.threads = t * f;
  PFrogsort2P_init_left(&Linit);

  // this is insitu hence parallelizing does not speed-up things (by contrast) hence for now serially
  Frogsort2P_init_right(x + nl, ori, nr, nr - 1, nr + br - 1, f);
  // xx TODO or segmented parallel for the bigger part

  //PFrogsort2P_sort_left(aux, nl, 0, f);
  PFrogsort2P_sort_left_t Lsort;
  Lsort.x = aux;
  Lsort.n = nl;
  Lsort.L = 0;
  Lsort.p = f;
  Lsort.threads = t * f;

  //PFrogsort2P_sort_right(ori, nr, nr + br - 1, f);
  PFrogsort2P_sort_right_t Rsort;
  Rsort.x = ori;
  Rsort.n = nr;
  Rsort.R = nr + br - 1;
  Rsort.p = f;
  Rsort.threads = t * (1-f);

  // running Lsort and Rsort in parallel is slower due to page faults
  // PFrogsort2P_sort_right(&Rsort);
  // PFrogsort2P_sort_left(&Lsort);
  pthread_t thread0, thread1;
  pthread_create(&thread1, NULL, PFrogsort2P_sort_right, &Rsort);
  pthread_create(&thread0, NULL, PFrogsort2P_sort_left, &Lsort);
  pthread_join(thread1, NULL);
  pthread_join(thread0, NULL);

  //FrogmergeP_asc_right(x     , aux, aux+nl-1 , ori+br , x+n-1);
  FrogmergeP_asc_t arg;
  arg.x = x;
  arg.ll = aux;
  arg.lr = aux+nl-1;
  arg.rl = ori+br;
  arg.rr = x+n-1;
  PFrogmergeP_asc_right(&arg);

  FREE(aux);

  p->secs = getNewSecs();
  p->size = 1 + f;
  return;
}

void PFrogsort2P_exsitu(ValueT *x
                        , IndexT n
                        , double f
                        , double t
                        , PerfT *p
)
{
  ValueT *aux;
  IndexT b = floor(f*n);
  if (b==0){
    PFrogsort2P_Frogsort1P_exsitu(x, n, p);
    return;
  }
  IndexT naux = n + b;
  aux = (ValueT *) MALLOC(naux, ValueT);

  //PFrogsort2P_init_left(x, aux, n, 0, 0, f);
  PFrogsort2P_init_left_t Linit;
  Linit.x = x;
  Linit.y = aux;
  Linit.n = n;
  Linit.l = 0;
  Linit.L = 0;
  Linit.p = f;
  Linit.threads = t;
  PFrogsort2P_init_left(&Linit);


  //PFrogsort2P_sort_left(aux, n, 0, f);
  PFrogsort2P_sort_left_t Lsort;
  Lsort.x = aux;
  Lsort.n = n;
  Lsort.L = 0;
  Lsort.p = f;
  Lsort.threads = t;
  PFrogsort2P_sort_left(&Lsort);

  Scopy(x, aux, n, t);

  FREE(aux);

  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}

