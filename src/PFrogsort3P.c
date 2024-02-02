/*
# greeNsort parallel Frogsort3 (pointer implementation) 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"

#include "PFrogmergeP.h"
#include "Insertionsort_l2r.h"

static IndexT min(IndexT a, IndexT b){
	return b < a ? b : a;
}



static void PFrogsort3P_Frogsort1P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
);
static void PFrogsort3P_Frogsort1P_init_left(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT l
  , IndexT L
){
  IndexT nr;
  //Rprintf("PFrogsort3P_Frogsort1P_init_left l=%d r=%d L=%d R=%d\n", l, l + n - 1, L, L + n - 1); R_FlushConsole();
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
    PFrogsort3P_Frogsort1P_init_left (x, y, n - nr, l    , L       );
    PFrogsort3P_Frogsort1P_init_right(x, y, nr    , l+n-1, L+n+nr-1);
  }
}
static void PFrogsort3P_Frogsort1P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
){
  IndexT nl;
  //Rprintf("PFrogsort3P_Frogsort1P_init_right l=%d r=%d L=%d R=%d\n", r - n + 1, r, R - n + 1, R); R_FlushConsole();
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
    PFrogsort3P_Frogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    PFrogsort3P_Frogsort1P_init_right(x, y, n - nl, r    , R       );
  }
}




static void PFrogsort3P_Frogsort1P_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
);
static void PFrogsort3P_Frogsort1P_sort_left(
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
    PFrogsort3P_Frogsort1P_sort_left (y, n - nr, L       );
    PFrogsort3P_Frogsort1P_sort_right(y, nr    , L+n+nr-1);
    FrogmergeP_asc_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1));
  }
}
static void PFrogsort3P_Frogsort1P_sort_right(
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
    PFrogsort3P_Frogsort1P_sort_left (y, nl    , R-n-nl+1);
    PFrogsort3P_Frogsort1P_sort_right(y, n - nl, R       );
    FrogmergeP_asc_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R);
  }
}



typedef struct PFrogsort3P_PFrogsort1P_init_left_t{
  ValueT *x;
  ValueT *y;
  IndexT n;
  IndexT l;
  IndexT L;
  double threads;
} PFrogsort3P_PFrogsort1P_init_left_t;
typedef struct PFrogsort3P_PFrogsort1P_init_right_t{
  ValueT *x;
  ValueT *y;
  IndexT n;
  IndexT r;
  IndexT R;
  double threads;
} PFrogsort3P_PFrogsort1P_init_right_t;

typedef struct PFrogsort3P_PFrogsort1P_sort_left_t{
  ValueT *y;
  IndexT n;
  IndexT L;
  double threads;
} PFrogsort3P_PFrogsort1P_sort_left_t;
typedef struct PFrogsort3P_PFrogsort1P_sort_right_t{
  ValueT *y;
  IndexT n;
  IndexT R;
  double threads;
} PFrogsort3P_PFrogsort1P_sort_right_t;





static void * PFrogsort3P_PFrogsort1P_init_right(void *arg);
static void * PFrogsort3P_PFrogsort1P_init_left(void *arg){
  PFrogsort3P_PFrogsort1P_init_left_t *parg = (PFrogsort3P_PFrogsort1P_init_left_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    PFrogsort3P_Frogsort1P_init_left(parg->x, parg->y, parg->n, parg->l, parg->L);
  }else{
    pthread_t thread0, thread1;
    PFrogsort3P_PFrogsort1P_init_left_t  L;
    PFrogsort3P_PFrogsort1P_init_right_t R;

    IndexT nr = parg->n  / 2;

    //PFrogsort3P_Frogsort1P_init_left (x, y, n - nr, l    , L       );
    L.x = parg->x;
    L.y = parg->y;
    L.n = parg->n - nr;
    L.l = parg->l;
    L.L = parg->L;
    L.threads = parg->threads / 2.0;
    //PFrogsort3P_Frogsort1P_init_right(x, y, nr    , l+n-1, L+n+nr-1);
    R.x = parg->x;
    R.y = parg->y;
    R.n = nr;
    R.r = parg->l + parg->n - 1;
    R.R = parg->L + parg->n + nr - 1;
    R.threads = L.threads;

    // PFrogsort3P_PFrogsort1P_init_left(&L);
    // PFrogsort3P_PFrogsort1P_init_right(&R);

    pthread_create(&thread0, NULL, PFrogsort3P_PFrogsort1P_init_left, &L);
    pthread_create(&thread1, NULL, PFrogsort3P_PFrogsort1P_init_right, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}
static void * PFrogsort3P_PFrogsort1P_init_right(void *arg){
  PFrogsort3P_PFrogsort1P_init_right_t *parg = (PFrogsort3P_PFrogsort1P_init_right_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    PFrogsort3P_Frogsort1P_init_right(parg->x, parg->y, parg->n, parg->r, parg->R);
  }else{
    pthread_t thread0, thread1;
    PFrogsort3P_PFrogsort1P_init_left_t L;
    PFrogsort3P_PFrogsort1P_init_right_t R;

    IndexT nl = parg->n  / 2;

    //PFrogsort3P_Frogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    L.x = parg->x;
    L.y = parg->y;
    L.n = nl;
    L.l = parg->r - parg->n + 1;
    L.L = parg->R - parg->n - nl + 1;
    L.threads = parg->threads / 2.0;
    //PFrogsort3P_Frogsort1P_init_right(x, y, n - nl, r    , R       );
    R.x = parg->x;
    R.y = parg->y;
    R.n = parg->n - nl;
    R.r = parg->r;
    R.R = parg->R;
    R.threads = L.threads;

    // PFrogsort3P_PFrogsort1P_init_left(&L);
    // PFrogsort3P_PFrogsort1P_init_right(&R);

    pthread_create(&thread0, NULL, PFrogsort3P_PFrogsort1P_init_left, &L);
    pthread_create(&thread1, NULL, PFrogsort3P_PFrogsort1P_init_right, &R);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}





static void * PFrogsort3P_PFrogsort1P_sort_right(void *arg);
static void * PFrogsort3P_PFrogsort1P_sort_left(void *arg){
  PFrogsort3P_PFrogsort1P_sort_left_t *parg = (PFrogsort3P_PFrogsort1P_sort_left_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    PFrogsort3P_Frogsort1P_sort_left(parg->y, parg->n, parg->L);
  }else{
    PFrogsort3P_PFrogsort1P_sort_left_t  L;
    PFrogsort3P_PFrogsort1P_sort_right_t R;

    IndexT nr = parg->n  / 2;

    //PFrogsort3P_Frogsort1P_sort_left (y, n - nr, L       );
    L.y = parg->y;
    L.n = parg->n - nr;
    L.L = parg->L;
    L.threads = parg->threads / 2.0;
    //PFrogsort3P_Frogsort1P_sort_right(y, nr    , L+n+nr-1);
    R.y = parg->y;
    R.n = nr;
    R.R = parg->L + parg->n + nr - 1;
    R.threads = L.threads;

    // PFrogsort3P_PFrogsort1P_sort_left(&L);
    // PFrogsort3P_PFrogsort1P_sort_right(&R);

    pthread_t thread0, thread1;
    pthread_create(&thread0, NULL, PFrogsort3P_PFrogsort1P_sort_left, &L);
    pthread_create(&thread1, NULL, PFrogsort3P_PFrogsort1P_sort_right, &R);
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

static void * PFrogsort3P_PFrogsort1P_sort_right(void *arg){
  PFrogsort3P_PFrogsort1P_sort_right_t *parg = (PFrogsort3P_PFrogsort1P_sort_right_t*)arg;
  if (parg->threads <= 1 || (parg->n <= THREAD_LIMIT_SORT)){
    PFrogsort3P_Frogsort1P_sort_right(parg->y, parg->n, parg->R);
  }else{
    PFrogsort3P_PFrogsort1P_sort_left_t L;
    PFrogsort3P_PFrogsort1P_sort_right_t R;

    IndexT nl = parg->n  / 2;

    //PFrogsort3P_Frogsort1P_sort_left (y, nl    , R-n-nl+1);
    L.y = parg->y;
    L.n = nl;
    L.L = parg->R - parg->n - nl + 1;
    L.threads = parg->threads / 2.0;
    //PFrogsort3P_Frogsort1P_sort_right(y, n - nl, R       );
    R.y = parg->y;
    R.n = parg->n - nl;
    R.R = parg->R;
    R.threads = L.threads;

    // PFrogsort3P_PFrogsort1P_sort_left(&L);
    // PFrogsort3P_PFrogsort1P_sort_right(&R);

    pthread_t thread0, thread1;
    pthread_create(&thread0, NULL, PFrogsort3P_PFrogsort1P_sort_left, &L);
    pthread_create(&thread1, NULL, PFrogsort3P_PFrogsort1P_sort_right, &R);
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


static void PFrogsort3P_SFrogsort1P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double threads
);

static void PFrogsort3P_DFrogsort1P_init_left(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT l
  , IndexT L
  , double threads
){
  //Rprintf("PFrogsort3P_DFrogsort1P_init_left l=%d r=%d L=%d R=%d\n", l, l + n - 1, L, L + n - 1); R_FlushConsole();
  if (threads <= 1 || (n <= THREAD_LIMIT_SORT)){
    PFrogsort3P_Frogsort1P_init_left(x, y, n, l, L);
  }else{
    IndexT nr = n  / 2;

    //PFrogsort3P_Frogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    PFrogsort3P_PFrogsort1P_init_left_t arg;
    arg.x = x;
    arg.y = y;
    arg.n = n - nr;
    arg.l = l;
    arg.L = L;
    arg.threads = threads;
    PFrogsort3P_PFrogsort1P_init_left(&arg);

    PFrogsort3P_SFrogsort1P_init_right(x, y, nr    , l+n-1, L+n+nr-1, threads);
  }
}

static void PFrogsort3P_DFrogsort1P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double threads
){
  //Rprintf("PFrogsort3P_DFrogsort1P_init_right l=%d r=%d L=%d R=%d\n", r - n + 1, r, R - n + 1, R); R_FlushConsole();
  if (threads <= 1 || (n <= THREAD_LIMIT_SORT)){
    PFrogsort3P_Frogsort1P_init_right(x, y, n, r, R);
  }else{
    IndexT nl = n  / 2;

    //PFrogsort3P_Frogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    PFrogsort3P_PFrogsort1P_init_left_t arg;
    arg.x = x;
    arg.y = y;
    arg.n = nl;
    arg.l = r-n+1;
    arg.L = R-n-nl+1;
    arg.threads = threads;
    PFrogsort3P_PFrogsort1P_init_left(&arg);

    //PFrogsort3P_Frogsort1P_init_right(x, y, n - nl, r    , R);
    PFrogsort3P_SFrogsort1P_init_right(x, y, n - nl, r    , R       , threads);
  }
}





static void PFrogsort3P_SFrogsort1P_init_left(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT l
  , IndexT L
  , double threads
){
  //Rprintf("PFrogsort3P_SFrogsort1P_init_left l=%d r=%d L=%d R=%d\n", l, l + n - 1, L, L + n - 1); R_FlushConsole();
  if (threads <= 1 || (n <= THREAD_LIMIT_SORT)){
    PFrogsort3P_Frogsort1P_init_left(x, y, n, l, L);
  }else{
    IndexT nr = n  / 2;
    PFrogsort3P_DFrogsort1P_init_left (x, y, n - nr, l    , L       , threads);
    PFrogsort3P_SFrogsort1P_init_right(x, y, nr    , l+n-1, L+n+nr-1, threads);
  }
}
static void PFrogsort3P_SFrogsort1P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double threads
){
  //Rprintf("PFrogsort3P_SFrogsort1P_init_right l=%d r=%d L=%d R=%d\n", r - n + 1, r, R - n + 1, R); R_FlushConsole();
  if (threads <= 1 || (n <= THREAD_LIMIT_SORT)){
    PFrogsort3P_Frogsort1P_init_right(x, y, n, r, R);
  }else{
    IndexT nl = n  / 2;
    PFrogsort3P_DFrogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1, threads);
    PFrogsort3P_SFrogsort1P_init_right(x, y, n - nl, r    , R       , threads);
  }
}



void PFrogsort3P_PFrogsort1P_insitu(ValueT *x, IndexT n, double t, PerfT *p)
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

  // PFrogsort3P_Frogsort1P_init_left(x      , aux, nl, 0     , 0         );
  PFrogsort3P_PFrogsort1P_init_left_t Linit;
  Linit.x = x;
  Linit.y = aux;
  Linit.n = nl;
  Linit.l = 0;
  Linit.L = 0;
  Linit.threads = t;
  PFrogsort3P_PFrogsort1P_init_left(&Linit);

  // must not be full parallel, either serial or xx TODO segmented parallel
  PFrogsort3P_Frogsort1P_init_right(x + nl, x + (n - nr - br), nr, nr - 1      , nr + br - 1);
  //PFrogsort3P_SFrogsort1P_init_right(x + nl, x + (n - nr - br), nr, nr - 1      , nr + br - 1, t);

  //PFrogsort3P_Frogsort1P_sort_left(aux, nl, 0);
  PFrogsort3P_PFrogsort1P_sort_left_t Lsort;
  Lsort.y = aux;
  Lsort.n = nl;
  Lsort.L = 0;
  Lsort.threads = t; //t/3.0;

  //PFrogsort3P_Frogsort1P_sort_right(x + (n - nr - br), nr, nr + br - 1);
  PFrogsort3P_PFrogsort1P_sort_right_t Rsort;
  Rsort.y = x + (n - nr - br);
  Rsort.n = nr;
  Rsort.R = nr + br - 1;
  Rsort.threads = t; //t*2.0/3.0;

  // running Lsort and Rsort in parallel is slower due to page faults
  PFrogsort3P_PFrogsort1P_sort_right(&Rsort);
  PFrogsort3P_PFrogsort1P_sort_left(&Lsort);

  // pthread_create(&thread1, NULL, PFrogsort3P_PFrogsort1P_sort_right, &Rsort);
  // pthread_create(&thread0, NULL, PFrogsort3P_PFrogsort1P_sort_left, &Lsort);
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

  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + naux / (double) n;
  return;
}


void PFrogsort3P_PFrogsort1P_exsitu(ValueT *x, IndexT n, double t, PerfT *p)
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

  //Frogsort1P_init_left(x, aux, n, 0, 0);
  PFrogsort3P_PFrogsort1P_init_left_t Linit;
  Linit.x = x;
  Linit.y = aux;
  Linit.n = n;
  Linit.l = 0;
  Linit.L = 0;
  Linit.threads = t;
  PFrogsort3P_PFrogsort1P_init_left(&Linit);

  //PFrogsort3P_Frogsort1P_sort_left(aux, n, 0);
  PFrogsort3P_PFrogsort1P_sort_left_t Lsort;
  Lsort.y = aux;
  Lsort.n = n;
  Lsort.L = 0;
  Lsort.threads = t;
  PFrogsort3P_PFrogsort1P_sort_left(&Lsort);

  Scopy(x, aux, n, t);

  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}





static void PFrogsort3P_initsort_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT b
, IndexT r
, IndexT R
, double t
);
static void PFrogsort3P_initsort_left(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT b
, IndexT l
, IndexT L
, double t
){
  IndexT nr,nl,br,bl;
  IndexT r,R;
  //Rprintf("PFrogsort3P_sort_left n=%d b=%d l=%d L=%d\n", n, b, l, L); R_FlushConsole();
  if (n <= b+b+1){
  	// fork
  	PFrogsort3P_Frogsort1P_init_left(x, y, n, l, L);
    //PFrogsort3P_Frogsort1P_sort_left(y, n, L);
    PFrogsort3P_PFrogsort1P_sort_left_t arg;
    arg.y = y;
    arg.n = n;
    arg.L = L;
    arg.threads = t;
    PFrogsort3P_PFrogsort1P_sort_left(&arg);
  }else{
    // cut
		nr = b; // == min(b, n / 2) ensured by recursion
		nl = n - nr;
		br = min(b, nr / 2);
		bl = min(b, nl / 2);
		r = l + n - 1;
		R = L + n + b - 1;
		PFrogsort3P_initsort_left (x, y, nl, bl, l, L, t);
		PFrogsort3P_initsort_right(x, y, nr, br, r, R, t);
		SFrogmergeP_asc_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1), t);
	}
}
static void PFrogsort3P_initsort_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT b
, IndexT r
, IndexT R
, double t
){
  IndexT nr,nl,br,bl;
  IndexT l,L;
  //Rprintf("PFrogsort3P_sort_right n=%d b=%d r=%d R=%d\n", n, b, r, R); R_FlushConsole();
  if (n <= b+b+1){
	// fork
	  PFrogsort3P_Frogsort1P_init_right(x, y, n, r, R);
    //PFrogsort3P_Frogsort1P_sort_right(y, n, R);
    PFrogsort3P_PFrogsort1P_sort_right_t arg;
    arg.y = y;
    arg.n = n;
    arg.R = R;
    arg.threads = t;
    PFrogsort3P_PFrogsort1P_sort_right(&arg);
  }else{
	  // cut
		nl = b; // == min(b, n / 2) ensured by recursion
		nr = n - nl;
		bl = min(b, nl / 2);
		br = min(b, nr / 2);
		l = r - n + 1;
		L = R - n - b + 1;
		PFrogsort3P_initsort_left (x, y, nl, bl, l, L, t);
		PFrogsort3P_initsort_right(x, y, nr, br, r, R, t);
		SFrogmergeP_asc_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R, t);
  }
}


void PFrogsort3P_insitu(ValueT *x
, IndexT n
, double f
, double t
, PerfT *p
)
{
  IndexT b = ceil(f*n);
  f = ((double)b) / n;
  double fi = ((double) b) / (n+b); // f inner
  IndexT nl = floor(fi * n);  // inner
  IndexT nr = n - nl;         // outer
  IndexT br = nl;             // outer
  IndexT bl = b-nl;           // inner
  //Rprintf("f=%f fi=%f n=%d b=%d nl=%d nr=%d bl=%d br=%d\n", f, fi, n, b, nl, nr, bl, br);
#if INSERTIONSORT_LIMIT > 0
  if (bl==0 || br==0 || nl <= bl+bl+1 || nr <= br+br+1 || bl+bl+1 < INSERTIONSORT_LIMIT)
#else
    if (bl==0 || br==0 || nl <= bl+bl+1 || nr <= br+br+1 || bl+bl+1 < 3)
#endif
  {
    PFrogsort3P_PFrogsort1P_insitu(x, n, t, p);
    return;
  }
  ValueT *aux = (ValueT *) MALLOC(b, ValueT);
  //Rprintf("f=%g nl=%d nr=%d bl=%d br=%d\n", f, nl, nr, bl, br);

  PFrogsort3P_initsort_left (x     , aux              , nl, bl, 0     , 0          , t);
  PFrogsort3P_initsort_right(x + nl, x + (n - nr - br), nr, br, nr - 1, nr + br - 1, t);
  //FrogmergeP_asc_right(x, aux, aux+nl-1 , x+n-nr      , x+n-1);
  SFrogmergeP_asc_right(x, aux, aux+nl-1 , x+n-nr      , x+n-1, t);

  FREE(aux);

  p->secs = getNewSecs();
  p->size = 1 + b / (double) n;
  return;
}

void PFrogsort3P_exsitu(ValueT *x
, IndexT n
, double f
, double t
, PerfT *p
)
{
  IndexT b =  floor(f*n);  // we switch this buffer between two equally sized halves
#if INSERTIONSORT_LIMIT > 0
  if (b==0 || n <= b+b+1 || b+b+1 < INSERTIONSORT_LIMIT)
#else
  if (b==0 || n <= b+b+1 || b+b+1 < 3)
#endif
  {
    PFrogsort3P_PFrogsort1P_exsitu(x, n, t, p);
    return;
  }
  IndexT naux = n + b;
  //Rprintf("n=%d nl=%d nr=%d f=%f b=%d naux=%d\n", n, nl, nr, f, b, naux);
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);

  PFrogsort3P_initsort_left (x     , aux                   , n, b, 0     , 0          , t);
  //copy(x, aux, n);
  Scopy(x, aux, n, t);

  FREE(aux);

  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}
