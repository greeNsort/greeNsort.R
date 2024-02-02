/*
# greeNsort Frogsort2 moving Matrix-Columns by first row keys (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "MFrogmergeP.h"
#include "MInsertionsort_l2r.h"

static void MFrogsort2P_MFrogsort1P_init_right(
    IntValueT *x
  , IntValueT *y
  , IndexT n
  , IndexT m
  , IndexT r
  , IndexT R
);
static void MFrogsort2P_MFrogsort1P_init_left(
    IntValueT *x
  , IntValueT *y
  , IndexT n
  , IndexT m
  , IndexT l
  , IndexT L
){
  IndexT nr;
  // Rprintf("MFrogsort2P_MFrogsort1P_init_left n=%d m=%d l=%d L=%d\n", n, m, l, L);
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += l*m;
    y += L*m;
    for (nr=0; nr<n*m; nr+=m)
      MMOVE(y+nr, x+nr, m);
    MInsertionsort_l2r(y, 0, n - 1, m);
  }
#else
  IntValueT (t)[m];  // xx C99, optional in C11 Variable Length Arrays(VLA)
  if (n <= 2){
    if (n == 2){
      if (LT(x[(l+1)*m], x[l*m])){
        MMOVE(t, x+(l+1)*m, m);  // to avoid inplace overwrite
        MMOVE(y+(L+1)*m, x+l*m, m);
        MMOVE(y+L*m, t, m);      // to avoid inplace overwrite
      }else{
        MMOVE(y+(L+1)*m, x+(l+1)*m, m);
        MMOVE(y+L*m, x+l*m, m);
      }
    }else{
      MMOVE(y+L*m, x+l*m, m);
    }
  }
#endif
  else{
    nr = n  / 2;
    MFrogsort2P_MFrogsort1P_init_left (x, y, n - nr, m, l    , L       );
    MFrogsort2P_MFrogsort1P_init_right(x, y, nr    , m, l+n-1, L+n+nr-1);
  }
}
static void MFrogsort2P_MFrogsort1P_init_right(
    IntValueT *x
  , IntValueT *y
  , IndexT n
  , IndexT m
  , IndexT r
  , IndexT R
){
  IndexT nl;
  //Rprintf("MFrogsort2P_MFrogsort1P_init_right n=%d m=%d r=%d R=%d\n", n, m, r, R);
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += (r - n + 1)*m;
    y += (R - n + 1)*m;
    for (nl=(n-1)*m; nl >= 0; nl-=m)  // to avoid inplace overwrite
      MMOVE(y+nl, x+nl, m);
    MInsertionsort_l2r(y, 0, n - 1, m);
  }
#else
  IntValueT (t)[m];  // xx C99, optional in C11 Variable Length Arrays(VLA)
  if (n <= 2){
    if (n == 2){
      if (LT(x[r*m], x[(r-1)*m])){
        MMOVE(t,x+(r-1)*m, m);  // to avoid inplace overwrite
        MMOVE(y+(R-1)*m, x+r*m, m);
        MMOVE(y+R*m, t, m);  // to avoid inplace overwrite
      }else{
        MMOVE(y+(R-1)*m, x+(r-1)*m, m);
        MMOVE(y+R*m, x+r*m, m);
      }
    }else{
      MMOVE(y+R*m, x+r*m, m);
    }
  }
#endif
  else{
    nl = n  / 2;
    MFrogsort2P_MFrogsort1P_init_left (x, y, nl    , m, r-n+1, R-n-nl+1);
    MFrogsort2P_MFrogsort1P_init_right(x, y, n - nl, m, r    , R       );
  }
}


static void MFrogsort2P_MFrogsort1P_sort_right(
    IntValueT *y
  , IndexT n
  , IndexT m
  , IndexT R
);
static void MFrogsort2P_MFrogsort1P_sort_left(
    IntValueT *y
  , IndexT n
  , IndexT m
  , IndexT L
){
  IndexT nr;
  //Rprintf("MFrogsort2P_MFrogsort1P_sort_left n=%d m=%d L=%d\n", n, m, L);
#if INSERTIONSORT_LIMIT > 0
  if (n > INSERTIONSORT_LIMIT)
#else
    if (n > 2)
#endif
{
  nr = n  / 2;
  MFrogsort2P_MFrogsort1P_sort_left (y, n - nr, m, L       );
  MFrogsort2P_MFrogsort1P_sort_right(y, nr    , m, L+n+nr-1);
  MFrogmergeP_asc_left(y+(L+n-1)*m, y+L*m, y+(L+(n-nr)-1)*m, y+(L+n)*m, y+(L+n+nr-1)*m, m);
}
}
static void MFrogsort2P_MFrogsort1P_sort_right(
    IntValueT *y
  , IndexT n
  , IndexT m
  , IndexT R
){
  //Rprintf("MFrogsort2P_MFrogsort1P_sort_right n=%d m=%d R=%d\n", n, m, R);
  IndexT nl;
#if INSERTIONSORT_LIMIT > 0
  if (n > INSERTIONSORT_LIMIT)
#else
    if (n > 2)
#endif
{
  nl = n  / 2;
  MFrogsort2P_MFrogsort1P_sort_left (y, nl    , m, R-n-nl+1);
  MFrogsort2P_MFrogsort1P_sort_right(y, n - nl, m, R       );
  MFrogmergeP_asc_right(y+(R-n+1)*m, y+(R-n-nl+1)*m, y+(R-n)*m, y+(R-(n-nl)+1)*m, y+R*m, m);
}
}


void MFrogsort2P_MFrogsort1P_insitu(IntValueT *x, IndexT n, IndexT m, PerfT *p)
{
  // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
  if (n<3){
    MInsertionsort_l2r(x, 0, n-1, m);
    p->secs = getNewSecs();
    p->size = 1;
    return;
  }
  IndexT nr = (n / 3) * 2;
  IndexT nl = n - nr;
  IndexT bl = nl / 2;
  IndexT br = nr / 2;
  IndexT naux = nl + bl;
  IntValueT *aux = (IntValueT *) MALLOC(naux*m, IntValueT);
  MFrogsort2P_MFrogsort1P_init_left (x       , aux                , nl, m, 0      , 0          );
  MFrogsort2P_MFrogsort1P_init_right(x + nl*m, x + (n - nr - br)*m, nr, m, nr - 1 , nr + br - 1);
  MFrogsort2P_MFrogsort1P_sort_left(           aux                , nl, m         , 0          );
  MFrogsort2P_MFrogsort1P_sort_right(          x + (n - nr - br)*m, nr, m         , nr + br - 1);
  MFrogmergeP_asc_right(x     , aux, aux+(nl-1)*m, x+(n-nr)*m, x+(n-1)*m, m);
  // Better tuning: see below
  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + naux / (double) n;
  return;
}

void MFrogsort2P_MFrogsort1P_exsitu(IntValueT *x, IndexT n, IndexT m, PerfT *p)
{
  // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
  if (n<3){
    MInsertionsort_l2r(x, 0, n-1, m);
    p->secs = getNewSecs();
    p->size = 1;
    return;
  }

  // standard solution for fair comparison
  IndexT b = n / 2;
  IndexT naux = n + b;
  IntValueT *aux = (IntValueT *) MALLOC(naux*m, IntValueT);
  MFrogsort2P_MFrogsort1P_init_left(x, aux, n, m, 0, 0);
  MFrogsort2P_MFrogsort1P_sort_left(aux, n, m, 0);
  for (IndexT i=0; i<n*m; i+=m)
    MMOVE(x+i, aux+i, m);

  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}



static void MFrogsort2P_init_right(
    IntValueT *x
  , IntValueT *y
  , IndexT n
  , IndexT m
  , IndexT r
  , IndexT R
  , double p
);
static void MFrogsort2P_init_left(
    IntValueT *x
  , IntValueT *y
  , IndexT n
  , IndexT m
  , IndexT l
  , IndexT L
  , double p
){
  IndexT nr = floor(p*n);
  //Rprintf("MFrogsort2P_init_left n=%d l=%d L=%d p=%f nr=%d\n", n, l, L, p, nr); R_FlushConsole();
  if (
// #if INSERTIONSORT_LIMIT > 0
//       nr <= p*INSERTIONSORT_LIMIT
// #else
//     nr < 1
// #endif
#if INSERTIONSORT_LIMIT > 0
  n <= INSERTIONSORT_LIMIT || nr <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nr < 1
#endif
  ){
    x += l*m;
    y += L*m;
    for (nr=0; nr<n*m; nr+=m)
      MMOVE(y+nr, x+nr, m);
    // Insertionsort_l2r(y, 0, n - 1);
  }else{
    MFrogsort2P_init_left (x, y, n - nr, m, l    , L       , p);
    MFrogsort2P_init_right(x, y, nr    , m, l+n-1, L+n+nr-1, p);
  }
}
static void MFrogsort2P_init_right(
    IntValueT *x
  , IntValueT *y
  , IndexT n
  , IndexT m
  , IndexT r
  , IndexT R
  , double p
){
  IndexT nl = floor(p*n);
  //Rprintf("MFrogsort2P_init_right n=%d r=%d R=%d p=%f nl=%d\n", n, r, R, p, nl); R_FlushConsole();
  if (
// #if INSERTIONSORT_LIMIT > 0
//       nl <= p*INSERTIONSORT_LIMIT
// #else
//     nl < 1
// #endif
#if INSERTIONSORT_LIMIT > 0
  n <= INSERTIONSORT_LIMIT || nl <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nl < 1
#endif
  ){
    x += (r - n + 1)*m;
    y += (R - n + 1)*m;
    for (nl=0; nl<n*m; nl+=m)
      MMOVE(y+nl, x+nl, m);
    // Insertionsort_l2r(y, 0, n - 1);
  }else{
    MFrogsort2P_init_left (x, y, nl    , m, r-n+1, R-n-nl+1, p);
    MFrogsort2P_init_right(x, y, n - nl, m, r    , R       , p);
  }
}

static void MFrogsort2P_sort_right(
    IntValueT *x
  , IndexT n
  , IndexT m
  , IndexT R
  , double p
);
static void MFrogsort2P_sort_left(
    IntValueT *x
  , IndexT n
  , IndexT m
  , IndexT L
  , double p
){
  IndexT nr = floor(p*n);
  //Rprintf("MFrogsort2P_sort_left n=%d L=%d p=%f nr=%d\n", n, L, p, nr); R_FlushConsole();
  if (
// #if INSERTIONSORT_LIMIT > 0
//       nr <= p*INSERTIONSORT_LIMIT
// #else
//   nr < 1
// #endif
#if INSERTIONSORT_LIMIT > 0
  n <= INSERTIONSORT_LIMIT || nr <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nr < 1
#endif
  ){
    MInsertionsort_l2r(x, L, L + n - 1, m);
  }else{
    MFrogsort2P_sort_left (x, n - nr, m, L, p);
    MFrogsort2P_sort_right(x, nr, m, L+n+nr-1, p);
    MFrogmergeP_asc_left(x+(L+n-1)*m, x+L*m, x+(L+(n-nr)-1)*m, x+(L+n)*m, x+(L+n+nr-1)*m, m);
  }
}
static void MFrogsort2P_sort_right(
    IntValueT *x
, IndexT n
, IndexT m
, IndexT R
  , double p
){
  IndexT nl = floor(p*n);
  //Rprintf("MFrogsort2P_sort_right n=%d R=%d p=%f nl=%d\n", n, R, p, nl); R_FlushConsole();
  if (
// #if INSERTIONSORT_LIMIT > 0
//       nl <= p*INSERTIONSORT_LIMIT
// #else
//     nl < 1
// #endif
#if INSERTIONSORT_LIMIT > 0
  n <= INSERTIONSORT_LIMIT || nl <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nl < 1
#endif
  ){
    MInsertionsort_l2r(x, R - n + 1, R, m);
  }else{
    MFrogsort2P_sort_left (x, nl, m, R-n-nl+1, p);
    MFrogsort2P_sort_right(x, n - nl, m, R, p);
    MFrogmergeP_asc_right(x+(R-n+1)*m, x+(R-n-nl+1)*m, x+(R-n)*m, x+(R-(n-nl)+1)*m, x+R*m, m);
  }
}


void MFrogsort2P_insitu(IntValueT *x
                         , IndexT n
                         , IndexT m
                         , double f
                         , PerfT *p
)
{
  IndexT br = ceil(n * f / (1 + f));
  IndexT nr = n - br;
  IndexT nl = br; // = n - nr
  IndexT bl = floor(nl*f);
  IndexT b = bl+br;
  //Rprintf("f=%f n=%d b=%d nl=%d nr=%d bl=%d br=%d\n", f, n, b, nl, nr, bl, br);
  if (bl==0 || br==0){
    MFrogsort2P_MFrogsort1P_insitu(x, n, m, p);
    return;
  }
  IntValueT *aux = (IntValueT *) MALLOC(b*m, IntValueT);
  IntValueT *ori;
  ori = x + (n - nr - br)*m;
  MFrogsort2P_init_left (x       , aux, nl, m, 0     , 0          , f);
  MFrogsort2P_init_right(x + nl*m, ori, nr, m, nr - 1, nr + br - 1, f);
  MFrogsort2P_sort_right(          ori, nr, m        , nr + br - 1, f);
  MFrogsort2P_sort_left(           aux, nl, m        , 0          , f);
  MFrogmergeP_asc_right(x, aux, aux+(nl-1)*m, ori+br*m, x+(n-1)*m, m);
  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + f;
  return;
}

void MFrogsort2P_exsitu(IntValueT *x
                        , IndexT n
                        , IndexT m
                        , double f
                        , PerfT *p
)
{
  IndexT nm=n*m;
  IntValueT *aux;
  IndexT b = floor(f*n);
  if (b==0){
    MFrogsort2P_MFrogsort1P_exsitu(x, n, m, p);
    return;
  }
  IndexT naux = n + b;
  aux = (IntValueT *) MALLOC(naux*m, IntValueT);
  MFrogsort2P_init_left(x, aux, n, m, 0, 0, f);
  MFrogsort2P_sort_left(   aux, n, m   , 0, f);
  for (IndexT i=0; i<nm; i+=m)
    MMOVE(x+i, aux+i, m);
  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}

