/*
# greeNsort Frogsort2 B-tuned (not really)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "FrogmergeB.h"
#include "Insertionsort_l2r.h"

static void Frogsort2B_Frogsort1_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
);
static void Frogsort2B_Frogsort1_init_left(
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
    Frogsort2B_Frogsort1_init_left (x, y, n - nr, l    , L       );
    Frogsort2B_Frogsort1_init_right(x, y, nr    , l+n-1, L+n+nr-1);
  }
}
static void Frogsort2B_Frogsort1_init_right(
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
    Frogsort2B_Frogsort1_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    Frogsort2B_Frogsort1_init_right(x, y, n - nl, r    , R       );
  }
}


static void Frogsort2B_Frogsort1_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
);
static void Frogsort2B_Frogsort1_sort_left(
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
  Frogsort2B_Frogsort1_sort_left (y, n - nr, L       );
  Frogsort2B_Frogsort1_sort_right(y, nr    , L+n+nr-1);
  FrogmergeB_asc_left_stable(y, n - nr, nr, L);
}
}
static void Frogsort2B_Frogsort1_sort_right(
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
  Frogsort2B_Frogsort1_sort_left (y, nl    , R-n-nl+1);
  Frogsort2B_Frogsort1_sort_right(y, n - nl, R       );
  FrogmergeB_asc_right_stable(y, nl, n - nl, R);
}
}


void Frogsort2B_Frogsort1_insitu(ValueT *x, IndexT n, PerfT *p)
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
  Frogsort2B_Frogsort1_init_left(x      , aux, nl, 0     , 0         );
  Frogsort2B_Frogsort1_init_right(x + nl, x + (n - nr - br), nr, nr - 1      , nr + br - 1);
  Frogsort2B_Frogsort1_sort_left(aux, nl, 0     );
  Frogsort2B_Frogsort1_sort_right(x + (n - nr - br), nr, nr + br - 1);
  FrogmergeB_asc_right_final (x     , aux              , nl, x + (n - nr), nr         );
  // Better tuning: see below
  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + naux / (double) n;
  return;
}


void Frogsort2B_Frogsort1_exsitu(ValueT *x, IndexT n, PerfT *p)
{
  // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
  if (n<3){
    Insertionsort_l2r(x, 0, n-1);
    p->secs = getNewSecs();
    p->size = 1;
    return;
  }
  IndexT nl = n / 2;
  IndexT nr = n - nl;
  IndexT bl = nl / 2;
  IndexT br = nr / 2;
  IndexT naux = n + bl + br;
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);

  // standard solution for fair comparison
  Frogsort2B_Frogsort1_init_left(x, aux, n, 0, 0);
  Frogsort2B_Frogsort1_sort_left(aux, n, 0);
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];

  // Mild tuning: let the lst merge copy back
  // Frogsort2B_Frogsort1_init_left (x     , aux                   , nl, 0     , 0          );
  // Frogsort2B_Frogsort1_init_right(x + nl, aux + (naux - nr - br), nr, nr - 1, nr + br - 1);
  // Frogsort2B_Frogsort1_sort_left (aux                   , nl, 0          );
  // Frogsort2B_Frogsort1_sort_right(aux + (naux - nr - br), nr, nr + br - 1);
  // FrogmergeB_asc_right_full(x, aux, nl, aux + (naux - nr), nr);

  // Better tuning:
  // size aux = 1/3
  // sort 1/6 in aux, sort another 1/6 in x and partial-inplace-merge to aux (now aux is completely filled)
  // sort rest 2/3 in x using 1/3 free  buffer
  // merge 2/3 with 2/6
  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}



static void Frogsort2B_init_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT r
, IndexT R
, double p
);
static void Frogsort2B_init_left(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT l
, IndexT L
, double p
){
  IndexT nr = floor(p*n);
  // Rprintf("Frogsort2B_init_left n=%d l=%d L=%d p=%f nr=%d\n", n, l, L, p, nr); R_FlushConsole();
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
    Frogsort2B_init_left (x, y, n - nr, l    , L       , p);
    Frogsort2B_init_right(x, y, nr    , l+n-1, L+n+nr-1, p);
  }
}
static void Frogsort2B_init_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT r
, IndexT R
, double p
){
  IndexT nl = floor(p*n); // outer
  // Rprintf("Frogsort2B_init_right n=%d r=%d R=%d p=%f nl=%d\n", n, r, R, p, nl); R_FlushConsole();
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
    Frogsort2B_init_left (x, y, nl    , r-n+1, R-n-nl+1, p);
    Frogsort2B_init_right(x, y, n - nl, r    , R       , p);
  }
}

static void Frogsort2B_sort_right(
  ValueT *x
, IndexT n
, IndexT R
, double p
);
static void Frogsort2B_sort_left(
  ValueT *x
, IndexT n
, IndexT L
, double p
){
  IndexT nr = floor(p*n); // inner
  // Rprintf("Frogsort2B_sort_left n=%d L=%d p=%f nr=%d\n", n, L, p, nr); R_FlushConsole();
  if (
#if INSERTIONSORT_LIMIT > 0
      n <= INSERTIONSORT_LIMIT || nr <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nr < 1
#endif
  ){
    Insertionsort_l2r(x, L, L + n - 1);
  }else{
    Frogsort2B_sort_left (x, n - nr, L, p);
    Frogsort2B_sort_right(x, nr, L+n+nr-1, p);
    FrogmergeB_asc_left_stable(x, n - nr, nr, L);
  }
}
static void Frogsort2B_sort_right(
  ValueT *x
, IndexT n
, IndexT R
, double p
){
  IndexT nl = floor(p*n); // inner
  // Rprintf("Frogsort2B_sort_right n=%d R=%d p=%f nl=%d\n", n, R, p, nl); R_FlushConsole();
  if (
#if INSERTIONSORT_LIMIT > 0
      n <= INSERTIONSORT_LIMIT || nl <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nl < 1
#endif
  ){
    Insertionsort_l2r(x, R - n + 1, R);
  }else{
    Frogsort2B_sort_left (x, nl, R-n-nl+1, p);
    Frogsort2B_sort_right(x, n - nl, R, p);
    FrogmergeB_asc_right_stable(x, nl, n - nl, R);
  }
}


void Frogsort2B_insitu(ValueT *x
, IndexT n
, double f
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
    Frogsort2B_Frogsort1_insitu(x, n, p);
    return;
  }
  ValueT *aux = (ValueT *) MALLOC(b, ValueT);
  ValueT *ori;
  ori = x + (n - nr - br);
  Frogsort2B_init_left(x      , aux, nl, 0     , 0         , f);
  Frogsort2B_init_right(x + nl, ori, nr, nr - 1, nr + br - 1, f);
  Frogsort2B_sort_right(ori, nr, nr + br - 1, f);
  Frogsort2B_sort_left(aux, nl, 0, f);
  FrogmergeB_asc_right_final(x, aux, nl, ori+br, nr);
  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + f;
  return;
}

void Frogsort2B_exsitu(ValueT *x
, IndexT n
, double f
, PerfT *p
)
{
  ValueT *aux;
  IndexT b = floor(f*n);
  if (b==0){
    Frogsort2B_Frogsort1_exsitu(x, n, p);
    return;
  }
  IndexT naux = n + b;
  aux = (ValueT *) MALLOC(naux, ValueT);
  Frogsort2B_init_left(x, aux, n, 0, 0, f);
  Frogsort2B_sort_left(aux, n, 0, f);
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}

