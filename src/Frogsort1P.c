/*
# greeNsort Frogsort1 (pointered implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "FrogmergeP.h"
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
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += l;
    y += L;
    for (nr=0; nr<n; nr++)
      y[nr] = x[nr];
    Insertionsort_l2r(y, 0, n - 1);
  }else
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
  }else
#endif
  {
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
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += r - n + 1;
    y += R - n + 1;
    for (nl=n-1; nl >= 0; nl--)  // to avoid inplace overwrite
      y[nl] = x[nl];
    Insertionsort_l2r(y, 0, n - 1);
  }else
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
  }else
#endif
  {
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
  if (
#if INSERTIONSORT_LIMIT > 0
  n > INSERTIONSORT_LIMIT
#else
  n > 2
#endif
  ){
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
  if (
#if INSERTIONSORT_LIMIT > 0
  n > INSERTIONSORT_LIMIT
#else
  n > 2
#endif
  ){
    IndexT nl = n  / 2;
    Frogsort1P_sort_left (y, nl    , R-n-nl+1);
    Frogsort1P_sort_right(y, n - nl, R       );
    FrogmergeP_asc_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R);
  }
}


void Frogsort1P_insitu(ValueT *x, IndexT n)
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
  Frogsort1P_init_left(x      , aux, nl, 0     , 0         );
  Frogsort1P_init_right(x + nl, x + (n - nr - br), nr, nr - 1      , nr + br - 1);
  Frogsort1P_sort_left(aux, nl, 0);
  Frogsort1P_sort_right(        x + (n - nr - br), nr              , nr + br - 1);
  FrogmergeP_asc_right(x     , aux, aux+nl-1 , x+n-nr , x+n-1);
  // Better tuning: see below
  FREE(aux);
  return;
}

void Frogsort1P_exsitu(ValueT *x, IndexT n)
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
  Frogsort1P_init_left(x, aux, n, 0, 0);
  Frogsort1P_sort_left(aux, n, 0);
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];

  // Mild tuning: let the lst merge copy back
  // IndexT nl = n / 2;
  // IndexT nr = n - nl;
  // IndexT bl = nl / 2;
  // IndexT br = nr / 2;
  // IndexT naux = n + bl + br;
  // ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  // Frogsort3P_Frogsort1P_init_left (x     , aux                   , nl, 0     , 0          );
  // Frogsort3P_Frogsort1P_init_right(x + nl, aux + (naux - nr - br), nr, nr - 1, nr + br - 1);
  // Frogsort3P_Frogsort1P_sort_left (aux                   , nl, 0          );
  // Frogsort3P_Frogsort1P_sort_right(aux + (naux - nr - br), nr, nr + br - 1);
  // FrogmergeP_asc_right    (x, aux, aux+nl-1 , aux + (naux - nr), aux + naux - 1);

  FREE(aux);
  return;
}
