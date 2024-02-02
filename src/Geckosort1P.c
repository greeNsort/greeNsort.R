/*
# greeNsort Geckosort1 (pointered implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "GeckomergeP.h"
#include "Insertionsort_l2r.h"
#if INSERTIONSORT_LIMIT > 0
#include "Insertionsort_r2l.h"
#endif

static void Geckosort1P_init_right_stable(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT r
, IndexT R
);
static void Geckosort1P_init_left_reverse(
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
      Insertionsort_r2l(y, 0, n - 1);
      // y -= L;
      // for (nr=L; nr<L+n; nr++)
      //   Rprintf("\nGeckosort1P_init_left_reverse y[%d]=%f", nr, y[nr]);
    }
#else
  if (n <= 2){
    if (n == 2){
      ValueT t;
      if (LE(x[l], x[l+1])){
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
    // for (nr=L; nr<L+n; nr++)
    //   Rprintf("\nGeckosort1P_init_left_reverse y[%d]=%f", nr, y[nr]);
  }
#endif
  else{
    nr = n  / 2;
    Geckosort1P_init_left_reverse (x, y, n - nr, l    , L       );
    Geckosort1P_init_right_stable(x, y, nr    , l+n-1, L+n+nr-1);
  }
}
static void Geckosort1P_init_right_stable(
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
//       y -= R - n + 1;
//       for (nl=R - n + 1;nl<=R;nl++)
// 	      Rprintf("\nGeckosort1P_init_right_stable y[%d]=%f", nl, y[nl]);
    }
#else
  ValueT t;
  if (n <= 2){
    if (n == 2){
      if (GT(x[r-1], x[r])){
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
    // for (nl=R - n + 1;nl<=R;nl++)
    //   Rprintf("\nGeckosort1P_init_right_stable y[%d]=%f", nl, y[nl]);
  }
#endif
  else{
    nl = n  / 2;
    Geckosort1P_init_left_reverse (x, y, nl    , r-n+1, R-n-nl+1);
    Geckosort1P_init_right_stable(x, y, n - nl, r    , R       );
  }
}


static void Geckosort1P_sort_right_stable(
    ValueT *y
  , IndexT n
  , IndexT R
);
static void Geckosort1P_sort_left_reverse(
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
    Geckosort1P_sort_left_reverse (y, n - nr, L       );
    Geckosort1P_sort_right_stable(y, nr    , L+n+nr-1);
    GeckomergeP_asc_left_reverse(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1));
  }
}
static void Geckosort1P_sort_right_stable(
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
    Geckosort1P_sort_left_reverse (y, nl    , R-n-nl+1);
    Geckosort1P_sort_right_stable(y, n - nl, R       );
    GeckomergeP_asc_right_stable(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R);
  }
}

void Geckosort1P_insitu(ValueT *x, IndexT n)
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
  ValueT *ori;
  ori = x + (n - nr - br);
  Geckosort1P_init_left_reverse(x      , aux, nl, 0     , 0         );
  Geckosort1P_init_right_stable(x + nl, ori, nr, nr - 1, nr + br - 1);
  Geckosort1P_sort_left_reverse(aux, nl, 0);
  Geckosort1P_sort_right_stable(ori, nr, nr + br - 1);
  GeckomergeP_asc_right_stable(x, aux, aux+nl-1, x+n-nr, x+n-1);
  FREE(aux);
  return;
}


void Geckosort1P_exsitu(ValueT *x, IndexT n)
{
  // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
  if (n<3){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }

  IndexT b = n / 2;
  IndexT naux = n + b;
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  Geckosort1P_init_right_stable(x, aux, n, n-1, n+b-1);
  Geckosort1P_sort_right_stable(aux, n, n+b-1);
  aux += b;
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];
  aux -= b;
  FREE(aux);
  return;
}

