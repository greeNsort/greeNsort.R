/*
# greeNsort Squidsort2 (pointered implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "SquidmergeP.h"
#include "Insertionsort_l2r.h"
#include "Insertionsort_r2l.h"
#include <stdbool.h>


static void Squidsort2P_Squidsort1P_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
);
static void Squidsort2P_Squidsort1P_init_left(
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
  }
#else
  if (n <= 2){
    y[L] = x[l];
    if (n == 2){
      y[L+1] = x[l+1];
    }
  }
#endif
  else{
    nr = n  / 2;
    Squidsort2P_Squidsort1P_init_left (x, y, n - nr, l    , L       );
    Squidsort2P_Squidsort1P_init_right(x, y, nr    , l+n-1, L+n+nr-1);
  }
}
static void Squidsort2P_Squidsort1P_init_right(
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
  }
#else
  if (n <= 2){
    y[R] = x[r];
    if (n == 2){
      y[R-1] = x[r-1];
    }
  }
#endif
  else{
    nl = n  / 2;
    Squidsort2P_Squidsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    Squidsort2P_Squidsort1P_init_right(x, y, n - nl, r    , R       );
  }
}


static bool Squidsort2P_Squidsort1P_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
);
static bool Squidsort2P_Squidsort1P_sort_left(
    ValueT *y
  , IndexT n
  , IndexT L
){
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    y += L;
    if ((LT(y[n-1], y[0]))){
      Insertionsort_r2l(y, 0, n - 1); return TRUE;
    }else{
      Insertionsort_l2r(y, 0, n - 1); return FALSE;
    }
  }
#else
  if (n <= 2){
    if (n == 2){
      if (LT(y[L+1], y[L])){
        return TRUE;
      }else{
        return FALSE;
      }
    }
    return FALSE;
  }
#endif
  else{
    bool lrev, rrev;
    IndexT nr = n  / 2;
    lrev = Squidsort2P_Squidsort1P_sort_left (y, n - nr, L       );
    rrev = Squidsort2P_Squidsort1P_sort_right(y, nr    , L+n+nr-1);
    if (lrev){
      if (rrev){
        SquidmergeP_des_des_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1)); return TRUE;
      }else{
        SquidmergeP_des_asc_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1)); return TRUE;
      }
    }else{
      if (rrev){
        SquidmergeP_asc_des_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1)); return FALSE;
      }else{
        SquidmergeP_asc_asc_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1)); return FALSE;
      }
    }
  }
}
static bool Squidsort2P_Squidsort1P_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
){
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    y += R - n + 1;
    if ((LT(y[n-1], y[0]))){
      Insertionsort_r2l(y, 0, n - 1); return TRUE;
    }else{
      Insertionsort_l2r(y, 0, n - 1); return FALSE;
    }
  }
#else
  if (n <= 2){
    if (n == 2){
      if (LT(y[R], y[R-1])){
        return TRUE;
      }else{
        return FALSE;
      }
    }
    return FALSE;
  }
#endif
  else{
    bool lrev, rrev;
    IndexT nl = n  / 2;
    lrev = Squidsort2P_Squidsort1P_sort_left (y, nl    , R-n-nl+1);
    rrev = Squidsort2P_Squidsort1P_sort_right(y, n - nl, R       );
    if (lrev){
      if (rrev){
        SquidmergeP_des_des_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R); return TRUE;
      }else{
        SquidmergeP_des_asc_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R); return FALSE;
      }
    }else{
      if (rrev){
        SquidmergeP_asc_des_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R); return TRUE;
      }else{
        SquidmergeP_asc_asc_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R); return FALSE;
      }
    }
  }
}


void Squidsort2P_Squidsort1P_insitu(ValueT *x, IndexT n, PerfT *p)
{
  // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
  if (n<3){
    Insertionsort_l2r(x, 0, n-1);
    p->secs = getNewSecs();
    p->size = 1;
    return;
  }
  bool lrev, rrev;
  IndexT nr = (n / 3) * 2;
  IndexT nl = n - nr;
  IndexT bl = nl / 2;
  IndexT br = nr / 2;
  IndexT naux = nl + bl;
  ValueT t;
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  Squidsort2P_Squidsort1P_init_left(x      , aux, nl, 0     , 0         );
  Squidsort2P_Squidsort1P_init_right(x + nl, x, nr, nr-1, n - 1);
  lrev = Squidsort2P_Squidsort1P_sort_left(aux, nl, 0);
  rrev = Squidsort2P_Squidsort1P_sort_right(x, nr, n - 1);
  if (lrev){
    if (rrev){
      //  -- \\ -->
      SquidmergeP_des_des_right(x, aux, aux+nl-1, x+n-nr, x+n-1);
      for (bl=0,br=n-1;bl<br;bl++,br--){
        t = x[bl]; x[bl] = x[br]; x[br] = t;
      }
    }else{
      //  -- \/ -->
      SquidmergeP_des_asc_right(x, aux, aux+nl-1, x+n-nr, x+n-1);
    }
  }else{
    if (rrev){
      //  -- /\ -->
      SquidmergeP_asc_des_right(x, aux, aux+nl-1, x+n-nr, x+n-1);
      for (bl=0,br=n-1;bl<br;bl++,br--){
        t = x[bl]; x[bl] = x[br]; x[br] = t;
      }
    }else{
      //  -- // -->
      SquidmergeP_asc_asc_right(x, aux, aux+nl-1, x+n-nr, x+n-1);
    }
  }
  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + naux / (double) n;
  return;
}

void Squidsort2P_Squidsort1P_exsitu(ValueT *x, IndexT n, PerfT *p)
{
  // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
  if (n<3){
    Insertionsort_l2r(x, 0, n-1);
    p->secs = getNewSecs();
    p->size = 1;
    return;
  }
  bool rev;
  IndexT b = n / 2;
  IndexT naux = n + b;
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  Squidsort2P_Squidsort1P_init_right(x, aux, n, n-1, n+b-1);
  rev = Squidsort2P_Squidsort1P_sort_right(aux, n, n+b-1);
  aux += b;
  if (rev){
    aux += n - 1;
    for (IndexT i=0; i<n; i++)
      x[i] = aux[-i];
    aux -= n - 1;
  }else{
    for (IndexT i=0; i<n; i++)
      x[i] = aux[i];
  }
  aux -= b;
  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}




static void Squidsort2P_init_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT r
, IndexT R
, double p
);
static void Squidsort2P_init_left(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT l
, IndexT L
, double p
){
  IndexT nr = floor(p*n);
  if (
#if INSERTIONSORT_LIMIT > 0
    nr <= p*INSERTIONSORT_LIMIT
#else
    nr < 1
#endif
  ){
    x += l;
    y += L;
    for (nr=0; nr<n; nr++)
      y[nr] = x[nr];
  }else{
    Squidsort2P_init_left (x, y, n - nr, l    , L       , p);
    Squidsort2P_init_right(x, y, nr    , l+n-1, L+n+nr-1, p);
  }
}
static void Squidsort2P_init_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT r
, IndexT R
, double p
){
  IndexT nl = floor(p*n);
  if (
#if INSERTIONSORT_LIMIT > 0
    nl <= p*INSERTIONSORT_LIMIT
#else
    nl < 1
#endif
  ){
    x += r - n + 1;
    y += R - n + 1;
    for (nl=0; nl<n; nl++)
      y[nl] = x[nl];
  }else{
    Squidsort2P_init_left (x, y, nl    , r-n+1, R-n-nl+1, p);
    Squidsort2P_init_right(x, y, n - nl, r    , R       , p);
  }
}


static bool Squidsort2P_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
  , double p
);
static bool Squidsort2P_sort_left(
    ValueT *y
  , IndexT n
  , IndexT L
  , double p
){
  IndexT nr = floor(p*n);
  if (
#if INSERTIONSORT_LIMIT > 0
      nr <= p*INSERTIONSORT_LIMIT
#else
    nr < 1
#endif
  )
  {
    y += L;
    if ((LT(y[n-1], y[0]))){
      Insertionsort_r2l(y, 0, n - 1); return TRUE;
    }else{
      Insertionsort_l2r(y, 0, n - 1); return FALSE;
    }
  }else{
    bool lrev, rrev;
    lrev = Squidsort2P_sort_left (y, n - nr, L       , p);
    rrev = Squidsort2P_sort_right(y, nr    , L+n+nr-1, p);
    if (lrev){
      if (rrev){
        SquidmergeP_des_des_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1)); return TRUE;
      }else{
        SquidmergeP_des_asc_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1)); return TRUE;
      }
    }else{
      if (rrev){
        SquidmergeP_asc_des_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1)); return FALSE;
      }else{
        SquidmergeP_asc_asc_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1)); return FALSE;
      }
    }
  }
}
static bool Squidsort2P_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
  , double p
){
  IndexT nl = floor(p*n);
  if (
#if INSERTIONSORT_LIMIT > 0
      nl <= p*INSERTIONSORT_LIMIT
#else
    nl < 1
#endif
  )
  {
    y += R - n + 1;
    if ((LT(y[n-1], y[0]))){
      Insertionsort_r2l(y, 0, n - 1); return TRUE;
    }else{
      Insertionsort_l2r(y, 0, n - 1); return FALSE;
    }
  }else{
    bool lrev, rrev;
    lrev = Squidsort2P_sort_left (y, nl    , R-n-nl+1, p);
    rrev = Squidsort2P_sort_right(y, n - nl, R       , p);
    if (lrev){
      if (rrev){
        SquidmergeP_des_des_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R); return TRUE;
      }else{
        SquidmergeP_des_asc_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R); return FALSE;
      }
    }else{
      if (rrev){
        SquidmergeP_asc_des_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R); return TRUE;
      }else{
        SquidmergeP_asc_asc_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R); return FALSE;
      }
    }
  }
}


void Squidsort2P_insitu(ValueT *x, IndexT n, double f, PerfT *p)
{
  IndexT br = ceil(n * f / (1 + f));
  IndexT nr = n - br;
  IndexT nl = br; // = n - nr
  IndexT bl = floor(nl*f);
  IndexT b = bl+br;
  //Rprintf("f=%f fi=%f n=%d b=%d nl=%d nr=%d bl=%d br=%d\n", f, fi, n, b, nl, nr, bl, br);
  if (bl==0 || br==0){
    Squidsort2P_Squidsort1P_insitu(x, n, p);
    return;
  }
  bool lrev, rrev;
  ValueT t;
  ValueT *aux = (ValueT *) MALLOC(b, ValueT);
  Squidsort2P_init_left(x      , aux, nl, 0     , 0         , f);
  Squidsort2P_init_right(x + nl, x, nr, nr-1, n - 1, f);
  lrev = Squidsort2P_sort_left(aux, nl, 0, f);
  rrev = Squidsort2P_sort_right(x, nr, n - 1, f);
  if (lrev){
    if (rrev){
      //  -- \\ -->
      SquidmergeP_des_des_right(x, aux, aux+nl-1, x+n-nr, x+n-1);
      for (bl=0,br=n-1;bl<br;bl++,br--){
        t = x[bl]; x[bl] = x[br]; x[br] = t;
      }
    }else{
      //  -- \/ -->
      SquidmergeP_des_asc_right(x, aux, aux+nl-1, x+n-nr, x+n-1);
    }
  }else{
    if (rrev){
      //  -- /\ -->
      SquidmergeP_asc_des_right(x, aux, aux+nl-1, x+n-nr, x+n-1);
      for (bl=0,br=n-1;bl<br;bl++,br--){
        t = x[bl]; x[bl] = x[br]; x[br] = t;
      }
    }else{
      //  -- // -->
      SquidmergeP_asc_asc_right(x, aux, aux+nl-1, x+n-nr, x+n-1);
    }
  }
  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + f;
  return;
}

void Squidsort2P_exsitu(ValueT *x, IndexT n, double f, PerfT *p)
{
  IndexT b = floor(f*n);
  if (b==0){
    Squidsort2P_Squidsort1P_exsitu(x, n, p);
    return;
  }
  bool rev;
  IndexT naux = n + b;
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  Squidsort2P_init_right(x, aux, n, n-1, n+b-1, f);
  rev = Squidsort2P_sort_right(aux, n, n+b-1, f);
  aux += b;
  if (rev){
    aux += n - 1;
    for (IndexT i=0; i<n; i++)
      x[i] = aux[-i];
    aux -= n - 1;
  }else{
    for (IndexT i=0; i<n; i++)
      x[i] = aux[i];
  }
  aux -= b;
  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}

