/*
# greeNsort Frogsort6
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "Frogmerge.h"
#include "Insertionsort_l2r.h"

static IndexT min(IndexT a, IndexT b){
	return b < a ? b : a;
}


static void Frogsort6_Frogsort2_Frogsort1_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
);
static void Frogsort6_Frogsort2_Frogsort1_init_left(
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
    Frogsort6_Frogsort2_Frogsort1_init_left (x, y, n - nr, l    , L       );
    Frogsort6_Frogsort2_Frogsort1_init_right(x, y, nr    , l+n-1, L+n+nr-1);
  }
}
static void Frogsort6_Frogsort2_Frogsort1_init_right(
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
    Frogsort6_Frogsort2_Frogsort1_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    Frogsort6_Frogsort2_Frogsort1_init_right(x, y, n - nl, r    , R       );
  }
}


static void Frogsort6_Frogsort2_Frogsort1_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
);
static void Frogsort6_Frogsort2_Frogsort1_sort_left(
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
  Frogsort6_Frogsort2_Frogsort1_sort_left (y, n - nr, L       );
  Frogsort6_Frogsort2_Frogsort1_sort_right(y, nr    , L+n+nr-1);
  Frogmerge_asc_left_stable(y, n - nr, nr, L);
}
}
static void Frogsort6_Frogsort2_Frogsort1_sort_right(
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
  Frogsort6_Frogsort2_Frogsort1_sort_left (y, nl    , R-n-nl+1);
  Frogsort6_Frogsort2_Frogsort1_sort_right(y, n - nl, R       );
  Frogmerge_asc_right_stable(y, nl, n - nl, R);
}
}


void Frogsort6_Frogsort2_Frogsort1_insitu(ValueT *x, IndexT n, PerfT *p)
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
  Frogsort6_Frogsort2_Frogsort1_init_left(x      , aux, nl, 0     , 0         );
  Frogsort6_Frogsort2_Frogsort1_init_right(x + nl, x + (n - nr - br), nr, nr - 1      , nr + br - 1);
  Frogsort6_Frogsort2_Frogsort1_sort_left(aux, nl, 0     );
  Frogsort6_Frogsort2_Frogsort1_sort_right(x + (n - nr - br), nr, nr + br - 1);
  Frogmerge_asc_right_final (x     , aux              , nl, x + (n - nr), nr         );
  // Better tuning: see below
  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + naux / (double) n;
  return;
}


void Frogsort6_Frogsort2_Frogsort1_exsitu(ValueT *x, IndexT n, PerfT *p)
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
  Frogsort6_Frogsort2_Frogsort1_init_left(x, aux, n, 0, 0);
  Frogsort6_Frogsort2_Frogsort1_sort_left(aux, n, 0);
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];

  // Mild tuning: let the lst merge copy back
  // Frogsort6_Frogsort2_Frogsort1_init_left (x     , aux                   , nl, 0     , 0          );
  // Frogsort6_Frogsort2_Frogsort1_init_right(x + nl, aux + (naux - nr - br), nr, nr - 1, nr + br - 1);
  // Frogsort6_Frogsort2_Frogsort1_sort_left (aux                   , nl, 0          );
  // Frogsort6_Frogsort2_Frogsort1_sort_right(aux + (naux - nr - br), nr, nr + br - 1);
  // Frogmerge_asc_right_full(x, aux, nl, aux + (naux - nr), nr);

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



static void Frogsort6_Frogsort2_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double p
);
static void Frogsort6_Frogsort2_init_left(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT l
  , IndexT L
  , double p
){
  IndexT nr = floor(p*n);
  // Rprintf("Frogsort6_Frogsort2_init_left n=%d l=%d L=%d p=%f nr=%d\n", n, l, L, p, nr); R_FlushConsole();
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
    Frogsort6_Frogsort2_init_left (x, y, n - nr, l    , L       , p);
    Frogsort6_Frogsort2_init_right(x, y, nr    , l+n-1, L+n+nr-1, p);
  }
}
static void Frogsort6_Frogsort2_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double p
){
  IndexT nl = floor(p*n);
  // Rprintf("Frogsort6_Frogsort2_init_right n=%d r=%d R=%d p=%f nl=%d\n", n, r, R, p, nl); R_FlushConsole();
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
    Frogsort6_Frogsort2_init_left (x, y, nl    , r-n+1, R-n-nl+1, p);
    Frogsort6_Frogsort2_init_right(x, y, n - nl, r    , R       , p);
  }
}

static void Frogsort6_Frogsort2_sort_right(
    ValueT *x
  , IndexT n
  , IndexT R
  , double p
);
static void Frogsort6_Frogsort2_sort_left(
    ValueT *x
  , IndexT n
  , IndexT L
  , double p
){
  IndexT nr = floor(p*n);
  // Rprintf("Frogsort6_Frogsort2_sort_left n=%d L=%d p=%f nr=%d\n", n, L, p, nr); R_FlushConsole();
  if (
#if INSERTIONSORT_LIMIT > 0
      n <= INSERTIONSORT_LIMIT || nr <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nr < 1
#endif
  ){
    Insertionsort_l2r(x, L, L + n - 1);
  }else{
    Frogsort6_Frogsort2_sort_left (x, n - nr, L, p);
    Frogsort6_Frogsort2_sort_right(x, nr, L+n+nr-1, p);
    Frogmerge_asc_left_stable(x, n - nr, nr, L);
  }
}
static void Frogsort6_Frogsort2_sort_right(
    ValueT *x
  , IndexT n
  , IndexT R
  , double p
){
  IndexT nl = floor(p*n);
  // Rprintf("Frogsort6_Frogsort2_sort_right n=%d R=%d p=%f nl=%d\n", n, R, p, nl); R_FlushConsole();
  if (
#if INSERTIONSORT_LIMIT > 0
      n <= INSERTIONSORT_LIMIT || nl <= p*INSERTIONSORT_LIMIT
#else
  n <= 2 || nl < 1
#endif
){
    Insertionsort_l2r(x, R - n + 1, R);
  }else{
    Frogsort6_Frogsort2_sort_left (x, nl, R-n-nl+1, p);
    Frogsort6_Frogsort2_sort_right(x, n - nl, R, p);
    Frogmerge_asc_right_stable(x, nl, n - nl, R);
  }
}


void Frogsort6_Frogsort2_insitu(ValueT *x
                        , IndexT n
                        , double f
                        , PerfT *p
)
{
  IndexT b = ceil(f*n);
  f = ((double)b) / n;
  double fi = ((double) b) / (n+b); // f inner
  IndexT nl = floor(fi * n);  // inner
  IndexT nr = n - nl;         // outer
  IndexT bl = b - nl;         // inner
  IndexT br = n - nr;         // outer
  if (bl==0 || br==0){
    Frogsort6_Frogsort2_Frogsort1_insitu(x, n, p);
    return;
  }
  ValueT *aux = (ValueT *) MALLOC(b, ValueT);
  ValueT *ori;
  ori = x + (n - nr - br);
  Frogsort6_Frogsort2_init_left(x      , aux, nl, 0     , 0         , f);
  Frogsort6_Frogsort2_init_right(x + nl, ori, nr, nr - 1, nr + br - 1, f);
  Frogsort6_Frogsort2_sort_right(ori, nr, nr + br - 1, f);
  Frogsort6_Frogsort2_sort_left(aux, nl, 0, f);
  Frogmerge_asc_right_final(x, aux, nl, ori+br, nr);
  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + b / (double) n;
  return;
}

void Frogsort6_Frogsort2_exsitu(ValueT *x
                        , IndexT n
                        , double f
                        , PerfT *p
)
{
  ValueT *aux;
  IndexT b = floor(f*n);
  if (b==0){
    Frogsort6_Frogsort2_Frogsort1_exsitu(x, n, p);
    return;
  }
  IndexT naux = n + b;
  aux = (ValueT *) MALLOC(naux, ValueT);
  Frogsort6_Frogsort2_init_left(x, aux, n, 0, 0, f);
  Frogsort6_Frogsort2_sort_left(aux, n, 0, f);
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}

static void Frogsort6_initsort_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT b
, IndexT n2
, IndexT r
, IndexT R
);
static void Frogsort6_initsort_left(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT b
, IndexT n2
, IndexT l
, IndexT L
){
  IndexT nr,nl,br,bl;
  IndexT r,R;
  //Rprintf("Frogsort6_sort_left n=%d b=%d l=%d L=%d\n", n, b, l, L); R_FlushConsole();
  if (n <= n2){
  	// fork
  	double f = b/((double)n);
    if (f>0.5)
      f = 0.5;
  	Frogsort6_Frogsort2_init_left(x, y, n, l, L, f);
  	Frogsort6_Frogsort2_sort_left(y, n, L, f);
  }else{
    // cut
		nr = b; // == min(b, n / 2) ensured by recursion
		nl = n - nr;
		br = min(b, nr / 2);
		bl = min(b, nl / 2);
		r = l + n - 1;
		R = L + n + b - 1;
		Frogsort6_initsort_left (x, y, nl, bl, n2, l, L);
		Frogsort6_initsort_right(x, y, nr, br, n2, r, R);
		Frogmerge_asc_left_stable(y, nl, nr, L);
	}
}
static void Frogsort6_initsort_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT b
, IndexT n2
, IndexT r
, IndexT R
){
  IndexT nr,nl,br,bl;
  IndexT l,L;
  //Rprintf("Frogsort6_sort_right n=%d b=%d r=%d R=%d\n", n, b, r, R); R_FlushConsole();
  if (n <= n2){
    // fork
    double f = b/((double)n);
    if (f>0.5)
      f = 0.5;
    Frogsort6_Frogsort2_init_right(x, y, n, r, R, f);
    Frogsort6_Frogsort2_sort_right(y, n, R, f);
  }else{
	  // cut
		nl = b; // == min(b, n / 2) ensured by recursion
		nr = n - nl;
		bl = min(b, nl / 2);
		br = min(b, nr / 2);
		l = r - n + 1;
		L = R - n - b + 1;
		Frogsort6_initsort_left (x, y, nl, bl, n2, l, L);
		Frogsort6_initsort_right(x, y, nr, br, n2, r, R);
		Frogmerge_asc_right_stable(y, nl, nr, R);
  }
}



void Frogsort6_insitu(ValueT *x
, IndexT n
, double f3
, double f2
, PerfT *p
)
{
  if (f3==f2){
    Frogsort6_Frogsort2_insitu(x, n, f3, p);
    return;
  }
  IndexT b = ceil(f3*n);  // xx adjust to new calc of Frogsort2 ??
  f3 = ((double)b) / n;
  double fi = ((double) b) / (n+b); // f inner
  if (f2<f3)
      f2 = f3;
  IndexT nl = floor(fi * n); // inner
  IndexT nr = n - nl;        // outer
  IndexT bl = b - nl;        // inner
  IndexT br = n - nr;        // outer
  IndexT b2l = floor(f2*nl);
  IndexT b2r = floor(f2*nr);
#if INSERTIONSORT_LIMIT > 0
  if (bl==0 || br==0 || nl <= b2l || nr <= b2r || b2l < INSERTIONSORT_LIMIT || b2r < INSERTIONSORT_LIMIT)
#else
  if (bl==0 || br==0 || nl <= b2l || nr <= b2r || b2l < 1 || b2r < 1)
#endif
  {
    Frogsort6_Frogsort2_insitu(x, n, f3, p);
    return;
  }
  ValueT *aux = (ValueT *) MALLOC(b, ValueT);
  //Rprintf("f3=%g nl=%d nr=%d bl=%d br=%d b2l=%d b2r=%d\n", f3, nl, nr, bl, br, b2l, b2r);
  Frogsort6_initsort_left (x     , aux              , nl, bl, b2l, 0     , 0          );
  Frogsort6_initsort_right(x + nl, x + (n - nr - br), nr, br, b2r, nr - 1, nr + br - 1);
  Frogmerge_asc_right_final(x, aux, nl, x + (n - nr), nr);

  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + b / (double) n;
  return;
}


void Frogsort6_exsitu(ValueT *x
, IndexT n
, double f3
, double f2
, PerfT *p
)
{
  if (f3 == f2){
    Frogsort6_Frogsort2_exsitu(x, n, f3, p);
    return;
  }
  IndexT b =  floor(f3*n);    // the shared buffer size for knifed execution (like Frogsort3)
  IndexT n2 =  floor(b/f2);  // from this n we switch to non-shared splitted buffer for forked execution (Frogsort2)
#if INSERTIONSORT_LIMIT > 0
  if (b==0 || n <= n2 || n2 < INSERTIONSORT_LIMIT)
#else
    if (b==0 || n <= n2 || n2 < 1)
#endif
  {
    Frogsort6_Frogsort2_exsitu(x, n, f3, p);
    return;
  }
  IndexT naux = n + b;
  //Rprintf("n=%d nl=%d nr=%d f3=%f f2=%f b=%d n2=%d naux=%d\n", n, nl, nr, f3, f2, b, n2, naux);
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  Frogsort6_initsort_left (x, aux, n, b, n2, 0, 0);
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];

// Mild tuning: let the lst merge copy back
//   IndexT nl = n / 2;
//   IndexT nr = n - nl;
//   Frogsort6_initsort_left (x     , aux                   , nl, b, n2, 0     , 0          );
//   Frogsort6_initsort_right(x + nl, aux + (naux - nr - b), nr, b, n2, nr - 1, nr + b - 1);
//   Frogmerge_asc_right_full(x, aux, nl, aux + (naux - nr), nr);

  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}
