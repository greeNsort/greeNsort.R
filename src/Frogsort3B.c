/*
# greeNsort Frogsort3 B-tuned (not really)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "FrogmergeB.h"
#include "Insertionsort_l2r.h"

static IndexT min(IndexT a, IndexT b){
	return b < a ? b : a;
}


static void Frogsort3B_Frogsort1_init_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
);
static void Frogsort3B_Frogsort1_init_left(
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
    Frogsort3B_Frogsort1_init_left (x, y, n - nr, l    , L       );
    Frogsort3B_Frogsort1_init_right(x, y, nr    , l+n-1, L+n+nr-1);
  }
}
static void Frogsort3B_Frogsort1_init_right(
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
    Frogsort3B_Frogsort1_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    Frogsort3B_Frogsort1_init_right(x, y, n - nl, r    , R       );
  }
}


static void Frogsort3B_Frogsort1_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
);
static void Frogsort3B_Frogsort1_sort_left(
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
  Frogsort3B_Frogsort1_sort_left (y, n - nr, L       );
  Frogsort3B_Frogsort1_sort_right(y, nr    , L+n+nr-1);
  FrogmergeB_asc_left_stable(y, n - nr, nr, L);
}
}
static void Frogsort3B_Frogsort1_sort_right(
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
  Frogsort3B_Frogsort1_sort_left (y, nl    , R-n-nl+1);
  Frogsort3B_Frogsort1_sort_right(y, n - nl, R       );
  FrogmergeB_asc_right_stable(y, nl, n - nl, R);
}
}


void Frogsort3B_Frogsort1_insitu(ValueT *x, IndexT n, PerfT *p)
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
  Frogsort3B_Frogsort1_init_left(x      , aux, nl, 0     , 0         );
  Frogsort3B_Frogsort1_init_right(x + nl, x + (n - nr - br), nr, nr - 1      , nr + br - 1);
  Frogsort3B_Frogsort1_sort_left(aux, nl, 0     );
  Frogsort3B_Frogsort1_sort_right(x + (n - nr - br), nr, nr + br - 1);
  FrogmergeB_asc_right_final (x     , aux              , nl, x + (n - nr), nr         );
  // Better tuning: see below
  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + naux / (double) n;
  return;
}


void Frogsort3B_Frogsort1_exsitu(ValueT *x, IndexT n, PerfT *p)
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
  Frogsort3B_Frogsort1_init_left(x, aux, n, 0, 0);
  Frogsort3B_Frogsort1_sort_left(aux, n, 0);
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];

  // Mild tuning: let the lst merge copy back
  // Frogsort3B_Frogsort1_init_left (x     , aux                   , nl, 0     , 0          );
  // Frogsort3B_Frogsort1_init_right(x + nl, aux + (naux - nr - br), nr, nr - 1, nr + br - 1);
  // Frogsort3B_Frogsort1_sort_left (aux                   , nl, 0          );
  // Frogsort3B_Frogsort1_sort_right(aux + (naux - nr - br), nr, nr + br - 1);
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





static void Frogsort3B_initsort_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT b
, IndexT r
, IndexT R
);
static void Frogsort3B_initsort_left(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT b
, IndexT l
, IndexT L
){
  IndexT nr,nl,br,bl;
  IndexT r,R;
  //Rprintf("Frogsort3B_sort_left n=%d b=%d l=%d L=%d\n", n, b, l, L); R_FlushConsole();
  if (n <= b+b+1){
  	// fork
  	Frogsort3B_Frogsort1_init_left(x, y, n, l, L);
    Frogsort3B_Frogsort1_sort_left(y, n, L);
  }else{
    // cut
		nr = b; // == min(b, n / 2) ensured by recursion
		nl = n - nr;
		br = min(b, nr / 2);
		bl = min(b, nl / 2);
		r = l + n - 1;
		R = L + n + b - 1;
		Frogsort3B_initsort_left (x, y, nl, bl, l, L);
		Frogsort3B_initsort_right(x, y, nr, br, r, R);
		FrogmergeB_asc_left_stable(y, nl, nr, L);
	}
}
static void Frogsort3B_initsort_right(
  ValueT *x
, ValueT *y
, IndexT n
, IndexT b
, IndexT r
, IndexT R
){
  IndexT nr,nl,br,bl;
  IndexT l,L;
  //Rprintf("Frogsort3B_sort_right n=%d b=%d r=%d R=%d\n", n, b, r, R); R_FlushConsole();
  if (n <= b+b+1){
	// fork
	  Frogsort3B_Frogsort1_init_right(x, y, n, r, R);
    Frogsort3B_Frogsort1_sort_right(y, n, R);
  }else{
	  // cut
		nl = b; // == min(b, n / 2) ensured by recursion
		nr = n - nl;
		bl = min(b, nl / 2);
		br = min(b, nr / 2);
		l = r - n + 1;
		L = R - n - b + 1;
		Frogsort3B_initsort_left (x, y, nl, bl, l, L);
		Frogsort3B_initsort_right(x, y, nr, br, r, R);
		FrogmergeB_asc_right_stable(y, nl, nr, R);
  }
}



void Frogsort3B_insitu(ValueT *x
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
  IndexT br = nl;             // outer
  IndexT bl = b - nl;         // inner
  //Rprintf("f=%f fi=%f n=%d b=%d nl=%d nr=%d bl=%d br=%d\n", f, fi, n, b, nl, nr, bl, br);
#if INSERTIONSORT_LIMIT > 0
  if (bl==0 || br==0 || nl <= bl+bl+1 || nr <= br+br+1 || bl+bl+1 < INSERTIONSORT_LIMIT)
#else
    if (bl==0 || br==0 || nl <= bl+bl+1 || nr <= br+br+1 || bl+bl+1 < 3)
#endif
{
    Frogsort3B_Frogsort1_insitu(x, n, p);
    return;
  }
  ValueT *aux = (ValueT *) MALLOC(b, ValueT);
  //Rprintf("f=%g nl=%d nr=%d bl=%d br=%d\n", f, nl, nr, bl, br);
  Frogsort3B_initsort_left (x     , aux              , nl, bl, 0     , 0          );
  Frogsort3B_initsort_right(x + nl, x + (n - nr - br), nr, br, nr - 1, nr + br - 1);
  FrogmergeB_asc_right_final(x, aux, nl, x + (n - nr), nr);
  FREE(aux);
  p->secs = getNewSecs();
  p->size = 1 + b / (double) n;
  return;
}

void Frogsort3B_exsitu(ValueT *x
, IndexT n
, double f
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
    Frogsort3B_Frogsort1_exsitu(x, n, p);
    return;
  }
  IndexT naux = n + b;
  //Rprintf("n=%d nl=%d nr=%d f=%f b=%d naux=%d\n", n, nl, nr, f, b, naux);
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  Frogsort3B_initsort_left (x     , aux                   , n, b, 0     , 0          );
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];


// Mild tuning: let the lst merge copy back
//   IndexT nl = n / 2;
//   IndexT nr = n - nl;
//   Frogsort3B_initsort_left (x     , aux                   , nl, b, 0     , 0          );
//   Frogsort3B_initsort_right(x + nl, aux + (naux - nr - b), nr, b, nr - 1, nr + b - 1);
//   FrogmergeB_asc_right_full(x, aux, nl, aux + (naux - nr), nr);

  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}
