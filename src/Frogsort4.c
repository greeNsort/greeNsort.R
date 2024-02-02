/*
# greeNsort Frogsort4
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "Frogmerge.h"
#include "Frogmerge4.h"
#include "Insertionsort_l2r.h"

static void Frogsort4_Frogsort1_initsort_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
);
static void Frogsort4_Frogsort1_initsort_left(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT l
  , IndexT L
){
  IndexT nr;
  //Rprintf("Frogsort4_Frogsort1_initsort_left n=%d l=%d L=%d\n", n, l, L); R_FlushConsole();
  if (
#if INSERTIONSORT_LIMIT > 0
      n <= INSERTIONSORT_LIMIT
#else
    n <= 2
#endif
  ){
    x += l;
    y += L;
    for (nr=0; nr<n; nr++)  // to avoid inplace overwrite
    //for (nr=n-1; nr >= 0; nr--)  // to avoid inplace overwrite
      y[nr] = x[nr];
    Insertionsort_l2r(y, 0, n - 1);
  }else{
    nr = n  / 2;
    Frogsort4_Frogsort1_initsort_left (x, y, n - nr, l    , L       );
    Frogsort4_Frogsort1_initsort_right(x, y, nr    , l+n-1, L+n+nr-1);
    Frogmerge_asc_left_stable(y, n - nr, nr, L);
  }
}
static void Frogsort4_Frogsort1_initsort_right(
    ValueT *x
  , ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
){
  IndexT nl;
  // Rprintf("Frogsort4_Frogsort1_sort_right n=%d r=%d R=%d\n", n, r, R); R_FlushConsole();
  if (
#if INSERTIONSORT_LIMIT > 0
      n <= INSERTIONSORT_LIMIT
#else
    n <= 2
#endif
  ){
    x += r - n + 1;
    y += R - n + 1;
    for (nl=0; nl<n; nl++)
    //for (nl=n-1; nl >= 0; nl--)  // to avoid inplace overwrite
      y[nl] = x[nl];
    Insertionsort_l2r(y, 0, n - 1);
  }else{
    nl = n  / 2;
    Frogsort4_Frogsort1_initsort_left (x, y, nl    , r-n+1, R-n-nl+1);
    Frogsort4_Frogsort1_initsort_right(x, y, n - nl, r    , R       );
    Frogmerge_asc_right_stable(y, nl, n - nl, R);
  }
}




static void Frogsort4_Frogsort1_insitu(ValueT *x, IndexT n, PerfT *p)
{
  // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
  if (n<3){
    Insertionsort_l2r(x, 0, n-1);
    p->secs = getNewSecs();
    p->size=1;
    return;
  }
  IndexT nr = (n / 3) * 2;
  IndexT nl = n - nr;
  IndexT bl = nl / 2;
  IndexT br = nr / 2;
  IndexT naux = nl + bl;
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  Frogsort4_Frogsort1_initsort_left(x      , aux, nl, 0     , 0         );
  Frogsort4_Frogsort1_initsort_right(x + nl, x + (n - nr - br), nr, nr - 1      , nr + br - 1);
  Frogmerge_asc_right_final (x     , aux              , nl, x + (n - nr), nr );
  FREE(aux);
  p->secs = getNewSecs();
  p->size= 1 + naux/(double) n;;
  return;
}


void Frogsort4_Frogsort1_exsitu(ValueT *x
                                       , IndexT n
                                       , PerfT *p
)
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
  Frogsort4_Frogsort1_initsort_left (x     , aux                   , nl, 0     , 0          );
  Frogsort4_Frogsort1_initsort_right(x + nl, aux + (naux - nr - br), nr, nr - 1, nr + br - 1);
  Frogmerge_asc_right_full(x, aux, nl, aux + (naux - nr), nr);
  FREE(aux);
  p->secs = getNewSecs();
  p->size = naux / (double) n;
  return;
}






static void Frogsort4_init_right(
  ValueT *x
, ValueT *y
, IndexT d
, IndexT n
, IndexT r
);
static void Frogsort4_init_left(
  ValueT *x
, ValueT *y
, IndexT d
, IndexT n
, IndexT l
){
  IndexT nr;
  //Rprintf("Frogsort4_init_left n=%d d=%d l=%d L=%d\n", n, d, l, L); R_FlushConsole();
  if (n <= d){
    Frogsort4_Frogsort1_initsort_left(x, y, n, l, l);
  }else{
    nr = n / 2;
    Frogsort4_init_left (x, y, d, n - nr, l);
    Frogsort4_init_right(x, y, d, nr    , l+n-1);
  }
}
static void Frogsort4_init_right(
  ValueT *x
, ValueT *y
, IndexT d
, IndexT n
, IndexT r
){
  IndexT nl;
  //Rprintf("Frogsort4_init_right n=%d d=%d l=%d L=%d r=%d R=%d\n", n, d, r-n+1, R-n+1, r, R); R_FlushConsole();
  if (n <= d){
    Frogsort4_Frogsort1_initsort_left(x, y, n, r-n+1, r-n+1); // note that we initially always sort to left, because the buffer is right of us
  }else{
    nl = n / 2;
    Frogsort4_init_left (x, y, d, nl    , r-n+1);
    Frogsort4_init_right(x, y, d, n - nl, r    );
  }
}

static void Frogsort4_init_right_insitu(
    ValueT *x
  , IndexT d
  , IndexT n
  , IndexT r
);
static void Frogsort4_init_left_insitu(
    ValueT *x
  , IndexT d
  , IndexT n
  , IndexT l
){
  IndexT nr;
  //Rprintf("Frogsort4_init_left_insitu n=%d d=%d l=%d\n", n, d, l); R_FlushConsole();
  if (n <= d){
    x +=l;
    ValueT *z = x - d;
    for(nr=0;nr<n;nr++)
      z[nr] = x[nr];
    x -= l;
    Frogsort4_Frogsort1_initsort_left(x, x, n, l, l-d);
  }else{
    nr = n / 2;
    Frogsort4_init_left_insitu (x, d, n - nr, l    );
    Frogsort4_init_right_insitu(x, d, nr    , l+n-1);
  }
}
static void Frogsort4_init_right_insitu(
    ValueT *x
  , IndexT d
  , IndexT n
  , IndexT r
){
  IndexT nl;
  //Rprintf("Frogsort4_init_right_insitu n=%d d=%d l=%d r=%d\n", n, d, r-n+1, r); R_FlushConsole();
  if (n <= d){
    x += (r-n+1);
    ValueT *z = x - d;
    for(nl=0;nl<n;nl++)
      z[nl] = x[nl];
    x -= (r-n+1);
    Frogsort4_Frogsort1_initsort_left(x, x, n, r-n+1, r-n+1-d); // note that we initially always sort to left, because the buffer is right of us
  }else{
    nl = n / 2;
    Frogsort4_init_left_insitu (x, d, nl    , r-n+1);
    Frogsort4_init_right_insitu(x, d, n - nl, r    );
  }
}
// we require that n >= 2*d
static void Frogsort4_init_left_insitu_extra(
    ValueT *x
  , ValueT *z
  , IndexT d
  , IndexT n
){
  IndexT nr;
  //Rprintf("Frogsort4_init_left_insitu_extra n=%d d=%d\n", n, d); R_FlushConsole();
  if (n <= d+d){
    nr = d/2;
    Frogsort4_Frogsort1_initsort_left(x, z, d - nr, 0, 0);  // here we free d - nr elements
    Frogsort4_Frogsort1_initsort_left(x, x, nr, d - nr, 0);
    Frogmerge_asc_left_final(z, z, nr, x, d - nr); // here we free nr d elements, hence exactly d in total
    if (n>d)
      Frogsort4_Frogsort1_initsort_left(x, x, n - d, d, 0);
  }else{
    nr = n / 2;
    Frogsort4_init_left_insitu_extra (x, z, d, n - nr    );
    Frogsort4_init_right_insitu      (x, d, nr    , n-1);
  }
}

static void Frogsort4_sort_right(
  ValueT *x
, ValueT *y
, IndexT d
, IndexT n
, IndexT r
, IndexT R
);
static void Frogsort4_sort_left(
  ValueT *x
, ValueT *y
, IndexT d
, IndexT n
, IndexT l
, IndexT L
){
  IndexT nr;
  //Rprintf("Frogsort4_sort_left n=%d d=%d l=%d L=%d\n", n, d, l, L); R_FlushConsole();
  if (n <= d+d){
    if (n <= d){
      // just move
      x+=l;
      y+=L;
      //for (nr=n-1;nr>=0;nr--)
      for (nr=0;nr<n;nr++)
          y[nr] = x[nr];
    }else{
      // move and merge
      nr = n / 2;
      Frogmerge_asc_left_full(y + L, x + l, n-nr, x + l + (n-nr), nr);
    }
  }else{
    nr = n / 2;
    Frogsort4_sort_left (x, y, d, n - nr, l    , L       );
    Frogsort4_sort_right(x, y, d, nr    , l+n-1, L+n+nr-1);
    Frogmerge_asc_left_stable(y, n - nr, nr, L);
  }
}
static void Frogsort4_sort_right(
  ValueT *x
, ValueT *y
, IndexT d
, IndexT n
, IndexT r
, IndexT R
){
  IndexT nl;
  //Rprintf("Frogsort4_sort_right n=%d d=%d l=%d L=%d r=%d R=%d\n", n, d, r-n+1, R-n-n/2+1, r, R); R_FlushConsole();
  if (n <= d+d){
    if (n <= d){
      // just move
      x+=r-n+1;
      y+=R-n+1;
      //for (nl=n-1;nl>=0;nl--)  // note that in phase II we always work from right to left
      for (nl=0;nl<n;nl++)  // note that in phase II we always work from left to right
          y[nl] = x[nl];
    }else{
      // move and merge
      nl = n / 2;
      Frogmerge_asc_right_full(y + R - n + 1, x + r - n + 1, nl, x + r - (n-nl) + 1, n-nl);
    }
  }else{
    nl = n / 2;
    Frogsort4_sort_left (x, y, d, nl, r-n+1, R-n-nl+1);
    Frogsort4_sort_right(x, y, d, n - nl, r, R);
    Frogmerge_asc_right_stable(y, nl, n - nl, R);
  }
}


static void Frogsort4_sort_right_insitu(
    ValueT *x
  , ValueT *y
  , IndexT d
  , IndexT n
  , IndexT r
  , IndexT R
);
static void Frogsort4_sort_left_insitu(
    ValueT *x
  , ValueT *y
  , IndexT d
  , IndexT n
  , IndexT l
  , IndexT L
){
  IndexT nr;
  //Rprintf("Frogsort4_sort_left_insitu n=%d d=%d l=%d L=%d\n", n, d, l, L); R_FlushConsole();
  if (n <= d+d){
    if (n <= d){
      // just move
      x+=l;
      y+=L;
      //for (nr=n-1;nr>=0;nr--)
      for (nr=0;nr<n;nr++)
        y[nr] = x[nr];
    }else{
      // move and merge
      nr = n / 2;
      Frogmerge_asc_left_full(y + L, x + l, n-nr, x + l + (n-nr), nr);
    }
  }else{
    nr = n / 2;
    Frogsort4_sort_left_insitu (x, y, d, n - nr, l    , L       );
    Frogsort4_sort_right_insitu(x, y, d, nr    , l+n-1, L+n+nr-1);
    Frogmerge_asc_left_stable(y, n - nr, nr, L);
  }
}
static void Frogsort4_sort_right_insitu(
    ValueT *x
  , ValueT *y
  , IndexT d
  , IndexT n
  , IndexT r
  , IndexT R
){
  IndexT nl;
  //Rprintf("Frogsort4_sort_right_insitu n=%d d=%d l=%d L=%d r=%d R=%d\n", n, d, r-n+1, R-n-n/2+1, r, R); R_FlushConsole();
  if (n <= d+d){
    if (n <= d){
      // just move
      x+=r-n+1;
      y+=R-n+1;
      //for (nl=n-1;nl>=0;nl--)  // note that in phase II we always work from right to left
      for (nl=0;nl<n;nl++)  // note that in phase II we always work from left to right
        y[nl] = x[nl];
    }else{
      // move and merge
      nl = n / 2;
      Frogmerge_asc_right_full(y + R - n + 1, x + r - n + 1, nl, x + r - (n-nl) + 1, n-nl);
    }
  }else{
    nl = n / 2;
    Frogsort4_sort_left_insitu (x, y, d, nl, r-n+1, R-n-nl+1);
    Frogsort4_sort_right_insitu(x, y, d, n - nl, r, R);
    Frogmerge_asc_right_stable(y, nl, n - nl, R);
  }
}
static void Frogsort4_sort_left_insitu_extra(
    ValueT *x
  , ValueT *y
  , ValueT *z
  , IndexT d
  , IndexT n
 ){
  IndexT nr;
  //Rprintf("Frogsort4_sort_left_insitu_extra n=%d d=%d\n", n, d); R_FlushConsole();
  if (n <= d+d){
    if (n <= d){
      // just move
      //for (nr=n-1;nr>=0;nr--)
      for (nr=0;nr<n;nr++)
        y[nr] = z[nr];
    }else{
      // move and merge
      nr = n / 2;
      Frogmerge_asc_left_full(y, z, d, x, n - d);
    }
  }else{
    nr = n / 2;
    Frogsort4_sort_left_insitu_extra (x, y, z, d, n - nr);
    Frogsort4_sort_right_insitu(x, y, d, nr    , n-1-d, n+nr-1);
    Frogmerge_asc_left_stable(y, n - nr, nr, 0);
  }
}




void Frogsort4_insitu(ValueT *x, IndexT n
, IndexT b  // size of initial buffer
, PerfT *p
)
{
  PerfT p1,p2,p3;
  IndexT d = b+b;   // size of datablock
  IndexT nr = floor((n-d-d) / 3) * 2;
  IndexT nl = n - nr;
  if (nl < d || nr < d){
    Frogsort4_Frogsort1_insitu(x, n, p);
    return;
  }
  IndexT naux2 = nl + nl/2;
  //Rprintf("n=%d b=%d d=%d naux2=%d nl=%d nr=%d\n", n, b, d, naux2, nl, nr);

   // Phase Quasi-Inplace = sorting from left to right pieces of size <=d using a single moving buffer b
  ValueT *aux1 = (ValueT *) MALLOC(d, ValueT);
  Frogsort4_init_left_insitu_extra(x, aux1, d, nl);
  Frogsort4_init_left_insitu(x, d, nr , nl);

  p1.secs = getNewSecs();

  // Phase Semi-Inplace = leaves move merge pairs of pieces from aux1 to aux2, branches just merge as usual
  ValueT *aux2 = (ValueT *) MALLOC(naux2, ValueT);

  Frogsort4_sort_left_insitu_extra(x, aux2, aux1, d, nl);
  FREE(aux1);
  p2.secs = getNewSecs();

  Frogsort4_sort_left_insitu      (x, x,      d, nr, nl-d, 0);

  Frogmerge_asc_left_full(x, aux2, nl, x, nr);
  FREE(aux2);

  p->secs = getNewSecs();
  p3.secs = p->secs - p2.secs;
  p2.secs -= p1.secs;
  p1.size = n + d;
  p2.size = n + d + naux2;
  p3.size = n + naux2;
  p->size = (p1.secs*p1.size + p2.secs*p2.size + p3.secs*p3.size) / p->secs / n;
  return;
}

void Frogsort4_exsitu(ValueT *x, IndexT n
, IndexT b  // size of initial block
, PerfT *p
)
{
  PerfT p1,p2;
  IndexT d = b+b;   // size of datablock
  IndexT naux = n+b; // exsitu + buffer
  IndexT nr = floor((naux-d-b) / 3) * 2;
  IndexT nl = n - nr;
  if (nl < d || nr < d){
    Frogsort4_Frogsort1_exsitu(x, n, p);
    return;
  }
  IndexT naux2 = nl + nl/2;
  //Rprintf("n=%d b=%d d=%d nl=%d nr=%d naux=%d naux2=%d\n", n, b, d, nl, nr, naux, naux2);

  // Phase Quasi-Inplace = sorting from left to right pieces of size <=d using a single moving buffer b
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  Frogsort4_init_left(x   , aux   , d, nl, 0);
  Frogsort4_init_left(x+nl, aux+nl, d, nr, 0);

  p1.secs = getNewSecs();

  // Phase Semi-Inplace = leaves move merge pairs of pieces from aux to aux2, branches just merge as usual
  ValueT *aux2 = (ValueT *) MALLOC(naux2, ValueT);

  Frogsort4_sort_left(aux, aux2, d, nl, 0, 0);
  Frogsort4_sort_left(aux, aux, d, nr, nl, 0);
  Frogmerge_asc_left_full(x, aux2, nl, aux, nr);

  FREE(aux2);
  FREE(aux);

  p->secs = getNewSecs();
  p2.secs = p->secs - p1.secs;
  p1.size = naux;
  p2.size = naux + naux2;
  p->size = (p1.secs*p1.size + p2.secs*p2.size) / p->secs / n;
  return;
}

