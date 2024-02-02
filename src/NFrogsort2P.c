/*
# greeNsort Frog2 sorting Matrix-Columns by movig separated first row keys 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "NInsertionsort_l2r.h" // needed first for KEY redefinition
#include "NFrogmergeP.h"

/*
static void NFrogsort2P_NFrogsort1P_init_right(
    IntIndT *x
  , IntIndT *y
  , IndexT n
  , IndexT r
  , IndexT R
);
static void NFrogsort2P_NFrogsort1P_init_left(
    IntIndT *x
  , IntIndT *y
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
    NInsertionsort_l2r(y, 0, n - 1);
  }
#else
  IntIndT t;
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
    NFrogsort2P_NFrogsort1P_init_left (x, y, n - nr, l    , L       );
    NFrogsort2P_NFrogsort1P_init_right(x, y, nr    , l+n-1, L+n+nr-1);
  }
}
static void NFrogsort2P_NFrogsort1P_init_right(
    IntIndT *x
  , IntIndT *y
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
    NInsertionsort_l2r(y, 0, n - 1);
  }
#else
  IntIndT t;
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
    NFrogsort2P_NFrogsort1P_init_left (x, y, nl    , r-n+1, R-n-nl+1);
    NFrogsort2P_NFrogsort1P_init_right(x, y, n - nl, r    , R       );
  }
}
*/



static void NFrogsort2P_NFrogsort1P_init_right(
  IntValueT *x
, IntIndT *y
, IndexT n
, IndexT m
, IndexT r
, IndexT R
);
static void NFrogsort2P_NFrogsort1P_init_left(
  IntValueT *x
, IntIndT *y
, IndexT n
, IndexT m
, IndexT l
, IndexT L
){
  IndexT nr;
#if INSERTIONSORT_LIMIT > 0
  IndexT j;
  if (n <= INSERTIONSORT_LIMIT){
    x += l*m;
    y += L;
    for (nr=0, j=0; nr<n; nr++, j+=m){
      (y+nr)->key = x[j];
      (y+nr)->rec = l+nr;
    }
    NInsertionsort_l2r(y, 0, n - 1);
  }
#else
  IntIndT a,b;
  if (n <= 2){
    if (n == 2){
      a.key = x[(l+1)*m];
      b.key = x[l*m];
      if (LT(a, b)){
        (y+L+1)->key = x[l*m];
        (y+L+1)->rec = l;
        (y+L)->key = x[(l+1)*m];
        (y+L)->rec = l+1;
      }else{
        (y+L)->key = x[l*m];
        (y+L)->rec = l;
        (y+L+1)->key = x[(l+1)*m];
        (y+L+1)->rec = l+1;
      }
    }else{
      (y+L)->key = x[l*m];
      (y+L)->rec = l;
    }
  }
#endif
  else{
    nr = n  / 2;
    NFrogsort2P_NFrogsort1P_init_left (x, y, n - nr, m, l    , L       );
    NFrogsort2P_NFrogsort1P_init_right(x, y, nr    , m, l+n-1, L+n+nr-1);
  }
}
static void NFrogsort2P_NFrogsort1P_init_right(
  IntValueT *x
, IntIndT *y
, IndexT n
, IndexT m
, IndexT r
, IndexT R
){
  IndexT nl;
#if INSERTIONSORT_LIMIT > 0
  IndexT j;
  if (n <= INSERTIONSORT_LIMIT){
    x += (r - n + 1)*m;
    y += R - n + 1;
    for (nl=n-1, j=(n-1)*m; nl >= 0; nl--,j-=m){
      (y+nl)->key = x[j];
      (y+nl)->rec = r - n + 1 + nl;
    }
    NInsertionsort_l2r(y, 0, n - 1);
  }
#else
  IntIndT a,b;
  if (n <= 2){
    if (n == 2){
      a.key = x[r*m];
      b.key = x[(r-1)*m];
      if (LT(a, b)){
        (y+R-1)->key = x[r*m];
        (y+R-1)->rec = r;
        (y+R)->key = x[(r-1)*m];
        (y+R)->rec = r-1;
      }else{
        (y+R)->key = x[r*m];
        (y+R)->rec = r;
        (y+R-1)->key = x[(r-1)*m];
        (y+R-1)->rec = r-1;
      }
    }else{
      (y+R)->key = x[r*m];
      (y+R)->rec = r;
    }
  }
#endif
  else{
    nl = n  / 2;
    NFrogsort2P_NFrogsort1P_init_left (x, y, nl    , m, r-n+1, R-n-nl+1);
    NFrogsort2P_NFrogsort1P_init_right(x, y, n - nl, m, r    , R       );
  }
}


static void NFrogsort2P_NFrogsort1P_sort_right(
    IntIndT *y
  , IndexT n
  , IndexT R
);
static void NFrogsort2P_NFrogsort1P_sort_left(
    IntIndT *y
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
  NFrogsort2P_NFrogsort1P_sort_left (y, n - nr, L       );
  NFrogsort2P_NFrogsort1P_sort_right(y, nr    , L+n+nr-1);
  NFrogmergeP_asc_left(y+L+n-1, y+L, y+(L+(n-nr)-1), y+(L+n), y+(L+n+nr-1));
}
}
static void NFrogsort2P_NFrogsort1P_sort_right(
    IntIndT *y
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
  NFrogsort2P_NFrogsort1P_sort_left (y, nl    , R-n-nl+1);
  NFrogsort2P_NFrogsort1P_sort_right(y, n - nl, R       );
  NFrogmergeP_asc_right(y+R-n+1, y+(R-n-nl+1), y+(R-n), y+(R-(n-nl)+1), y+R);
}
}


/*
static void NFrogsort2P_init_right(
    IntIndT *x
  , IntIndT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double p
);
static void NFrogsort2P_init_left(
    IntIndT *x
  , IntIndT *y
  , IndexT n
  , IndexT l
  , IndexT L
  , double p
){
  IndexT nr = floor(p*n);
  // Rprintf("NFrogsort2P_init_left n=%d l=%d L=%d p=%f nr=%d\n", n, l, L, p, nr); R_FlushConsole();
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
    x += l;
    y += L;
    for (nr=0; nr<n; nr++)
      y[nr] = x[nr];
    // NInsertionsort_l2r(y, 0, n - 1);
  }else{
    NFrogsort2P_init_left (x, y, n - nr, l    , L       , p);
    NFrogsort2P_init_right(x, y, nr    , l+n-1, L+n+nr-1, p);
  }
}
static void NFrogsort2P_init_right(
    IntIndT *x
  , IntIndT *y
  , IndexT n
  , IndexT r
  , IndexT R
  , double p
){
  IndexT nl = floor(p*n);
  // Rprintf("NFrogsort2P_init_right n=%d r=%d R=%d p=%f nl=%d\n", n, r, R, p, nl); R_FlushConsole();
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
    x += r - n + 1;
    y += R - n + 1;
    for (nl=0; nl<n; nl++)
      y[nl] = x[nl];
    // NInsertionsort_l2r(y, 0, n - 1);
  }else{
    NFrogsort2P_init_left (x, y, nl    , r-n+1, R-n-nl+1, p);
    NFrogsort2P_init_right(x, y, n - nl, r    , R       , p);
  }
}
*/


static void NFrogsort2P_init_right(
  IntValueT *x
, IntIndT *y
, IndexT n
, IndexT m
, IndexT r
, IndexT R
, double p
);
static void NFrogsort2P_init_left(
    IntValueT *x
, IntIndT *y
, IndexT n
, IndexT m
, IndexT l
, IndexT L
, double p
){
  IndexT j, nr = floor(p*n);
  // Rprintf("NFrogsort2P_init_left n=%d l=%d L=%d p=%f nr=%d\n", n, l, L, p, nr); R_FlushConsole();
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
    y += L;
    for (nr=0,j=0; nr<n; nr++, j+=m){
      (y+nr)->key = x[j];
      (y+nr)->rec = l+nr;
    }
    // NInsertionsort_l2r(y, 0, n - 1);
  }else{
    NFrogsort2P_init_left (x, y, n - nr, m, l    , L       , p);
    NFrogsort2P_init_right(x, y, nr    , m, l+n-1, L+n+nr-1, p);
  }
}
static void NFrogsort2P_init_right(
    IntValueT *x
, IntIndT *y
, IndexT n
, IndexT m
, IndexT r
, IndexT R
, double p
){
  IndexT j, nl = floor(p*n);
  // Rprintf("NFrogsort2P_init_right n=%d r=%d R=%d p=%f nl=%d\n", n, r, R, p, nl); R_FlushConsole();
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
    y += R - n + 1;
    for (nl=0,j=0; nl<n; nl++, j+=m){
      (y+nl)->key = x[j];
      (y+nl)->rec = r - n + 1 + nl;
    }
    // NInsertionsort_l2r(y, 0, n - 1);
  }else{
    NFrogsort2P_init_left (x, y, nl    , m, r-n+1, R-n-nl+1, p);
    NFrogsort2P_init_right(x, y, n - nl, m, r    , R       , p);
  }
}


static void NFrogsort2P_sort_right(
    IntIndT *x
  , IndexT n
  , IndexT R
  , double p
);
static void NFrogsort2P_sort_left(
    IntIndT *x
  , IndexT n
  , IndexT L
  , double p
){
  IndexT nr = floor(p*n);
  // Rprintf("NFrogsort2P_sort_left n=%d L=%d p=%f nr=%d\n", n, L, p, nr); R_FlushConsole();
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
    NInsertionsort_l2r(x, L, L + n - 1);
  }else{
    NFrogsort2P_sort_left (x, n - nr, L, p);
    NFrogsort2P_sort_right(x, nr, L+n+nr-1, p);
    NFrogmergeP_asc_left(x+L+n-1, x+L, x+(L+(n-nr)-1), x+(L+n), x+(L+n+nr-1));
  }
}
static void NFrogsort2P_sort_right(
    IntIndT *x
  , IndexT n
  , IndexT R
  , double p
){
  IndexT nl = floor(p*n);
  // Rprintf("NFrogsort2P_sort_right n=%d R=%d p=%f nl=%d\n", n, R, p, nl); R_FlushConsole();
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
    NInsertionsort_l2r(x, R - n + 1, R);
  }else{
    NFrogsort2P_sort_left (x, nl, R-n-nl+1, p);
    NFrogsort2P_sort_right(x, n - nl, R, p);
    NFrogmergeP_asc_right(x+R-n+1, x+(R-n-nl+1), x+(R-n), x+(R-(n-nl)+1), x+R);
  }
}




// void NFrogsort2P_NFrogsort1P_insitu(IntIndT *x, IndexT n, PerfT *p)
// {
//   // Special case: up to n == 2 we switch to insertion sort because naux would be larger than n
//   if (n<3){
//     NInsertionsort_l2r(x, 0, n-1);
//     p->secs = getDeltaSecs();
//     p->size = 1;
//     return;
//   }
//   IndexT nr = (n / 3) * 2;
//   IndexT nl = n - nr;
//   IndexT bl = nl / 2;
//   IndexT br = nr / 2;
//   IndexT naux = nl + bl;
//   IntIndT *aux = (IntIndT *) MALLOC(naux, IntIndT);
//   NFrogsort2P_NFrogsort1P_init_left(x      , aux, nl, 0     , 0         );
//   NFrogsort2P_NFrogsort1P_init_right(x + nl, x + (n - nr - br), nr, nr - 1      , nr + br - 1);
//   NFrogsort2P_NFrogsort1P_sort_left(aux, nl, 0);
//   NFrogsort2P_NFrogsort1P_sort_right(x + (n - nr - br), nr, nr + br - 1);
//   NFrogmergeP_asc_right(x     , aux, aux+nl-1 , x+n-nr , x+n-1);
//   // Better tuning: see below
//   FREE(aux);
//   p->secs = getDeltaSecs();
//   p->size = 1 + naux / (double) n;
//   return;
// }



void NFrogsort2P_insitu(
  IntValueT *x
, IndexT n
, IndexT m
, double f
, PerfT *p
)
{
  IndexT i,j, nm=n*m;
  double IntIndSize, IntValueSize = nm*sizeof(IntValueT);
  PerfT p2, p3;
  IntIndT * ind;
  IndexT nind;
  IntValueT *aux2;
  IndexT b = floor(f*n); // xx adjust to new calc of Frogsort2 ??
  if (n<3){  // xx can use somthing bigger here (but not INSERTIONSORT_LIMIT which can be 0)
    nind = n;
    IntIndSize  = nind*sizeof(IntIndT);
    IntIndT * ind = (IntIndT *) MALLOC(nind, IntIndT);
    for (i=0,j=0; i<n; i++,j+=m){
      ind[i].key = x[j];
      ind[i].rec = i;
    }
    NInsertionsort_l2r(ind, 0, n-1);
    p2.secs = getDeltaSecs();
    p2.size = IntIndSize;
    if (m>1){
      aux2 = (IntValueT *) MALLOC(nm, IntValueT);
      Norder_exsitu(
        x  // source array of n records with m integers
        , aux2 // target array of n records with m integers for fast O(N) reordering
        , ind  // order of record indexes in struct field .rec for re-ordering
        , n    // number of records
        , m    // number of integer in record
      );
      FREE(aux2);
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize+IntValueSize;
    }else{
      for (i=0;i<n;i++)
        x[i] = ind[i].key;
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize;
    }
  }else if (b==0){
    // IntIndT * ind = (IntIndT *) MALLOC(n, IntIndT);
    // for (i=0,j=0; i<n; i++,j+=m){
    //   ind[i].key = x[j];
    //   ind[i].rec = i;
    // }
    // b = n / 2;
    // nind = n + b;
    // indaux = (IntIndT *) MALLOC(nind, IntIndT);
    // NFrogsort2P_NFrogsort1P_init_left(ind, indaux, n, 0, 0);
    // FREE(ind);
    // NFrogsort2P_NFrogsort1P_sort_left(indaux, n, 0);
    b = n / 2;
    nind = n + b;
    IntIndSize  = nind*sizeof(IntIndT);
    ind = (IntIndT *) MALLOC(nind, IntIndT);
    NFrogsort2P_NFrogsort1P_init_left(x, ind, n, m, 0, 0);
    NFrogsort2P_NFrogsort1P_sort_left(ind, n, 0);
    p2.secs = getDeltaSecs();
    p2.size = IntIndSize;
    if (m>1){
      aux2 = (IntValueT *) MALLOC(nm, IntValueT);
      Norder_exsitu(
        x   // source array of n records with m integers
        , aux2   // target array of n records with m integers for fast O(N) reordering
        , ind // order of record indexes in struct field .rec for re-ordering
        , n      // number of records
        , m      // number of integer in record
      );
      FREE(aux2);
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize+IntValueSize;
    }else{
      for (i=0;i<n;i++)
        x[i] = ind[i].key;
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize;
    }
  }else{
    // IntIndT * ind = (IntIndT *) MALLOC(n, IntIndT);
    // for (i=0,j=0; i<n; i++,j+=m){
    //   ind[i].key = x[j];
    //   ind[i].rec = i;
    // }
    // nind = n + b;
    // IntIndT * indaux = (IntIndT *) MALLOC(nind, IntIndT);
    // NFrogsort2P_init_left(ind, indaux, n, 0, 0, f);
    // FREE(ind);
    nind = n + b;
    ind = (IntIndT *) MALLOC(nind, IntIndT);
    IntIndSize  = nind*sizeof(IntIndT);
    NFrogsort2P_init_left(x, ind, n, m, 0, 0, f);
    NFrogsort2P_sort_left(ind, n, 0, f);
    p2.secs = getDeltaSecs();
    p2.size = IntIndSize;
    if (m>1){
      aux2 = (IntValueT *) MALLOC(nm, IntValueT);
      Norder_exsitu(
        x   // source array of n records with m integers
        , aux2   // target array of n records with m integers for fast O(N) reordering
        , ind    // order of record indexes in struct field .rec for re-ordering
        , n      // number of records
        , m      // number of integer in record
      );
      FREE(aux2);
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize+IntValueSize;
    }else{
      for (i=0;i<n;i++)
        x[i] = ind[i].key;
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize;
    }
  }
  // Rprintf("2 secs=%f, size=%f\n", p2.secs, p2.size);
  // Rprintf("3 secs=%f, size=%f\n", p3.secs, p3.size);
  p->secs =  p2.secs + p3.secs;
  p->size = 1 + // data
    (p2.secs*p2.size + p3.secs*p3.size) / (p->secs*IntValueSize); // buffer

}

void NFrogsort2P_exsitu(
  IntValueT *x
, IndexT n
, IndexT m
, double f
, PerfT *p
)
{
  IndexT i,j, nm=n*m;
  double IntIndSize, IntValueSize = nm*sizeof(IntValueT);
  PerfT p1, p2, p3, p4;
  IntIndT * ind;
  IndexT nind;
  IntValueT *aux,  *aux2;
  aux = (IntValueT *) MALLOC(nm, IntValueT);
  for (i=0;i<nm;i+=m)
    MMOVE(aux+i, x+i, m);
  p1.secs = getDeltaSecs();
  p1.size = 0;
  IndexT b = floor(f*n);
  if (n<3){  // xx can use somthing bigger here (but not INSERTIONSORT_LIMIT which can be 0)
    nind = n;
    IntIndSize  = nind*sizeof(IntIndT);
    IntIndT * ind = (IntIndT *) MALLOC(nind, IntIndT);
    for (i=0,j=0; i<n; i++,j+=m){
      ind[i].key = x[j];
      ind[i].rec = i;
    }
    NInsertionsort_l2r(ind, 0, n-1);
    p2.secs = getDeltaSecs();
    p2.size = IntIndSize;
    if (m>1){
      aux2 = (IntValueT *) MALLOC(nm, IntValueT);
      Norder_exsitu(
        aux  // source array of n records with m integers
      , aux2 // target array of n records with m integers for fast O(N) reordering
      , ind  // order of record indexes in struct field .rec for re-ordering
      , n    // number of records
      , m    // number of integer in record
      );
      FREE(aux2);
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize+IntValueSize;
    }else{
      for (i=0;i<n;i++)
        aux[i] = ind[i].key;
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize;
    }
  }else if (b==0){
    // IntIndT * ind = (IntIndT *) MALLOC(n, IntIndT);
    // for (i=0,j=0; i<n; i++,j+=m){
    //   ind[i].key = x[j];
    //   ind[i].rec = i;
    // }
    // b = n / 2;
    // nind = n + b;
    // indaux = (IntIndT *) MALLOC(nind, IntIndT);
    // NFrogsort2P_NFrogsort1P_init_left(ind, indaux, n, 0, 0);
    // FREE(ind);
    // NFrogsort2P_NFrogsort1P_sort_left(indaux, n, 0);
    b = n / 2;
    nind = n + b;
    IntIndSize  = nind*sizeof(IntIndT);
    ind = (IntIndT *) MALLOC(nind, IntIndT);
    NFrogsort2P_NFrogsort1P_init_left(x, ind, n, m, 0, 0);
    NFrogsort2P_NFrogsort1P_sort_left(ind, n, 0);
    p2.secs = getDeltaSecs();
    p2.size = IntIndSize;
    if (m>1){
      aux2 = (IntValueT *) MALLOC(nm, IntValueT);
      Norder_exsitu(
          aux   // source array of n records with m integers
        , aux2   // target array of n records with m integers for fast O(N) reordering
        , ind // order of record indexes in struct field .rec for re-ordering
        , n      // number of records
        , m      // number of integer in record
      );
      FREE(aux2);
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize+IntValueSize;
    }else{
      for (i=0;i<n;i++)
        aux[i] = ind[i].key;
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize;
    }
  }else{
    // IntIndT * ind = (IntIndT *) MALLOC(n, IntIndT);
    // for (i=0,j=0; i<n; i++,j+=m){
    //   ind[i].key = x[j];
    //   ind[i].rec = i;
    // }
    // nind = n + b;
    // IntIndT * indaux = (IntIndT *) MALLOC(nind, IntIndT);
    // NFrogsort2P_init_left(ind, indaux, n, 0, 0, f);
    // FREE(ind);
    nind = n + b;
    ind = (IntIndT *) MALLOC(nind, IntIndT);
    IntIndSize  = nind*sizeof(IntIndT);
    NFrogsort2P_init_left(x, ind, n, m, 0, 0, f);
    NFrogsort2P_sort_left(ind, n, 0, f);
    p2.secs = getDeltaSecs();
    p2.size = IntIndSize;
    if (m>1){
      aux2 = (IntValueT *) MALLOC(nm, IntValueT);
      Norder_exsitu(
        aux   // source array of n records with m integers
        , aux2   // target array of n records with m integers for fast O(N) reordering
        , ind    // order of record indexes in struct field .rec for re-ordering
        , n      // number of records
        , m      // number of integer in record
      );
      FREE(aux2);
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize+IntValueSize;
    }else{
      for (i=0;i<n;i++)
        aux[i] = ind[i].key;
      FREE(ind);
      p3.secs = getDeltaSecs();
      p3.size = IntIndSize;
    }
  }
  for (i=0;i<nm;i+=m)
    MMOVE(x+i, aux+i, m);
  FREE(aux);
  p4.secs = getDeltaSecs();
  p4.size = 0;
  // Rprintf("1 secs=%f, size=%f\n", p1.secs, p1.size);
  // Rprintf("2 secs=%f, size=%f\n", p2.secs, p2.size);
  // Rprintf("3 secs=%f, size=%f\n", p3.secs, p3.size);
  // Rprintf("4 secs=%f, size=%f\n", p4.secs, p4.size);
  // Rprintf("5 secs=%f, size=%f\n", p5.secs, p5.size);
  p->secs = p1.secs + p2.secs + p3.secs + p4.secs;
  p->size = 1 + // data
    (p1.secs*p1.size + p2.secs*p2.size + p3.secs*p3.size + p4.secs*p4.size) / (p->secs*IntValueSize); // buffer
}

