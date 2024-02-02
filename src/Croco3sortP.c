/*
# greeNsort Croco3sort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/


//#define KMERGE_DEBUG 1
#undef KMERGE_DEBUG
#include "kmerge.h"

#include "Insertionsort_l2r.h"

/*

nl
11>=22>=33

nr
11<=22<=33

L
1..23.
123...

R
.12..3
...123

*/

static void Croco3sort_init_right(
    ValueT *y  // target
  , ValueT *x  // source
  , IndexT n
  , IndexT r
  , IndexT R
);
static void Croco3sort_init_left(
    ValueT *y
  , ValueT *x
  , IndexT n
  , IndexT l
  , IndexT L
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco3sort_init_left n=%d l=%d L=%d\n", n, l, L);
#endif
  IndexT n1,n2,n3;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += l;
    y += L;
    for (n1=0; n1<n; n1++)
      y[n1] = x[n1];
    Insertionsort_l2r(y, 0, n - 1);
  }
#else
  if (n <= 2){
    if (n == 2){
      if (LT(x[l+1], x[l])){
        y[L+1] = x[l];
        y[L] = x[l+1];
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
    n1 = n2 = n3 = n / 3;
    n -= (n3+n3+n3);
    n1 += (n>0 ? 1 : 0);
    n2 += (n>1 ? 1 : 0);
    // 1..23.
    Croco3sort_init_left (y, x, n1, l        , L              );  // l1 L1
    Croco3sort_init_right(y, x, n2, l+n1+n2-1, L+n1+n1+n2+n2-1);  // r2 R2
    Croco3sort_init_left (y, x, n3, l+n1+n2  , L+n1+n1+n2+n2  );  // l3 L3
  }
}
static void Croco3sort_init_right(
    ValueT *y
  , ValueT *x
  , IndexT n
  , IndexT r
  , IndexT R
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco3sort_init_right n=%d r=%d R=%d\n", n, r, R);
#endif
  IndexT n1,n2,n3;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += r - n + 1;
    y += R - n + 1;
    for (n1=0; n1<n; n1++)
      y[n1] = x[n1];
    Insertionsort_l2r(y, 0, n - 1);
  }
#else
  if (n <= 2){
    if (n == 2){
      if (LT(x[r], x[r-1])){
        y[R-1] = x[r];
        y[R] = x[r-1];
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
    n1 = n2 = n3 = n / 3;
    n -= (n1+n1+n1);
    n3 += (n>0 ? 1 : 0);
    n2 += (n>1 ? 1 : 0);
    // .12..3
    Croco3sort_init_right(y, x, n1, r-n3-n2  , R-n3-n3-n2-n2  );  // r1 R1
    Croco3sort_init_left (y, x, n2, r-n3-n2+1, R-n3-n3-n2-n2+1);  // l2 L2
    Croco3sort_init_right(y, x, n3, r        , R              );  // r3 R3
  }
}


static void Croco3sort_init_right_inplace(
    ValueT *y  // target
  , ValueT *x  // source
  , IndexT n
  , IndexT r
  , IndexT R
);
static void Croco3sort_init_left_inplace(
    ValueT *y
  , ValueT *x
  , IndexT n
  , IndexT l
  , IndexT L
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco3sort_init_left_inplace n=%d l=%d L=%d\n", n, l, L);
#endif
  IndexT n1,n2,n3;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += l;
    y += L;
    for (n1=0; n1<n; n1++)
      y[n1] = x[n1];
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
#ifdef KMERGE_DEBUG
      Rprintf("%d:%f  %d:%f\n", L, y[L], L+1, y[L+1]);
#endif
    }else{
      y[L] = x[l];
#ifdef KMERGE_DEBUG
      Rprintf("%d:%f\n", L, y[L]);
#endif
    }
  }
#endif
  else{
    n1 = n2 = n3 = n / 3;
    n -= (n3+n3+n3);
    n1 += (n>0 ? 1 : 0);
    n2 += (n>1 ? 1 : 0);
    // 1..23.
    Croco3sort_init_left_inplace (y, x, n1, l        , L              );  // l1 L1
    Croco3sort_init_right_inplace(y, x, n2, l+n1+n2-1, L+n1+n1+n2+n2-1);  // r2 R2
    Croco3sort_init_left_inplace (y, x, n3, l+n1+n2  , L+n1+n1+n2+n2  );  // l3 L3
  }
}
static void Croco3sort_init_right_inplace(
    ValueT *y
  , ValueT *x
  , IndexT n
  , IndexT r
  , IndexT R
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco3sort_init_right_inplace n=%d r=%d R=%d\n", n, r, R);
#endif
  IndexT n1,n2,n3;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += r - n + 1;
    y += R - n + 1;
    for (n1=0; n1<n; n1++)
      y[n1] = x[n1];
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
#ifdef KMERGE_DEBUG
      Rprintf("%d:%f  %d:%f\n", R-1, y[R-1], R, y[R]);
#endif
    }else{
      y[R] = x[r];
#ifdef KMERGE_DEBUG
      Rprintf("%d:%f\n", R, y[R]);
#endif
    }
  }
#endif
  else{
    n1 = n2 = n3 = n / 3;
    n -= (n1+n1+n1);
    n3 += (n>0 ? 1 : 0);
    n2 += (n>1 ? 1 : 0);
    // .12..3
    Croco3sort_init_right_inplace(y, x, n1, r-n3-n2  , R-n3-n3-n2-n2  );  // r1 R1
    Croco3sort_init_left_inplace (y, x, n2, r-n3-n2+1, R-n3-n3-n2-n2+1);  // l2 L2
    Croco3sort_init_right_inplace(y, x, n3, r        , R              );  // r3 R3
  }
}


// like before but using a register for swapping (to protect against inplace overwrite)
static void Croco3sort_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
);
static void Croco3sort_sort_left(
    ValueT *y
  , IndexT n
  , IndexT L
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco3sort_sort_left_inplace n=%d L=%d\n", n, L);
#endif
  IndexT n1,n2,n3;
#if INSERTIONSORT_LIMIT > 0
  if (n > INSERTIONSORT_LIMIT)
#else
  if (n > 2)
#endif
  {
  n1 = n2 = n3 = n / 3;
  n1 += ((n - (n3+n3+n3))>0 ? 1 : 0);
  n2 += ((n - (n3+n3+n3))>1 ? 1 : 0);
  // 1..23.
  Croco3sort_sort_left (y, n1, L              );  // L1
  Croco3sort_sort_right(y, n2, L+n1+n1+n2+n2-1);  // R2
  Croco3sort_sort_left (y, n3, L+n1+n1+n2+n2  );  // L3
  y += L; // L1
  left_pimerge3(y+n-1, y, y+n1-1, y+n1+n1+n2, y+n1+n1+n2+n2-1, y+n1+n1+n2+n2, y+n1+n2+n-1);
  }
}
static void Croco3sort_sort_right(
    ValueT *y
  , IndexT n
  , IndexT R
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco3sort_sort_right_inplace n=%d R=%d\n", n, R);
#endif
  IndexT n1,n2,n3;
#if INSERTIONSORT_LIMIT > 0
  if (n > INSERTIONSORT_LIMIT)
#else
  if (n > 2)
#endif
  {
  n1 = n2 = n3 = n / 3;
  n3 += ((n - (n1+n1+n1))>0 ? 1 : 0);
  n2 += ((n - (n1+n1+n1))>1 ? 1 : 0);
  // .12..3
  Croco3sort_sort_right(y, n1, R-n3-n3-n2-n2  );  // R1
  Croco3sort_sort_left (y, n2, R-n3-n3-n2-n2+1);  // L2
  Croco3sort_sort_right(y, n3, R              );  // R3
  y += R; // r3
  right_pimerge3(y-n+1, y-n3-n3-n2-n2-n1+1, y-n3-n3-n2-n2, y-n3-n3-n2-n2+1, y-n3-n3-n2, y-n3+1, y);
  }
}


void Croco3sort_insitu(ValueT *x, IndexT n)
{
  IndexT const k = 3;
  if (n < k){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
  IndexT nr = n /2;  // amount of data that can be handled in original  memory
  IndexT nl = n - nr;           // size of buffer
  IndexT naux = nl + nl;
#ifdef KMERGE_DEBUG
  Rprintf("n=%d nl=%d nr=%d naux=%d\n", n, nl, nr, naux);
#endif
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
#ifdef KMERGE_DEBUG
  Rprintf("Croco3sort_init_left (aux, x, nl, 0  , 0);\n");
#endif
  Croco3sort_init_left (aux, x, nl, 0  , 0);
  Croco3sort_sort_left (aux,    nl,      0);
#ifdef KMERGE_DEBUG
  Rprintf("Croco3sort_initsort_left(x, x, nr, nl , 0);\n");
#endif
  Croco3sort_init_left_inplace( x, x, nr, nl , 0);
  Croco3sort_sort_left        ( x   , nr,      0);
#ifdef KMERGE_DEBUG
  for (IndexT i=0;i<nl;i++)
    Rprintf("p=%p i=%d aux=%f;\n", aux+i, i, aux[i]);
  for (IndexT i=0;i<nr;i++)
    Rprintf("p=%p i=%d x=%f;\n", x+i, i, x[i]);
#endif
  // this is NOT an inplace merge
  left_merge2(x+n-1, aux, aux+nl-1, x, x+nr-1);
  FREE(aux);
  return;
}

void Croco3sort_exsitu(ValueT *x, IndexT n)
{
  IndexT const k = 3;
  if (n < k){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
  ValueT *aux = (ValueT *) MALLOC(2*n, ValueT);
  Croco3sort_init_left(aux,   x, n, 0, 0);
  Croco3sort_sort_left(aux,      n,    0);
  for (IndexT i=0;i<n;i++){
    x[i] = aux[i];
  }
  FREE(aux);
  return;
}
