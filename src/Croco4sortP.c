/*
# greeNsort Croco4sort
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
11>=33>=44>=22

nr
33<=11<=22<=44

L
1..2.34.
1...234.
1234....

R
.12.3..4
.123...4
....1234

*/

static void Croco4sort_init_right(
    ValueT *y  // target
  , ValueT *x  // source
  , IndexT n
  , IndexT r
  , IndexT R
);
static void Croco4sort_init_left(
    ValueT *y
  , ValueT *x
  , IndexT n
  , IndexT l
  , IndexT L
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco4sort_init_left n=%d l=%d L=%d\n", n, l, L);
#endif
  IndexT n1,n2,n3,n4;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += l;
    y += L;
    for (n1=0; n1<n; n1++)
      y[n1] = x[n1];
    Insertionsort_l2r(y, 0, n - 1);
  }
#else
  if (n <= 3){
    if (n == 2){
      if (LT(x[l+1], x[l])){
        y[L+1] = x[l];
        y[L] = x[l+1];
      }else{
        y[L+1] = x[l+1];
        y[L] = x[l];
      }
    }else if(n==1){
      y[L] = x[l];
    }else{
      x += l;
      y += L;
      for (n1=0; n1<n; n1++)
        y[n1] = x[n1];
      Insertionsort_l2r(y, 0, n - 1);
    }
  }
#endif
  else{
    n1 = n2 = n3 = n4 = n / 4;
    n -= (n2+n2+n2+n2);
    n1 += (n>0 ? 1 : 0);
    n3 += (n>1 ? 1 : 0);
    n4 += (n>2 ? 1 : 0);
    // 1..2.34.
    Croco4sort_init_left (y, x, n1, l           , L                    );  // l1 L1
    Croco4sort_init_right(y, x, n2, l+n1+n2   -1, L+n1+n1+n2+n2      -1);  // r2 R2
    Croco4sort_init_right(y, x, n3, l+n1+n2+n3-1, L+n1+n1+n2+n2+n3+n3-1);  // r3 R3
    Croco4sort_init_left (y, x, n4, l+n1+n2+n3  , L+n1+n1+n2+n2+n3+n3  );  // l4 L4
  }
}
static void Croco4sort_init_right(
    ValueT *y
  , ValueT *x
  , IndexT n
  , IndexT r
  , IndexT R
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco4sort_init_right n=%d r=%d R=%d\n", n, r, R);
#endif
  IndexT n1,n2,n3,n4;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    x += r - n + 1;
    y += R - n + 1;
    for (n1=0; n1<n; n1++)
      y[n1] = x[n1];
    Insertionsort_l2r(y, 0, n - 1);
  }
#else
  if (n <= 3){
    if (n == 2){
      if (LT(x[r], x[r-1])){
        y[R-1] = x[r];
        y[R] = x[r-1];
      }else{
        y[R-1] = x[r-1];
        y[R] = x[r];
      }
    }else if(n == 1){
      y[R] = x[r];
    }else{
      x += r - n + 1;
      y += R - n + 1;
      for (n1=0; n1<n; n1++)
        y[n1] = x[n1];
      Insertionsort_l2r(y, 0, n - 1);
    }
  }
#endif
  else{
    n1 = n2 = n3 = n4 = n / 4;
    n -= (n3+n3+n3+n3);
    n4 += (n>0 ? 1 : 0);
    n2 += (n>1 ? 1 : 0);
    n1 += (n>2 ? 1 : 0);
    // .12..3
    Croco4sort_init_right(y, x, n1, r-n4-n3-n2  , R-n4-n4-n3-n3-n2-n2  );  // r1 R1
    Croco4sort_init_left (y, x, n2, r-n4-n3-n2+1, R-n4-n4-n3-n3-n2-n2+1);  // l2 L2
    Croco4sort_init_left (y, x, n3, r-n4-n3   +1, R-n4-n4-n3-n3      +1);  // l3 L3
    Croco4sort_init_right(y, x, n4, r           , R                    );  // r4 R4
  }
}


static void Croco4sort_init_right_inplace(
    ValueT *y  // target
  , ValueT *x  // source
  , IndexT n
  , IndexT r
  , IndexT R
);
static void Croco4sort_init_left_inplace(
    ValueT *y
  , ValueT *x
  , IndexT n
  , IndexT l
  , IndexT L
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco4sort_init_left_inplace n=%d l=%d L=%d\n", n, l, L);
#endif
  IndexT n1,n2,n3,n4;
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
  if (n <= 3){
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
    }else if(n == 1){
      y[L] = x[l];
#ifdef KMERGE_DEBUG
      Rprintf("%d:%f\n", L, y[L]);
#endif
    }else{
      x += l;
      y += L;
      for (n1=0; n1<n; n1++)
        y[n1] = x[n1];
      Insertionsort_l2r(y, 0, n - 1);
#ifdef KMERGE_DEBUG
      for (n1=0; n1<n; n1++)
        Rprintf("%d:%f\n", n1, y[n1]);
#endif
    }
  }
#endif
  else{
    n1 = n2 = n3 = n4 = n / 4;
    n -= (n2+n2+n2+n2);
    n1 += (n>0 ? 1 : 0);
    n3 += (n>1 ? 1 : 0);
    n4 += (n>2 ? 1 : 0);
    // 1..2.34.
    Croco4sort_init_left_inplace (y, x, n1, l           , L                    );  // l1 L1
    Croco4sort_init_right_inplace(y, x, n2, l+n1+n2   -1, L+n1+n1+n2+n2      -1);  // r2 R2
    Croco4sort_init_right_inplace(y, x, n3, l+n1+n2+n3-1, L+n1+n1+n2+n2+n3+n3-1);  // r3 R3
    Croco4sort_init_left_inplace (y, x, n4, l+n1+n2+n3  , L+n1+n1+n2+n2+n3+n3  );  // l4 L4
  }
}
static void Croco4sort_init_right_inplace(
    ValueT *y
  , ValueT *x
  , IndexT n
  , IndexT r
  , IndexT R
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco4sort_init_right_inplace n=%d r=%d R=%d\n", n, r, R);
#endif
  IndexT n1,n2,n3,n4;
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
  if (n <= 3){
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
    }else if(n == 1){
      y[R] = x[r];
#ifdef KMERGE_DEBUG
      Rprintf("%d:%f\n", R, y[R]);
#endif
    }else{
      x += r - n + 1;
      y += R - n + 1;
      for (n1=0; n1<n; n1++)
          y[n1] = x[n1];
      Insertionsort_l2r(y, 0, n - 1);
#ifdef KMERGE_DEBUG
      for (n1=0; n1<n; n1++)
        Rprintf("%d:%f\n", n1, y[n1]);
#endif
    }
  }
#endif
  else{
    n1 = n2 = n3 = n4 = n / 4;
    n -= (n3+n3+n3+n3);
    n4 += (n>0 ? 1 : 0);
    n2 += (n>1 ? 1 : 0);
    n1 += (n>2 ? 1 : 0);
    // .12.3..4
    Croco4sort_init_right_inplace(y, x, n1, r-n4-n3-n2  , R-n4-n4-n3-n3-n2-n2  );  // r1 R1
    Croco4sort_init_left_inplace (y, x, n2, r-n4-n3-n2+1, R-n4-n4-n3-n3-n2-n2+1);  // l2 L2
    Croco4sort_init_left_inplace (y, x, n3, r-n4-n3   +1, R-n4-n4-n3-n3      +1);  // l3 L3
    Croco4sort_init_right_inplace(y, x, n4, r           , R                    );  // r4 R4
  }
}


// like before but using a register for swapping (to protect against inplace overwrite)
static void Croco4sort_sort_right(
    ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
);
static void Croco4sort_sort_left(
    ValueT *y
  , IndexT n
  , IndexT l
  , IndexT L
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco4sort_sort_left n=%d l=%d L=%d\n", n, l, L);
  for (IndexT i=0;i<(2*n);i++)
    Rprintf("i=%d y=%f\n", L+i, y[L+i]);
#endif
  IndexT n1,n2,n3,n4;
#if INSERTIONSORT_LIMIT > 0
  if (n > INSERTIONSORT_LIMIT)
#else
    if (n > 3)
#endif
{
  n1 = n2 = n3 = n4 = n / 4;
  n1 += ((n-(n2+n2+n2+n2))>0 ? 1 : 0);
  n3 += ((n-(n2+n2+n2+n2))>1 ? 1 : 0);
  n4 += ((n-(n2+n2+n2+n2))>2 ? 1 : 0);
  // 1..2.34.
  Croco4sort_sort_left (y, n1, l           , L                    );  // l1 L1
  Croco4sort_sort_right(y, n2, l+n1+n2   -1, L+n1+n1+n2+n2      -1);  // r2 R2
  Croco4sort_sort_right(y, n3, l+n1+n2+n3-1, L+n1+n1+n2+n2+n3+n3-1);  // r3 R3
  Croco4sort_sort_left (y, n4, l+n1+n2+n3  , L+n1+n1+n2+n2+n3+n3  );  // l4 L4
  // 1...234.
#ifdef KMERGE_DEBUG
  Rprintf("<= Croco4sort_sort_left n=%d l=%d L=%d\n", n, l, L);
  Rprintf("<= n1=%d n2=%d n3=%d n4=%d\n", n1, n2, n3, n4);
  for (IndexT i=0;i<(2*n);i++)
    Rprintf("i=%d y=%f\n", L+i, y[L+i]);
#endif
  y += L+n1+n1+n2;
  ValueT *x = y+n3; // target location reaches from L+n1+n1+n2+n3 to L+n1+n1+n2+n2+n3-1
  for (IndexT i = 0; i<n2; i++)
    *(x+i) = *(y+i);
  // 1234....
  y -= (n1+n1+n2); // l1
#ifdef KMERGE_DEBUG
  y-=L;
  Rprintf("== Croco4sort_sort_left n=%d l=%d L=%d\n", n, l, L);
  Rprintf("== n1=%d n2=%d n3=%d n4=%d\n", n1, n2, n3, n4);
  for (IndexT i=0;i<(2*n);i++)
    Rprintf("i=%d y=%f\n", L+i, y[L+i]);
  y+=L;
#endif
  left_pimerge4(y+n-1, y, y+n1-1, y+n1+n-n4    , y+n-n4+n1+n2-1    , y+n-n4+n1+n2    , y+n+n-n4-n4-1        , y+n+n-n4-n4       , y+n+n-n4-1);
#ifdef KMERGE_DEBUG
  y-=L;
#endif
}
#ifdef KMERGE_DEBUG
    Rprintf("<- Croco4sort_sort_left n=%d l=%d L=%d\n", n, l, L);
    for (IndexT i=0;i<n;i++)
      Rprintf("i=%d y=%f\n", L+i, y[L+i]);
#endif
}
static void Croco4sort_sort_right(
    ValueT *y
  , IndexT n
  , IndexT r
  , IndexT R
){
#ifdef KMERGE_DEBUG
  Rprintf("Croco4sort_sort_right n=%d r=%d R=%d\n", n, r, R);
  for (IndexT i=0;i<(2*n);i++)
    Rprintf("i=%d y=%f\n", R-2*n+1+i, y[R-2*n+1+i]);
#endif
  IndexT n1,n2,n3,n4;
#if INSERTIONSORT_LIMIT > 0
  if (n > INSERTIONSORT_LIMIT)
#else
    if (n > 3)
#endif
{
  n1 = n2 = n3 = n4 = n / 4;
  n4 += ((n-(n3+n3+n3+n3))>0 ? 1 : 0);
  n2 += ((n-(n3+n3+n3+n3))>1 ? 1 : 0);
  n1 += ((n-(n3+n3+n3+n3))>2 ? 1 : 0);
  // .12.3..4
  Croco4sort_sort_right(y, n1, r-n4-n3-n2  , R-n4-n4-n3-n3-n2-n2  );  // r1 R1
  Croco4sort_sort_left (y, n2, r-n4-n3-n2+1, R-n4-n4-n3-n3-n2-n2+1);  // l2 L2
  Croco4sort_sort_left (y, n3, r-n4-n3   +1, R-n4-n4-n3-n3      +1);  // l3 L3
  Croco4sort_sort_right(y, n4, r           , R                    );  // r4 R4
  // .123...4
  y += (R-n4-n4-n3);
  ValueT *x = y-n2;  // target location reaches from R-n4-n4-n3-n3-n2+1 to R-n4-n4-n3-n2
  for (IndexT i = 0; i<n3; i++)
    *(x-i) = *(y-i);
  // ....1234
  y -= (-n4-n4-n3); // r4
  right_pimerge4(y-n+1, y-n-n+n1+1, y-n-n+n1+n1       , y-n-n+n1+n1+1        , y-n+n1-n4-n3    , y-n+n1-n4-n3+1    , y-n4-n+n1    , y-n4+1, y);
#ifdef KMERGE_DEBUG
  y-=R;
#endif
}
#ifdef KMERGE_DEBUG
    Rprintf("<- Croco4sort_sort_right n=%d r=%d R=%d\n", n, r, R);
    for (IndexT i=0;i<n;i++)
      Rprintf("i=%d y=%f\n", R-n+1+i, y[R-n+1+i]);
#endif
}


void Croco4sort_insitu(ValueT *x, IndexT n)
{
  // Special case: up to n == 2 we switch to insertion sort because otherwise we get empty branches below
  IndexT const k = 4;
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
  Rprintf("Croco4sort_init+sort_left (aux, x, nl, 0  , 0);\n");
#endif
  Croco4sort_init_left (aux, x, nl, 0  , 0);
  Croco4sort_sort_left (aux, nl, 0  , 0);
#ifdef KMERGE_DEBUG
  Rprintf("Croco4sort_init+sort_left(x, x, nr, nl , 0);\n");
#endif
  Croco4sort_init_left_inplace( x, x, nr, nl , 0);
  Croco4sort_sort_left        ( x   , nr, nl , 0);
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

void Croco4sort_exsitu(ValueT *x, IndexT n)
{
  IndexT const k = 4;
  if (n < k){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
  ValueT *aux = (ValueT *) MALLOC(2*n, ValueT);
  Croco4sort_init_left(aux,   x, n, 0, 0);
  Croco4sort_sort_left(aux,      n, 0, 0);
  for (IndexT i=0;i<n;i++){
    x[i] = aux[i];
  }
  FREE(aux);
  return;
}
