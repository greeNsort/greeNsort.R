/*
# greeNsort Simplsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

/* indirecting via blocks similar to quicksortBM does not help
static void Simplsort_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT i=l,j=m+1,k=l;
  // begin of block processing
  ValueT L[MERGESORT_BLOCKSIZE], R[MERGESORT_BLOCKSIZE];
  IndexT I=MERGESORT_BLOCKSIZE, J=MERGESORT_BLOCKSIZE;
  while(i+MERGESORT_BLOCKSIZE <= m && j+MERGESORT_BLOCKSIZE <= r){
    if (I==MERGESORT_BLOCKSIZE){
      I = 0;
      while(I<MERGESORT_BLOCKSIZE)
        L[I++] = src[i++];
      I = 0;
    }
    if (J==MERGESORT_BLOCKSIZE){
      J = 0;
      while(J<MERGESORT_BLOCKSIZE)
        R[J++] = src[j++];
      J = 0;
    }
    while(I<MERGESORT_BLOCKSIZE && J<MERGESORT_BLOCKSIZE){
      tar[k++] = LT(R[J], L[I]) ? R[J++] : L[I++];
    }
  }
  while(I<MERGESORT_BLOCKSIZE)
    tar[k++] = LT(src[j], L[I]) ? src[j++] : L[I++];
  while(J<MERGESORT_BLOCKSIZE)
    tar[k++] = LT(R[J], src[i]) ? R[J++] : src[i++];
  // end of block processing
  while(i<=m && j<=r){
    tar[k++] = LT(src[j], src[i]) ? src[j++] : src[i++];
  }
  while(i <= m)
    tar[k++] = src[i++];
  while(j <= r)
    tar[k++] = src[j++];
}
*/

static void Simplsort_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT i=l,j=m+1,k=l;
  while(i<=m && j<=r){
    tar[k++] = LT(src[j], src[i]) ? src[j++] : src[i++];
  }
  while(i <= m)
    tar[k++] = src[i++];
  while(j <= r)
    tar[k++] = src[j++];
}



static void Simplsort_recurse(ValueT *a, ValueT *b, IndexT l, IndexT r){
  if (l<r){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if ((r-l) < INSERTIONSORT_LIMIT){
      for (m=l;m<=r; m++)
        a[m] = b[m];
      Insertionsort_l2r(a, l, r);
      return ;
    }
#endif
    m = l + (r-l)/2;
    Simplsort_recurse(b, a, l  , m);
    Simplsort_recurse(b, a, m+1, r);
    Simplsort_merge(a, b, l, m, r);
  }
}


void Simplsort_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  Simplsort_recurse(x, aux, 0, n-1);
  FREE(aux);
}

void Simplsort_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  Simplsort_recurse(aux, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
