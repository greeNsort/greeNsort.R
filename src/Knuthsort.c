/*
# greeNsort Knuthsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

/* Following Knuth we need only one termination check per iteration
static void Knuthsort_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  //Rprintf("\nl=%d m=%d r=%d\n", l, m, r);
  //for (int i=l; i<=r; i++)
    //Rprintf("in i=%d t=%f s=%f\n", i, tar[i], src[i]);
  IndexT i=l,j=m+1,k=l;
  ValueT u, v;
  // begin of block processing
  ValueT L[MERGESORT_BLOCKSIZE], R[MERGESORT_BLOCKSIZE];
  IndexT I=MERGESORT_BLOCKSIZE, J=MERGESORT_BLOCKSIZE;
  if (i+MERGESORT_BLOCKSIZE <= m && j+MERGESORT_BLOCKSIZE <= r){
    while(i+MERGESORT_BLOCKSIZE <= m && j+MERGESORT_BLOCKSIZE <= r){
      if (I==MERGESORT_BLOCKSIZE){
        for(I = 0; I<MERGESORT_BLOCKSIZE; I++)
          L[I] = src[i+I];
        u = L[I = 0];
      }
      if (J==MERGESORT_BLOCKSIZE){
        for(J = 0; J<MERGESORT_BLOCKSIZE; J++)
          R[J] = src[j+J];
        v = R[J = 0];
      }
      for (;;){
        //Rprintf("FOR k=%d i=%d j=%d I=%d J=%d u=%f v=%f\n", k, i, j, I, J, u, v);
        if (LT(v, u)){
          tar[k++] = v; ++J;
          if (J==MERGESORT_BLOCKSIZE){  // check either here
            j += MERGESORT_BLOCKSIZE;
            break;
          }
          v = R[J];
        }else{
          tar[k++] = u; ++I;
          if (I==MERGESORT_BLOCKSIZE){  // or check here
            i += MERGESORT_BLOCKSIZE;
            break;
          }
          u = L[I];
        }
      }
    }
    if (I < MERGESORT_BLOCKSIZE){
      // R ist exhausted but j might still have elements
      if (j<=r){
        v = src[j];
        for (;;){
          //Rprintf("I k=%d i=%d j=%d I=%d J=%d u=%f v=%f\n", k, i, j, I, J, u, v);
          if (LT(v, u)){
            tar[k++] = v; ++j;
            if (j > r)
              break; // j is exhausted, we migth still have elements left in L
            v = src[j];
          }else{
            tar[k++] = u; ++I;
            if (I==MERGESORT_BLOCKSIZE){
              i += MERGESORT_BLOCKSIZE;
              if (i>m){
                goto finj;
              }
              u = src[i];
              goto finb; // now L (and R) are exhausted but i might still have elements
            }
            u = L[I];
          }
        }
      }
      while(I < MERGESORT_BLOCKSIZE)
        tar[k++] = L[I++];
      i += MERGESORT_BLOCKSIZE;
      goto fini;
    }
    if (J < MERGESORT_BLOCKSIZE){
      // L ist exhausted but j might still have elements
      if (i<=m){
        u = src[i];
        for (;;){
          //Rprintf("J k=%d i=%d j=%d I=%d J=%d u=%f v=%f\n", k, i, j, I, J, u, v);
          if (LT(v, u)){
            tar[k++] = v; ++J;
            if (J==MERGESORT_BLOCKSIZE){  // or check here
              j += MERGESORT_BLOCKSIZE;
              if (j>r){
                goto fini;
              }
              v = src[j];
              goto finb; // now L (and R) are exhausted but i might still have elements
            }
            v = R[J];
          }else{
            tar[k++] = u; ++i;
            if (i > m)
              break; // i is exhausted, we migth still have elements left in R
            u = src[i];
          }
        }
      }
      while(J < MERGESORT_BLOCKSIZE)
        tar[k++] = R[J++];
      j += MERGESORT_BLOCKSIZE;
      goto finj;
    }
  }else{
    u = src[i];
    v = src[j];
  }
  // end of block processing
  finb:
    for (;;){
      //Rprintf("for k=%d i=%d j=%d u=%f v=%f\n", k, i, j, u, v);
      if (LT(v, u)){
        tar[k++] = v; ++j;
        if (j > r)  // check either here
          break;
        v = src[j];
      }else{
        tar[k++] = u; ++i;
        if (i > m)  // or check here
          break;
        u = src[i];
      }
    }
  fini:
    while(i <= m){
      tar[k++] = src[i++];
    }
  finj:
    while(j <= r){
      tar[k++] = src[j++];
    }
  //for (i=l; i<=r; i++)
    //Rprintf("out i=%d t=%f s=%f\n", i, tar[i], src[i]);
}
*/


/* Following Knuth we need only one termination check per iteration
static void Knuthsort_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT i=l,j=m+1,k=l;
  ValueT u, v;
  // begin of block processing
  ValueT L[MERGESORT_BLOCKSIZE], R[MERGESORT_BLOCKSIZE];
  IndexT I=MERGESORT_BLOCKSIZE, J=MERGESORT_BLOCKSIZE;
  while(i+MERGESORT_BLOCKSIZE <= m && j+MERGESORT_BLOCKSIZE <= r){
    if (I==MERGESORT_BLOCKSIZE){
      I = 0;
      while(I<MERGESORT_BLOCKSIZE){
        L[I++] = src[i++];
      }
      I = 0;
    }
    if (J==MERGESORT_BLOCKSIZE){
      J = 0;
      while(J<MERGESORT_BLOCKSIZE){
        R[J++] = src[j++];
      }
      J = 0;
    }
    for (;;){
      if (LT(R[J], L[I])){
        tar[k++] = R[J++];
        if (J==MERGESORT_BLOCKSIZE)  // check either here
          break;
      }else{
        tar[k++] = L[I++];
        if (I==MERGESORT_BLOCKSIZE)  // or check here
          break;
      }
    }
  }
  if (I < MERGESORT_BLOCKSIZE){
    if (j<=r){
      for (;;){
        if (LT(src[j], L[I])){
          tar[k++] = src[j++];
          if (j > r)
            break;
        }else{
          tar[k++] = L[I++];
          if (I==MERGESORT_BLOCKSIZE)  // or check here
            goto finb;
        }
      }
    }
    while(I < MERGESORT_BLOCKSIZE)
      tar[k++] = L[I++];
  }
  if (J < MERGESORT_BLOCKSIZE){
    if (i<=m){
      for (;;){
        if (LT(R[J], src[i])){
          tar[k++] = R[J++];
          if (J==MERGESORT_BLOCKSIZE)  // or check here
            goto finb;
        }else{
          tar[k++] = src[i++];
          if (i > m)
            break;
        }
      }
    }
    while(J < MERGESORT_BLOCKSIZE)
      tar[k++] = R[J++];
  }
  // end of block processing
  finb:
  if (i<=m && j<=r){
    u=src[i], v=src[j];
    for (;;){
      if (LT(v, u)){
        tar[k++] = v;
        ++j;
        if (j > r)  // check either here
          break;
        v = src[j];
      }else{
        tar[k++] = u;
        ++i;
        if (i > m)  // or check here
          break;
        u = src[i];
      }
    }
  }
  fini:
  while(i <= m){
    tar[k++] = src[i++];
  }
  finj:
  while(j <= r){
    tar[k++] = src[j++];
  }
}
*/

// Following Knuth we need only one termination check per iteration
static void Knuthsort_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT i=l,j=m+1,k=l;
  ValueT u=src[i], v=src[j];
  for (;;){
    if (LT(v, u)){
      tar[k++] = v; j++;
      if (j > r)  // either one check here
        break;
      v = src[j];
    }else{
      tar[k++] = u; i++;
      if (i > m)  // or one check here
        break;
      u = src[i];
    }
  }
  while(i <= m)
    tar[k++] = src[i++];
  while(j <= r)
    tar[k++] = src[j++];
}


/*
// KnuthsortB is not possible and SimplsortB is not faster
static void Knuthsort_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT i=l,j=m+1,k=l;
  ValueT u=src[i], v=src[j];
  while(i<m && j<r){
    LT(v, u) ? (tar[k++] = v, v = src[++j]) : (tar[k++] = u, u = src[++i]);
  }
  while(i<=m && j<=r){
    tar[k++] = LT(src[j], src[i]) ? src[j++] : src[i++];
  }
  while(i <= m)
    tar[k++] = src[i++];
  while(j <= r)
    tar[k++] = src[j++];
}
 */



/* Following Knuth we need only one termination check per iteration
static void Knuthsort_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT i=l,j=m+1,k=l;
  for (;;){
    if (LT(src[j], src[i])){
      tar[k++] = src[j++];
      if (j > r)  // check either here
        break;
    }else{
      tar[k++] = src[i++];
      if (i > m)  // or check here
        break;
    }
  }
  while(i <= m)
    tar[k++] = src[i++];
  while(j <= r)
    tar[k++] = src[j++];
}
*/


static void Knuthsort_recurse(ValueT *a, ValueT *b, IndexT l, IndexT r){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(a, l, r);
  }else
#else
  if (l<r)
#endif
  {
    m = l + (r-l)/2;
    Knuthsort_recurse(b, a, l  , m);
    Knuthsort_recurse(b, a, m+1, r);
    Knuthsort_merge(a, b, l, m, r);
  }
}


void Knuthsort_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  Knuthsort_recurse(x, aux, 0, n-1);
  FREE(aux);
}

void Knuthsort_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see bMsort
  }
  Knuthsort_recurse(aux, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
