/*
# greeNsort Omitsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

#define OMIT_CORRELATED_PRESORTING
//#define SKIP_CORRELATED_PRESORTING

static ValueT * Omitsort_merge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT j=m+1;
  ValueT v=src[j];
#ifdef OMIT_CORRELATED_PRESORTING
  if (LE(src[m], v))
    return src;
#endif
  IndexT k=l;
#ifdef SKIP_CORRELATED_PRESORTING
  IndexT i=l+(m-l)/2;
  while (i<m && GE(v,src[i])){
    while (l<=i)
      tar[k++] = src[l++];
    i=l+(m-l)/2;
  }
#endif
  ValueT u=src[l];
  for (;;){
    if (LT(v, u)){
      tar[k++] = v;
      ++j;
      if (j > r)  // check either here
        break;
      v = src[j];
    }else{
      tar[k++] = u;
      ++l;
      if (l > m)  // or check here
        break;
      u = src[l];
    }
  }
  while(l <= m)
    tar[k++] = src[l++];
  while(j <= r)
    tar[k++] = src[j++];
  return tar;
}

/*
static ValueT * Omitsort_halfmerge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT i=l,j=m+1,k=l;
  ValueT u=src[i], v=tar[j];
  if (LT(v, src[m])){  // if we are not presorted we compare and merge
    for (;;){
      if (LT(v, u)){
        tar[k++] = v;
        ++j;
        if (j > r)  // check either here
          break;
        v = tar[j];
      }else{
        tar[k++] = u;
        ++i;
        if (i > m)  // or check here
          break;
        u = src[i];
      }
    }
  }
  // move the rest of the left half to tar
  while(i <= m)
    tar[k++] = src[i++];
  // rest of the right half is already there
  return tar;
}
*/

// here the right half is already stored in tar
static ValueT * Omitsort_halfmerge(ValueT *tar, ValueT *src, IndexT l, IndexT m, IndexT r){
  IndexT j=m+1;
  ValueT v=tar[j];
  IndexT k=l;
  IndexT i=l+(m-l)/2;
  while (i<m && GE(v,src[i])){
    while (l<=i)
      tar[k++] = src[l++];
    i=l+(m-l)/2;
  }
  ValueT u=src[l];
  if (LT(v, src[m])){  // if we are not presorted we compare and merge
    for (;;){
      if (LT(v, u)){
        tar[k++] = v;
        ++j;
        if (j > r)  // check either here
          break;
        v = tar[j];
      }else{
        tar[k++] = u;
        ++l;
        if (l > m)  // or check here
          break;
        u = src[l];
      }
    }
  }
  // move the rest of the left half to tar
  while(l <= m)
    tar[k++] = src[l++];
  // rest of the right half is already there
  return tar;
}

static ValueT * Omitsort_recurse(ValueT *ori, ValueT *tar, ValueT *src, IndexT l, IndexT r){
#if INSERTIONSORT_LIMIT > 0
  if ((r-l) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(ori, l, r);
    return ori;
  }
#else
  if (r <= l)
    return(ori);
  if ((r-l) == 1){
    ValueT t;
    if (LT(ori[r], ori[l])){
      t = ori[l];
      ori[l] = ori[r];
      ori[r] = t;
    }
    return ori;
  }
#endif
  ValueT *L, *R;
  IndexT m = l + (r-l)/2;
  L = Omitsort_recurse(ori, src, tar, l  , m);
  R = Omitsort_recurse(ori, src, tar, m+1, r);
  if (L==R){
    if (L==src){
      return Omitsort_merge(tar, src, l, m, r);
    }else{
      return Omitsort_merge(src, tar, l, m, r);
    }
  }else{
      return Omitsort_halfmerge(R, L, l, m, r);
  }
}


void Omitsort_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *ret;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  ret = Omitsort_recurse(x, x, aux, 0, n-1);
  if (ret==aux){
    for (i=0; i<n; i++)
      x[i] = aux[i];
  }
  FREE(aux);
}

void Omitsort_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *ret;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i=0; i<n; i++)
    aux[i] = x[i];
  ret = Omitsort_recurse(aux, aux, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = ret[i];
  FREE(aux);
}
