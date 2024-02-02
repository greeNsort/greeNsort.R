/*
# greeNsort Omitsort (pointer implementation)
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

static ValueT * OmitsortP_merge(ValueT *z, ValueT *l, ValueT *m, ValueT *r){
  // greeNsort skip ANY operation on perfect preorder
  ValueT *j=m+1;
  ValueT v=*j;
#ifdef OMIT_CORRELATED_PRESORTING
  if (LE(*m, v))
    return l;
#endif
  ValueT *k=z;
#ifdef SKIP_CORRELATED_PRESORTING
  ValueT *i=l+(m-l)/2;
  while (i<m && GE(v,*i)){
    while (l<=i)
      *(k++) = *(l++);
    i=l+(m-l)/2;
  }
#endif
  ValueT u=*l;
  for (;;){
    if (LT(v, u)){
      *(k++) = v;
      ++j;
      if (j > r)  // check either here
        break;
      v = *j;
    }else{
      *(k++) = u;
      ++l;
      if (l > m)  // or check here
        break;
      u = *l;
    }
  }
  while(l <= m)
    *(k++) = *(l++);
  while(j <= r)
    *(k++) = *(j++);
  return z;
}

// here the right half is in-place
static void OmitsortP_halfmerge(ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  ValueT *k = rl - (lr-ll+1);
  ValueT v=*rl;
  if (LT(v, *lr)){  // if we are not presorted we compare and merge
    ValueT u=*ll;
    for (;;){
      if (LT(v, u)){
        *(k++) = v;
        rl++;
        if (rl > rr)  // check either here
          break;
        v = *rl;
      }else{
        *(k++) = u;
        ++ll;
        if (ll > lr)  // or check here
          break;
        u = *ll;
      }
    }
  }
  // move the rest of the left half to right
  while(ll <= lr)
    *(k++) = *(ll++);
}

static ValueT * OmitsortP_recurse(ValueT *ori, ValueT *tar, ValueT *src, IndexT n){
  IndexT r=n-1;
#if INSERTIONSORT_LIMIT > 0
    if ((n-1) < INSERTIONSORT_LIMIT){
      Insertionsort_l2r(ori, 0, n-1);
      return ori;
    }
#else
    if (n<2)
      return ori;
    ValueT t;
    if (n == 2){
      if (LT(*(ori+r), *ori)){
        t = *ori;
        *ori = *(ori+r);
        *(ori+r) = t;
      }
      return ori;
    }
#endif
    ValueT *L, *R;
    IndexT m = n/2;
    L = OmitsortP_recurse(ori, src, tar, m);
    R = OmitsortP_recurse(ori+m, src+m, tar+m, n-m) - m;
    // now the data should be in src, unless we skipped the merge
    if (L==R){
      if (L==src){
        return OmitsortP_merge(tar, src, src+m-1, src+r);
      }else{
        return OmitsortP_merge(src, tar, tar+m-1, tar+r);
      }
    }else{
        OmitsortP_halfmerge(L, L+m-1, R+m, R+r); return R;
    }
}



void OmitsortP_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *ret;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  ret = OmitsortP_recurse(x, x, aux, n);
  if (ret==aux){
    for (i=0; i<n; i++)
      x[i] = aux[i];
  }
  FREE(aux);
}

void OmitsortP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *ret;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i=0; i<n; i++)
    aux[i] = x[i];
  ret = OmitsortP_recurse(aux, aux, aux2, n);
  for (i=0; i<n; i++)
    x[i] = ret[i];
  FREE(aux);
}
