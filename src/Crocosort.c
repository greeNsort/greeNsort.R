/*
# greeNsort Crocosort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

/* merges the two sorted sequences aux[0,m-1] aux[m,n-1] to x[0,n-1]
static void Crocosort_finalmerge_exsitu(ValueT *x, ValueT *aux, IndexT m, IndexT n){
  IndexT i,j,k;
  k = n-1;
  i = m-1;
  j = n+m-1; // = 2*m+(n-m)-1
  m += m;    // use m as stopper for j
  //if (i>=0 && j >=m)
  ValueT u=aux[i], v=aux[j];
  for (;;){
    if (GT(u, v)){
      x[k--] =  u; i--;
      if (i < 0)
        break;
      u = aux[i];
    }else{
      x[k--] = v; j--;
      if (j < m)
        break;
      v = aux[j];
    }
  }
  while(j >= m)
    x[k--] = aux[j--];
  while(i >= 0)
    x[k--] = aux[i--];
}
*/


// merges the two sorted sequences x[0,i-1] aux[0,j-1] to x[0,i+j-1]
static void Crocosort_finalmerge_insitu(ValueT *x, ValueT *aux, IndexT m, IndexT n){
  IndexT i,j,k;
  i = m-1;
  k = n-1;
  j = k-m; // = n-m-1
  //if (i>=0 && j >=0)
  ValueT u=x[i], v=aux[j];
  for (;;){
    if (GT(u, v)){
      x[k--] = u; i--;
      if (i < 0)
        break;
      u = x[i];
    }else{
      x[k--] = v; j--;
      if (j < 0)
        break;
      v = aux[j];
    }
  }
  while(j >= 0)
    x[k--] = aux[j--];
  // since inplace not needed
  //while(i >= 0)
  //  x[k--] = x[i--];
}


// receives the two sorted sequences aux[l+l,l+m] and aux[m+m+2,m+r+1]
// transfers them to aux[r+l+1,r+m+1] and aux[r+m+2,r+r+1]
// and merges them to aux[l+l,l+r]
static void Crocosort_relocmerge(ValueT *aux, IndexT l, IndexT m, IndexT r){
  IndexT I,J,i,j,k;
  // simple tuning if perfectly preordered
  if (LE(aux[l+m], aux[m+m+2])){
    k = l+m+1;
    j=m+m+2; // index
    J=m+r+1; // stopper
    while(j<=J){
      aux[k++] = aux[j++];
    }
    return;
  }

  // transfer
  k = r+r+1;
  // upper sequence
  j=m+r+1; // index
  J=m+m+2; // stopper
  while(j>=J)
    aux[k--] = aux[j--];

  // merge
  J = k + 1; // stopper
  j = r+r+1; // index
  k = r+l;
  I = l+l; // stopper
  i = l+m; // index
  //if (i >= I && j >= J)
  ValueT u=aux[i], v=aux[j];
  for (;;){
    if (GT(u, v)){
      aux[k--] =  u; i--;
      if (i < I)
        break;
      u = aux[i];
    }else{
      aux[k--] = v; j--;
      if (j < J)
        break;
      v = aux[j];
    }
  }
  while(j >= J){
    aux[k--] = aux[j--];
  }
  // since inplace not needed
  // while(i >= I)
    // aux[k--] = aux[i--];
}

// must not forget to handle single elements !!
static void Crocosort_init(ValueT *x, ValueT *aux, IndexT l, IndexT r){
    IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if ((r-l) < INSERTIONSORT_LIMIT){
      IndexT M;
      // move from original source x[l,r] to b-chunk data aux[l+l,l+r]
      m=l;
      M=l+l;
      while(m<=r)
        aux[M++] = x[m++];
      // sort data of b-chunk
      Insertionsort_l2r(aux, l+l, l+r);
    }else
#else
    ValueT t;
    if ((r-l)<2){
      if (l==r){
        aux[l+l] = x[l];
      }else{
        if (LT(x[l+1],x[l])){
          t = x[l];      // to avoid inplace overwrite
          aux[l+l]=x[l+1];
          aux[l+l+1]=t;  // to avoid inplace overwrite
        }else{
          aux[l+l]=x[l];
          aux[l+l+1]=x[l+1];
        }
      }
    }else
#endif
  {
    m = l + (r-l)/2;
    Crocosort_init(x, aux, m+1, r);
    Crocosort_init(x, aux, l  , m);
  }
}

static void Crocosort_sort(ValueT *x, ValueT *aux, IndexT l, IndexT r){
#if INSERTIONSORT_LIMIT > 0
    if ((r-l) >= INSERTIONSORT_LIMIT)
#else
    if ((r-l)>1)
#endif
  {
    IndexT m = l + (r-l)/2;
    Crocosort_sort(x, aux, m+1, r);
    Crocosort_sort(x, aux, l  , m);
    Crocosort_relocmerge(aux, l, m, r);
  }
}

void Crocosort_insitu(ValueT *x, IndexT n)
{
#if INSERTIONSORT_LIMIT > 0
    if (n <= INSERTIONSORT_LIMIT){
      Insertionsort_l2r(x, 0, n-1);
      return ;
    }
#else
    if (n < 2){
      return ;
    }
#endif
  IndexT m = n/2;
  ValueT *aux = (ValueT *) MALLOC((n-m)*2, ValueT);
  Crocosort_init(x+m, aux, 0, n-m-1);
  Crocosort_init(x, x, 0, m-1);
  Crocosort_sort(x+m, aux, 0, n-m-1);
  Crocosort_sort(x, x, 0, m-1);
  Crocosort_finalmerge_insitu(x, aux, m, n);
  FREE(aux);
}

void Crocosort_exsitu(ValueT *x, IndexT n)
{
  if (n < 2){
    return ;
  }
  //IndexT m = n/2;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  Crocosort_init(x, aux, 0, n-1);
  Crocosort_sort(x, aux, 0, n-1);
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];

  // Mild tuning: let the lst merge copy back
  // Crocosort_recurse(x+m, aux+2*m, 0, n-m-1);
  // Crocosort_recurse(x, aux, 0, m-1);
  // Crocosort_recurse(x+m, aux+2*m, 0, n-m-1);
  // Crocosort_recurse(x, aux, 0, m-1);
  // Crocosort_finalmerge_exsitu(x, aux, m, n);
  FREE(aux);
}
