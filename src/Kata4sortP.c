/*
# greeNsort Kata4sort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

//#define KMERGE_DEBUG 1
#undef KMERGE_DEBUG
#include "kmerge.h"


static void Kata4sort_recurse(ValueT *a, ValueT *b, IndexT l, IndexT r){
#ifdef KMERGE_DEBUG
  Rprintf("Kata4sort_recurse l=%d r=%d\n", l, r);
#endif
  if (l<=r){
    IndexT n4=r-l+1, n3, n2, n1;
#if INSERTIONSORT_LIMIT > 0
    if ((r-l) < INSERTIONSORT_LIMIT){
      Insertionsort_l2r(a, l, r);
      return ;
    }
#else
    ValueT t;
    if (n4 < 4){
      if (n4==2){
        if (LT(a[r], a[l])){
          t = a[l];
          a[l] = a[r];
          a[r] = t;
        }
      }else if (n4==3){
        Insertionsort_l2r(a, l, r);
      }
      return;
    }
#endif
    n1 = n4 / 4;
    n2 = n1+n1;
    n3 = n2+n1;
    if (n3+n1 < n4)
      n3++;
    if (n3+n1 < n4)
      n2++;
#ifdef KMERGE_DEBUG
    Rprintf("->  %d..%d  %d..%d  %d..%d  %d..%d\n", l   , l+n1-1, l+n1, l+n2-1, l+n2, l+n3-1, l+n3, r     );
#endif
    Kata4sort_recurse(b, a, l   , l+n1-1);
    Kata4sort_recurse(b, a, l+n1, l+n2-1);
    Kata4sort_recurse(b, a, l+n2, l+n3-1);
    Kata4sort_recurse(b, a, l+n3, r     );
#ifdef KMERGE_DEBUG
    Rprintf("<-  %d..%d  %d..%d  %d..%d  %d..%d\n", l   , l+n1-1, l+n1, l+n2-1, l+n2, l+n3-1, l+n3, r     );
#endif
    // who exhausts first?
    if (LE(b[l+n1-1], b[l+n2-1])){
      if (LE(b[l+n3-1], b[r])){
        if (LE(b[l+n1-1], b[l+n3-1]))
          right_merge4_exhausts1(a+l, b+l, b+l+n1-1, b+l+n1, b+l+n2-1, b+l+n2, b+l+n3-1, b+l+n3, b+l+n4-1);
        else
          right_merge4_exhausts3(a+l, b+l, b+l+n1-1, b+l+n1, b+l+n2-1, b+l+n2, b+l+n3-1, b+l+n3, b+l+n4-1);
      }else{
        if (LE(b[l+n1-1], b[r]))
          right_merge4_exhausts1(a+l, b+l, b+l+n1-1, b+l+n1, b+l+n2-1, b+l+n2, b+l+n3-1, b+l+n3, b+l+n4-1);
        else
          right_merge4_exhausts4(a+l, b+l, b+l+n1-1, b+l+n1, b+l+n2-1, b+l+n2, b+l+n3-1, b+l+n3, b+l+n4-1);
      }
    }else{
      if (LE(b[l+n3-1], b[r])){
        if (LE(b[l+n2-1], b[l+n3-1]))
          right_merge4_exhausts2(a+l, b+l, b+l+n1-1, b+l+n1, b+l+n2-1, b+l+n2, b+l+n3-1, b+l+n3, b+l+n4-1);
        else
          right_merge4_exhausts3(a+l, b+l, b+l+n1-1, b+l+n1, b+l+n2-1, b+l+n2, b+l+n3-1, b+l+n3, b+l+n4-1);
      }else{
        if (LE(b[l+n2-1], b[r]))
          right_merge4_exhausts2(a+l, b+l, b+l+n1-1, b+l+n1, b+l+n2-1, b+l+n2, b+l+n3-1, b+l+n3, b+l+n4-1);
        else
          right_merge4_exhausts4(a+l, b+l, b+l+n1-1, b+l+n1, b+l+n2-1, b+l+n2, b+l+n3-1, b+l+n3, b+l+n4-1);
      }
    }
  }
}


void Kata4sort_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  // half of initial copying can be avoided, see KatasortA
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  Kata4sort_recurse(x, aux, 0, n-1);
  FREE(aux);
}

void Kata4sort_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i = 0; i < n; i++){
    aux2[i] = aux[i] = x[i]; // half of initial copying to aux2 can be avoided, see KatasortA
  }
  Kata4sort_recurse(aux, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}

