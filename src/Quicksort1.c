/*
# greeNsort Quicksort1 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


static void Quicksort1(ValueT *x, IndexT l, IndexT r)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
  IndexT i=l, j=r, p = l+randIndex(r-l+1);
  ValueT t, v = x[p];
  up:
    while(i<=r){
      if (LT(v, x[i]))
        goto down;
      i++;
    }
    i = r;
  down:
    while(j >= l){
      if (LT(x[j], v))
        goto change;
      j--;
    }
    j = l;
  change:
    if (i<j){
      SWAP(x[i],x[j],t);
      i++; j--;
      goto up;
    }else if(i < p){
      SWAP(x[i],x[p],t);
      i++;
    }else if(p < j){
      SWAP(x[p],x[j],t);
      j--;
    }

  // Rprintf("\n\nleft");
  //   for (IndexT k=l; k<=j; k++)
  //     Rprintf(" %d", (int)x[k]);
  // Rprintf("\nmiddle");
  // for (IndexT k=j+1; k<=i-1; k++)
  //   Rprintf(" %d", (int)x[k]);
  // Rprintf("\nright");
  // for (IndexT k=i+1; k<=r; k++)
  //   Rprintf(" %d", (int)x[k]);

  Quicksort1(x, l, j);
  Quicksort1(x, i, r);
}

void Quicksort1_insitu(ValueT *x, IndexT n)
{
  Quicksort1(x, 0, n-1);
}

void Quicksort1_exsitu(ValueT *x, IndexT n)
{
	IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Quicksort1(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
