/*
# greeNsort partial Quicksort2 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static ValueT Quickpartright2(ValueT *x, IndexT l, IndexT r, IndexT k)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return x[k];
  }
#else
  if (l >= r)
    return x[k];
#endif
  IndexT j,i = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[i], x[r], v); // now pivot is in v and x[r]
  i = l-1; j = r;
  for (;;){
    while (LT(x[++i], v)); // sentinel stop of for loop
    while (LT(v, x[--j])){
      if (j <= i)       // explicit stop of for loop
        break;
    }
    if (j <= i)
      break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[i], x[r], t);
  if (i == k){
    Quickpartright2(x, i+1, r, k);
    return x[i];
  }else if(k<i){
    Quickpartright2(x, i+1, r, k);
    return Quickpartright2(x, l, i-1, k);
  }else{
    return Quickpartright2(x, i+1, r, k);
  }
}

ValueT Quickpartright2_insitu(ValueT *x, IndexT n, IndexT k){
  return Quickpartright2(x, 0, n-1, k);
}

ValueT Quickpartright2_exsitu(ValueT *x, IndexT n, IndexT k){
  IndexT i;
  ValueT ret, *aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0; i<n; i++)
    aux[i] = x[i];
  ret = Quickpartright2(aux, 0, n-1, k);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
  return ret;
}
