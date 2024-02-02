/*
# greeNsort Chicksort (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


static void ChicksortP(ValueT *x, IndexT n)
{
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
#endif
    if (n < 2)
      return;
#if INSERTIONSORT_LIMIT > 0
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
#endif
  IndexT j, i = randIndex(n);
  ValueT t, v;
  j = n-1;
  SWAP(x[i], x[j], v); // first argument pivot now in v and x[j]
  i = -1;
  while (i+2 < j){
    // distance 2 guaranteed by while loop
    if (GT(x[++i],v)){
      for(;;){
        if (i >= j)       // explicit stop of for loop
          goto ifin;
        if (LE(x[--j],v))
          break;
      }
      SWAP(x[i], x[j], t);
      if (i+1>=j) // guarantee distance 2
        break;
    };
    if (LT(x[--j],v)){
      for(;;){
        if (i >= j)       // explicit stop of for loop
          goto jfin;
        if (GE(x[++i],v))
          break;
      }
      SWAP(x[i], x[j], t);
    };
  }
  // i+2 >= j: finalize pointer movement until i >= j
  while (LT(x[++i],v)); // sentinel stop of for loop
  goto ifin;

  jfin:
    // x[j]<j : undo this move
    i = j+1;

  ifin:
    // we always need to exchange, because we MUST return the pivot
    // and no other value is guaranteed to be the pivot
    // debugprint("fin l=%d i=%d j=%d r=%d v=%f\n", l, i, j, r, v);
    SWAP(x[i], x[n-1], t);

  ChicksortP(x, i);
  ChicksortP(x+i+1, n-i-1);
}

void ChicksortP_insitu(ValueT *x, IndexT n)
{
  ChicksortP(x, n);
}

void ChicksortP_exsitu(
    ValueT *x
  , IndexT n
)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  ChicksortP(aux, n);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
