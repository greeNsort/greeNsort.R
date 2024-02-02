/*
# greeNsort Chicksort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


#if 0==1
// this straight implementation works fine but is a bit slower than quick2
static void Chicksort_straight(ValueT *x, IndexT l, IndexT r)
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
  IndexT j, i = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
  i = l-1;
  j = r;
#ifdef DEBUG
  IndexT k;
  Rprintf("entering l=%d r=%d v=%f\n", l, r, v);
  for (k=l;k<=r;k++)
      Rprintf("ini x[%d]=%f\n", k, x[k]);
#endif
  for (;;){
    debugprint("++i=%d j=%d\n", i+1,j);
    if (i < j){
      if (GT(x[++i],v)){
        for(;;){
          debugprint("# i=%d --j=%d\n", i,j-1);
          if (i >= j)       // explicit stop of for loop
            goto ifin;
          if (LE(x[--j],v))
            break;
        }
        debugprint("ifor l=%d i=%d j=%d r=%d v=%f\n", l, i, j, r, v);
        SWAP(x[i], x[j], t);
      };
    }else{
      // i == j
      goto ifin;
    }
    debugprint("i=%d --j=%d\n", i,j-1);
    if (i < j){
      if (LT(x[--j],v)){
        for(;;){
          debugprint("# ++i=%d j=%d\n", i+1,j);
          if (i >= j)       // explicit stop of for loop
            goto jfin;
          if (GE(x[++i],v))
            break;
        }
        debugprint("jfor l=%d i=%d j=%d r=%d v=%f\n", l, i, j, r, v);
        SWAP(x[i], x[j], t);
      };
    }else{
      // i == j
      goto ifin;
    }
  }
  jfin:
    // x[j]<j : undo this move
    i = j+1;

  ifin:
    // we always need to exchange, because we MUST return the piviot
    // and no other value is guaranteed to be the pivot
    debugprint("fin l=%d i=%d j=%d r=%d v=%f\n", l, i, j, r, v);
    SWAP(x[i], x[r], t);
#ifdef DEBUG
    Rprintf("returning l=%d i=%d r=%d v=%f\n", l, i, r, v);
    for (k=l;k<=r;k++)
      Rprintf("fin x[%d]=%f\n", k, x[k]);
#endif

  Chicksort_straight(x, l, i-1);
  Chicksort_straight(x, i+1, r);
}
#endif



static void Chicksort(ValueT *x, IndexT l, IndexT r)
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
  IndexT j, i = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
  i = l-1;
  j = r;
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
    SWAP(x[i], x[r], t);

  Chicksort(x, l, i-1);
  Chicksort(x, i+1, r);
}

void Chicksort_insitu(ValueT *x, IndexT n)
{
  Chicksort(x, 0, n-1);
  // Chicksort_straight(x, 0, n-1);
}

void Chicksort_exsitu(
    ValueT *x
  , IndexT n
)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Chicksort(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}

