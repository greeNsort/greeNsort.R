/*
# greeNsort partial Zacksort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static RangeIndexT Zackpartleft_TieLeft(
    ValueT *x
  , IndexT l, IndexT r, IndexT k
);


static void Zacksort_TieLeft(
    ValueT *x
  , IndexT l, IndexT r
);

// pivot partioning placing all pivot ties high
static void Zacksort_TieRight(
    ValueT *x
  , IndexT l, IndexT r
){
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
#ifdef SCANNED_RANGE
  scanned_range += r-l+1;
#endif
  IndexT j, i = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]

  // in the DIET pre-loop we do (almost) the same as for pivot-ties in the main loop

  // disadvantage: double negation
  // j = r - 1;  // we restore the usual initialization i=l-1 below
  // for (;j>=l;j--) // explicit stop of for loop
  //   if (!EQ(x[j], v))
  //     break;

  // disadvantage: repeated last EQ comparision
  // j = r;
  // while (EQ(x[--j], v)) if (j <= l) break;  // explicit stop of for loop
  // if (EQ(x[j], v)) j--;

  // elegant
  j = r;
  while (EQ(x[--j], v)) if (j <= l) return;  // explicit stop of for loop

  //Rprintf("TieRight l=%d i=%d j=%d r=%d p=%f\n", l, i, j, r, v);
  //if (l <= j){  // distinct values, not all value tie with pivot
  // restore  the usual initialization i=l-1; j=r, i.e. we test the same value that was EQ again for GE
  i = l - 1;  j++;
  for (;;){
    while(LT(x[++i], v));                     // sentinel stop guaranteed by pivot at the right
    while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
    if (j <= i)break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[i], x[r], t); // the index i from the first loop is guaranteed to be in a legal position
  Zacksort_TieLeft(x, l, i-1);
  Zacksort_TieLeft(x, i+1, r);
  //}
  // else all pivot ties
}

// pivot partioning placing all pivot ties low
static void Zacksort_TieLeft(
    ValueT *x
  , IndexT l, IndexT r
){
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
#ifdef SCANNED_RANGE
  scanned_range += r-l+1;
#endif
  IndexT i, j = l+randIndex(r-l+1); //, distinct=FALSE;
  ValueT t, v;
  SWAP(x[j], x[l], v);  // first argument pivot now in v and x[l]

  // in the DIET pre-loop we do (almost) the same as for pivot-ties in the main loop

  // disadvantage: double negation
  // i = l+1; // we restore the usual initialization i=l, j=r+1 below
  // for (;i<=r;i++) // explicit stop of for loop
  //   if (!EQ(x[i], v))
  //     break;

  // disadvantage: repeated last EQ comparision
  // i = l;
  // while (EQ(x[++i], v)) if (i >= r) break;  // explicit stop of for loop
  // if (EQ(x[i], v)) i++;

  // elegant
  i = l;
  while (EQ(x[++i], v)) if (i >= r) return;  // explicit stop of for loop

  //Rprintf("TieLeft l=%d i=%d j=%d r=%d p=%f\n", l, i, j, r, v);
  //if (i <= r){
  // restore  the usual initialization i=l; j=r+1, i.e. we test the same value that was EQ again for GE
  i--; j = r+1;
  for (;;){
    while(GT(x[--j], v));                      // sentinel stop guaranteed by pivot at the left
    while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
    if (i >= j) break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[l], x[j], t); // the index j from the first loop is guaranteed to be in a legal position
  Zacksort_TieRight(x, l, j-1);
  Zacksort_TieRight(x, j+1, r);
  //}
}


// pivot partioning placing all pivot ties high
static RangeIndexT Zackpartleft_TieRight(
    ValueT *x
  , IndexT l, IndexT r, IndexT k
){
  RangeIndexT ret;
  IndexT i,j;
  assert(l<=k);
  assert(k<=r);
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    for(i=k-1;i>=l;i--){
      if (NE(x[i],x[k]))
        break;
    }
    for(j=k+1;j<=r;j++){
      if (NE(x[j],x[k]))
        break;
    }
    ret.min = i+1;
    ret.max = j-1;
    return ret;
  }
#else
  if (l >= r){
    ret.min = ret.max = k;
    return ret;
  }
#endif
  ValueT t, v;
  i = l+randIndex(r-l+1);
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]

  // elegant
  j = r;
  while (EQ(x[--j], v)) if (j <= l) goto final;  // explicit stop of for loop

  // restore  the usual initialization i=l-1; j=r, i.e. we test the same value that was EQ again for GE
  i = l - 1;  j++;
  for (;;){
    while(LT(x[++i], v));                      // sentinel stop guaranteed by pivot at the right
    while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
    if (j <= i)break;
    SWAP(x[i], x[j], t);
  }

  if(k<i){
    return Zackpartleft_TieLeft(x, l, i-1, k);
  }else{
    //Zackpartleft_TieLeft(x, l, i-1, k);
    Zacksort_TieLeft(x, l, i-1);
    return Zackpartleft_TieLeft(x, i, r, k);
  }

  final:
    ret.min = l;
  ret.max = r;
  return ret;

}

// pivot partioning placing all pivot ties low
static RangeIndexT Zackpartleft_TieLeft(
    ValueT *x
  , IndexT l, IndexT r, IndexT k
){
  RangeIndexT ret;
  IndexT i,j;
  assert(l<=k);
  assert(k<=r);
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    for(i=k-1;i>=l;i--){
      if (NE(x[i],x[k]))
        break;
    }
    for(j=k+1;j<=r;j++){
      if (NE(x[j],x[k]))
        break;
    }
    ret.min = i+1;
    ret.max = j-1;
    return ret;
  }
#else
  if (l >= r){
    ret.min = ret.max = k;
    return ret;
  }
#endif
  j = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[j], x[l], v);  // first argument pivot now in v and x[l]

  // elegant
  i = l;
  while (EQ(x[++i], v)) if (i >= r) goto final;  // explicit stop of for loop

  // restore  the usual initialization i=l; j=r+1, i.e. we test the same value that was EQ again for GE
  i--; j = r+1;
  for (;;){
    while(GT(x[--j], v));                      // sentinel stop guaranteed by pivot at the left
    while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
    if (i >= j)break;
    SWAP(x[i], x[j], t);
  }

  if(k<=j){
    return Zackpartleft_TieRight(x, l, j, k);
  }else{
    //Zackpartleft_TieRight(x, l, j, k);
    Zacksort_TieRight(x, l, j);
    return Zackpartleft_TieRight(x, j+1, r, k);
  }

  final:
    ret.min = l;
  ret.max = r;
  return ret;
}

RangeIndexT Zackpartleft_insitu(ValueT *x, IndexT n, IndexT k){
  return Zackpartleft_TieLeft(x, 0, n-1, k);
}

RangeIndexT Zackpartleft_exsitu(ValueT *x, IndexT n, IndexT k){
  IndexT i;
  RangeIndexT ret;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0; i<n; i++)
    aux[i] = x[i];
  ret = Zackpartleft_TieLeft(aux, 0, n-1, k);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
  return ret;
}
