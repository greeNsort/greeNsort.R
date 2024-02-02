/*
# greeNsort Zacksort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


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

void Zacksort_insitu(
  ValueT *x
, IndexT n
)
{
  Zacksort_TieLeft(x, 0, n-1);
}

void Zacksort_exsitu(
  ValueT *x
, IndexT n
)
{
	IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Zacksort_TieLeft(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}



#if 0==1

  // median of three

	IndexT a,b,c;
	a = randIndex(n);
	b = randIndex(n);
	c = randIndex(n);
  // median pivot
  i = LT(x[a], x[b]) ?
      (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)
    : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);

  // median pivot
  j = LT(x[a], x[b]) ?
      (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)
    : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);

#endif

