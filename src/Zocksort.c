/*
# greeNsort Zocksort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


// pivot partioning placing all pivot ties low
static void Zocksort_TieLeft(
    ValueT *x
  , IndexT l, IndexT r
){
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
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
  SWAP(x[j], x[l], t); // the index j from the first loop is guaranteed to be in a legal position
  Zocksort_TieLeft(x, l, j-1);
  Zocksort_TieLeft(x, j+1, r);
  //}
}

// version with two sentinels: not faster
// static void Zocksort_TieLeft(
//     ValueT *x
//   , IndexT l, IndexT r
// ){
// #if INSERTIONSORT_LIMIT > 0
//   if (r - l < INSERTIONSORT_LIMIT){
// #ifdef SCANNED_RANGE
//     scanned_range += r-l+1;
// #endif
//     Insertionsort_l2r(x, l, r);
//     return;
//   }
// #else
//   if (l >= r)
//     return;
// #endif
// #ifdef SCANNED_RANGE
//   scanned_range += r-l+1;
// #endif
//   IndexT i, j = l+randIndex(r-l+1), distinct=FALSE;
//   ValueT t, v;
//   SWAP(x[j], x[l], v);  // first argument pivot now in v and x[l]
//   i = l;
//   while (EQ(x[++i], v)) if (i >= r) return;  // explicit stop of for loop
//
//   // first main traversal
//   i--; j = r+1;
//   while(GT(x[--j], v));                      // sentinel stop guaranteed by pivot at the left
//   while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
//   if (i < j){
//     SWAP(x[i], x[j], t);
//     for (;;){
//       while(GT(x[--j], v));   // sentinel stop guaranteed by pivot at the left
//       while (LE(x[++i], v));  // sentinel stop guaranteed by pivot at the left
//       if (i >= j) break;
//       SWAP(x[i], x[j], t);
//     }
//   }
//   SWAP(x[l], x[j], t); // the index j from the first loop is guaranteed to be in a legal position
//   Zocksort_TieLeft(x, l, j-1);
//   Zocksort_TieLeft(x, j+1, r);
// }


void Zocksort_insitu(
    ValueT *x
  , IndexT n
)
{
  Zocksort_TieLeft(x, 0, n-1);
}


void Zocksort_exsitu(
  ValueT *x
, IndexT n
)
{
	IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Zocksort_TieLeft(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}

