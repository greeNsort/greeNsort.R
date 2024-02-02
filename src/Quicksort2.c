/*
# greeNsort Quicksort2 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


static void Quicksort2(ValueT *x, IndexT l, IndexT r)
{
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
  //Rprintf("l=%d  i=%d  r=%d\n", l, i, r);
  ValueT t, v;
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
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
  Quicksort2(x, l, i-1);
  Quicksort2(x, i+1, r);
}

// version with two sentinels
// static void Quicksort1(ValueT *x, IndexT l, IndexT r)
// {
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
//   IndexT j, i = l+randIndex(r-l+1);
//   //Rprintf("l=%d  i=%d  r=%d\n", l, i, r);
//   ValueT t, v;
//   SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
//   i = l-1; j = r;
//
//   // first loop traversal is different: no pivot on the left
//   while (LT(x[++i], v)); // sentinel stop of for loop
//   while (LT(v, x[--j]))
//     if (j <= i)       // explicit stop of for loop
//       break;
//
//   // main loop with two sentinels
//   if (i < j){
//     SWAP(x[i], x[j], t);
//     for (;;){
//       while (LT(x[++i], v)); // sentinel stop of for loop
//       while (LT(v, x[--j])); // sentinel stop of for loop
//       if (j <= i)
//         break;
//       SWAP(x[i], x[j], t);
//     }
//   }
//   SWAP(x[i], x[r], t);
//   Quicksort1(x, l, i-1);
//   Quicksort1(x, i+1, r);
// }


void Quicksort2_insitu(ValueT *x, IndexT n)
{
  Quicksort2(x, 0, n-1);
}

void Quicksort2_exsitu(ValueT *x, IndexT n)
{
	IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Quicksort2(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
