/*
# greeNsort Quicksort3 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

/* Jens version: a bit simpler and faster than Bentley&McIlroy (except for *very* tied data) */
static void Quicksort3(ValueT *x, IndexT l, IndexT r)
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
  IndexT k, p, q, j, i = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
  i = p = l-1; j = q = r;
  for (;;){
    while (LT(x[++i],v)); // sentinel stop of for loop
    while (LT(v,x[--j])) if (j <= i) break; // explicit stop of for loop
    if (j <= i) break;
    SWAP(x[i], x[j], t);  // for both ties this SWAP is superfluous - Datacentric Thinking: faster for non-tied, do not tune easy tied case
    if (EQ(x[i],v)) {++p; SWAP(x[p], x[i],t);}
    if (EQ(v,x[j])) {--q; SWAP(x[j], x[q], t);}
  }
  SWAP(x[i], x[r], t);
  j = i-1; i = i+1;
  for (k = l; k < p; k++, j--) SWAP(x[k], x[j], t);
  for (k = r-1; k > q; k--, i++) SWAP(x[i], x[k], t);
  Quicksort3(x, l, j);
  Quicksort3(x, i, r);
}



/* Bentley & McIlroy
#define vecswap(a, b, n) if (n > 0) swapfunc(a, b, n)
static void swapfunc(ValueT *a, ValueT *b, IndexT n)
{
    ValueT t;
    for( ; n > 0; a += 1, b += 1, n -= 1)
      SWAP(*a, *b, t);
}

void Quicksort4(ValueT *x, IndexT l, IndexT r)
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
  IndexT pa,pb,pc,pd,s = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[s], x[l], v); // first argument pivot now in v and x[l]
  pa = pb = l;
  pc = pd = r;
  for (;;) {
    while (pb <= pc && LE(x[pb],v)) {
      if (EQ(x[pb],v)) { SWAP(x[pa], x[pb], t); pa++; }
      pb++;
    }
    while (pc >= pb && GE(x[pc], v)) {
      if (EQ(x[pc],v)) { SWAP(x[pc], x[pd], t); pd--; }
      pc--;
    }
    if (pb > pc) break;
    SWAP(x[pb], x[pc], t);  // this necer SWAPs ties, but we loose speed in the above loops
    pb++;
    pc--;
  }
  s = MIN(pa-l, pb-pa); vecswap(x+l, x+(pb-s), s);
  s = MIN(pd-pc, r-pd); vecswap(x+pb, x+((r-s)+1), s);
  // if ((s = pb-pa) > 1) Quicksort4(x, l, l+s);
  // if ((s = pd-pc) > 1) Quicksort4(x, (r-s)+1, r);
  Quicksort4(x, l, l+(pb-pa));
  Quicksort4(x, (r-(pd-pc))+1, r);
}
*/

void Quicksort3_insitu(ValueT *x, IndexT n)
{
  Quicksort3(x, 0, n-1);
}

void Quicksort3_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Quicksort3(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
