/*
# greeNsort Gapped Knuthsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "Insertionsort_l2r.h"
#include "GInsertionsort_l2r.h"
#include "OddEven_init_left.h"
#include "OddEven_back_left.h"
#include "OddEven_init_right.h"
#include "OddEven_back_right.h"

void gapprint(ValueT *x, IndexT from, IndexT to){
  Rprintf("FROM=%d TO=%d\n", from, to); R_FlushConsole();
  for (IndexT i=from;i<to;INC2(i)){
    Rprintf("  i=%d x=%f\n", i, x[i]); R_FlushConsole();
  }
}
void gapprint2(ValueT *left, ValueT *right, IndexT nl, IndexT nr){
  Rprintf("nl=%d nr=%d\n", nl, nr); R_FlushConsole();
  for (IndexT i=0;i<nl;INC2(i)){
    Rprintf("  i=%d lx=%f\n", i, left[i]); R_FlushConsole();
  }
  for (IndexT i=0;i<nr;INC2(i)){
    Rprintf("  i=%d rx=%f\n", i, right[i]); R_FlushConsole();
  }
}

// merges from odd to even or from even to odd, depending on input parameter odd
// may skip merging if sorted already
// returns new parameter odd
static IndexT GKnuthsort_merge(ValueT *x, IndexT m, IndexT n, IndexT odd)
{
  m *= 2;
  n *= 2;
  IndexT i=odd, j=m+odd, k=1-odd;
  ValueT u=x[i], v=x[j];
  // simple tuning for perfectly presorted
  // if (LE(x[j-2],v))
  //   return odd;
  for (;;){
    if (LT(v, u)){
      x[k] = v; INC2(k); INC2(j);
      if (j >= n)  // check either here
        break;
      v = x[j];
    }else{
      x[k] = u; INC2(k); INC2(i);
      if (i >= m)  // or check here
        break;
      u = x[i];
    }
  }
  while(i < m){
    x[k] = x[i]; INC2(k); INC2(i);
  }
  while(j < n){
    x[k] = x[j]; INC2(k); INC2(j);
  }
  return(1-odd);
}

// dito but left half of data is in left and right half of data is in right
// where nl <= nr
static IndexT GKnuthsort_merge_halfes(ValueT *left, ValueT *right, IndexT nl, IndexT nr, IndexT odd)
{
  nl *= 2;
  nr *= 2;
  IndexT i=odd, j=odd, k=1-odd;
  ValueT u=left[i], v=right[j];
  // simple tuning for perfectly presorted
  // if (LE(left[nl-2+odd],v))
  //   return odd;
  while(k<nl){
    if (LT(v, u)){
      left[k] = v; INC2(k); INC2(j);
      // j cannot exhaust before k exhausts because nl <= nr
      if (j<nr) // but without this check we read beyond
        v = right[j];
    }else{
      left[k] = u; INC2(k); INC2(i);
      // also i cannot exhaust before k exhausts because nl == nl
      if (i<nl) // but without this check we read beyond
        u = left[i];
    }
  }
  k=1-odd;
  if (i<nl && j<nr)
  for(;;){
    if (LT(v, u)){
      right[k] = v; INC2(k); INC2(j);
      if (j >= nr)  // check either here
        break;
      v = right[j];
    }else{
      right[k] = u; INC2(k); INC2(i);
      if (i >= nl)  // or check here
        break;
      u = left[i];
    }
  }
  while(i < nl){
    right[k] = left[i]; INC2(k); INC2(i);
  }
  while(j < nr){
    right[k] = right[j]; INC2(k); INC2(j);
  }
  return(1-odd);
}


static IndexT GKnuthsort_rec(ValueT *x, IndexT n, IndexT odd)
{
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    GInsertionsort_l2r(x, 0, n-1);
    return odd;
  }
#else
  if (n<=2){
    if (n==2){
      if (GT(x[0], x[2])){
        ValueT t;
        t = x[0];
        x[0] = x[2];
        x[2] = t;
      }
    }
    return odd;
  }
#endif
  IndexT leftodd, rightodd, m = n/2;
  leftodd = GKnuthsort_rec(x    ,   m, odd);
  rightodd = GKnuthsort_rec(x+m+m, n-m, odd);
  if (leftodd != rightodd){
    IndexT i, m2 = m+m;
    if (leftodd){
      leftodd = 0;
      for (i=0;i<m2;INC2(i))
        x[i] = x[i+1];
    }else{
      IndexT n2 = n+n;
      for (i=m2;i<n2;INC2(i))
        x[i] = x[i+1];
    }
  }
  return GKnuthsort_merge(x, m, n, leftodd);
}

void GKnuthsort_insitu(ValueT *x, IndexT n)
{
  // Special case: up to n == 5 we switch to insertion sort because naux would be larger than n
  if (n<6){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }

  // nl <= nr
  IndexT nl = n/2;
  IndexT nr = n-nl;
  ValueT *aux = (ValueT *) MALLOC(2*nr, ValueT);
  // right half to aux
  OddEven_init_right(x+nl, aux, nr);
  // left half to x
  OddEven_init_left (x   , x  , nl);

  IndexT  leftodd = GKnuthsort_rec(  x, nl, 0); // zero denotes inital even position of data
  IndexT rightodd = GKnuthsort_rec(aux, nr, 0); // zero denotes initial even position of data

  if (leftodd != rightodd){
    IndexT i;
    if (leftodd){
      IndexT n2=nl+nl;
      leftodd = 0;
      for (i=0;i<n2;INC2(i))
        x[i] = x[i+1];
    }else{
      IndexT n2=nr+nr;
      for (i=0;i<n2;INC2(i))
        aux[i] = aux[i+1];
    }
  }

  // Rprintf("leftodd=%d rightodd=%d\n", leftodd, rightodd);
  // gapprint(  x+ leftodd, 0, 2*nl);
  // gapprint(aux+rightodd, 0, 2*nr);

  leftodd = GKnuthsort_merge_halfes(x, aux, nl, nr, leftodd);

  // Rprintf("leftodd=%d\n", leftodd);
  // gapprint(  x+ leftodd, 0, 2*nl);
  // gapprint(aux+ leftodd, 0, 2*nr);

  OddEven_back_left(x   ,   x+leftodd, nl);
  OddEven_back_left(x+nl, aux+leftodd, nr);
  FREE(aux);
  return;
}

void GKnuthsort_exsitu(ValueT *x, IndexT n)
{
  if (n<3){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
  ValueT *aux = (ValueT *) MALLOC(2*n, ValueT);
  OddEven_init_right(x, aux, n);
  IndexT odd = GKnuthsort_rec(aux, n, 0); // zero denotes initial even position of data
  OddEven_back_left(x, aux+odd, n);
  FREE(aux);
  return;
}
