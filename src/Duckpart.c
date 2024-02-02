/*
# greeNsort Duck partial sorting algorithm
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

// set to 0 for simple straight POET-loop instead of Partial_Insertionsort
#define PARTIAL_INSERTIONSORT_LIMIT 0

#if PARTIAL_INSERTIONSORT_LIMIT
static IndexT Partial_Insertionsort_l2r(
    ValueT *x
  , IndexT l
  , IndexT r
)
{
  IndexT i, j, limit=0;
  ValueT t;
  for (i=l+1;i<=r;i++){
    j=i;
    MOVE(t, x[i]);
    while (j>l && GT(x[j-1], t)){
      MOVE(x[j], x[j-1]);
      j--;
    }
    MOVE(x[j], t);
    limit += (i-j);
    if (limit > PARTIAL_INSERTIONSORT_LIMIT) return i;
  }
  return i;
}
static IndexT Partial_Insertionsort_r2l(
    ValueT *x
  , IndexT l
  , IndexT r
)
{
  IndexT i, j, limit=0;
  ValueT t;
  for (i=r-1;i>=l;i--){
    j=i;
    MOVE(t, x[i]);
    while (j<r && LT(x[j+1], t)){
      MOVE(x[j], x[j+1]);
      j++;
    }
    MOVE(x[j], t);
    limit += (j-i);
    if (limit > PARTIAL_INSERTIONSORT_LIMIT) return i;
  }
  return i;
}
#endif

// ingenious: simply POET instead of DIET before MAIN. Simplicity wins!



static void Duckpart_TieLeft(
    ValueT *x
  , IndexT l, IndexT r, IndexT *p
);

// pivot partioning placing all pivot ties high
static void Duckpart_TieRight(
    ValueT *x
  , IndexT l, IndexT r, IndexT *p
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
  ValueT t, v;
  IndexT i, j;

  // POET replaces DIET: tentatively invest more COMPs than DIET (and MAIN has to restart from scratch)
#if PARTIAL_INSERTIONSORT_LIMIT
  if (Partial_Insertionsort_r2l(x, l, r) < l){
    //if ((r-l)>48)Rprintf(" ET Ducksort_TieRight d=%d\n", r-l+1);
    return;
  }
#else
  j = r;
  while (--j, LE(x[j], x[j+1])) if (j <= l) return;  // explicit stop of for loop
#endif

  j = r - randIndex(r-l+1);
  SWAP(x[j], x[r], v);  // first argument pivot now in v and x[r]

  // MAIN
  j = r;
  i = l - 1;
  for (;;){
    while(LT(x[++i], v));                     // sentinel stop of for loop
    while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
    if (j <= i)break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[i], x[r], t); // the index i from the first loop is guaranteed to be in a legal position

  if (i < p[0]){
    Duckpart_TieLeft(x, i+1, r, p);
  }else if(i > p[1]){
    Duckpart_TieRight(x, l, i-1, p);
  }else{
    Duckpart_TieRight(x, l, i-1, p);
    Duckpart_TieLeft(x, i+1, r, p);
  }
}

// pivot partioning placing all pivot ties low
static void Duckpart_TieLeft(
    ValueT *x
  , IndexT l, IndexT r, IndexT *p
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
  ValueT t, v;
  IndexT i, j;

  // POET replaces DIET: tentatively invest more COMPs than DIET (and MAIN has to restart from scratch)
#if PARTIAL_INSERTIONSORT_LIMIT
  if (Partial_Insertionsort_l2r(x, l, r) > r){
    //if ((r-l)>48)Rprintf(" ET Ducksort_TieLeft d=%d\n", r-l+1);
    return;
  }
#else
  i = l;
  while (++i, LE(x[i-1], x[i])) if (i >= r) return;  // explicit stop of for loop
#endif

  i = l + randIndex(r-l+1);
  SWAP(x[i], x[l], v);  // first argument pivot now in v and x[l]

  // MAIN
  i = l;
  j = r + 1;
  for (;;){
    while(GT(x[--j], v)); // sentinel stop guaranteed by pivot at the left
    while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
    if (i >= j) break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[j], x[l], t); // the index j from the first loop is guaranteed to be in a legal position

  if (j < p[0]){
    Duckpart_TieLeft(x, j+1, r, p);
  }else if(j > p[1]){
    Duckpart_TieRight(x, l, j-1, p);
  }else{
    Duckpart_TieRight(x, l, j-1, p);
    Duckpart_TieLeft(x, j+1, r, p);
  }
}

void Duckpart_insitu(ValueT *x, IndexT n, IndexT *partial){
  Duckpart_TieLeft(x, 0, n-1, partial);
  return;
}

void Duckpart_exsitu(ValueT *x, IndexT n, IndexT *partial){
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0; i<n; i++)
    aux[i] = x[i];
  Duckpart_TieLeft(aux, 0, n-1, partial);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
  return;
}
