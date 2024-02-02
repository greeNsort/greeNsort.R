/*
# greeNsort partial Zucksort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static void Zuckpart_TieLeft(
    ValueT *x
  , IndexT l, IndexT r, IndexT *p
);

// pivot partioning placing all pivot ties high
static void Zuckpart_TieRight(
    ValueT *x
  , IndexT l, IndexT r, IndexT *p
){
  IndexT i,j;
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r){
    return;
  }
#endif
  ValueT t, v;
  i = l+randIndex(r-l+1);
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]

  // elegant
  j = r;
  while (EQ(x[--j], v)) if (j <= l) return;  // explicit stop of for loop

  // restore  the usual initialization i=l-1; j=r, i.e. we test the same value that was EQ again for GE
  i = l - 1;  j++;
  for (;;){
    while(LT(x[++i], v));                      // sentinel stop guaranteed by pivot at the right
    while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
    if (j <= i)break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[i], x[r], t); // the index i from the first loop is guaranteed to be in a legal position

  if (i < p[0]){
    Zuckpart_TieLeft(x, i+1, r, p);
  }else if(i > p[1]){
    Zuckpart_TieRight(x, l, i-1, p);
  }else{
    Zuckpart_TieRight(x, l, i-1, p);
    Zuckpart_TieLeft(x, i+1, r, p);
  }
}

// pivot partioning placing all pivot ties low
static void Zuckpart_TieLeft(
    ValueT *x
  , IndexT l, IndexT r, IndexT *p
){
  IndexT i,j;
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r){
    return;
  }
#endif
  j = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[j], x[l], v);  // first argument pivot now in v and x[l]

  // elegant
  i = l;
  while (EQ(x[++i], v)) if (i >= r) return;  // explicit stop of for loop

  // restore  the usual initialization i=l; j=r+1, i.e. we test the same value that was EQ again for GE
  i--; j = r+1;
  for (;;){
    while(GT(x[--j], v));                      // sentinel stop guaranteed by pivot at the left
    while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
    if (i >= j)break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[l], x[j], t); // the index j from the first loop is guaranteed to be in a legal position

  if (j < p[0]){
    Zuckpart_TieLeft(x, j+1, r, p);
  }else if(j > p[1]){
    Zuckpart_TieRight(x, l, j-1, p);
  }else{
    Zuckpart_TieRight(x, l, j-1, p);
    Zuckpart_TieLeft(x, j+1, r, p);
  }
}

void Zuckpart_insitu(ValueT *x, IndexT n, IndexT *partial){
  Zuckpart_TieLeft(x, 0, n-1, partial);
  return;
}

void Zuckpart_exsitu(ValueT *x, IndexT n, IndexT *partial){
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0; i<n; i++)
    aux[i] = x[i];
  Zuckpart_TieLeft(aux, 0, n-1, partial);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
  return;
}
