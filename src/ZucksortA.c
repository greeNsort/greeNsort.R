/*
# greeNsort Zucksort A-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


static void ZucksortA_TieLeft(
    ValueT *x
  , IndexT l, IndexT r
);

// pivot partioning placing all pivot ties high
static void ZucksortA_TieRight(
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
  IndexT j, i = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
  // DIET
  j = r;
  while (EQ(x[--j], v)) if (j <= l) return;  // explicit stop of for loop
  // restore  the usual initialization i=l-1; j=r, i.e. we test the same value that was EQ again for GE
  i = l - 1;  j++;
  for (;;){
    while(LT(x[++i], v));                     // sentinel stop guaranteed by pivot at the right
    while (GE(x[--j], v)) if (j <= i) break;  // explicit stop of for loop
    if (j <= i)break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[i], x[r], t); // the index i from the first loop is guaranteed to be in a legal position
  ZucksortA_TieRight(x, l, i-1);
  ZucksortA_TieLeft(x, i+1, r);
}

// pivot partioning placing all pivot ties low
static void ZucksortA_TieLeft(
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
  IndexT i, j = l+randIndex(r-l+1);
  ValueT t, v;
  SWAP(x[j], x[l], v);  // first argument pivot now in v and x[l]
  // in the DIET pre-loop we do (almost) the same as for pivot-ties in the main loop
  i = l;
  while (EQ(x[++i], v)) if (i >= r) return;  // explicit stop of for loop
  // restore  the usual initialization i=l; j=r+1, i.e. we test the same value that was EQ again for GE
  i--; j = r+1;
  for (;;){
    while(GT(x[--j], v));                      // sentinel stop guaranteed by pivot at the left
    while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
    if (i >= j) break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[l], x[j], t); // the index j from the first loop is guaranteed to be in a legal position
  ZucksortA_TieRight(x, l, j-1);
  ZucksortA_TieLeft(x, j+1, r);
}


/* was. ------ ZocksortA experiments for Ducksort ------------------------------------------------------

// simplicity wins: first POET then MAIN
static void ZocksortA_TieLeft(
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
  ValueT t, v;
  IndexT i,j;

  // POET replaces DIET: tentatively invest more COMPs than DIET (and MAIN has to restart from scratch)
  i = l;
  while (++i, LE(x[i-1], x[i])) if (i >= r) return;  // explicit stop of for loop

  i = l+randIndex(r-l+1);
  SWAP(x[i], x[l], v);  // first argument pivot now in v and x[l]

  // MAIN
  i = l;
  j = r+1;
  for (;;){
    while(GT(x[--j], v)); // sentinel stop guaranteed by pivot at the left
    while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
    if (i >= j) break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[j], x[l], t); // the index j from the first loop is guaranteed to be in a legal position
  ZocksortA_TieLeft(x, l, j-1);
  ZocksortA_TieLeft(x, j+1, r);
}

// first DIET then POET and MAIN with usual sentinel: 5% extra due to bad locality of splitted first SWAP for sentinel
static void ZocksortA_TieLeft(
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
  ValueT t, v;
  IndexT k, i, j = l+randIndex(r-l+1);
  // start the first SWAP (but do not destroy the original order)
  v=x[j];

  // DIET
  i = l - 1; // here we need to include l itself because sentinel is not yet in place (one more COMP than without POET)
  while (EQ(x[++i], v)) if (i >= r) return;  // explicit stop of for loop
  i--;

  // POET reuses gain of DIET
  k = i; // i is compared again (now two more COMP than without POET)
  while (++k, LE(x[k-1], x[k])) if (k >= r) return;  // explicit stop of for loop

  // too expensive: we rarely pass the pivot
  // binary search for GT v in the sequence i..k-1 so far
  // BSEARCH_ASC_TORIGHT
  // IndexT m;
  // k--;
  // while (i<k){
  //   m = k - ((k - i) / 2);
  //   if (LT(v, x[m]))
  //     k = m - 1;
  //   else
  //     i = m;
  // }

  // LEAN but still too expensive
  // k--;
  // if (LE(x[k], v))
  //   i = k;

  // finalize the first SWAP (now where presorting was not perfect) is too expensive (bad locality)
  x[j] = x[l];
  x[l] = v;

  //MAIN: reuse of gain of DIET (POET is waste)
  j = r+1;
  for (;;){
    while(GT(x[--j], v));  // sentinel stop guaranteed by pivot at the left
    while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
    if (i >= j) break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[j], x[l], t); // the index j from the first loop is guaranteed to be in a legal position
  ZocksortA_TieLeft(x, l, j-1);
  ZocksortA_TieLeft(x, j+1, r);
}
*/


/* first DIET then POET and MAIN with two sentinels *after* POET: 5% extra due to bad locality of last sentinel SWAP
static void ZocksortA_TieLeft(
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
  ValueT t, v;
  IndexT i, j;
  IndexT I, J;
  // just pivot value, pivot location not used
  v=x[l+randIndex(r-l+1)];
  i = l - 1; // here we need to include l itself (one more COMP than without POET)

  // DIET
  while (EQ(x[++i], v)) if (i >= r) return;  // explicit stop of for loop
  i--;

  // POET reuses the gain of DIET
  j = i; // i is compared again (now two more COMP than without POET)
  while (++j, LE(x[j-1], x[j])) if (j >= r) return;  // explicit stop of for loop

  // MAIN reuses the gain of DIET (POET is waste)
  j = r+1;
  while(GT(x[--j], v)) if (i >= j) break;   // explicit stop of for loop
  while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
  if (i < j){
    // do not SWAP and use as sentinels
    I = i;
    J = j;
    for (;;){
      while(GT(x[--j], v)) if (i >= j) break;   // explicit stop of for loop
      while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
      if (i >= j) break;
      SWAP(x[i], x[j], t);
    }
    // delayed swap of sentinels
    SWAP(x[I], x[J], t);
  }
  ZocksortA_TieLeft(x, l, j);
  ZocksortA_TieLeft(x, j+1, r);
}
*/


/* first DIET then POET and MAIN without sentinels: 1.5% extra
static void ZocksortA_TieLeft(
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
  ValueT t, v;
  IndexT i, j;
  // just pivot value, pivot location not used
  v=x[l+randIndex(r-l+1)];
  i = l - 1; // here we need to include l itself (one more COMP than without POET)

  // DIET
  while (EQ(x[++i], v)) if (i >= r) return;  // explicit stop of for loop
  i--;

  // POET reuses the gain of DIET
  // j = i + (i<l ? 1 : 0); // i is compared again (now two more COMP than without POET)
  // while (++j, LE(x[j-1], x[j])) if (j >= r) return;  // explicit stop of for loop

  // MAIN reuses the gain of DIET (POET is wasted)
  j = r+1;
  for (;;){
    while(GT(x[--j], v)) if (i >= j) break;   // explicit stop of for loop
    while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
    if (i >= j) break;
    SWAP(x[i], x[j], t);
  }
  // no swap of pivot to j, j belongs to left
  ZocksortA_TieLeft(x, l, j);
  ZocksortA_TieLeft(x, j+1, r);
}
*/


/* first DIET then POET (Pre-Order Early Termination), BIAS (Binary Identified Asymmetric Search) and MAIN without sentinels: 1.5% extra
static void ZocksortA_TieLeft(
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
  ValueT t, v;
  IndexT i, j;
  // just pivot value, pivot location not used
  v=x[l+randIndex(r-l+1)];
  i = l - 1; // here we need to include l itself (one more COMP than without POET)

  // Rprintf("l=%d r=%d v=%f\n", l+1, r+1, v);
  // for (j=l;j<=r;j++)
  //   Rprintf(" %f", x[j]);
  // Rprintf("\n");

  // DIET loop
  while (EQ(x[++i], v)) if (i >= r) return;  // explicit stop of for loop and ET
  i--; // re-use previous work except last element

  // Rprintf("-- i=%d after DIET\n", i+1);

  // POET loop: invest extra COMPs (last element of DIET is compared again, except for last element == l)
  j = i + (i<l ? 1 : 0); // i is compared again (now two more COMP than without POET)
  while (++j, LE(x[j-1], x[j])) if (j >= r) return;  // explicit stop of for loop and ET
  // Rprintf("-- i=%d j=%d after POET\n", i+1, j+1);

  // BSEARCH_ASC_GT BSEARCH_ASC_TORIGHT find the rightmost element LE pivot in i
  i += (i<l ? 1 : 0); // first element of sequence
  IndexT m;
  IndexT q=j; // first element after sequence
  j--;        // last element of sequence (i <= j)
  while (i<j){
    m = j - ((j - i) / 2);
    if (GT(x[m], v))
      j = m - 1;
    else
      i = m;
  }
  // Rprintf("-- i=%d q=%d after BSEARCH_ASC_TORIGHT\n", i+1, q+1);
  if (GT(x[i], v))
    i--;
  // Rprintf("-- i=%d q=%d after BSEARCH_ASC_GT\n", i+1, q+1);

  j = r+1;

  // BIAS loop
  for (;;){
    while(GT(x[--j], v)) if (i >= j) goto fin;   // explicit stop of for loop
    if (++i >= q) break;  // explicit stop of for loop
    if (i >= j) goto fin;
    SWAP(x[i], x[j], t);
    // Rprintf("BIAS SWAP(%d,%d)\n", i+1, j+1);
  }
  // Rprintf("-- i=%d j=%d after BIAS\n", i+1, j+1);
  --i;
  ++j;

  // MAIN loop reuses the gain of DIET, POET and BIAS
  if (i<j){
    for (;;){
      while(GT(x[--j], v)) if (i >= j) break;   // explicit stop of for loop
      while (LE(x[++i], v)) if (i >= j) break;  // explicit stop of for loop
      if (i >= j) break;
      SWAP(x[i], x[j], t);
      // Rprintf("MAIN SWAP(%d,%d)\n", i+1, j+1);
    }
    // Rprintf("-- i=%d j=%d after MAIN\n", i+1, j+1);
  }

  fin:
    // no swap of pivot to j, j belongs to left
    ZocksortA_TieLeft(x, l, j);
    ZocksortA_TieLeft(x, j+1, r);
}
*/

void ZucksortA_insitu(
    ValueT *x
  , IndexT n
)
{
  // single non-recursive POET-loop
  IndexT   r=n-1, i = 0;
  while (++i, LE(x[i-1], x[i]))
    if (i >= r)
      return;
  ZucksortA_TieLeft(x, 0, n-1);
}


void ZucksortA_exsitu(
    ValueT *x
  , IndexT n
)
{
  // single non-recursive POET-loop
  IndexT   r=n-1, i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  i = 0;
  while (++i, LE(aux[i-1], aux[i]))
    if (i >= r)
      goto finish;
  ZucksortA_TieLeft(aux, 0, n-1);
  finish:
    for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
