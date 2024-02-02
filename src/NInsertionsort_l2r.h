/*
# greeNsort Insertionsort sorting Matrix-Columns by movig separated first row keys 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// insertion sorting ascending from left to right
// modifies *x

#ifndef ALREADY_DEFINED_NInsertionsort_l2r_h
#define ALREADY_DEFINED_NInsertionsort_l2r_h

#include "math.h"
#include "algo.h"

#undef KEY

#ifdef STABLE_TEST
#define KEY(x)((IntValueT) stable_dblkey((double) x.key) )
#else
#define KEY(x)(x.key)
#endif



/*
// starting from left
static void NInsertionsort_l2r(
  IntIndT *x
, IndexT l
, IndexT r
)
{
  IndexT i,j;
  IntIndT t;
  // Sedgewick: put correct element as sentinel into rightmost position
  // this ruins scaling of sorting almost sorted data to big problems
  for (i=r;i>l;i--){
    if (LT(x[i],x[i-1])) {
      SWAP(x[i-1],x[i],t);
    }
  }
  for (i=l+2;i<=r;i++){
    j=i;
    MOVE(t, x[i]);
    while (LT(t,x[j-1])){
      MOVE(x[j], x[j-1]);
    j--;
    }
    MOVE(x[j], t);
  }
}
*/


// starting from left
static void NInsertionsort_l2r(
    IntIndT *x
  , IndexT l
  , IndexT r
)
{
  IndexT i, j;
  IntIndT t;
  for (i=l+1;i<=r;i++){
    j=i;
    MOVE(t, x[i]);
    while (j>l && GT(x[j-1], t)){
      MOVE(x[j], x[j-1]);
      j--;
    }
    MOVE(x[j], t);
  }
}



/* The following version with a sentinel would be 10% faster, but is not possible for _r2l

static void SentinelNInsertionsort_l2r(
    IntIndT *x
  , IndexT l
  , IndexT r
)
{
  IndexT i, j;
  IntIndT t;

  MOVE(t, x[l]);
  for (i=l+1;i<=r;i++){
    if (GT(t, x[i]))
      SWAP(x[i], x[1], t);
  }

  for (i=l+1;i<=r;i++){
    j=i;
    MOVE(t, x[i]);
    while (GT(x[j-1], t)){
      MOVE(x[j], x[j-1]);
      j--;
    }
    MOVE(x[j], t);
  }
}

*/


static void Norder_exsitu(
  IntValueT *s  // source and target array of n records with m integers
, IntValueT *t  // temp array of n records with m integers for fast O(N) reordering
, IntIndT *o    // order of record indexes in struct field .rec for re-ordering
, IndexT n      // number of records
, IndexT m      // number of integer in record
)
{
  IndexT  i, im;
  // reorder
  for (i = 0, im=0; i < n; i++, im+=m){
    MMOVE(t+im, s+o[i].rec*m, m);
  }
  for (i = 0, im=0; i < n; i++, im+=m){
    MMOVE(s+im, t+im, m);
  }
}

// 2-pass: much too expensive
// static void Norder2_exsitu(
//   IntValueT *s  // source array of n records with m integers
// , IntValueT *t  // target array of n records with m integers for fast O(N) reordering
// , IntIndT *o    // order of record indexes in struct field .rec for re-ordering
// , IndexT n      // number of records
// , IndexT m      // number of integer in record
// )
// {
//   IndexT  i, j, im, il, ir;
//   //IndexT r = (IndexT) sqrt((double) n);
//   IndexT r = (IndexT) round(pow((double) n, 1.0/3));
//   IndexT r2 = r*r;
//   //Rprintf("r=%d r2=%d\n", r, r2);
//   IndexT * b = (IndexT *) MALLOC(r, IndexT);
//   IndexT * p = (IndexT *) MALLOC(n, IndexT);
//   for (i = 0, j = 0; i < r; i++, j+=r2){
//     b[i] = j;
//   }
//   i = 0;
//   for (i=0; i<n; i++){
//     o[i].key = o[i].rec / r2;
//     // Rprintf("i=%d o.key=%d o.rec=%d\n", i, o[i].key, o[i].rec);
//   }
//   for (il = 0;il<n;il+=r2){
//     ir = il + r2;
//     if (ir>n)
//       ir = n;
//     for (j=0; j<r; j++)
//     for (i=il,im=il*m; i<ir; i++,im+=m){
//       if (o[i].key == j){
//         // Rprintf("il=%d i=%d ir=%d im=%d j=%d o.key=%d o.rec=%d b[j]=%d\n", il, i, ir, im, j, o[i].key, o[i].rec, b[j]);
//         p[i] = b[j];
//         MMOVE(t+b[j]*m, s+o[i].rec*m, m);
//         b[j]++;
//       }
//     }
//   }
//
//   for (i = 0, im=0; i < n; i++, im+=m){
//     // Rprintf("i=%d p[i]=%d\n", i, p[i]);
//     MMOVE(s+im, t+p[i]*m, m);
//   }
//
//   FREE(p);
//   FREE(b);
// }


#endif
