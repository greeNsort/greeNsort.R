/*
# greeNsort Insertionsort sorting Matrix-Columns by movig separated first row keys 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"

#include "NInsertionsort_l2r.h"

void NInsertionsort_l2r_insitu(IntValueT *x, IndexT n, IndexT m)
{
  IndexT i,j, nm=n*m;
  // initialize small records with only key and index
  IntIndT * ind = (IntIndT *) MALLOC(n, IntIndT);
  for (i=0,j=0; i<n; i++,j+=m){
    ind[i].key = x[j];
    ind[i].rec = i;
  }
  // sort
  NInsertionsort_l2r(ind, 0, n-1);
  // apply index to reorder
  IntValueT * aux = (IntValueT *) MALLOC(nm, IntValueT);
  Norder_exsitu(
    x   // source array of n records with m integers
  , aux // target array of n records with m integers for fast O(N) reordering
  , ind // order of record indexes in struct field .rec for re-ordering
  , n      // number of records
  , m      // number of integer in record
  );
  FREE(ind);
  for (i=0;i<nm;i+=m)
    MMOVE(x+i, aux+i, m);
  FREE(aux);
}

void NInsertionsort_l2r_exsitu(IntValueT *x, IndexT n, IndexT m)
{
  IndexT i,j, nm=n*m;
  IntIndT * ind = (IntIndT *) MALLOC(n, IntIndT);
  for (i=0,j=0; i<n; i++,j+=m){
    ind[i].key = x[j];
    ind[i].rec = i;
  }
  NInsertionsort_l2r(ind, 0, n-1);
  IntValueT * aux = (IntValueT *) MALLOC(nm, IntValueT);
  for (i=0;i<nm;i+=m)
    MMOVE(aux+i, x+i, m);
  IntValueT * aux2 = (IntValueT *) MALLOC(nm, IntValueT);
  Norder_exsitu(
      aux  // source array of n records with m integers
    , aux2 // target array of n records with m integers for fast O(N) reordering
    , ind // order of record indexes in struct field .rec for re-ordering
    , n      // number of records
    , m      // number of integer in record
  );
  FREE(ind);
  for (i=0;i<nm;i+=m)
    MMOVE(aux+i, aux2+i, m);
  FREE(aux2);
  for (i=0;i<nm;i+=m)
    MMOVE(x+i, aux+i, m);
  FREE(aux);
}
