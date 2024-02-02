/*
# greeNsort Knuth sorting Matrix-Columns by movig separated first row keys 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "NInsertionsort_l2r.h"

// Following Knuth we need only one termination check per iteration
static void NKnuthsortP_merge(IntIndT *z, IntIndT *l, IntIndT *m, IntIndT *r){
  IntIndT *j=m+1;
  IntIndT u=*l, v=*j;
  for (;;){
    if (LT(v, u)){
      *(z++) = *j++;
      if (j > r)  // check either here
        break;
      v = *j;
    }else{
      *(z++) = *l++;
      if (l > m)  // or check here
        break;
      u= *l;
    }
  }
  while(l <= m)
    *(z++) = *(l++);
  while(j <= r)
    *(z++) = *(j++);
}

static void NKnuthsortP_recurse(IntIndT *a, IntIndT *b, IndexT n){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    NInsertionsort_l2r(a, 0, n-1);
  }else
#else
  if (n>1)
#endif
  {
    m = n/2;
    NKnuthsortP_recurse(b, a, m);
    NKnuthsortP_recurse(b+m, a+m, n-m);
    NKnuthsortP_merge(a, b, b+m-1, b+n-1);
  }
}

void NKnuthsortP_insitu(
  IntValueT *x
, IndexT n
, IndexT m
, PerfT *p
)
{
  IndexT i,j, nm=n*m;
  PerfT p2, p3;
  double IntValueSize = nm*sizeof(IntValueT);
  double IntIndSize = n*sizeof(IntIndT);
  // initialize small records with only key and index
  IntIndT * ind = (IntIndT *) MALLOC(n, IntIndT);
  IntIndT * indaux = (IntIndT *) MALLOC(n, IntIndT);
  for (i=0,j=0; i<n; i++,j+=m){
    indaux[i].key = ind[i].key = x[j];
    indaux[i].rec = ind[i].rec = i;
  }
  // sort
  NKnuthsortP_recurse(ind, indaux, n);
  FREE(indaux);
  p2.secs = getDeltaSecs();
  p2.size = 2*IntIndSize;
  if (m>1){
    // apply index to reorder
    IntValueT * aux2 = (IntValueT *) MALLOC(nm, IntValueT);
    Norder_exsitu(
      x   // source array of n records with m integers
      , aux2 // target array of n records with m integers for fast O(N) reordering
      , ind // order of record indexes in struct field .rec for re-ordering
      , n      // number of records
      , m      // number of integer in record
    );
    FREE(aux2);
    FREE(ind);
    p3.secs = getDeltaSecs();
    p3.size = IntIndSize+IntValueSize;
  }else{
    for (i=0;i<n;i++)
      x[i] = ind[i].key;
    FREE(ind);
    p3.secs = getDeltaSecs();
    p3.size = IntIndSize;
  }
  // Rprintf("2 secs=%f, size=%f\n", p2.secs, p2.size);
  // Rprintf("3 secs=%f, size=%f\n", p3.secs, p3.size);
  p->secs = p2.secs + p3.secs;
  p->size = 1 + // data
    (p2.secs*p2.size + p3.secs*p3.size) / (p->secs*IntValueSize); // buffer
}

void NKnuthsortP_exsitu(
  IntValueT *x
, IndexT n
, IndexT m
, PerfT *p
)
{
  IndexT i,j, nm=n*m;
  PerfT p1, p2, p3, p4;
  double IntValueSize = nm*sizeof(IntValueT);
  double IntIndSize = n*sizeof(IntIndT);
  IntValueT *aux = (IntValueT *) MALLOC(nm, IntValueT);
  for (i=0;i<nm;i+=m)
    MMOVE(aux+i, x+i, m);
  p1.secs = getDeltaSecs();
  p1.size = 0;
  // initialize small records with only key and index
  IntIndT * ind = (IntIndT *) MALLOC(n, IntIndT);
  IntIndT * indaux = (IntIndT *) MALLOC(n, IntIndT);
  for (i=0,j=0; i<n; i++,j+=m){
    indaux[i].key = ind[i].key = aux[j];
    indaux[i].rec = ind[i].rec = i;
  }
  // sort
  NKnuthsortP_recurse(ind, indaux, n);
  FREE(indaux);
  p2.secs = getDeltaSecs();
  p2.size = 2*IntIndSize;
  if (m>1){
    // apply index to reorder
    IntValueT * aux2 = (IntValueT *) MALLOC(nm, IntValueT);
    Norder_exsitu(
      aux   // source array of n records with m integers
      , aux2 // target array of n records with m integers for fast O(N) reordering
      , ind // order of record indexes in struct field .rec for re-ordering
      , n      // number of records
      , m      // number of integer in record
    );
    FREE(aux2);
    FREE(ind);
    p3.secs = getDeltaSecs();
    p3.size = IntIndSize+IntValueSize;
  }else{
    for (i=0;i<n;i++)
      aux[i] = ind[i].key;
    FREE(ind);
    p3.secs = getDeltaSecs();
    p3.size = IntIndSize;
  }
  for (i=0;i<nm;i+=m)
    MMOVE(x+i, aux+i, m);
  FREE(aux);
  p4.secs = getDeltaSecs();
  p4.size = 0;
  // Rprintf("1 secs=%f, size=%f\n", p1.secs, p1.size);
  // Rprintf("2 secs=%f, size=%f\n", p2.secs, p2.size);
  // Rprintf("3 secs=%f, size=%f\n", p3.secs, p3.size);
  // Rprintf("4 secs=%f, size=%f\n", p4.secs, p4.size);
  p->secs = p1.secs + p2.secs + p3.secs + p4.secs;
  p->size = 1 + // data
    (p1.secs*p1.size + p2.secs*p2.size + p3.secs*p3.size + p4.secs*p4.size ) / (p->secs*IntValueSize); // buffer
}
