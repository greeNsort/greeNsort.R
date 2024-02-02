/*
# greeNsort Knuthsort moving Matrix-Columns by first row keys 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "MInsertionsort_l2r.h"
#endif

// Following Knuth we need only one termination check per iteration
static void MKnuthsortP_merge(IntValueT *z, IntValueT *l, IntValueT *m, IntValueT *r, IndexT w){
  IntValueT *j=m+w;
  IntValueT u=*l, v=*j;
  for (;;){
    if (LT(v, u)){
      MMOVE(z, j, w); z+=w; j+=w;
      if (j > r)  // check either here
        break;
      v = *j;
    }else{
      MMOVE(z, l, w); z+=w; l+=w;
      if (l > m)  // or check here
        break;
      u= *l;
    }
  }
  while(l <= m){
    MMOVE(z, l, w); z+=w; l+=w;
  }
  while(j <= r){
    MMOVE(z, j, w); z+=w; j+=w;
  }
}

static void MKnuthsortP_recurse(IntValueT *a, IntValueT *b, IndexT n, IndexT w){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    MInsertionsort_l2r(a, 0, n-1, w);
  }else
#else
  if (n>1)
#endif
  {
    m = n/2;
    MKnuthsortP_recurse(b, a, m, w);
    MKnuthsortP_recurse(b+m*w, a+m*w, n-m, w);
    MKnuthsortP_merge(a, b, b+(m-1)*w, b+(n-1)*w, w);
  }
}

void MKnuthsortP_insitu(IntValueT *x, IndexT n, IndexT m)
{
  IndexT i, nm=n*m;
  IntValueT *aux = (IntValueT *) MALLOC(nm, IntValueT);
  // half of initial copying can be avoided, see bMsort
  for (i = 0; i < nm; i+=m){
    MMOVE(aux+i, x+i, m);
  }
  MKnuthsortP_recurse(x, aux, n, m);
  FREE(aux);
}


void MKnuthsortP_exsitu(IntValueT *x, IndexT n, IndexT m)
{
  IndexT i, nm=n*m;
  IntValueT *aux = (IntValueT *) MALLOC(nm+nm, IntValueT);
  IntValueT *aux2 = aux + nm;
  for (i = 0; i < nm; i+=m){
    MMOVE(aux+i, x+i, m);
    MMOVE(aux2+i, x+i, m);
  }
  MKnuthsortP_recurse(aux, aux2, n, m);
  for (i = 0; i < nm; i+=m)
    MMOVE(x+i, aux+i, m);
  FREE(aux);
}
