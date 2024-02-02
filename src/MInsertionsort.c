/*
# greeNsort Insertionsort moving Matrix-Columns by first row keys 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"

#include "MInsertionsort_l2r.h"


void MInsertionsort_l2r_insitu(IntValueT *x, IndexT n, IndexT m)
{
  MInsertionsort_l2r(x, 0, n-1, m);
}

void MInsertionsort_l2r_exsitu(IntValueT *x, IndexT n, IndexT m)
{
  IndexT i, nm=n*m;
  IntValueT * aux = (IntValueT *) MALLOC(nm, IntValueT);
  for (i=0;i<nm;i+=m)
    MMOVE(aux+i, x+i, m);
  MInsertionsort_l2r(aux, 0, n-1, m);
  for (i=0;i<nm;i+=m)
    MMOVE(x+i, aux+i, m);
  FREE(aux);
}

