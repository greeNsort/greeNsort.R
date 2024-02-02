/*
# greeNsort DIET ACPT sort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "DIET_ACPT_sort.h"
#include "OddEven_back_left.h"
#include "SZickPart2Split.h"

// includes pivot in right recursion
void DietACPTsort_exsitu(
ValueT *x
, IndexT n
){
  if (n>1){
    ValueT *aux;
    aux = (ValueT *) MALLOC(2*n, ValueT);
    ValueT aux5[5];
    for (IndexT i=0;i<n;i++)
      aux[i] = x[i];
    DietACPTsort_rec_TieRight(aux, 0, n-1, aux5);
    OddEven_back_left(x, aux, n);  // copy from even positions in aux to x
    FREE(aux);
  }
}

void DietACPTsort_insitu(
ValueT *x
, IndexT n
){
  if (n>1){
    IndexT nl,nr;
    nl = n/2;
    nr = n-nl;
    ValueT *aux;
    aux = (ValueT *) MALLOC(2*nr, ValueT);
    ValueT aux5[5];
    // put lower nl left in x and the upper nr left in aux
    SZickPart2Split(x, aux, 0, n-1, nl-1);
    //int i;
    // for (i=0;i<nl;i++)
      // Rprintf("left x[%d]=%f\n", i, x[i]);
    // for (i=0;i<nr;i++)
      // Rprintf("right aux[%d]=%f\n", i, aux[i]);
    DietACPTsort_rec_TieRight(x, 0, nl-1, aux5);
    DietACPTsort_rec_TieRight(aux, 0, nr-1, aux5);
    OddEven_back_left(x, x, nl);      // copy from even positions in x to x
    OddEven_back_left(x+nl, aux, nr);  // copy from even positions in aux to x
    FREE(aux);
  }
}

