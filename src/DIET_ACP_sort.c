/*
# greeNsort DIET ACP sort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "DIET_ACP_sort.h"

// includes pivot in right recursion
void DietACPsort_insitu(
ValueT *x
, IndexT n
){
  if (n>1){
    ValueT *buf;
    buf = (ValueT *) MALLOC(n, ValueT);
    ValueT aux5[5];
    DietACPsort_rec_TieLeft(
      x   // ValueT *ori
    , x   // ValueT *dat
    , buf // ValueT *buf
    , 0   // IndexT l
    , n-1 // IndexT r
    , aux5
    );
    FREE(buf);
  }
}

// includes pivot in right recursion
void DietACPsort_exsitu(
ValueT *x
, IndexT n
){
  if (n>1){
    IndexT i;
    ValueT *buf;
    buf = (ValueT *) MALLOC(2*n, ValueT);
    for (i=0;i<n;i++)
      buf[i] = x[i];
    ValueT aux5[5];
    DietACPsort_rec_TieLeft(
      buf   // ValueT *ori
    , buf   // ValueT *dat
    , buf+n // ValueT *buf
    , 0     // IndexT l
    , n-1   // IndexT r
    , aux5
    );
    for (i=0;i<n;i++)
      x[i] = buf[i];
    FREE(buf);
  }
}
