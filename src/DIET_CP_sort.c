/*
# greeNsort DIET CP sort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "DIET_CP_sort.h"

// includes pivot in right recursion
void DietCPsort_insitu(
ValueT *x
, IndexT n
){
  if (n>1){
    ValueT *buf;
    buf = (ValueT *) MALLOC(n, ValueT);
    DietCPsort_rec_TieRight(
      x   // ValueT *ori
    , x   // ValueT *dat
    , buf // ValueT *buf
    , 0   // IndexT l
    , n-1 // IndexT r
    );
    FREE(buf);
  }
}

// includes pivot in right recursion
void DietCPsort_exsitu(
ValueT *x
, IndexT n
){
  if (n>1){
    IndexT i;
    ValueT *buf;
    buf = (ValueT *) MALLOC(2*n, ValueT);
    for (i=0;i<n;i++)
      buf[i] = x[i];
    DietCPsort_rec_TieRight(
      buf   // ValueT *ori
    , buf   // ValueT *dat
    , buf+n // ValueT *buf
    , 0     // IndexT l
    , n-1   // IndexT r
    );
    for (i=0;i<n;i++)
      x[i] = buf[i];
    FREE(buf);
  }
}
