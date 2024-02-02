/*
# greeNsort DIET Pc sort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "DIET_Pc_sort.h"
#include "DIET_count.h"


// includes pivot in right recursion
void DietPcsort_insitu(
ValueT *x
, IndexT n
){
  if ((n-1) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
  IndexT c[2];
  ValueT v = x[randIndex(n)];
  IndexT done = DIET_count_TieRight(x, 0, n-1, v, c);
  if (!done){
    ValueT *buf;
    buf = (ValueT *) MALLOC(n, ValueT);
    DietPcsort_rec_TieRight(
      x   // ValueT *ori
    , x   // ValueT *dat
    , buf // ValueT *buf
    , 0   // IndexT l
    , n-1 // IndexT r
    , v   // pivot value
    , c   // pointer to K initialized counters
    , done
    );
    FREE(buf);
  }
}

// includes pivot in right recursion
void DietPcsort_exsitu(
ValueT *x
, IndexT n
){
  if ((n-1) < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
  IndexT c[2];
  ValueT v = x[randIndex(n)];
  IndexT done = DIET_count_TieRight(x, 0, n-1, v, c);
  if (!done){
    IndexT i;
    ValueT *buf;
    buf = (ValueT *) MALLOC(2*n, ValueT);
    for (i=0;i<n;i++)
      buf[i] = x[i];
    DietPcsort_rec_TieRight(
      buf   // ValueT *ori
    , buf   // ValueT *dat
    , buf+n // ValueT *buf
    , 0     // IndexT l
    , n-1   // IndexT r
    , v   // pivot value
    , c   // pointer to K initialized counters
    , done
    );
    for (i=0;i<n;i++)
      x[i] = buf[i];
    FREE(buf);
  }
}
