/*
# greeNsort Pugsort 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "Pickselect.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


static void Pugsort(ValueT *x, IndexT l, IndexT r)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
  IndexT i = (r+l)/2;
  Pickselect(x, l, r, i);
  Pugsort(x, l, i-1);
  Pugsort(x, i+1, r);
}

void Pugsort_insitu(ValueT *x, IndexT n)
{
  Pugsort(x, 0, n-1);
}
