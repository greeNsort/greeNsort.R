/*
# greeNsort DIET CPT partition
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_DIET_CPT_part_h
#define ALREADY_DEFINED_DIET_CPT_part_h

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif
#include "DIET_count.h"

// ascending partitioning of data between l and r around pivot
// returns 1 if all values are the same (tie)
static IndexT DietCPTsort_partition_TieRight(
ValueT *x    // pointer to data and buffer
, IndexT l      // leftmost position to be sorted
, IndexT r      // rightmost position to be sorted, the rightmost buffer position is r+(r-l+1)
, ValueT v   // pivot value
, IndexT *c     // pointer to K counters
)
{
  IndexT i, n = r - l + 1;
  IndexT done,i0,i1;  // 2 positions to write to
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    done = TRUE;
  }else
#endif
  done = DIET_count_TieRight(x, l, r, v, c);
  if (done){
    // finalize and signal stop of partitioning
    for (i=r,i0=l+n+n-2;i>l;i--,DEC2(i0)){
      x[i0]= x[i];
    }
  }else{
    // initialize positions in buffer part
    i0 = l+n;
    i1 = i0 + c[0];
    // partition
    for (i=l; i<=r; i++){
      if (LT(x[i], v)){
        x[i0++] = x[i];
      }else{
        x[i1++] = x[i];
      }
    }
    // shift into final positions leaving 100% buffer
    i = l;
    i0 = l+n;
    i1 = i0 + c[0];
    // copy data
    while(i0<i1)
      x[i++] = x[i0++];
    // skip buffer
    i += c[0];
    i1 = i0+c[1];
    // copy data
    while(i0<i1)
      x[i++] = x[i0++];
  }
  return(done);
}

static IndexT DietCPTsort_partition_TieLeft(
ValueT *x    // pointer to data and buffer
, IndexT l      // leftmost position to be sorted
, IndexT r      // rightmost position to be sorted, the rightmost buffer position is r+(r-l+1)
, ValueT v   // pivot value
, IndexT *c     // pointer to K counters
)
{
  IndexT i, n = r - l + 1;
  IndexT done,i0,i1;  // 2 positions to write to
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    done = TRUE;
  }else
#endif
  done = DIET_count_TieLeft(x, l, r, v, c);
  if (done){
    // finalize and signal stop of partitioning
    for (i=r,i0=l+n+n-2;i>l;i--,DEC2(i0)){
      x[i0]= x[i];
    }
  }else{
    // initialize positions in buffer part
    i0 = l+n;
    i1 = i0 + c[0];
    // partition
    for (i=l; i<=r; i++){
      if (LE(x[i], v)){
        x[i0++] = x[i];
      }else{
        x[i1++] = x[i];
      }
    }
    // shift into final positions leaving 100% buffer
    i = l;
    i0 = l+n;
    i1 = i0 + c[0];
    // copy data
    while(i0<i1)
      x[i++] = x[i0++];
    // skip buffer
    i += c[0];
    i1 = i0+c[1];
    // copy data
    while(i0<i1)
      x[i++] = x[i0++];
  }
  return(done);
}

#endif
