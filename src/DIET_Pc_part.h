/*
# greeNsort DIET Pc partition
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_DIET_Pc_part_h
#define ALREADY_DEFINED_DIET_Pc_part_h

#include "algo.h"

// ascending partitioning of data between l and r around pivot
// returns 1 if partitioning is done (due to all ties)
// and does not copy data in the done case
static IndexT DIET_Pc_partition_TieRight(
  ValueT *dat       // pointer to memory with current source data now
, ValueT *buf       // pointer to memory for buffer (receives target data)
, IndexT l      // leftmost position to be sorted
, IndexT r      // rightmost position to be sorted
, ValueT v     // pivot value
, IndexT *c     // pointer to K initialized counters
, ValueT vl     // pivot value
, ValueT vr     // pivot value
, IndexT *cl     // pointer to K non-initialized counters
, IndexT *cr     // pointer to K non-initialized counters
)
{
  IndexT done;
  IndexT i;       // 1 position to read from
  IndexT i0, i1;  // 2 positions to write to
  IndexT cl1=0, cr1=0; // local registers for counting
  // initialize positions in buffer part
  i0 = l;
  i1 = i0 + c[0];
  // DIET loop
  for (i=l; i<=r; i++){
    if (NE(dat[i], v))
      break;
    buf[i1++] = dat[i];
    // count TieLeft for the next split
    if (GT(dat[i], vr))
      cr1++;
  }
  if (i>r)
    done = 1;
  else
    done = 0;
  // MAIN loop
  for (; i<=r; i++){
    if (LT(dat[i], v)){
      buf[i0++] = dat[i];
      // count TieLeft for the next split
      if (GT(dat[i], vl))
        cl1++;
    }else{
      buf[i1++] = dat[i];
      // count TieLeft for the next split
      if (GT(dat[i], vr))
        cr1++;
    }
  }
  // for (i=l; i<=r; i++)
  //   Rprintf("DIET_Pc_partition_TieRight %d : %f : %f\n", i, dat[i], buf[i]);
  cl[0] = c[0] - cl1;
  cl[1] = cl1;
  cr[0] = c[1] - cr1;
  cr[1] = cr1;
  return done;
}

static IndexT DIET_Pc_partition_TieLeft(
  ValueT *dat       // pointer to memory with current source data now
, ValueT *buf       // pointer to memory for buffer (receives target data)
, IndexT l      // leftmost position to be sorted
, IndexT r      // rightmost position to be sorted
, ValueT v     // pivot value
, IndexT *c     // pointer to K initialized counters
, ValueT vl     // pivot value
, ValueT vr     // pivot value
, IndexT *cl     // pointer to K non-initialized counters
, IndexT *cr     // pointer to K non-initialized counters
)
{
  IndexT done;
  IndexT i;       // 1 position to read from
  IndexT i0, i1;  // 2 positions to write to
  IndexT cl0=0, cr0=0; // local registers for counting
  // initialize positions in buffer part
  i0 = l;
  i1 = i0 + c[0];
  // DIET loop
  for (i=l; i<=r; i++){
    if (NE(dat[i], v))
      break;
    buf[i0++] = dat[i];
    // count TieRight for the next split
    if (LT(dat[i], vl))
      cl0++;
  }
  if (i>r)
    done = 1;
  else
    done = 0;
  // MAIN loop
  for (; i<=r; i++){
    if (LE(dat[i], v)){
      buf[i0++] = dat[i];
      // count TieRight for the next split
      if (LT(dat[i], vl))
        cl0++;
    }else{
      buf[i1++] = dat[i];
      // count TieRight for the next split
      if (LT(dat[i], vr))
        cr0++;
    }
  }
  // for (i=l; i<=r; i++)
  //   Rprintf("DIET_Pc_partition_TieLeft %d : %f : %f\n", i, dat[i], buf[i]);
  cl[0] = cl0;
  cl[1] = c[0] - cl0;
  cr[0] = cr0;
  cr[1] = c[1] - cr0;
  return done;
}

// fallback partitioning derived from DIET_cP2 but returning DIET done
static IndexT DIET_P_partition_TieRight(
    ValueT *dat       // pointer to memory with current source data now
  , ValueT *buf       // pointer to memory for buffer (receives target data)
  , IndexT l      // leftmost position to be sorted
  , IndexT r      // rightmost position to be sorted
  , ValueT v     // pivot value
  , IndexT *c     // pointer to K initialized counters
)
{
  IndexT done;
  IndexT i;
  IndexT i0, i1;  // 2 positions to write to
  // initialize positions in buffer part
  i0 = l;
  i1 = i0 + c[0];
  // DIET loop
  for (i=l; i<=r; i++){
    if (NE(dat[i], v))
      break;
    buf[i1++] = dat[i];
  }
  if (i>r)
    done = 1;
  else
    done = 0;
  // MAIN loop
  for (; i<=r; i++){
    if (LT(dat[i], v)){
      buf[i0++] = dat[i];
    }else{
      buf[i1++] = dat[i];
    }
  }
  return done;
}

static IndexT DIET_P_partition_TieLeft(
    ValueT *dat       // pointer to memory with current source data now
  , ValueT *buf       // pointer to memory for buffer (receives target data)
  , IndexT l      // leftmost position to be sorted
  , IndexT r      // rightmost position to be sorted
  , ValueT v     // pivot value
  , IndexT *c     // pointer to K initialized counters
)
{
  IndexT done;
  IndexT i;
  IndexT i0, i1;  // 2 positions to write to
  // initialize positions in buffer part
  i0 = l;
  i1 = i0 + c[0];
  // DIET loop
  for (i=l; i<=r; i++){
    if (NE(dat[i], v))
      break;
    buf[i0++] = dat[i];
  }
  if (i>r)
    done = 1;
  else
    done = 0;
  // MAIN loop
  for (; i<=r; i++){
    if (LE(dat[i], v)){
      buf[i0++] = dat[i];
    }else{
      buf[i1++] = dat[i];
    }
  }
  return done;
}


#endif
