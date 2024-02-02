/*
# greeNsort stable ternary Partitioning to distant buffer
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_SZackPart3Copy_h
#define ALREADY_DEFINED_SZackPart3Copy_h

#include "Zackselect.h"

// stably creating three partitions in aux
// l..(ri.min-1)  |   ri.min <= k <= ri.max   |   ri.max+1..r
// while keeping ties to k together in center part RangeIndexT.min .. RangeIndexT.max
static RangeIndexT SZackPart3Copy(ValueT *x, ValueT *aux, IndexT l, IndexT r, IndexT k){
  IndexT i, il, ic, ir;
  // selecting is unstable, hence we must select on a copy
  for (i=l;i<=r;i++)
    aux[i] = x[i];
  RangeIndexT ri = Zackselect_TieLeft(aux, l, r, k);
  ValueT v = aux[k];
  // using Zackselect allows to determine the number of median ties allowed in lower part without cost of 50% scan
  // stably partition putting ties to lower part
  il = l;
  ic = ri.min;
  ir = ri.max+1;
  for (i=l; i<=r ;i++){
    if (LT(x[i],v)){
      aux[il++] = x[i];
    }else{
      if (EQ(x[i],v))
        aux[ic++] = x[i];
      else
        aux[ir++] = x[i];
    }
  }
  return ri;
}

#endif
