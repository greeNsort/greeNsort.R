/*
# greeNsort stable ternary buffer median Partitioning
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_SZickPart3Split_h
#define ALREADY_DEFINED_SZickPart3Split_h

#include "Zickselect.h"

// stably creating three partitions
// but putting l..k to x[l..] and (k+1)..r to aux[l..]
// while keeping ties to k together in center part RangeIndexT.min .. RangeIndexT.max across the split
static RangeIndexT SZickPart3Split(ValueT *x, ValueT *aux, IndexT l, IndexT r, IndexT k){
  IndexT i, il, ic, ir;
  // selecting is unstable, hence we must select on a copy
  for (i=l;i<=r;i++)
    aux[i] = x[i];
  RangeIndexT ri = Zickselect_TieLeft(aux, l, r, k);
  ValueT v = aux[k];
  // using Zickselect allows to determine the number of median ties allowed in lower part without cost of 50% scan
  // stably partition putting ties to lower part
  il = l;
  ic = ri.min;
  ir = l + (ri.max - k);
  for (i=l; ic<=k; i++){
    if (LE(x[i],v)){
      if (EQ(x[i],v))
        x[ic++] = x[i];
      else
        x[il++] = x[i];
    }else{
      aux[ir++] = x[i];
    }
  }
  // number of median ties in lower part is exhausted, from here median ties go to upper part
  ic = l;
  for (; i<=r ;i++){
    if (LT(x[i],v)){
      x[il++] = x[i];
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
