/*
# greeNsort stable binary buffer median Partitioning
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_SZickPart2Split_h
#define ALREADY_DEFINED_SZickPart2Split_h

#include "Zickselect.h"

// stably creating two partitions
// putting l..k to x[l..] and (k+1)..r to aux[l..]
// ties with sorted(x)[k] can go to both partitions
static RangeIndexT SZickPart2Split(ValueT *x, ValueT *aux, IndexT l, IndexT r, IndexT k){
  IndexT i, il, ic, ir;
  // selecting is unstable, hence we must select on a copy
  for (i=l;i<=r;i++){
    aux[i] = x[i];
  }
  // Rprintf("SZickPart2Split l=%d r=%d k=%d\n", l, r, k); R_FlushConsole();
  RangeIndexT ri = Zickselect_TieLeft(aux, l, r, k);
  ValueT v = aux[k];
  // Rprintf("SZickPart2Split min=%d max=%d v=%f\n", ri.min, ri.max, v); R_FlushConsole();
  // determine number of median ties allowed in lower part (note that we don't have full ordering and therefor need to search through the complete lower part)
  // for (i=k-1,nmt=1; i>=l; i--)
    // if (EQ(aux[i],v))
      // nmt++;
  // using Zickselect allows to determine the number of median ties allowed in lower part without cost of 50% scan
  // stably partition putting ties to lower part
  il = l;
  ic = ri.min; // just used as tie counter for loop termination
  ir = l;
  for (i=l; ic<=k; i++){
    if (LE(x[i],v)){
      x[il++] = x[i];
      if (EQ(x[i],v))
        ic++;
    }else{
      aux[ir++] = x[i];
    }
  }
  // number of median ties in lower part is exhausted, from here median ties go to upper part
  for (; i<=r ;i++){
    if (LT(x[i],v)){
      x[il++] = x[i];
    }else{
      aux[ir++] = x[i];
    }
  }
  return ri;
}


#endif
