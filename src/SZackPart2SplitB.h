/*
# greeNsort stable binary buffer Partitioning (B-tuned xx ??) 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_SZackPart2SplitB_h
#define ALREADY_DEFINED_SZackPart2SplitB_h

#include "ZuckselectB.h"

// stably creating two partitions
// putting l..k to x[l..] and (k+1)..r to aux[l..]
// ties with sorted(x)[k] can go to both partitions
static RangeIndexT SZackPart2SplitB(ValueT *x, ValueT *aux, IndexT l, IndexT r, IndexT k){
  IndexT i, il, ic, ir;
  // selecting is unstable, hence we must select on a copy
  for (i=l;i<=r;i++)
    aux[i] = x[i];
  RangeIndexT ri = ZuckselectB_TieLeft(aux, l, r, k);
  ValueT v = aux[k];
  // determine number of median ties allowed in lower part (note that we don't have full ordering and therefor need to search through the complete lower part)
  // for (i=k-1,nmt=1; i>=l; i--)
    // if (EQ(aux[i],v))
      // nmt++;
  // using ZackselectB allows to determine the number of median ties allowed in lower part without cost of 50% scan
  // stably partition putting ties to lower part
  il = l;
  ic = ri.min; // just used as tie counter for loop termination
  ir = l;
  for (i=l; ic<=k; i++){
    LE(x[i],v) ? (x[il++] = x[i]) : (aux[ir++] = x[i]);
    EQ(x[i],v) ? ic++ : 0;
  }
  // number of median ties in lower part is exhausted, from here median ties go to upper part
  for (; i<=r ;i++){
    LT(x[i],v) ? (x[il++] = x[i]) : (aux[ir++] = x[i]);
  }
  return ri;
}


#endif
