/*
# greeNsort Insertionorder
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_Insertionorder_l2r_h
#define ALREADY_DEFINED_Insertionorder_l2r_h

#include "algo.h"

// insertion ordering ascending from left to right
// modifies *o

static void Insertionorder_l2r(
  ValueT *x
, int *o
, int l
, int r
)
{
  int i,j;
  ValueT v;
  int p;
  for (i=l+1;i<=r;i++){
    j=i;
    MOVE(p, o[i]);
    MOVE(v, x[p]);
    while (j>l && GT(x[o[j-1]], v)){
      MOVE(o[j], o[j-1]);
      j--;
    }
    MOVE(o[j], p);
  }
}

#endif
