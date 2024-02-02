/*
# greeNsort Duck selection algorithm
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "Duckselect.h"

RangeIndexT Duckselect_insitu(ValueT *x, IndexT n, IndexT k){
  return Duckselect_TieLeft(x, 0, n-1, k);
}

RangeIndexT Duckselect_exsitu(ValueT *x, IndexT n, IndexT k){
  RangeIndexT ret;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  for (IndexT i=0; i<n; i++)
    aux[i] = x[i];
  ret = Duckselect_TieLeft(aux, 0, n-1, k);
  FREE(aux);
  return ret;
}
