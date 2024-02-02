/*
# greeNsort Duck selection algorithm B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "DuckselectB.h"

RangeIndexT DuckselectB_insitu(ValueT *x, IndexT n, IndexT k){
  return DuckselectB_TieLeft(x, 0, n-1, k);
}

RangeIndexT DuckselectB_exsitu(ValueT *x, IndexT n, IndexT k){
  RangeIndexT ret;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  for (IndexT i=0; i<n; i++)
    aux[i] = x[i];
  ret = DuckselectB_TieLeft(aux, 0, n-1, k);
  FREE(aux);
  return ret;
}
