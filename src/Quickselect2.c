/*
# greeNsort partial Quickselect 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "Quickselect2.h"

ValueT Quickselect2_insitu(ValueT *x, IndexT n, IndexT k){
  return Quickselect2(x, 0, n-1, k);
}

ValueT Quickselect2_exsitu(ValueT *x, IndexT n, IndexT k){
  ValueT ret, *aux = (ValueT *) MALLOC(n, ValueT);
  for (IndexT i=0; i<n; i++)
    aux[i] = x[i];
  ret = Quickselect2(aux, 0, n-1, k);
  FREE(aux);
  return ret;
}
