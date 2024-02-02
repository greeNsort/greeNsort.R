/*
# greeNsort Pick selection 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "Pickselect.h"

ValueT Pickselect_insitu(ValueT *x, IndexT n, IndexT k){
  return Pickselect(x, 0, n-1, k);
}

ValueT Pickselect_exsitu(ValueT *x, IndexT n, IndexT k){
  ValueT ret;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  for (IndexT i=0; i<n; i++)
    aux[i] = x[i];
  ret = Pickselect(aux, 0, n-1, k);
  FREE(aux);
  return ret;
}
