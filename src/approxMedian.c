/*
# greeNsort approximate median
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "approxMedian_exsitu.h"
#include "approxMedian_insitu_left.h"

ValueT approxMedian_exsitu(ValueT *x, IndexT n){
  ValueT *buf = (ValueT *) MALLOC(n/5+n%5, ValueT);
  ValueT aux5[5];
  ValueT ret = approxMedian_exsitu_rec(x, buf, aux5, n);
  FREE(buf);
  return ret;
}
ValueT approxMedian_insitu(ValueT *x, IndexT n){
 return approxMedian_insitu_left(x, 0, n-1);
}
