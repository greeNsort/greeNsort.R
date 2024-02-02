/*
# greeNsort gapped wrapup
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_OddEven_back_right_h
#define ALREADY_DEFINED_OddEven_back_right_h

#include "algo.h"

// copy elements even positions in databuffer to data
// data and databuffer may overlap: right-aligned
static void OddEven_back_right(ValueT *data, ValueT *databuffer, IndexT n){
  IndexT i,i2;
  for (i = n-1, i2=n+n-2; i>=0; i--, DEC2(i2)){
    data[i] = databuffer[i2];
  }
}

#endif
