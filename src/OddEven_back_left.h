/*
# greeNsort gapped wrapup
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_OddEven_back_left_h
#define ALREADY_DEFINED_OddEven_back_left_h

#include "algo.h"

// copy elements even positions in databuffer to data
// data and databuffer may overlap: left-aligned
static void OddEven_back_left(ValueT *data, ValueT *databuffer, IndexT n){
  IndexT i,i2;
  for (i = 0, i2=0; i<n; i++, INC2(i2)){
    data[i] = databuffer[i2];
  }
}

#endif
