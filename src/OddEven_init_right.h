/*
# greeNsort gapped wrapup
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_OddEven_init_right_h
#define ALREADY_DEFINED_OddEven_init_right_h

#include "algo.h"

// copy elements from data to even positions in databuffer
// data and databuffer may overlap: right-aligned
static void OddEven_init_right(ValueT *data, ValueT *databuffer, IndexT n){
  IndexT i,i2;
  for (i = 0, i2=0; i<n; i++, INC2(i2)){
    databuffer[i2] = data[i];
  }
}

#endif
