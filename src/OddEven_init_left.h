/*
# greeNsort gapped wrapup
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_OddEven_init_left_h
#define ALREADY_DEFINED_OddEven_init_left_h

#include "algo.h"

// copy elements from data to even positions in databuffer
// data and databuffer may overlap: left-aligned
static void OddEven_init_left(ValueT *data, ValueT *databuffer, IndexT n){
  IndexT i,i2;
  for (i = n-1, i2=n+n-2; i>=0; i--, DEC2(i2)){
    databuffer[i2] = data[i];
  }
}
#endif
