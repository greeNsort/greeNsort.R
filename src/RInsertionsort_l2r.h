/*
# greeNsort indirect Insertionsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// insertion sorting ascending from left to right
// modifies *x
// moves smallest value as sentinel to the left

#ifndef ALREADY_DEFINED_RInsertionsort_l2r_h
#define ALREADY_DEFINED_RInsertionsort_l2r_h

#include "algo.h"
#include <stdbool.h>

#undef LT
#undef LE
#undef GT
#undef GE
#undef EQ
#undef NE

#undef LT
#undef LE
#undef GT
#undef GE
#undef EQ
#undef NE

// coding these as functions allows to use them with inline increments like LE(x[++i], v) without multiple incrementing
static inline bool GT(ValueT *a, ValueT *b){
  ValueT A = KEY(*a);
  ValueT B = KEY(*b);
  return A > B || ( A == B && a > b);
}
static inline bool GE(ValueT *a, ValueT *b){
  ValueT A = KEY(*a);
  ValueT B = KEY(*b);
  return A > B || ( A == B && a >= b);
}
static inline bool LT(ValueT *a, ValueT *b){
  ValueT A = KEY(*a);
  ValueT B = KEY(*b);
  return A < B || ( A == B && a < b);
}
static inline bool LE(ValueT *a, ValueT *b){
  ValueT A = KEY(*a);
  ValueT B = KEY(*b);
  return A < B || ( A == B && a <= b);
}
static inline bool EQ(ValueT *a, ValueT *b){
  return a == b;
}
static inline bool NE(ValueT *a, ValueT *b){
  return a != b;
}

static void RInsertionsort_l2r(
    ValueT **x
  , IndexT l
  , IndexT r
)
{
  IndexT i;
  ValueT *t;
  for (i=r;i>l;i--){
    if (LT(x[i],x[i-1])) {
      SWAP(x[i-1],x[i],t);
    }
  }
  for (i=l+2;i<=r;i++){
    IndexT j=i;
    MOVE(t, x[i]);
      while (LT(t,x[j-1])){
        MOVE(x[j], x[j-1]);
        j--;
      }
      MOVE(x[j], t);
  }
}

#endif

