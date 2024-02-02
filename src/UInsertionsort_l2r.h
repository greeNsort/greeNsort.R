/*
# greeNsort indirect Insertionsort for variable-sized strings
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// insertion sorting ascending from left to right
// modifies *x

#ifndef ALREADY_DEFINED_UInsertionsort_l2r_h
#define ALREADY_DEFINED_UInsertionsort_l2r_h

#include "algo.h"
#include <stdbool.h>

#undef LT
#undef LE
#undef GT
#undef GE
#undef EQ
#undef NE

#define LT(a,b)(STRCMP(a,b) < 0)
#define LE(a,b)(STRCMP(a,b) <= 0)
#define GT(a,b)(STRCMP(a,b) > 0)
#define GE(a,b)(STRCMP(a,b) >= 0)
#define EQ(a,b)(STRCMP(a,b) == 0)
#define NE(a,b)(STRCMP(a,b) != 0)


static void UInsertionsort_l2r(
    char **x
  , IndexT l
  , IndexT r
)
{
  IndexT i;
  char* t;
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
