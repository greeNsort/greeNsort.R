/*
# greeNsort indirect stabilized Insertionsort for variable-sized strings
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// insertion sorting ascending from left to right
// modifies *x

#ifndef ALREADY_DEFINED_WInsertionsort_l2r_h
#define ALREADY_DEFINED_WInsertionsort_l2r_h

#include "algo.h"
#include <stdbool.h>

#undef LT
#undef LE
#undef GT
#undef GE
#undef EQ
#undef NE

/*
#define GT(a,b)( (STRCMP(a,b)> 0) || ( (STRCMP(a,b)==0) && (a> b)) )
#define GE(a,b)( (STRCMP(a,b)> 0) || ( (STRCMP(a,b)==0) && (a>=b)) )
#define LT(a,b)( (STRCMP(a,b)< 0) || ( (STRCMP(a,b)==0) && (a< b)) )
#define LE(a,b)( (STRCMP(a,b)< 0) || ( (STRCMP(a,b)==0) && (a<=b)) )
#define EQ(a,b)( (STRCMP(a,b)==0) && (a==b) )
#define NE(a,b)( (STRCMP(a,b)!=0) || (a!=b) )
 */

// coding these as functions allows to use them with inline increments like LE(x[++i], v) without multiple incrementing
static inline bool GT(char *a, char *b){
  int c = STRCMP(a,b);
  return c> 0 || ( c==0 && a> b);
}
static inline bool GE(char *a, char *b){
  int c = STRCMP(a,b);
  return c> 0 || ( c==0 && a>=b);
}
static inline bool LT(char *a, char *b){
  int c = STRCMP(a,b);
  return c< 0 || ( c==0 && a< b);
}
static inline bool LE(char *a, char *b){
  int c = STRCMP(a,b);
  return c< 0 || ( c==0 && a<=b);
}
static inline bool EQ(char *a, char *b){
  return a==b;
}
static inline bool NE(char *a, char *b){
  return a!=b;
}


static void WInsertionsort_l2r(
    char **x
  , IndexT l
  , IndexT r
)
{
  IndexT i,j;
  char* t;
  for (i=l+1;i<=r;i++){
    j=i;
    MOVE(t, x[i]);
      while (j>l && GT(x[j-1], t)){
        MOVE(x[j], x[j-1]);
        j--;
      }
      MOVE(x[j], t);
  }
}

#endif
