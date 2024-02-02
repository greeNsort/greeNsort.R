/*
# greeNsort direct Insertionsort for variable-sized strings
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// insertion sorting ascending
// modifies *x
// begins with moving the smallest value as sentinel to the right

#ifndef ALREADY_DEFINED_VInsertionsort_l2r_h
#define ALREADY_DEFINED_VInsertionsort_l2r_h

#include "algo.h"
#include "strtools.h"

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

static void VInsertionsort_l2r(
    char *x // sentinel x[l-1] == \0
  , char *b // leftmost char of buffer
  , IndexT L   // leftmost char  x[l]
  , IndexT R   // rightmost char x[r] == \0
)
{
  //assert(x[L-1]=='\0');
  assert(x[R]=='\0');
  assert(L <= R);
  if (R<L)
    return;

  IndexT l,r,ln,rn,i;

  l = L;
  ln = strlen_lr(x+l);
  r = l+ln-1;
  if (r>R)
    return;

  l = strbegin(x,R);
  while (l > L){
    r = l;
    l = strprev(x,l);
    ln = strcpylen_lr(b,x+l);
    i = l;
    while(r<R && GT(b,x+r)){
      rn = strcpylen_lr(x+i,x+r);
      i+=rn;
      r+=rn;
    }
    strcpy_lr(x+i, b, ln);
  }

}

#endif
