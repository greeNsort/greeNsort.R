/*
# greeNsort Insertionsort moving Matrix-Columns by first row keys 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// insertion sorting ascending from left to right
// modifies *x

#ifndef ALREADY_DEFINED_Insertionsort_l2r_h
#define ALREADY_DEFINED_Insertionsort_l2r_h

#include "algo.h"

#undef KEY

#ifdef STABLE_TEST
#define KEY(x)((IntValueT) stable_dblkey((double) x) )
#else
#define KEY(x)(x)
#endif

// starting from left
static void MInsertionsort_l2r(
    IntValueT *x
  , IndexT l
  , IndexT r
  , IndexT m
)
{
  IndexT i, j, lm=l*m;
  IntValueT (t)[m];  // xx C99, optional in C11 Variable Length Arrays(VLA)
  //Rprintf("MInsertionsort_l2r_insitu l=%d r=%d m=%d\n", l, r, m);
  for (i=(l+1)*m; i<=r*m; i+=m){
    //Rprintf("l=%d i=%d r=%d m=%d v=%d\n", l, i, r, m, *(x+i));
    j=i;
    MMOVE(t, x+i, m);
    while (j>lm && GT(x[j-m], t[0])){
      //Rprintf("  l=%d i=%d j=%d x[j-m]=%d t[0]=%\n", l, i, j, x[j-m], t[0]);
      MMOVE(x+j, x+(j-m), m);
      j-=m;
    }
    MMOVE(x+j, t, m);
  }
}

#endif
