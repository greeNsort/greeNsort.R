/*
# greeNsort direct Insertionsort for variable-sized strings
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "VInsertionsort_l2r.h"

void VInsertionsort_insitu(
  char *x
, IndexT n
, IndexT m
, IndexT b
, PerfT *p
)
{
  char *z = (char *) MALLOC(b+1, typeof(char));
  VInsertionsort_l2r(x, z+1, 1, m-1);
  FREE(z);
  p->secs = getNewSecs();
  p->size = ((m+b)*sizeof(char)) / (m*(double)sizeof(char));
}


void VInsertionsort_exsitu(
    char *x
  , IndexT n
  , IndexT m
  , IndexT b
  , PerfT *p
)
{
  IndexT i;
  char *y = (char *) MALLOC(m, typeof(char));
  char *z = (char *) MALLOC(b, typeof(char));
  // copies also the leading \0 in x[0]
  for (i=0;i<m;i++){
    y[i] = x[i];
  }
  VInsertionsort_l2r(y, z, 1, m-1);
  for (i=1;i<m;i++){
    x[i] = y[i];
  }
  FREE(z);
  FREE(y);
  p->secs = getNewSecs();
  p->size = ((m+b)*sizeof(char)) / (m*(double)sizeof(char));
}
