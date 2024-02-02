/*
# greeNsort indirect stabilized Insertionsort for variable-sized strings
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "WInsertionsort_l2r.h"
#include "strtools.h"

void WInsertionsort_insitu(
  char *x
, IndexT n
, IndexT m
, PerfT *p
)
{
  PerfT p1,p2;
  IndexT i,s;
  char **z = (char **) MALLOC(n, typeof(char*));
  m = 1;
  for (i=0;i<n;i++){
    s = strlen_lr(x+m);
    z[i] = x+m;
    m += s;
  }
  WInsertionsort_l2r(z, 0, n-1);
  p1.secs = getNewSecs();
  p1.size = (n*sizeof(char*) + m*sizeof(char)) / ((double) m*sizeof(char));

  // reorder using extra buffer is faster and much easier for size varying elements
  char *y = (char *) MALLOC(m, typeof(char));
  m = 1;
  for (i=0;i<n;i++){
    m += strcpylen_lr(y+m, z[i]);
  }
  for (i=1;i<m;i++){
    x[i] = y[i];
  }
  FREE(y);

  FREE(z);
  p2.secs = getNewSecs() - p1.secs;
  p2.size = (n*sizeof(char*) + 2*m*sizeof(char)) / ((double) m*sizeof(char));

  p->secs = p1.secs + p2.secs;
  p->size= (p1.secs*p1.size + p2.secs*p2.size) / p->secs;
}


void WInsertionsort_exsitu(
    char *x
  , IndexT n
  , IndexT m
  , PerfT *p
)
{
  IndexT i,s;
  char *y = (char *) MALLOC(m, typeof(char));
  char **z = (char **) MALLOC(n, typeof(char*));
  m = 1;
  for (i=0;i<n;i++){
    s = strcpylen_lr(y+m, x+m);
    z[i] = y+m;
    m += s;
  }
  WInsertionsort_l2r(z, 0, n-1);
  // now copy back
  m = 1;
  for (i=0;i<n;i++){
    m += strcpylen_lr(x+m, z[i]);
  }
  FREE(y);
  FREE(z);
  p->secs = getNewSecs();
  p->size = (n*sizeof(char*) + m*sizeof(char)) / ((double) m*sizeof(char));
}
