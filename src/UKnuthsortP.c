/*
# greeNsort indirect Knuthsort for variable-sized strings (pointered implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "UInsertionsort_l2r.h"
#include "strtools.h"


// Following Knuth we need only one termination check per iteration
static void UKnuthsortP_merge(char **z, char **l, char **m, char **r){
  char **j=m+1;
  for (;;){
    if (LT(*j, *l)){
      *(z++) = *j++;
      if (j > r)
        break;
    }else{
      *(z++) = *l++;
      if (l > m)
        break;
    }
  }
  while(l <= m)
    *(z++) = *(l++);
  while(j <= r)
    *(z++) = *(j++);
}

static void UKnuthsortP_recurse(char **a, char **b, IndexT n){
  IndexT m;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    UInsertionsort_l2r(a, 0, n-1);
  }else
#else
    if (n>1)
#endif
  {
    m = n/2;
    UKnuthsortP_recurse(b, a, m);
    UKnuthsortP_recurse(b+m, a+m, n-m);
    UKnuthsortP_merge(a, b, b+m-1, b+n-1);
    //Uright_merge2(a, b, b+m-1, b+m, b+n-1);
  }
}


void UKnuthsortP_insitu(char *x, IndexT n, IndexT m, PerfT *p)
{
  PerfT p1,p2;
  IndexT i,s;
  char **z = (char **) MALLOC(n, typeof(char*));
  char **aux = (char **) MALLOC(n, typeof(char*));
  m = 1;
  for (i=0;i<n;i++){
    s = strlen_lr(x+m);
    aux[i] = z[i] = x+m;
    m += s;
  }
  UKnuthsortP_recurse(z, aux, n);
  FREE(aux);
  p1.secs = getNewSecs();
  p1.size = (2*n*sizeof(char*) + m*sizeof(char)) / ((double) m*sizeof(char));
  char *y = (char *) MALLOC(m, typeof(char));
  // now copy back stably
  m = 1;
  for (i=0;i<n;i++){
    m += strcpylen_lr(y+m, z[i]);
  }
  for (i=0;i<m;i++){
    x[i] = y[i];
  }
  FREE(y);
  FREE(z);
  p2.secs = getNewSecs() - p1.secs;
  p2.size = (n*sizeof(char*) + 2*m*sizeof(char)) / ((double) m*sizeof(char));
  p->secs = p1.secs + p2.secs;
  p->size= (p1.secs*p1.size + p2.secs*p2.size) / p->secs;
}


void UKnuthsortP_exsitu(char *x, IndexT n, IndexT m, PerfT *p)
{
  IndexT i,s;
  char *y = (char *) MALLOC(m, typeof(char));
  char **z = (char **) MALLOC(n, typeof(char*));
  char **aux = (char **) MALLOC(n, typeof(char*));
  m = 1;
  for (i=0;i<n;i++){
    s = strcpylen_lr(y+m, x+m);
    aux[i] = z[i] = y+m;
    m += s;
  }
  UKnuthsortP_recurse(z, aux, n);
  FREE(aux);
  // now copy back
  m = 1;
  for (i=0;i<n;i++){
    m += strcpylen_lr(x+m, z[i]);
  }
  FREE(y);
  FREE(z);
  p->secs = getNewSecs();
  p->size = (2*n*sizeof(char*) + m*sizeof(char)) / ((double) m*sizeof(char));
}
