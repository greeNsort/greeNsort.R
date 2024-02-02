/*
# greeNsort Selectionsort2
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include <math.h>

IndexT Selectionsort2(double *x, IndexT l, IndexT r){
  IndexT m = 1;
  ValueT t, t2;
  IndexT i,j,k,ilo,ihi,n = r - l + 1;
  if (n>2){
    IndexT s = round(sqrt((double) n));
    m = n/s;
    IndexT ns = n%s;
    m += ns>0 ? 1 : 0;
    IndexT *hi = (IndexT *) MALLOC(m, IndexT);
    // initialize m slices
    hi[0] = l+s-1;
    for (i=1;i<(m-1);i++){
      hi[i] = hi[i-1] + s;
    }
    hi[m-1] = r;
    for (i=0;i<m;i++){
      // slice max
      ilo = i==0 ? 0 : hi[i-1]+1;
      ihi = hi[i];
      j = ilo;
      t = x[j];
      while(ilo < ihi){
        ilo++;
        if (GT(x[ilo], t)){
          j = ilo;
          t = x[j];
        }
      }
      if (j!=ihi){
        t2 = x[ihi];
        x[ihi] = t;
        x[j] = t2;
      }
    }
    // main double loop
    k = r; // quiet compiler
    for (i= m-1; i>0; i--){
      for (k=hi[i]; k>= hi[i-1]+1; k--){
        ilo = 0;
        ihi = i;
        j = ihi;
        t = x[k];
        while(ilo < ihi){
          ihi--;
          if (GT(x[hi[ihi]], t)){
            t = x[hi[ihi]];
            j = ihi;
          }
        }
        //print(c(j=j, i=i))
        // if j==i the max is already where it should be
        if (j!=i){
          // max is of one of the others
          if (k==hi[i-1]+1){
            // the last one of this slice
            t2 = x[k];
          }else{
            // still others under hi[i], rescue known max of i to avoid updating slice i
            t2 = x[k-1];
            x[k-1] = x[k];
          }
          // write max
          x[k] = t;  // x[hi[j]]
          // slice i gets new value, does not shrink
          x[hi[j]] = t2;
        }
        hi[i] = k - 1;
        // update slice j if it has more than one element (can happen only to slice i)
        ilo = j==0 ? 0 : hi[j-1]+1;
        ihi = hi[j];
        if (ilo < ihi){
          j = ilo;
          t = x[ilo];
          while(ilo < ihi){
            ilo++;
            if (GT(x[ilo], t)){
              t = x[ilo];
              j = ilo;
            }
          }
          if (j!=ihi){
            t2 = x[ihi];
            x[ihi] = t;
            x[j] = t2;
          }
        }
      }
    }
    // the lst element of the first slice is the max hence in place
    i = k-1;
    FREE(hi);
  }else{
    i = r;
  }
  if (i > l)
  for (; i > l; i--){
    j = 0;
    k = j;
    t = x[j];
    while(j < i){
      j++;
      if (GT(x[j], t)){
        t = x[j];
        k = j;
      }
    }
    t2 = x[i];
    x[i] = t;
    x[k] = t2;
  }
  return m;
}





void Selectionsort2_insitu(ValueT *x, IndexT n, PerfT *p)
{
  IndexT m = Selectionsort2(x, 0, n-1);
  p->secs = getNewSecs();
  p->size = (n*sizeof(ValueT)+m*sizeof(IndexT))/((double) n*sizeof(ValueT));
}

void Selectionsort2_exsitu(ValueT *x, IndexT n, PerfT *p)
{
  IndexT i;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  double s1 = getNewSecs();
  IndexT m = Selectionsort2(aux, 0, n-1);
  double s2 = getDeltaSecs();
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
  double s3 = getDeltaSecs();
  p->secs = s1+s2+s3;
  p->size = (p->secs*2*n*sizeof(ValueT)+s2*m*sizeof(IndexT))/((double) n*sizeof(ValueT));
}
