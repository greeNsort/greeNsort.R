/*
# greeNsort C-version of dotnet partial sorting algorithm
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static void PartialQuickSort(ValueT *x, /*IndexT n, */ IndexT l, IndexT r, IndexT lpart, IndexT rpart){
  IndexT i, j;
  ValueT t, v;
  do {
    i = l;
    j = r;
    //originally deterministic:
    //v = x[i + ((j - i) / 2)];
    //here with random pivot for fair comparison and security
    v = x[l+randIndex(r-l+1)];
    do
    {
      //while (i <= n && LT(x[i], v))  // using n instead of r here is wrong, this only works because of sentinel stop, but then comparing i <= n is a waste: inefficient QED
        while (i <= r && LT(x[i], v))
        {
        i++;
      }

      //while (j >= 0 && GT(x[j], v))  // using 0 instead of l here is wrong, this only works because of sentinel stop, but then comparing j >= 0 is a waste: inefficient QED
        while (j >= l && GT(x[j], v))
        {
        j--;
      }

      if (i > j)
      {
        break;
      }

      if (i < j)
      {
        SWAP(x[i], x[j], t);
      }

      i++;
      j--;
    }
    while (i <= j);

    // why only narrow on one side??

    if (lpart >= i)
    {
      l = i + 1;
    }
    else if (rpart <= j)
    {
      r = j - 1;
    }

    if (j - l <= r - i)
    {
      if (l < j)
      {
        PartialQuickSort(x, /*n,*/ l, j, lpart, rpart);
      }

      l = i;
    }
    else
    {
      if (i < r)
      {
        PartialQuickSort(x, /*n,*/ i, r, lpart, rpart);
      }

      r = j;
    }
  }
  while (l < r);
}



void Dotnetpart_insitu(ValueT *x, IndexT n, IndexT *partial)
{
  PartialQuickSort(x, /*n,*/ 0, n-1, partial[0]-1, partial[1]-1);
}

void Dotnetpart_exsitu(ValueT *x, IndexT n, IndexT *partial)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  PartialQuickSort(aux, /*n,*/ 0, n-1, partial[0]-1, partial[1]-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
