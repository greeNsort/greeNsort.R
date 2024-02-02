/*
# greeNsort Insertionsort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"

#include "Insertionsort_l2r.h"
#include "Insertionsort_r2l.h"
#include "Insertionsort_desc_l2r.h"
#include "Insertionsort_desc_r2l.h"


void Insertionsort_l2r_insitu(ValueT *x, IndexT n)
{
  Insertionsort_l2r(x, 0, n-1);
}

void Insertionsort_l2r_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Insertionsort_l2r(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}

void Insertionsort_r2l_insitu(ValueT *x, IndexT n)
{
  Insertionsort_r2l(x, 0, n-1);
}

void Insertionsort_r2l_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Insertionsort_r2l(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}


void Insertionsort_desc_l2r_insitu(ValueT *x, IndexT n)
{
  Insertionsort_desc_l2r(x, 0, n-1);
}

void Insertionsort_desc_l2r_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Insertionsort_desc_l2r(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}


void Insertionsort_desc_r2l_insitu(ValueT *x, IndexT n)
{
  Insertionsort_desc_r2l(x, 0, n-1);
}

void Insertionsort_desc_r2l_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Insertionsort_desc_r2l(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}



