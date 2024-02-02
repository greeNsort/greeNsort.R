/*
# greeNsort Insertionorder
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"

#include "Insertionorder_l2r.h"
#include "Insertionorder_r2l.h"

void Insertionorder_l2r_insitu(ValueT *x, int *o, int n)
{
  Insertionorder_l2r(x, o, 0, n-1);
}

void Insertionorder_l2r_exsitu(ValueT *x, int *o, int n)
{
  int i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  int * aux2= (int *) MALLOC(n, int);
  for (i=0;i<n;i++){
    aux[i] = x[i];
    aux2[i] = o[i];
  }
  Insertionorder_l2r(aux, aux2, 0, n-1);
  for (i=0;i<n;i++){
    x[i] = aux[i];
    o[i] = aux2[i];
  }
}


void Insertionorder_r2l_insitu(ValueT *x, int *o, int n)
{
  Insertionorder_r2l(x, o, 0, n-1);
}

void Insertionorder_r2l_exsitu(ValueT *x, int *o, int n)
{
  int i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  int * aux2= (int *) MALLOC(n, int);
  for (i=0;i<n;i++){
    aux[i] = x[i];
    aux2[i] = o[i];
  }
  Insertionorder_r2l(aux, aux2, 0, n-1);
  for (i=0;i<n;i++){
    x[i] = aux[i];
    o[i] = aux2[i];
  }
}
