/*
# greeNsort gluecode for Astrelin's Grailsort and Grailsqrt
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"

#define SORT_TYPE double

typedef int bool;
#define true 1
#define false 0

#undef K
#undef K1
#undef KK

#undef KEY
#ifdef STABLE_TEST
#define   KEY(x)stable_dblkey((x)[0])
#else
#define   KEY(x)((x)[0])
#endif

/*
inline int cmp64(double *a, double *b){
	if(a[0] < b[0]) return -1;
	if(a[0] > b[0]) return 1;
	return 0;
}
#define SORT_CMP cmp64
*/

#undef LT
#undef LE
#undef GT
#undef GE
#undef EQ
#undef NE

#define LT(A,B) (KEY(A) < KEY(B))
#define LE(A,B) (KEY(A) <= KEY(B))
#define GT(A, B) LT((B), (A))
#define GE(A, B) LE((B), (A))
#define EQ(A,B) (KEY(A) == KEY(B))
#define NE(A,B) (KEY(A) != KEY(B))

#define SORT_CMP(a,b)( (KEY(a) < KEY(b)) ? (-1) : ( (KEY(a) > KEY(b)) ? (1) : ( 0 ) ) )

#include "GrailsortP.h"


void GrailsortP_insitu(double *x, IndexT n)
{
  GrailSort(x, (int) n);
}

void GrailsortP_exsitu(double *x, IndexT n)
{
  IndexT i;
  double *aux = (double *) MALLOC(n, double);
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  GrailSort(aux, (int) n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}

void GrailsqrtP_insitu(double *x, IndexT n)
{
  GrailSortWithDynBuffer(x, (int) n);
  //GrailSortWithBuffer(x, (int) n);
}

void GrailsqrtP_exsitu(double *x, IndexT n)
{
  IndexT i;
  double *aux = (double *) MALLOC(n, double);
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  GrailSortWithDynBuffer(aux, (int) n);  /* 2*sqrt(n) elements buffer */
  //GrailSortWithBuffer(aux, (int) n);   /* 512 elements  buffer */
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}

void InplaceMergesortP_insitu(double *x, IndexT n)
{
  RecStableSort(x, (int) n);
}

void InplaceMergesortP_exsitu(double *x, IndexT n)
{
  IndexT i;
  double *aux = (double *) MALLOC(n, double);
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  RecStableSort(aux, (int) n);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
}
