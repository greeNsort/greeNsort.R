/*
# greeNsort gluecode for Astrelin's Sqrtsort
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

#include "SqrtsortP.h"


void SqrtsortP_insitu(double *x, IndexT n, PerfT *p)
{
  SqrtSort(x, (int) n, p);
  p->secs = getNewSecs();
}

void SqrtsortP_exsitu(double *x, IndexT n, PerfT *p)
{
  int i;
  double *aux = (double *) MALLOC(n, double);
  for (i = 0; i < n; i++){
    aux[i] = x[i];
  }
  SqrtSort(aux, (int) n, p);
  for (i=0; i<n; i++)
    x[i] = aux[i];
  FREE(aux);
  p->secs = getNewSecs();
}
