/*
# greeNsort Tricksort is Dual-Pivot sort with dead code-branches removed (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

// Tricksort imroves and simplifies dupisort
// by maintaining a better less symmetric invariant
// ties to both pivots are not projected to the same partition
// but to middle and right partition

static void TricksortP(ValueT *x, IndexT n) {
	ValueT lp, rp, t;
	IndexT i, j, k;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += n;
#endif
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
#else
  if (n < 2)
    return;
#endif
#ifdef SCANNED_RANGE
    scanned_range += n;
#endif
  i = randIndex(n);
  j = randIndex(n);
  if (j==i){
    if (j>0)
      i = j-1;
    else
      j = 1;
  }
	if (NE(x[j],x[i])) {
    // two pivot invariant: a < lp <= b < rp <= c
    // i is first index right of a, j is first index left of c
    if (LT(x[j],x[i])) {
      lp = x[j];
      rp = x[i];
    }else{
      lp = x[i];
      rp = x[j];
    }
    if (j<i){
      x[i] = x[n - 1];
      x[j] = x[0];
    }else{
      x[i] = x[0];
      x[j] = x[n - 1];
    }
    j = n - 2;
    i = 1;
    // pre loop speeds up ascending, descending
    // while(i<=j && LT(x[i],lp))
      // i++;
		for (k = i; k <= j; k++) {
			t = x[k];
			if (LT(t,lp)) {
				x[k] = x[i];
				x[i++] = t;
			}else if (GE(t,rp)) {
				while (GE(x[j],rp) && k < j) {
					j--;
				}
         if (LT(x[j],lp)) {
           x[k] = x[i];
           x[i++] = x[j];
           x[j--] = t;
         }else{
           x[k] = x[j];
           x[j--] = t;
         }
			}
		}
    // swap and recurse
    x[0] = x[i - 1];
    x[i - 1] = lp;
    TricksortP(x, i - 1);
		TricksortP(x + i, j - i + 1);
    x[n - 1] = x[j + 1];
    x[j + 1] = rp;
    TricksortP(x + j + 2, n - j - 2);
	}else{
    // one pivot invariant: a < lp = b < c
    // i is first index right of a, j is first index left of c
    lp = x[i];
    if (j<i){
      x[i] = x[n - 1];
      x[j] = x[0];
    }else{
      x[i] = x[0];
      x[j] = x[n - 1];
    }
    j = n - 2;
    i = 1;
    // pre loop speeds up ascending, descending
    // while(i<=j && LT(x[i],lp))
      // i++;
		for (k = i; k <= j; k++) {
			t = x[k];
			if (NE(t,lp)) { // we likely have ties, therefore it pays to first check !=lp instead of <lp else >lp
				if (LT(t,lp)) {
					x[k] = x[i];
					x[i++] = t;
				}else{
					while (GT(x[j],lp) && k < j) {
						j--;
					}
          if (LT(x[j],lp)) {
             x[k] = x[i];
             x[i++] = x[j];
             x[j--] = t;
         }else{
           x[k] = x[j];
           x[j--] = t;
         }
				}
			}
		}
    // swap and recurse
    x[0] = x[i - 1];
    x[i - 1] = lp;
    TricksortP(x, i - 1);
    x[n - 1] = x[j + 1];
    x[j + 1] = lp;
    TricksortP(x + j + 2, n - j - 2);
	}
}

void TricksortP_insitu(ValueT *x, IndexT n)
{
  TricksortP(x, n);
}

void TricksortP_exsitu(ValueT *x, IndexT n)
{
	IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  TricksortP(aux, n);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}

