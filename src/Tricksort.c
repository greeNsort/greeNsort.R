/*
# greeNsort Tricksort is Dual-Pivot sort with dead code-branches removed
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

// Tricksort improves and simplifies Dupisort
// by maintaining a better less symmetric invariant
// ties to both pivots are not projected to the same partition
// but to middle and right partition

static void Tricksort(ValueT *x, IndexT l, IndexT r) {
	ValueT lp, rp, t;
	IndexT i, j, k;
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
#ifdef SCANNED_RANGE
    scanned_range += r-l+1;
#endif
  i = l + randIndex(r - l + 1);
  j = l + randIndex(r - l + 1);
  if (j==i){
    if (j>l)
      i = j-1;
    else
      j = l+1;
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
      x[i] = x[r];
      x[j] = x[l];
    }else{
      x[i] = x[l];
      x[j] = x[r];
    }
    i = l + 1;
    j = r - 1;
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
			// }else if (GT(t,rp)) {
				// while (GT(x[j],rp) && k < j) {
					j--;
				}
				// x[k] = x[j];
				// x[j--] = t;
				// t = x[k];
				// if (LT(t,lp)) {
					// x[k] = x[i];
					// x[i++] = t;
				// }
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
    x[l] = x[i - 1];
    x[i - 1] = lp;
    Tricksort(x, l, i - 2);
		Tricksort(x, i, j);
    x[r] = x[j + 1];
    x[j + 1] = rp;
    Tricksort(x, j + 2, r);
	}else{
    // one pivot invariant: a < lp = b < c
    // i is first index right of a, j is first index left of c
    lp = x[i];
    if (j<i){
      x[i] = x[r];
      x[j] = x[l];
    }else{
      x[i] = x[l];
      x[j] = x[r];
    }
    i = l + 1;
    j = r - 1;
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
					// x[k] = x[j];
					// x[j--] = t;
					// t = x[k];
					// if (LT(t,lp)) {
						// x[k] = x[i];
						// x[i++] = t;
					// }
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
    x[l] = x[i - 1];
    x[i - 1] = lp;
    Tricksort(x, l, i - 2);
    x[r] = x[j + 1];
    x[j + 1] = lp;
    Tricksort(x, j + 2, r);
	}
}


void Tricksort_insitu(ValueT *x, IndexT n)
{
  Tricksort(x, 0, n-1);
}

void Tricksort_exsitu(ValueT *x, IndexT n)
{
	IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Tricksort(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
