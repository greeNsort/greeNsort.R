/*
# greeNsort Dual-Pivot sorting (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


// version sampling 2 pivots
// and some minor improvements
static void DupisortP(ValueT *x, IndexT n) {
	ValueT lp, rp, t;
	IndexT i, j, k;



#if DUAL_PIVOT_TINY_SIZE > 0
  if (n <= DUAL_PIVOT_TINY_SIZE){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
#else

  if (n < 2)
    return;
#endif
  i = randIndex(n);
  j = randIndex(n);
  if (j==i){
    if (j>0)
      i = j-1;
    else

      j = 1;
  }
	// sorting
	if (NE(x[j],x[i])) {
    // invariant: a < lp <= b <= rp < c
    if (LT(x[i],x[j])) {
      lp = x[i];
      rp = x[j];
    }else{
      lp = x[j];
      rp = x[i];
    }
    if (j<i){
      x[i] = x[n-1];
      x[j] = x[0];
    }else{
      x[i] = x[0];
      x[j] = x[n-1];
    }
    // center part pointers


    i = 1;
    j = n - 2;
		for (k = i; k <= j; k++) {
			t = x[k];
			if (LT(t,lp)) {
				x[k] = x[i];
				x[i++] = t;
			}else if (GT(t,rp)) {
				while (GT(x[j],rp) && k < j) {
					j--;
				}
				x[k] = x[j];
				x[j--] = t;
				t = x[k];
				if (LT(t,lp)) {
					x[k] = x[i];
					x[i++] = t;
				}
			}
		}
	// center part
    // equal elements (too big center partition)

    if (j - i >= n - DUAL_PIVOT_DIST_SIZE) {
      // invariant: a < p = b < c
      for (k = i; k <= j; k++) {
        t = x[k];
        if (EQ(t,lp)) {
          x[k] = x[i];
          x[i++] = t;
        }
        else if (EQ(t,rp)) {
          x[k] = x[j];
          x[j--] = t;
          t = x[k];
          if (EQ(t,lp)) {
            x[k] = x[i];
            x[i++] = t;
          }
        }
      }
    }
    // swap and recurse
    x[0] = x[i - 1];
    x[i - 1] = lp;

    DupisortP(x, i-1);
		DupisortP(x+i, j-i+1);
    x[n-1] = x[j + 1];
    x[j + 1] = rp;

    DupisortP(x+j+2, n-j-2);
	}else{
    // invariant: a < p = b < c
    lp = x[i];
    if (j<i){
      x[i] = x[n-1];
      x[j] = x[0];
    }else{
      x[i] = x[0];
      x[j] = x[n-1];
    }
    // center part pointers
    i = 1;
    j = n - 2;
		for (k = i; k <= j; k++) {
			t = x[k];
			if (NE(t,lp)) { // we likely have ties, therefore it pays to first check !=lp instead of <lp else >lp
				if (LT(t,lp)) {
					x[k] = x[i];
					x[i++] = t;
				}else{
					while (GT(x[j],lp /* was rp */) && k < j) {
						j--;
					}
					x[k] = x[j];
					x[j--] = t;
					t = x[k];
					if (LT(t,lp)) {
						x[k] = x[i];
						x[i++] = t;
					}
				}
			}
		}
    // swap and recurse
    x[0] = x[i - 1];
    x[i - 1] = lp;

    DupisortP(x, i-1);
    x[n-1] = x[j + 1];
    x[j + 1] = lp;
    DupisortP(x+j+2, n-j-2);
	}
}


void DupisortP_insitu(ValueT *x, IndexT n)
{
  DupisortP(x, n);
}

void DupisortP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  DupisortP(aux, n);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
