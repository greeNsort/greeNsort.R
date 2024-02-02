/*
# greeNsort Dual-Pivot sorting
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
static void Dupisort(ValueT *x, IndexT l, IndexT r) {
	ValueT lp, rp, t;
	IndexT i, j, k;
#if DUAL_PIVOT_TINY_SIZE > 0
  if (r - l < DUAL_PIVOT_TINY_SIZE){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
  i = l + randIndex(r - l + 1);
  j = l + randIndex(r - l + 1);
  if (j==i){
    if (j>l)
      i = j-1;
    else
      j = l+1;
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
      x[i] = x[r];
      x[j] = x[l];
    }else{
      x[i] = x[l];
      x[j] = x[r];
    }
    // center part pointers
    i = l + 1;
    j = r - 1;
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
    if (j - i >= (r-l+1) - DUAL_PIVOT_DIST_SIZE) {
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
    x[l] = x[i - 1];
    x[i - 1] = lp;
    Dupisort(x, l, i - 2);
		Dupisort(x, i, j);
    x[r] = x[j + 1];
    x[j + 1] = rp;
    Dupisort(x, j + 2, r);
	}else{
    // invariant: a < p = b < c
    lp = x[i];
    if (j<i){
      x[i] = x[r];
      x[j] = x[l];
    }else{
      x[i] = x[l];
      x[j] = x[r];
    }
    // center part pointers
    i = l + 1;
    j = r - 1;
		for (k = i; k <= j; k++) {
			t = x[k];
			if (NE(t,lp)) { // we likely have ties, therefore it pays to first check !=lp instead of <lp else >lp
				if (LT(t,lp)) {
					x[k] = x[i];
					x[i++] = t;
				}else{
					while (GT(x[j],lp /* was rp */ ) && k < j) {
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
    x[l] = x[i - 1];
    x[i - 1] = lp;
    Dupisort(x, l, i - 2);
    x[r] = x[j + 1];
    x[j + 1] = lp;
    Dupisort(x, j + 2, r);
	}
}


#if 0==1
// Sedgewicks version: bad: has redundant swaps between elements of x>rp
static void Dupisort_Sedgewick(ValueT *x, IndexT l, IndexT r) {
	IndexT n = r - l + 1;
	ValueT lp, rp, t;
	IndexT i, j, k;
  if (n <= DUAL_PIVOT_TINY_SIZE){
    Insertionsort_l2r(x, l, r);
    return;
  }
  i = l + randIndex(n);
  j = l + randIndex(n);
  if (LT(x[i],x[j])){
    lp = x[i];
    rp = x[j];
  }else{
    lp = x[j];
    rp = x[i];
  }
  x[i] = x[r];
  x[j] = x[l];
	// center part pointers
	i = l + 1;
	j = r - 1;
  // index
  k = i;
  while (k <= j) {
      if       (LT(x[k], lp)){SWAP(x[i], x[k], t); i++; k++;}
      else if  (LT(rp, x[k])){SWAP(x[k], x[j], t); j--;}
      else k++;
  }
  x[l] = x[--i]; x[i] = lp;
  x[r] = x[++j]; x[j] = rp;
  // recurse
  Dupisort_Sedgewick(x, l, i-1);
  if (LT(x[i], x[j]))
    Dupisort_Sedgewick(x, i+1, j-1);
  Dupisort_Sedgewick(x, j+1, r);
}
#endif

#if 0==1
// one of the original Versions of dual pivot quicksort ()
// using a sorting network
// for selecting  {2,4} out of {1,2,3,4,5} deterministic pivots
static void Dupisort_orig(ValueT *x, IndexT l, IndexT r) {
	IndexT n = r - l;
	ValueT t;
	ValueT lp, rp;
	IndexT k;
	IndexT i, j;
	IndexT diffPivots;
	IndexT m1, m2, m3, m4, m5;
	IndexT sixth;
  if (n < DUAL_PIVOT_TINY_SIZE){
    Insertionsort_l2r(x, l, r);
    return;
  }
	// median indexes
	sixth = n / 6;
	m1 = l + sixth;
	m2 = m1 + sixth;
	m3 = m2 + sixth;
	m4 = m3 + sixth;
	m5 = m4 + sixth;
	// 5-element sorting network
	if (GT(x[m1] , x[m2])) { t = x[m1]; x[m1] = x[m2]; x[m2] = t; }
	if (GT(x[m4] , x[m5])) { t = x[m4]; x[m4] = x[m5]; x[m5] = t; }
	if (GT(x[m1] , x[m3])) { t = x[m1]; x[m1] = x[m3]; x[m3] = t; }
	if (GT(x[m2] , x[m3])) { t = x[m2]; x[m2] = x[m3]; x[m3] = t; }
	if (GT(x[m1] , x[m4])) { t = x[m1]; x[m1] = x[m4]; x[m4] = t; }
	if (GT(x[m3] , x[m4])) { t = x[m3]; x[m3] = x[m4]; x[m4] = t; }
	if (GT(x[m2] , x[m5])) { t = x[m2]; x[m2] = x[m5]; x[m5] = t; }
	if (GT(x[m2] , x[m3])) { t = x[m2]; x[m2] = x[m3]; x[m3] = t; }
	if (GT(x[m4] , x[m5])) { t = x[m4]; x[m4] = x[m5]; x[m5] = t; }
	// pivots: [ < lp | lp <= && <= rp | > rp ]
	lp = x[m2];
	rp = x[m4];
	diffPivots = NE(lp,rp);
	x[m2] = x[l];
	x[m4] = x[r];
	// center part pointers
	i = l + 1;
	j = r - 1;
	// sorting
	if (diffPivots) {
    // invariant: a < lp <= b <= rp < c
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
	}
	else {
    // invariant: a < p = b < c
		for (k = i; k <= j; k++) {
			t = x[k];
			if (NE(t,lp)) {
				if (t < lp) {
					x[k] = x[i];
					x[i++] = t;
				}else{
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
		}
	}
	// swap
	x[l] = x[i - 1];
	x[i - 1] = lp;
	x[r] = x[j + 1];
	x[j + 1] = rp;
	// l and r parts
	Dupisort_orig(x, l, i - 2);
	Dupisort_orig(x, j + 2, r);
	// correction pass if center partition is too big
	if (j - i > n - DUAL_PIVOT_DIST_SIZE && diffPivots) {
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
	// center part
	if (diffPivots) {
		Dupisort_orig(x, i, j);
	}
}

void Dupisort_orig_insitu(ValueT *x, IndexT n)
{
  Dupisort_orig(x, 0, n-1);
}

#endif




#if 0==1
// dito pointer version using sorting network to select {2,4} out of {1,2,3,4,5} random pivots
// compared against ducksort with median-3 pivot
// result: not substantially different, so we go with simple sampling
static void DupisortPM(ValueT *x, IndexT n) {
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

	IndexT m1, m2, m3, m4, m5;
  i = n/5;
  j = n;
	m1 = j - i + randIndex(i);
  j -= i;
	m2 = j - i + randIndex(i);
  j -= i;
	m3 = j - i + randIndex(i);
  j -= i;
	m4 = j - i + randIndex(i);
  j -= i;
	m5 = randIndex(j);
	// 5-element sorting network
	if (GT(x[m1] , x[m2])) { t = x[m1]; x[m1] = x[m2]; x[m2] = t; }
	if (GT(x[m4] , x[m5])) { t = x[m4]; x[m4] = x[m5]; x[m5] = t; }
	if (GT(x[m1] , x[m3])) { t = x[m1]; x[m1] = x[m3]; x[m3] = t; }
	if (GT(x[m2] , x[m3])) { t = x[m2]; x[m2] = x[m3]; x[m3] = t; }
	if (GT(x[m1] , x[m4])) { t = x[m1]; x[m1] = x[m4]; x[m4] = t; }
	if (GT(x[m3] , x[m4])) { t = x[m3]; x[m3] = x[m4]; x[m4] = t; }
	if (GT(x[m2] , x[m5])) { t = x[m2]; x[m2] = x[m5]; x[m5] = t; }
	if (GT(x[m2] , x[m3])) { t = x[m2]; x[m2] = x[m3]; x[m3] = t; }
	if (GT(x[m4] , x[m5])) { t = x[m4]; x[m4] = x[m5]; x[m5] = t; }
	// pivots: [ < lp | lp <= && <= rp | > rp ]
	lp = x[m2];
	rp = x[m4];
	x[m2] = x[0];
	x[m4] = x[n-1];

	// center part pointers
	i = 1;
	j = n - 2;
	// sorting
	if (NE(lp,rp)) {
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
    // equal elements (not convincing)
    if (j - i >= n - DUAL_PIVOT_DIST_SIZE) {
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
		DupisortPM(x+i, j-i+1);
	}else{
		for (k = i; k <= j; k++) {
			t = x[k];
			if (NE(t,lp)) { // we likely have ties, therefore it pays to first check !=lp instead of <lp else >lp
				if (LT(t,lp)) {
					x[k] = x[i];
					x[i++] = t;
				}else{
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
		}
	}
	// swap
	x[0] = x[i - 1];
	x[i - 1] = lp;
	x[n-1] = x[j + 1];
	x[j + 1] = rp;
	// l and r parts
	DupisortPM(x, i-1);
	DupisortPM(x+j+2, n-j-2);
}
#endif

void Dupisort_insitu(ValueT *x, IndexT n)
{
  Dupisort(x, 0, n-1);
}

void Dupisort_exsitu(ValueT *x, IndexT n)
{
	IndexT i;
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  for (i=0;i<n;i++)
    aux[i] = x[i];
  Dupisort(aux, 0, n-1);
  for (i=0;i<n;i++)
    x[i] = aux[i];
  FREE(aux);
}
