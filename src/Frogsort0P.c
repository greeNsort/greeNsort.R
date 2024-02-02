/*
# greeNsort Frogsort0 (pointered implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "Frogmerge.h"
#include "Insertionsort_l2r.h"

static void Frogsort0P_init(ValueT *from, ValueT *to, IndexT n){
  IndexT i, ntri = n / 2;
  if (n % 2){
    to[0] = R_NegInf;
    to[2] = from[0];
    from += 1;
    to +=3;
  }
  if (ntri){
    for (i = 0; i<ntri; i++){
      to[0] = from[0];
      to[2] = from[1];
      from += 2;
      to += 3;
    }
  }
}


static void Frogsort0P_merge_right_final(
  ValueT *x
, ValueT *ldat  // pointer to left triplet of left chunk
, IndexT nl        // number of values in left data
, ValueT *rdat  // pointer to left triplet of right chunk
, IndexT nr        // number of values in right chunk
)
{
  IndexT ll = 0;
  const IndexT lr = nl - 1;
  IndexT rl = 0;
  const IndexT rr = nr - 1;
  nr = 0;  // re-using nr as write index
  if (ll<=lr){
    ValueT u = ldat[ll], v = rdat[rl];
    for(;;){
  	  if (LT(v, u)){
    		x[nr++] = v; rl++;
    		if (rl>rr)
    		  break;
    		v = rdat[rl];
  	  }else{
    		x[nr++] = u; ll++;
    		if (ll>lr)
    		  return;
    		u = ldat[ll];
  	  }
  	}
  }
  while(ll<=lr){
	x[nr++] = ldat[ll++];
  }
  return;
}

static void Frogsort0P_merge_left(
  ValueT *x // pointer to triplets
, IndexT l     // leftmost triplet
, IndexT m     // number of triplets in left stream, we must have m >= n-m
, IndexT n     // total number of triplets
)
{
  const IndexT ll = 3*l;               // left stream: left position
        IndexT lr = ll + m+m - 1;      // left stream: right position
  const IndexT rl = lr       + 1 + n;  // right stream: left position
        IndexT rr = ll + 3*n - 1;      // right stream: right position
        n = rr        - n;          // re-using n as write index
  if (rl<=rr){
    ValueT u = x[lr], v = x[rr];
    for(;;){
      if (LT(v, u)){
        x[n--] = u; lr--;
    	  if (ll>lr)
            break;
    	  u = x[lr];
      }else{
        x[n--] = v; rr--;
        if (rl>rr)
          return;
        v = x[rr];
      }
    }
  }
  while(rl<=rr){
      x[n--] = x[rr--];
  }
  return;
}

static void Frogsort0P_merge_right(
  ValueT *x // pointer to triplets
, IndexT l     // leftmost triplet
, IndexT m     // number of triplets in left stream, we must have m <= n-m
, IndexT n     // total number of triplets
)
{
        IndexT ll = 3*l;               // left stream: left position
  const IndexT lr = ll + m+m - 1;      // left stream: right position
        IndexT rl = lr       + 1 + n;  // right stream: left position
  const IndexT rr = ll + 3*n - 1;      // right stream: right position
             n = ll + n;            // re-using n as write index
  if(ll<=lr){
    ValueT u = x[ll], v = x[rl];
    for(;;){
      if (LT(v, u)){
        x[n++] = v; rl++;
        if (rl>rr)
          break;
        v = x[rl];
      }else{
        x[n++] = u; ll++;
    	  if (ll>lr)
            return;
    	  u = x[ll];
      }
    }
  }
  while(ll<=lr){
      x[n++] = x[ll++];
  }
  return;
}


#if INSERTIONSORT_LIMIT > 0

static void Frogsort0P_TunedCase_left(
  ValueT *x // pointer to working memory
, IndexT l     // leftmost triplet to sort
, IndexT n     // number of triplets to sort
){
    IndexT i=1,j=2;
    x += 3*l;
    x[i++] = x[j++];
    n += n;
    while(i<n){
      x[i++] = x[j++];
      j++;
      x[i++] = x[j++];
    }
    Insertionsort_l2r(x, 0, n-1);
}

static void Frogsort0P_TunedCase_right(
  ValueT *x // pointer to working memory
, IndexT l     // leftmost triplet to sort
, IndexT n     // number of triplets to sort
){
    IndexT i,j;
    x += 3*l;
    i = 3*n-1;
    j = i-1;
    x[--i] = x[--j];
    while(j>0){
      x[--i] = x[--j];
      --j;
      x[--i] = x[--j];
    }
    Insertionsort_l2r(x+n, 0, n+n-1);
}

#else

static void Frogsort0P_BaseCase_left(
  ValueT *x // pointer to triplet
){
  if (LT(x[2], x[0])){
    x[1] = x[0];
    x[0] = x[2];
  }else{
    x[1] = x[2];
  }
}

static void Frogsort0P_BaseCase_right(
  ValueT *x // pointer to triplet
){
  if (LT(x[2], x[0])){
    x[1] = x[2];
    x[2] = x[0];
  }else{
    x[1] = x[0];
  }
}

#endif



static void Frogsort0P_sort_right(
  ValueT *x // pointer to triplets
, IndexT l     // leftmost triplet to sort
, IndexT n     // number of triplets to sort
);

static void Frogsort0P_sort_left(
  ValueT *x // pointer to triplets
, IndexT l     // leftmost triplet to sort
, IndexT n     // number of triplets to sort
){
#if INSERTIONSORT_LIMIT > 0
  if (n <= TRIPLET_LIMIT){
    Frogsort0P_TunedCase_left(x,l,n);
  }
#else
  if (n==1){
    Frogsort0P_BaseCase_left(x+3*l);
  }
#endif
  else if(n>1){
    IndexT m = n - n/2; // Frogsort0P_merge_left must have m >= n-m
    Frogsort0P_sort_left (x, l  ,   m);
    Frogsort0P_sort_right(x, l+m, n-m);
    Frogsort0P_merge_left(x, l, m, n);
  }
}

static void Frogsort0P_sort_right(
  ValueT *x // pointer to triplets
, IndexT l     // leftmost triplet to sort
, IndexT n     // number of triplets to sort
){
#if INSERTIONSORT_LIMIT > 0
  if (n <= TRIPLET_LIMIT){
    Frogsort0P_TunedCase_right(x,l,n);
  }
#else
  if (n==1){
    Frogsort0P_BaseCase_right(x+3*l);
  }
#endif
  else if(n>1){
    IndexT m = n/2; // Frogsort0P_merge_right must have m <= n-m
    Frogsort0P_sort_left (x, l  ,   m);
    Frogsort0P_sort_right(x, l+m, n-m);
    Frogsort0P_merge_right(x, l, m, n);
  }
}


void Frogsort0P_insitu(ValueT *x, IndexT n)
{
  // Special case: up to n == 5 we switch to insertion sort because naux would be larger than n
  if (n<6){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
  IndexT rtri = n/3;
  IndexT nr = rtri*2;
  IndexT nl = n-nr;
  IndexT ltri = nl/2 + nl%2;
  ValueT *aux = (ValueT *) MALLOC(3*ltri, ValueT);
  ValueT *ori = x + (n - rtri*3);
  Frogsort0P_init(x, aux, nl);
  Frogsort0P_init(x+nl, ori, nr);
  Frogsort0P_sort_left(aux, 0, ltri);
  Frogsort0P_sort_right(ori, 0, rtri);
  Frogsort0P_merge_right_final(x, aux+nl%2, nl, ori+rtri, nr);
  FREE(aux);
  return;
}

void Frogsort0P_exsitu(ValueT *x, IndexT n)
{
  ValueT *aux;
  // Special case: up to n == 5 we switch to insertion sort because naux would be larger than n
  if (n<6){
    Insertionsort_l2r(x, 0, n-1);
    return;
  }
  IndexT ntri = n/2 + n%2;
  aux = (ValueT *) MALLOC(3*ntri, ValueT);
  Frogsort0P_init(x, aux, n);
  Frogsort0P_sort_left(aux, 0, ntri);
  if (n%2)
    aux++;
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];
  if (n%2)
    aux--;
  FREE(aux);
  return;
}

