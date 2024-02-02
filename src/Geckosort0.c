/*
# greeNsort Geckosort0
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "Insertionsort_l2r.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_r2l.h"
#endif

static void Geckosort0_init(ValueT *from, ValueT *to, IndexT n){
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

static void Geckosort0_merge_right_final(
  ValueT *x
, ValueT *ldat  // pointer to left triplet of left chunk
, IndexT nl        // number of values in left data
, ValueT *rdat  // pointer to left triplet of right chunk
, IndexT nr        // number of values in right chunk
)
{
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  IndexT rl = 0;
  const IndexT rr = nr - 1;
  nr = 0;  // re-using nr as write index
  if (ll<=lr){
    ValueT u = ldat[lr], v = rdat[rl];
    for(;;){
      if (LT(v, u)){
        x[nr++] = v; rl++;
    	  if (rl>rr)
    	    break;
    	  v = rdat[rl];
      }else{
        x[nr++] = u; lr--;
    	  if (ll>lr)
      		return;
    	  u = ldat[lr];
      }
    }
  }
  while(ll<=lr){
	  x[nr++] = ldat[lr--];
  }
  return;
}

static void Geckosort0_merge_left(
  ValueT *x // pointer to triplets
, IndexT l     // leftmost triplet
, IndexT m     // number of triplets in left stream, we must have m >= n-m
, IndexT n     // total number of triplets
)
{
  const IndexT ll = 3*l;               // left stream: left position
        IndexT lr = ll + m+m - 1;      // left stream: right position
        IndexT rl = lr       + 1 + n;  // right stream: left position
  const IndexT rr = ll + 3*n - 1;      // right stream: right position
        n = rr        - n;          // re-using n as write index
  if (rl<=rr){
    ValueT u = x[lr], v = x[rl];
    for(;;){
      if (GE(v, u)){
        x[n--] = u; lr--;
  	    if (ll>lr)
          break;
    	  u = x[lr];
      }else{
        x[n--] = v; rl++;
        if (rl>rr)
          return;
        v = x[rl];
      }
    }
  }
  while(rl<=rr){
    x[n--] = x[rl++];
  }
  return;
}
static void Geckosort0_merge_right(
  ValueT *x // pointer to triplets
, IndexT l     // leftmost triplet
, IndexT m     // number of triplets in left stream, we must have m <= n-m
, IndexT n     // total number of triplets
)
{
  const IndexT ll = 3*l;               // left stream: left position
        IndexT lr = ll + m+m - 1;      // left stream: right position
        IndexT rl = lr       + 1 + n;  // right stream: left position
  const IndexT rr = ll + 3*n - 1;      // right stream: right position
             n = ll + n;            // re-using n as write index
  if(ll<=lr){
    ValueT u = x[lr], v = x[rl];
    for(;;){
      if (LT(v, u)){
        x[n++] = v; rl++;
        if (rl>rr)
          break;
        v = x[rl];
      }else{
        x[n++] = u; lr--;
  	    if (ll>lr)
          return;
  	    u = x[lr];
      }
    }
  }
  while(ll<=lr){
    x[n++] = x[lr--];
  }
  return;
}

#if INSERTIONSORT_LIMIT > 0

static void Geckosort0_TunedCase_Left(
  ValueT *x // pointer to triplet
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
    Insertionsort_r2l(x, 0, n-1);
}

static void Geckosort0_TunedCase_right(
  ValueT *x // pointer to triplet
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

static void Geckosort0_BaseCase_left(
  ValueT *x // pointer to triplet
){
  if (GE(x[2], x[0])){
    x[1] = x[0];
    x[0] = x[2];
  }else{
    //x[0] = x[0];
    x[1] = x[2];
  }
}

static void Geckosort0_BaseCase_right(
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


static void Geckosort0_sort_right(
  ValueT *x // pointer to triplets
, IndexT l     // leftmost triplet to sort
, IndexT n     // number of triplets to sort
);

static void Geckosort0_sort_left(
  ValueT *x // pointer to triplets
, IndexT l     // leftmost triplet to sort
, IndexT n     // number of triplets to sort
){
#if INSERTIONSORT_LIMIT > 0
  if (n <= TRIPLET_LIMIT){
    Geckosort0_TunedCase_Left(x,l,n);
#else
  if (n==1){
    Geckosort0_BaseCase_left(x+3*l);
#endif
  }else{
    IndexT m = n - n/2; // Geckosort0_merge_left must have m >= n-m
    Geckosort0_sort_left (x, l  ,   m);
    Geckosort0_sort_right(x, l+m, n-m);
    Geckosort0_merge_left(x, l, m, n);
  }
}

static void Geckosort0_sort_right(
  ValueT *x // pointer to triplets
, IndexT l     // leftmost triplet to sort
, IndexT n     // number of triplets to sort
){
#if INSERTIONSORT_LIMIT > 0
  if (n <= TRIPLET_LIMIT){
    Geckosort0_TunedCase_right(x,l,n);
#else
  if (n==1){
    Geckosort0_BaseCase_right(x+3*l);
#endif
  }else{
    IndexT m = n/2; // Geckosort0_merge_right must have m <= n-m
    Geckosort0_sort_right(x, l+m, n-m);
    Geckosort0_sort_left (x, l  ,   m);
    Geckosort0_merge_right(x, l, m, n);
  }
}


void Geckosort0_insitu(ValueT *x, IndexT n)
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

  ValueT *ori;
  ori = x + (n - rtri*3);

  Geckosort0_init(x, aux, nl);
  Geckosort0_init(x+nl, ori, nr);
  Geckosort0_sort_right(ori, 0, rtri);
  Geckosort0_sort_left(aux, 0, ltri);
  Geckosort0_merge_right_final(x, aux, nl, ori+rtri, nr);
  FREE(aux);
  return;
}
void Geckosort0_exsitu(ValueT *x, IndexT n)
{
  ValueT *aux;
  // Special case: up to n == 5 we switch to insertion sort because naux would be larger than n
  if (n<6){
    aux = (ValueT *) MALLOC(n, ValueT);
    for (IndexT i=0; i<n; i++)
      aux[i] = x[i];
    Insertionsort_l2r(aux, 0, n-1);
    for (IndexT i=0; i<n; i++)
      x[i] = aux[i];
    FREE(aux);
    return;
  }
  IndexT ntri = n/2 + n%2;
  aux = (ValueT *) MALLOC(3*ntri, ValueT);
  Geckosort0_init(x, aux, n);
  Geckosort0_sort_right(aux, 0, ntri);
  aux += 3*ntri-n;
  for (IndexT i=0; i<n; i++)
    x[i] = aux[i];
  aux -= 3*ntri-n;
  FREE(aux);
  return;
}
