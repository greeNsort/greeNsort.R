/*
# greeNsort common Gecko merging
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_Geckomerge_h
#define ALREADY_DEFINED_Geckomerge_h

#include "algo.h"

static void Geckomerge_asc_right_full(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT rr = nr - 1;
  IndexT rl = 0;
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  nr = 0;  // re-using nr as write index
  ValueT u=ldat[lr], v=rdat[rl];
  // simple tuning for perfectly presorted
  //if (GT(ldat[ll], rdat[rl]))
  for(;;){
	// Rprintf("merge nr=%d ll=%d lr=%d rl=%d rr=%d\n", nr, ll, lr, rl, rr);
    if (LE(u, v)){
      x[nr++] = u; lr--;
	    if (ll>lr)
        break;
	    u = ldat[lr];
    }else{
      x[nr++] = v; rl++;
      if (rl>rr)
        break;
      v = rdat[rl];
    }
  }
  while(ll<=lr){
	// Rprintf("final nr=%d ll=%d lr=%d rl=%d rr=%d\n", nr, ll, lr, rl, rr);
    x[nr++] = ldat[lr--];
  }
  while(rl<=rr){
	// Rprintf("final nr=%d ll=%d lr=%d rl=%d rr=%d\n", nr, ll, lr, rl, rr);
    x[nr++] = rdat[rl++];
  }
  return;
}
static void Geckomerge_asc_right_final(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT rr = nr - 1;
  IndexT rl = 0;
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  nr = 0;  // re-using nr as write index
  ValueT u=ldat[lr], v=rdat[rl];
  // simple tuning for perfectly presorted
  //if (GT(ldat[ll], rdat[rl]))
  for(;;){
    if (LE(u, v)){
      x[nr++] = u; lr--;
      if (ll>lr)
        return;
      u = ldat[lr];
    }else{
      x[nr++] = v; rl++;
      if (rl>rr)
        break;
      v = rdat[rl];
    }
  }
  while(ll<=lr){
    x[nr++] = ldat[lr--];
  }
  return;
}
static void Geckomerge_asc_right_stable(
  ValueT *x
, IndexT nl  // == b
, IndexT nr
, IndexT R
){
  const IndexT rr = R;
  IndexT rl = R - nr + 1;
  IndexT lr = rl - nl - 1;
  const IndexT ll = lr - nl + 1;
  nr  = lr + 1;  // re-using nr as write-index
  ValueT u=x[lr], v=x[rl];
  // simple tuning for perfectly presorted
  //if (GT(x[ll], x[rl]))
  for(;;){
    if (LE(u, v)){
      x[nr++] = u; lr--;
      if (ll>lr)
        return;
      u = x[lr];
    }else{
      x[nr++] = v; rl++;
      if (rl>rr)
        break;
      v = x[rl];
    }
  }
  while(ll<=lr){
      x[nr++] = x[lr--];
  }
  return;
}
/*
static void Geckomerge_asc_right_reverse(
  ValueT *x
, IndexT nl  // == b
, IndexT nr
, IndexT R
){
  const IndexT rr = R;
  IndexT rl = R - nr + 1;
  IndexT lr = rl - nl - 1;
  const IndexT ll = lr - nl + 1;
  nr  = lr + 1;  // re-using nr as write-index
  // simple tuning for perfectly presorted
  //if (GE(x[ll], x[rl]))
  for(;;){
    if (LT(x[lr], x[rl])){
      x[nr++] = x[lr--];
      if (ll>lr)
        return;
    }else{
      x[nr++] = x[rl++];
      if (rl>rr)
        break;
    }
  }
  while(ll<=lr){
      x[nr++] = x[lr--];
  }
  return;
}
*/

/*
static void Geckomerge_desc_right_full(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT rr = nr - 1;
  IndexT rl = 0;
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  nr = 0;  // re-using nr as write index
  // simple tuning for perfectly presorted
  //if (LT(ldat[ll], rdat[rl]))
  for(;;){
    if (GE(ldat[lr], rdat[rl])){
      x[nr++] = ldat[lr--];
	  if (ll>lr)
        break;
    }else{
      x[nr++] = rdat[rl++];
      if (rl>rr)
        break;
    }
  }
  while(ll<=lr){
      x[nr++] = ldat[lr--];
  }
  while(rl<=rr){
    x[nr++] = rdat[rl++];
  }
  return;
}
static void Geckomerge_desc_right_final(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT rr = nr - 1;
  IndexT rl = 0;
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  nr = 0;  // re-using nr as write index
  // simple tuning for perfectly presorted
  //if (LT(ldat[ll], rdat[rl]))
  for(;;){
    if (GE(ldat[lr], rdat[rl])){
      x[nr++] = ldat[lr--];
	  if (ll>lr)
        return;
    }else{
      x[nr++] = rdat[rl++];
      if (rl>rr)
        break;
    }
  }
  while(ll<=lr){
      x[nr++] = ldat[lr--];
  }
  return;
}
static void Geckomerge_desc_right_stable(
  ValueT *x
, IndexT nl  // == b
, IndexT nr
, IndexT R
){
  const IndexT rr = R;
  IndexT rl = R - nr + 1;
  IndexT lr = rl - nl - 1;
  const IndexT ll = lr - nl + 1;
  nr  = lr + 1;  // re-using nr as write-index
  // simple tuning for perfectly presorted
  //if (LT(x[ll], x[rl]))
  for(;;){
    if (GE(x[lr], x[rl])){
      x[nr++] = x[lr--];
	  if (ll>lr)
        return;
    }else{
      x[nr++] = x[rl++];
      if (rl>rr)
        break;
    }
  }
  while(ll<=lr){
      x[nr++] = x[lr--];
  }
  return;
}
static void Geckomerge_desc_right_reverse(
  ValueT *x
, IndexT nl  // == b
, IndexT nr
, IndexT R
){
  const IndexT rr = R;
  IndexT rl = R - nr + 1;
  IndexT lr = rl - nl - 1;
  const IndexT ll = lr - nl + 1;
  nr  = lr + 1;  // re-using nr as write-index
  // simple tuning for perfectly presorted
  //if (LE(x[ll], x[rl]))
  while(ll<=lr){
    if (GT(x[lr], x[rl])){
      x[nr++] = x[lr--];
	  if (ll>lr)
        return;
    }else{
      x[nr++] = x[rl++];
      if (rl>rr)
        break;
    }
  }
  while(ll<=lr){
      x[nr++] = x[lr--];
  }
  return;
}
*/


/*
static void Geckomerge_asc_left_full(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT rr = nr - 1;
  IndexT rl = 0;
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  nl = nl + nr - 1;  // re-using nl as write index
  // simple tuning for perfectly presorted
  //if (LT(ldat[lr], rdat[rr]))
  for(;;){
    if (GE(ldat[lr], rdat[rl])){
      x[nl--] = rdat[rl++];
      if (rl>rr)
        break;
    }else{
      x[nl--] = ldat[lr--];
      if (ll>lr)
        break;
    }
  }
  while(rl<=rr){
    x[nl--] = rdat[rl++];
  }
  while(ll<=lr){
    x[nl--] = ldat[lr--];
  }
  return;
}
*/
/*
static void Geckomerge_asc_left_final(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT rr = nr - 1;
  IndexT rl = 0;
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  nl = nl + nr - 1;  // re-using nl as write index
  // simple tuning for perfectly presorted
  //if (LT(ldat[lr], rdat[rr]))
  for(;;){
    if (GE(ldat[lr], rdat[rl])){
      x[nl--] = rdat[rl++];
      if (rl>rr)
        return;
    }else{
      x[nl--] = ldat[lr--];
      if (ll>lr)
        break;
    }
  }
  while(rl<=rr){
      x[nl--] = rdat[rl++];
  }
  return;
}
*/
/*
static void Geckomerge_asc_left_stable(
  ValueT *x
, IndexT nr  // == b
, IndexT nl
, IndexT L
){
  const IndexT ll = L;
  IndexT lr = L + nl - 1;
  IndexT rl = lr + nr + 1;
  const IndexT rr = rl + nr - 1;
  nl  = rl - 1;  // re-using nl as write-index
  // simple tuning for perfectly presorted
  //if (LT(x[lr], x[rr]))
  for(;;){
    if (GE(x[lr], x[rl])){
      x[nl--] = x[rl++];
      if (rl>rr)
        return;
    }else{
      x[nl--] = x[lr--];
      if (ll>lr)
        break;
    }
  }
  while(rl<=rr){
      x[nl--] = x[rl++];
  }
  return;
}
*/
static void Geckomerge_asc_left_reverse(
  ValueT *x
, IndexT nr  // == b
, IndexT nl
, IndexT L
){
  const IndexT ll = L;
  IndexT lr = L + nl - 1;
  IndexT rl = lr + nr + 1;
  const IndexT rr = rl + nr - 1;
  nl  = rl - 1;  // re-using nl as write-index
  ValueT u=x[lr], v=x[rl];
  // simple tuning for perfectly presorted
  //if (LE(x[lr], x[rr]))
  for(;;){
    if (GT(u, v)){
      x[nl--] = v; rl++;
      if (rl>rr)
        return;
      v = x[rl];
    }else{
      x[nl--] = u; lr--;
      if (ll>lr)
        break;
      u = x[lr];
    }
  }
  while(rl<=rr){
      x[nl--] = x[rl++];
  }
  return;
}

/*
static void Geckomerge_desc_left_full(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT rr = nr - 1;
  IndexT rl = 0;
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  nl = nl + nr - 1;  // re-using nl as write index
  // simple tuning for perfectly presorted
  //if (GT(ldat[lr], rdat[rr]))
  for(;;){
    if (LE(ldat[lr], rdat[rl])){
      x[nl--] = rdat[rl++];
      if (rl>rr)
        break;
    }else{
      x[nl--] = ldat[lr--];
      if (ll>lr)
        break;
    }
  }
  while(rl<=rr){
    x[nl--] = rdat[rl++];
  }
  while(ll<=lr){
    x[nl--] = ldat[lr--];
  }
  return;
}
static void Geckomerge_desc_left_final(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT rr = nr - 1;
  IndexT rl = 0;
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  nl = nl + nr - 1;  // re-using nl as write index
  // simple tuning for perfectly presorted
  //if (GT(ldat[lr], rdat[rr]))
  for(;;){
    if (LE(ldat[lr], rdat[rl])){
      x[nl--] = ldat[rl++];
      if (rl>rr)
        return;
    }else{
      x[nl--] = rdat[lr--];
      if (ll>lr)
        break;
    }
  }
  while(rl<=rr){
      x[nl--] = ldat[rl++];
  }
  return;
}
static void Geckomerge_desc_left_stable(
  ValueT *x
, IndexT nr  // == b
, IndexT nl
, IndexT L
){
  const IndexT ll = L;
  IndexT lr = L + nl - 1;
  IndexT rl = lr + nr + 1;
  const IndexT rr = rl + nr - 1;
  nl  = rl - 1;  // re-using nl as write-index
  // simple tuning for perfectly presorted
  //if (GT(x[lr], x[rl]))
  for(;;){
    if (LE(x[lr], x[rr])){
      x[nl--] = x[rl++];
      if (rl>rr)
        return;
    }else{
      x[nl--] = x[lr--];
      if (ll>lr)
        break;
    }
  }
  while(rl<=rr){
      x[nl--] = x[rl++];
  }
  return;
}
static void Geckomerge_desc_left_reverse(
  ValueT *x
, IndexT nr  // == b
, IndexT nl
, IndexT L
){
  const IndexT ll = L;
  IndexT lr = L + nl - 1;
  IndexT rl = lr + nr + 1;
  const IndexT rr = rl + nr - 1;
  nl  = rl - 1;  // re-using nl as write-index
  // simple tuning for perfectly presorted
  //if (GE(x[lr], x[rr]))
  for(;;){
    if (LT(x[lr], x[rl])){
      x[nl--] = x[rl++];
      if (rl>rr)
        return;
    }else{
      x[nl--] = x[lr--];
      if (ll>lr)
        break;
    }
  }
  while(rl<=rr){
      x[nl--] = x[rl++];
  }
  return;
}
*/

#endif
