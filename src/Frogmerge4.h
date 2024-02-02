/*
# greeNsort Frogsort4 merging
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_Frogmerge4_h
#define ALREADY_DEFINED_Frogmerge4_h

#include "algo.h"

static void Frogmerge_asc_left_full(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  const IndexT rl = 0;
  IndexT rr = nr - 1;
  
  // for(int i=0;i<nl;i++)
  //   Rprintf("Frogmerge_asc_left_full ldat[%d]=%f\n", i, ldat[i]); R_FlushConsole();
  // for(int i=0;i<nr;i++)
  //   Rprintf("Frogmerge_asc_left_full rdat[%d]=%f\n", i, rdat[i]); R_FlushConsole();
    
  nl = nl + nr - 1;  // re-using nl as write index
  // simple tuning for perfectly presorted
  //if (GT(ldat[lr], rdat[rl]))
  ValueT u=ldat[lr], v=rdat[rr];
  for(;;){
    if (GT(u, v)){
      x[nl--] = u; lr--;
	    if (ll>lr)
        break;
	    u = ldat[lr];
    }else{
      x[nl--] = v; rr--;
      if (rl>rr)
        break;
      v = rdat[rr];
    }
  }
  while(rl<=rr){
      x[nl--] = rdat[rr--];
  }
  while(ll<=lr){
      x[nl--] = ldat[lr--];
  }
  return;
}


static void Frogmerge_asc_left_final(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  const IndexT rl = 0;
  IndexT rr = nr - 1;
  nl = nl + nr - 1;  // re-using nl as write index
  // simple tuning for perfectly presorted
  //if (GT(ldat[lr], rdat[rl]))
  ValueT u=ldat[lr], v=rdat[rr];
  for(;;){
    if (GT(u, v)){
      x[nl--] = u; lr--;
	    if (ll>lr)
        break;
	    u = ldat[lr];
    }else{
      x[nl--] = v; rr--;
      if (rl>rr)
        return;
      v = rdat[rr];
    }
  }
  while(rl<=rr){
      x[nl--] = rdat[rr--];
  }
  return;
}


static void Frogmerge_desc_right_full(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  IndexT ll = 0;
  const IndexT lr = nl - 1;
  IndexT rl = 0;
  const IndexT rr = nr - 1;
  nr = 0;  // re-using nr as write-index
  // simple tuning for perfectly presorted
  //if (LT(ldat[lr], rdat[rl]))  
  ValueT u=ldat[ll], v=rdat[rl];
  for(;;){
    if (LT(u, v)){
      x[nr++] = v; rl++;
      if (rl>rr)
        break;
      v = rdat[rl];
    }else{
      x[nr++] = u; ll++;
	    if (ll>lr)
        break;
	    u = ldat[ll];
    }
  }
  while(ll<=lr){
      x[nr++] = ldat[ll++];
  }
  while(rl<=rr){
      x[nr++] = rdat[rl++];
  }
  return;
}


static void Frogmerge_desc_right_final(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  IndexT ll = 0;
  const IndexT lr = nl - 1;
  IndexT rl = 0;
  const IndexT rr = nr - 1;
  nr = 0;  // re-using nr as write-index
  // simple tuning for perfectly presorted
  //if (LT(ldat[lr], rdat[rl])) 
  ValueT u=ldat[ll], v=rdat[rl];
  for(;;){
    if (LT(u, v)){
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
  while(ll<=lr){
      x[nr++] = ldat[ll++];
  }
  return;
}


static void Frogmerge_desc_right_stable(
  ValueT *x
, IndexT nl  // == b
, IndexT nr
, IndexT R
){
  const IndexT rr = R;
  IndexT rl = R - nr + 1;
  const IndexT lr = rl - nl - 1;
  IndexT ll = lr - nl + 1;
  nr  = lr + 1;  // re-using nr as write-index
  // simple tuning for perfectly presorted
  //if (LT(x[lr], x[rl]))
  ValueT u=x[ll], v=x[rl];
  for(;;){
    if (LT(u, v)){
      x[nr++] = v; rl++;
      if (rl>rr)
        break;
      v = x[rl];
    }else{
      x[nr++] = u; ll++;
	    if (ll>lr)
        return;
	    u = x[ll];
    }
  }
  while(ll<=lr){
      x[nr++] = x[ll++];
  }
  return;
}


static void Frogmerge_desc_left_full(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  const IndexT rl = 0;
  IndexT rr = nr - 1;
  nl = nl + nr - 1;  // re-using nl as write index
  // simple tuning for perfectly presorted
  //if (LT(ldat[lr], rdat[rl]))
  ValueT u=ldat[lr], v=rdat[rr];
  for(;;){
    if (LT(u, v)){
      x[nl--] = u; lr--;
	    if (ll>lr)
        break;
	    u = ldat[lr];
    }else{
      x[nl--] = v; rr--;
      if (rl>rr)
        break;
      v = rdat[rr];
    }
  }
  while(rl<=rr){
      x[nl--] = rdat[rr--];
  }
  while(ll<=lr){
      x[nl--] = ldat[lr--];
  }
  return;
}


static void Frogmerge_desc_left_final(
  ValueT *x
, ValueT *ldat  // pointer to left element of left stream
, IndexT nl        // number of values in left stream
, ValueT *rdat  // pointer to left element of right stream
, IndexT nr        // number of values in right stream
)
{
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  const IndexT rl = 0;
  IndexT rr = nr - 1;
  nl = nl + nr - 1;  // re-using nl as write index
  // simple tuning for perfectly presorted
  //if (LT(ldat[lr], rdat[rl]))
  ValueT u=ldat[lr], v=rdat[rr];
  for(;;){
    if (LT(u, v)){
      x[nl--] = u; lr--;
	    if (ll>lr)
        break;
	    u = ldat[lr];
    }else{
      x[nl--] = v; rr--;
      if (rl>rr)
        return;
      v = rdat[rr];
    }
  }
  while(rl<=rr){
      x[nl--] = rdat[rr--];
  }
  return;
}


static void Frogmerge_desc_left_stable(
  ValueT *x
, IndexT nr  // == b
, IndexT nl
, IndexT L
){
  const IndexT ll = L;
  IndexT lr = L + nl - 1;
  const IndexT rl = lr + nr + 1;
  IndexT rr = rl + nr - 1;
  nl  = rl - 1;  // re-using nl as write-index
  // simple tuning for perfectly presorted
  //if (LT(x[lr], x[rl]))
  ValueT u=x[lr], v=x[rr];
  for(;;){
    if (LT(u, v)){
      x[nl--] = u; lr--;
      if (ll>lr)
        break;
      u = x[lr];
    }else{
      x[nl--] = v; rr--;
      if (rl>rr)
        return;
      v = x[rr];
    }
  }
  while(rl<=rr){
      x[nl--] = x[rr--];
  }
  return;
}

#endif
