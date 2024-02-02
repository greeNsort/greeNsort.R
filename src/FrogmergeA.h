/*
# greeNsort common Frog merging A-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_FrogmergeA_h
#define ALREADY_DEFINED_FrogmergeA_h

#include "algo.h"

static void FrogmergeA_asc_right_full(
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

  // for(int i=0;i<nl;i++)
  //   Rprintf("FrogmergeA_asc_right_full ldat[%d]=%f\n", i, ldat[i]); R_FlushConsole();
  // for(int i=0;i<nr;i++)
  //   Rprintf("FrogmergeA_asc_right_full rdat[%d]=%f\n", i, rdat[i]); R_FlushConsole();

  nr = 0;  // re-using nr as write-index
  ValueT u=ldat[ll], v=rdat[rl];
  if (GT(ldat[lr], v)){  // simple tuning for perfectly presorted
    for(;;){
      if (GT(u, v)){
        x[nr++] = v; rl++;
        if (rl>rr)
          break;
        v = rdat[rl];
      }else{
        x[nr++] = u; ll++;
  	  if (ll>lr)
          break;
      }
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

static void FrogmergeA_asc_right_final(
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
  ValueT u=ldat[ll], v=rdat[rl];
  if (GT(ldat[lr], v)){ // simple tuning for perfectly presorted
    for(;;){
      if (GT(u, v)){
        x[nr++] = v; rl++;
        if (rl>rr)
          break;
        v = rdat[rl];
      }else{
        x[nr++] = u; ll++;
  	  if (ll>lr)
          return;
      }
      u = ldat[ll];
    }
  }
  while(ll<=lr){
      x[nr++] = ldat[ll++];
  }
  return;
}


static void FrogmergeA_asc_right_stable(
  ValueT *x
, IndexT nl  // == b
, IndexT nr
, IndexT R
){
  const IndexT rr = R;
  IndexT rl = R - nr + 1;
  const IndexT lr = rl - nl - 1;
  IndexT ll = lr - nl + 1;

  // for(int i=ll;i<=lr;i++)
  //   Rprintf("FrogmergeA_asc_right_stable left x[%d]=%f\n", i, x[i]); R_FlushConsole();
  // for(int i=rl;i<=rr;i++)
  //   Rprintf("FrogmergeA_asc_right_stable right x[%d]=%f\n", i, x[i]); R_FlushConsole();

  nr  = lr + 1;  // re-using nr as write-index
  // simple tuning for perfectly presorted
  ValueT u=x[ll], v=x[rl];
  if (GT(x[lr], v)){
    for(;;){
      if (GT(u, v)){
        x[nr++] = v; rl++;
        if (rl>rr)
          break;
        v = x[rl];
      }else{
        x[nr++] =u; ll++;
  	  if (ll>lr)
          return;
      }
      u = x[ll];
    }
  }
  while(ll<=lr){
      x[nr++] = x[ll++];
  }
  return;
}

static void FrogmergeA_asc_left_stable(
    ValueT *x
  , IndexT nl
  , IndexT nr  // == b
  , IndexT L
){
  const IndexT ll = L;
  IndexT lr = L + nl - 1;
  const IndexT rl = lr + nr + 1;
  IndexT rr = rl + nr - 1;

  // for(int i=ll;i<=lr;i++)
  //   Rprintf("FrogmergeA_asc_left_stable left x[%d]=%f\n", i, x[i]); R_FlushConsole();
  // for(int i=rl;i<=rr;i++)
  //   Rprintf("FrogmergeA_asc_left_stable right x[%d]=%f\n", i, x[i]); R_FlushConsole();

  nl  = rl - 1;  // re-using nl as write-index
  // simple tuning for perfectly presorted
  ValueT u=x[lr], v=x[rr];
  if (GT(u, x[rl])){
    for(;;){
      if (GT(u, v)){
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
  }
  while(rl<=rr){
    x[nl--] = x[rr--];
  }
  return;
}

#endif
