/*
# greeNsort common Frog merging (pointered implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_FrogmergeP_h
#define ALREADY_DEFINED_FrogmergeP_h

#include "algo.h"

static void FrogmergeP_asc_right(
  ValueT *x
, ValueT *ll
, ValueT *lr
, ValueT *rl
, ValueT *rr
){
  // Rprintf("FrogmergeP_asc_right n=%d nl=%d nr=%d\n", rr-rl+lr-ll+2, lr-ll+1, rr-rl+1);
  // for (ValueT *i=ll;i<=lr;i++)
  //   Rprintf(" l=%f\n", *i);
  // for (ValueT *i=rl;i<=rr;i++)
  //   Rprintf(" r=%f\n", *i);
  ValueT u=*ll, v=*rl;
  for(;;){
    if (GT(u, v)){
      *(x++) = v; rl++;
      if (rl>rr)
        break;
      v = *rl;
    }else{
      *(x++) = u; ll++;
  	  if (ll>lr)
        return;
  	  u = *ll;
    }
  }
  while(ll<=lr){
    *(x++) = *ll++;
  }
  return;
}

static void FrogmergeP_asc_left(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  // Rprintf("FrogmergeP_asc_left n=%d nl=%d nr=%d\n", rr-rl+lr-ll+2, lr-ll+1, rr-rl+1);
  // for (ValueT *i=ll;i<=lr;i++)
  //   Rprintf(" l=%f\n", *i);
  // for (ValueT *i=rl;i<=rr;i++)
  //   Rprintf(" r=%f\n", *i);
  ValueT u=*lr, v=*rr;
  for(;;){
    if (GT(u, v)){
      *(x--) = u; lr--;
      if (ll>lr)
        break;
      u = *lr;
    }else{
      *(x--) = v; rr--;
      if (rl>rr)
        return;
      v = *rr;
    }
  }
  while(rl<=rr){
    *(x--) = *rr--;
  }
  return;
}

/*
static void FrogmergeP_asc_right(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  while(ll<=lr && rl<=rr){
    *(x++) = GT(*ll, *rl) ? *(rl++) : *(ll++);
  }
  while(ll<=lr){
    *(x++) = *(ll++);
  }
  return;
}

static void FrogmergeP_asc_left(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  while(ll<=lr && rl<=rr){
    *(x--) = GT(*lr, *rr) ? *(lr--) : *(rr--);
  }
  while(rl<=rr){
    *(x--) = *rr--;
  }
  return;
}
*/

#endif
