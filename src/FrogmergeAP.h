/*
# greeNsort common Frog merging A-tuned (pointered implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_FrogmergeAP_h
#define ALREADY_DEFINED_FrogmergeAP_h

#include "algo.h"

#define OMIT_CORRELATED_PRESORTING
//#define SKIP_CORRELATED_PRESORTING

static void FrogmergeAP_asc_right(
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
  ValueT v=*rl;
#ifdef OMIT_CORRELATED_PRESORTING
  if (GT(*lr, v)){
#endif
#ifdef SKIP_CORRELATED_PRESORTING
    ValueT *i=ll+(lr-ll)/2;
    while (i<lr && GE(v,*i)){
      while (ll<=i)
        *(x++) = *(ll++);
      i=ll+(lr-ll)/2;
    }
#endif
    ValueT u=*ll;
    for(;;){
      if (GT(u, v)){
        *(x++) = v; rl++;
        if (rl>rr)
          break;
        v = *rl;
      }else{
        *(x++) =u; ll++;
        if (ll>lr)
          return;
      }
      u = *ll;
    }
#ifdef OMIT_CORRELATED_PRESORTING
  }
#endif
  while(ll<=lr){
    *(x++) = *(ll++);
  }
  return;
}

static void FrogmergeAP_asc_left(
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
  ValueT u=*lr;
#ifdef OMIT_CORRELATED_PRESORTING
  if (GT(u, *rl)){
#endif
#ifdef SKIP_PARTIAL_PRESORTING
    ValueT *i=rr-(rr-rl)/2;
    while (i>rl && GE(*i,u)){
      while (rr>=i)
        *(x--) = *(rr--);
      i=rr-(rr-rl)/2;
    }
#endif
    ValueT v=*rr;
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
#ifdef OMIT_CORRELATED_PRESORTING
  }
#endif
  while(rl<=rr){
    *(x--) = *rr--;
  }
  return;
}

/*
static void FrogmergeAP_asc_right(
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
  if (GT(*lr, v)){
    for(;;){
      if (GT(u, v)){
        *(x++) = v; rl++;
        if (rl>rr)
          break;
        v = *rl;
      }else{
        *(x++) =u; ll++;
    	  if (ll>lr)
            return;
        }
        u = *ll;
    }
  }
  while(ll<=lr){
    *(x++) = *(ll++);
  }
  return;
}

static void FrogmergeAP_asc_left(
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
  if (GT(u, *rl)){
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
  }
  while(rl<=rr){
    *(x--) = *rr--;
  }
  return;
}

*/

#endif
